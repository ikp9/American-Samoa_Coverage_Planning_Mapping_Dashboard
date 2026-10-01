# Shared American Samoa utilities, geography, and CVX mapping.
# Vaccine status functions are in as_schedule.R; no raw County is used as residence.

normalize_as_text <- function(x) {
  x <- toupper(as.character(x))
  x <- gsub("['\u2018\u2019\u02bb\u02bc]", "", x)
  x <- gsub("&", " AND ", x, fixed = TRUE)
  x <- trimws(gsub(" +", " ", gsub("[^A-Z0-9]+", " ", x)))
  x[x %in% c("", "UNKNOWN", "UNK", "MISSING", "NA", "N A", "NULL")] <- NA_character_
  x
}

clean_report_names <- function(x) {
  n <- tolower(gsub("([a-z0-9])([A-Z])", "\\1_\\2", names(x)))
  n <- gsub("^_+|_+$", "", gsub("[^a-z0-9]+", "_", n))
  n[grepl("^[0-9]", n)] <- paste0("x", n[grepl("^[0-9]", n)])
  names(x) <- make.unique(n, sep = "_")
  x
}

coalesce_character_columns <- function(data, candidates) {
  out <- rep(NA_character_, nrow(data))
  for (column in intersect(candidates, names(data))) {
    value <- trimws(as.character(data[[column]]))
    value[value == ""] <- NA_character_
    out <- dplyr::coalesce(out, value)
  }
  out
}

add_missing_columns <- function(data, columns, value = NA) {
  for (column in setdiff(columns, names(data))) data[[column]] <- rep(value, nrow(data))
  data
}

first_nonmissing <- function(x) {
  y <- x[!is.na(x)]
  if (is.character(y)) y <- y[trimws(y) != ""]
  if (length(y)) y[[1]] else x[NA_integer_][[1]]
}

parse_as_date <- function(x) {
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXt")) return(as.Date(x, tz = "UTC"))
  if (is.numeric(x)) return(as.Date(x, origin = "1899-12-30"))
  value <- trimws(as.character(x)); value[value == ""] <- NA_character_
  result <- as.Date(suppressWarnings(lubridate::parse_date_time(
    value, orders = c("ymd", "mdy", "ymd HMS", "mdy HMS"), quiet = TRUE, tz = "UTC"
  )))
  serial <- !is.na(value) & grepl("^[0-9]{5}(\\.[0-9]+)?$", value)
  result[serial] <- as.Date(as.numeric(value[serial]), origin = "1899-12-30")
  result
}

newest_matching_file <- function(directory, pattern, required = TRUE) {
  files <- list.files(directory, pattern, full.names = TRUE, ignore.case = TRUE)
  files <- files[!grepl("^~\\$", basename(files))]
  if (!length(files)) {
    if (!required) return(NA_character_)
    stop("No file matching ", pattern, " in ", directory, call. = FALSE)
  }
  files[[which.max(file.info(files)$mtime)]]
}

extract_patient_id <- function(data) {
  existing <- coalesce_character_columns(data, c(
    "patient_id", "patient_identifier", "webiz_id", "record_id", "patient_number"
  ))
  patient_name <- coalesce_character_columns(data, c("patient_name", "patient", "name"))
  extracted <- stringr::str_match(patient_name, "\\(([^()]*)\\)\\s*$")[, 2]
  extracted <- trimws(extracted)
  dplyr::coalesce(existing, extracted)
}

AS_AGE_LEVELS <- c("2-3 months", "4-5 months", "6-11 months", "12-18 months",
  "19-35 months", "3 years", "4 years", "5 years", "6 years")
as_age_group <- function(age_months) {
  as.integer(cut(age_months, breaks = c(2,4,6,12,19,36,48,60,72,84), right = FALSE))
}
as_age_label <- function(age_months) AS_AGE_LEVELS[as_age_group(age_months)]

read_as_geography <- function(path) {
  # Header rows are specified for the supplied formatted workbook.
  cross <- clean_report_names(readxl::read_excel(path, sheet = "IIS Crosswalk", skip = 6))
  official <- clean_report_names(readxl::read_excel(path, sheet = "Official Villages", skip = 4))
  counties <- clean_report_names(readxl::read_excel(path, sheet = "Counties", skip = 4))
  needed <- c("iis_city", "census_geography", "island_atoll", "district", "county")
  if (!all(needed %in% names(cross))) stop("Geography crosswalk headers changed.", call. = FALSE)
  cross <- cross |> dplyr::transmute(
    city_key = normalize_as_text(iis_city), village = census_geography,
    island_atoll, district, county,
    latitude = as.numeric(latitude), longitude = as.numeric(longitude),
    population_year = 2020L, population = as.numeric(x2020_population),
    population_under5 = as.numeric(x2020_population_under_age_5),
    area_km2 = as.numeric(land_area_sq_km),
    geography_qa = qa_status, geography_type, metric_basis
  ) |> dplyr::filter(!is.na(city_key))
  canonical <- official |> dplyr::transmute(
    city_key = normalize_as_text(census_village), village = census_village,
    island_atoll, district, county,
    latitude = as.numeric(latitude), longitude = as.numeric(longitude),
    population_year = 2020L, population = as.numeric(x2020_population),
    population_under5 = as.numeric(x2020_population_under_age_5),
    area_km2 = as.numeric(land_area_sq_km), geography_qa = "Matched",
    geography_type = "Official Census village", metric_basis = "2020 Census village"
  )
  lookup <- dplyr::bind_rows(cross, canonical) |> dplyr::distinct(city_key, .keep_all = TRUE)
  lookup$population_density_km2 <- ifelse(lookup$area_km2 > 0,
    lookup$population / lookup$area_km2, NA_real_)
  counties <- counties |> dplyr::transmute(
    county, district, island_atoll, latitude = as.numeric(latitude),
    longitude = as.numeric(longitude), population_year = 2020L,
    population = as.numeric(x2020_population),
    population_under5 = as.numeric(x2020_population_under_age_5),
    area_km2 = as.numeric(land_area_sq_km)
  ) |> dplyr::mutate(population_density_km2 = population / dplyr::na_if(area_km2, 0))
  if (anyDuplicated(counties$county)) stop("Duplicate official county references.")
  list(lookup = lookup, official = canonical, counties = counties)
}

clinic_county_match <- function(clinic, counties, overrides = NULL) {
  key <- normalize_as_text(clinic)
  if (is.na(key)) return(NA_character_)
  if (!is.null(overrides) && nrow(overrides)) {
    hit <- stringr::str_detect(key, stringr::regex(overrides$clinic_pattern, ignore_case = TRUE))
    selected <- unique(overrides$county[hit])
    if (length(selected) == 1) return(selected)
    if (length(selected) > 1) return(NA_character_)
  }
  hits <- counties$county[vapply(normalize_as_text(counties$county), function(x)
    stringr::str_detect(paste0(" ", key, " "), stringr::fixed(paste0(" ", x, " "))), logical(1))]
  if (length(hits) == 1) hits else NA_character_
}

assign_as_geography <- function(data, reference, overrides = NULL) {
  # RR City is the sole residence input. Clinic can supply a COUNTY, never a village.
  rr_city <- coalesce_character_columns(data, c("rr_city"))
  clinic <- coalesce_character_columns(data, c(
    "clinic", "clinic_name", "patient_clinic", "patient_default_clinic", "default_clinic", "patient_default_provider",
    "provider_name", "provider", "default_provider"
  ))
  city_key <- normalize_as_text(rr_city)
  mapped <- reference$lookup[match(city_key, reference$lookup$city_key), ]
  if (!is.null(overrides)) {
    if (!all(c("clinic_pattern", "county") %in% names(overrides)) ||
        any(!overrides$county %in% reference$counties$county)) stop("Invalid clinic county override file.")
  }
  # Clinic names and residence/clinic pairs repeat across thousands of children.
  # Resolve each distinct value once, then return results in the original order.
  clinic_keys <- normalize_as_text(clinic)
  unique_clinics <- unique(clinic_keys)
  clinic_matches <- vapply(unique_clinics, clinic_county_match, character(1), reference$counties, overrides)
  inferred <- unname(clinic_matches[match(clinic_keys,unique_clinics)])
  geography_key <- paste(match(city_key,reference$lookup$city_key),
    match(inferred,reference$counties$county),sep="/")
  unique_keys <- unique(geography_key)
  row_index <- match(geography_key,unique_keys)
  representative_rows <- match(unique_keys,geography_key)
  rows <- lapply(representative_rows, function(i) {
    m <- mapped[i, ]; clinic_county <- inferred[i]
    matched <- !is.na(m$village)
    ambiguous <- matched && (grepl(";", m$county) || grepl(";", m$district))
    method <- if (matched) "Reminder/Recall City" else "Unresolved"
    resolved <- matched && !ambiguous
    if (!matched || ambiguous) {
      candidates <- if (matched) trimws(strsplit(m$county, ";", fixed = TRUE)[[1]]) else reference$counties$county
      if (!is.na(clinic_county) && clinic_county %in% candidates) {
        c <- reference$counties[match(clinic_county, reference$counties$county), ]
        m$county <- c$county; m$district <- c$district
        if (!matched) {
          m$village <- "Unknown"; m$island_atoll <- c$island_atoll
          for (field in c("latitude", "longitude", "population_year", "population",
                          "population_under5", "area_km2", "population_density_km2")) m[[field]] <- c[[field]]
          m$metric_basis <- "2020 Census county (village unknown)"
        }
        method <- if (matched) "RR City + clinic county" else "Clinic county only"
        resolved <- TRUE
      }
    }
    if (!matched && is.na(clinic_county)) {
      for (field in c("village", "county", "district", "island_atoll")) m[[field]] <- "Unknown"
    }
    m$geography_source <- method
    m$county_resolved <- resolved
    m$coordinate_basis <- if (!matched && resolved) "County centroid; village unknown" else
      if (matched) "Census village or combined geography point" else "Missing"
    m$geography_qa <- if (!matched) if (resolved) "Unknown village; clinic county" else "Unmatched RR City / clinic" else
      if (!resolved) "Multiple county/district candidates" else dplyr::coalesce(m$geography_qa, "Matched")
    m$clinic_conflict <- matched && !ambiguous && !is.na(clinic_county) && clinic_county != m$county
    m
  })
  geo <- dplyr::bind_rows(rows)[row_index,] |> dplyr::select(-city_key)
  data <- data |> dplyr::select(-dplyr::any_of(names(geo)))
  dplyr::bind_cols(data, geo) |> dplyr::mutate(
    region = island_atoll, state = island_atoll, city = village,
    community_id = paste(island_atoll, district, county, village, sep = "__")
  )
}

normalize_cvx <- function(x) {
  x <- trimws(as.character(x))
  good <- !is.na(x) & grepl("^[0-9]+(\\.0+)?$", x)
  out <- rep(NA_character_, length(x)); out[good] <- as.character(as.integer(as.numeric(x[good])))
  out
}

# Only documented antigen-containing CVX codes are accepted. DT/Td/Tdap are not DTaP;
# PPSV23, PCV21, and pneumococcal-unspecified are not credited by this AAP
# childhood PCV15/20 assessment; historical pediatric conjugate codes are retained.
AS_CVX_CODES <- list(
  dtap = c("1","20","22","50","102","106","107","110","120","130","132","146","170","198"),
  ipv = c("10","110","120","130","132","146","170","195"),
  mmr = c("3","94"),
  hib = c("17","22","46","47","48","49","50","51","102","120","132","146","148","170","198"),
  hepb = c("8","42","43","44","45","51","102","104","110","132","146","189","193","198","220"),
  pcv = c("100","133","152","177","215","216"),
  rota = c("74","116","119","122"), var = c("21","94"),
  hepa = c("31","52","83","84","85","104","193")
)

AS_VACCINE_NEED_MAP <- c(DTaP="need_dtap", IPV="need_ipv", MMR="need_mmr", Hib="need_hib",
  HepB="need_hepb", PCV="need_pcv", Rotavirus="need_rota", Varicella="need_var", HepA="need_hepa")
AS_COVERAGE_MAP <- c(DTaP="dtap_utd", IPV="ipv_utd", MMR="mmr_utd", Hib="hib_utd",
  HepB="hepb_utd", PCV="pcv_utd", Rotavirus="rota_utd", Varicella="var_utd", HepA="hepa_utd")
AS_PRODUCT_MAP <- c(Vaxelis="Vaxelis", Pediarix="Pediarix", Pentacel="Pentacel",
  Quadracel="Quadracel", Kinrix="Kinrix", ProQuad="ProQuad",
  `Hib single-antigen (per inventory)`="Hib_Single", `DTaP single-antigen`="DTaP_Single",
  `IPV single-antigen`="IPV_Single", `HepB single-antigen`="HepB_Single", `MMR single-antigen`="MMR_Single",
  `PCV conjugate (per inventory)`="PCV_Product", RotaTeq="RotaTeq", Rotarix="Rotarix",
  Varivax="Varivax", `HepA pediatric (per inventory)`="HepA_Product")

read_as_cvx <- function(path) {
  cvx <- clean_report_names(readxl::read_excel(path, sheet = "Web_cvx")) |> dplyr::transmute(
    cvx = normalize_cvx(cvx_code), cvx_description = cvx_short_description,
    full_vaccine_name, vaccine_status = vaccine_status,
    nonvaccine = tolower(as.character(nonvaccine)) == "true"
  ) |> dplyr::filter(!is.na(cvx))
  if (anyDuplicated(cvx$cvx)) stop("Duplicate CVX codes in the reference table.")
  cvx
}

classify_as_antigens <- function(cvx) {
  if (is.na(cvx)) return(character())
  names(AS_CVX_CODES)[vapply(AS_CVX_CODES, function(codes) cvx %in% codes, logical(1))]
}

as_product_name <- function(cvx) {
  known <- c(`110`="Pediarix", `120`="Pentacel", `130`="DTaP-IPV (Kinrix/Quadracel; brand not identified by CVX)",
    `146`="Vaxelis", `132`="DTaP-IPV-Hib-HepB historical", `94`="ProQuad/MMRV",
    `51`="Hib-HepB", `49`="PedvaxHIB (PRP-OMP)", `48`="Hib PRP-T",
    `116`="RotaTeq", `119`="Rotarix", `133`="Prevnar 13", `215`="Vaxneuvance (PCV15)",
    `216`="Prevnar 20", `198`="DTP-HepB-Hib (non-US)", `102`="DTP-Hib-HepB")
  unname(known[cvx])
}

build_as_vaccine_history <- function(detail, cvx_lookup, analysis_date, patient_ids=NULL) {
  code <- coalesce_character_columns(detail, c("vaccination_code_id", "cvx", "cvx_code", "vaccine_cvx_code"))
  date <- coalesce_character_columns(detail, c("vaccination_date", "immunization_date", "date_administered", "service_date"))
  # Preserve native Excel date classes when available.
  date_column <- intersect(c("vaccination_date", "immunization_date", "date_administered", "service_date"), names(detail))
  if (!length(date_column) || all(is.na(code))) stop("Details report needs Vaccination Code ID and Vaccination Date.")
  date <- parse_as_date(detail[[date_column[[1]]]])
  status <- coalesce_character_columns(detail, c("vaccination_status", "service_status", "administration_status"))
  # CVX classifications are constant: classify each distinct code once.
  normalized_codes <- normalize_cvx(code)
  unique_codes <- unique(normalized_codes)
  antigen_lookup <- purrr::map(unique_codes,classify_as_antigens)
  events <- tibble::tibble(
    patient_id = extract_patient_id(detail), cvx = normalized_codes, vaccination_date = date,
    reported_product = coalesce_character_columns(detail, c("product_name","vaccine_name","vaccination_name","vaccination_description","vaccine")),
    excluded_status = stringr::str_detect(dplyr::coalesce(status,""), stringr::regex("REFUS|DECLIN|DEFER|CONTRA|NOT GIVEN|ERROR|DELETED", ignore_case=TRUE))
  ) |> dplyr::left_join(cvx_lookup, by = "cvx") |> dplyr::mutate(
    mapped = !is.na(cvx_description), antigen = antigen_lookup[match(cvx,unique_codes)],
    product_group = dplyr::coalesce(as_product_name(cvx), cvx_description, "Unmapped CVX"),
    include_event = !is.na(patient_id) & !is.na(vaccination_date) & vaccination_date <= analysis_date &
      !excluded_status & !dplyr::coalesce(nonvaccine, FALSE) & mapped
  )
  # Product frequencies count distinct patient/date/CVX administrations, not antigen rows.
  use <- events |> dplyr::filter(include_event) |> dplyr::distinct(patient_id, cvx, vaccination_date, .keep_all=TRUE)
  excluded_display <- function(x) stringr::str_detect(dplyr::coalesce(x,""),
    stringr::regex("RSV|RESPIRATORY SYNCYTIAL|NIRSEVIMAB|CLESROVIMAB|BEYFORTUS|ENFLONSIA",ignore_case=TRUE))
  audit <- use |> dplyr::filter(!excluded_display(cvx_description)) |>
    dplyr::mutate(combination = lengths(antigen) > 1) |>
    dplyr::count(cvx, cvx_description, product_group, combination, name="Administrations (n)") |>
    dplyr::arrange(dplyr::desc(`Administrations (n)`))
  audit$`Share of administrations (%)` <- if (nrow(audit)) 100*audit$`Administrations (n)`/sum(audit$`Administrations (n)`) else numeric()
  issues <- events |> dplyr::mutate(issue = dplyr::case_when(
    is.na(patient_id) ~ "Missing patient ID", is.na(vaccination_date) ~ "Missing / unparseable date",
    vaccination_date > analysis_date ~ "After analysis date (excluded)", !mapped ~ "Unmapped CVX (excluded)",
    excluded_status ~ "Non-administration status (excluded)", nonvaccine ~ "Nonvaccine (excluded)",
    lengths(antigen)==0 ~ "Outside pediatric indicators (excluded)", TRUE ~ NA_character_
  )) |> dplyr::filter(!is.na(issue),!excluded_display(cvx_description)) |>
    dplyr::count(cvx, cvx_description, issue, name="Rows (n)")
  # Keep the product audit/QA for the complete Details report, but expand antigen
  # histories only for the current cohort when its patient IDs are supplied.
  cohort_use <- if(is.null(patient_ids))use else use |> dplyr::filter(patient_id %in% patient_ids)
  long <- cohort_use |> dplyr::filter(lengths(antigen)>0) |> tidyr::unnest_longer(antigen) |>
    dplyr::group_by(patient_id,antigen,vaccination_date) |> dplyr::summarise(
      cvx=if(dplyr::n_distinct(cvx)>1 && dplyr::first(antigen)=="hib")
        if(all(cvx %in% c("49","51")))"49" else "17" else
        if(dplyr::n_distinct(cvx)>1 && dplyr::first(antigen)=="rota")"122" else dplyr::first(cvx),
      product_group=paste(sort(unique(product_group)),collapse="; "),.groups="drop") |>
    dplyr::arrange(patient_id,antigen,vaccination_date)
  list(events = long, product_audit = audit, cvx_issues = issues)
}
