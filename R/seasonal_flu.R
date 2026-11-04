# National seasonal influenza coverage. Only aggregates reach the dashboard.
AS_FLU_AGES <- c("6 months - 4 years", "5 - 12 years", "13 - 17 years", "18 - 64 years", "65+ years")

as_flu_date <- function(x) {
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXt")) return(as.Date(x, tz = "UTC"))
  if (is.numeric(x)) return(as.Date(x, origin = "1899-12-30"))
  text <- trimws(as.character(x))
  result <- as.Date(rep(NA_character_, length(text)))
  for (fmt in c("%Y-%m-%d", "%m/%d/%Y", "%m/%d/%y")) {
    pending <- is.na(result) & !is.na(text) & nzchar(text)
    result[pending] <- suppressWarnings(as.Date(text[pending], format = fmt))
  }
  serial <- is.na(result) & !is.na(text) & grepl("^[0-9]+(\\.[0-9]+)?$", text)
  result[serial] <- as.Date(as.numeric(text[serial]), origin = "1899-12-30")
  result
}

read_as_flu_population <- function(path) {
  raw <- readxl::read_excel(path, sheet = "AmSam", col_names = FALSE,
    col_types = "text", .name_repair = "minimal")
  labels <- toupper(gsub("[[:space:]]", "", raw[[1]]))
  expected <- c("6MONTHS-4YEARS", "5YEARS-12YEARS", "13YEARS-17YEARS", "18YEARS-64YEARS", "65+YEARS")
  if (any(vapply(expected, function(x) sum(labels == x, na.rm = TRUE), integer(1)) != 1L))
    stop("The AmSam Census sheet must contain each of the five age groups once.")
  population <- suppressWarnings(as.numeric(gsub(",", "", raw[[2]][match(expected, labels)])))
  total <- suppressWarnings(as.numeric(gsub(",", "", raw[[2]][which(labels == "TOTAL")])))
  if (any(!is.finite(population) | population <= 0) || length(total) != 1L ||
      !is.finite(total) || abs(sum(population) - total) > 1)
    stop("The AmSam Census populations must be positive and match the total.")
  data.frame(age_group = AS_FLU_AGES, population = population)
}

latest_as_flu_file <- function(directory) {
  files <- list.files(directory,
    pattern = "^Dataset 3_American_Samoa_Flu_DeID_[0-9]{6}\\.xlsx$", full.names = TRUE)
  dates <- as.Date(sub(".*_([0-9]{6})\\.xlsx$", "\\1", files), "%m%d%y")
  valid <- !is.na(dates)
  if (!any(valid)) stop("No American Samoa seasonal flu extract was found.")
  files <- files[valid]; dates <- dates[valid]
  files[order(dates, file.info(files)$mtime, decreasing = TRUE)[[1]]]
}

as_flu_settings <- function(source, settings_file) {
  # Legacy extracts lack analysis_date. Their completed reporting cutoff is stored against
  # the exact export filename, never inferred from the filename or config.R.
  settings <- utils::read.csv(settings_file, colClasses = "character")
  if (!all(c("source_file", "season_start", "reporting_through") %in% names(settings)))
    stop("Seasonal flu settings need source_file, season_start and reporting_through.")
  row <- which(settings$source_file == basename(source))
  if (length(row) != 1L) stop("Record the verified dates for ", basename(source), " in seasonal_flu_settings.csv.")
  start <- as_flu_date(settings$season_start[row])
  cutoff <- as_flu_date(settings$reporting_through[row])
  # Prefer the exported analysis_date when supplied by a future extract.
  headers <- names(readxl::read_excel(source, n_max = 0))
  if ("analysis_date" %in% headers) {
    exported <- readxl::read_excel(source,
      col_types = ifelse(headers == "analysis_date", "text", "skip"))
    values <- unique(as_flu_date(exported$analysis_date))
    if (length(values) != 1L || anyNA(values)) stop("The flu extract must have one valid analysis_date.")
    cutoff <- values
  }
  if (is.na(start) || is.na(cutoff) || start > cutoff || format(start, "%m-%d") != "07-01")
    stop("Seasonal flu dates need a July 1 season start and a valid analysis cutoff.")
  list(season_start = start, cutoff = cutoff)
}

aggregate_as_flu <- function(data, population, season_start, analysis_date) {
  date_cols <- grep("^flu_date[0-9]+$", names(data), value = TRUE)
  if (!"agegroup" %in% names(data) || !length(date_cols))
    stop("Seasonal flu data need agegroup and at least one flu_date column.")
  if (anyDuplicated(population$age_group) || !setequal(population$age_group, AS_FLU_AGES) ||
      any(!is.finite(population$population) | population$population <= 0))
    stop("Each Census age group needs one positive population denominator.")
  month_start <- as.Date(format(analysis_date, "%Y-%m-01"))
  month_end <- seq(month_start, by = "month", length.out = 2L)[[2]] - 1
  through <- if (analysis_date == month_end) analysis_date else month_start - 1
  through <- min(through, seq(season_start, by = "year", length.out = 2L)[[2]] - 1)
  if (through < season_start) stop("There is no completed reporting month in this season.")
  months <- seq(season_start, as.Date(format(through, "%Y-%m-01")), by = "month")
  # Preserve the analytic extract's age groups, including its boundary handling.
  ages <- AS_FLU_AGES[match(as.character(data$agegroup), as.character(1:5))]
  events <- do.call(rbind, lapply(date_cols, function(column) data.frame(
    person = seq_len(nrow(data)), age_group = ages, date = as_flu_date(data[[column]]))))
  events <- unique(events[!is.na(events$age_group) & !is.na(events$date) &
    events$date >= season_start & events$date <= through, , drop = FALSE])
  events <- events[order(events$date), , drop = FALSE]
  first <- events[!duplicated(events$person), , drop = FALSE]
  curves <- expand.grid(month = months, age_group = AS_FLU_AGES, stringsAsFactors = FALSE)
  curves$population <- population$population[match(curves$age_group, population$age_group)]
  curves$people <- vapply(seq_len(nrow(curves)), function(i) {
    end <- seq(curves$month[i], by = "month", length.out = 2L)[[2]] - 1
    as.integer(sum(first$age_group == curves$age_group[i] & first$date <= end))
  }, integer(1))
  curves$coverage <- 100 * curves$people / curves$population
  doses <- data.frame(age_group = AS_FLU_AGES,
    doses = vapply(AS_FLU_AGES, function(age) sum(events$age_group == age), integer(1)))
  list(curves = curves, doses = doses, population = population,
    season_start = season_start, through = through)
}

load_as_seasonal_flu <- function(directory, population_file, settings_file) {
  tryCatch({
    source <- latest_as_flu_file(directory)
    dates <- as_flu_settings(source, settings_file)
    population <- read_as_flu_population(population_file)
    headers <- names(readxl::read_excel(source, n_max = 0))
    keep <- c("agegroup", grep("^flu_date[0-9]+$", headers, value = TRUE))
    data <- readxl::read_excel(source, col_types = ifelse(headers %in% keep, "text", "skip"))
    aggregate_as_flu(data, population, dates$season_start, dates$cutoff)
  }, error = function(e) list(error = conditionMessage(e), through = as.Date(NA_character_)))
}

as_flu_selected_curves <- function(result, ages) {
  x <- result$curves[result$curves$age_group %in% ages, , drop = FALSE]
  if (!nrow(x)) return(x)
  total <- stats::aggregate(cbind(people, population) ~ month, data = x, FUN = sum)
  total$age_group <- "Total"
  total$coverage <- 100 * total$people / total$population
  rbind(x, total[names(x)])
}

as_flu_plot <- function(result, ages, ymax) {
  x <- as_flu_selected_curves(result, ages)
  colors <- setNames(c("#0072B2", "#E69F00", "#009E73", "#CC79A7", "#D55E00", "#003E73"), c(AS_FLU_AGES, "Total"))
  p <- plotly::plot_ly()
  for (age in c(ages, "Total")) {
    line <- x[x$age_group == age, , drop = FALSE]
    p <- plotly::add_trace(p, data = line, x = ~month, y = ~coverage,
      type = "scatter", mode = "lines+markers", name = age,
      line = list(color = colors[[age]], width = if (age == "Total") 4 else 2,
        dash = if (age == "Total") "dash" else "solid"),
      text = ~paste0(age_group, "<br>", format(month, "%B %Y"), "<br>",
        format(people, big.mark = ","), " people / ", format(population, big.mark = ","),
        " population<br>", sprintf("%.2f%%", coverage)),
      hovertemplate = "%{text}<extra></extra>")
  }
  p <- plotly::layout(p,
    xaxis = list(title = "Month", tickvals = unique(x$month), tickformat = "%b %Y"),
    yaxis = list(title = "Cumulative vaccination coverage (%)", range = c(0, ymax), ticksuffix = "%"),
    legend = list(orientation = "h", y = -0.25), margin = list(b = 140, l = 75))
  plotly::config(p, scrollZoom = TRUE, displaylogo = FALSE)
}
