# Run from the root: source("R/run_analysis.R"); result <- run_as_analysis()
# Quarto calls the same function, so there is a single analytic pipeline.

run_as_analysis <- function(project_dir=NULL,analysis_date=NULL,verbose=TRUE) {
  packages<-c("dplyr","tidyr","purrr","stringr","lubridate","readxl","readr","openxlsx","here","tibble")
  missing<-packages[!vapply(packages,requireNamespace,logical(1),quietly=TRUE)]
  if(length(missing))stop("Install required packages: ",paste(missing,collapse=", "))
  if(is.null(project_dir)) {
    here::i_am("config.R")
    project_dir<-here::here()
  }
  env<-environment()
  source(file.path(project_dir,"config.R"),local=env)
  source(file.path(project_dir,"R","as_functions.R"),local=env)
  source(file.path(project_dir,"R","as_schedule.R"),local=env)
  source(file.path(project_dir,"R","as_tables.R"),local=env)
  source(file.path(project_dir,"R","prepare_data.R"),local=env)
  config<-AS_CONFIG
  if(!is.null(analysis_date))config$analysis_date<-as.Date(analysis_date)
  analysis_date<-as.Date(config$analysis_date)
  if(length(analysis_date)!=1 || is.na(analysis_date))stop("Analysis date must be YYYY-MM-DD.")
  raw_dir<-file.path(project_dir,"data","Raw Patient Data")
  output_dir<-file.path(project_dir,"data","Analytic Code Output")
  dir.create(output_dir,recursive=TRUE,showWarnings=FALSE)
  progress_file<-file.path(output_dir,"American_Samoa_Analysis_Progress.log")
  writeLines(character(),progress_file)
  started<-proc.time()[["elapsed"]]
  progress<-function(text) {
    line<-sprintf("[%s | %.1f min] %s",format(Sys.time(),"%H:%M:%S"),
      (proc.time()[["elapsed"]]-started)/60,text)
    cat(line,"\n",file=progress_file,append=TRUE)
    if(isTRUE(verbose))cat(line,"\n",file=stderr())
    flush.console()
  }
  completed<-FALSE
  on.exit(if(!completed)progress("Run stopped before completion. See the Console/Render pane for the error or interruption."),add=TRUE)
  progress(paste("Starting American Samoa analysis; assessment date",analysis_date))
  pick<-function(override,pattern) {
    if(is.null(override))newest_matching_file(raw_dir,pattern) else {
      path<-if(grepl("^(/|[A-Za-z]:)",override))override else file.path(project_dir,override)
      if(!file.exists(path))stop("Configured input does not exist: ",path)
      path
    }
  }
  files<-c(Roster=pick(config$roster_file,"(^Roster.*|.*Patient.?Roster.*)\\.xlsx$"),
    Details=pick(config$details_file,"(^Details?.*|.*Patient.?Details?.*Services.*)\\.xlsx$"),
    Reminder=pick(config$reminder_file,"(^RR.*|.*Reminder.*|.*Recall.*)\\.xlsx$"))
  reports<-lapply(seq_along(files),function(i) {
    progress(paste("Reading",names(files)[i],"report..."))
    report<-clean_report_names(readxl::read_excel(files[i],guess_max=100000))
    progress(sprintf("Read %s: %s rows",names(files)[i],format(nrow(report),big.mark=",")))
    report
  })
  names(reports)<-names(files)
  roster<-reports$Roster; detail<-reports$Details; reminder<-reports$Reminder
  progress("Selecting active children and linking Reminder/Recall...")
  roster$patient_id<-extract_patient_id(roster)
  dob_column<-intersect(c("dob","date_of_birth","birth_date","birthdate"),names(roster))
  if(!length(dob_column))stop("Roster must contain DOB or Date of Birth.")
  roster$dob<-parse_as_date(roster[[dob_column[[1]]]])
  status<-normalize_as_text(coalesce_character_columns(roster,c("clinic_status","patient_status","status")))
  roster$roster_status_unknown<-is.na(status)
  roster$.roster_active<-is.na(status)|status=="ACTIVE"
  roster_clean<-roster |> dplyr::filter(.roster_active,!is.na(patient_id),!is.na(dob),dob<=analysis_date) |>
    dplyr::select(-.roster_active)
  if(anyDuplicated(roster_clean$patient_id))stop("Duplicate active roster patient IDs. Resolve duplicates before analysis.")
  roster_cohort<-roster_clean |> dplyr::filter(!is.na(as_age_group(age_months_at(dob,analysis_date))))
  if(!nrow(roster_cohort))stop("No active children aged 2-83 months as of the analysis date.")
  progress(sprintf("Active children aged 2-83 months: %s",format(nrow(roster_cohort),big.mark=",")))
  rr<-tibble::tibble(patient_id=extract_patient_id(reminder),
    rr_city=coalesce_character_columns(reminder,c("city","patient_city","address_city")),
    immunization_recommendations=coalesce_character_columns(reminder,c(
      "immunization_recommendations","immunization_recommendation","recommendations","recommendation"))) |>
    dplyr::filter(!is.na(patient_id)) |> dplyr::group_by(patient_id) |> dplyr::summarise(
      rr_city_conflict=dplyr::n_distinct(normalize_as_text(rr_city),na.rm=TRUE)>1,
      rr_city=if(rr_city_conflict)NA_character_ else first_nonmissing(rr_city),
      immunization_recommendations=paste(sort(unique(stats::na.omit(immunization_recommendations))),collapse="; "),
      onreminder=1L,.groups="drop")
  progress("Assigning census geography and clinic county fallback...")
  geography<-read_as_geography(file.path(project_dir,config$geography_file))
  override_path<-file.path(project_dir,config$clinic_county_overrides)
  overrides<-if(file.exists(override_path))readr::read_csv(override_path,show_col_types=FALSE) else NULL
  linked<-roster_cohort |> dplyr::left_join(rr,by="patient_id") |> assign_as_geography(geography,overrides)
  progress("Mapping CVX codes, auditing products, and building vaccine histories...")
  cvx_lookup<-read_as_cvx(file.path(project_dir,config$cvx_file))
  history<-build_as_vaccine_history(detail,cvx_lookup,analysis_date,patient_ids=roster_cohort$patient_id)
  progress(sprintf("Assessing vaccination histories (%s antigen records)...",format(nrow(history$events),big.mark=",")))
  # Patient-age indicators, valid dose counts, due-now flags, one-visit product plan.
  analytic_full<-linked |> add_as_utd_flags(history$events,analysis_date,config,progress=progress)
  progress("Selecting vaccine products for eligible doses...")
  analytic_full<-analytic_full |> add_as_need_and_product_flags(config) |>
    dplyr::arrange(island_atoll,district,county,village,age_months,patient_id)
  date_stamp<-format(analysis_date,"%m%d%y")
  # Allowlist prevents newly added roster columns from leaking into dashboard exports.
  geo_columns<-c("island_atoll","district","county","village","community_id","region","state","city",
    "latitude","longitude","population_year","population","population_under5","area_km2","population_density_km2",
    "metric_basis","geography_type","geography_source","geography_qa","county_resolved","coordinate_basis","clinic_conflict")
  safe_columns<-c(geo_columns,"age_months","age_years","agegroup","age_group_label","onreminder",
    "rr_city_conflict","roster_status_unknown","utd","utd_no_mmr","individual_risk_score","days_since_last_vax",
    "months_since_last_vax","analysis_date","rota_all_rotarix",unname(AS_PRODUCT_MAP),unname(AS_COVERAGE_MAP),
    unname(AS_VACCINE_NEED_MAP),paste0(names(AS_CVX_CODES),"_count"),
    paste0(names(AS_CVX_CODES),"_required_doses"),paste0(names(AS_CVX_CODES),"_next_final"),
    paste0(names(AS_CVX_CODES),"_invalid_doses"),as.vector(outer(names(AS_CVX_CODES),paste0(1:5,"utd"),paste0)),
    "dtap54utd","ipv43utd")
  crosswalk<-analytic_full |> dplyr::distinct(patient_id) |> dplyr::mutate(dashboard_patient_id=sprintf("AS%06d",dplyr::row_number()))
  analytic_deid<-analytic_full |> dplyr::left_join(crosswalk,by="patient_id") |>
    dplyr::select(patient_id=dashboard_patient_id,dplyr::any_of(unique(safe_columns)))
  need_columns<-unname(AS_VACCINE_NEED_MAP)
  operational<-analytic_full |> dplyr::filter(dplyr::if_any(dplyr::all_of(need_columns),~.x==1)) |>
    dplyr::select(dplyr::any_of(c("patient_id","patient_name","first_name","last_name","dob","age_months",
      "age_group_label",geo_columns,"primary_contact_lastname_firstname","primary_contact_name","telephone",
      "phone","cell_phone","address_line_1","address_line_2")),dplyr::all_of(need_columns),
      dplyr::all_of(unname(AS_PRODUCT_MAP)),dplyr::ends_with("_next_due_date"))
  missing_coordinates<-analytic_deid |> dplyr::filter(is.na(latitude)|is.na(longitude)|village=="Unknown"|!county_resolved) |>
    dplyr::count(island_atoll,district,county,village,geography_qa,name="Children (n)")
  run_qa<-tibble::tibble(
    Measure=c("Roster rows read","Rows excluded (status / ID / DOB)","Active children aged 2-83 months",
      "Included children with unspecified roster status","Children UTD for core series","Children on reminder/recall",
      "Children with missing/ambiguous village or county","Conflicting RR cities","Unmapped CVX report rows",
      "Invalid tracked antigen doses (age/interval/max-age)","Reminder IDs absent from active roster"),
    Value=c(nrow(roster),nrow(roster)-nrow(roster_clean),nrow(analytic_full),sum(analytic_full$roster_status_unknown),
      sum(analytic_full$utd==1),sum(analytic_full$onreminder==1),sum(analytic_full$village=="Unknown"|!analytic_full$county_resolved),
      sum(analytic_full$rr_city_conflict %in% TRUE),sum(history$cvx_issues$`Rows (n)`[history$cvx_issues$issue=="Unmapped CVX (excluded)"]),
      sum(as.matrix(analytic_full[paste0(names(AS_CVX_CODES),"_invalid_doses")])),sum(!rr$patient_id %in% roster_clean$patient_id)))
  paths<-c(full=file.path(output_dir,paste0("Dataset 1_American_Samoa_2-83 Mos_Full_Dataset_",date_stamp,".xlsx")),
    deid=file.path(output_dir,paste0("Dataset 2_American_Samoa_2-83 Mos_DeID_Full_Dataset_",date_stamp,".xlsx")),
    operational=file.path(output_dir,paste0("American_Samoa_Patients_Needing_Vaccination_",date_stamp,".xlsx")),
    tables=file.path(output_dir,paste0("American_Samoa_Child_Coverage_Planning_Tables_",date_stamp,".xlsx")),
    coordinates=file.path(output_dir,"Missing_Coordinates.xlsx"),
    id_crosswalk=file.path(output_dir,paste0("PRIVATE_Patient_ID_Crosswalk_",date_stamp,".xlsx")))
  for(name in c("full","deid","operational","coordinates","id_crosswalk")) {
    progress(paste("Writing",basename(paths[[name]]),"..."))
    data<-switch(name,full=analytic_full,deid=analytic_deid,operational=operational,coordinates=missing_coordinates,id_crosswalk=crosswalk)
    openxlsx::write.xlsx(data,paths[[name]],asTable=nrow(data)>0,overwrite=TRUE)
  }
  progress("Building and formatting the eight summary tables...")
  tables<-build_as_tables(analytic_deid)
  write_as_table_workbook(tables,paths[["tables"]],analysis_date,history$product_audit,history$cvx_issues,run_qa)
  progress("Preparing dashboard CSVs and QA files...")
  prepare_as_dashboard(analytic_deid,project_dir,config)
  readr::write_csv(run_qa,file.path(output_dir,paste0("Run_QA_",date_stamp,".csv")))
  readr::write_csv(history$product_audit,file.path(output_dir,paste0("Product_Use_Audit_",date_stamp,".csv")))
  readr::write_csv(history$cvx_issues,file.path(output_dir,paste0("CVX_Mapping_QA_",date_stamp,".csv")))
  message("American Samoa analysis complete: ",nrow(analytic_full)," children; as of ",analysis_date)
  completed<-TRUE
  progress("COMPLETE. Excel outputs and dashboard data are ready.")
  invisible(list(run_qa=run_qa,paths=paths,input_files=files,tables=tables,
    product_audit=history$product_audit,cvx_issues=history$cvx_issues))
}
