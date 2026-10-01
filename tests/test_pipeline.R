# End-to-end synthetic import/export test. Runs in a temporary project copy.
# Rscript tests/test_pipeline.R
source("config.R")
source("R/as_functions.R")
source("R/as_schedule.R")
source("R/run_analysis.R")
fixture<-if(nzchar(Sys.getenv("AS_TEST_FIXTURE_DIR")))Sys.getenv("AS_TEST_FIXTURE_DIR") else tempfile("as-pipeline-")
dir.create(fixture,recursive=TRUE,showWarnings=FALSE)
dir.create(file.path(fixture,"data","Raw Patient Data"),recursive=TRUE,showWarnings=FALSE)
dir.create(file.path(fixture,"www"),recursive=TRUE,showWarnings=FALSE)
invisible(file.copy("R",fixture,recursive=TRUE,overwrite=TRUE))
invisible(file.copy(c("config.R","app.R","American Samoa_Coverage_Planning_Mapping_Dashboard.Rproj"),fixture,overwrite=TRUE))
invisible(file.copy("www/styles.css",file.path(fixture,"www"),overwrite=TRUE))
invisible(file.copy(c("data/American_Samoa_GIS_Demographics.xlsx","data/CVX Codes.xlsx"),file.path(fixture,"data"),overwrite=TRUE))
invisible(file.create(file.path(fixture,".here")))
at<-as.Date("2026-09-30")
ages<-c(2,4,6,12,15,20,25,39,50,65,77,20)
ids<-paste0("PRIVATE_ID_",seq_along(ages))
births<-month_date(at,-ages)
names<-paste0("SYNTHETIC PERSON ",seq_along(ages)," (",ids,")")
roster<-data.frame(`Patient Name`=names,DOB=births,`Clinic Status`="ACTIVE",
  County="Pago Pago",City="Pago Pago",Clinic=c(rep("Tualauta Clinic",10),"Lealataua Clinic","Unknown clinic"),
  `Unexpected Sensitive Field`=rep("SHOULD_NEVER_EXPORT",12),check.names=FALSE)
rr<-data.frame(`Patient Name`=names,City=c("AUNUU","TAFUNA","FAGAIMA","GATAIVAI","TAFUNA","AFONO",
  "TAFUNA","FAGATOGO","OFU","NUUULI","not in crosswalk","MALAELOA"),County="Pago Pago",
  `Immunization Recommendations`="DTAP (4); MMR (2); PCV (4); RSV",check.names=FALSE)
rows<-list(); j<-0L
add_event<-function(i,cvx,m) {
  date<-month_date(births[i],m)
  if(date<=at) {
    j<<-j+1L;rows[[j]]<<-data.frame(`Patient Name`=names[i],`Vaccination Code ID`=cvx,
      `Vaccination Date`=date,check.names=FALSE)
  }
}
for(i in seq_along(ages)) {
  for(m in c(2,4,6,15))add_event(i,"20",m)
  for(m in c(2,4,6))add_event(i,"10",m)
  for(m in c(2,4,6,12)) {add_event(i,"48",m);add_event(i,"133",m)}
  for(m in c(0,2,6))add_event(i,"08",m)
  add_event(i,"03",12);add_event(i,"21",12);add_event(i,"83",12)
  for(m in c(2,4))add_event(i,"119",m)
}
add_event(6,"146",2);add_event(6,"110",4)
add_event(6,"306",2);add_event(6,"33",15);add_event(6,"28",15)
add_event(6,"999999",16)
details<-dplyr::bind_rows(rows)
# Duplicate same-date record must not add an antigen dose.
details<-dplyr::bind_rows(details,details[1,])
details<-dplyr::bind_rows(details,data.frame(`Patient Name`=names[6],`Vaccination Code ID`="03",
  `Vaccination Date`=at+1,check.names=FALSE))
# These active roster children are outside the 2-83-month analysis cohort.
# Their Details administrations must remain in the full-report product audit.
outside_names<-c("SYNTHETIC OUTSIDE (OUTSIDE_84)","SYNTHETIC OUTSIDE (OUTSIDE_1)")
outside_roster<-roster[c(1,1),]
outside_roster$`Patient Name`<-outside_names
outside_roster$DOB<-month_date(at,c(-84,-1))
roster<-dplyr::bind_rows(roster,outside_roster)
details<-dplyr::bind_rows(details,data.frame(`Patient Name`=outside_names[1],
  `Vaccination Code ID`="146",`Vaccination Date`=at-400,check.names=FALSE))
raw_dir<-file.path(fixture,"data","Raw Patient Data")
openxlsx::write.xlsx(roster,file.path(raw_dir,"Patient Roster_test.xlsx"))
openxlsx::write.xlsx(rr,file.path(raw_dir,"Reminder Recall_test.xlsx"))
openxlsx::write.xlsx(details,file.path(raw_dir,"Patient Details with Services_test.xlsx"))
result<-run_as_analysis(project_dir=fixture,analysis_date=at)
check<-function(value,label)if(!isTRUE(value))stop("FAILED: ",label)
check(all(file.exists(result$paths)),"All expected output workbooks exist")
check(result$run_qa$Value[result$run_qa$Measure=="Active children aged 2-83 months"]==12,"Denominator has one child per roster ID")
den<-result$tables[["IIS Denominator"]]
check(den$`Children (n)`[den$`Island/Atoll`=="American Samoa Total"]==12,"IIS territory total reconciles")
check(sum(den$`Children (n)`[den$`Island/Atoll`!="American Samoa Total"])==12,"County totals reconcile without geography duplication")
deid<-readxl::read_excel(result$paths[["deid"]])
check(!any(grepl("name|contact|address|birth|^dob$|unexpected",names(deid),ignore.case=TRUE)),"Allowlist excludes unrecognized identifiers")
check(!any(vapply(deid,function(x)any(grepl("PRIVATE_ID_|SHOULD_NEVER_EXPORT|SYNTHETIC PERSON",as.character(x))),logical(1))),"No source identifiers or free text in deidentified workbook")
patient<-readr::read_csv(file.path(fixture,"data","patient_dashboard.csv"),show_col_types=FALSE)
check(!any(grepl("rsv|nirsevimab|clesrovimab",names(patient),ignore.case=TRUE)),"Excluded antigen absent from dashboard")
check(nrow(patient)==12,"Dashboard child count reconciles")
full<-readxl::read_excel(result$paths[["full"]])
check(!any(full$patient_id %in% c("OUTSIDE_84","OUTSIDE_1")),"Early cohort filtering excludes children below 2 or above 83 months")
for(name in names(result$tables))check(!any(grepl("RSV",as.matrix(result$tables[[name]]),ignore.case=TRUE)),paste("Excluded antigen absent from",name))
check(!any(grepl("RSV",as.matrix(result$product_audit),ignore.case=TRUE)),"Excluded antigen absent from product-use audit")
expected<-list(
  `IIS Denominator`=c("Island/Atoll","District","County","Children (n)"),
  `Reminder by Village`=c("Island/Atoll","District","County","Village","Children on Reminder/Recall (n)"),
  `Needs by Vaccine`=c("Island/Atoll","District","County","Village","Vaccine Type","Children Due (n)"),
  `Products Needed`=c("Island/Atoll","District","County","Village","Vaccine Product","Doses Needed (n)"),
  `UTD by Village`=c("Island/Atoll","District","County","Village","Denominator","Children UTD (n)","Children UTD (%)"),
  `Coverage 19-35 Months`=c("Island/Atoll","District","County","Vaccine","Denominator","Children UTD (n)","Children UTD (%)"),
  `Series 19-35 Months`=c("Island/Atoll","District","County","Series","Denominator","Children UTD (n)","Children UTD (%)"))
for(name in names(expected))check(identical(names(result$tables[[name]]),expected[[name]]),paste("Requested columns:",name))
dose_header<-names(readxl::read_excel(result$paths[["tables"]],sheet="Dose Coverage",skip=2,n_max=1))
check(identical(dose_header,c("Island/Atoll","District","County","Age Group","Denominator","Children UTD (n)","Children UTD (%)")),"Dose Coverage exact seven columns within vaccine sections")
community<-readr::read_csv(file.path(fixture,"data","community_summary.csv"),show_col_types=FALSE)
check(sum(community$child_population)==12,"Community child counts reconcile")
tafuna<-community[community$village=="Tafuna",]
check(length(unique(tafuna$population))==1,"Alias rows use one parent-village population value")
check(any(result$product_audit$cvx=="146") && any(result$product_audit$cvx=="110"),"Combination frequency audit includes observed products")
check(result$product_audit$`Administrations (n)`[result$product_audit$cvx=="146"]==2,
  "Product audit retains the Details administration outside the age cohort")
check(any(result$cvx_issues$issue=="After analysis date (excluded)"),"Future administrations excluded and flagged")
check(any(result$cvx_issues$issue=="Unmapped CVX (excluded)"),"Unknown CVX excluded and flagged")
check(deid$mmr_count[deid$age_months==20][1]==1,"Future MMR does not inflate coverage")
progress_path<-file.path(fixture,"data","Analytic Code Output","American_Samoa_Analysis_Progress.log")
progress_text<-readLines(progress_path)
check(any(grepl("Children assessed: 12 / 12",progress_text,fixed=TRUE)),"Live progress reports cohort completion")
check(grepl("COMPLETE",tail(progress_text,1),fixed=TRUE),"Successful run ends with COMPLETE in the progress log")
message("PASS: synthetic end-to-end workflow, requested table columns, reconciliation, and export privacy")
message("Synthetic fixture: ",fixture)
if(nzchar(Sys.getenv("AS_TEST_FIXTURE_MARKER")))writeLines(fixture,Sys.getenv("AS_TEST_FIXTURE_MARKER"))
