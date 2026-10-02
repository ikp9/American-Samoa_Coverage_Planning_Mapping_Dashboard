# Open the .Rproj in RStudio, then open this file and click Source.
# Assessment date and report paths are in config.R.
# This runs the complete analysis and prints live progress in the Console.
# The updated analysis script must be saved as R/run_analysis.R.

if(!file.exists("config.R") || !file.exists("R/run_analysis.R")) {
  stop("Open the American Samoa .Rproj before sourcing RUN_ANALYSIS.R.",call.=FALSE)
}
analysis_root<-normalizePath(".",winslash="/",mustWork=TRUE)
analysis_code<-file.path(analysis_root,"R","run_analysis.R")
message("Loading analysis code: ",analysis_code)
# Load a fresh copy, so a function from an earlier run cannot be reused.
analysis_environment<-new.env(parent=globalenv())
sys.source(analysis_code,envir=analysis_environment)
if(!exists("AS_PATIENT_EXPORT_REVISION",envir=analysis_environment,inherits=FALSE) ||
   !identical(analysis_environment$AS_PATIENT_EXPORT_REVISION,"20261002_35_COLUMNS")) {
  stop("The updated 35-column export code was not found. Replace R/run_analysis.R with the file in the update package.",call.=FALSE)
}
message("Updated 35-column patient export loaded.")
result<-analysis_environment$run_as_analysis(project_dir=analysis_root)
print(result$run_qa)
message("Patients needing vaccination workbook: ",
  normalizePath(result$paths[["operational"]],winslash="/",mustWork=TRUE))
message("Analysis finished. Open app.R and click Run App to view the dashboard.")
