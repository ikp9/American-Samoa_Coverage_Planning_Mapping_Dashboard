# Open the .Rproj in RStudio, then open this file and click Source.
# Assessment date and report paths are in config.R.
# This runs the complete analysis and prints live progress in the Console.

if(!file.exists("config.R") || !file.exists("R/run_analysis.R")) {
  stop("Open the American Samoa .Rproj before sourcing RUN_ANALYSIS.R.",call.=FALSE)
}
source("R/run_analysis.R")
result<-run_as_analysis()
print(result$run_qa)
message("Analysis finished. Open app.R and click Run App to view the dashboard.")
