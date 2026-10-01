# Run once from R/RStudio. Quarto itself is a separate installation.
packages<-c("shiny","bslib","dplyr","tidyr","purrr","stringr","lubridate","readxl",
  "readr","openxlsx","here","tibble","DT","leaflet","plotly","knitr")
missing<-packages[!vapply(packages,requireNamespace,logical(1),quietly=TRUE)]
if(length(missing))install.packages(missing,repos="https://cloud.r-project.org")
message("Project packages are installed.")
