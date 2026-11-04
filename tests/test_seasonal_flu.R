# Run from the project root; core calculation tests need only base R.
source("R/seasonal_flu.R")
population <- data.frame(age_group = AS_FLU_AGES, population = c(100, 200, 300, 400, 500))
patients <- data.frame(agegroup = c(1, 2, 3, 4, 5, 1, NA, 0),
  flu_date1 = as.Date(c("2026-07-01", "2026-09-30", "2026-10-01", "2026-06-30",
    "2026-07-02", "2026-07-03", "2026-07-04", "2026-07-04")),
  flu_date2 = as.Date(c("2026-08-05", "2026-09-30", NA, NA, NA, "2026-07-03", NA, NA)))
result <- aggregate_as_flu(patients, population, as.Date("2026-07-01"), as.Date("2026-09-30"))
total <- subset(as_flu_selected_curves(result, AS_FLU_AGES), age_group == "Total")
stopifnot(identical(total$people, c(3, 3, 4)), all(total$population == 1500),
  all(abs(total$coverage - 100 * c(3, 3, 4) / 1500) < 1e-12),
  sum(result$doses$doses) == 5)
single <- as_flu_selected_curves(result, AS_FLU_AGES[1])
stopifnot(all(single$people == 2), all(single$population == 100), all(single$coverage == 2),
  nrow(as_flu_selected_curves(result, character())) == 0)
partial <- aggregate_as_flu(patients, population, as.Date("2026-07-01"), as.Date("2026-10-19"))
stopifnot(partial$through == as.Date("2026-09-30"), identical(partial$curves, result$curves))
empty <- patients; empty$flu_date1[] <- NA; empty$flu_date2[] <- NA
zeros <- aggregate_as_flu(empty, population, as.Date("2026-07-01"), as.Date("2026-09-30"))
stopifnot(all(zeros$curves$coverage == 0), sum(zeros$doses$doses) == 0)
for (bad in list(population[-1, ], transform(population, population = 0), rbind(population, population[1, ]))) {
  stopifnot(inherits(try(aggregate_as_flu(patients, bad, as.Date("2026-07-01"),
    as.Date("2026-09-30")), silent = TRUE), "try-error"))
}
stopifnot(all(as_flu_date(c("2026-07-01", "07/01/2026", "46204")) == as.Date("2026-07-01")))
cat("PASS: seasonal people/doses, duplicate dates, age selection, weighted totals, zero doses, denominators and reporting boundaries.\n")

# Optional integration check against the local workbooks.
if (requireNamespace("readxl", quietly = TRUE)) {
  pop <- read_as_flu_population("data/Census_Flu_2026.xlsx")
  stopifnot(identical(pop$population, c(4130, 8141, 5514, 28339, 3147)))
  loaded <- load_as_seasonal_flu("data/Analytic Code Output", "data/Census_Flu_2026.xlsx",
    "data/seasonal_flu_settings.csv")
  stopifnot(is.null(loaded$error))
  cat("PASS: local workbook loading and Census population validation.\n")
}
