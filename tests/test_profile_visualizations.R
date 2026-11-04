# Exercise chart inputs and heatmap navigation without loading Shiny or Plotly.
source("R/prepare_data.R")
helpers <- c("profile_village_choices", "profile_selection", "priority_component_columns",
  "priority_component_labels", "priority_component_weights", "score_label", "profile_component_summary")
for (expr in parse("app.R")) {
  if (is.call(expr) && identical(expr[[1]], as.name("<-")) &&
      as.character(expr[[2]])[1] %in% helpers) eval(expr)
}
x <- read.csv("data/community_summary.csv", check.names = FALSE)
for (i in seq_len(nrow(x))) {
  d <- profile_component_summary(x[i, ], x)
  stopifnot(abs(round(sum(d$points), 1) - x$priority_score[i]) < 1e-8,
    identical(d$weight, c(0.90, 0.05, 0.05)),
    identical(d$median, unname(vapply(x[priority_component_columns], median, numeric(1), na.rm = TRUE))))
  target <- profile_selection(x, x$community_id[i])
  stopifnot(target$island == x$island_atoll[i], target$district == x$district[i],
    target$community_id == x$community_id[i], target$district %in% target$districts,
    target$community_id %in% target$villages,
    all(x$island_atoll[match(target$villages, x$community_id)] == target$island),
    all(x$district[match(target$villages, x$community_id)] == target$district),
    !anyDuplicated(names(target$villages)))
}
missing <- x[1:2, ]; missing[priority_component_columns] <- NA_real_
d <- profile_component_summary(missing[1, ], missing)
stopifnot(all(is.na(d$score)), all(is.na(d$points)), all(is.na(d$median)),
  identical(score_label(c(NA, 0, 100)), c("N/A", "0.0", "100.0")),
  inherits(tryCatch(profile_selection(x, "invalid-community"), error = identity), "error"))
cat("PASS: all chart contributions, reference medians, missing data and every heatmap selection path\n")
