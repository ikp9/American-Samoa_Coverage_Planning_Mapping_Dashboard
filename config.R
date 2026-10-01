# Change these settings before each refresh. Run from the project root.
AS_CONFIG <- list(
  analysis_date = as.Date("2026-09-29"),
  # NULL selects the newest workbook matching the documented report names.
  roster_file = NULL,
  details_file = NULL,
  reminder_file = NULL,
  geography_file = "data/American_Samoa_GIS_Demographics.xlsx",
  cvx_file = "data/CVX Codes.xlsx",
  # Optional local CSV: clinic_pattern,county (exact official county name).
  clinic_county_overrides = "data/clinic_county_overrides.csv",
  # Priority proxy for travel from Tutuila; not an observed travel-time measure.
  primary_island = "Tutuila Island",
  # Combinations are used only when ALL their antigens are eligible today.
  # Reorder/remove products to match local supply after reviewing Product Use Audit.
  combination_preference = c("Vaxelis", "Pediarix", "Pentacel"),
  school_entry_product = "None", # "Kinrix"/"Quadracel" after brand/inventory review
  allow_mmrv = FALSE,
  default_rotavirus_product = "RotaTeq", # or "Rotarix"; history takes precedence
  # Retrospective validity only. Prospective due dates use full intervals.
  grace_days = 4L
)
