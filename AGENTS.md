# Dashboard requirements

- Every dashboard page, including new pages, must display `Data as of [as of date]`
  at the top. Preserve the shared `page_navbar()` header in `app.R` so new navigation
  panels inherit it. Separate page layouts must include the same date line.
- Use the displayed data's exported `analysis_date` for this date, not today's date
  or the configuration date (which may have changed since the data was generated).
