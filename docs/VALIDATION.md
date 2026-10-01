# Validation record

Validated September 30, 2026, using R 4.3.3 and synthetic patient records.

- 60 focused checks passed for age boundaries, valid dose intervals, grace
  periods, Hib/PCV catch-up, rotavirus formulation/maximum ages, CVX exclusions,
  geography precedence, clinic fallback, and combination-product selection.
- The three-report synthetic workflow completed. Cohort, county, and community
  counts reconciled without duplicating children. The eight requested table
  layouts passed column checks, including seven columns within each vaccine
  section of Dose Coverage.
- Patient/dashboard export checks passed for identifier allowlists, excluded
  vaccine indicators, unknown CVX codes, duplicate administrations, and future
  administration dates.
- Shiny server checks serialized 13 map, plot, and table outputs. Island,
  district, county, and village filter changes produced the expected child
  counts. These were server checks; an interactive browser session was not
  available for visual dashboard inspection.
- The formatted synthetic workbook was rendered to PDF for layout inspection.
  Source R files passed parsing. The Quarto report uses the tested R pipeline;
  a complete Quarto HTML render was not run in this environment.
- The processing update preserves completed ages for 26,000 date pairs,
  including month-end, leap-day, and negative intervals. All assessment fields
  and attributes matched the previous version on a 200-child Date-based fixture.
  Assessment time was 25.176 seconds before and 3.634 seconds after (about 7x
  faster on this machine; actual report runtimes depend on size and hardware).
- Cached geography matched the previous implementation for 1,000 mixed rows.
  CVX classification caching left all histories/audits unchanged. An additional
  cohort test confirms that children outside ages 2-83 months are excluded from
  assessment while their administrations remain in the full-report product audit.
- The progress log records child counts and a final COMPLETE marker. Console
  progress was also checked through knitr with hidden messages.
- The included GIS and CVX workbooks are unchanged copies of the supplied files.

The American Samoa Patient Roster, Patient Details with Services, and
Reminder/Recall reports were not provided. Actual coverage estimates,
combination-vaccine frequencies, and local clinic aliases therefore remain to
be reviewed on the first real-data run. Synthetic records and their generated
outputs are not included in this delivery.
