# American-Samoa_Coverage_Planning_Mapping_Dashboard
American Samoa vaccination coverage, planning, and mapping dashboard

# American Samoa Coverage Planning and Mapping Dashboard

This project adapts the child Quarto analysis and five-page Shiny dashboard for
American Samoa. The GIS/demographic and CVX workbooks are included. Patient reports
are not included. There are no precomputed American Samoa coverage estimates.

## Run the project

1. In the R console, run `source("R/install_packages.R")` once.
3. Put the three American Samoa Excel reports in `data/Raw Patient Data/`:
   Patient Roster, Patient Details with Services, and Reminder/Recall.
   Names beginning `Roster`, `Details`/`Detail`, and `RR` also work. The newest
   matching file is chosen by modification time. To select a specific report,
   set its path in `config.R`.
4. Set `analysis_date` in **config.R** to the assessment date. The supplied default
   is September 30, 2026. Review the product settings described below.
5. Open **RUN_ANALYSIS.R** and click **Source** in RStudio. The Console shows
   each stage and a child counter every 100 children. This produces the Excel
   tables and dashboard CSVs directly. Wait for **COMPLETE**.
   
Clicking Source in R/run_analysis.R only loads the function—it doesn’t start the analysis.
To start it now, enter this in the RStudio Console:
result <- run_as_analysis()

For future runs, open RUN_ANALYSIS.R in the main project folder, outside the R folder, and click Source. You should see “Starting American Samoa analysis” followed by progress messages.

6. Open **app.R** and click **Run App** to view the dashboard.

If you also want an HTML analysis report, render
`analysis/American_Samoa_Child_Coverage_Planning_Analysis.qmd` in RStudio/Quarto
as an alternative to step 5. That report runs the same analysis. Its optional
`age_as_of_date` parameter overrides config.R for that render. The Quarto bar
can remain at 18% while the run-analysis chunk is working; stage messages and
child counts show activity in the Render pane.

Progress is also written to
`data/Analytic Code Output/American_Samoa_Analysis_Progress.log`. It contains
stage names and aggregate counts. It is overwritten on each run.

Only **RUN_ANALYSIS.R** (or the Quarto file) starts the analysis. The files in
`R/` are loaded automatically. From the Console, the equivalent command is:

```r
source("RUN_ANALYSIS.R")
```

R 4.2 or later is required for the native pipe used by these files. Quarto is
required only to render the HTML analysis report. The pipeline can run entirely
from R without Quarto.

## Project files

| File or folder | Purpose |
|---|---|
| `RUN_ANALYSIS.R` | Single analysis entry point with live progress in the Console |
| `config.R` | Assessment date, input paths, clinic overrides, product preferences |
| `analysis/American_Samoa_Child_Coverage_Planning_Analysis.qmd` | Rendered analysis and QA report |
| `R/run_analysis.R` | Imports, joins, patient exports, summary workbooks, dashboard refresh |
| `R/as_functions.R` | Geography, report columns, CVX reference joins, product-use audit |
| `R/as_schedule.R` | Valid-dose evaluation, age/catch-up status, due-now and product flags |
| `R/as_tables.R` | Requested summary table columns and formatting |
| `R/prepare_data.R` | Dashboard summaries and CSVs |
| `app.R`, `www/styles.css` | Shiny dashboard and retained visual styling |
| `data/Raw Patient Data/` | Three local input reports |
| `data/Analytic Code Output/` | Generated patient files, eight-table workbook, audit/QA files |
| `tests/` | Synthetic schedule and end-to-end checks |
| `.gitignore` | Protects raw reports and generated patient data while allowing code and public references |

## Geography rules

- **Reminder/Recall City** is the residence input. The roster City and raw County
  fields are not used to assign residence.
- IIS city spellings/localities map to the workbook's **Census Geography** names.
  Official village names, county, district, and island/atoll appear in the tables.
- When a village/county cannot be determined, roster clinic text can identify an
  official county. County-only inference leaves Village as **Unknown** and labels
  the map point as a county centroid. It never assigns a village from clinic location.
- If a village crosses counties/districts, a matching clinic county can resolve
  that ambiguity. Otherwise the workbook's combined labels are retained and
  flagged. A conflicting clinic does not override an unambiguous RR residence.
- Conflicting RR cities for the same child are flagged and treated as unresolved.
- For a clinic name without an explicit county, copy the header-only
  `data/clinic_county_overrides.example.csv` to `data/clinic_county_overrides.csv`
  and add locally verified `clinic_pattern,county` rows. Patterns are case-insensitive
  regular expressions against normalized clinic text; county must use the exact
  official name from the workbook. The override CSV is ignored by Git.
- Combined census designations such as **Leusoali'i + Maia** remain combined.
  They cannot be assigned to an individual village without additional residence data.
- Population/density use the supplied **2020 Census** statistics. Patient records
  and locality aliases never multiply those population figures. The IIS child
  cohort supplies coverage denominators; Census under-age-5 population is a
  planning reference and is not an age-2-83-month IIS denominator.

## Vaccine status

The code joins cleaned **Vaccination Code ID** values to the supplied **CVX Code**
column, including leading-zero and Excel-decimal normalization. It does not use
the CVX workbook's `internalID` as a vaccination code. If a report uses a local
code system instead of CVX, review **CVX Mapping QA** and supply a verified
crosswalk before relying on its coverage estimates. Missing/unmapped codes are
excluded and reported, rather than guessed from a vaccine name.

Recorded combination vaccines credit each tracked antigen. DT/Td/Tdap are not
counted as DTaP; PPSV23 and PCV21 are not credited by this AAP childhood PCV15/20
assessment. Bivalent,
monovalent, and unspecified OPV and fractional-dose IPV require separate review
and are not automatically credited as a complete IPV dose. Product-use auditing
retains other administered CVX products for review.

Assessment uses the **first routine age anchor** in the AAP schedule, matching
the CNMI approach. Being below that threshold within a recommended age range
does not by itself mean a child is clinically overdue.

| Vaccine | Assessment rule |
|---|---|
| DTaP | 1/2/3 doses beginning at 2/4/6 months; dose 4 beginning at 15 months; school-entry dose beginning at 48 months. Dose 5 is unnecessary when valid dose 4 occurred at >=4 years and >=6 months after dose 3. |
| IPV | 1/2/3 doses beginning at 2/4/6 months; final dose at >=4 years and >=6 months after the previous dose. Three doses can complete the series when the third meets that final-dose rule. |
| MMR / Varicella | First dose at 12 months; second routine dose at 48 months. Earlier valid second doses can count, but the routine planner does not request dose 2 before 48 months. |
| Hib | PedvaxHIB-only infant series differs from PRP-T/Vaxelis/mixed/unknown series. Ages at prior doses determine catch-up completion; a first valid dose at >=15 months can complete catch-up. |
| PCV | Ages at previous valid doses determine catch-up. A first dose at >=24 months can complete the healthy-child catch-up series. |
| Hib / PCV at >=60 months | No routine healthy-child catch-up is generated. Vaccine-specific eligible denominators exclude these children; they remain in overall core coverage. |
| HepB | Second dose by the 2-month assessment anchor; third by the 6-month anchor. The final dose must meet minimum age 24 weeks and dose-1/dose-2 intervals. |
| HepA | First dose at 12 months; dose 2 after >=6 calendar months from dose 1. One valid dose can be age appropriate while waiting for dose 2. |
| Rotavirus | Rotarix-only histories need 2 doses; RotaTeq/mixed/unknown histories need 3. First dose must precede 15 weeks; no dose after age 8 months, 0 days. |

Duplicate patient/antigen/dates count once. Administrations after the assessment
date and doses failing minimum ages/intervals are excluded. Retrospective dose
validity allows the configurable four-day grace period; prospective due dates
use full intervals. Ages are assessed in calendar months. A same-day conflict
between Hib or rotavirus formulations is treated as unknown formulation.

**Core UTD** retains the measures: DTaP, IPV, MMR, Hib, HepB, PCV, varicella,
and HepA. Rotavirus has a separate coverage indicator. Seasonal influenza,
COVID-19, medical-risk schedules, immunity from disease, and contraindications
are not part of this core measure. The provided healthy-child catch-up job aids
do not resolve risk-based vaccine needs. RSV has no indicator, product, footnote,
or displayed audit row in the exported tables or dashboard.

The pipeline separates **not UTD** from **eligible for a dose today**. A child
can have an incomplete series while waiting for the next minimum interval.
IIS recommendations are retained in the private full dataset for reconciliation;
they do not bypass age, interval, product, or catch-up completion rules.

## Product review and stock planning

American Samoa patient reports were not supplied with this project. Consequently,
the most common combinations have **not yet been measured**. Each analysis run
creates **Product Use Audit** and **CVX Mapping QA** tabs/CSVs from the selected
Details report. The product audit counts distinct patient/date/CVX administrations,
not expanded antigen records, and distinguishes combination vaccines.

The product definitions add **Pentacel**, **Kinrix**, **Quadracel**, **ProQuad**, and
**Rotarix** to the prior planning options. CVX 130 identifies DTaP-IPV, but cannot
distinguish Kinrix from Quadracel. Hib, PCV, and pediatric HepA single-antigen
planning rows use generic inventory labels so an unsupported brand is not assumed.

After the first run, compare the audit with current supplies and edit config.R:

- `combination_preference` orders Vaxelis, Pediarix, and Pentacel. Remove any
  unavailable product. All component antigens must be eligible today; covered
  single-antigen doses are then removed, avoiding double-counting.
- Vaxelis is limited to eligible primary doses and is never selected as a Hib
  final booster. Pediarix is limited to the primary DTaP/IPV series; Pentacel is
  limited to children under 5 years and its approved dose positions.
- `school_entry_product` defaults to **None** (separate DTaP/IPV). Kinrix/Quadracel
  options require local review of previous brands and available inventory; CVX
  alone cannot establish every label-specific previous-brand condition.
- `allow_mmrv` defaults to FALSE. When enabled, ProQuad is selected only for
  children aged >=4 years with both MMR and varicella eligible.
- `default_rotavirus_product` is RotaTeq. Rotarix-only history takes precedence.
  Change the default for newly starting series to match available stock.

**Doses Needed** is one eligible administration per child/product for this visit,
not the total remaining doses across future visits. These are planning estimates;
the administering team confirms IIS forecasts, clinical eligibility, and stock.

## Requested tables

| Sheet | Columns |
|---|---|
| IIS Denominator | Island/Atoll, District, County, Children (n) |
| Reminder by Village | Island/Atoll, District, County, Village, Children on Reminder/Recall (n) |
| Needs by Vaccine | Island/Atoll, District, County, Village, Vaccine Type, Children Due (n) |
| Products Needed | Island/Atoll, District, County, Village, Vaccine Product, Doses Needed (n) |
| Dose Coverage | Island/Atoll, District, County, Age Group, Denominator, Children UTD (n), Children UTD (%) |
| UTD by Village | Island/Atoll, District, County, Village, Denominator, Children UTD (n), Children UTD (%) |
| Coverage 19-35 Months | Island/Atoll, District, County, Vaccine, Denominator, Children UTD (n), Children UTD (%) |
| Series 19-35 Months | Island/Atoll, District, County, Series, Denominator, Children UTD (n), Children UTD (%) |

Dose Coverage uses vaccine section titles to retain the exact requested seven
columns. The internal long table also carries Vaccine for calculation. UTD by
Village uses the full 2-83-month cohort, without an unrequested Age Group column.
Percentages are stored on a 0-100 scale and displayed with one decimal place.
An American Samoa total is clearly labeled; detail geography counts reconcile
to it. Zero eligible denominators have a blank percentage, not an artificial 0%.

The numeric **4:3:1:3:3:4** series is reported as a valid-dose-count measure. A
separately labeled **age-appropriate core series** accepts Hib/PCV catch-up with
fewer doses. This prevents calling a catch-up-adjusted metric a literal 4:3:1:3:3:4.

## Dashboard and refresh

Five pages retain the CNMI structure: Situation overview, Vaccination coverage,
Vaccination planning, Community risk & access, and Data quality & methods.
Island/Atoll, District, County, and Village selectors cascade, and age filters
apply to coverage/planning. The Situation overview card is labeled **Children
2 - 83 months**. Coverage maps use <65%, 65-<85%, and >=85% legend categories.
Maps use explicit OpenStreetMap tiles without an API key; circle markers do not
use the unsupported `highlightOptions` argument.

Priority scoring retains the CNMI 90/1/1/4/4 community-risk/access/density/
transmission/burden weighting. The 4% burden component splits equally between
observed IIS high-risk child burden and the Census village population-size
percentile. Off-Tutuila islands use an inter-island-access
proxy; census village density replaces CNMI region density. Each reference
village is ranked once for population/density, even when clinic data splits its
residents between counties. No-history children
receive the maximum recency-risk component. Missing geography/density is flagged
and uses a neutral missing-component score rather than silently zero risk.
Scores are relative planning measures, not validated outbreak probabilities.

For each refresh, update reports/date, rerun the pipeline, review QA, and restart
the app. The app uses computed CSVs and does not rewrite Hib/PCV status from raw
dose counts. To rebuild only dashboard summaries from the newest pseudonymized
export:

```r
source("config.R")
source("R/as_functions.R")
source("R/prepare_data.R")
prepare_as_dashboard()
```

## Git and deployment

The new project contains no Git history. Initialize a repository in this project
folder if needed. Code and the two public reference workbooks are allowed by
`.gitignore`. Raw reports, all analytic outputs, ID crosswalks, patient CSVs,
operational lists, and rendered analysis files are ignored by default.

The file named DeID is **pseudonymized**, not a claim of HIPAA de-identification.
It uses generated AS IDs and an explicit field allowlist. The separate private
crosswalk holds original IDs. Patient-level ages and small-area vaccine status
remain sensitive; generated files are not included in the delivered ZIP.

For an authorized Posit Connect deployment, bundle only `app.R`, `config.R`,
`R/as_functions.R`, `R/prepare_data.R`, `www/`, and the five generated dashboard
CSVs. Raw reports and patient Excel exports are not required. The dashboard
needs the patient CSV to apply age/geography filters, so access to a deployed
instance must match the approved use of those data. No CNMI deployment manifest,
account settings, or patient files have been copied into this project.

## Validation

```r
source("tests/test_schedule.R")
source("tests/test_pipeline.R")
```

The tests use synthetic records to check schedule boundaries, catch-up,
CVX exclusions, geography precedence/ambiguity, product double-counting,
denominator reconciliation, requested table columns, and identifier exclusion.
Real-report QA must confirm headers, CVX use, clinic aliases, vaccine histories,
cohort counts, and locally stocked products before operational use.

## Schedule references

- [AAP 2026 schedule, updated September 2, 2026](https://downloads.aap.org/AAP/PDF/AAP-Immunization-Schedule.pdf)
- [CDC PedvaxHIB-only catch-up, January 2025](https://www.cdc.gov/vaccines/hcp/imz-schedules/downloads/job-aids/hib-pedvax.pdf)
- [CDC ActHIB/Hiberix/Pentacel/Vaxelis/unknown Hib catch-up, January 2025](https://www.cdc.gov/vaccines/hcp/imz-schedules/downloads/job-aids/hib-acthib.pdf)
- [CDC pediatric PCV catch-up, January 2025](https://www.cdc.gov/vaccines/hcp/imz-schedules/downloads/job-aids/pneumococcal.pdf)
- [CDC Vaxelis guidance](https://www.cdc.gov/mmwr/volumes/69/wr/mm6905a5.htm)
- [CDC U.S. vaccine age/dose indications](https://www.cdc.gov/vaccines/hcp/vaccines-us/)
- [FDA Quadracel indication](https://www.fda.gov/vaccines-blood-biologics/vaccines/quadracel)
- [FDA Pentacel prescribing information](https://www.fda.gov/media/74385/download)

The supplied GIS workbook's Sources & Methods tab documents census and locality
sources. Its original bytes have been retained.
