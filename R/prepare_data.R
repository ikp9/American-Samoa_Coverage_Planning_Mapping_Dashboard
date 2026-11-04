# Rebuild dashboard CSVs from the exported pseudonymized dataset without raw reports.
# From the project root: source("config.R"); source("R/as_functions.R");
# source("R/prepare_data.R"); prepare_as_dashboard()

safe_mean <- function(x) if(all(is.na(x)))NA_real_ else mean(x,na.rm=TRUE)
safe_median <- function(x) if(all(is.na(x)))NA_real_ else stats::median(x,na.rm=TRUE)
safe_quantile <- function(x,p) if(all(is.na(x)))NA_real_ else as.numeric(stats::quantile(x,p,na.rm=TRUE))
percentile_100 <- function(x) {
  good<-!is.na(x); out<-rep(NA_real_,length(x))
  if(!any(good))return(out)
  out[good]<-if(sum(good)==1 || length(unique(x[good]))==1)50 else 100*(rank(x[good],ties.method="average")-1)/(sum(good)-1)
  out
}
rescale_100 <- function(x) {
  good<-!is.na(x); out<-rep(NA_real_,length(x)); if(!any(good))return(out)
  limits<-range(x[good]); out[good]<-if(diff(limits)==0)50 else 100*(x[good]-limits[1])/diff(limits)
  out
}

rank_reviewed_communities <- function(community, downgraded) {
  # Rank reviewed categories first, then score. Reviewed small communities form
  # a separate bottom block within Moderate; ties share the minimum rank.
  community$priority_score_rank<-rank(-community$priority_score,ties.method="min",na.last="keep")
  community$priority_rank<-rep(NA_real_,nrow(community))
  offset<-0L
  for(level in c("High","Moderate","Low")) {
    for(bottom in if(level=="Moderate")c(FALSE,TRUE) else FALSE) {
      in_bottom<-!is.na(downgraded) & downgraded
      rows<-which(!is.na(community$priority_score) &
        as.character(community$priority_group)==level &
        (if(level=="Moderate")in_bottom==bottom else TRUE))
      community$priority_rank[rows]<-offset+rank(-community$priority_score[rows],ties.method="min")
      offset<-offset+length(rows)
    }
  }
  community
}

apply_community_priority_review <- function(community) {
  if (anyNA(community$child_population) || any(community$child_population < 0))
    stop("Community child populations must be non-missing and non-negative.")
  unknown_label <- function(x) {
    key<-toupper(trimws(as.character(x)))
    is.na(key) | key %in% c("", "NA", "N/A", "MISSING") |
      grepl("(^|[^A-Z])UNKNOWN([^A-Z]|$)",key)
  }
  unknown<-unknown_label(community$village) | unknown_label(community$county)
  base<-if("priority_group_before_review" %in% names(community))
    as.character(community$priority_group_before_review) else as.character(community$priority_group)
  small<-community$child_population <= 10
  review_required<-unknown & community$child_population > 30
  level<-base
  level[small & !is.na(base) & base=="High"]<-"Moderate"
  level[review_required]<-"High"
  community$priority_group_before_review<-base
  community$priority_group<-level
  community$small_denominator<-small
  community$data_quality_flag<-vapply(seq_len(nrow(community)),function(i) {
    flags<-trimws(strsplit(as.character(community$data_quality_flag[i]),";",fixed=TRUE)[[1]])
    flags<-flags[!is.na(flags) & nzchar(flags) & !flags %in% c("Ready","Small denominator","Data quality review required")]
    if(small[i])flags<-c(flags,"Small denominator")
    if(review_required[i])flags<-c(flags,"Data quality review required")
    if(length(flags))paste(unique(flags),collapse="; ") else "Ready"
  },character(1))
  rank_reviewed_communities(community,small & !is.na(base) & base=="High")
}

village_priority_export <- function(community) {
  community<-apply_community_priority_review(community)
  fields<-c("county","district","village","analysis_date","priority_rank","priority_group",
    "priority_score","child_population","utd_coverage","utd_no_mmr_coverage",
    "children_not_utd","proportion_not_utd","mmr_eligible_n","mmr_not_utd_n","mmr_coverage",
    "community_patient_risk_component","transmission_component","burden_component",
    "median_individual_risk","p75_individual_risk","high_risk_n","high_risk_percent",
    "population_year","population","area_km2","population_density_km2","density_percentile_component",
    "high_density_component_for_score","population_size_component","observed_burden_component")
  as.data.frame(community[,fields])
}

village_priority_guide <- function() {
  data.frame(Indicator=c("Row definition","Source","Snapshot",
    "priority_score","community_patient_risk_component","individual_risk_score",
    "high_risk_n / high_risk_percent","transmission_component","density_percentile_component",
    "high_density_component_for_score","burden_component","observed_burden_component",
    "population_size_component","Missing demographic references","priority_group",
    "Coverage units","Weights and contributions","Small denominator review","Priority ranking"),
    Definition=c("One dashboard village/county/district community per row; county splits are retained. Do not sum repeated Census references.",
    "Exported community summary from the IIS child cohort aged 2-83 months and 2020 Census village references.",
    "Scores, ranks, groups and component inputs are a snapshot as of analysis_date. Refresh the analysis to update them.",
    "Round(0.90 * community patient risk + 0.05 * transmission + 0.05 * burden, 1); scale 0-100.",
    "0.60 * median individual risk + 0.20 * 75th-percentile individual risk + 0.20 * high-risk percentage.",
    "20 points if non-MMR core series is not UTD + 55 if MMR is not UTD + 25 * min(months since last vaccination / 36, 1). No history receives 25 recency points. Age-ineligible antigens do not count as overdue.",
    "Count / percentage of cohort children with individual risk score >=50; child_population is the denominator.",
    "high_density_component_for_score * community_patient_risk_component / 100.",
    "0-100 percentile rank of log(1 + population density); average ranks for ties; each reference village counted once.",
    "Density percentile, or 50 when unavailable.",
    "0.50 * observed_burden_component + 0.50 * population_size_component (50 if population percentile is missing).",
    "log(1 + high_risk_n), min-max scaled to 0-100 across community rows; 50 if all counts are equal.",
    "0-100 percentile rank of log(1 + total Census population); average ranks for ties; 50 when all references are equal.",
    "Blank source values remain blank. Density and population inputs use a neutral score of 50 when missing.",
    "High if UTD coverage <30% or MMR coverage <50%, or score above upper tertile. Otherwise Moderate above lower tertile, else Low. Cutoffs use all community rows.",
    "Coverage and percentage fields are percentage points (0-100). MMR denominator includes eligible children only. UTD means up to date.",
    "Weights are 90%, 5%, 5%. Multiply each component by its weight and round the sum to one decimal for priority_score.",
    "All rows with 10 or fewer cohort children move from High to Moderate, including Unknown villages/counties. Unknown rows with more than 30 children are High and flagged for data-quality review. Other rows follow the score/coverage rules. Scores stay unchanged; review flags and original levels are retained in the dashboard CSV.",
    "High first, then Moderate, then Low. Villages reduced from High to Moderate for small denominators rank below other Moderate villages. Each block uses descending scores; ties share the minimum rank. The dashboard CSV retains the numerical-score-only rank."),
    check.names=FALSE)
}

write_village_priority_workbook <- function(community,path) {
  data<-village_priority_export(community)
  guide<-village_priority_guide()
  wb<-openxlsx::createWorkbook()
  date_line<-paste("Data as of",paste(sort(unique(as.character(community$analysis_date))),collapse="; "))
  header<-openxlsx::createStyle(fgFill="#003E73",fontColour="#FFFFFF",textDecoration="bold",wrapText=TRUE,valign="center")
  for(sheet in c("Village scores","Indicator guide")) {
    openxlsx::addWorksheet(wb,sheet,gridLines=FALSE)
    openxlsx::writeData(wb,sheet,date_line,startRow=1,colNames=FALSE)
    x<-if(sheet=="Village scores")data else guide
    openxlsx::writeDataTable(wb,sheet,x,startRow=3,tableStyle="TableStyleMedium2",headerStyle=header)
    openxlsx::setColWidths(wb,sheet,cols=seq_len(ncol(x)),widths=24)
    openxlsx::setRowHeights(wb,sheet,rows=3,heights=60)
    openxlsx::freezePane(wb,sheet,firstActiveRow=4,firstActiveCol=if(sheet=="Village scores")4 else 1)
  }
  counts<-c("priority_rank","child_population","children_not_utd","mmr_eligible_n","mmr_not_utd_n","high_risk_n","population_year","population")
  decimal_cols<-which(vapply(data,is.numeric,logical(1)) & !names(data) %in% counts)
  openxlsx::addStyle(wb,"Village scores",openxlsx::createStyle(numFmt="0.0"),
    rows=seq_len(nrow(data))+3,cols=decimal_cols,gridExpand=TRUE)
  openxlsx::addStyle(wb,"Village scores",openxlsx::createStyle(numFmt="yyyy-mm-dd"),
    rows=seq_len(nrow(data))+3,cols=match("analysis_date",names(data)),gridExpand=TRUE)
  openxlsx::setRowHeights(wb,"Village scores",rows=seq_len(nrow(data))+3,heights=36)
  openxlsx::setColWidths(wb,"Indicator guide",cols=1:2,widths=c(38,115))
  openxlsx::addStyle(wb,"Indicator guide",openxlsx::createStyle(wrapText=TRUE,valign="top"),rows=seq_len(nrow(guide))+3,cols=1:2,gridExpand=TRUE)
  openxlsx::setRowHeights(wb,"Indicator guide",rows=seq_len(nrow(guide))+3,heights=48)
  dir.create(dirname(path),recursive=TRUE,showWarnings=FALSE)
  openxlsx::saveWorkbook(wb,path,overwrite=TRUE)
  invisible(path)
}

prepare_as_dashboard <- function(patient=NULL,project_dir=here::here(),config=AS_CONFIG) {
  if(is.null(patient)) {
    file<-newest_matching_file(file.path(project_dir,"data","Analytic Code Output"),
      "^Dataset 2_American_Samoa_2-83 Mos_DeID_Full_Dataset_.*\\.xlsx$")
    patient<-readxl::read_excel(file)
  }
  if(!nrow(patient))stop("No patient records available for the dashboard.")
  # Enforce the interface even when rebuilding from an externally edited dataset.
  required<-c("patient_id","community_id","island_atoll","district","county","village","latitude","longitude",
    "age_months","agegroup","age_group_label","onreminder","utd","utd_no_mmr","individual_risk_score",
    "months_since_last_vax","population_year","population","population_under5","area_km2","population_density_km2",
    "metric_basis","geography_qa","geography_source","coordinate_basis","county_resolved","clinic_conflict",
    "analysis_date",unname(AS_VACCINE_NEED_MAP),unname(AS_COVERAGE_MAP),unname(AS_PRODUCT_MAP))
  missing<-setdiff(required,names(patient)); if(length(missing))stop("Dashboard dataset missing: ",paste(missing,collapse=", "))
  # Only listed fields enter a dashboard file. Dates of birth, contact details,
  # vaccination dates, raw residence strings, and source IDs never enter these CSVs.
  patient<-patient |> dplyr::select(dplyr::all_of(unique(required))) |> dplyr::mutate(
    patient_id=as.character(patient_id),region=island_atoll,
    analysis_date=as.Date(analysis_date),not_utd=as.integer(utd==0),
    mmr_not_utd=ifelse(is.na(mmr_utd),NA_integer_,as.integer(mmr_utd==0)),
    high_individual_risk=as.integer(individual_risk_score>=50))
  geo<-c("community_id","island_atoll","district","county","village","region")
  community<-patient |> dplyr::group_by(dplyr::across(dplyr::all_of(geo))) |> dplyr::summarise(
    population_year=first_nonmissing(population_year),population=first_nonmissing(population),
    population_under5=first_nonmissing(population_under5),area_km2=first_nonmissing(area_km2),
    population_density_km2=first_nonmissing(population_density_km2),metric_basis=first_nonmissing(metric_basis),
    
    # These are reference values for one geography, not sums over patients/aliases.
    child_population=dplyr::n_distinct(patient_id),children_not_utd=sum(not_utd==1),
    proportion_not_utd=100*safe_mean(not_utd),utd_coverage=100*safe_mean(utd),utd_no_mmr_coverage=100*safe_mean(utd_no_mmr),
    mmr_eligible_n=sum(!is.na(mmr_utd)),mmr_not_utd_n=sum(mmr_not_utd==1,na.rm=TRUE),mmr_coverage=100*safe_mean(mmr_utd),
    var_coverage=100*safe_mean(var_utd),hepa_coverage=100*safe_mean(hepa_utd),
    median_months_since_vax=safe_median(ifelse(not_utd==1,months_since_last_vax,NA_real_)),
    median_individual_risk=safe_median(individual_risk_score),p75_individual_risk=safe_quantile(individual_risk_score,0.75),
    high_risk_n=sum(high_individual_risk),high_risk_percent=100*safe_mean(high_individual_risk),
    latitude=first_nonmissing(latitude),longitude=first_nonmissing(longitude),
    geography_qa=paste(sort(unique(geography_qa)),collapse="; "),
    coordinate_basis=first_nonmissing(coordinate_basis),county_resolved=all(county_resolved),
    clinic_conflicts=sum(clinic_conflict %in% TRUE),analysis_date=max(analysis_date),.groups="drop") |>
    dplyr::mutate(
      community_patient_risk_component=0.60*median_individual_risk+0.20*p75_individual_risk+0.20*high_risk_percent)
  
  # Percentiles use each official/combined village once, even if clinic resolves
  # residents of a cross-county village into separate administrative rows.
  density<-community |> dplyr::filter(village!="Unknown") |>
    dplyr::distinct(island_atoll,village,population_density_km2,population) |>
    dplyr::mutate(density_percentile_component=percentile_100(log1p(population_density_km2)),
      population_size_component=percentile_100(log1p(population))) |>
    dplyr::select(island_atoll,village,density_percentile_component,population_size_component)
  community<-community |> dplyr::left_join(density,by=c("island_atoll","village")) |> dplyr::mutate(
    high_density_component_for_score=dplyr::coalesce(density_percentile_component,50),
    transmission_component=high_density_component_for_score*community_patient_risk_component/100,
    observed_burden_component=rescale_100(log1p(high_risk_n)),
   
     # Within the burden weight, use both IIS high-risk child burden
    # and Census population size. The reference ranking counts each village once.
    burden_component=0.50*observed_burden_component+0.50*dplyr::coalesce(population_size_component,50),
    # Weight patient risk, transmission, and burden at 90%, 5%, and 5%.
    priority_score=round((90*community_patient_risk_component+
      5*transmission_component+5*burden_component)/100,1),
    priority_rank=dplyr::min_rank(dplyr::desc(priority_score)))
  cuts<-stats::quantile(community$priority_score,c(1/3,2/3),na.rm=TRUE,names=FALSE)
  community<-community |> dplyr::mutate(
    priority_group=dplyr::case_when(utd_coverage<30 | mmr_coverage<50 ~ "High",priority_score>cuts[2] ~ "High",
      priority_score>cuts[1] ~ "Moderate",TRUE ~ "Low"),
    data_quality_flag=dplyr::case_when(village=="Unknown" ~ "Unknown village / county point",
      !county_resolved ~ "Multiple county/district candidates",is.na(latitude)|is.na(longitude) ~ "Missing coordinates",
      stringr::str_detect(geography_qa,"Review") ~ "Review locality match",is.na(population_density_km2) ~ "Missing population reference",
      TRUE ~ "Ready")) |> dplyr::arrange(priority_rank,island_atoll,district,county,village)
  community<-apply_community_priority_review(community) |> dplyr::arrange(priority_rank,island_atoll,district,county,village)
  long_needs<-function(map,item,value) {
    out<-patient |> dplyr::select(dplyr::all_of(c(geo,"agegroup","age_group_label",unname(map)))) |>
      tidyr::pivot_longer(dplyr::all_of(unname(map)),names_to="variable",values_to="needed") |>
      dplyr::mutate(item=names(map)[match(variable,unname(map))]) |>
      dplyr::group_by(dplyr::across(dplyr::all_of(c(geo,"agegroup","age_group_label"))),item) |>
      dplyr::summarise(n=sum(needed==1),.groups="drop")
    names(out)[names(out)=="item"]<-item; names(out)[names(out)=="n"]<-value
    out
  }
  quality<-community |> dplyr::transmute(island_atoll,district,county,village,region,children=child_population,
    latitude,longitude,coordinate_status=data_quality_flag,geography_qa,coordinate_basis,clinic_conflicts)
  data_dir<-file.path(project_dir,"data")
  for(name in c("community_summary","community_vaccine_needs","community_product_needs","patient_dashboard","qa_as_geography")) {
    x<-switch(name,community_summary=community,
      community_vaccine_needs=long_needs(AS_VACCINE_NEED_MAP,"vaccine","children_due"),
      community_product_needs=long_needs(AS_PRODUCT_MAP,"product","doses_needed"),patient_dashboard=patient,qa_as_geography=quality)
    readr::write_csv(x,file.path(data_dir,paste0(name,".csv")),na="")
  }
  score_path<-file.path(data_dir,"Analytic Code Output",paste0("American_Samoa_Village_Priority_Indicators_",
    format(max(community$analysis_date),"%m%d%y"),".xlsx"))
  write_village_priority_workbook(community,score_path)
  message("Village priority indicators: ",score_path)
  message("American Samoa dashboard data preparation complete.")
  invisible(community)
}
