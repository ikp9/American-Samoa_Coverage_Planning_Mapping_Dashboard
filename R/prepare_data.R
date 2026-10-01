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
      island_access_component=ifelse(island_atoll=="Unknown",50,ifelse(island_atoll!=config$primary_island,100,0)),
      community_patient_risk_component=0.60*median_individual_risk+0.20*p75_individual_risk+0.20*high_risk_percent)
  
  # Percentiles use each official/combined village once, even if clinic resolves
  # residents of a cross-county village into separate administrative rows.
  density<-community |> dplyr::filter(village!="Unknown") |>
    dplyr::distinct(island_atoll,village,population_density_km2,population) |>
    dplyr::mutate(density_percentile_component=percentile_100(log1p(population_density_km2)),
      population_size_component=percentile_100(log1p(population))) |>
    dplyr::select(island_atoll,village,density_percentile_component,population_size_component)
  community<-community |> dplyr::left_join(density,by=c("island_atoll","village")) |> dplyr::mutate(
    low_density_component_for_score=dplyr::coalesce(100-density_percentile_component,50),
    high_density_component_for_score=dplyr::coalesce(density_percentile_component,50),
    transmission_component=high_density_component_for_score*community_patient_risk_component/100,
    observed_burden_component=rescale_100(log1p(high_risk_n)),
   
     # Within the retained 4% burden weight, use both IIS high-risk child burden
    # and Census population size. The reference ranking counts each village once.
    burden_component=0.50*observed_burden_component+0.50*dplyr::coalesce(population_size_component,50),
    priority_score=round(0.90*community_patient_risk_component+0.01*island_access_component+
      0.01*low_density_component_for_score+0.04*transmission_component+0.04*burden_component,1),
    priority_rank=dplyr::min_rank(dplyr::desc(priority_score)))
  cuts<-stats::quantile(community$priority_score,c(1/3,2/3),na.rm=TRUE,names=FALSE)
  community<-community |> dplyr::mutate(
    priority_group=dplyr::case_when(utd_coverage<30 | mmr_coverage<50 ~ "High",priority_score>cuts[2] ~ "High",
      priority_score>cuts[1] ~ "Moderate",TRUE ~ "Low"),
    data_quality_flag=dplyr::case_when(village=="Unknown" ~ "Unknown village / county point",
      !county_resolved ~ "Multiple county/district candidates",is.na(latitude)|is.na(longitude) ~ "Missing coordinates",
      stringr::str_detect(geography_qa,"Review") ~ "Review locality match",is.na(population_density_km2) ~ "Missing population reference",
      TRUE ~ "Ready")) |> dplyr::arrange(priority_rank,island_atoll,district,county,village)
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
  message("American Samoa dashboard data preparation complete.")
  invisible(community)
}
