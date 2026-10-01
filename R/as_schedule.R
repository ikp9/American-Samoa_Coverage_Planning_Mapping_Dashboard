# Routine age anchors: 2,4,6 months; DTaP booster at 15 months; MMR/VAR2 at 48 months.
# Status and due-today are distinct. Hib/PCV catch-up follows the linked CDC job aids.
# Healthy-child rules apply; medical risk, contraindications, live-vaccine spacing,
# maternal HepB status, and weight at birth require IIS/clinical review.

month_date <- function(date, months) {
  if(!length(date) || !length(months))return(as.Date(character()))
  lubridate::add_with_rollback(date, lubridate::period(months=months))
}
max_date <- function(...) {
  x <- c(...); x <- x[!is.na(x)]
  if (length(x)) as.Date(max(as.numeric(x)), origin="1970-01-01") else as.Date(NA)
}
age_months_at <- function(dob, date) {
  # Completed calendar months, equivalent to the previous lubridate calculation
  # for Date inputs. Compare days directly; month-end anniversaries roll to the
  # first of the following month, matching time_length(interval(...), "months").
  if(!length(dob) || !length(date))return(numeric())
  birth<-as.POSIXlt(as.Date(dob),tz="UTC")
  end<-as.POSIXlt(as.Date(date),tz="UTC")
  as.numeric(12L*(end$year-birth$year)+end$mon-birth$mon-(end$mday<birth$mday))
}
date_at_least <- function(date, earliest, grace=0) !is.na(date) && !is.na(earliest) && date >= earliest-grace

hib_pcv_state <- function(dates, cvx, dob, at, antigen, grace=0L) {
  n <- length(dates); ages <- age_months_at(dob, dates+grace)
  a <- age_months_at(dob, at); final <- FALSE
  if (n == 0) return(list(complete=FALSE, next_date=dob+42, final=a>=if(antigen=="hib")15 else 24))
  last <- tail(dates,1)
  if (antigen == "hib") {
    complete <- (ages[1]>=15) || (n>=2 && tail(ages,1)>=15) ||
      (n>=2 && ages[1]>=12) || (n>=3 && tail(ages,1)>=12)
    if (complete) return(list(complete=TRUE,next_date=as.Date(NA),final=FALSE))
    if (n==1) {
      delay <- if (ages[1]>=12)56 else 28
      next_date <- last+delay; final <- a>=15 || ages[1]>=12
    } else if (n==2) {
      omp_only <- all(cvx %in% c("49","51"))
      third_infant <- ages[1]<7 && tail(ages,1)<12 && !omp_only && a<12
      next_date <- if(third_infant) last+28 else max_date(month_date(dob,12),last+56)
      final <- !third_infant
    } else {
      next_date <- max_date(month_date(dob,12),last+56); final <- TRUE
    }
  } else {
    complete <- ages[1]>=24 || (n>=2 && tail(ages,1)>=24) ||
      (n>=2 && ages[1]>=12) || (n>=3 && tail(ages,1)>=12)
    if (complete) return(list(complete=TRUE,next_date=as.Date(NA),final=FALSE))
    if(n==1) {
      # CDC job aid: one dose before age 12m -> dose 2 after >=4w at age 12-23m;
      # at age >=24m a single final catch-up dose uses an >=8w interval.
      delay <- if(ages[1]>=12 || a>=24)56 else 28
      next_date <- last+delay; final <- ages[1]>=12 || a>=24
    } else if(n==2) {
      infant_third <- tail(ages,1)<7 && a<12
      next_date <- if(infant_third) last+28 else max_date(month_date(dob,12),last+56)
      final <- !infant_third
    } else {
      next_date <- max_date(month_date(dob,12),last+56); final <- TRUE
    }
  }
  list(complete=FALSE,next_date=next_date,final=final)
}

minimum_next_date <- function(antigen, dates, cvx, dob, candidate, retrospective=TRUE, grace=0L) {
  n <- length(dates)
  if (antigen %in% c("hib","pcv")) return(hib_pcv_state(dates,cvx,dob,candidate+(if(retrospective)grace else 0),antigen,grace)$next_date)
  if(n==0) return(if(antigen=="hepb")dob else if(antigen %in% c("mmr","var","hepa"))month_date(dob,12) else dob+42)
  last <- tail(dates,1)
  if(antigen=="dtap") {
    if(n<3) return(last+28)
    if(n==3) return(max_date(month_date(dob,12),month_date(last,if(retrospective)4 else 6)))
    return(max_date(month_date(dob,48),month_date(last,6)))
  }
  if(antigen=="ipv") {
    if(n==1) return(last+28)
    if(n==2 && candidate+(if(retrospective)grace else 0) < month_date(dob,48)) return(last+28)
    return(max_date(month_date(dob,48),month_date(last,6)))
  }
  if(antigen=="hepb") {
    if(n==1) return(last+28)
    return(max_date(dob+168,dates[1]+112,last+56))
  }
  if(antigen=="hepa") return(month_date(last,6))
  # Retrospective VAR dose 2 after >=4w is accepted; future doses use >=3 calendar months.
  if(antigen=="var") return(if(retrospective)last+28 else month_date(last,3))
  last+28
}

valid_as_doses <- function(events, antigen, dob, grace=4L) {
  events <- events[order(events$vaccination_date), , drop=FALSE]
  dates <- as.Date(character()); codes <- character(); invalid <- 0L
  for(i in seq_len(nrow(events))) {
    candidate <- events$vaccination_date[i]; code <- events$cvx[i]
    earliest <- minimum_next_date(antigen, dates, codes, dob, candidate, TRUE, grace)
    # Additional records after a completed Hib/PCV catch-up series do not create
    # a new series requirement or a false invalid-dose warning.
    if(antigen %in% c("hib","pcv") && is.na(earliest))next
    valid <- candidate>=dob && date_at_least(candidate,earliest,grace)
    if(antigen=="rota") {
      # Maximum ages have no grace period.
      valid <- valid && candidate<=month_date(dob,8) && (length(dates)>0 || candidate<dob+105)
    }
    if(valid) { dates<-c(dates,candidate); codes<-c(codes,code) } else invalid<-invalid+1L
  }
  list(dates=dates,cvx=codes,invalid=invalid)
}

as_antigen_status <- function(antigen, history, dob, at, grace=0L) {
  dates<-history$dates; codes<-history$cvx; n<-length(dates); a<-age_months_at(dob,at)
  ages<-age_months_at(dob,dates+grace)
  date_n<-function(k) if(n>=k)dates[k] else as.Date(NA)
  required<-switch(antigen,
    dtap=if(a<4)1L else if(a<6)2L else if(a<15)3L else if(a<48)4L else 5L,
    ipv=if(a<4)1L else if(a<6)2L else if(a<48)3L else 4L,
    hepb=if(a<6)2L else 3L,
    mmr=if(a<12)0L else if(a<48)1L else 2L,
    var=if(a<12)0L else if(a<48)1L else 2L,
    hepa=if(a<12)0L else if(n==1 && at<month_date(dates[1],6))1L else 2L,
    rota=if(a<4)1L else if(a<6)2L else if(n>0 && all(codes=="119"))2L else 3L,
    hib=if(a<4)1L else if(a<6)2L else 3L,
    pcv=if(a<4)1L else if(a<6)2L else 3L)
  eligible<-TRUE; complete<-FALSE; next_final<-FALSE
  if(antigen %in% c("hib","pcv")) {
    state<-hib_pcv_state(dates,codes,dob,at,antigen,grace); complete<-state$complete
    next_date<-state$next_date; next_final<-state$final
    eligible<-a<60
    if(a<12) {
      if(n>0 && ages[1]>=7) required<-min(required,2L)
      if(antigen=="hib" && n>=2 && all(codes[1:2] %in% c("49","51"))) required<-min(required,2L)
      if(antigen=="pcv" && n>=2 && ages[2]>=7) required<-min(required,2L)
      utd<-as.integer(n>=required)
      # Preserve routine infant age anchors when the child is currently UTD.
      if(n>=required && !complete) {
        anchor<-if(n==1)4 else if(n==2 && required==2 && a<6)6 else if(next_final)12 else 6
        if(antigen=="hib" && n==2 && all(codes[1:2] %in% c("49","51"))) anchor<-12
        next_date<-max_date(next_date,month_date(dob,anchor))
      }
    } else { utd<-as.integer(complete); required<-if(complete)n else n+1L }
    if(!eligible) {utd<-NA_integer_; next_date<-as.Date(NA)}
  } else {
    if(antigen=="dtap" && n>=4 && ages[4]>=48 && date_at_least(dates[4],month_date(dates[3],6),grace)) required<-4L
    if(antigen=="ipv" && n>=3 && ages[3]>=48 && date_at_least(dates[3],month_date(dates[2],6),grace)) required<-3L
    utd<-if(required==0)NA_integer_ else as.integer(n>=required)
    max_doses<-switch(antigen,dtap=5L,ipv=4L,hepb=3L,mmr=2L,var=2L,hepa=2L,
      rota=if(n>0 && all(codes=="119"))2L else 3L)
    complete<-n>=max_doses || (antigen %in% c("dtap","ipv") && a>=48 && n>=required)
    next_date<-if(complete)as.Date(NA) else minimum_next_date(antigen,dates,codes,dob,at,FALSE)
    if(!complete) {
      anchor<-switch(antigen,
        dtap=c(2,4,6,15,48)[min(n+1,5)], ipv=c(2,4,6,48)[min(n+1,4)],
        hepb=c(0,2,6)[min(n+1,3)], mmr=if(n==0)12 else 48,
        var=if(n==0)12 else 48, hepa=12,
        rota=c(2,4,6)[min(n+1,3)])
      next_date<-max_date(next_date,month_date(dob,anchor))
    }
    if(antigen=="rota") {
      eligible<-at<=month_date(dob,8) && (n>0 || at<dob+105)
      if(!eligible)next_date<-as.Date(NA)
    }
  }
  due<-eligible && !is.na(next_date) && at>=next_date
  list(count=n,utd=utd,required=required,complete=complete,need=as.integer(due),
    next_date=next_date,next_final=next_final,dates=dates,cvx=codes,invalid=history$invalid)
}

evaluate_as_patient <- function(events, dob, at, grace=4L) {
  out<-list()
  for(antigen in names(AS_CVX_CODES)) {
    e<-events[events$antigen==antigen,,drop=FALSE]
    status<-as_antigen_status(antigen,valid_as_doses(e,antigen,dob,grace),dob,at,grace)
    out[[paste0(antigen,"_count")]]<-status$count
    out[[paste0(antigen,"_utd")]]<-status$utd
    out[[paste0(antigen,"_required_doses")]]<-status$required
    out[[paste0("need_",antigen)]]<-status$need
    out[[paste0(antigen,"_next_due_date")]]<-status$next_date
    out[[paste0(antigen,"_next_final")]]<-as.integer(status$next_final)
    out[[paste0(antigen,"_invalid_doses")]]<-status$invalid
    for(k in 1:5) {
      out[[paste0(antigen,"_dose",k,"_date")]]<-if(status$count>=k)status$dates[k] else as.Date(NA)
      out[[paste0(antigen,k,"utd")]]<-as.integer(status$count>=k)
    }
    if(antigen=="rota") out$rota_all_rotarix<-as.integer(status$count>0 && all(status$cvx=="119"))
  }
  tibble::as_tibble(out)
}

add_as_utd_flags <- function(data, events, analysis_date, config=AS_CONFIG, progress=NULL) {
  data <- data |> dplyr::mutate(
    age_months=age_months_at(dob,analysis_date), age_years=floor(age_months/12),
    agegroup=as_age_group(age_months), age_group_label=as_age_label(age_months)
  ) |> dplyr::filter(!is.na(agegroup))
  if(!nrow(data))stop("No active children aged 2-83 months as of the analysis date.")
  split_events<-split(events,events$patient_id)
  empty<-events[FALSE,]
  # Match once rather than repeatedly searching a named list of patient IDs.
  event_indices<-match(data$patient_id,names(split_events))
  names(event_indices)<-data$patient_id
  n<-nrow(data); started<-proc.time()[["elapsed"]]
  indicators<-lapply(seq_len(n),function(i) {
    e<-if(is.na(event_indices[i]))empty else split_events[[event_indices[i]]]
    result<-evaluate_as_patient(e,data$dob[i],analysis_date,config$grace_days)
    if(is.function(progress) && (i==1L || i%%100L==0L || i==n)) {
      elapsed<-proc.time()[["elapsed"]]-started
      remaining<-if(i>=100L) sprintf("; approximately %.1f minutes remaining in this step",elapsed*(n-i)/i/60) else ""
      progress(sprintf("Children assessed: %s / %s (%.1f%%)%s",format(i,big.mark=","),format(n,big.mark=","),100*i/n,remaining))
    }
    result
  }) |> dplyr::bind_rows()
  data<-dplyr::bind_cols(data,indicators)
  data$last_vaccination_date<-as.Date(vapply(event_indices,function(j) {
    if(is.na(j))return(NA_real_)
    e<-split_events[[j]]
    if(!nrow(e))return(NA_real_)
    as.numeric(max(e$vaccination_date))
  },numeric(1)),origin="1970-01-01")
  data |> dplyr::mutate(
    onreminder=dplyr::coalesce(as.integer(onreminder),0L),
    dtap54utd=ifelse(age_months>=48,dtap_utd,as.integer(dtap_count>=5)),
    ipv43utd=ifelse(age_months>=48,ipv_utd,as.integer(ipv_count>=4)),
    days_since_last_vax=as.numeric(analysis_date-last_vaccination_date),
    months_since_last_vax=pmin(days_since_last_vax/30.4375,36),
    # Core series retains the CNMI definition: DTaP/IPV/MMR/Hib/HepB/PCV/VAR/HepA.
    # Rotavirus is reported separately; healthy-child Hib/PCV eligibility ends at 60m.
    utd_no_mmr=as.integer(dplyr::coalesce(dtap_utd,1L)==1 & dplyr::coalesce(ipv_utd,1L)==1 &
      dplyr::coalesce(hib_utd,1L)==1 & dplyr::coalesce(hepb_utd,1L)==1 & dplyr::coalesce(pcv_utd,1L)==1 &
      dplyr::coalesce(var_utd,1L)==1 & dplyr::coalesce(hepa_utd,1L)==1),
    utd=as.integer(utd_no_mmr==1 & dplyr::coalesce(mmr_utd,1L)==1),
    # No recorded vaccination history receives the maximum recency component.
    t_i_tilde=dplyr::coalesce(months_since_last_vax/36,1),
    individual_risk_score=100*(0.20*(utd_no_mmr==0)+0.55*(dplyr::coalesce(mmr_utd,1L)==0)+0.25*t_i_tilde),
    analysis_date=analysis_date
  )
}

as_product_plan <- function(row, config=AS_CONFIG) {
  need<-setNames(as.logical(unlist(row[paste0("need_",names(AS_CVX_CODES))])),names(AS_CVX_CODES))
  products<-setNames(rep(0L,length(AS_PRODUCT_MAP)),unname(AS_PRODUCT_MAP))
  nextdose<-function(a) as.integer(row[[paste0(a,"_count")]])+1L
  use_combo<-function(product,antigens) {
    products[[product]]<<-1L; need[antigens]<<-FALSE
  }
  a<-as.numeric(row$age_months)
  for(product in config$combination_preference) {
    if(product=="Vaxelis" && a<60 && all(need[c("dtap","ipv","hib","hepb")]) &&
       nextdose("dtap")<=3 && nextdose("ipv")<=3 && nextdose("hepb")<=3 &&
       (row$hib_next_final==0 || row$hib_count==0))
      use_combo(product,c("dtap","ipv","hib","hepb"))
    if(product=="Pediarix" && a<84 && all(need[c("dtap","ipv","hepb")]) && nextdose("dtap")<=3 && nextdose("ipv")<=3)
      use_combo(product,c("dtap","ipv","hepb"))
    if(product=="Pentacel" && a<60 && all(need[c("dtap","ipv","hib")]) && nextdose("dtap")<=4 && nextdose("ipv")<=4)
      use_combo(product,c("dtap","ipv","hib"))
  }
  if(a>=48 && a<84 && all(need[c("dtap","ipv")]) && nextdose("dtap")==5 && nextdose("ipv")==4 &&
      config$school_entry_product %in% c("Kinrix","Quadracel")) use_combo(config$school_entry_product,c("dtap","ipv"))
  if(isTRUE(config$allow_mmrv) && a>=48 && all(need[c("mmr","var")])) use_combo("ProQuad",c("mmr","var"))
  single<-c(dtap="DTaP_Single",ipv="IPV_Single",mmr="MMR_Single",hib="Hib_Single",hepb="HepB_Single",
    pcv="PCV_Product",var="Varivax",hepa="HepA_Product")
  for(antigen in names(single))products[[single[[antigen]]]]<-as.integer(need[[antigen]])
  if(need[["rota"]]) products[[if(row$rota_all_rotarix==1)"Rotarix" else config$default_rotavirus_product]]<-1L
  tibble::as_tibble(as.list(products))
}

add_as_need_and_product_flags <- function(data, config=AS_CONFIG) {
  if(any(!config$combination_preference %in% c("Vaxelis","Pediarix","Pentacel")))stop("Invalid combination preference.")
  if(!config$default_rotavirus_product %in% c("RotaTeq","Rotarix"))stop("Invalid rotavirus product.")
  plans<-lapply(seq_len(nrow(data)),function(i)as_product_plan(data[i,],config)) |> dplyr::bind_rows()
  data<-dplyr::bind_cols(data,plans)
  # Keep IIS recommendations for private reconciliation; they do not override age/interval rules.
  patterns<-c(dtap="DTAP",ipv="POLIO|IPV",mmr="MMR|MEASLES",hib="HIB",hepb="HEP ?B|HEPATITIS B",
    pcv="PCV|PREVNAR|PNEUMO|VAXNEUVANCE",rota="ROTA",var="VARICELLA|VARIVAX|MMRV",hepa="HEP ?A|HEPATITIS A")
  text<-dplyr::coalesce(data$immunization_recommendations,"")
  for(a in names(patterns))data[[paste0("iis_recommends_",a)]]<-as.integer(stringr::str_detect(text,stringr::regex(patterns[[a]],ignore_case=TRUE)))
  data
}
