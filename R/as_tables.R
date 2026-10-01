# Requested table layouts. Age-appropriate coverage retains vaccine sections;
# Dose Coverage is a territory-wide matrix of individual doses by age group.

AS_COVERAGE_19_35_MAP <- c(
  `DTaP (4 doses)`="dtap_utd",`IPV (3 doses)`="ipv_utd",`MMR (1 dose)`="mmr_utd",
  `Hib (3-4 doses or completed catch-up)`="hib_utd",`HepB (3 doses)`="hepb_utd",
  `PCV (4 doses or completed catch-up)`="pcv_utd",
  `Rotavirus (2-3 doses, product-specific)`="rota_utd",`Varicella (1 dose)`="var_utd",
  `HepA (1-2 doses; dose 2 when due)`="hepa_utd"
)

build_as_dose_coverage <- function(patient) {
  positions<-c(dtap=5L,ipv=4L,mmr=2L,hib=4L,hepb=3L,pcv=4L,rota=3L,var=2L,hepa=2L)
  vaccine_names<-c(dtap="DTaP",ipv="IPV",mmr="MMR",hib="Hib",hepb="HepB",pcv="PCV",
    rota="Rotavirus",var="Varicella",hepa="HepA")
  variables<-unlist(lapply(names(positions),function(a)paste0(a,seq_len(positions[[a]]),"utd")),use.names=FALSE)
  labels<-unlist(lapply(names(positions),function(a)paste(vaccine_names[[a]],seq_len(positions[[a]]))),use.names=FALSE)
  denominators<-as.integer(table(factor(patient$age_group_label,levels=AS_AGE_LEVELS)))
  names(denominators)<-AS_AGE_LEVELS
  observed<-patient |> dplyr::select(age_group_label,dplyr::all_of(variables)) |>
    tidyr::pivot_longer(dplyr::all_of(variables),names_to="variable",values_to="has_dose") |>
    dplyr::group_by(age_group_label,variable) |>
    dplyr::summarise(coverage=100*sum(has_dose==1,na.rm=TRUE)/dplyr::n(),.groups="drop")
  values<-matrix(NA_real_,nrow=length(variables),ncol=length(AS_AGE_LEVELS),
    dimnames=list(NULL,AS_AGE_LEVELS))
  if(nrow(observed))values[cbind(match(observed$variable,variables),
    match(observed$age_group_label,AS_AGE_LEVELS))]<-observed$coverage
  out<-dplyr::bind_cols(tibble::tibble(`Vaccine Dose`=labels),tibble::as_tibble(values))
  attr(out,"age_denominators")<-denominators
  out
}

as_total <- function(x, village=FALSE) {
  x$island_atoll<-"American Samoa Total"; x$district<-"All Districts"; x$county<-"All Counties"
  if(village)x$village<-"All Villages"
  x
}
table_geography <- function(x, village=FALSE) {
  x<-x |> dplyr::rename(`Island/Atoll`=island_atoll,District=district,County=county)
  if(village)x<-x |> dplyr::rename(Village=village)
  x
}
build_as_tables <- function(patient) {
  county_geo<-c("island_atoll","district","county")
  village_geo<-c(county_geo,"village")
  sort_table<-function(x) {
    x |> dplyr::mutate(.total=ifelse(island_atoll=="American Samoa Total",0L,1L)) |>
      dplyr::arrange(.total,island_atoll,district,county) |> dplyr::select(-.total) |>
      dplyr::relocate(dplyr::all_of(county_geo),dplyr::any_of("village"))
  }
  denominator<-patient |> dplyr::count(dplyr::across(dplyr::all_of(county_geo)),name="Children (n)")
  total<-tibble::tibble(`Children (n)`=nrow(patient)) |> as_total()
  denominator<-dplyr::bind_rows(total,denominator) |> sort_table() |> table_geography()
  reminder<-patient |> dplyr::filter(onreminder==1) |>
    dplyr::count(dplyr::across(dplyr::all_of(village_geo)),name="Children on Reminder/Recall (n)")
  total<-tibble::tibble(`Children on Reminder/Recall (n)`=sum(patient$onreminder==1)) |> as_total(TRUE)
  reminder<-dplyr::bind_rows(total,reminder) |> sort_table() |> table_geography(TRUE)
  needs<-function(map,label,value_label) {
    long<-patient |> dplyr::select(dplyr::all_of(c(village_geo,unname(map)))) |>
      tidyr::pivot_longer(dplyr::all_of(unname(map)),names_to="variable",values_to="needed") |>
      dplyr::mutate(item=names(map)[match(variable,unname(map))])
    detail<-long |> dplyr::group_by(dplyr::across(dplyr::all_of(village_geo)),item) |>
      dplyr::summarise(n=sum(needed==1,na.rm=TRUE),.groups="drop")
    total<-long |> dplyr::group_by(item) |> dplyr::summarise(n=sum(needed==1,na.rm=TRUE),.groups="drop") |> as_total(TRUE)
    out<-dplyr::bind_rows(total,detail) |> sort_table() |> table_geography(TRUE)
    names(out)[names(out)=="item"]<-label; names(out)[names(out)=="n"]<-value_label
    out |> dplyr::select(dplyr::all_of(c("Island/Atoll","District","County","Village",label,value_label)))
  }
  coverage<-function(data,map,grouping,item_label) {
    long<-data |> dplyr::select(dplyr::all_of(c(county_geo,grouping,unname(map)))) |>
      tidyr::pivot_longer(dplyr::all_of(unname(map)),names_to="variable",values_to="utd_value") |>
      dplyr::mutate(item=names(map)[match(variable,unname(map))])
    summarize<-function(x,groups) x |> dplyr::group_by(dplyr::across(dplyr::all_of(groups))) |>
      dplyr::summarise(Denominator=sum(!is.na(utd_value)),`Children UTD (n)`=sum(utd_value==1,na.rm=TRUE),.groups="drop") |>
      dplyr::mutate(`Children UTD (%)`=100*`Children UTD (n)`/dplyr::na_if(Denominator,0))
    detail<-summarize(long,c(county_geo,grouping,"item"))
    total<-summarize(long,c(grouping,"item")) |> as_total()
    if(!nrow(total) && !length(grouping)) total<-tibble::tibble(
      item=names(map),Denominator=0L,`Children UTD (n)`=0L,`Children UTD (%)`=NA_real_) |> as_total()
    out<-dplyr::bind_rows(total,detail) |> sort_table() |> table_geography()
    if(length(grouping))out<-out |> dplyr::mutate(age_group_label=factor(age_group_label,levels=AS_AGE_LEVELS)) |>
      dplyr::arrange(`Island/Atoll`,District,County,age_group_label) |>
      dplyr::mutate(age_group_label=as.character(age_group_label)) |> dplyr::rename(`Age Group`=age_group_label)
    names(out)[names(out)=="item"]<-item_label
    out |> dplyr::select(dplyr::all_of(c("Island/Atoll","District","County",if(length(grouping))"Age Group",item_label,
      "Denominator","Children UTD (n)","Children UTD (%)")))
  }
  county_vaccine<-coverage(patient,AS_COVERAGE_MAP,"age_group_label","Vaccine")
  utd_detail<-patient |> dplyr::group_by(dplyr::across(dplyr::all_of(village_geo))) |>
    dplyr::summarise(Denominator=dplyr::n(),`Children UTD (n)`=sum(utd==1),.groups="drop")
  utd_total<-tibble::tibble(Denominator=nrow(patient),`Children UTD (n)`=sum(patient$utd==1)) |> as_total(TRUE)
  utd<-dplyr::bind_rows(utd_total,utd_detail) |>
    dplyr::mutate(`Children UTD (%)`=100*`Children UTD (n)`/dplyr::na_if(Denominator,0)) |>
    sort_table() |> table_geography(TRUE)
  children_19_35<-patient |> dplyr::filter(dplyr::between(age_months,19,35)) |>
    dplyr::mutate(
      series_431334=as.integer(dtap4utd==1 & ipv3utd==1 & mmr1utd==1 & hib3utd==1 & hepb3utd==1 & pcv4utd==1),
      series_extended=as.integer(series_431334==1 & var1utd==1 & hepa_utd==1)
    )
  series_map<-c(`4:3:1:3:3:4 (valid dose counts)`="series_431334",
    `4:3:1:3:3:4 + Varicella + age-appropriate HepA`="series_extended",
    `Age-appropriate core series (Hib/PCV catch-up accepted)`="utd")
  list(
    `IIS Denominator`=denominator,
    `Reminder by Village`=reminder,
    `Needs by Vaccine`=needs(AS_VACCINE_NEED_MAP,"Vaccine Type","Children Due (n)"),
    `Products Needed`=needs(AS_PRODUCT_MAP,"Vaccine Product","Doses Needed (n)"),
    `UTD by county and vaccine`=county_vaccine,
    `Dose Coverage`=build_as_dose_coverage(patient),
    `UTD by Village`=utd,
    `Coverage 19-35 Months`=coverage(children_19_35,AS_COVERAGE_19_35_MAP,character(),"Vaccine"),
    `Series 19-35 Months`=coverage(children_19_35,series_map,character(),"Series")
  )
}

AS_SCHEDULE_NOTES <- c(
  "Assessment uses the first routine age anchor; an incomplete catch-up series can be not UTD while the next dose is not yet eligible.",
  "DTaP dose 4 begins at 15 months. MMR and varicella dose 2 begin at 48 months. HepA dose 2 requires >=6 calendar months after dose 1.",
  "Hib and PCV accept age/product-specific catch-up. Healthy children aged >=60 months are excluded from Hib/PCV eligible denominators; no routine catch-up is generated.",
  "Rotarix is a 2-dose series; RotaTeq/mixed/unknown is a 3-dose series. No first dose at >=15 weeks; no doses after age 8 months, 0 days.",
  "Core UTD retains the CNMI eight-antigen definition (DTaP, IPV, MMR, Hib, HepB, PCV, Varicella, HepA). Rotavirus is a separate indicator. Influenza/COVID-19 and risk-specific schedules are not in core UTD.",
  "Combined county/district labels preserve unresolved source geography. Totals include each child once. Census population values must not be summed across locality aliases.",
  "AAP schedule (updated September 2, 2026): https://downloads.aap.org/AAP/PDF/AAP-Immunization-Schedule.pdf",
  "CDC Hib/PCV catch-up job aids (January 2025): https://www.cdc.gov/vaccines/hcp/imz-schedules/downloads/job-aids/hib-pedvax.pdf ; hib-acthib.pdf ; pneumococcal.pdf"
)

write_as_table_workbook <- function(tables,path,analysis_date,product_audit=NULL,cvx_issues=NULL,run_qa=NULL) {
  wb<-openxlsx::createWorkbook()
  title_style<-openxlsx::createStyle(fontSize=13,textDecoration="bold",fontColour="#FFFFFF",fgFill="#0056A6",wrapText=TRUE,halign="left")
  header_style<-openxlsx::createStyle(textDecoration="bold",fontColour="#FFFFFF",fgFill="#003E73",wrapText=TRUE,halign="center",valign="center")
  body_style<-openxlsx::createStyle(fontSize=10,halign="left",valign="center",wrapText=TRUE)
  count_style<-openxlsx::createStyle(numFmt="#,##0",halign="center")
  percent_style<-openxlsx::createStyle(numFmt="0.0",halign="center")
  note_style<-openxlsx::createStyle(fontSize=9,fontColour="#495057",wrapText=TRUE)
  table_number<-0L
  block<-function(sheet,data,row,title,notes=character(),body_height=30,
      percentage_columns=which(grepl("%",names(data),fixed=TRUE)),header_height=42) {
    nc<-ncol(data); nr<-nrow(data)
    openxlsx::writeData(wb,sheet,title,startRow=row,colNames=FALSE)
    openxlsx::mergeCells(wb,sheet,cols=seq_len(nc),rows=row)
    openxlsx::addStyle(wb,sheet,title_style,rows=row,cols=seq_len(nc),gridExpand=TRUE)
    openxlsx::setRowHeights(wb,sheet,rows=row,heights=36)
    table_number<<-table_number+1L
    # Native tables allow each vaccine section to filter independently. A single
    # worksheet filter would mix the merged section titles with its data rows.
    openxlsx::writeDataTable(wb,sheet,data,startRow=row+2,withFilter=TRUE,
      tableName=paste0("AS_Table_",table_number),tableStyle="none",bandedRows=FALSE)
    openxlsx::addStyle(wb,sheet,header_style,rows=row+2,cols=seq_len(nc),gridExpand=TRUE)
    openxlsx::setRowHeights(wb,sheet,rows=row+2,heights=header_height)
    if(nr>0) {
      rows<-seq.int(row+3,row+2+nr)
      openxlsx::addStyle(wb,sheet,body_style,rows=rows,cols=seq_len(nc),gridExpand=TRUE)
      openxlsx::setRowHeights(wb,sheet,rows=rows,heights=body_height)
      numbers<-which(vapply(data,is.numeric,logical(1)))
      if(length(numbers))openxlsx::addStyle(wb,sheet,count_style,rows=rows,cols=numbers,gridExpand=TRUE,stack=TRUE)
      pct<-percentage_columns
      if(length(pct))openxlsx::addStyle(wb,sheet,percent_style,rows=rows,cols=pct,gridExpand=TRUE,stack=TRUE)
    }
    r<-row+nr+5
    for(note in notes) {
      openxlsx::writeData(wb,sheet,note,startRow=r,colNames=FALSE)
      openxlsx::mergeCells(wb,sheet,cols=seq_len(nc),rows=r)
      openxlsx::addStyle(wb,sheet,note_style,rows=r,cols=seq_len(nc),gridExpand=TRUE)
      openxlsx::setRowHeights(wb,sheet,rows=r,heights=30); r<-r+1
    }
    r+2
  }
  for(sheet in names(tables)) {
    openxlsx::addWorksheet(wb,sheet,gridLines=FALSE)
    data<-tables[[sheet]]
    note_indices<-switch(sheet,
      `IIS Denominator`=6L,`Reminder by Village`=6L,
      `Needs by Vaccine`=c(1L,3L,4L,6L,7L,8L),
      `Products Needed`=c(1L,6L,7L,8L),
      `UTD by Village`=c(2L,3L,5L,6L,7L,8L),
      `Coverage 19-35 Months`=c(2L,3L,4L,6L,7L,8L),
      `Series 19-35 Months`=c(3L,5L,6L,7L,8L),
      seq_along(AS_SCHEDULE_NOTES))
    notes<-AS_SCHEDULE_NOTES[note_indices]
    if(sheet=="Dose Coverage")notes<-c(
      "Each cell is the percentage of children in that age group with at least the indicated number of valid vaccine doses. Column headers show denominators (n).",
      "Blank cells indicate no children in that age group. These are individual-dose counts; age-appropriate completion and catch-up exceptions are on UTD by county and vaccine.",
      "Rotarix requires 2 doses; other/mixed rotavirus histories require 3. Hib/PCV completed catch-up can use fewer doses than the routine infant series.",
      AS_SCHEDULE_NOTES[c(7L,8L)])
    if(sheet %in% c("IIS Denominator","Reminder by Village"))notes<-c(
      "The IIS cohort is active roster children aged 2-83 months on the assessment date. Reminder/Recall counts are a subset of that cohort.",notes)
    if(sheet=="Products Needed")notes<-c("Doses Needed is one eligible administration per child/product for the current visit, not all future doses needed to complete the series. Product preferences are in config.R; clinical and inventory review precede administration.",notes)
    if(sheet=="Series 19-35 Months")notes<-c("The numeric 4:3:1:3:3:4 series is a valid-dose count measure. The separately labeled age-appropriate core series accepts Hib/PCV catch-up with fewer doses.",notes)
    if(sheet=="UTD by county and vaccine") {
      nc<-ncol(data)-1L
      pinned_header<-names(data)[names(data)!="Vaccine"]
      openxlsx::writeData(wb,sheet,"Age-appropriate coverage by vaccine type, age group and county",startRow=1,colNames=FALSE)
      openxlsx::mergeCells(wb,sheet,cols=seq_len(nc),rows=1)
      openxlsx::addStyle(wb,sheet,title_style,rows=1,cols=seq_len(nc),gridExpand=TRUE)
      openxlsx::setRowHeights(wb,sheet,rows=1,heights=36)
      openxlsx::writeData(wb,sheet,matrix(pinned_header,nrow=1),startRow=2,colNames=FALSE)
      openxlsx::addStyle(wb,sheet,header_style,rows=2,cols=seq_len(nc),gridExpand=TRUE)
      openxlsx::setRowHeights(wb,sheet,rows=2,heights=42)
      r<-3
      for(vaccine in names(AS_COVERAGE_MAP)) {
        if(r>3)openxlsx::pageBreak(wb,sheet,i=r-1L)
        part<-data[data$Vaccine==vaccine,setdiff(names(data),"Vaccine"),drop=FALSE]
        r<-block(sheet,part,r,paste("American Samoa -",vaccine,"age-appropriate coverage -",format(analysis_date)),
          if(vaccine==tail(names(AS_COVERAGE_MAP),1))notes else character())
      }
    } else if(sheet=="Dose Coverage") {
      nc<-ncol(data)
      denominators<-attr(data,"age_denominators")
      names(data)[-1]<-paste0(AS_AGE_LEVELS,"\n(n = ",format(denominators,big.mark=",",trim=TRUE),")")
      block(sheet,data,1,paste("American Samoa - Dose coverage by age group (%) -",format(analysis_date)),
        notes,percentage_columns=2:nc,header_height=60)
    } else {
      block(sheet,data,1,paste("American Samoa -",sheet,"-",format(analysis_date)),notes); nc<-ncol(data)
    }
    openxlsx::setColWidths(wb,sheet,cols=seq_len(nc),widths=24)
    if(sheet %in% c("Needs by Vaccine","Products Needed","Series 19-35 Months"))openxlsx::setColWidths(wb,sheet,cols=if(sheet=="Series 19-35 Months")4 else 5,widths=48)
    if(sheet=="Coverage 19-35 Months")openxlsx::setColWidths(wb,sheet,cols=4,widths=44)
    if(sheet=="Dose Coverage") {
      openxlsx::setColWidths(wb,sheet,cols=2:nc,widths=14)
      openxlsx::setColWidths(wb,sheet,cols=1,widths=24)
      openxlsx::freezePane(wb,sheet,firstActiveRow=4,firstActiveCol=2)
    } else if(sheet=="UTD by county and vaccine") {
      openxlsx::freezePane(wb,sheet,firstActiveRow=3)
    } else openxlsx::freezePane(wb,sheet,firstActiveRow=4,firstActiveCol=4)
    openxlsx::pageSetup(wb,sheet,orientation="landscape",fitToWidth=1,fitToHeight=0,
      printTitleRows=if(sheet=="UTD by county and vaccine")1:2 else 1:3)
  }
  for(item in list(list("Product Use Audit",product_audit),list("CVX Mapping QA",cvx_issues),list("Run QA",run_qa))) {
    if(is.null(item[[2]]))next
    audit_data<-item[[2]]
    if(item[[1]]=="Product Use Audit") {
      audit_data$combination<-ifelse(audit_data$combination,"Yes","No")
      names(audit_data)[1:4]<-c("CVX Code","CVX Description","Vaccine Product / Group","Combination Vaccine")
    }
    if(item[[1]]=="CVX Mapping QA")names(audit_data)[1:3]<-c("CVX Code","CVX Description","Issue")
    openxlsx::addWorksheet(wb,item[[1]],gridLines=FALSE)
    block(item[[1]],audit_data,1,paste("American Samoa -",item[[1]],"-",format(analysis_date)),
      body_height=if(item[[1]]=="Product Use Audit")48 else 36)
    widths<-switch(item[[1]],`Product Use Audit`=c(10,38,44,16,20,24),
      `CVX Mapping QA`=c(12,42,52,20),`Run QA`=c(75,20))
    openxlsx::setColWidths(wb,item[[1]],cols=seq_len(ncol(audit_data)),widths=widths)
    openxlsx::freezePane(wb,item[[1]],firstActiveRow=4)
    openxlsx::pageSetup(wb,item[[1]],orientation="landscape",fitToWidth=1,fitToHeight=0,printTitleRows=1:3)
  }
  openxlsx::saveWorkbook(wb,path,overwrite=TRUE)
  invisible(path)
}
