# Run from the project root: Rscript tests/test_schedule.R
# Synthetic records only; no patient reports required.
source("config.R")
source("R/as_functions.R")
source("R/as_schedule.R")
source("R/as_tables.R")
checks<-0L
check<-function(condition,label) {
  if(!isTRUE(condition))stop("FAILED: ",label,call.=FALSE)
  checks<<-checks+1L
}
dob<-as.Date("2020-01-01")
events_for<-function(antigen,months,cvx) {
  tibble::tibble(antigen=antigen,vaccination_date=month_date(dob,months),cvx=rep(cvx,length.out=length(months)))
}
status<-function(antigen,months,codes,age,grace=0L) {
  e<-events_for(antigen,months,codes)
  as_antigen_status(antigen,valid_as_doses(e,antigen,dob,grace),dob,month_date(dob,age))
}

check(status("dtap",c(2,4,6),"20",12)$utd==1,"DTaP dose 4 not required at 12 months")
check(status("dtap",c(2,4,6),"20",15)$need==1,"DTaP dose 4 due at 15 months")
check(status("dtap",c(2,4,6,48),"20",50)$utd==1,"Valid DTaP4 at 4 years can finish series")
check(status("dtap",c(2,4,6,15),"20",50)$need==1,"DTaP4 before 4 years requires fifth dose")
check(status("dtap",c(2,4,9,12),"20",15)$count==3,"Too-short DTaP dose 4 interval excluded")
check(status("ipv",c(2,4,48),"10",50)$utd==1,"IPV third dose at 4 years and >=6m after second finishes")
check(status("ipv",c(2,4,6),"10",50)$need==1,"IPV infant doses need school-entry dose")
check(status("hib",c(15),"49",20)$complete,"Single Hib dose at >=15 months completes catch-up")
check(status("hib",c(2,4),"49",6)$utd==1,"Two PedvaxHIB infant doses appropriate at six months")
check(status("hib",c(2,4),"49",6)$need==0,"PedvaxHIB has no third infant dose")
check(status("hib",c(2,4),"48",6)$need==1,"PRP-T Hib needs third infant dose")
check(status("hib",c(2,4),c("49","48"),6)$need==1,"Mixed Hib history needs third infant dose")
check(status("hib",c(2,4,6,12),"48",20)$complete,"PRP-T Hib booster after first birthday completes")
check(status("hib",c(7,8,12),"48",20)$complete,"Late-start Hib completes in three doses")
check(status("hib",c(12,14),"49",20)$complete,"Hib start 12-14 months completes after >=8 weeks")
check(status("hib",numeric(),character(),60)$need==0,"No healthy-child Hib catch-up at five years")
check(is.na(status("hib",numeric(),character(),60)$utd),"Five-year-olds excluded from Hib eligibility")
check(status("pcv",c(24),"133",30)$complete,"Single PCV dose at >=24 months finishes catch-up")
check(status("pcv",c(12,14),"216",20)$complete,"PCV start after first birthday needs two doses")
check(!status("pcv",c(2,4,6),"133",20)$complete,"Three infant PCV doses need booster")
check(status("pcv",c(2,4,6,12),"133",20)$complete,"PCV infant series plus booster complete")
check(status("pcv",c(7,8),"133",11)$utd==1,"PCV late infant start has two-dose infant requirement")
check(status("pcv",c(7,8,12),"133",20)$complete,"Late-start PCV completes with three doses")
check(status("pcv",numeric(),character(),60)$need==0,"No healthy-child PCV catch-up after fifth birthday")
check(status("hepa",c(12),"83",15)$utd==1,"HepA dose 2 waits six calendar months")
check(status("hepa",c(12),"83",18)$need==1,"HepA second dose due at six-month interval")
check(status("hepb",c(0,2,6),"8",20)$utd==1,"Valid HepB infant series")
check(status("hepb",c(0,1,3),"8",20)$count==2,"HepB final dose before 24 weeks excluded")
check(status("mmr",c(12),"3",35)$need==0,"Routine MMR2 not due at 35 months")
check(status("mmr",c(12),"3",48)$need==1,"Routine MMR2 starts at 48 months")
check(status("var",c(12),"21",47)$need==0,"Routine varicella2 not due before four years")
check(status("rota",c(2,4),"119",6)$utd==1,"Rotarix two-dose completion")
check(status("rota",c(2,4),"116",6)$need==1,"RotaTeq third dose at six months")
check(status("rota",c(2,4),"116",9)$need==0,"No rotavirus catch-up after eight months")
check(status("rota",numeric(),character(),4)$need==0,"Never start rotavirus at four months")
check(length(classify_as_antigens("33"))==0,"PPSV23 never counted as PCV")
check(!"dtap" %in% classify_as_antigens("28"),"DT never counted as DTaP")
check(!"ipv" %in% classify_as_antigens("178"),"Bivalent OPV never counted as complete IPV")
check(setequal(classify_as_antigens("146"),c("dtap","ipv","hib","hepb")),"Vaxelis credits all four antigens")
check(normalize_cvx(" 008.0 ")=="8","CVX leading zeros/whitespace/Excel decimals normalized")

geo<-read_as_geography("data/American_Samoa_GIS_Demographics.xlsx")
sample<-tibble::tibble(rr_city=c("AUNUU","TAFUNA","not a known village","PAGO PAGO","TAFUNA"),
  county=rep("Pago Pago",5),city=rep("Pago Pago",5),
  clinic=c("Tualauta Clinic","Tualauta Health Center","Lealataua Clinic","Tualauta Clinic",NA_character_))
assigned<-assign_as_geography(sample,geo)
check(assigned$village[1]=="Aunu'u" && assigned$island_atoll[1]=="Aunu'u Island","RR residence has precedence over clinic")
check(assigned$county[2]=="Tualauta" && assigned$district[2]=="Western District","Clinic resolves Tafuna county ambiguity")
check(assigned$village[3]=="Unknown" && assigned$county[3]=="Lealataua","Clinic identifies county without fabricating village")
check(assigned$county[4]=="Ma'oputasi" && assigned$clinic_conflict[4],"Conflicting clinic never overrides known RR residence")
check(grepl(";",assigned$county[5]) && !assigned$county_resolved[5],"Ambiguous counties retained without duplicating child")
check(nrow(assigned)==nrow(sample),"Geography links preserve one record per child")
check(nrow(geo$official)==77 && sum(geo$official$population)==49710,"Official-village population control total")

row<-tibble::tibble(age_months=6,need_dtap=1L,need_ipv=1L,need_mmr=0L,need_hib=1L,need_hepb=1L,
  need_pcv=1L,need_rota=1L,need_var=0L,need_hepa=0L,dtap_count=2L,ipv_count=2L,hib_count=2L,
  hepb_count=2L,hib_next_final=0L,rota_all_rotarix=0L)
plan<-as_product_plan(row)
check(plan$Vaxelis==1 && plan$DTaP_Single==0 && plan$Hib_Single==0,"Combination removes covered single-antigen doses")
row$hib_next_final<-1L
plan<-as_product_plan(row)
check(plan$Vaxelis==0 && plan$Pediarix==1 && plan$Hib_Single==1,"Vaxelis never used for Hib final booster")
row$need_hepb<-0L
plan<-as_product_plan(row)
check(plan$Pentacel==1 && plan$Pediarix==0,"Pentacel covers eligible DTaP/IPV/Hib without unnecessary HepB")
row$need_ipv<-0L
plan<-as_product_plan(row)
check(sum(unlist(plan[c("Vaxelis","Pediarix","Pentacel")]))==0,"No combination when a component is not due")
row$need_hepb<-1L; row$need_ipv<-1L; row$age_months<-20L
row$dtap_count<-0L;row$ipv_count<-0L;row$hepb_count<-0L;row$hib_count<-0L
plan<-as_product_plan(row)
check(plan$Vaxelis==1,"Vaxelis first primary dose allowed for late-start catch-up; not a booster")

e<-events_for("hib",c(2,4,6,12),"48")
e$vaccination_date[4]<-e$vaccination_date[4]-4
grace_history<-valid_as_doses(e,"hib",dob,4L)
grace_status<-as_antigen_status("hib",grace_history,dob,month_date(dob,20),4L)
check(grace_status$complete,"Retrospective Hib booster four days before birthday counts as final")
e<-events_for("dtap",c(2,4,6,48),"20")
e$vaccination_date[4]<-e$vaccination_date[4]-4
grace_status<-as_antigen_status("dtap",valid_as_doses(e,"dtap",dob,4L),dob,month_date(dob,50),4L)
check(grace_status$utd==1,"Retrospective school-entry grace period applies to DTaP4 exception")
e<-events_for("ipv",c(2,44,48),"10")
check(valid_as_doses(e,"ipv",dob,4L)$invalid==1,"IPV final-dose six-month interval enforced")
e<-events_for("hepa",12,"83")
check(as_antigen_status("hepa",valid_as_doses(e,"hepa",dob,4L),dob,month_date(dob,18)-2,4L)$need==0,
  "Prospective HepA due date never uses retrospective grace")
e<-events_for("hepb",0,"8");e$vaccination_date<-dob-1
check(length(valid_as_doses(e,"hepb",dob,4L)$dates)==0,
  "No administration before date of birth can count")
# Fast completed-age arithmetic must preserve the original calendar behavior.
set.seed(20260930)
births<-as.Date("1980-01-01")+sample.int(17000,20000,replace=TRUE)
ends<-births+sample(-370:3000,20000,replace=TRUE)
check(identical(age_months_at(births,ends),
  floor(lubridate::time_length(lubridate::interval(births,ends),"months"))),
  "Fast completed ages match the original method for 20,000 date pairs")
births<-rep(as.Date(c("2020-01-31","2020-02-29","2021-01-30","2021-01-31")),each=1500)
ends<-rep(as.Date("2019-01-01")+0:1499,4)
check(identical(age_months_at(births,ends),
  floor(lubridate::time_length(lubridate::interval(births,ends),"months"))),
  "Fast completed ages preserve month-end, leap-day, and negative interval boundaries")
check(identical(age_months_at(as.Date(c(NA,"2020-01-01")),as.Date("2021-01-01")),c(NA_real_,12)),
  "Completed ages retain NA and scalar recycling")
cat("PASS:",checks,"schedule, CVX, geography, and product checks\n")
