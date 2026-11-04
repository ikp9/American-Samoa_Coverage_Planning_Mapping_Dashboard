# Synthetic boundary checks; base R only.
source("R/prepare_data.R")
x<-data.frame(village=c("A","B","C","D","Unknown","F","G"),county="Known",
  child_population=c(0,10,11,5,10,10,11),
  priority_group=c("High","High","High","Low","High","Moderate","Moderate"),
  priority_score=c(90,80,70,20,60,50,40),priority_rank=1:7,
  data_quality_flag=c("Ready","Missing coordinates","Ready","Ready","Unknown village / county point","Ready","Ready"))
y<-apply_community_priority_review(x)
stopifnot(identical(y$priority_group,c("Moderate","Moderate","High","Low","Moderate","Moderate","Moderate")),
  identical(y$priority_group_before_review,x$priority_group),
  identical(y$priority_score,x$priority_score),
  identical(y$priority_rank,c(4,5,1,7,6,2,3)),
  identical(y$priority_score_rank,rank(-x$priority_score,ties.method="min")),
  identical(y$small_denominator,c(TRUE,TRUE,FALSE,TRUE,TRUE,TRUE,FALSE)),
  y$data_quality_flag[2]=="Missing coordinates; Small denominator",
  y$data_quality_flag[5]=="Unknown village / county point; Small denominator",
  y$data_quality_flag[3]=="Ready",identical(apply_community_priority_review(y),y))
x$child_population[1]<-NA
stopifnot(inherits(tryCatch(apply_community_priority_review(x),error=identity),"error"))
boundaries<-data.frame(village="Unknown",county="Unknown",child_population=c(10,11,30,31),
  priority_group=c("High","High","Low","Low"),priority_score=c(90,80,20,10),data_quality_flag="Ready")
b<-apply_community_priority_review(boundaries)
stopifnot(identical(b$priority_group,c("Moderate","High","Low","High")),
  !any(grepl("Data quality review required",b$data_quality_flag[1:3])),
  grepl("Data quality review required",b$data_quality_flag[4]),
  identical(b$priority_score,boundaries$priority_score),identical(apply_community_priority_review(b),b))
cat("PASS: small-denominator boundaries, review ranking, known/unknown geography, flags, unchanged scores and idempotence\n")
