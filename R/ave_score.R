ave_score <- function(some_student){
  # load dataset
  #require(PISA2012lite)
  require(data.table)
  require(magrittr)
  require(dplyr)

  subjects<-c("MATH","MACC","MACQ","MACS","MACU","MAPE","MAPF","MAPI")
  areas <-c("China-Shanghai","Hong Kong-China","Macao-China")
  scores<-data.frame(areas)


  #df<-data_frame(MATH,MACC,MACQ,MACS,MACU,MAPE,MAPF,MAPI)
  #plausible


  for (subject in subjects){
    score<-c()
    col_name<-paste("PV","1",subject,sep = "")
    selected_pla <- some_student[,c("CNT",col_name,"W_FSTUWT")]
    set_colnames(selected_pla,c("CNT","sum","weight"))

    for (area in areas){
      selected_pla_area<-selected_pla[CNT==area]
      s <- sum((selected_pla_area$sum * selected_pla$weight)/sum(selected_pla$weight))
      score<-c(score,s)
    }

    scorec$area<-score
  }
  return(score)
}
