ave_score_china <- function(some_student){
  # load dataset
  require(data.table)
  require(magrittr)


  subjects<-c("MATH","MACC","MACQ","MACS","MACU","MAPE","MAPF","MAPI")
  areas <-c("China-Shanghai","Hong Kong-China","Macao-China")
  scores<-data.frame(areas)

  selected_pla <- select(student_china,starts_with("PV1"))
  selected_pla$CNT <- student_china[,c("CNT")]
  selected_pla$weight<-student_china[,c("W_FSTUWT")]

  for (subject in subjects){
    score<-c()

    for (area in areas){
      t<-selected_pla[CNT==area]

      s <- sum((select(t,ends_with(subject)) * t$weight)/sum(t$weight))
      score<-c(score,s)
    }
    scores[subject]<-score

  }
  return(scores)
}
