ave_score <- function(area,subject,student_china){
  # load dataset

  require(data.table)
  require(magrittr)
  require(dplyr)



  some_student <- student_china[CNT == area]

  #plausible
  selected_pla <- select(some_student,ends_with(subject))
  selected_pla <- select(selected_pla,starts_with("PV"))
  selected_pla <- cbind(selected_pla, sum = rowSums(selected_pla)/5)
  score <- sum((selected_pla$sum * some_student$W_FSTUWT)/sum(some_student$W_FSTUWT))
  return(score)


}
