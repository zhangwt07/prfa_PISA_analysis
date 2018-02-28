parent_occupational <- function(student_china){
  # load dataset
  require(PISA2012lite)
  require(data.table)
  require(magrittr)
  require(dplyr)
  require(ggplot2)
  require(cowplot)

  areas <-c("China-Shanghai","Hong Kong-China","Macao-China")
  selected_student<- student_china[CNT %in% areas,.(CNT,HISEI,PV1MATH)]

  myplots<-list()
  i<-1
  for (t in areas) {
    i<-i
    s <- selected_student[CNT==t]

    s<-s%>%
      group_by(HISEI) %>%
      summarise_all(funs(mean(., na.rm=TRUE)) )
    s$CNT <- rep(t,nrow(s))

    p<- ggplot(s,aes(x=HISEI,y=PV1MATH))+
      geom_point(na.rm = TRUE)+
      geom_smooth(method = 'loess',na.rm = TRUE)

    myplots[[i]]<-p
    i<-i+1
  }


  cowplot::plot_grid(myplots[[1]],myplots[[2]],myplots[[3]],nrow = 2,labels = areas,align = "h")

}
