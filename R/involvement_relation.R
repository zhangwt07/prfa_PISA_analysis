
involvement_relation <- function(parent_china){
  require(dplyr)


  p <- parent_china[,c("CNT","SCHOOLID","STIDSTD","PARINVOL")]
  setkey(p,"SCHOOLID","STIDSTD")
  setkey(student_china,"SCHOOLID","STIDSTD")

  p<-p[student_china, MATH := PV1MATH]
  p1<-p[CNT=="Hong Kong-China",.(PARINVOL,MATH)]
  p2<-p[CNT=="Macao-China",.(PARINVOL,MATH)]


  p1<-p1%>%
    group_by(PARINVOL) %>%
    summarise_all(funs(mean(., na.rm=TRUE)) )
  p1$CNT <- rep("Hong Kong-China",nrow(p1))

  p2<-p2%>%
    group_by(PARINVOL) %>%
    summarise_all(funs(mean(., na.rm=TRUE)) )
  p2$CNT <- rep("Macao-China",nrow(p2))

  p<-rbind(p1,p2)

  ggplot(p,aes(x=PARINVOL,y=MATH,group=CNT,color=CNT))+
    geom_point(na.rm=TRUE)+
    geom_smooth(method = 'loess')

}
