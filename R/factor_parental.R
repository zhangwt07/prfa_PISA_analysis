factor_parental<-function(student_china,parent_china){


areas <-c("China-Shanghai","Hong Kong-China","Macao-China")
student_subset<- student_china[CNT %in% areas,.(CNT,STRATUM, STIDSTD,WEALTH,PV1MATH,HISCED,HISEI)]
parent_subset<-parent_china[CNT %in% areas,.(STRATUM, STIDSTD, PARINVOL)]

setkey(student_subset, STRATUM, STIDSTD)
setkey(parent_subset, STRATUM, STIDSTD)

p<-student_subset[parent_subset,PARINVOL:=PARINVOL]

factors<-c("WEALTH","HISCED","HISEI")


p1<-p[CNT=="Hong Kong-China",.(PARINVOL,WEALTH)]
p2<-p[CNT=="Macao-China",.(PARINVOL,WEALTH)]
p1<-p1%>%
  group_by(WEALTH) %>%
  summarise_all(funs(mean(., na.rm=TRUE)) )
p1$CNT <- rep("Hong Kong-China",nrow(p1))
p2<-p2%>%
  group_by(WEALTH) %>%
  summarise_all(funs(mean(., na.rm=TRUE)) )
p2$CNT <- rep("Macao-China",nrow(p2))
p3<-rbind(p1,p2)
ggplot(p3,aes(x=WEALTH,y=PARINVOL,group=CNT,color=CNT))+
  geom_point(na.rm=TRUE)+
  geom_smooth(method = 'loess')

p1<-p[CNT=="Hong Kong-China",.(PARINVOL,HISCED)]
p2<-p[CNT=="Macao-China",.(PARINVOL,HISCED)]
p1<-p1%>%
  group_by(HISCED) %>%
  summarise_all(funs(mean(., na.rm=TRUE)) )
p1$CNT <- rep("Hong Kong-China",nrow(p1))
p2<-p2%>%
  group_by(HISCED) %>%
  summarise_all(funs(mean(., na.rm=TRUE)) )
p2$CNT <- rep("Macao-China",nrow(p2))
p3<-rbind(p1,p2)
ggplot(p3,aes(x=HISCED,y=PARINVOL,group=CNT,color=CNT))+
  geom_point(na.rm=TRUE)+
  geom_smooth(method = 'loess')

p1<-p[CNT=="Hong Kong-China",.(PARINVOL,HISEI)]
p2<-p[CNT=="Macao-China",.(PARINVOL,HISEI)]
p1<-p1%>%
  group_by(HISEI) %>%
  summarise_all(funs(mean(., na.rm=TRUE)) )
p1$CNT <- rep("Hong Kong-China",nrow(p1))
p2<-p2%>%
  group_by(HISEI) %>%
  summarise_all(funs(mean(., na.rm=TRUE)) )
p2$CNT <- rep("Macao-China",nrow(p2))
p3<-rbind(p1,p2)
ggplot(p3,aes(x=HISEI,y=PARINVOL,group=CNT,color=CNT))+
  geom_point(na.rm=TRUE)+
  geom_smooth(method = 'loess')

}
