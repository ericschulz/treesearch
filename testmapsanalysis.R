#House keeping
rm(list=ls())
#required packages packages
packages <- c('ggplot2', 'plyr', 'jsonlite')
#load them
lapply(packages, library, character.only = TRUE)


getlocation<-function(movement){
  map<-matrix(0,nrow=23, ncol=23)
  col<-12
  row<-12
  for (i in seq_along(movement)){
    map[row,col]<-map[row,col]+1
    if (movement[i]=="up"){row<-row+1}
    if (movement[i]=="down"){row<-row-1}
    if (movement[i]=="right"){col<-col+1}
    if (movement[i]=="left"){col<-col-1}
  }
  return(map)
}



x<-1:168
format<-formatC(format="d",x,flag="0",width=ceiling(log10(max(x))))

dat<-data.frame(id=numeric(), map=numeric(), info=numeric(), outcome=numeric(), trap=numeric())
for (i in 1:168){
  myjson<-fromJSON(paste0("/home/hanshalbe/bamcp/ppt", format[i], ".json"))
  maps<-numeric()
  infos<-numeric()
  trap<-numeric()
  for (j in 1:8){
    m<-outer(myjson$allRowParameters[[5+j]] ,myjson$allColParameters[[5+j]])
    mt<-t(matrix(c("no","up" ,"no", "left", "no", "right", "no", "down", "no"), nrow=3, ncol=3))
    mt<-mt[m[11:13,11:13]==0.8]
    bait<-mt[mt!="no"]
    info<-ifelse(sum(myjson$allExploredRows[[5+j]])+sum(myjson$allExploredCols[[5+j]])>200, "high", "low")
    if (bait=="right" & info=="high"){map<-"map1"}
    if (bait=="left" & info=="low"){map<-"map2"}
    if (bait=="left" & info=="high"){map<-"map3"}
    if (bait=="right" & info=="low"){map<-"map4"}
    if (bait=="down" & info=="high"){map<-"map5"}
    if (bait=="up" & info=="low"){map<-"map6"}
    if (bait=="up" & info=="high"){map<-"map7"}
    if (bait=="down" & info=="low"){map<-"map8"}
    trap<-c(trap, myjson$allMovementTrackers[[5+j]][1]==bait)
    maps<-c(maps, map)
    infos<-c(infos,info)
  }
  id<-rep(i, 8)
  outcome<-as.numeric(unlist(myjson$allPotatoCounts[6:13]))
  dummy<-data.frame(id=id, map=maps, info=infos, outcome=outcome, trap=trap)
  dat<-rbind(dat, dummy)
}

dat$res<-mapvalues(dat$map, paste0("map", 1:8), 
          c("restricted", "restricted", "unrestricted","unrestricted","restricted","restricted","unrestricted","unrestricted" )) 


m1<-lm(outcome~info+res, data=dat)
summary(m1)

m2<-glm(trap~info+res, data=dat, family="binomial")
summary(m2)

library(plyr)
se<-function(x){sd(x)/sqrt(length(x))}
ddply(dat, ~res, summarize, m=mean(outcome), se=se(outcome))

dplot<-ddply(dnew2,~Group,summarise,mean=mean(dist),se=se(dist))


#limits for stand errors, approx. 95% CI
limits <- aes(ymax = mean + se, ymin=mean - se)

#do the plot
p <- ggplot(dplot, aes(y=mean, x=Group, fill=Group)) + 
  #bars
  geom_bar(position="dodge", stat="identity")+
  #0 to 1
  ylim(c(-1.1,0)) + 
  #golden ratio error bars
  geom_errorbar(limits, position="dodge", width=0.31)+
  #point size
  geom_point(size=3)+
  #title
  theme_classic() +xlab("\nKernel")+ylab("Estimate - Real\n")+
  #fill with Wes Anderson colors
  scale_fill_manual(values=c("grey50", "grey75"), guide = FALSE)+
  #adjust text size
  theme(text = element_text(size=22, family="serif"))+
  #adjust text size
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill=NA, size=2))
