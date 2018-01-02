#House keeping
rm(list=ls())
#required packages packages
packages <- c('ggplot2', 'plyr', 'jsonlite')
#load them
lapply(packages, library, character.only = TRUE)


getlocation<-function(movement){
  map<-matrix(0,nrow=210, ncol=210)
  col<-106
  row<-106
  for (i in seq_along(movement)){
    map[row,col]<-map[row,col]+1
    if (movement[i]=="up"){row<-row+1}
    if (movement[i]=="down"){row<-row-1}
    if (movement[i]=="right"){col<-col+1}
    if (movement[i]=="left"){col<-col-1}
  }
  return(map)
}

getprobs<-function(movement, m){
  map<-matrix(0,nrow=210, ncol=210)
  col<-106
  row<-106
  probs<-rep(0, length(movement))
  for (i in seq_along(movement)){
    map[row,col]<-map[row,col]+1
    probs[i]<-m[row, col]
    if (movement[i]=="up"){row<-row+1}
    if (movement[i]=="down"){row<-row-1}
    if (movement[i]=="right"){col<-col+1}
    if (movement[i]=="left"){col<-col-1}
  }
  return(probs)
}

idnum<-168
#getting the right format for the way the json's are saved
format<-formatC(format="d", 1:idnum, flag="0", width=ceiling(log10(max(idnum))))

#initialize data frame
dat<-data.frame(prob=numeric(), trial=numeric(), condition=numeric(), block=numeric(), id=numeric())

#loop through the json files
for (i in 1:idnum){
  #get individual json
  myjson<-fromJSON(paste0("/home/hanshalbe/bamcp/ppt", format[i], ".json"))
  probs<-numeric()
  numb<-numeric()
  for (j in 1:5){
    p<-getprobs(myjson$allMovementTrackers[[j]], outer(myjson$allColParameters[[j]], myjson$allRowParameters[[j]]))
    numb<-c(numb, 1:100)
  }
  dummy<-data.frame(prob=p, trial=numb, condition=rep(myjson$condition, 5), block=rep(1:5, each=100), id=rep(i, 500))
  dat<-rbind(dat, dummy)
}
head(dat, 100)
#recode conditions
dat$cond<-mapvalues(dat$condition, 1:4, c("1-2", "1-2", "0.5-0.5", "0.5-0.5"))


#create a frame for plotting with means and standard errors per block and condition
dplot<-ddply(dat, ~cond+trial, summarize, mu=mean(prob), se=se(prob))
#change condition to factor for plotting and type consitency
dplot$cond<-as.factor(dplot$cond)
#set limits
limits <- aes(ymax = mu + se, ymin=mu - se)

#dodge bars a little
pd <- position_dodge(.2)

#plot the means over blocks and condition
p<-ggplot(dplot, aes(x=trial, y=mu, col=cond)) +
  #error bars
  geom_errorbar(aes(ymin=mu-se, ymax=mu+se), width=0.2, size=1, position=pd) +
  #lines
  geom_line(position=pd, size=1) +
  #classic theme, legend on bottom
  theme_classic()+theme(text = element_text(size=22,  family="serif"), legend.position="bottom")+
  ylab("Mean probability")+xlab("Trial")+
  guides(col=guide_legend(title="Condition (beta distribution)"))+
  #adjust text size
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill=NA, size=2))
#show plot
print(p)

library(lme4)
head(dat)
rm(trial)
rm(probs)
dat$id<-as.factor(dat$id)
m<-glm(prob~trial, family='binomial', data=dat)
summary(m)
cor.test(dat$trial, dat$prob)
