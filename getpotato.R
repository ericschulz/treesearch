#House keeping
rm(list=ls())
#required packages packages
library(jsonlite)

#getting the differences
mydiff<-function(x){
  #start
  y<-x
  #loop through
  for (i in 2:length(x)){
    #difference to previous entry
    y[i]<-y[i]-x[i-1]
  }
  #retun
  return(y)
}

#recovered data
d<-read.csv("/home/hanshalbe/Desktop/treesearch/recovered.csv")
unique(d$id)

#id number
idnum<-168
#format
format<-formatC(format="d", 1:idnum, flag="0", width=ceiling(log10(max(idnum))))

#intialize dataframe
dat<-data.frame(X=numeric(), id=numeric(), block=numeric(),proposal=numeric(), potato=numeric())
#loop through
for (i in 1:168){
  #read in json
  myjson<-fromJSON(paste0("/home/hanshalbe/bamcp/ppt", format[i], ".json"))
  #pay offs per block
  payoff<-mydiff(as.numeric(unlist(myjson$allPayoffCounts)))[1:5]
  #loop through blocks
  for (blocks in 1:5){
    #tracler of distance
    track<-rep(0, 5)
    #loop through proposals
    for (p in 1:20){
      #select proposal
      ds<-subset(d, id==i & block==blocks & proposal==p)
      #distance in scores
      track[p]<-abs(sum((0.02*ds$potato)*0.985^(0:99))-payoff[blocks])
    }
    #bets proposal
    dtake<-subset(d, id==110 & block==blocks & proposal==which.min(track))
    #bind together
    dat<-rbind(dat, dtake)
  }
}
head(dat)
d<-ddply(dat, ~id, summarize, l=length(block))
dim(subset(d, l>300))
