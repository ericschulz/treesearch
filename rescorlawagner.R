dat<-read.csv("/home/hanshalbe/Desktop/treesearch/potato.csv")

RescorlaWagner<-function(pars, action, outcome){
  alpha<-pars[1]
  tau<-pars[2]
  valence<-rep(0,4)
  collect<-numeric()
  for (i in 1:length(action)){
    iota<-1/tau
    soft<-exp(iota*valence)
    rho<-soft/sum(soft)
    collect<-rbind(collect,rho)
    if (action[i]=="up"){
      valence[1]<-valence[1]+alpha*(outcome[i]-valence[1])
    }
    if (action[i]=="down"){
      valence[2]<-valence[2]+alpha*(outcome[i]-valence[2])
    }
    if (action[i]=="left"){
      valence[3]<-valence[3]+alpha*(outcome[i]-valence[3])
    }
    if (action[i]=="right"){
      valence[4]<-valence[4]+alpha*(outcome[i]-valence[4])
    }
}
  return(collect)
}
dd<-subset(dat, id==1 & round==1)
RescorlaWagner(c(0.2,0.001), dd$action, dd$potato)


library(plyr)
library(DEoptim)
fitRescorlaWagner<-function(dat, pars){
  nLL<-rep(0, length(unique(dat$round)))
  for (i in 1:length(unique(dat$round))){
    dd<-subset(dat, round==unique(dat$round)[i])
    p<-RescorlaWagner(pars, dd$action, dd$potato)
    chosen<-mapvalues(dd$action, c("up","down", "left", "right"), 1:4)
    p <- (pmax(p, 0.00001))
    p <- (pmin(p, 0.99999))
    nLL[i] <- -sum(log(p[cbind(c(1:100),chosen)]))
   }
return(sum(nLL))
}


lbound <- c(exp(-10),exp(-10))
ubound <- c(1-exp(-10),exp(10))

dp<-subset(dat, id==14)

rsquare<-rep(0,5)
for (rounds in 1:5){
  dlearn<-subset(dp, round!=rounds)
  mfit<-DEoptim(fitRescorlaWagner, lower=lbound, upper=ubound, dat=dlearn)
  dtest<-subset(dp, round==rounds)
  out<-fitRescorlaWagner(dtest, mfit$optim$bestmem)
  rsquare[rounds]<-1-out/(-log(1/4)*100)
}

rsquare<-ifelse(rsquare<0,0,rsquare)
