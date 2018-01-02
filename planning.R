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

getexplore<-function(movement, m){
  map<-matrix(0,nrow=210, ncol=210)
  col<-106
  row<-106
  exploration<-rep(0, length(movement))
  for (i in seq_along(movement)){
    map[row,col]<-map[row,col]+1
    g<-getlocation(movement[1:i])
    rs<-rowSums(g)
    cs<-colSums(g)
    exploration[i]<-0.5*rs[row]/sum(rs)+0.5*cs[col]/sum(cs)
    if (movement[i]=="up"){row<-row+1}
    if (movement[i]=="down"){row<-row-1}
    if (movement[i]=="right"){col<-col+1}
    if (movement[i]=="left"){col<-col-1}
  }
  return(exploration)
}

randprobs<-function(movement, m){
  map<-matrix(0,nrow=210, ncol=210)
  col<-106
  row<-106
  probs<-rep(0, length(movement))
  for (i in seq_along(movement)){
    map[row,col]<-map[row,col]+1
    probs[i]<-m[row, col]
    move<-sample(c("up", "down", "left", "right"), 1)
    if (move=="up"){row<-row+1}
    if (move=="down"){row<-row-1}
    if (move=="right"){col<-col+1}
    if (move=="left"){col<-col-1}
  }
  return(probs)
}

randexplore<-function(movement, m){
  map<-matrix(0,nrow=210, ncol=210)
  col<-106
  row<-106
  exploration<-rep(0, length(movement))
  for (i in seq_along(movement)){
    map[row,col]<-map[row,col]+1
    rs<-rowSums(map)
    cs<-colSums(map)
    exploration[i]<-0.5*rs[row]/sum(rs)+0.5*cs[col]/sum(cs)
    move<-sample(c("up", "down", "left", "right"), 1)
    if (move=="up"){row<-row+1}
    if (move=="down"){row<-row-1}
    if (move=="right"){col<-col+1}
    if (move=="left"){col<-col-1}
  }
  return(exploration)
}



x<-1:168
format<-formatC(format="d",x,flag="0",width=ceiling(log10(max(x))))
correlation<-randcor<-exploration<-randexp<-numb<-numeric()
for (i in 1:168){
  myjson<-fromJSON(paste0("/home/hanshalbe/bamcp/ppt", format[i], ".json"))
  for (j in 1:5){
    p<-getprobs(myjson$allMovementTrackers[[j]], outer(myjson$allColParameters[[j]], myjson$allRowParameters[[j]]))
    prand<-randprobs(myjson$allMovementTrackers[[j]], outer(myjson$allColParameters[[j]], myjson$allRowParameters[[j]]))
    e<-getexplore(myjson$allMovementTrackers[[j]], outer(myjson$allColParameters[[j]], myjson$allRowParameters[[j]]))
    erand<-randexplore(myjson$allMovementTrackers[[j]], outer(myjson$allColParameters[[j]], myjson$allRowParameters[[j]]))
    correlation<-c(correlation, acf(p, plot=FALSE)$acf[2:11])
    exploration<-c(exploration, acf(e, plot=FALSE)$acf[2:11])
    randcor<-c(randcor, acf(prand, plot=FALSE)$acf[2:11])
    randexp<-c(randexp, acf(erand, plot=FALSE)$acf[2:11])
    numb<-c(numb, 1:10)
  }
}

d<-data.frame(cor=correlation, rcor=randcor, exp=exploration, rexp=randexp, num=numb)

dplot<-ddply(d, ~num, summarize, cor=mean(cor), rcor=mean(randcor), rex=mean(exp), randex=mean(randexp))

dplot<-data.frame(horizon=rep(1:10, 4), 
                  correlation=c(dplot$cor, dplot$rcor, dplot$rex, dplot$randex), 
                  Method=rep(rep(c("Empirical", "Random"), each=10), 2),
                  Value=rep(c("Expectation", "Certainty"), each=20))

dplot<-subset(dplot, horizon<6)

p1 <- ggplot(dplot, aes(x=horizon, y=correlation, col=Method)) + 
  #bars
  geom_line(stat="identity")+
  #0 to 1
  ylim(c(0,0.8)) + 
  #golden ratio error bars
  geom_point(size=1)+
  #title
  facet_wrap(~Value, scales = "free_y")+
  theme_classic() +xlab("Horizon")+ylab("ACF")+
  ggtitle("Planning horizon")+
  theme(text = element_text(size=20, family="serif"),
        strip.background = element_blank(),
        plot.title = element_text(size=20),
        legend.position = "bottom")

pdf("horizon.pdf", width=6, height=4)
p1
dev.off()
