library(plyr)
#heuristic tree search: applies root sampling and random search
##pastmoves: history of past moves
##rewards: history of past rewards
##nsteps: number of steps to plan ahead
##neval: number of evaluations
heuristictreesearch<-function(pastmoves, rewards, nsteps, neval){
  
  #initialize matrix of visited tiles
  mapvisit<-mappayoff<-matrix(0, 200, 200)
  #initialize starting position
  row<-100
  col<-100
  
  #creat maps of explored tiles and received rewards
  #loop over past moves
  for (i in seq_along(pastmoves)){
    #update positions
    if (pastmoves[i]=="up"){row<-row+1}
    if (pastmoves[i]=="down"){row<-row-1}
    if (pastmoves[i]=="right"){col<-col+1}
    if (pastmoves[i]=="left"){col<-col-1}
    #check if reward occured
    if (rewards[i]==1){mappayoff[row,col]<-1}
    #track visited states
    mapvisit[row,col]<-mapvisit[row,col]+1
  }
  
  #row sums of payoffs: k
  rsp<-rowSums(mappayoff)
  #column sums of payoffs: k
  csp<-rowSums(mappayoff)
  #frequency of row visits: n
  rsn<-rowSums(mapvisit)
  #frequency of column visits: n
  csn<-rowSums(mapvisit)
  #Bayesian Beta-posterior
  qualcol<-(1+csp)/(1+2+csn)
  qualrow<-(1+rsp)/(1+2+rsn)
  
  #initialize all random moves as a matrix
  ##has as many columns as horizon length
  ##has as many rows as evaluations
  moves<-matrix(sample(c("up", "down", "left", "right"), nsteps*neval, replace = TRUE), ncol=nsteps) 
  
  #initialize tracker frame with pseudo observation
  dd<-data.frame(first=c("up", "down", "left", "right"), reward=rep(0,4))
  
  #loop over evaluations
  for (n in 1:neval){
    #current map is outer product of row and column Ps
    ##If this is bigger than uniform sampled value, give reward
    currentmap<-outer(qualrow,qualcol)
    #copy visited map
    mvisit<-mapvisit
    #steps for this run
    steps<-moves[n,]
    #initialize reward
    reward<-rep(0, length(steps))
    #copy row and column state
    rowrow<-row
    colcol<-col
    #loop over steps
    for (step in 1:length(steps)){
      #update state
      if (steps[step]=="up"){rowrow<-rowrow+1}
      if (steps[step]=="down"){rowrow<-rowrow-1}
      if (steps[step]=="right"){colcol<-colcol+1}
      if (steps[step]=="left"){colcol<-colcol-1}
      #current map
      pmap<-currentmap
      #if a state has been visited, then it's reward is 0
      pmap[mvisit>0]<-0
      #collect reward
      reward[step]<-pmap[rowrow,colcol]
      #update visits
      mvisit[rowrow,colcol]<-mvisit[rowrow,colcol]+1
    }
    #attach data frame
    dd<-rbind(dd, data.frame(first=steps[1], reward=sum(reward)))
  }
  #summarize: get the mean per first step
  dout<-ddply(dd, ~first, summarize, reward=mean(reward))
#return results  
return(dout)   
}

#data frame of all ids and rounds
dselect<-expand.grid(id=1:168, round=1:5)
#dat of exepriment
dat<-read.csv("/home/ucabchu/Scratch/safeopt/treesearch/potato.csv")
#cluster argument
select<-as.integer(commandArgs(TRUE)[1])
#select participant and trial
dp<-subset(dat, id==dselect$id[select] & round==dselect$round[select])
#collector matrix
collect<-matrix(0, ncol=4, nrow=100)
#loop over trials
for (trialn in 1:100){
  #data frame up to that trial
  din<-subset(dp, trial <= trialn)
  #perform tree search
  out<-heuristictreesearch(pastmoves=din$action, rewards=din$potato, nsteps=2, neval=10000)
  #sort alphabetically
  out<-out[sort(out$first),]
  #collect results
  collect[trialn,]<-out$reward
}

#change into data frame
dout<-as.data.frame(collect)
#name after moves
names(dout)<-c("down", "left", "right", "up")
#batch name
batchname<-paste0("/home/ucabchu/Scratch/treesearch/tree_id",dselect$id[select],"_round",dselect$round[select], ".csv")
#save
write.csv(dout, batchname)
