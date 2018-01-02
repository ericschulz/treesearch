#House keeping
rm(list=ls())
#required packages packages
packages <- c('ggplot2', 'plyr', 'jsonlite')
#load them
lapply(packages, library, character.only = TRUE)

idnum<-168
#getting the right format for the way the json's are saved
format<-formatC(format="d", 1:idnum, flag="0", width=ceiling(log10(max(idnum))))


getpos<-function(movement){
  col<-0
  row<-0
  collect<-data.frame(row=rep(0,100), col=rep(0,100))
  for (i in seq_along(movement)){
    if (movement[i]=="up"){row<-row+1}
    if (movement[i]=="down"){row<-row-1}
    if (movement[i]=="right"){col<-col+1}
    if (movement[i]=="left"){col<-col-1}
    collect$row[i]<-row
    collect$col[i]<-col
  }
  return(collect)
}

dat<-data.frame(id=numeric(), round=numeric(), trial=numeric(),action=numeric(), potato=numeric())
for (id in 1:168){
  myjson<-fromJSON(paste0("/home/hanshalbe/bamcp/ppt", format[id], ".json"))
  for (round in 1:5){
    action<-myjson$allMovementTrackers[[round]]
    pos<-getpos(action)
    pos<-paste0(pos$col,"|", pos$row)
    pos<-duplicated(pos)
    k<-1
    potato<-rep(0,100)
    for (i in 1:100){
      if (!pos[i]){
        potato[i]<-myjson$allPayoffTrackers[[round]][k]
        k<-k+1
        }else{
          potato[i]<-0
        }
     }
    dummy<-data.frame(id=rep(id, 100), round=rep(round,100), trial=1:100, action=action, potato=potato)
    dat<-rbind(dat, dummy)
   }
}

write.csv(dat, "/home/hanshalbe/Desktop/treesearch/potato.csv")
