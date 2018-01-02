#House keeping
rm(list=ls())
#required packages packages
packages <- c('ggplot2', 'plyr', 'jsonlite')
#load them
lapply(packages, library, character.only = TRUE)

library(grid)
library(gridExtra)
library(png)

x<-1:168
format<-formatC(format="d",x,flag="0",width=ceiling(log10(max(x))))

m<-rep(list(matrix(0, nrow=23, ncol=23)), 8)

for (i in 1:168){
  myjson<-fromJSON(paste0("/home/hanshalbe/bamcp/ppt", format[i], ".json"))
  for (j in 1:8){
    ma<-outer(myjson$allRowParameters[[5+j]] ,myjson$allColParameters[[5+j]])
    mt<-t(matrix(c("no","up" ,"no", "left", "no", "right", "no", "down", "no"), nrow=3, ncol=3))
    mt<-mt[ma[11:13,11:13]==0.8]
    bait<-mt[mt!="no"]
    info<-ifelse(sum(myjson$allExploredRows[[5+j]])+sum(myjson$allExploredCols[[5+j]])>200, "high", "low")
    if (bait=="right" & info=="high"){m[[1]]<-m[[1]]+getlocation(myjson$allMovementTrackers[[5+j]])}
    if (bait=="left" & info=="low"){m[[2]]<-m[[2]]+getlocation(myjson$allMovementTrackers[[5+j]])}
    if (bait=="left" & info=="high"){m[[3]]<-m[[3]]+getlocation(myjson$allMovementTrackers[[5+j]])}
    if (bait=="right" & info=="low"){m[[4]]<-m[[4]]+getlocation(myjson$allMovementTrackers[[5+j]])}
    if (bait=="down" & info=="high"){m[[5]]<-m[[5]]+getlocation(myjson$allMovementTrackers[[5+j]])}
    if (bait=="up" & info=="low"){m[[6]]<-m[[6]]+getlocation(myjson$allMovementTrackers[[5+j]])}
    if (bait=="up" & info=="high"){m[[7]]<-m[[7]]+getlocation(myjson$allMovementTrackers[[5+j]])}
    if (bait=="down" & info=="low"){m[[8]]<-m[[8]]+getlocation(myjson$allMovementTrackers[[5+j]])}
  }
}



myplot<-function(k, title){
  img <- readPNG(paste0("/home/hanshalbe/Documents/newimage", k, ".png"))
  g <- rasterGrob(img, interpolate=TRUE, width=unit(1,"npc"), height=unit(1,"npc"))
  map<-m[[k]][9:15, 9:15]
  dp<-data.frame(x=rep(-3:3 ,each=7), y=rep(-3:3, 7), count=as.vector(map))
  dp$count<-ifelse(dp$count==max(dp$count), NA, dp$count)
  dp$count<-ifelse(dp$count==0, NA, dp$count)
  p  <- ggplot(dp, aes(x = x, y = y, fill = count))+
  annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)+
  geom_tile(alpha=0.4)+ theme_bw()+
  scale_fill_distiller(palette = "Spectral")+
  scale_x_continuous(expand = c(0, 0))+ 
  scale_y_discrete(expand = c(0, 0))+ guides(fill=FALSE)+
  theme(strip.background = element_blank(),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+ggtitle(title)
  return(p)
}

myplot(1, "Restricted space, high information")
myplot(2, "Restricted space, low information")
myplot(3, "Unrestricted space, high information")
myplot(4, "Unrestricted space, low information")
myplot(5, "Restricted space, high information")
myplot(6, "Restricted space, low information")
myplot(7, "Unrestricted space, high information")
myplot(8, "Unrestricted space, low information")




pdf("testmaps.pdf", width=6, height=10)
grid.arrange(p1, p5, p2, p6, p7, p3, p8, p4, ncol=2)
dev.off()

pdf("testmap1.pdf")
p1+ggtitle("")
dev.off()

pdf("testmap2.pdf")
p2+ggtitle("")
dev.off()

pdf("testmap3.pdf")
p3+ggtitle("")
dev.off()

pdf("testmap4.pdf")
p4+ggtitle("")
dev.off()

pdf("testmap5.pdf")
p5+ggtitle("")
dev.off()

pdf("testmap6.pdf")
p6+ggtitle("")
dev.off()

pdf("testmap7.pdf")
p7+ggtitle("")
dev.off()

pdf("testmap8.pdf")
p8+ggtitle("")
dev.off()