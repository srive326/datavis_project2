library(lubridate)     # for wday(...)
library(ggplot2)
library(RColorBrewer)
library(lattice)

charz<- c(as.character(1:62))
path <- "/Users/stephanierivera/Documents/datavis_project2/heatmaps/"

list_ofheatmaps <- list()

for (i in 1:length(heatmap_list)){
latest_ting <- data.frame(heatmap_list[[i]])

latest_ting[is.na(latest_ting)] <- 0
latest_ting[latest_ting == 3 ] <- 0
latest_ting[latest_ting == 2 ] <- 0

happy <- aggregate(latest_ting[,2:25], by=list(day = latest_ting$day), FUN=sum)
names(happy) <- c("day",1:24)
happy<- happy[c(4,2,6,7,5,1,4),]
jpeg(file=paste(path,charz[i],".jpg", sep = ""))
heatmap(as.matrix(t(happy[,2:25])), Rowv=NA, Colv=NA, col = cm.colors(256),scale="none", margins=c(8,10), labCol = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
dev.off()

}

list_ofheatmaps

jpeg(file="/Users/stephanierivera/Documents/datavis_project2/heatmaps/heat.jpg")
heatmap(as.matrix(t(happy[,2:25])), Rowv=NA, Colv=NA, col = cm.colors(256),scale="none", margins=c(8,10), labCol = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
dev.off()