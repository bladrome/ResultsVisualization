library(ggplot2)
library(reshape2)

##################################### bacteria metabolishm

width = 2
height <- 2

boxplotsize <- 0.3
boxaxissize <- 0.3

plotbacfun <-  function (plotdata){
  boxaxissize <- 0.001
  lx <- 1
  rx <- 2
  ymingap = 0.005
  lymin <- max(plotdata$value[1:5])  + 1 * ymingap
  rymin <- max(plotdata$value[6:10]) + 1 * ymingap
  lrymax <- max(plotdata$value) + 2 * ymingap
  p <- ggplot(plotdata, aes(group, value, fill = group)) + 
    stat_boxplot(geom='errorbar', size = boxplotsize) +
    geom_boxplot(outlier.shape = NA, size = boxplotsize) +
    geom_point(position = position_jitter(), shape = 21, colour = "black", fill = "white") + 
    labs(x = "") +
    theme(axis.text.x = element_text(angle = 40, vjust = 0.5, hjust = 0.5), 
          legend.title = element_blank(),
          axis.text = element_text(colour = "black")) +
    ggtitle(plotdata[1,1])  +
    
    geom_segment(aes(x=1, y=lymin, xend=1, yend=lrymax))+#, size = boxaxissize))+
    geom_segment(aes(x=2, y=rymin, xend=2, yend=lrymax))+#, size = boxaxissize))+
    geom_segment(aes(x=1, y=lrymax, xend=1.40, yend=lrymax))+#, size = boxaxissize))+ 
    geom_segment(aes(x=1.60, y=lrymax, xend=2, yend=lrymax))+#, size = boxaxissize))+
    annotate("text", x=1.5, y=lrymax, label="*", size=2) +
  
    labs(y="")
  
  p
}

plist <<-  list()
plistindex <<-  1

plotorgbac <- function(filepath, organ){
  datadf <- read.csv(filepath)
  for (i in 1:length(datadf[,1])){
    #print(datadf[i, ])
    plotdata <- as.data.frame(datadf[i, ])
    total <- plotdata$Total
    
    plotdata <- melt(plotdata[-length(colnames(plotdata))])
    plotdata$value <- plotdata$value / total
    plotdata$group <- substring(plotdata$variable, 1, 4)
    p <- plotbacfun(plotdata)
    
    ttestpvalue <- t.test(plotdata$value[1:5], plotdata$value[6:10])$p.value
    print(ttestpvalue)
    
    plist[[plistindex]] <<-  as.list(p)
    #print(plistindex)
    plistindex <<-  plistindex + 1
    
    filesave = paste0(organ, "_", i, ".pdf")
    ggsave(filesave, width = width, height = height)
    #print(p)
  }

}

filepath = "data/rumenbac.csv"
plotorgbac(filepath, "Rumen")

filepath = "data/cecumbac.csv"
plotorgbac(filepath, "Cecum")

filepath = "data/colonbac.csv"
plotorgbac(filepath, "Colon")




library(patchwork)
plist[[1]] + plist[[2]] + 
  plist[[3]] + plist[[4]] + plist[[5]] + 
  plist[[6]] + plist[[7]] + 
  plot_layout(ncol = 3)
ggsave("figure3_listbox.pdf", width = 8, height = 11)

