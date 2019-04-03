library(openxlsx)
library(pheatmap)
library(RColorBrewer)
library(ggplot2)
library(reshape2)

datadf <- read.xlsx("heatmapdata.xlsx", sheet = 2)
funcnames  = rownames(datadf)

meltdatadf = melt(datadf)
# meltdatadf$group = stringr::str_sub(meltdatadf$variable, 1, 1)
meltdatadf$group = stringr::str_extract(meltdatadf$variable, ".*_")

meltdatadf = meltdatadf[-2]
meltdatadf = aggregate(meltdatadf$value, by=list(func=meltdatadf$X1, time=meltdatadf$group), FUN=mean)
datadf = acast(meltdatadf, func ~ time)

datadf = datadf[,c(1, 5, 8, 2, 3, 4, 6, 7)]

savefilename = "heatmap.pdf"
pheatmap(as.matrix((datadf)), 
         filename = savefilename, 
         border_color = "white", 
         show_rownames = T, 
         show_colnames = T, 
         cluster_rows = F, 
         cluster_cols = F, 
         color = colorRampPalette(c("#AC2024", "white", "#89BC54"))(500)
         )





wilcoxtestmatrix <- function(datadf){
  retmatrix = matrix(data=0,
                     nrow = length(colnames(datadf)) - 1,
                     ncol = length(colnames(datadf)) - 1)
  for (i in 2:length(colnames(datadf))){
    for (j in i:length(colnames(datadf))){
      print(i)
      print(j)
      x <- datadf[, i]
      y <- datadf[, j]
      retmatrix[i-1, j-1] = retmatrix[j-1, i-1] = wilcox.test(x = x, y = y)$p.value
    }
  }
  
  retmatrix
}


wilcoxtestmatrix <- function(datadf){
  shape1 = length(rownames(datadf))
  #datadf = as.matrix(datadf)
  retmatrix = c(1:shape1)
  for (i in 1:shape1){
    x <- datadf[i,2:25]
    retmatrix[i] = wilcox.test(x = as.double(x))$p.value
  }
  
  retmatrix
}

wilcoxpval = wilcoxtestmatrix(datadf)
fdr = p.adjust(wilcoxpval, method = 'fdr')

resdf = data.frame(func=datadf[,1], pval = wilcoxpval, fdr = fdr)

write.csv(resdf, "pvalfdr.csv")