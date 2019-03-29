library(ggplot2)
library(reshape2)
datadf <- read.table("./data/anova_result - ¸±±¾.xls", sep = '\t', 
                     header = T,
                     stringsAsFactors = F)

topK = 10
newcol <- c("#9E0142",  "#D53E4F", "#F46D43", "#FDAE61", "#FEE08B", "#FFFFBF", "#E6F598", "#ABDDA4", "#66C2A5", "#3288BD", "#5E4FA2")

datadf <- datadf[1:topK,]

xorder <- datadf[1]$X

namelist <- strsplit(xorder, ';', fixed = T)
genusname = c()
for (i in 1:length(namelist)){
  genusname[i] = namelist[[i]][7]
}
genusname = substring(genusname, 5)

datadf$X <- genusname

meltdatadf <- melt(datadf)
sdmustk <- stringr::str_detect(colnames(datadf), "sd")
meanmustk <- stringr::str_detect(colnames(datadf), "mean")

plotdf <- subset(datadf, select = c(T, meanmustk[2:length(meanmustk)]))
plotdf <- melt(plotdf)

ggplot(plotdf, aes(factor(X,levels = rev(genusname)), 
                   value, 
                   fill=variable)) +
  geom_col(position = position_dodge()) +
  coord_flip() +
  theme(
    axis.text.x = element_text(angle=90, hjust = 0.5, vjust = 0.5),
    axis.text = element_text(colour = "black")
  ) + 
  scale_y_continuous(expand = expand_scale(0,0), limits = c(0, 30)) +
  labs(x = "", y = "Mean proportions (%)")+
  scale_fill_manual(values = 
                    colorRampPalette(
                      #colors = c("#599D75", "#4172AF", "#D49F35", "#D49F35")
                      colors = c("#0EA01F", "#0A76E2", "#EA0F82", "#E5B210", "#9E0142", "#5E4FA2")
                    )(8))


ggsave("S2_diffgenus.pdf", width = 7, height = 8)
