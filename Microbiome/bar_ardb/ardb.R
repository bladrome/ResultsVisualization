library(reshape2)
library(ggplot2)


datadf <- read.table("./data/kru_H_result.xls")
datadf$ardb <- rownames(datadf)

datadf <- datadf[0:10,]

meltdatadf <- melt(datadf)
meanmusk <- meltdatadf$variable %in% grep("mean", meltdatadf$variable, value=TRUE)
sdmusk <- meltdatadf$variable %in% grep("sd", meltdatadf$variable, value=TRUE)


meandf <- subset(meltdatadf, meanmusk)
sddf <- subset(meltdatadf, sdmusk)

plotdata <- meandf
plotdata$sd <- sddf$value
colnames(plotdata) <- c("ardb", "organ", "mean", "sd")
plotdata$group <- substring(plotdata$organ, 1, 4)


ggplot(plotdata, aes(
  factor(ardb, levels = datadf$ardb), 
  mean, 
  fill = group)) + 
  geom_col(position = position_dodge()) +
  theme(
    axis.text = element_text(colour = "black"),
    axis.text.x = element_text(hjust = 0.9, vjust = 0.9, angle = 40)
  ) + 
  scale_fill_manual(values = 
                       colorRampPalette(
                         #colors = c("#599D75", "#4172AF", "#D49F35", "#D49F35")
                         colors = c("#0EA01F", "#0A76E2", "#EA0F82", "#E5B210", "#9E0142", "#5E4FA2")
                       )(6)) +
  labs(x="", y = "mean proportions (%)")
  #scale_x_discrete(labels = datadf$ardb) +
  #coord_flip()
ggsave("ardb.pdf", width = 10, height = 5)

