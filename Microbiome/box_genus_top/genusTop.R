library(ggplot2)
library(reshape2)

##################################### bacteria metabolishm

boxplotsize=0.3

datadf <- read.csv("genustop_30.csv")
Genus <- datadf[1]
datadf <- datadf[-1] / datadf$Total
datadf = datadf[-length(colnames(datadf))]
datadf$Genus <- Genus$Genus

plotdata <- melt(datadf)
plotdata$group <- substring(plotdata$variable, 1, 4)
plotdata$condition <- substring(plotdata$variable, 1, 1)
plotdata$organ <- substring(plotdata$variable, 3, 4)

#ggplot(plotdata, aes(factor(organ, levels = Genus), value, fill = condition)) + 
ggplot(plotdata, aes(factor(Genus), value, fill = condition)) + 
  stat_boxplot(geom='errorbar', size = boxplotsize) +
  geom_boxplot(outlier.shape = NA, size = boxplotsize) +
  geom_point(position = position_jitter(), shape = 21, colour = "black", fill = "white") + 
  labs(x = "") +
  theme(
    #axis.text.x = element_text(angle = 40, vjust = 0.1, hjust = 0.5), 
        legend.title = element_blank(),
        axis.text = element_text(colour = "black")) +
  coord_flip()
