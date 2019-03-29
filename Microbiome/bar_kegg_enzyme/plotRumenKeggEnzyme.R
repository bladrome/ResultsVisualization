library(xlsx)
library(ggplot2)
library(reshape2)

# F #EE7671 S #27B9B9

newcol <- c("#EE7671", "#27B9B9")
newcol <- c("#27B9B9", "#EE7671")

datadf <- read.xlsx("./data/RumenKeggEnzyme.xls", sheetIndex = 1)

datadf = datadf[-length(colnames(datadf))]

plotdata <- melt(datadf)
plotdata$condition <- substring(plotdata$variable, 1, 1)
aggdata <- aggregate(plotdata, list(Enzyme = plotdata$Enzyme, condition = plotdata$condition, 
    Description = plotdata$Description), FUN = mean)
plotdata <- aggdata[c(1, 2, 3, 7)]

ggplot(plotdata, aes(factor(Enzyme, levels = rev(datadf$Enzyme)), value, fill = factor(condition, 
    levels = c("S", "F")))) + geom_col(width = 0.5, position = position_dodge2(padding = 0.2, 
    width = 0.01)) + # stat_boxplot(geom='errorbar', size = boxplotsize) +
# stat_boxplot(geom='errorbar') + geom_boxplot(outlier.shape = NA, size =
# boxplotsize) + geom_boxplot(outlier.shape = NA) + geom_point(position =
# position_dodge(), shape = 21, colour = 'black', fill = 'white') +
labs(x = "") + ggtitle("Rumen Kegg Enzyme") + theme(axis.text.x = element_text(angle = 40, 
    vjust = 0.9, hjust = 0.9), legend.title = element_blank(), axis.text = element_text(colour = "black")) + 
    # scale_y_continuous(limits = c(0.00, 0.20), breaks=c(seq(0.00, 0.20, 0.05))) +
# scale_y_continuous(limits = c(0.5, 6.5), breaks=c(seq(0.5, 6.5, 0.5))) +
scale_fill_manual(values = newcol) + scale_y_continuous(limits = c(0, 0.1), expand = expand_scale(0, 
    0)) + # ylim(0, 0.1) +
coord_flip()


ggsave("Rumenkeggenzyme.pdf", width = 4, height = 8)
