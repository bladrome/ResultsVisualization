library(ggplot2)
library(reshape2)
library(xlsx)

##################################### bacteria metabolishm

k = 31

boxplotsize = 0.3
# for dubug
Coloncazymefile = "data/ColonCAZy.xls"
datadf <- read.xlsx(Coloncazymefile, sheetIndex = 1)
# datadf <- read.table(Coloncazymefile, header = T) datadf <-
# read.xlsx(Coloncazymefile, sheetIndex = 1) Total <-
# as.integer(levels(datadf$Total)) datadf <- datadf[-length(colnames(datadf))]
# datadf$Total <- Total Top k
datadf <- datadf[0:k, ]

# No Total
datadf = datadf[-length(colnames(datadf))]

## Row normalize Description <- datadf$Description datadf <- datadf[-1] /
## colSums(datadf[-1]) datadf <- datadf[-1] / rowSums(datadf[-1])
## datadf$Description <- Description

Family <- datadf$Family
datadf = datadf[-1]
datadf$Family <- Family

Description <- datadf$Description
datadf = datadf[-1]
# datadf$Description <- Description

# sig test
pvalue <- vector()
for (i in 1:length(rownames(datadf))) {
    grazing <- datadf[i, 1:5]
    drylot <- datadf[i, 6:10]
    
    testres <- t.test(grazing, drylot)
    pvalue[[i]] <- testres$p.value
}

# datadf$pvalue <- pvalue datadf <- datadf[order(datadf$pvalue),]


plotdata <- melt(datadf)
plotdata$group <- substring(plotdata$variable, 1, 4)
plotdata$condition <- substring(plotdata$variable, 1, 1)
plotdata$organ <- substring(plotdata$variable, 3, 4)

ggplot(plotdata, aes(factor(Family, levels = rev(datadf$Family)), value, fill = factor(condition, 
    levels = c("S", "F")))) + stat_boxplot(geom = "errorbar", size = boxplotsize) + 
    geom_boxplot(outlier.shape = NA, size = boxplotsize) + # geom_point(position = position_dodge(), shape = 21, colour = 'black', fill =
# 'white') +
labs(x = "") + ggtitle(substring(Coloncazymefile, 14, 18)) + theme(axis.text.x = element_text(angle = 40, 
    vjust = 0.9, hjust = 0.9), legend.title = element_blank(), axis.text = element_text(colour = "black")) + 
    # scale_y_continuous(limits = c(0.00, 0.20), breaks=c(seq(0.00, 0.20, 0.05))) +
scale_y_continuous(limits = c(0.5, 6.5), breaks = c(seq(0.5, 6.5, 0.5))) + scale_fill_manual(values = c("#27B9B9", 
    "#EE7671")) + # ylim(0, 0.1) +
coord_flip()


width = 7
ggsave("ColonCAZy.pdf", width = width, height = 5)
