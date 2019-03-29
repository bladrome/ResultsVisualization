library(ggplot2)
library(reshape2)

##################################### bacteria metabolishm
width = 2
height <- 2

boxplotsize <- 0.3
boxaxissize <- 0.3

k = 30

plotkegg <- function(pathwayfile) {
    
    # for dubug pathwayfile = 'data/Pathway_colon.csv'
    datadf <- read.csv(pathwayfile)
    # Top k
    datadf <- datadf[0:k, ]
    
    # No kegg num
    datadf <- datadf[-1]
    # No Total
    datadf = datadf[-length(colnames(datadf))]
    
    ## Row normalize Description <- datadf$Description datadf <- datadf[-1] /
    ## colSums(datadf[-1]) datadf <- datadf[-1] / rowSums(datadf[-1])
    ## datadf$Description <- Description
    
    Description <- datadf$Description
    datadf = datadf[-1]
    datadf$Description <- Description
    
    # sig test
    pvalue <- vector()
    for (i in 1:length(rownames(datadf))) {
        grazing <- datadf[i, 1:5]
        drylot <- datadf[i, 6:10]
        
        print(grazing)
        print(drylot)
        testres <- t.test(grazing, drylot)
        pvalue[[i]] <- testres$p.value
    }
    
    # datadf$pvalue <- pvalue datadf <- datadf[order(datadf$pvalue),]
    
    
    plotdata <- melt(datadf)
    plotdata$group <- substring(plotdata$variable, 1, 4)
    plotdata$condition <- substring(plotdata$variable, 1, 1)
    plotdata$organ <- substring(plotdata$variable, 3, 4)
    
    ggplot(plotdata, aes(factor(Description, levels = rev(datadf$Description)), value, 
        fill = factor(condition, levels = c("S", "F")))) + stat_boxplot(geom = "errorbar", 
        size = boxplotsize) + geom_boxplot(outlier.shape = NA, size = boxplotsize) + 
        # geom_point(position = position_dodge(), shape = 21, colour = 'black', fill =
    # 'white') +
    labs(x = "") + ggtitle(substring(pathwayfile, 14, 18)) + theme(axis.text.x = element_text(angle = 40, 
        vjust = 0.9, hjust = 0.9), legend.title = element_blank(), axis.text = element_text(colour = "black")) + 
        # scale_y_continuous(limits = c(0.00, 0.20), breaks=c(seq(0.00, 0.20, 0.05))) +
    scale_y_continuous(limits = c(0.5, 5.5), breaks = c(seq(0.5, 5.5, 0.5))) + scale_fill_manual(values = c("#27B9B9", 
        "#EE7671")) + # ylim(0, 0.1) +
    coord_flip()
    
}


width = 7
pathwayfile <- "data/Pathway_rumen.csv"
p <- plotkegg(pathwayfile)
ggsave("Rumensigbac.pdf", width = width, height = 5)
Rup <- p
pathwayfile <- "data/Pathway_cecum.csv"
p <- plotkegg(pathwayfile)
ggsave("Cecumsigbac.pdf", width = width, height = 5)
Cep <- p
pathwayfile <- "data/Pathway_colon.csv"
p <- plotkegg(pathwayfile)
ggsave("Colonsigbac.pdf", width = width, height = 5)
Cop <- p


library(patchwork)
Rup/Cep/Cop + plot_layout(ncol = 1)

ggsave("sigbac.pdf", width = 10, height = 30)
