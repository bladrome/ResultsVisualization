library(ggplot2)
library(xlsx)
library(reshape2)

##################################### Methane metabolism


width = 2
height <- 2

boxplotsize = 0.3
boxaxissize = 0.3


# datadf <- read.xlsx(xlsxFile = './Pathway2018-12-21.xls', sheet = 1)
datadf <- xlsx::read.xlsx("data/Pathway2018-12-21.xls", sheetIndex = 1)

methane <- subset(datadf, Description == "Methane metabolism")

plotdata <- subset(methane, select = c(paste("F_LW", seq(1, 5), sep = ""), paste("S_LW", 
    seq(1, 5), sep = "")))

# plotdata = plotdata / methane$Total plotdata = plotdata / sum(plotdata)

# F_LW3 F_LW2 F_LW1 F_LW5 F_LW4 S_LW2 S_LW3 S_LW1 S_LW4 S_LW5 0.0079870627
# 0.0064789929 0.010865361 0.0082641738 0.0118866474 0.010911132 0.0129770994
# 0.0123849731 0.0120311737 0.0113533538

plotdata[1, ] = c(0.0108653610070224, 0.00647899286525562, 0.00798706266631188, 0.0118866474302925, 
    0.00826417380237604, 0.0123849730782434, 0.0109111319779077, 0.0129770993712004, 
    0.0120311737302916, 0.0113533537704428)


methane$Total
meltplotdata <- melt(plotdata)
meltplotdata$group = substring(meltplotdata$variable, 1, 4)

methp <- ggplot(meltplotdata, aes(group, value, fill = group)) + stat_boxplot(geom = "errorbar", 
    size = boxplotsize) + geom_boxplot(outlier.shape = NA, size = boxplotsize) + 
    geom_point(position = position_jitter(), shape = 21, colour = "black", fill = "white") + 
    labs(x = "") + ggtitle("Methane") + theme(axis.text.x = element_text(angle = 40, 
    vjust = 0.5, hjust = 0.5), legend.title = element_blank(), axis.text = element_text(colour = "black"))

ggsave("methane.pdf", width = 4, height = 4)


##################################### bacteria metabolishm


rumenmusk = c(paste("F_LW", seq(1, 5), sep = ""), paste("S_LW", seq(1, 5), sep = ""))

datadf <- xlsx::read.xlsx("data/NRbac2.xlsx", sheetIndex = 1)
plotdata <- aggregate(datadf[, 9:length(colnames(datadf))], list(bacteria = datadf$Genus), 
    sum)



plotbacfun <- function() {
    bacplot <- subset(bacvector, select = rumenmusk)
    bacplot = bacplot/bacvector$Total
    
    bacplot <- melt(bacplot)
    bacplot$group = substring(bacplot$variable, 1, 4)
    
    p <- ggplot(bacplot, aes(group, value, fill = group)) + stat_boxplot(geom = "errorbar", 
        size = boxplotsize) + geom_boxplot(outlier.shape = NA, size = boxplotsize) + 
        geom_point(position = position_jitter(), shape = 21, colour = "black", fill = "white") + 
        labs(x = "") + theme(axis.text.x = element_text(angle = 40, vjust = 0.5, 
        hjust = 0.5), legend.title = element_blank(), axis.text = element_text(colour = "black"))
    
    ggsave(filesave, width = width, height = height)
    p
}


plotdata$bacteria

bacvector <- subset(plotdata, bacteria == "g__Alistipes")
filesave <- "g_Alis.pdf"
Alisp <- plotbacfun()
bacvector <- subset(plotdata, bacteria == "g__Methanobrevibacter")
filesave <- "g_Meth.pdf"
Methp <- plotbacfun()



library(patchwork)
p <- methp + Alisp + Methp
ggsave("figure3_boxplot.pdf", width = 8, height = 3)
