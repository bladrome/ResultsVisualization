library(ggplot2)
library(stringr)

red = "#E46161"
blue = "#3377AE"
redblue = "#9368C0"

height <- 2
width <- 1.5
scale <- 1

boxplotsize = 0.3
boxaxissize = 0.3

alldatadf <- read.csv("./data/quantify_marking_plot.csv")


plotdatadf <- function(datadf) {
    
    datadf$group <- str_extract(datadf$sample, regex(".*[^0-9]"))
    datadf$condition <- gsub("N-", "", str_extract(datadf$sample, regex(".*-")))
    
    datadf$group <- gsub("LW", "Rumen", datadf$group)
    datadf$group <- gsub("MC", "Cecum", datadf$group)
    datadf$group <- gsub("JC", "Colon", datadf$group)
    datadf$group <- gsub("F", "Grazing", datadf$group)
    datadf$group <- gsub("S", "Drylot", datadf$group)
    
    position <- str_extract(datadf$sample, regex("...?[0-9]"))
    datadf$position <- str_extract(position, regex(".*[^0-9]"))
    
    datadf$group
    
    ggplot(datadf, aes(factor(group, levels = c("Grazing-Rumen", "Drylot-Rumen", 
        "N-Grazing-Rumen", "N-Drylot-Rumen", "Grazing-Cecum", "Drylot-Cecum", "N-Grazing-Cecum", 
        "N-Drylot-Cecum", "Grazing-Colon", "Drylot-Colon", "N-Grazing-Colon", "N-Drylot-Colon")), 
        value, fill = condition)) + stat_boxplot(geom = "errorbar", width = 0.75, 
        size = boxplotsize) + geom_boxplot(size = boxplotsize, outlier.shape = NA) + 
        # geom_point() +
    theme_light() + theme(panel.grid = element_blank(), axis.text.x = element_text(angle = 40, 
        hjust = 0.9, vjust = 0.9), axis.text = element_text(colour = "black"), axis.ticks = element_line(colour = "black")) + 
        scale_fill_manual(values = c(red, blue)) + labs(x = "", y = "")
    
}

datadf = alldatadf[1:30, ]
lumenp <- plotdatadf(datadf)
datadf = alldatadf[31:60, ]
mucosap <- plotdatadf(datadf)

library(patchwork)
lumenp + mucosap

ggsave("quantity_compose.pdf", width = 8, height = 4)
