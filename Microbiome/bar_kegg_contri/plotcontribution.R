
library(ggplot2)
library(reshape2)
library(xlsx)

newcol <- c("#9E0142",  "#D53E4F", "#F46D43", "#FDAE61", "#FEE08B", "#FFFFBF", "#E6F598", "#ABDDA4", "#66C2A5", "#3288BD", "#5E4FA2")

datadf <- read.xlsx("data/KEGG2018-12-26.xls", sheetIndex = 1)

rumenmusk <- c(
  "Function",
  "Taxon",
  paste0(rep("F_LW", 5), seq(1,5)),
  paste0(rep("S_LW", 5), seq(1,5))
)

rumendf <- subset(datadf, Function == "Methane metabolism", select = rumenmusk)

Frumendf <- rumendf[,3:7]
Srumendf <- rumendf[,8:12]

plotdf <- data.frame(
  #Function=rumendf$Function, 
  Taxon=substring(rumendf$Taxon, 4), 
  Grazing=rowSums(Frumendf), 
  Drylot=rowSums(Srumendf)
  )
plotdf$Grazing = plotdf$Grazing / sum(plotdf$Grazing)
plotdf$Drylot = plotdf$Drylot / sum(plotdf$Drylot)

plotdf <- melt(plotdf)
ggplot(plotdf, aes(variable, value, 
                   #fill = Taxon))+
                   fill = factor(Taxon, levels = c("ers", "Butyrivibrio", "Ruminococcus", 
                                                   "Methanobrevibacter", "Clostridium", 
                                                   "unclassified_f__Lachnospiraceae", 
                                                   "unclassified_p__Firmicutes", 
                                                   "Selenomonas", "Bacteroides", 
                                                   "unclassified_d__Bacteria", 
                                                   "Prevotella"))
                   ))+
  geom_col(position="stack") +
  scale_fill_manual(values = colorRampPalette(colors = newcol)(length(levels(plotdf$Taxon)))) +
  labs(x = "", y = "")  +
  ggtitle("Methane metabolism") +
  #theme_bw() +
  theme(legend.title = element_blank(),
        legend.position = "right",
        axis.text = element_text(color = "black"),
        axis.text.x = element_text(angle = 40, hjust = 0.9, vjust = 0.9),
        panel.background = element_blank()
  ) +# coord_flip() +
  scale_y_continuous(expand = expand_scale(0,0)) +
  scale_x_discrete(expand = expand_scale(0,0))

ggsave("methane.pdf", width = 4, height = 5)
