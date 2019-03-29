
library(ggplot2)
library(reshape2)
library(stringr)
library(RColorBrewer)

phylumabu <- "./data/taxa.percents.table.CSV"

newcol <- c("#9E0142",  "#D53E4F", "#F46D43", "#FDAE61", "#FEE08B", "#FFFFBF", "#E6F598", "#ABDDA4", "#66C2A5", "#3288BD", "#5E4FA2")

samabudf <- read.csv(phylumabu, header = TRUE)
samabudf$OTU.ID <- substring(samabudf$OTU.ID, 28)
plotdf <- melt(samabudf)

baseplot <- ggplot(plotdf, aes(x = factor(variable, 
                                          levels = c("G_Ru", "S_Ru", "G_M_Ru", "S_M_Ru",
                                                     "G_Co", "S_Co", "G_M_Co", "S_M_Co",
                                                     "G_Ce", "S_Ce", "G_M_Ce", "S_M_Ce"),
                                          
                              ), y = value,
                              fill=factor(OTU.ID, levels = rev(samabudf$OTU.ID))))
baseplot + geom_col(position="stack") +
scale_fill_manual(values = colorRampPalette(colors = newcol)(length(levels(plotdf$variable)))) +
labs(x = "", y = "")  +
theme_bw() +
theme(legend.title = element_blank(),
      legend.position = "right",
      axis.text = element_text(color = "black"),
      axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5)
      #panel.background = element_blank()
      ) +# coord_flip() +
scale_y_continuous(expand = expand_scale(0,0))


width <- 7
height <- 6
ggsave(filename = "phylumbar.pdf", width = width, height = height)
