
library(ggplot2)
library(reshape2)
library(stringr)
library(RColorBrewer)

Ruabu <- "./Rumenabu/taxa.percents.table.xls"
Ceabu <- "./Cecumabu/taxa.percents.table.xls"
Coabu <- "./Colonabu/taxa.percents.table.xls"

newcol <- c("#9E0142",  "#D53E4F", "#F46D43", "#FDAE61", "#FEE08B", "#FFFFBF", "#E6F598", "#ABDDA4", "#66C2A5", "#3288BD", "#5E4FA2")

plotbar <- function(abufile, groupindex, organindex = "Rumen"){
  # test sort
  #abufile <- Coabu;groupindex <- 16
samabudf <- read.table(abufile, header = TRUE, sep = '\t')
samplenames <- colnames(samabudf)
taxnames <- samabudf$OTU.ID
samabudf <- samabudf[-1]
samabudf <- t(samabudf)
colnames(samabudf) <- taxnames
#samabudf <- data.frame(samabudf, samplenames = samplenames[-1])

#colnames(samabudf) <- str_extract(originames, regex("[^0-9]+"))

samabudf = data.frame(samabudf, group = str_extract(samplenames[-1], regex("[^0-9]+")))
print(colnames(samabudf))

#plotdf <- aggregate(samabudf, by = list(group), FUN=mean)
plotdf <- aggregate(x = samabudf[-groupindex], by = c(group=list(samabudf$group)), FUN = mean)
plotdf <- melt(plotdf)

stackorder <- unique(substring(plotdf$variable, 28))

if ( organindex == "Rumen" ){
  baseplot <- ggplot(plotdf, aes(x = factor(group, 
                                            levels = c("G_Ru", "S_Ru", "G_M_Ru", "S_M_Ru")),
                                 y = value, fill = factor(substring(variable, 28), 
                                                          levels = rev(stackorder) ))) 
}
if ( organindex == "Colon" ){
  baseplot <- ggplot(plotdf, aes(x = factor(group, 
                                            levels = c("G_Co", "S_Co", "G_M_Co", "S_M_Co")),
                                 y = value, fill = factor(substring(variable, 28), 
                                                          levels = rev(stackorder) )) )
}
if ( organindex == "Cecum" ){
  baseplot <- ggplot(plotdf, aes(x = factor(group, 
                                            levels = c("G_Ce", "S_Ce", "G_M_Ce", "S_M_Ce")),
                                 y = value, fill = factor(substring(variable, 28), 
                                                          levels = rev(stackorder) )) )
}
  baseplot + geom_col(position="stack") +
  scale_fill_manual(values = colorRampPalette(colors = newcol)(length(levels(plotdf$variable)))) +
  #scale_fill_manual(values = colorRampPalette(brewer.pal(8, "Dark2"))(length(factor(plotdf$variable)))) +
  #scale_fill_manual(values = colorRampPalette(brewer.pal(8, "Paired"))(length(levels(plotdf$variable)))) +
  #scale_fill_manual(values = colorRampPalette(colors = c("#A65728", "#F8ED39", "#F57F20", "#98509F", "#4EAF49",  "#88CDEA", "#397FB9", "#FAA51A", "#E21E26"))(length(levels(plotdf$variable)))) +
  #scale_fill_manual(values = colorRampPalette(colors = c("blue", "red"))(length(levels(plotdf$variable)))) +
  labs(x = "", y = "")  +
  theme_bw() +
  theme(legend.title = element_blank(),
        legend.position = "right",
        axis.text = element_text(color = "black"),
        axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5)
        #panel.background = element_blank()
        ) +# coord_flip() +
  scale_y_continuous(expand = expand_scale(0,0))
  #guide_legend(ncol = 2)

  #c("#A65728", "#F8ED39", "#F57F20", "#98509F", "#4EAF49", "#88CDEA", "#397FB9", "#FAA51A", "#E21E26")
}

width <- 4
height <- 7
abufile <- Ruabu;groupindex <- 16; organindex <- "Rumen"
plotbar(abufile = abufile, groupindex = groupindex, organindex)
ggsave(filename = "Rubar.pdf", width = width, height = height)

abufile <- Ceabu;groupindex <- 17; organindex <- "Cecum"
plotbar(abufile = abufile, groupindex = groupindex,organindex)
ggsave(filename = "Cebar.pdf", width = width, height = height)

abufile <- Coabu;groupindex <- 16; organindex <- "Colon"
plotbar(abufile = abufile, groupindex = groupindex, organindex)
ggsave(filename = "Cobar.pdf", width = width, height = height)