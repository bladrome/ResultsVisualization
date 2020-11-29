library(ggplot2)
library(reshape2)
library(stringr)
library(forcats)
library(purrr)

boxplotsize=0.1
width = 12
height = 8

args <- commandArgs()
infilename <- "./phlya-tongdata22.csv"
top <- 20

outfilename <- "output.pdf"
titlename <- ""

datadf <- read.csv(infilename, stringsAsFactors=FALSE)
xorder <- colnames(datadf)[-1]


taxtotal <- rowSums(datadf[-1])
colnames(datadf)[1] <- "taxlevel"
datadf[-1] = scale(datadf[-1], center=FALSE, scale=colSums(datadf[-1]))

head(colSums(datadf[,-1]))

topindex = order(taxtotal, decreasing=TRUE)
datadf <- datadf[topindex,]
datadf <- datadf[0:top,]


print("-----------------------------------")
datadf$taxlevel = unlist(map(str_split(datadf$taxlevel[0:top], ';'), function(x){x[6]}))
datadf$taxlevel = unlist(map(datadf$taxlevel, function(x) {substring(x, 4)}))
legendorder <- datadf$taxlevel
pltdf <- melt(datadf)
pltdf$value = pltdf$value * 100

pltdf$variable <- substring(pltdf$variable, 2)
pltdf$group <- substring(pltdf$variable, 2, 3)
print(head(pltdf))
print(tail(pltdf))

## ggplot(pltdf, aes(factor(taxlevel, levels=rev(datadf$taxlevel)), value, fill=group)) +
ggplot(pltdf, aes(variable, value,
                  fill=factor(taxlevel, level=rev(legendorder)))) +
  geom_bar(stat='identity', position='fill') +
  ## geom_bar(position='stack') +
  labs(x = "",
       y = "") +
  theme(legend.title = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        axis.text = element_text(colour = "black")) +
  #ylim(0, 0.2) +
  ggtitle(titlename) +
  scale_fill_manual(values =
                    colorRampPalette(
                      #colors = c("#599D75", "#4172AF", "#D49F35", "#D49F35")
                      colors = rev(c("#5E4FA2","#E5B210", "#9E0142","#0EA01F", "#0A76E2", "#EA0F82"))
                    )(20)) +
  expand_limits(x=0, y=0) +
  scale_y_continuous(expand = c(0, 0),
                     labels=scales::percent)
  ## scale_x_discrete(expand = c(0, 0))
  ## coord_flip()

ggsave(filename = outfilename, width = width, height = height)

## ggplot(plotdata, aes(factor(Family, levels = rev(datadf$Family)), value, fill = factor(condition,levels = c("S", "F")))) +
##   stat_boxplot(geom = "errorbar", size = boxplotsize) +
##   geom_boxplot(outlier.shape = NA, size = boxplotsize) +
##   geom_point(position = position_dodge(), shape = 21, colour = 'black') +
##   labs(x = "") +
##   scale_y_continuous(limits = c(0.5, 6.5), breaks = c(seq(0.5, 6.5, 0.5))) + scale_fill_manual(values = c("#27B9B9",
##     "#EE7671")) + # ylim(0, 0.1) +
## coord_flip()
