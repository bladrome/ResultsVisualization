library(ggplot2)
library(patchwork)
library(RColorBrewer)
library(reshape2)
datadf = read.csv("./weightpH.csv")

red = "#E46161"
blue = "#3377AE"

height <- 2
width <- 1.5
scale <- 1
datadf


metadatadf = read.csv("./metabolize.csv")
#metadatadf = unname(metadatadf)
#Aceticacid = cbind(metadatadf[1:3], value = metadatadf[4], metabolize = rep("Acetic acid", 30))
#Butyricacid = cbind(metadatadf[1:3], value = metadatadf[5], metabolize = rep("Butyric acid", 30))
#Propionicacid = cbind(metadatadf[1:3], value = metadatadf[6], metabolize = rep("Propionic acid", 30))
#metadatadf = rbind(Aceticacid, Butyricacid, Propionicacid)
#colnames(metadatadf) = c("position", "condition", "sample", "value", "metabolize")


height <- 2
width <- 1.5
scale <- 1
datadf

boxplotsize = 0.3
boxaxissize = 0.3

birthplot = ggplot(datadf, mapping = aes(x = rev(group), y = birth, fill = group)) + 
  #geom_errorbar(aes(ymin = min(birth), ymax = max(birth)), width = .1) +
  stat_boxplot(geom = "errorbar",width=0.75,aes(color=birth),size = boxplotsize)+
  geom_boxplot(size = boxplotsize,outlier.shape = NA) + 
  #geom_point(position = "jitter",  shape=1, size = 1) +
  #geom_point(position = "jitter",  colour = "white", size = 0.2) +
  theme_classic() +
  labs( x = "",
        y = "Birth weight (kg)") +
  ylim(2.0, 3.5) +
  theme(axis.text.x = element_text(angle = 40, vjust = 0.5, hjust = 0.9), 
        legend.title = element_blank(),
        axis.text = element_text(colour = "black"),
        axis.line = element_line(size=boxaxissize)) +
  scale_fill_manual(values = c(blue, red))  +
  geom_point(position = "jitter", shape=21, size = 1, colour = "black", fill = "white") 
birthplot
ggsave(filename = "Birthweight.pdf", width = width, height = height, scale = scale)

######################################################################

yearplot = ggplot(datadf, mapping = aes(x = rev(group), y = year, fill = group)) + 
  stat_boxplot(geom = "errorbar",width=0.75,aes(color=birth),size = boxplotsize)+
  geom_boxplot(size = boxplotsize, outlier.shape = NA) + 
  theme_classic() +
  ylim(12, 45)+
  theme(axis.text.x = element_text(angle = 40, vjust = 0.5, hjust = 0.9), 
        #legend.box = element_blank(), 
        legend.title = element_blank(),
        axis.text = element_text(colour = "black"),
        axis.line = element_line(size=boxaxissize)) +
  # scale_fill_manual(values = colorRampPalette(brewer.pal(8, "Accent"))(length(factor(datadf$group)))) +
  labs( y = "Yearly weight (kg)", x = "")+
  scale_fill_manual(values = c(blue, red)) +
  geom_point(position = "jitter", shape=21, size = 1, colour = "black", fill = "white") 
yearplot
ggsave(filename = "Yearlyweight.pdf", width = width, height = height)

######################################################################




########################## Rumen Metabolize
metaplot <- function(plotdatadf){
  
metabolizeplot = ggplot(plotdatadf, mapping = aes(x = variable, 
                                                  y = value, 
                                                  fill = factor(condition,
                                                                levels = c("Grazing", "Drylot")))) +
  #geom_col(position = "dodge") +
  #geom_boxplot() + 
  geom_violin() +
  #geom_jitter(width = 0.1) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 35, vjust = 0.9, hjust = 0.99), 
        legend.title = element_blank()) +
  #scale_fill_manual(values = colorRampPalette(brewer.pal(8, "Accent"))(length(factor(datadf$condition)))) +
  labs(y = "Proportion (%)")
  #ylim(0, 1)
metabolizeplot
}

plotdatadf = subset(metadatadf,  position == "Rumen")
plotdatadf
metaplot(melt(plotdatadf))
ggsave(filename = "Rumenmetabolizesplot.pdf",
       width = width, height = height, scale = scale)

plotdatadf = subset(metadatadf,  position == "Cecum")
plotdatadf
metaplot(melt(plotdatadf))
ggsave(filename = "Cecummetabolizesplot.pdf",
       width = width, height = height, scale = scale)

plotdatadf = subset(metadatadf,  position == "Colon")
plotdatadf
metaplot(melt(plotdatadf))
ggsave(filename = "Colonmetabolizesplot.pdf", 
       width = width, height = height, scale = scale)




###############PH

lwph = data.frame(group = datadf$group, position = rep("Rumen", 10), ph = datadf$lwph)
jcph = data.frame(group = datadf$group, position = rep("Colon", 10), ph = datadf$jcph)
mcph = data.frame(group = datadf$group, position = rep("Cecum", 10), ph = datadf$mcph)
phdatadf = rbind(lwph, mcph, jcph)

t.test(lwph$ph[1:5], lwph$ph[6:10])$p.value
t.test(jcph$ph[1:5], jcph$ph[6:10])$p.value
t.test(mcph$ph[1:5], mcph$ph[6:10])$p.value


#    group  lwph  mcph  jcph
#1  Drylot 5.064 6.636 6.672
#2 Grazing 6.172 6.624 6.534

#    group variable value
#1  Drylot     lwph 5.064
#2 Grazing     lwph 6.172
#3  Drylot     mcph 6.636
#4 Grazing     mcph 6.624
#5  Drylot     jcph 6.672
#6 Grazing     jcph 6.534
phdf = datadf[-3:-5]
phmeandf = aggregate(phdf[-1:-2], by=list(group=phdf$group), mean)
#X   group num birth  year lwph mcph jcph
#1   1 Grazing F-1  2.30 13.00 5.98 6.78 6.57
#2   2 Grazing F-2  2.40 12.70 6.38 6.47 6.30
#3   3 Grazing F-3  2.35 14.00 6.28 6.65 6.65
#4   4 Grazing F-4  2.10 17.50 6.09 6.63 6.50
#5   5 Grazing F-5  2.40 16.35 6.13 6.59 6.65
#6   6  Drylot S-1  2.50 20.40 5.17 6.44 6.66
#7   7  Drylot S-2  2.50 24.50 4.81 6.65 6.66
#8   8  Drylot S-3  2.20 22.00 5.54 6.77 6.79
#9   9  Drylot S-4  2.10 18.30 4.75 6.58 6.52
#10 10  Drylot S-5  2.40 24.00 5.05 6.74 6.73
sd1 = sd(phdf$lwph[6:10])
sd2 = sd(phdf$lwph[1:5])
sd3 = sd(phdf$mcph[6:10])
sd4 = sd(phdf$mcph[1:5])
sd5 = sd(phdf$jcph[6:10])
sd6 = sd(phdf$jcph[1:5])
stard = c(sd1, sd2, sd3, sd4, sd5, sd6)
phdf <- cbind(melt(phmeandf), sd=stard)

phplot = ggplot(phdf, mapping = aes(x = variable, y = value, fill = factor(group, levels = c("Grazing", "Drylot")))) + 
  geom_col(position = position_dodge2(0.3)) + 
  geom_errorbar(aes(ymin = value, ymax = value + sd), 
                position = position_dodge2(width = 0.5, padding = 0.5),size = boxplotsize) +
  theme_classic() +
  labs( y = "pH value", x = "")+
  scale_fill_manual(values = c(red, blue)) +
  ylim(0,10) + 
  theme(axis.text.x = element_text(angle = 35, vjust = 0.9, hjust = 0.99), 
        legend.title = element_blank(),
        axis.text = element_text(colour = "black"),
        axis.line = element_line(size=boxaxissize)) +
  ylim(0, 10) +
  scale_y_continuous(expand = c(0,0), limits = c(0, 10))

phplot
ggsave(filename = "phplot.pdf", width = width, height = height, scale = scale)


height <- 3
width <- 3
scale <- 1

metaplot  <- function(df, variable, barselectcolvar = "A"){
  # for debug
  #df = metadatadf
  #variable = metadatadf$Butyric.acid
  #barselectcolvar = "B"
  
  bardf <- aggregate(df, by = list(position = df$position, 
                                           condition = df$condition), FUN=mean)[c(1,2,6,7,8)]
  print(bardf)
  if (barselectcolvar == "A"){
    barselectcol = 3
  } else if (barselectcolvar == "B"){
    barselectcol = 5
  } else if (barselectcolvar == "P"){
    barselectcol = 4
  }
  
  
ggplot() + 
  
#  stat_boxplot(data = metadatadf, 
#           aes(x = factor(metadatadf$position, levels = c("Rumen", "Cecum", "Colon")),
#              #group = factor(metadatadf$position, levels = c("Rumen", "Cecum", "Colon")),
#               group = substring(metadatadf$sample, 1,3),
#               fill = factor(metadatadf$condition, levels = c("Grazing", "Drylot")),
#               y = variable),
#           position = position_dodge(width = 0.5),
#           geom = "errorbar",width=0.75 , size=boxplotsize )+
  geom_bar(data = bardf, 
           aes(x = factor(bardf$position, levels = c("Rumen", "Cecum", "Colon")),
               fill= factor(bardf$condition, levels = c("Grazing", "Drylot")), 
               y = bardf[,barselectcol]),
           stat = "identity", position = position_dodge2()) + 
  geom_point(data = metadatadf, 
             aes(x = metadatadf$position, 
                 #fill= factor(metadatadf$condition, levels = c("Drylot", "Grazing")), 
                 colour= factor(metadatadf$condition, levels = c("Grazing", "Drylot")), 
                 y = variable),
             shape = 21, 
             fill="white",
             position = position_jitterdodge()) +
  theme_classic() +
  theme(
    axis.text = element_text(colour = "black"), 
    axis.text.x = element_text(angle = 35, vjust = 0.9, hjust = 0.99),
    legend.title = element_blank(),
    legend.position = "none",
    axis.line = element_line(size=boxaxissize)
    ) +
  labs(x = "") +
  scale_y_continuous(expand = expand_scale(0,0)) +
  scale_fill_manual(values = c(red, blue))  
}


metaA <- metaplot(metadatadf, metadatadf$Acetic.acid, "A") + expand_limits(y=c(0.58, 1.0))
ggsave(filename = "ColonmetabolizesplotA.pdf", 
       width = width, height = height, scale = scale)

metaP <- metaplot(metadatadf, metadatadf$Propionic.acid, "P") + expand_limits(y=c(0.11, 0.35))
ggsave(filename = "ColonmetabolizesplotP.pdf",
       width = width, height = height, scale = scale)

metaB <- metaplot(metadatadf, metadatadf$Butyric.acid, "B") + expand_limits(y=c(0.025, 0.17))
ggsave(filename = "ColonmetabolizesplotB.pdf",
       width = width, height = height, scale = scale)


#(plot_spacer() | birthplot | yearplot | phplot) /
  #(metaA | metaB | metaP)
  

(metaA + birthplot + yearplot + phplot + plot_layout(nrow = 1)) / 
(metaA + metaP + metaB + plot_layout(nrow = 1))  + plot_layout(ncol = 1)
ggsave("Rplot.pdf", width = 8, height = 5)
