library(ggplot2)

datadf <- read.csv("./data/Muc2平均光密度plot.csv")
datadf$Colon_value = datadf$Colon_value * 100
datadf$Cecum_value = datadf$Cecum_value * 100

red = "#E46161"
blue = "#3377AE"
height <- 2
width <- 1.5
scale <- 1
datadf

boxplotsize = 0.3
boxaxissize = 0.3
colondf <- datadf[,c(1,2)]
colondf$group <- substring(colondf$Colon, 1, 4)
colondf$condition <- substring(colondf$Colon, 1, 1)
Colonp <- ggplot(colondf, aes(condition, Colon_value, fill=condition)) + 
  stat_boxplot(geom = "errorbar",width=0.75,size = boxplotsize)+
  geom_boxplot(size = boxplotsize, outlier.shape = NA) + 
  theme(
    #axis.text.x = element_text(angle = 40, vjust = 0.5, hjust = 0.9), 
    #legend.title = element_blank(),
    legend.position = "none",
    axis.text = element_text(colour = "black"),
    axis.line = element_line(size=boxaxissize)) +
  labs(x="") +
  ylim(25, 40) + 
  scale_fill_manual(values = c(red, blue)) +
  geom_point(position = "jitter", shape=21, size = 1, colour = "black", fill = "white")



cecumdf <- datadf[,c(3,4)]
cecumdf$group <- substring(cecumdf$Cecum, 1, 4)
cecumdf$condition <- substring(cecumdf$Cecum, 1, 1)
Cecump <- ggplot(cecumdf, aes(condition, Cecum_value, fill=condition)) + 
  stat_boxplot(geom = "errorbar",width=0.75,size = boxplotsize)+
  geom_boxplot(size = boxplotsize, outlier.shape = NA) + 
  theme(
    #axis.text.x = element_text(angle = 40, vjust = 0.5, hjust = 0.9), 
    #legend.title = element_blank(),
    axis.text = element_text(colour = "black"),
    axis.line = element_line(size=boxaxissize)) +
  labs(x="") +
  ylim(30, 45) +
  scale_fill_manual(values = c(red, blue)) +
  geom_point(position = "jitter", shape=21, size = 1, colour = "black", fill = "white")

library(patchwork)
Colonp + Cecump 
ggsave("lightdensity.pdf", width = 4.5, height = 3)
