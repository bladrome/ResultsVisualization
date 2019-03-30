library(ggplot2)
library(reshape2)
library(scales)

data = read.csv("data/horntype.csv")

data = data[-2:-3]

# data = data.frame(data[1], data[-1] / rowSums(data[-1]) * 100)

p <<-  list()
for (i in 1:length(rownames(data))) {
  # print(data[i,])
  dfp <- data.frame(data[i,])
  dfp <- melt(dfp)
  # print(dfp)
  p[[i]] <- ggplot(dfp, aes(x = "", y = value, fill = variable)) +
    geom_bar(width = 1, stat = "identity") +
    theme_bw() +
    theme(panel.border = element_blank(),
          panel.grid = element_blank(),
          axis.text = element_blank()) +
    ggtitle(dfp$Breed[1]) +
    labs(x="", y="") +
    scale_fill_manual(values = c("#6BAED6", "#FB6A4A", "#FFC427")) +
    #geom_text(aes(y = value / 2,label = percent(value / 100)), size = 5) +
                  #label = value), size = 5) +
    coord_polar("y", start = 0)
  #print(p[[i]])
  #ggsave(paste(dfp$Breed[1], i, ".pdf", sep = ''))
}

library(cowplot)
plot_grid(p[[1]], p[[2]], p[[3]], p[[4]], 
          p[[5]],p[[5]], p[[6]], p[[7]], p[[8]], p[[9]], p[[10]],p[[11]], nrow = 2)

ggsave("pie.pdf",width=20,height=5)
