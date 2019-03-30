library(ggplot2)
library(reshape2)

data = read.csv("data/lambnumber.csv")

t.test(data$BB, data$B.)

data = melt(data)

ggplot(data, aes(x=variable, y=value), group=variable) +
  scale_y_continuous(breaks = c(0, 1, 2, 3, 4)) +
  scale_x_discrete(labels=c("BB", "B+", "++")) +
  ylim(0, 4) +
  labs(x="FecBB mutation", y="Litter size") + 
  geom_boxplot()

plotsize = 3.2
ggsave("Tansheep.pdf", width =plotsize, height = plotsize)
