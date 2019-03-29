library(ggbiplot)
pcadata <- read.csv("./unweighted_unifrac_otu_file.csv")
pcadata_num = pcadata[-1]
pcadata_num = prcomp(pcadata_num, scale. = TRUE)

pcadata_num$group <- substr(pcadata$X, 1, 3)
ggbiplot(pcadata_num, obs.scale = 1, var.scale = 1, var.axes = FALSE, varname.size = 0, 
    ellipse = TRUE, group = pcadata_num$group)  #+
# scale_color_discrete(name = '') + theme(legend.direction = 'horizontal',
# legend.position = 'top')




### site
pcasite <- read.csv("./pcoa_sites.csv")
pcasite$group <- substr(pcasite$X, 1, 3)
pcaplot <- ggplot(pcasite, aes(x = PC1, y = PC2)) + geom_point(aes(colour = group), 
    size = 5) + scale_y_continuous(breaks = seq(-0.2, 0.2, 0.05)) + scale_x_continuous(breaks = seq(-0.2, 
    0.2, 0.05)) + labs(x = "PC1 (26.48%)", y = "PC2 (17.46%)") + theme_bw()
pcaplot

