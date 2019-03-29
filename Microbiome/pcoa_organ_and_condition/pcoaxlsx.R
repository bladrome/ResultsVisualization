library(stringr)
library(ggbiplot)
library(ape)
Allpcoafile = "./data/bray_curtis_taxa.table.xls"


allpcoaplot <- function(filepath){
  # filepath <- Rumenpcoafile
  unweightdata <- read.table(filepath)
  pcoares <- pcoa(unweightdata)
  xpc1 <- pcoares$values$Relative_eig[1] * 100
  ypc2 <- pcoares$values$Relative_eig[2] * 100
  Ddf <- pcoares$vectors
  Ddf <- data.frame(Ddf)
  ## When all, group should extract in range(1, 6)
  #print(stringr::str_extract(colnames(unweightdata), regex(".*[^0-9].*")))
  Ddf$group <- stringr::str_extract(colnames(unweightdata), regex(".*[^0-9]"))
  Ddf$condition <- substring(Ddf$group, 1, 1)
  Ddf$group <- substring(Ddf$group, 3)
  print(Ddf$condition)
  print(Ddf$group)
  
  ggplot(Ddf, aes(Ddf$Axis.1, Ddf$Axis.2, shape = condition,colour=group, fill = group)) + 
  #ggplot() +
    #geom_point(data = Ddf, aes(x = Ddf$Axis.1, y = Ddf$Axis.2, shape=condition), size=2.5) + 
    geom_point(colour = "white", size = 4) +  # for margin white
    #geom_point(size = 2.5) + ## for legend color
    #geom_point(aes(shape=condition), size=2.5) + 
    #geom_point(aes(shape=Ddf$condition,colour = "white"), size=1.5) + 
    #geom_point(shape=21, size=0.7, fill = "white", color = "white", alpha = 0.75) + 
    #stat_ellipse(linetype=2) +
    labs(x = paste("PC1", " ", "(", round(xpc1,2), "%)", sep = ""), 
         y = paste("PC2", " ", "(", round(ypc2,2), "%)", sep = "")) +
    #theme_bw() +
    theme(
      axis.text = element_text(color = "black")
    )  +
    scale_shape_manual(values = c(21, 24))  +
  #geom_text(aes(x = Ddf$Axis.1 + 0.1),label = rownames(pcoares$vectors), size = 2)
  
    scale_fill_manual(values = 
                        colorRampPalette(
                          #colors = c("#599D75", "#4172AF", "#D49F35", "#D49F35")
                          colors = c("#0EA01F", "#0A76E2", "#EA0F82", "#E5B210", "#9E0142", "#5E4FA2")
                          )(6)) +
    scale_color_manual(values = 
                        colorRampPalette(
                          #colors = c("#599D75", "#4172AF", "#D49F35", "#D49F35")
                          colors = c("#0EA01F", "#0A76E2", "#EA0F82", "#E5B210", "#9E0142", "#5E4FA2")
                          )(6))
    #barcol <- c("#9E0142", "#5E4FA2")
}

allpcoaplot(Allpcoafile)


width = 3.5;
height = 2.5;
pcap <- allpcoaplot(Allpcoafile) + ggtitle("All")
Allp <- pcap
ggsave(filename = "AllPCoA.pdf", width = width, height = height)
