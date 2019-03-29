library(stringr)
library(ggbiplot)
library(ape)


#599D75
#4172AF
#D49F35
#D49F35

#Rumenpcoafile = "./Ru185446251/unweighted_unifrac_otu_file.xls"
#Cecumpcoafile = "./Ce193701914/unweighted_unifrac_otu_file.xls.xls"
#Colonpcoafile = "./Co193801067/unweighted_unifrac_otu_file.xls.xls"
#Allpcoafile = "./pcoaall/unweighted_unifrac_otu_file.xls.xls"

Rumenlumenfile <- "./Rumenlumen7345/unweighted_unifrac_otu_file.xls.xls"
Rumenmucosafile <- "./Rumenmucosa6733/unweighted_unifrac_otu_file.xls.xls"

Cecumlumenfile <- "./Cecumlumen3638/unweighted_unifrac_otu_file.xls.xls"
Cecummucosafile <- "./Cecummucosa1843/unweighted_unifrac_otu_file.xls.xls"

Colonlumenfile <- "./Colonlumen9917/unweighted_unifrac_otu_file.xls.xls"
Colonmucosafile <- "./Colonmucosa1987/unweighted_unifrac_otu_file.xls.xls"


lumenfile <- "./lumen5640/unweighted_unifrac_otu_file.xls.xls"
mucosafile <- "./mucosa0801/unweighted_unifrac_otu_file.xls.xls"

pcoaplot <- function(filepath, organ = "lumen"){
 # filepath <- Rumenpcoafile
  unweightdata <- read.table(filepath)
  pcoares <- pcoa(unweightdata)
  xpc1 <- pcoares$values$Relative_eig[1] * 100
  ypc2 <- pcoares$values$Relative_eig[2] * 100
  Ddf <- pcoares$vectors
  Ddf <- data.frame(Ddf)
  ## When all, group should extract in range(1, 6)
  if (organ == "lumen"){
    Ddf$group <- substr(colnames(unweightdata), 1, 4)
  }
  else{
    Ddf$group <- substr(colnames(unweightdata), 1, 6)
  }
  ggplot(Ddf, aes(Ddf$Axis.1, Ddf$Axis.2, fill = group)) + 
           #geom_point(aes(fill = c("#599D75", "#4172AF", "#D49F35", "#D49F35")), shape=16, size=2) + 
           geom_point(shape = 21, colour = "white", size=3) + 
           #geom_point(shape=21, size=0.7, fill = "white", color = "white", alpha = 0.75) + 
           #stat_ellipse(aes(colour=group), linetype=2) +
    labs(x = paste("PC1", " ", "(", round(xpc1,2), "%)", sep = ""), 
         y = paste("PC2", " ", "(", round(ypc2,2), "%)", sep = "")) +
    theme(
      axis.text = element_text(color = "black")
    )  +
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
}

width = 5;
height = 4;



pcap <- pcoaplot(lumenfile, "lumen") + ggtitle("lumen")
lumenp <- pcap
ggsave(filename = "lumenPCoA.pdf", width = width, height = height)

pcap <- pcoaplot(mucosafile, "mucosa") + ggtitle("mucosa")
mucosap <- pcap
ggsave(filename = "mucosaPCoA.pdf", width = width, height = height)


library(patchwork)
p <- ((lumenp) + (mucosap))

ggsave("PCoALumenMucosa.pdf", p, width =7, height = 2.5)
ggsave("PCoALumenMucosa.svg", p, width =7, height = 2.5)
#ggsave("lumenPCoArogan.pdf", p, width =7, height = 16)



######  1-6
pcap <- pcoaplot(Rumenlumenfile) + ggtitle("Rumen lumen")
Rumenp <- pcap
ggsave(filename = "RumenlumenPCoA.pdf", width = width, height = height)
pcap <- pcoaplot(Cecumlumenfile) + ggtitle("Cecum lumen")
Cecump <- pcap
ggsave(filename = "CecumlumenPCoA.pdf", width = width, height = height)
pcap <- pcoaplot(Colonlumenfile) + ggtitle("Colon lumen")
Colonp <- pcap
ggsave(filename = "ColonlumenPCoA.pdf", width = width, height = height)

pcap <- pcoaplot(Rumenmucosafile) + ggtitle("Rumen mucosa")
RumenMp <- pcap
ggsave(filename = "RumenmucosaPCoA.pdf", width = width, height = height)
pcap <- pcoaplot(Cecummucosafile) + ggtitle("Cecum mucosa")
CecumMp <- pcap
ggsave(filename = "CecummucosaCoA.pdf", width = width, height = height)
pcap <- pcoaplot(Colonmucosafile) + ggtitle("Colon mucosa")
ColonMp <- pcap
ggsave(filename = "ColonmucosaPCoA.pdf", width = width, height = height)


library(patchwork)
p <- ((Rumenp) + (RumenMp) + (Cecump) + (CecumMp) + (Colonp) + (ColonMp) + plot_layout(ncol = 2))

ggsave("PCoAorgan.pdf", p, width =7, height = 8)
#ggsave("lumenPCoArogan.pdf", p, width =7, height = 16)
