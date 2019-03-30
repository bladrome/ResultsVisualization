library(vegan)
library(ggplot2)
library(stringr)
library(RColorBrewer)
library(xlsx)

red = "#E46161"
blue = "#3377AE"

width <- 2
height <- 2

shannnonplot <- function(trdf, filename) {
  shannonindex <- diversity(trdf)
  plotdf <- data.frame(sample = colnames(t(trdf)))
  plotdf$shannonindex <- (shannonindex)
  plotdf$group <- str_extract(plotdf$sample, regex("[^0-9]+"))
  plotdf$organ <- substring(plotdf$group, 1, 1)
  
  p <-
    ggplot(plotdf, aes(factor(
      group, levels = c("F_LW", "S_LW", "F_MC", "S_MC", "F_JC", "S_JC")
    ), shannonindex, fill = organ)) +
    geom_boxplot() +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 40, vjust = 0.5)) +
    labs(x = "", y = "Shannon Index") +
    #scale_fill_manual(values = colorRampPalette( colors = c("#A65728", "#F8ED39", "#F57F20", "#98509F", "#4EAF49",  "#88CDEA", "#397FB9", "#FAA51A", "#E21E26")) (length(unique(plotdf$group))))
    #scale_fill_manual(values = colorRampPalette( colors = c(red, blue, red, blue, red, blue, red, blue, red, blue)) (length(unique(plotdf$group))))
    scale_fill_manual(values = c(red, blue))
  ggsave(
    filename = filename,
    width = width,
    height = height,
    scale = 2
  )
  p
}

cutheadtail <- function(X, headnum, tailnum) {
  for (i in 1:headnum) {
    X = X[-1]
  }
  for (i in 1:tailnum) {
    X = X[-length(colnames(X))]
  }
  if ("N_F_LW1" %in% colnames(X)) {
    X <- subset(X, select = -N_F_LW1)
  }
  t(X)
}

#### NR
df <- read.csv("./NR/data0110/tax_p.xls", sep = '\t')
colnames(df)
trdf <- cutheadtail(df, 1, 1)
nrplot <- shannnonplot(trdf, "NRSI_p.pdf")


df <- read.csv("./NR/data0110/tax_g.xls", sep = '\t')
colnames(df)
trdf <- cutheadtail(df, 1, 1)
nrplot <- shannnonplot(trdf, "NRSI_g.pdf")



#### KEGG
df <- read.csv("./KEGG/Pathway.CSV")
colnames(df)
trdf <- cutheadtail(df, 2, 2)
keggplot <- shannnonplot(trdf, "KEGGSI.pdf")


library(patchwork)
p <- (nrplot | nrplot | keggplot)
ggsave("Figure2A.pdf", p, width = 8, height = 3)


#### GENE
#datatoi
#df <- read.csv("../../../Biodata/MJ31/reads_number.xls", sep = '\t')
#colnames(df)
#trdf <- cutheadtail(df, 1, 1)
#shannnonplot(trdf, "GENESI.pdf")

### COG
df <- read.xlsx("COG/COG_Function2018-12-13.xls", sheetIndex = 1)
colnames(df)
trdf <- cutheadtail(df, 2, 1)
shannnonplot(trdf, "COGSI.pdf")

### VFDB
df <- read.xlsx("VFDB/核心毒力因子丰度统计(Core)2018-12-13.xls", sheetIndex = 1)
colnames(df)
trdf <- cutheadtail(df, 2, 4)
shannnonplot(trdf, "VFDBSI.pdf")

### CARD
df <- read.xlsx("CARD/ARO丰度表2018-12-13.xls", sheetIndex = 1)
colnames(df)
trdf <- cutheadtail(df, 3, 1)
shannnonplot(trdf, "CARDSI.pdf")

### CAZYme
df <- read.xlsx("CAZYME/CAZy Family丰度表2018-12-13.xls", sheetIndex = 1)
colnames(df)
trdf <- cutheadtail(df, 2, 1)
shannnonplot(trdf, "CAZYMESI.pdf")
