library(ggplot2)
library(pheatmap)
library(RColorBrewer)
library(stringr)
library(gplots)

red = "#E46161"
blue = "#3377AE"

c("#ED2224", "#3B54A5", "#808080", "#85C882", "#B0D356", "#E7E51E", "#3D56A6", "#40C2CD")


heatmapplot <- function(taxfile, colorder, savefilename) {
    print(savefilename)
    # taxfile <- Rumentaxfile
    
    datadf <- read.table(taxfile, header = TRUE, sep = "\t")
    dim(datadf)
    
    # datadf = t(datadf)
    rnames <- datadf$OTU.ID
    datadf <- datadf[-1]
    rownames(datadf) <- rnames
    
    # pheatmap(as.matrix(log2(datadf + 0.000001)),
    pheatmap(as.matrix(log10(datadf + 1e-05)), filename = savefilename, scale = "column", 
        border_color = "white", show_rownames = T, show_colnames = T, cluster_rows = T, 
        cluster_cols = F, color = colorRampPalette(c("#AC2024", "white", "#89BC54"))(500))
    # color = colorRampPalette(c('#AC2024', '#ECCECF', '#FEFEFE', '#E8F1DD',
    # '#8CBD58'))(500))
    
    
}


Rumentaxfile = "Rumen/heatmap.taxa.table.xls"
Cecumtaxfile = "Cerum/heatmap.taxa.table.xls"
Colontaxfile = "Colon/heatmap.taxa.table.xls"

Rucolorder = c("G_Ru1", "G_Ru2", "G_Ru3", "G_Ru4", "G_Ru5", "S_Ru1", "S_Ru2", "S_Ru3", 
    "S_Ru4", "S_Ru5", "G_M_Ru1", "G_M_Ru2", "G_M_Ru3", "G_M_Ru4", "G_M_Ru5", "S_M_Ru1", 
    "S_M_Ru2", "S_M_Ru3", "S_M_Ru4", "S_M_Ru5")
Cecolorder = c("G_Ce1", "G_Ce2", "G_Ce3", "G_Ce4", "G_Ce5", "S_Ce1", "S_Ce2", "S_Ce3", 
    "S_Ce4", "S_Ce5", "G_M_Ce1", "G_M_Ce2", "G_M_Ce3", "G_M_Ce4", "G_M_Ce5", "S_M_Ce1", 
    "S_M_Ce2", "S_M_Ce3", "S_M_Ce4", "S_M_Ce5")
Cocolorder = c("G_Co1", "G_Co2", "G_Co3", "G_Co4", "G_Co5", "S_Co1", "S_Co2", "S_Co3", 
    "S_Co4", "S_Co5", "G_M_Co1", "G_M_Co2", "G_M_Co3", "G_M_Co4", "G_M_Co5", "S_M_Co1", 
    "S_M_Co2", "S_M_Co3", "S_M_Co4", "S_M_Co5")

width = 4
height = 3
p <- heatmapplot(Rumentaxfile, Rucolorder, "Rumenheatmap.pdf")
p
Rup <- p
p <- heatmapplot(Cecumtaxfile, Cecolorder, "Cecumheatmap.pdf")
p
Cep <- p
p <- heatmapplot(Colontaxfile, Cocolorder, "Colonheatmap.pdf")
p
Cop <- p
