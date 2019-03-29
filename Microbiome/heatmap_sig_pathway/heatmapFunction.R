library(openxlsx)
library(ggplot2)
library(openxlsx)
library(pheatmap)
# taxfile <- 'Sigheatmap.xlsx'
taxfile <- "data/Sigheatmap0104.xlsx"
datadf <- read.xlsx(taxfile, sheet = 2)

Function <- datadf[1]
datadf <- datadf[-1]
rownames(datadf) <- Function$X1


# pheatmap(as.matrix(log2(datadf + 0.000001)), pheatmap(as.matrix(log2(datadf +
# 0.000001)),
pheatmap(as.matrix(datadf), scale = "column", border_color = "white", show_rownames = T, 
    show_colnames = T, cluster_rows = F, cluster_cols = F, color = colorRampPalette(c("#AC2024", 
        "white", "#5ebeeb"))(100))
# color = colorRampPalette(c('#AC2024', 'white', '#89BC54'))(100)) color =
# colorRampPalette(c('#AC2024', '#ECCECF', '#FEFEFE', '#E8F1DD',
# '#8CBD58'))(100)) color = colorRampPalette(c('#ED2224', '#3B54A5', '#808080',
# '#85C882', '#B0D356', '#E7E51E', '#3D56A6', '#40C2CD'))(1000))

ggsave("Sigheatmap.pdf")
