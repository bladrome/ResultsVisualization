library(ggplot2)
library(pheatmap)
# taxfile <- 'Sigheatmap.xlsx'
taxfile <- "all_mirna.exp.txt.filter"
datadf <- read.csv(taxfile, sep="\t")


rownames <- datadf[1]
datadf <- datadf[-1]

datam = as.matrix(log2(datadf + 0.000001))
rownames(datam) = rownames$ID

px = pheatmap(datam,
## pheatmap(as.matrix(datadf),
         ## scale = "column",
         scale = "row",
         border_color = "white",
         ## color = colorRampPalette(c("#AC2024", "white", "#5ebeeb"))(600),
         color = colorRampPalette(c("#1E90FF", "white", "#FF0C00"))(600),
         show_rownames = T,
         show_colnames = T,
         cluster_rows = T,
         cluster_cols = T)

# color = colorRampPalette(c('#AC2024', 'white', '#89BC54'))(100)) color =
# colorRampPalette(c('#AC2024', '#ECCECF', '#FEFEFE', '#E8F1DD',
# '#8CBD58'))(100)) color = colorRampPalette(c('#ED2224', '#3B54A5', '#808080',
# '#85C882', '#B0D356', '#E7E51E', '#3D56A6', '#40C2CD'))(1000))

save_pheatmap_pdf <- function(x, filename, width=7, height=7) {
   stopifnot(!missing(x))
   stopifnot(!missing(filename))
   pdf(filename, width=width, height=height)
   grid::grid.newpage()
   grid::grid.draw(x$gtable)
   dev.off()
}

save_pheatmap_pdf(px, "heatmap.pdf", width=7, height=100)
