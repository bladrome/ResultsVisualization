library(ggplot2)
library(reshape2)
library(stringr)
library(RColorBrewer)

Rumentaxfile <- "./Rumen/taxa.percents.table.xls"
Cecumtaxfile <- "./Cecum/taxa.percents.table.xls"
Colontaxfile <- "./Colon/taxa.percents.table.xls"

taxplot <- function(taxfile) {
    
    taxfile <- Rumentaxfile
    taxdatadf <- read.table(taxfile, header = TRUE, sep = "\t", stringsAsFactors = FALSE)
    taxdatadf$OTU.ID[length(taxdatadf$OTU.ID)] <- "g__others"
    
    taxdatadf$genusnames <- str_extract(taxdatadf$OTU.ID, regex("g__.*"))
    plotdata <- cbind(genusnames = taxdatadf$genusnames, taxdatadf[str_detect(colnames(taxdatadf), 
        regex(".*M.*"))])
    # meltplotdata <- melt(plotdata)
    
    cnames <- colnames(plotdata)
    rnames <- plotdata$genusnames
    plotdata <- t(plotdata[-1])
    colnames(plotdata) <- rnames
    
    pvlist <- c()
    for (i in 1:length(rnames)) {
        pv <- t.test(plotdata[, i][1:5], plotdata[, i][6:10])$p.value
        pvlist[i] <- pv
        # print(pv)
    }
    
    # plotdata <- rbind(plotdata, pvlist <= 0.05)
    group <- substr(cnames, 1, 3)[-1]
    sigplotdata <- data.frame(plotdata[, rnames[pvlist <= 0.01]])
    sigplotdata <- aggregate(sigplotdata, by = (list(group)), FUN = sum)
    sigplotdata <- melt(sigplotdata)
    
    ggplot(sigplotdata, aes(x = variable, y = value, fill = Group.1)) + coord_flip() + 
        geom_col(position = "dodge")
    # scale_x_discrete(labels = rev(sigplotdata$variable)) theme(axis.text.x =
    # element_text(angle = 40, vjust = 0.95, hjust = 0.95))
}

width <- 5
height <- 5

taxplot(Rumentaxfile)
ggsave(filename = "Rumentax.pdf", width = width, height = height)
taxplot(Cecumtaxfile)
ggsave(filename = "Cecumtax.pdf", width = width, height = height)
taxplot(Colontaxfile)
ggsave(filename = "Colontax.pdf", width = width, height = height)
