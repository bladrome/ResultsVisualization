######################
#data format
#group labels size(fold_change) value
#
#####################
  data<-read.table("clipboard",header = T);head(data);dim(data)
  ggplot(data, aes(group,labels,size=size))+
    geom_point(aes(color = -log10(value)))+
    scale_colour_gradientn(colours = colorRampPalette(c("grey","orange","red"))(50))+
    theme_bw()+
    theme(text = element_text(size=8), axis.text.x = element_text(angle=30, hjust=1))+
    theme(text = element_text(size=8), axis.text.y = element_text(angle=0, hjust=1))+
    theme(panel.grid.minor=element_blank())+
    theme(panel.grid.major=element_blank())