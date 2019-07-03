library(ggplot2)
library(gtable)
library(grid)
library(gridExtra)
load("drosophila_simulations.Rdata")


library(ggpubr)

pdf("final_plots/dros_fig.pdf")

precision_df4=data.frame(prec=urerf_prec_list_bicfast, rec=urerf_rec_list_bicfast ,Algorithm = as.factor("URerF"))
precision_df5=data.frame(prec=iso_prec_list_bicfast, rec= iso_rec_list_bicfast,Algorithm = as.factor("Isomap"))
precision_df6=data.frame(prec=umap_prec_list_bicfast, rec=umap_rec_list_bicfast , Algorithm = as.factor("Umap"))
precision_df7=data.frame(prec=arf_prec_list_bicfast, rec= arf_rec_list_bicfast,Algorithm = as.factor("Random Forest"))
precision_df8 = data.frame(prec=euc_prec_list_bicfast ,rec=euc_rec_list_bicfast ,Algorithm=as.factor("Euclidean"))
precision_df_df=rbind(precision_df4, precision_df5, precision_df6, precision_df7, precision_df8)
p2 <- ggplot(precision_df_df, aes(prec, rec, colour = Algorithm)) +  geom_line(alpha=0.9, size=1.3, show.legend = TRUE) + geom_point(size=3, alpha=0.9, show.legend = TRUE, legend.position=c(0.4,0.9))+ylab("Geodesic Recall")  + xlab('Geodesic Precision') + scale_color_brewer(palette="Dark2")+ theme_grey(base_size = 15)

ggarrange(p2,  ncol=1, nrow=1, common.legend = TRUE, legend="top")

ggsave("final_plots/dros_fig.png")

#plot(p2)
