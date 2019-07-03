library(ggplot2)
library(gtable)
library(grid)
library(gridExtra)
load("hw_no_noise_precision_list.Rdata")


precision_df1=data.frame(at_K, prec=D_rf_precision_list, Algo = as.factor("URerF"), Norm =as.factor("no normalization"))
precision_df2=data.frame(at_K, prec=D_iso_precision_list, Algo = as.factor("Isomap"), Norm =as.factor("no normalization"))
precision_df3=data.frame(at_K, prec=D_umap_precision_list, Algo = as.factor("umap"), Norm =as.factor("no normalization"))
precision_df4=data.frame(at_K, prec=D_rf_norm_precision_list, Algo = as.factor("URerF"), Norm =as.factor("normalization"))
precision_df5=data.frame(at_K, prec=D_iso_norm_precision_list, Algo = as.factor("Isomap"), Norm =as.factor("normalization"))
precision_df6=data.frame(at_K, prec=D_umap_norm_precision_list, Algo = as.factor("umap"), Norm =as.factor("no normalization"))

precision_df=rbind(precision_df1, precision_df2, precision_df3, precision_df4, precision_df5, precision_df6)
p <- ggplot(precision_df, aes(at_K, prec, colour = Algo, shape = Norm)) + geom_line(alpha=0.9, show.legend = TRUE) + geom_point(alpha=0.9, show.legend = TRUE)  + xlab('@K') + ylab('Precision/Recall') + scale_color_brewer(palette="Dark2")
plot(p)                   

# recall_df1=data.frame(at_K, reca=D_rf_recall_list, Algo = as.factor("URerF"), DIM =as.factor("d'=0"))
# recall_df2=data.frame(at_K, reca=D_iso_recall_list, Algo = as.factor("Isomap"), DIM =as.factor("d'=0"))
# recall_df3=data.frame(at_K, reca=D_geo_recall_list, Algo = as.factor("Geo"), DIM =as.factor("d'=0"))
# recall_df4=data.frame(at_K, reca=D_rf_noise1_recall_list, Algo = as.factor("URerF"), DIM =as.factor("d'=6"))
# recall_df5=data.frame(at_K, reca=D_rf_noise2_recall_list, Algo = as.factor("URerF"), DIM =as.factor("d'=17"))
# recall_df6=data.frame(at_K, reca=D_iso_noise1_recall_list, Algo = as.factor("Isomap"), DIM =as.factor("d'=6"))
# recall_df7=data.frame(at_K, reca=D_iso_noise2_recall_list, Algo = as.factor("Isomap"), DIM =as.factor("d'=17"))
# recall_df8=data.frame(at_K, reca=D_umap_recall_list, Algo = as.factor("UMAP"), DIM =as.factor("d'=0"))
# recall_df9=data.frame(at_K, reca=D_umap_noise1_recall_list, Algo = as.factor("UMAP"), DIM =as.factor("d'=6"))
# recall_df10=data.frame(at_K, reca=D_umap_noise2_recall_list, Algo = as.factor("UMAP"), DIM =as.factor("d'=17"))
# recall_df=rbind(recall_df1, recall_df2, recall_df3, recall_df4, recall_df5, recall_df6, recall_df7, recall_df8, recall_df9, recall_df10)
# r <- ggplot(recall_df, aes(at_K, reca, colour = Algo, shape = DIM)) + geom_line(alpha=0.9, show.legend = FALSE) + geom_point(alpha=0.9, show.legend = FALSE) + xlab('@K') + ylab('Recall') + scale_color_brewer(palette="Dark2")
# #plot(r)
# 
# 
# #if need to change line thickness, then just set size=DIM but not geom_point, notice the difference with above
# pr_df1=data.frame(prec=D_rf_precision_list, reca=D_rf_recall_list, Algo = as.factor("URerF"), DIM =as.factor("d'=0"))
# pr_df2=data.frame(prec=D_iso_precision_list, reca=D_iso_recall_list, Algo = as.factor("Isomap"), DIM =as.factor("d'=0"))
# pr_df3=data.frame(prec=D_geo_precision_list, reca=D_geo_recall_list, Algo = as.factor("Geo"), DIM =as.factor("d'=0"))
# pr_df4=data.frame(prec=D_rf_noise1_precision_list, reca=D_rf_noise1_recall_list, Algo = as.factor("URerF"), DIM =as.factor("d'=6"))
# pr_df5=data.frame(prec=D_rf_noise2_precision_list, reca=D_rf_noise2_recall_list, Algo = as.factor("URerF"), DIM =as.factor("d'=17"))
# pr_df6=data.frame(prec=D_iso_noise1_precision_list, reca=D_iso_noise1_recall_list, Algo = as.factor("Isomap"), DIM =as.factor("d'=6"))
# pr_df7=data.frame(prec=D_iso_noise2_precision_list, reca=D_iso_noise2_recall_list, Algo = as.factor("Isomap"), DIM =as.factor("d'=17"))
# pr_df8=data.frame(prec=D_umap_precision_list, reca=D_umap_recall_list, Algo = as.factor("UMAP"), DIM =as.factor("d'=0"))
# pr_df9=data.frame(prec=D_umap_noise1_precision_list, reca=D_umap_noise1_recall_list, Algo = as.factor("UMAP"), DIM =as.factor("d'=6"))
# pr_df10=data.frame(prec=D_umap_noise2_precision_list, reca=D_umap_noise2_recall_list, Algo = as.factor("UMAP"), DIM =as.factor("d'=17"))
# pr_df=rbind(pr_df1, pr_df2, pr_df3, pr_df4, pr_df5, pr_df6, pr_df7, pr_df8, pr_df9, pr_df10)
# pr <- ggplot(pr_df, aes(reca, prec, colour = Algo, shape = DIM)) + geom_line(alpha=0.9) + geom_point(alpha=0.9) + xlab('Recall') + ylab('Precision') + scale_color_brewer(palette="Dark2")#+ theme(legend.text=element_text(size=1))
# 
# 
# grid.arrange(p, r, pr, layout_matrix=rbind(cbind(1,2),cbind(3,3)))
# 
