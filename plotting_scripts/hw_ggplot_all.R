#ggplot all
library(ggplot2)
library(gtable)
library(grid)
library(gridExtra)
load("hw_noise_precision_list.Rdata")
load("hw_no_noise_precision_list.Rdata")


precision_df1=data.frame(at_K, prec=D_rf_precision_list, Algo = as.factor("URerF"), Norm =as.factor("no rotation"), Noise=as.factor("No Noise"))
precision_df2=data.frame(at_K, prec=D_iso_precision_list, Algo = as.factor("Isomap"), Norm =as.factor("no rotation"), Noise=as.factor("No Noise"))
precision_df3=data.frame(at_K, prec=D_umap_precision_list, Algo = as.factor("umap"), Norm =as.factor("no rotation"), Noise=as.factor("No Noise"))
precision_df4=data.frame(at_K, prec=D_rf_norm_precision_list, Algo = as.factor("URerF"), Norm =as.factor("rotation"), Noise=as.factor("No Noise"))
precision_df5=data.frame(at_K, prec=D_iso_norm_precision_list, Algo = as.factor("Isomap"), Norm =as.factor("rotation"), Noise=as.factor("No Noise"))
precision_df6=data.frame(at_K, prec=D_umap_norm_precision_list, Algo = as.factor("umap"), Norm =as.factor("no rotation"), Noise=as.factor("No Noise"))

precision_df11=data.frame(at_K, prec=D_rf_noise_precision_list, Algo = as.factor("URerF"), Norm =as.factor("no rotation"), Noise=as.factor("Noise"))
precision_df12=data.frame(at_K, prec=D_iso_noise_precision_list, Algo = as.factor("Isomap"), Norm =as.factor("no rotation"), Noise=as.factor("Noise"))
precision_df13=data.frame(at_K, prec=D_umap_noise_precision_list, Algo = as.factor("umap"), Norm =as.factor("no rotation"), Noise=as.factor("Noise"))
precision_df14=data.frame(at_K, prec=D_rf_noise_norm_precision_list, Algo = as.factor("URerF"), Norm =as.factor("rotation"), Noise=as.factor("Noise"))
precision_df15=data.frame(at_K, prec=D_iso_noise_norm_precision_list, Algo = as.factor("Isomap"), Norm =as.factor("rotation"), Noise=as.factor("Noise"))
precision_df16=data.frame(at_K, prec=D_umap_noise_norm_precision_list, Algo = as.factor("umap"), Norm =as.factor("no rotation"), Noise=as.factor("Noise"))

precision_df=rbind(precision_df1, precision_df2, precision_df3, precision_df4, precision_df5, precision_df6,
                   precision_df11, precision_df12, precision_df13, precision_df14, precision_df15, precision_df16)

p <- ggplot(precision_df, aes(at_K, prec, colour = Algo, shape = Norm, linetype=Noise)) + geom_line(alpha=0.8, show.legend = TRUE) + geom_point(alpha=0.8, show.legend = TRUE)  + xlab('@K') + ylab('Precision/Recall') + scale_color_brewer(palette="Dark2")
plot(p)
