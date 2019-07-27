library(ggplot2)
library(gtable)
library(grid)
library(gridExtra)
#load("rotation_linear_varying_dims.Rdata")



#precision_df1=data.frame(noise_dims, prec=rf_prec_list, Algo = as.factor("URerF"))
#precision_df2=data.frame(noise_dims, prec=iso_prec_list, Algo = as.factor("Isomap"))
#precision_df3=data.frame(noise_dims, prec=umap_prec_list, Algo = as.factor("umap"))
#precision_df=rbind(precision_df1, precision_df2, precision_df3)
#p <- ggplot(precision_df, aes(noise_dims, prec, colour = Algo)) + geom_line(alpha=0.9, show.legend = TRUE) + geom_point(alpha=0.9, show.legend = TRUE)  + xlab('@K') + ylab('Precision/Recall') + scale_color_brewer(palette="Dark2")
#plot(p)   


#ggsave("linear_varying_dim.png", p) 
#library(ggplot2)
#library(gtable)
library(ggpubr)
#library(grid)
#library(gridExtra)

#pdf("final_plots/panel_figure_1.pdf")

#noise_dims=seq(0, 10, 2)
noise_dims=c(0, 5, 25, 50, 500, 2500, 5000)

load("rotation_linear_euc_dist_large.Rdata")

load("rotation_linear_varying_dims_with_adelerf_10k_d.Rdata")
load("rotation_linear_varying_dims_with_flann_normalized1000.Rdata")
precision_df4=data.frame(noise_dims, prec=rf_prec_list, Algorithm = as.factor("URerF"))
precision_df5=data.frame(noise_dims, prec=iso_prec_list, Algorithm = as.factor("Isomap"))
precision_df6=data.frame(noise_dims, prec=umap_prec_list, Algorithm = as.factor("Umap"))
precision_df7=data.frame(noise_dims, prec=arf_norm_prec_list, Algorithm = as.factor("Random Forest"))
precision_df8 = data.frame(noise_dims, prec=euc_dist_prec_list ,Algorithm=as.factor("Euclidean"))
precision_df88 = data.frame(noise_dims, prec=flann_norm_prec_list ,Algorithm=as.factor("Flann"))
precision_df_df=rbind(precision_df4, precision_df5, precision_df6, precision_df7, precision_df8, precision_df88)
p2 <- ggplot(precision_df_df, aes(noise_dims, prec, colour = Algorithm)) +  ggtitle("Linear") + geom_line(alpha=0.9, show.legend = TRUE) + geom_point(alpha=0.9, show.legend = TRUE, legend.position=c(0.4,0.9))+ylab("Geodesic Recall @ k=50")  + xlab('Noise Dimensions') + scale_color_brewer(palette="Dark2") + scale_x_continuous(trans="log2") + scale_y_continuous(breaks=seq(0, 1, 0.2))#+ theme_grey(base_size = 17)


load("normalization_mog_euc_dist_large.Rdata")
load("normalization_mog_varying_dims_with_adelerf_10k_d.Rdata")
load("normalization_mog_varying_dims_with_flann_normalized1000.Rdata")
precision_df11=data.frame(noise_dims, prec=rf_prec_list, Algorithm= as.factor("URerF"))
precision_df22=data.frame(noise_dims, prec=iso_prec_list, Algorithm = as.factor("Isomap"))
precision_df33=data.frame(noise_dims, prec=umap_prec_list, Algorithm = as.factor("Umap"))
precision_df44=data.frame(noise_dims, prec=arf_norm_prec_list, Algorithm = as.factor("Random Forest"))
precision_df55 = data.frame(noise_dims, prec=euc_dist_prec_list ,Algorithm=as.factor("Euclidean"))
precision_df66 = data.frame(noise_dims, prec=flann_norm_prec_list ,Algorithm=as.factor("Flann"))
precision_df_11=rbind(precision_df11, precision_df22, precision_df33, precision_df44, precision_df55, precision_df66)
p1 <- ggplot(precision_df_11, aes(noise_dims, prec, colour = Algorithm))  +
    ggtitle("Gaussian Mixture") +
    geom_line(alpha=0.9, show.legend = FALSE, legend.position = c(0.8, 0.2)) +
    geom_point(alpha=0.9, show.legend = FALSE, legend.position = c(0.8, 0.2))  + xlab('Noise Dimensions') + ylab(NULL)+scale_y_continuous(breaks=seq(0, 1, 0.2), labels=NULL) + scale_color_brewer(palette="Dark2") + scale_x_continuous(trans= "log2")


load("rotation_sphere_euc_dist_large.Rdata")
load("rotation_sphere_varying_dims_with_adelerf_10k_d.Rdata")
load("rotation_sphere_varying_dims_with_flann_normalized1000.Rdata")
precision_df111=data.frame(noise_dims, prec=rf_prec_list, Algorithm= as.factor("URerF"))
precision_df222=data.frame(noise_dims, prec=iso_prec_list, Algorithm = as.factor("Isomap"))
precision_df333=data.frame(noise_dims, prec=umap_prec_list, Algorithm = as.factor("Umap"))
precision_df444=data.frame(noise_dims, prec=arf_norm_prec_list, Algorithm = as.factor("Random Forest"))
precision_df555 = data.frame(noise_dims, prec=euc_dist_prec_list ,Algorithm=as.factor("Euclidean"))
precision_df666 = data.frame(noise_dims, prec=flann_norm_prec_list ,Algorithm=as.factor("Flann"))
precision_df_111=rbind(precision_df111, precision_df222, precision_df333, precision_df444, precision_df555, precision_df666)
p4 <- ggplot(precision_df_111, aes(noise_dims, prec, colour = Algorithm))  +
    ggtitle("Sphere") +
    geom_line(alpha=0.9, show.legend = FALSE, legend.position = c(0.8, 0.2)) +
    geom_point(alpha=0.9, show.legend = FALSE, legend.position = c(0.8, 0.2))  + xlab('Noise Dimensions') + ylab(NULL) +  scale_y_continuous(breaks=seq(0, 1, 0.2), labels=NULL) +  scale_color_brewer(palette="Dark2") + scale_x_continuous(trans="log2")#+ theme_grey(base_size =17)


load("normalization_helix_euc_dist_large.Rdata")
load("normalization_helix_varying_dims_with_adelerf_10k_d.Rdata")
load("normalization_helix_varying_dims_with_flann_normalized1000.Rdata")
precision_df7=data.frame(noise_dims, prec=rf_prec_list, Algorithm = as.factor("URerF"))
precision_df8=data.frame(noise_dims, prec=iso_prec_list, Algorithm = as.factor("Isomap"))
precision_df9=data.frame(noise_dims, prec=umap_prec_list, Algorithm = as.factor("Umap"))
precision_df10=data.frame(noise_dims, prec=arf_norm_prec_list, Algorithm = as.factor("Random Forest"))
precision_df11 = data.frame(noise_dims, prec=euc_dist_prec_list ,Algorithm=as.factor("Euclidean"))
precision_df12 = data.frame(noise_dims, prec=flann_norm_prec_list ,Algorithm=as.factor("Flann"))
precision_df_df2=rbind(precision_df7, precision_df8, precision_df9, precision_df10, precision_df11, precision_df12)
p3 <- ggplot(precision_df_df2, aes(noise_dims, prec, colour = Algorithm)) +  ggtitle("Helix") + geom_line(alpha=0.9, show.legend = TRUE) + geom_point(alpha=0.9, show.legend = TRUE, legend.position=c(0.4,0.9))+ xlab('Noise Dimensions') + ylab(NULL) + scale_color_brewer(palette="Dark2") + scale_x_continuous(trans="log2") +  scale_y_continuous(breaks=seq(0, 1, 0.2), labels=NULL) 

#+ theme_grey(base_size = 17)#grid.arrange(p1, p2, ncol=2, nrow=2)
ggarrange(p2, p3, p4, p1, ncol=4, nrow=3, common.legend = TRUE, legend="top")

ggsave("final_plots/varying_dim_adele_10k_d_with_euc_flann.png")

#plot(p)
