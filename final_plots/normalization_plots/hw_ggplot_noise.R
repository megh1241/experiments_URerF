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

noise_dims=seq(0, 10, 2)


load("linear_vd_final.Rdata")
precision_df4_new = data.frame(noise_dims, prec=rf_prec_list, Algorithm = as.factor("URerF"))

load("mog_vd_final.Rdata")
precision_df11_new = data.frame(noise_dims, prec=rf_prec_list, Algorithm= as.factor("URerF"))

load("sphere_vd_final.Rdata")
precision_df111_new = data.frame(noise_dims, prec=rf_prec_list, Algorithm= as.factor("URerF"))

load("helix_vd_final.Rdata")
precision_df7_new = data.frame(noise_dims, prec=rf_prec_list, Algorithm = as.factor("URerF"))



load("normalization_linear_varying_dims_final_9.Rdata")
precision_df4=data.frame(noise_dims, prec=rf_prec_list, Algorithm = as.factor("URerF"))
precision_df5=data.frame(noise_dims, prec=iso_prec_list, Algorithm = as.factor("Isomap"))
precision_df6=data.frame(noise_dims, prec=umap_prec_list, Algorithm = as.factor("Umap"))

precision_df_df=rbind(precision_df4_new, precision_df5, precision_df6)
p2 <- ggplot(precision_df_df, aes(noise_dims, prec, colour = Algorithm)) +  ggtitle("Linear") + geom_line(alpha=0.9, show.legend = TRUE) + geom_point(alpha=0.9, show.legend = TRUE, legend.position=c(0.4,0.9))+ylab("Geodesic Recall")  + xlab('Dimensions') + scale_color_brewer(palette="Dark2") + scale_x_continuous(breaks=seq(0, 10, 2)) + scale_y_continuous(breaks=seq(0, 1, 0.2))#+ theme_grey(base_size = 17)

load("normalization_mog_varying_dims_final_10.Rdata")
precision_df11=data.frame(noise_dims, prec=rf_prec_list, Algorithm= as.factor("URerF"))
precision_df22=data.frame(noise_dims, prec=iso_prec_list, Algorithm = as.factor("Isomap"))
precision_df33=data.frame(noise_dims, prec=umap_prec_list, Algorithm = as.factor("Umap"))

precision_df_11=rbind(precision_df11_new, precision_df22, precision_df33)
p1 <- ggplot(precision_df_11, aes(noise_dims, prec, colour = Algorithm))  +
    ggtitle("Gaussian Mixture") +
    geom_line(alpha=0.9, show.legend = FALSE, legend.position = c(0.8, 0.2)) +
    geom_point(alpha=0.9, show.legend = FALSE, legend.position = c(0.8, 0.2))  + xlab('Dimensions') + scale_y_continuous(breaks=seq(0, 1, 0.2), labels=NULL) + scale_color_brewer(palette="Dark2") + scale_x_continuous(breaks=seq(0, 10, 2))


load("normalization_sphere_varying_dims_final_10.Rdata")
precision_df111=data.frame(noise_dims, prec=rf_prec_list, Algorithm= as.factor("URerF"))
precision_df222=data.frame(noise_dims, prec=iso_prec_list, Algorithm = as.factor("Isomap"))
precision_df333=data.frame(noise_dims, prec=umap_prec_list, Algorithm = as.factor("Umap"))

precision_df_111=rbind(precision_df111_new, precision_df222, precision_df333)
p4 <- ggplot(precision_df_111, aes(noise_dims, prec, colour = Algorithm))  +
    ggtitle("Sphere") +
    geom_line(alpha=0.9, show.legend = FALSE, legend.position = c(0.8, 0.2)) +
    geom_point(alpha=0.9, show.legend = FALSE, legend.position = c(0.8, 0.2))  + xlab('Dimensions') + ylab(NULL) +  scale_y_continuous(breaks=seq(0, 1, 0.2), labels=NULL) +  scale_color_brewer(palette="Dark2") + scale_x_continuous(breaks=seq(0, 10, 2))#+ theme_grey(base_size =17)


load("normalization_helix_varying_dims_final_9.Rdata")
precision_df7=data.frame(noise_dims, prec=rf_prec_list, Algorithm = as.factor("URerF"))
precision_df8=data.frame(noise_dims, prec=iso_prec_list, Algorithm = as.factor("Isomap"))
precision_df9=data.frame(noise_dims, prec=umap_prec_list, Algorithm = as.factor("Umap"))

precision_df_df2=rbind(precision_df7_new, precision_df8, precision_df9)
p3 <- ggplot(precision_df_df2, aes(noise_dims, prec, colour = Algorithm)) +  ggtitle("Helix") + geom_line(alpha=0.9, show.legend = TRUE) + geom_point(alpha=0.9, show.legend = TRUE, legend.position=c(0.4,0.9))+ xlab('Dimensions') + ylab(NULL) + scale_color_brewer(palette="Dark2") + scale_x_continuous(breaks=seq(0, 10, 2)) +  scale_y_continuous(breaks=seq(0, 1, 0.2), labels=NULL) 

#+ theme_grey(base_size = 17)#grid.arrange(p1, p2, ncol=2, nrow=2)
ggarrange(p2, p3, p4, p1, ncol=4, nrow=3, common.legend = TRUE, legend="top")

ggsave("final_plots/varying_dim_normalization.png")

#plot(p)
