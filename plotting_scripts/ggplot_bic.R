library(ggplot2)
library(gtable)
library(grid)
library(gridExtra)
#load("rotation_linear_varying_dims.Rdata")


#ggsave("linear_varying_dim.png", p) 
#library(ggplot2)
#library(gtable)
library(ggpubr)
#library(grid)
#library(gridExtra)

#pdf("final_plots/panel_figure_1.pdf")

#at_k=seq(0, 10, 2)

at_k=seq(50, 200, 50)
load("rotation_linear_bicvstmtrer.Rdata")
precision_df4=data.frame(at_k, prec=iso_prec_list_twomeans, SplitCrit = as.factor("TwoMeans"))
precision_df5=data.frame(at_k, prec=iso_prec_list_bicmclust, SplitCrit = as.factor("BICMClust"))
precision_df6=data.frame(at_k, prec=iso_prec_list_bicfast, SplitCrit = as.factor("BICFast"))

precision_df_df=rbind(precision_df4, precision_df5, precision_df6)
p2 <- ggplot(precision_df_df, aes(at_k, prec, colour = SplitCrit)) +  ggtitle("Linear") + geom_line(alpha=0.9, show.legend = TRUE) + geom_point(alpha=0.9, show.legend = TRUE, legend.position=c(0.4,0.9))+ylab("Geodesic Recall")  + xlab('K') + scale_color_brewer(palette="Dark2") + scale_y_continuous(breaks=seq(0, 1, 0.2), limits = c(0,1))#+ theme_grey(base_size = 17)

load("normalization_mog_bicvstmtrer.Rdata")
precision_df11=data.frame(at_k, prec=iso_prec_list_twomeans, SplitCrit= as.factor("TwoMeans"))
precision_df22=data.frame(at_k, prec=iso_prec_list_bicmclust, SplitCrit = as.factor("BICMClust"))
precision_df33=data.frame(at_k, prec=iso_prec_list_bicfast, SplitCrit = as.factor("BICFast"))

precision_df_11=rbind(precision_df11, precision_df22, precision_df33)
p1 <- ggplot(precision_df_11, aes(at_k, prec, colour = SplitCrit))  +
    ggtitle("Gaussian Mixture") +
    geom_line(alpha=0.9, show.legend = FALSE, legend.position = c(0.8, 0.2)) +
    geom_point(alpha=0.9, show.legend = FALSE, legend.position = c(0.8, 0.2))  + xlab('K') + ylab(NULL)+scale_y_continuous(breaks=seq(0, 1, 0.2), limits=c(0,1), labels=NULL) + scale_color_brewer(palette="Dark2") 


load("rotation_sphere_bicvstmtrer.Rdata")
precision_df111=data.frame(at_k, prec=iso_prec_list_twomeans, SplitCrit= as.factor("TwoMeans"))
precision_df222=data.frame(at_k, prec=iso_prec_list_bicmclust, SplitCrit = as.factor("BICMClust"))
precision_df333=data.frame(at_k, prec=iso_prec_list_bicfast, SplitCrit = as.factor("BICFast"))
precision_df_111=rbind(precision_df111, precision_df222, precision_df333)
p4 <- ggplot(precision_df_111, aes(at_k, prec, colour = SplitCrit))  +
    ggtitle("Sphere") +
    geom_line(alpha=0.9, show.legend = FALSE, legend.position = c(0.8, 0.2)) +
    geom_point(alpha=0.9, show.legend = FALSE, legend.position = c(0.8, 0.2))  + xlab('K') + ylab(NULL) +  scale_y_continuous(breaks=seq(0, 1, 0.2), labels=NULL, limits = c(0, 1)) +  scale_color_brewer(palette="Dark2")#+ theme_grey(base_size =17)


load("normalization_helix_bicvstmtrer.Rdata")
precision_df7=data.frame(at_k, prec=iso_prec_list_twomeans, SplitCrit = as.factor("TwoMeans"))
precision_df8=data.frame(at_k, prec=iso_prec_list_bicmclust, SplitCrit = as.factor("BICMClust"))
precision_df9=data.frame(at_k, prec=iso_prec_list_bicfast, SplitCrit = as.factor("BICFast"))

precision_df_df2=rbind(precision_df7, precision_df8, precision_df9)
p3 <- ggplot(precision_df_df2, aes(at_k, prec, colour = SplitCrit)) +  ggtitle("Helix") + geom_line(alpha=0.9, show.legend = TRUE) + geom_point(alpha=0.9, show.legend = TRUE, legend.position=c(0.4,0.9))+ xlab('K') + ylab(NULL) + scale_color_brewer(palette="Dark2")  +  scale_y_continuous(breaks=seq(0, 1, 0.2), labels=NULL, limits = c(0,1)) 

#+ theme_grey(base_size = 17)#grid.arrange(p1, p2, ncol=2, nrow=2)
ggarrange(p2, p3, p4, p1, ncol=4, nrow=3, common.legend = TRUE, legend="top")

ggsave("final_plots/varying_split_crit.png")

#plot(p)
