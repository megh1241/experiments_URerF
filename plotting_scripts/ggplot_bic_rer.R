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
precision_df44=data.frame(at_k, prec=iso_prec_list_twomeans_rer, SplitCrit = as.factor("TwoMeans"), Algorithm = as.factor("URerF"))
precision_df55=data.frame(at_k, prec=iso_prec_list_bicmclust_rer, SplitCrit = as.factor("BICMClust"), Algorithm = as.factor("URerF"))
precision_df66=data.frame(at_k, prec=iso_prec_list_bicfast_rer, SplitCrit = as.factor("BICFast"), Algorithm = as.factor("URerF"))
precision_df4=data.frame(at_k, prec=iso_prec_list_twomeans, SplitCrit = as.factor("TwoMeans"), Algorithm = as.factor("URF"))
precision_df5=data.frame(at_k, prec=iso_prec_list_bicmclust, SplitCrit = as.factor("BICMClust"), Algorithm = as.factor("URF"))
precision_df6=data.frame(at_k, prec=iso_prec_list_bicfast, SplitCrit = as.factor("BICFast"), Algorithm = as.factor("URF"))

precision_df_df=rbind(precision_df4, precision_df5, precision_df6, precision_df44, precision_df55, precision_df66 )
p2 <- ggplot(precision_df_df, aes(at_k, prec, colour = SplitCrit, linetype=Algorithm)) +  ggtitle("Linear") + geom_line(alpha=0.9, show.legend = TRUE) + geom_point(alpha=0.9, show.legend = TRUE, legend.position=c(0.4,0.9))+ylab("Geodesic Recall")  + xlab('K') + scale_color_brewer(palette="Dark2") + scale_y_continuous(breaks=seq(0, 1, 0.2), limits = c(0,1))#+ theme_grey(base_size = 17)

load("normalization_mog_bicvstmtrer.Rdata")
precision_df12=data.frame(at_k, prec=iso_prec_list_twomeans_rer, SplitCrit = as.factor("TwoMeans"), Algorithm = as.factor("URerF"))
precision_df13=data.frame(at_k, prec=iso_prec_list_bicmclust_rer, SplitCrit = as.factor("BICMClust"), Algorithm = as.factor("URerF"))
precision_df23=data.frame(at_k, prec=iso_prec_list_bicfast_rer, SplitCrit = as.factor("BICFast"), Algorithm = as.factor("URerF"))
precision_df11=data.frame(at_k, prec=iso_prec_list_twomeans, SplitCrit = as.factor("TwoMeans"), Algorithm = as.factor("URF"))
precision_df22=data.frame(at_k, prec=iso_prec_list_bicmclust, SplitCrit = as.factor("BICMClust"), Algorithm = as.factor("URF"))
precision_df33=data.frame(at_k, prec=iso_prec_list_bicfast, SplitCrit = as.factor("BICFast"), Algorithm = as.factor("URF"))
precision_df_11=rbind(precision_df11, precision_df22, precision_df33, precision_df12, precision_df13, precision_df23)
p1 <- ggplot(precision_df_11, aes(at_k, prec, colour = SplitCrit, linetype=Algorithm))  +
    ggtitle("Gaussian Mixture") +
    geom_line(alpha=0.9, show.legend = FALSE, legend.position = c(0.8, 0.2)) +
    geom_point(alpha=0.9, show.legend = FALSE, legend.position = c(0.8, 0.2))  + xlab('K') + ylab(NULL)+scale_y_continuous(breaks=seq(0, 1, 0.2), limits=c(0,1), labels=NULL) + scale_color_brewer(palette="Dark2") 



load("rotation_sphere_bicvstmtrer.Rdata")
#precision_df111=data.frame(at_k, prec=iso_prec_list_twomeans, SplitCrit= as.factor("TwoMeans"))
#precision_df222=data.frame(at_k, prec=iso_prec_list_bicmclust, SplitCrit = as.factor("BICMClust"))
#precision_df333=data.frame(at_k, prec=iso_prec_list_bicfast, SplitCrit = as.factor("BICFast"))
precision_df122=data.frame(at_k, prec=iso_prec_list_twomeans_rer, SplitCrit = as.factor("TwoMeans"), Algorithm = as.factor("URerF"))
precision_df133=data.frame(at_k, prec=iso_prec_list_bicmclust_rer, SplitCrit = as.factor("BICMClust"), Algorithm = as.factor("URerF"))
precision_df233=data.frame(at_k, prec=iso_prec_list_bicfast_rer, SplitCrit = as.factor("BICFast"), Algorithm = as.factor("URerF"))
precision_df111=data.frame(at_k, prec=iso_prec_list_twomeans, SplitCrit = as.factor("TwoMeans"), Algorithm = as.factor("URF"))
precision_df222=data.frame(at_k, prec=iso_prec_list_bicmclust, SplitCrit = as.factor("BICMClust"), Algorithm = as.factor("URF"))
precision_df333=data.frame(at_k, prec=iso_prec_list_bicfast, SplitCrit = as.factor("BICFast"), Algorithm = as.factor("URF"))
precision_df_111=rbind(precision_df111, precision_df222, precision_df333, precision_df122, precision_df133, precision_df233)
p4 <- ggplot(precision_df_111, aes(at_k, prec, colour = SplitCrit, linetype=Algorithm))  +
    ggtitle("Sphere") +
    geom_line(alpha=0.9, show.legend = FALSE, legend.position = c(0.8, 0.2)) +
    geom_point(alpha=0.9, show.legend = FALSE, legend.position = c(0.8, 0.2))  + xlab('K') + ylab(NULL) +  scale_y_continuous(breaks=seq(0, 1, 0.2), labels=NULL, limits = c(0, 1)) +  scale_color_brewer(palette="Dark2")#+ theme_grey(base_size =17)


load("normalization_helix_bicvstmtrer.Rdata")
#precision_df7=data.frame(at_k, prec=iso_prec_list_twomeans, SplitCrit = as.factor("TwoMeans"))
#precision_df8=data.frame(at_k, prec=iso_prec_list_bicmclust, SplitCrit = as.factor("BICMClust"))
#precision_df9=data.frame(at_k, prec=iso_prec_list_bicfast, SplitCrit = as.factor("BICFast"))
precision_df77=data.frame(at_k, prec=iso_prec_list_twomeans_rer, SplitCrit = as.factor("TwoMeans"), Algorithm = as.factor("URerF"))
precision_df88=data.frame(at_k, prec=iso_prec_list_bicmclust_rer, SplitCrit = as.factor("BICMClust"), Algorithm = as.factor("URerF"))
precision_df99=data.frame(at_k, prec=iso_prec_list_bicfast_rer, SplitCrit = as.factor("BICFast"), Algorithm = as.factor("URerF"))
precision_df7=data.frame(at_k, prec=iso_prec_list_twomeans, SplitCrit = as.factor("TwoMeans"), Algorithm = as.factor("URF"))
precision_df8=data.frame(at_k, prec=iso_prec_list_bicmclust, SplitCrit = as.factor("BICMClust"), Algorithm = as.factor("URF"))
precision_df9=data.frame(at_k, prec=iso_prec_list_bicfast, SplitCrit = as.factor("BICFast"), Algorithm = as.factor("URF"))

precision_df_df2=rbind(precision_df7, precision_df8, precision_df9, precision_df77, precision_df88, precision_df99)
p3 <- ggplot(precision_df_df2, aes(at_k, prec, colour = SplitCrit, linetype=Algorithm)) +  ggtitle("Helix") + geom_line(alpha=0.9, show.legend = TRUE) + geom_point(alpha=0.9, show.legend = TRUE, legend.position=c(0.4,0.9))+ xlab('K') + ylab(NULL) + scale_color_brewer(palette="Dark2")  +  scale_y_continuous(breaks=seq(0, 1, 0.2), labels=NULL, limits = c(0,1)) 

#+ theme_grey(base_size = 17)#grid.arrange(p1, p2, ncol=2, nrow=2)
ggarrange(p2, p3, p4, p1, ncol=4, nrow=3, common.legend = TRUE, legend="top")

ggsave("final_plots/varying_split_crit_rer.png")

#plot(p)
