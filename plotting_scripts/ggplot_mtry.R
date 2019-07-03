library(ggplot2)
library(gtable)
library(grid)
library(gridExtra)
#load("rotation_linear_varying_dims.Rdata")
library(dplyr)
library(ggpubr)
library(RColorBrewer)
# Define the number of colors you want
#pdf("final_plots/panel_figure_1.pdf")

noise_dims=seq(0, 10, 2)
#minparents = seq(2, 500, 50)
mtries = seq(1, 10, 2)
at_K=seq(50, 200, 50)
a_arr = c(2, 2, 2, 2, 52, 52, 52, 52, 102, 102, 102, 102, 152, 152, 152, 152, 202, 202, 202, 202, 252, 252, 252, 252, 302, 302, 302, 302, 352, 352, 352, 352, 402, 402, 402, 402, 452, 452, 452, 452)


load("rotation_linear_version4_varying_dims_vary_mtry.Rdata")
#print(main_list[1:4])
counter=1
cnt = 1
precision_df_list = list()

nb.cols <- 10
mycolors <- colorRampPalette(brewer.pal(15, "Set2"))(nb.cols)
for (minp in mtries){
    a = counter
    b = counter+3
    precision_df=data.frame(at_k=at_K, precision=main_list[a:b], Mtry = as.factor(minp))
    print(precision_df)
    precision_df_list[[cnt]] = precision_df
    cnt = cnt + 1
    #list.append(precision_df_list, precision_df)
    counter = counter + 4
}

comb_df = bind_rows(precision_df_list, .id="id")
p2 <- ggplot(comb_df, aes(x=at_k, y=precision, color=Mtry)) +
    #scale_color_brewer(palette="Dark2") +  
    scale_fill_manual(values = mycolors) +  
   scale_color_discrete(breaks=mtries)+
    ggtitle("Linear") + geom_line(alpha=0.9, show.legend = TRUE) + geom_point(alpha=0.9, show.legend = TRUE) + ylab("Precision")  + xlab('K') +  scale_x_continuous(breaks=at_K) +  scale_y_continuous(breaks=c(0, 0.2, 0.4, 0.6, 0.8, 1.0), limits = c(0, 1))




load("normalization_mog_version4_varying_dims_vary_mtry.Rdata")
counter=1
cnt=1
precision_df_list = list()
for (minp in mtries){
    a = counter
    b = counter + 3
    precision_df=data.frame(at_k=at_K, precision=main_list[a:b], Mtry = as.factor(minp))
    precision_df_list[[cnt]] = precision_df
    counter = counter + 4
        cnt=cnt+1
}


comb_df = bind_rows(precision_df_list, .id="id")
p1 <- ggplot(comb_df, aes(x=at_k, y=precision, color=Mtry)) +
    scale_fill_manual(values = mycolors) +  
   scale_color_discrete(breaks=minparents)+
    ggtitle("Gaussian Mixture") + geom_line(alpha=0.9, show.legend = TRUE) + geom_point(alpha=0.9, show.legend = TRUE) + xlab('K') +  ylab(NULL) + scale_x_continuous(breaks=at_K) +  scale_y_continuous(breaks=c(0, 0.2, 0.4, 0.6, 0.8, 1.0), limits = c(0, 1), labels=NULL)



load("rotation_sphere_version4_varying_dims_vary_mtry.Rdata")
counter=1
cnt=1
precision_df_list = list()
for (minp in mtries){
    a = counter
    b = counter + 3
    precision_df=data.frame(at_k=at_K, precision=main_list[a:b], Mtry = as.factor(minp))
    precision_df_list[[cnt]] = precision_df
    counter = counter + 4
        cnt=cnt+1
}


comb_df = bind_rows(precision_df_list, .id="id")
p3 <- ggplot(comb_df, aes(x=at_k, y=precision, color=Mtry)) +scale_fill_manual(values = mycolors) +  scale_color_discrete(breaks=minparents)+ggtitle("Sphere") + geom_line(alpha=0.9, show.legend = TRUE) + geom_point(alpha=0.9, show.legend = TRUE) + xlab("K") + ylab(NULL) +  scale_x_continuous(breaks=at_K) +  scale_y_continuous(breaks=c(0, 0.2, 0.4, 0.6, 0.8, 1.0), limits = c(0, 1), labels=NULL)




load("normalization_helix_version4_varying_dims_vary_mtry.Rdata")
counter=1
cnt=1
precision_df_list = list()
for (minp in mtries){
    a = counter
    b = counter + 3
        
    precision_df=data.frame(at_k=at_K, precision=main_list[a:b], Mtry = as.factor(minp))
    print(precision_df)
    precision_df_list[[cnt]] = precision_df
    counter = counter + 4
    cnt=cnt+1
}


comb_df = bind_rows(precision_df_list, .id="id")
p4 <- ggplot(comb_df, aes(x=at_k, y=precision, color=Mtry)) + scale_fill_manual(values = mycolors) +  scale_color_discrete(breaks=minparents) + ggtitle("Helix") + geom_line(alpha=0.9, show.legend = TRUE) + geom_point(alpha=0.9, show.legend = TRUE) + xlab('K') + ylab(NULL) +  scale_x_continuous(breaks=at_K) + scale_y_continuous(breaks=c(0, 0.2, 0.4, 0.6, 0.8, 1.0), limits = c(0, 1), labels=NULL)

ggarrange(p2, p4, p3, p1, ncol=4, nrow=3, common.legend = TRUE, legend="top")

ggsave("final_plots/mtry_modified.png")

#plot(p2)
