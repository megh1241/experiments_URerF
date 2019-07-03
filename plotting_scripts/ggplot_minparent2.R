library(ggplot2)
library(gtable)
library(grid)
library(gridExtra)
#load("rotation_linear_varying_dims.Rdata")
library(dplyr)
library(ggpubr)

pdf("final_plots/panel_figure_1.pdf")

noise_dims=seq(0, 10, 2)

a_arr = c(10, 10, 10, 10, 10, 10, 15, 15, 15, 15, 15, 15, 20, 20, 20, 20, 20, 20, 25, 25, 25, 25, 25, 25, 30, 30, 30, 30, 30, 30)
load("rotation_linear_varying_dims_vary_minparent.Rdata")
precision_df_list = list()
a = 1
cnt = 1
print (main_list)
for (minp in minparents){
    precision_df=data.frame(noise_dimensions=noise_dims, precision=main_list[a : a+5], MinParent = as.factor(minp))
    precision_df_list[[cnt]] = precision_df
    a = a + 6
    cnt = cnt + 1
}

comb_df = bind_rows(precision_df_list, .id="id")

comb_df["Minparent"] = as.factor(a_arr)

p2 <- ggplot(comb_df, aes(x=noise_dimensions, y=precision, color=Minparent)) +
ggtitle("Linear") + geom_line(alpha=0.9, show.legend = TRUE) + geom_point(alpha=0.9, show.legend = TRUE, legend.position=c(0.4,0.9))+ylab("Geodesic Recall @ k=50")  + xlab('Dimensions') + scale_color_brewer(palette="Dark2") + scale_x_continuous(breaks=seq(0, 10, 2)) + scale_y_continuous(breaks=seq(0, 1, 0.2))#+ theme_grey(base_size = 17)

load("normalization_mog_varying_dims_vary_minparent.Rdata")

precision_df_list = list()
a = 1
cnt = 1
print (main_list)
for (minp in minparents){
    precision_df=data.frame(noise_dimensions=noise_dims, precision=main_list[a : a+5], MinParent = as.factor(minp))
    precision_df_list[[cnt]] = precision_df
    a = a + 6
    cnt = cnt + 1
}
comb_df = bind_rows(precision_df_list, .id="id")
comb_df["Minparent"] = as.factor(a_arr)
p1 <- ggplot(comb_df, aes(x=noise_dimensions, y=precision, color=Minparent)) +
ggtitle("Gaussian Mixture") + geom_line(alpha=0.9, show.legend = TRUE) + geom_point(alpha=0.9, show.legend = TRUE, legend.position=c(0.4,0.9)) + xlab('Dimensions') + scale_color_brewer(palette="Dark2") + scale_x_continuous(breaks=seq(0, 10, 2)) + scale_y_continuous(breaks=seq(0, 1, 0.2))#+ theme_grey(base_size = 17)

load("rotation_sphere_varying_dims_vary_minparent.Rdata")
precision_df_list = list()
a = 1
cnt = 1
print (main_list)
for (minp in minparents){
    precision_df=data.frame(noise_dimensions=noise_dims, precision=main_list[a : a+5], MinParent = as.factor(minp))
    precision_df_list[[cnt]] = precision_df
    a = a + 6
    cnt = cnt + 1
}

comb_df = bind_rows(precision_df_list, .id="id")
comb_df["Minparent"] = as.factor(a_arr)
p4 <- ggplot(comb_df, aes(x=noise_dimensions, y=precision, color=Minparent)) +
ggtitle("Sphere") + geom_line(alpha=0.9, show.legend = TRUE) + geom_point(alpha=0.9, show.legend = TRUE, legend.position=c(0.4,0.9))  + xlab('Dimensions') + scale_color_brewer(palette="Dark2") + scale_x_continuous(breaks=seq(0, 10, 2)) + scale_y_continuous(breaks=seq(0, 1, 0.2))#+ theme_grey(base_size = 17)

load("normalization_helix_varying_dims_vary_minparent.Rdata")
precision_df_list = list()
a = 1
cnt = 1
print (main_list)
for (minp in minparents){
    precision_df=data.frame(noise_dimensions=noise_dims, precision=main_list[a : a+5], MinParent = as.factor(minp))
    precision_df_list[[cnt]] = precision_df
    a = a + 6
    cnt = cnt + 1
}

comb_df = bind_rows(precision_df_list, .id="id")
comb_df["Minparent"] = as.factor(a_arr)
p3 <- ggplot(comb_df, aes(x=noise_dimensions, y=precision, color=Minparent)) +
ggtitle("Helix") + geom_line(alpha=0.9, show.legend = TRUE) + geom_point(alpha=0.9, show.legend = TRUE, legend.position=c(0.4,0.9))+ylab("Geodesic Recall @ k=50")  + xlab('Dimensions') + scale_color_brewer(palette="Dark2") + scale_x_continuous(breaks=seq(0, 10, 2)) + scale_y_continuous(breaks=seq(0, 1, 0.2))#+ theme_grey(base_size = 17)

ggarrange(p2, p3, p4, p1, ncol=4, nrow=3, common.legend = TRUE, legend="top")

