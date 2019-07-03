#hw_sim_noise
source("hw_functions.R")
source("precision_recall.R")
library(randomForest)
library(stats)
library(MASS)
library(Matrix)
library(scatterplot3d)
library(rerf)
library(vegan)
library(umap)
library(pracma)


noise_experiments_varying_dim<- function(experiment = "rotation", data = "sphere") {

num_of_points=1000

if (data == "hw")
    hw_data_object=hw_data(num_of_points)
else if(data == "helix")
    hw_data_object=helix_data(num_of_points)
else if(data == "linear")
    hw_data_object=linear_data(num_of_points)
else if(data == "mog")
    hw_data_object=mog_data(num_of_points)
else
    hw_data_object=upper_sphere(r=9, N=num_of_points)


hw_data=hw_data_object[[1]]
t=hw_data_object[[2]]

if (data == "hw")
    D_truth=hw_geodesic(t, num_of_points)
else if(data == "helix")
    D_truth=helix_geodesic(t, num_of_points)
else if(data=="linear")
    D_truth=linear_geodesic(t,  num_of_points)
else if(data == "mog")
    D_truth=mog_geodesic(hw_data)
else
    D_truth=sphere_geodesic(hw_data,9, num_of_points)


at_K=seq(50, 51, 1)

noise_dims = c(0, 10, 50, 100, 1000, 5000, 10000)
euc_dist_prec_list = c()

for (noise_dim in noise_dims) {
#generate noise


if(noise_dim > 0)
{
noise_1=generate_high_dim_uniform_noise(num_of_points, noise_dim, const = 70)
noise_2=generate_high_dim_gaussian_noise(num_of_points, noise_dim, const=70)

hw_noise_data=cbind(hw_data, noise_2)
}
else
    hw_noise_data = hw_data


D_rf = as.matrix(dist(hw_noise_data))

#########################actually generate the p-r list#################################
D_rf_noise_p_r_list = p_r_list(D_rf, D_truth, at_K, num_of_points)
D_rf_noise_precision_list= D_rf_noise_p_r_list$precisionList
euc_dist_prec_list = c(euc_dist_prec_list, D_rf_noise_precision_list[[1]])
}

save(euc_dist_prec_list,  file= paste(experiment, data, "euc_dist_small.Rdata", sep="_"))
}

