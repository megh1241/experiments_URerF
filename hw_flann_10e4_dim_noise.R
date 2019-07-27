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
library(rflann)

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
#noise_dims = seq(0, 10, 2) 
rf_prec_list = c()
iso_prec_list = c()
umap_prec_list = c()
arf_prec_list = c()
rf_norm_prec_list = c()
iso_norm_prec_list = c()
umap_norm_prec_list = c()
arf_norm_prec_list = c()

for (noise_dim in noise_dims) {
#generate noise

print("it: ")
print(noise_dim/2)

if(noise_dim > 0)
{
noise_1=generate_high_dim_uniform_noise(num_of_points, noise_dim, const = 70)
noise_2=generate_high_dim_gaussian_noise(num_of_points, noise_dim, const=70)

hw_noise_data=cbind(hw_data, noise_2)
}
else
    hw_noise_data = hw_data


#g_noise1=randomForest(hw_noise_data, ntree=300, keep.forest=FALSE, proximity=TRUE)
#simMat=g_noise1$proximity
#D_rf = 1-simMat

D_rf = Neighbour(hw_noise_data, hw_noise_data, 1000)$indices

#####################################################################
D_rf_p_r_list = p_r_list(D_rf, D_truth, at_K, num_of_points)
D_rf_precision_list= D_rf_p_r_list$precisionList
arf_prec_list = c(arf_prec_list, D_rf_precision_list[[1]])

}

flann_norm_prec_list = arf_prec_list
save(flann_norm_prec_list,
at_K, file= paste(experiment, data, "varying_dims_with_flann_normalized1000.Rdata", sep="_"))
}

