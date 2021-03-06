#hw_sim_noise
source("hw_functions.R")
source("precision_recall.R")
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
    D_truth=mog_geodesic(as.matrix(hw_data))
else
    D_truth=sphere_geodesic(hw_data, 9, num_of_points)


at_K=seq(50, 200, 50)


noise_dims = seq(0, 10, 2)
minparents = seq(2, 500, 50)
#noise_dims = seq(0, 2, 2)
#minparents = seq(10, 15, 5)
mtries = seq(1, 10, 2)
main_list = c()
noise_dim = 10

for (mtr in mtries) {
    noise_1=generate_high_dim_uniform_noise(num_of_points, noise_dim, const = 70)
    noise_2=generate_high_dim_gaussian_noise(num_of_points, noise_dim, const = 70)

    hw_noise_data=cbind(hw_data, noise_2)

    g_noise=Urerf(hw_noise_data, trees = 300, mtry = mtr,Progress = TRUE, LinearCombo=FALSE, splitCrit = "bicfast", normalizeData = FALSE)
    W_noise=g_noise$similarityMatrix
    D_rf_noise=1-W_noise


    D_iso_noise_p_r_list = p_r_list(D_rf_noise, D_truth, at_K, num_of_points)
    iso_prec_list = D_iso_noise_p_r_list$precisionList
    
    main_list = c(main_list, iso_prec_list)
}

save(main_list, minparents, at_K, file= paste(experiment, data, "version4_varying_dims_vary_mtry.Rdata", sep="_"))

}

