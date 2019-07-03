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

num_of_points=500

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

    noise_1=generate_high_dim_uniform_noise(num_of_points, 1, const = 70)
    noise_2=generate_high_dim_gaussian_noise(num_of_points, 1, const = 70)

#    hw_noise_data=cbind(hw_data, noise_1)
#hw_noise_data = as.matrix(hw_data)
hw_noise_data = hw_data


g_noise=Urerf(hw_noise_data, trees = 300, Progress = TRUE, LinearCombo=FALSE, splitCrit = "bicfast", normalizeData = FALSE)
W_noise=g_noise$similarityMatrix
D_rf_noise=1-W_noise
D_iso_noise_p_r_list = p_r_list(D_rf_noise, D_truth, at_K, num_of_points)
iso_prec_list_bicfast = D_iso_noise_p_r_list$precisionList
    

g_noise=Urerf(hw_noise_data, trees = 300, Progress = TRUE, LinearCombo=FALSE, splitCrit = "bicmclust", normalizeData = FALSE)
W_noise=g_noise$similarityMatrix
D_rf_noise=1-W_noise
D_iso_noise_p_r_list = p_r_list(D_rf_noise, D_truth, at_K, num_of_points)
iso_prec_list_bicmclust = D_iso_noise_p_r_list$precisionList


g_noise=Urerf(hw_noise_data, trees = 300, Progress = TRUE, LinearCombo=FALSE, splitCrit = "twomeans", normalizeData = FALSE)
W_noise=g_noise$similarityMatrix
D_rf_noise=1-W_noise
D_iso_noise_p_r_list = p_r_list(D_rf_noise, D_truth, at_K, num_of_points)
iso_prec_list_twomeans = D_iso_noise_p_r_list$precisionList


g_noise=Urerf(hw_noise_data, trees = 300, Progress = TRUE, LinearCombo=TRUE, splitCrit = "bicfast", normalizeData = FALSE)
W_noise=g_noise$similarityMatrix
D_rf_noise=1-W_noise
D_iso_noise_p_r_list = p_r_list(D_rf_noise, D_truth, at_K, num_of_points)
iso_prec_list_bicfast_rer = D_iso_noise_p_r_list$precisionList
    

g_noise=Urerf(hw_noise_data, trees = 300, Progress = TRUE, LinearCombo=TRUE, splitCrit = "bicmclust", normalizeData = FALSE)
W_noise=g_noise$similarityMatrix
D_rf_noise=1-W_noise
D_iso_noise_p_r_list = p_r_list(D_rf_noise, D_truth, at_K, num_of_points)
iso_prec_list_bicmclust_rer = D_iso_noise_p_r_list$precisionList


g_noise=Urerf(hw_noise_data, trees = 300, Progress = TRUE, LinearCombo=TRUE, splitCrit = "twomeans", normalizeData = FALSE)
W_noise=g_noise$similarityMatrix
D_rf_noise=1-W_noise
D_iso_noise_p_r_list = p_r_list(D_rf_noise, D_truth, at_K, num_of_points)
iso_prec_list_twomeans_rer = D_iso_noise_p_r_list$precisionList

save(at_K, iso_prec_list_bicfast_rer, iso_prec_list_bicmclust_rer, iso_prec_list_twomeans_rer, iso_prec_list_bicfast, iso_prec_list_bicmclust, iso_prec_list_twomeans, file= paste(experiment, data, "bicvstmtrer.Rdata", sep="_"))

#save(at_K, iso_prec_list_bicfast, iso_prec_list_bicmclust, iso_prec_list_twomeans, file= paste(experiment, data, "bicvstmtrer.Rdata", sep="_"))

}

