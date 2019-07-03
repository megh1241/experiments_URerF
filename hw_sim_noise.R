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

#noise_dims = c(0, 10, 50, 100, 1000, 5000, 10000)
noise_dims = seq(0, 10, 2) 
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


#hw_noise_data_normalized=normalizeData(hw_noise_data)
#rotation vs no rotation unnormaized (without noise)

##Uncomment####

#if (experiment == "normalize" || experiment == "normalization")
#    hw_noise_data=normalizeData(hw_noise_data)
#else{
#    n = ncol(hw_noise_data)
#    orthomatrix = randortho(n, type = "orthonormal")
#    hw_noise_data=hw_noise_data%*%orthomatrix 
#}

##Uncomment####

#URerF
g_noise=Urerf(hw_noise_data, trees = 300, Progress = TRUE, LinearCombo=FALSE, splitCrit = "bicfast", normalizeData = FALSE)
W_noise=g_noise$similarityMatrix
D_rf_noise=1-W_noise

hw_noise_data=normalizeData(hw_noise_data)
# isomap
D_eucd_noise = as.matrix(dist(hw_noise_data))
iso_dist_noise = as.matrix(isomapdist(D_eucd_noise, k=9))
#UMAP
custom.settings = umap.defaults
custom.settings$n_neighbors=9
a_noise = umap(hw_noise_data, config = custom.settings)
D_umap_noise=as.matrix(dist(a_noise$layout))

#Adele RF
g_noise1=randomForest(hw_noise_data, ntree=300, keep.forest=FALSE, proximity=TRUE)
simMat=g_noise1$proximity
D_rf = 1-simMat
############################################################################
#now we do normalized


#URerF norm
#g_noise_norm=Urerf(norm_hw_noise_data, trees = 300, Progress = TRUE, splitCrit = "bicfast", normalizeData = FALSE)
#W_noise_norm=g_noise_norm$similarityMatrix
#D_rf_noise_norm=1-W_noise_norm
# isomap norm
#D_eucd_noise_norm = as.matrix(dist(norm_hw_noise_data))
#iso_dist_noise_norm = as.matrix(isomapdist(D_eucd_noise_norm, k=9))
#UMAP norm
#a_noise_norm = umap(norm_hw_noise_data, config = custom.settings)
#D_umap_noise_norm=as.matrix(dist(a_noise_norm$layout))


#########################actually generate the p-r list#################################
D_rf_noise_p_r_list = p_r_list(D_rf_noise, D_truth, at_K, num_of_points)
D_rf_noise_precision_list= D_rf_noise_p_r_list$precisionList
rf_prec_list = c(rf_prec_list, D_rf_noise_precision_list[[1]])
#####################################################################################
D_iso_noise_p_r_list = p_r_list(iso_dist_noise, D_truth, at_K, num_of_points)
D_iso_noise_precision_list= D_iso_noise_p_r_list$precisionList
iso_prec_list = c(iso_prec_list, D_iso_noise_precision_list[[1]])

####################################################################
D_umap_noise_p_r_list = p_r_list(D_umap_noise, D_truth, at_K, num_of_points)
D_umap_noise_precision_list= D_umap_noise_p_r_list$precisionList
umap_prec_list = c(umap_prec_list, D_umap_noise_precision_list[[1]])

#####################################################################
D_rf_p_r_list = p_r_list(D_rf, D_truth, at_K, num_of_points)
D_rf_precision_list= D_rf_p_r_list$precisionList
arf_prec_list = c(arf_prec_list, D_rf_precision_list[[1]])


rf_norm_prec_list = rf_prec_list
iso_norm_prec_list = iso_prec_list
umap_norm_prec_list = umap_prec_list
arf_norm_prec_list = arf_prec_list


######normalized p################
#########################actually generate the p-r list#################################
#D_rf_noise_norm_p_r_list = p_r_list(D_rf_noise_norm, D_truth, at_K, num_of_points)
#D_rf_noise_norm_precision_list= D_rf_noise_norm_p_r_list$precisionList
#rf_norm_prec_list = c(rf_norm_prec_list, D_rf_noise_norm_precision_list[[1]])

#####################################################################################
#D_iso_noise_norm_p_r_list = p_r_list(iso_dist_noise_norm, D_truth, at_K, num_of_points)
#D_iso_noise_norm_precision_list= D_iso_noise_norm_p_r_list$precisionList
#iso_norm_prec_list = c(iso_norm_prec_list, D_iso_noise_norm_precision_list[[1]])

####################################################################
#D_umap_noise_norm_p_r_list = p_r_list(D_umap_noise_norm, D_truth, at_K, num_of_points)
#D_umap_noise_norm_precision_list= D_umap_noise_p_r_list$precisionList
#umap_norm_prec_list = c(umap_norm_prec_list, D_umap_noise_norm_precision_list[[1]])
}

save(rf_prec_list, iso_prec_list, umap_prec_list, rf_norm_prec_list, iso_norm_prec_list, umap_norm_prec_list, arf_norm_prec_list,
     at_K, file= paste(experiment, data, "varying_dims_with_adelerf_normalized.Rdata", sep="_"))
}

