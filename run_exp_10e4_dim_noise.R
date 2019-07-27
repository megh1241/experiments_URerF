#uncomment for euclidean prec recall
#source("hw_sim_euc_10e4noise.R")

#uncomment for flann prec recall
#source("hw_flann_10e4_dim_noise.R")

#uncomment for other prec recall
#source("hw_sim_10e4_dim_noise.R")


source("precision_recall.R")
library(stats)
library(MASS)
library(Matrix)
library(scatterplot3d)
library(rerf)
library(vegan)
library(umap)
library(pracma)


noise_experiments_varying_dim(experiment = "rotation", data="linear")
noise_experiments_varying_dim(experiment = "normalization", data="helix")
noise_experiments_varying_dim(experiment = "normalization", data="mog")
noise_experiments_varying_dim(experiment = "rotation", data="sphere")
