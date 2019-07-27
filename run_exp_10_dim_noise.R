#uncomment for euclidean distancec Prec and Rec
#source("hw_sim_euc_10_dim_noise.R")

#uncomment for flann Prec and Rec
#source("hw_flann_10_dim_noise.R")


#uncomment for URerF, RF, UMAP ISOMAP Prec and Rec
#source("hw_sim_noise.R")



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
