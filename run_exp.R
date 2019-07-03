
#first we do non_normalized
#hw_sim_noise
#source("hw_functions.R")
#source("vary_min_parent2.R")
#source("vary_mtry.R")
#source("hw_sim_noise.R")
source("vary_bicvstmt.R")
#source("hw_sim_euc.R")

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
