#hw_sim_noise
source("hw_functions.R")
source("pr_utility_functions.R")
library(stats)
library(MASS)
library(Matrix)
library(scatterplot3d)
library(rerf)
library(vegan)
library(umap)
library(pracma)
library(randomForest)
library(mbstructure)
library(quanteda)
data(MBconnectome)


#at_K=seq(50, 200, 50)

#out <- generate.graph(newrdat, vdf.right)
#g <- out$g
#vdf <- out$vdf

#dmax <- 50
#Xhat <- doEmbed(g, dmax)

#hw_noise_data = Xhat 
#labels = vdf$type
#at_K = seq(1, 30, by=2)

at_K=seq(50, 200, 50)
filepath_X = "data/42/X/001.txt" 
filepath_y = "data/42/Y/001.txt" 

dir_X = 'data/42/X'
dir_y = 'data/42/y'

files_X = list.files(path=dir_X, pattern = '.txt', full.names = TRUE)
files_y = list.files(path=dir_y, pattern = '.txt', full.names = TRUE)
X = matrix(nrow=0 ,ncol=43)
for (file in files_X){
    allLines <- readLines(con = file, n = -1)
    tokenize <- strsplit(allLines, split = ' ')
    temp_X <- sapply(tokenize, FUN = function(x) {as.double(unlist(x))} )
    X = rbind(X, temp_X)
}
X[is.nan(X)] =  -1 
X[!is.finite(X)] = -1
#print(X)
y = c()
for (file in files_y){
    ylines <- readLines(con = filepath_y, n = -1)
    tokenize <- strsplit(ylines, split = ' ')
    y_temp <- lapply(tokenize, FUN = function(x) {as.integer(x)} )
    y_temp <- lapply(y_temp, function(x) replace(x, !is.finite(x), 0))
    y  = c(y, y_temp)
}

y[is.nan(X)] =  0
y[!is.finite(X)] = 0

y=unlist(y)
#print(y)
hw_noise_data = X
print(hw_noise_data)
num_of_points = nrow(X)
labels = y
#labels = as.factor(labels)
g_noise=Urerf(X, trees = 200, mtry=6, Progress = TRUE, LinearCombo=TRUE, splitCrit = "bicfast", normalizeData = TRUE)

W_noise=g_noise$similarityMatrix
D_rf_noise=1-W_noise
D_iso_noise_p_r_list = p_r_list(D_rf_noise, labels, at_K, num_of_points)
urerf_prec_list_bicfast = D_iso_noise_p_r_list$precisionList
urerf_rec_list_bicfast = D_iso_noise_p_r_list$recallList
print("Urerf done")
# isomap
D_eucd_noise = as.matrix(dist(hw_noise_data))
iso_dist_noise = as.matrix(isomapdist(D_eucd_noise, k=9))
D_iso_noise_p_r_list = p_r_list(iso_dist_noise, labels, at_K, num_of_points)
iso_prec_list_bicfast = D_iso_noise_p_r_list$precisionList
iso_rec_list_bicfast = D_iso_noise_p_r_list$recallList

print("isomap done")

#UMAP
custom.settings = umap.defaults
custom.settings$n_neighbors=9
a_noise = umap(hw_noise_data, config = custom.settings)
umap_dist_noise=as.matrix(dist(a_noise$layout))
D_umap_noise_p_r_list = p_r_list(umap_dist_noise, labels, at_K, num_of_points)
umap_prec_list_bicfast = D_umap_noise_p_r_list$precisionList
umap_rec_list_bicfast = D_umap_noise_p_r_list$recallList

print("umap done")
#Adele RF
g_noise1=randomForest(hw_noise_data, ntree=300, keep.forest=FALSE, proximity=TRUE)
simMat=g_noise1$proximity
arf_dist_noise = 1-simMat
D_arf_noise_p_r_list = p_r_list(arf_dist_noise, labels, at_K, num_of_points)
arf_prec_list_bicfast = D_arf_noise_p_r_list$precisionList
arf_rec_list_bicfast = D_arf_noise_p_r_list$recallList

#EucDist
D_euc_noise_p_r_list = p_r_list(D_eucd_noise, labels, at_K, num_of_points)
euc_prec_list_bicfast = D_euc_noise_p_r_list$precisionList
euc_rec_list_bicfast = D_euc_noise_p_r_list$recallList

save(at_K, iso_prec_list_bicfast, iso_rec_list_bicfast, umap_prec_list_bicfast, umap_rec_list_bicfast, arf_prec_list_bicfast, arf_rec_list_bicfast,urerf_prec_list_bicfast, urerf_rec_list_bicfast, euc_prec_list_bicfast, euc_rec_list_bicfast, file="gene_exp_simulations.Rdata")
