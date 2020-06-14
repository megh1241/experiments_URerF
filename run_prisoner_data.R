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

at_K=seq(1000, 7000, 500)

dir_X = 'data_p'
files_X = list.files(path=dir_X, pattern = '.csv', full.names = TRUE)
X = matrix()
iter = 0
for (file in files_X){
    allLines <- readLines(con = file, n = -1)
    tokenize <- strsplit(allLines, split = ',')
    temp_X <- sapply(tokenize, FUN = function(x) {as.double(unlist(x))} )
    if (iter == 0)
        X = temp_X
    else
        X = rbind(X, temp_X)
    iter = iter + 1
    print('iter')
    print(iter)
}
X[is.nan(X)] =  -1 
X[!is.finite(X)] = -1
#print(X)
hw_noise_data = X
#print(hw_noise_data)
num_of_points = nrow(X)
    print('num of pointss')
print (num_of_points)

g_noise=Urerf(X, trees = 200, mtry=6, Progress = TRUE, LinearCombo=TRUE, splitCrit = "bicfast", normalizeData = TRUE)
save(g_noise, file="urerf_noise.Rdata")
print("Urerf done")

W_noise=g_noise$similarityMatrix
D_rf_noise=1-W_noise
D_iso_noise_p_r_list = p_r_list(D_rf_noise, labels, at_K, num_of_points)
urerf_prec_list_bicfast = D_iso_noise_p_r_list$precisionList
urerf_rec_list_bicfast = D_iso_noise_p_r_list$recallList
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
g_noise1=randomForest(hw_noise_data, ntree=200, keep.forest=FALSE, proximity=TRUE)
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
