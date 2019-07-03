# plot 5 curves in 1 plot
library(stats)
library(MASS)
library(Matrix)
library(scatterplot3d)
library(rerf)
library(vegan)
source("precision_recall.R")
library(umap)
library(geosphere)
library(rgl)

normalizeData <- function(X) {
  X <- sweep(X, 2, apply(X, 2, min), "-")
  sweep(X, 2, apply(X, 2, max), "/")
}

lim <- function(x){c(-max(abs(x)), max(abs(x))) * 1.1}

hw_data<-function(num_of_points=500){
  set.seed(1)
  t=runif(num_of_points, min = 0, max = 1)
  t=sort(t)
  x1=t^2
  x2=2*t*(1-t)
  x3=(1-t)^2
  data = cbind(x1, x2, x3)

 # return(list(data, t))
}


# generate D_geo, this is the geodesic distance
hw_geodesic<-function(t, num_of_points){
  f = function(x){sqrt(8*(3*x^2-3*x+1))}
  D_geo = matrix(rep(0, num_of_points*num_of_points), nrow = num_of_points, ncol = num_of_points)
  j=1
  while (j <= num_of_points){
    i=1
    while( i <= num_of_points){
      D_geo[i,j] = abs(integrate(f,t[i],t[j])$value)
      i = i +1
    }
    j=j+1
  }
  return(D_geo)
}

linear_data<-function(num_of_points=500){
  set.seed(1)
  t=runif(num_of_points, min = 0, max = 1)
  x1 = 21*t
  x2 = 6*t
  x3 = 14*t
  data = cbind(x1, x2, x3)
  return(list(data, t))
}



# generate D_geo, this is the geodesic distance
linear_geodesic<-function(t, num_of_points){
  f = function(x){4*x + 6*x + 9}
  D_geo = matrix(rep(0, num_of_points*num_of_points), nrow = num_of_points, ncol = num_of_points)
  j=1
  while (j <= num_of_points){
    i=1
    while( i <= num_of_points){
      D_geo[i,j] = abs(integrate(f,t[i],t[j])$value)
      i = i +1
    }
    j=j+1
  }
  return(D_geo)
}

mog_data<-function(num_of_points=500){
  t=runif(num_of_points, min = 0, max = 1)
    N <- num_of_points
    components <- sample(1:3,prob=c(0.3,0.3,0.4),size=N,replace=TRUE)
    mus <- c(0,3,-3)
    sds <- sqrt(c(1,1,1))
    samples <- rnorm(n=N,mean=mus[components],sd=sds[components])
    samples2 <- rnorm(n=N,mean=mus[components],sd=sds[components])
    samples3 <- rnorm(n=N,mean=mus[components],sd=sds[components])
    data = cbind(samples, samples2, samples3)
    return(list(data, t))
    #plot(data[1:(num_of_points),1], data[1:(num_of_points),2], xlab="x", ylab="y", col='red')
 #   plt=plot3d( data[1:(num_of_points),1], data[1:(num_of_points),2], data[1:(num_of_points),3], col='red', size=3, box=TRUE, axes=FALSE)
#rgl.bbox(alpha=0.8)
 #     rgl.postscript("persp3dd_mog.pdf","pdf")
  #  return (plt) 
}


mog_geodesic<-function(data){
     #D_geo = matrix(rep(3, num_of_points*num_of_points), nrow = num_of_points, ncol = num_of_points)
    #return(D_geo)
    #return (as.matrix(dist(as.matrix(data))))
    
    return(as.matrix(dist(data)))
}

helix_data<-function(num_of_points=500){
  set.seed(1)
  t = runif(num_of_points, 0, 1)
  p = (3 * pi / 2) * (1 + 2*sort(t));
  samples = as.matrix(cbind(cbind(2*p*cos(2*p), 2*p*sin(2*p)), 2*p))
data = samples
    return(list(data, p))
#plot_ret=    plot3d(data[1:(num_of_points),1], data[1:(num_of_points),2], data[1:(num_of_points),3],  col='red', size=3, axes=FALSE, box=TRUE)
#rgl.bbox(alpha=0.8)
#rgl.postscript("persp3dd_helix.pdf","pdf")
#  return(plot_ret)
}


# generate D_geo, this is the geodesic distance for helix
helix_geodesic<-function(t, num_of_points){
  f = function(p){sqrt( (2*cos(2*p)-4*p*sin(2*p))^2 + (2*sin(2*p) + 4*p*cos(2*p))^2  + 4)}
  D_geo = matrix(rep(0, num_of_points*num_of_points), nrow = num_of_points, ncol = num_of_points)
  j=1
  while (j <= num_of_points){
    i=1
    while( i <= num_of_points){
      D_geo[i,j] = abs(integrate(f,t[i],t[j])$value)
      i = i +1
    }
    j=j+1
  }
  return(D_geo)
}

#this generate ellipsoid data
upper_sphere<-function(r=3, N=500, plot=FALSE){
  
  set.seed(1)
  data=c()
  
  parameters=c()
  
  index=1
  while(index<=N){
    u_v=c(runif(1, 0, 2*pi), runif(1, 0, pi))
    parameters=rbind(parameters, u_v)
    index=index+1
  }
  rownames(parameters) <- c()
  colnames(parameters) <- c()
  
  data=c()
  row=1
  while (row<=dim(parameters)[1]){
    x = r*cos(parameters[row, 1])*sin(parameters[row,2])
    y = r*sin(parameters[row, 1])*sin(parameters[row,2])
    z = r*cos(parameters[row,2])
    data1=cbind(cbind(x,y), z)
    data=rbind(data, data1)
    row=row+1
  }
  
  #data=(data[order(data[,1]),])
  if (plot){
    plot3d(data[1:(num_of_points/5),1], data[1:(num_of_points/5),2], data[1:(num_of_points/5),3], col='red', size=3, xlim = c(-r-1,r+1), ylim=c(-r-1,r+1), zlim = c(-r-1, r+1))
    plot3d(data[(num_of_points/5+1):(2*num_of_points/5),1], data[(num_of_points/5+1):(2*num_of_points/5),2], data[(num_of_points/5+1):(2*num_of_points/5),3], col='green', size=3, add=TRUE)
    plot3d(data[(2*num_of_points/5+1):(3*num_of_points/5),1], data[(2*num_of_points/5+1):(3*num_of_points/5),2], data[(2*num_of_points/5+1):(3*num_of_points/5),3], col='orange', size=3, add=TRUE)
    plot3d(data[(3*num_of_points/5+1):(4*num_of_points/5),1], data[(3*num_of_points/5+1):(4*num_of_points/5),2], data[(3*num_of_points/5+1):(4*num_of_points/5),3], col='blue', size=3, add=TRUE)
    plot3d(data[(4*num_of_points/5+1):(num_of_points),1], data[(4*num_of_points/5+1):(num_of_points),2], data[(4*num_of_points/5+1):(num_of_points),3], size=3, add=TRUE)
  }
  return(list(data, parameters))
}

#we will generate the geodesic distance
f = function(data1, data2, r){ 
  value=(t(data1)%*%data2/(r^2))
  phi= acos(pmin(pmax(value,-1.0),1.0))
  d=r*phi
  return(d)
}

sphere_geodesic<-function(data, r, num_of_points){
  D_geo = matrix(rep(0, num_of_points*num_of_points), nrow = num_of_points, ncol = num_of_points)
  j=1
  while (j <= num_of_points){
    i=1
    while( i <= num_of_points){
      D_geo[i,j] = f(data[i,], data[j,], r)
      i = i +1
    }
    j=j+1
  }
  diag(D_geo)=0
  return(D_geo)
}

#this generate noise
generate_high_dim_gaussian_noise<-function(num_of_points, noise_dim=6, const=70){
  matrix_of_0 = matrix(rep(0, num_of_points*(noise_dim)), nrow = num_of_points, ncol = noise_dim)
  cov_matrix = matrix(rep(0, noise_dim*noise_dim), nrow = noise_dim, ncol = noise_dim)
  diag(cov_matrix) = c(rep(const, noise_dim))
  Sig1 = cov_matrix
  noise = mvrnorm(n = num_of_points, (rep(0, noise_dim)), Sig1, tol = 1e-7, empirical = FALSE, EISPACK = FALSE)
  rownames(noise) <- c()
  colnames(noise) <- c()
  return(noise)
}

normalizeData <- function(X) {
  X <- sweep(X, 2, apply(X, 2, min), "-")
  sweep(X, 2, apply(X, 2, max), "/")
}

# #### We generate the first noise 

generate_high_dim_gaussian_noise<-function(num_of_points, noise_dim=6, const=70){
  matrix_of_0 = matrix(rep(0, num_of_points*(noise_dim)), nrow = num_of_points, ncol = noise_dim)
  cov_matrix = matrix(rep(0, noise_dim*noise_dim), nrow = noise_dim, ncol = noise_dim)
  diag(cov_matrix) = c(rep(const, noise_dim))
  Sig1 = cov_matrix
  noise = mvrnorm(n = num_of_points, (rep(0, noise_dim)), Sig1, tol = 1e-7, empirical = FALSE, EISPACK = FALSE)
  rownames(noise) <- c()
  colnames(noise) <- c()
  return(noise)
}

generate_high_dim_uniform_noise<-function(num_of_points, noise_dim=6, const=70){
  #matrix_of_0 = matrix(rep(0, num_of_points*(noise_dim)), nrow = num_of_points, ncol = noise_dim)
  #cov_matrix = matrix(rep(0, noise_dim*noise_dim), nrow = noise_dim, ncol = noise_dim)
  #diag(cov_matrix) = c(rep(const, noise_dim))
  #Sig1 = cov_matrix
  #noise = mvrnorm(n = num_of_points, (rep(0, noise_dim)), Sig1, tol = 1e-7, empirical = FALSE, EISPACK = FALSE)
  noise=replicate(noise_dim, runif(num_of_points, (-1)*const, const)) 
  rownames(noise) <- c()
  colnames(noise) <- c()
  return(noise)
}
