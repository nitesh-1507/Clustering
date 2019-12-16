# Function that implements K-means algorithm for a given number of iterations. The default number of iterations is 100.
MyKmeans <- function(X, K, M = NULL, numIter = 100){
 
  # Check whether M is NULL or not. If NULL, initilize based on K random points from X. If not NULL, check for compatibility with X dimensions.
  if(length(M) == 0){
    
    M = X[sample(nrow(X), K, replace = FALSE), , drop = F]
    
  }else{
    
    if(ncol(X) != ncol(M)){
      
      stop('The number of column in M and X do not match')
    }
  }
  
  # Implement K-means algorithm 
  
  # pre-compute X^2 to be used in distance calculation
  X_square = rowSums(X * X)
  
  # loop iterration over numIter
  for(i in 1:numIter){
    
    # Distance matrix of size nrow(X) * nrow(M)
    Distance_matrix = matrix(rep(0, nrow(X) * nrow(M)), nrow(X), nrow(M))
    
    # Filling up distance matrix
    for(j in 1:nrow(M)){
      
      
      Distance_matrix[, j] = X_square - 2 * (X %*% M[j,]) + sum(M[j,] * M[j,])
      
    }
    
    # Vector of cluster assigment
    Y = rep(0, nrow(X))
    
    # Assigning clusters to each observation
    for(k in 1:nrow(X)){
      
      Y[k] = which.min(Distance_matrix[k, ])
      
    }
    
    # Cluster centroid matrix update corresponding to the number of clusters left
    M = matrix(rep(0, length(unique(Y)) * ncol(X)), length(unique(Y)), ncol(X))
    
    # Assigning new centroid after cluster prediction
    for(l in 1:length(unique(Y))){
      
      # centroid update
      M[l, ] = colMeans(X[Y == unique(Y)[l], ])
      
    }
 
    
  }
  
  # Return the vector of assignments Y
  return(Y)
}
