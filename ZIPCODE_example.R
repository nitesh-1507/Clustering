# Application of K-means algorithm to ZIPCODE data

# Rand Index Calculation example
library(fossil)
cluster1 <- c(2,2,1,3)
cluster2 <- c(3,3,2,1)
rand.index(cluster1, cluster2) # clusters match

# Load the ZIPCODE data
zipcode <- read.table("ZIPCODE.txt", header = F)

# Extract the true digits
Y_actual <- as.vector(zipcode[ , 1])

# Extract the data points
X_actual <- as.matrix(zipcode[ , -1])

# [ToDo] Try K-means algorithm nRep times with different starting points on ZIPCODE data. Calculate Rand Index at each replication
nRep <- 50

# vector to store index at every replication
rand_index = rep(0, nRep)

# maps the replication start time
start_time = Sys.time()

# calculating similarity on every iteration
for(i in 1:nRep){
  
  # predicted clusters
  Y_pred = MyKmeans(X_actual, 10, NULL, 100)
  
  # caculating similarity between actual and predicted clusters
  rand_index[i] = rand.index(Y_actual, Y_pred)
  
}

# maps the replication end time
end_time = Sys.time()


# [ToDo] Report mean Rand Index

rand_index_mean = mean(rand_index)

# [ToDo] Report mean run time for one replication on your machine

mean_time = ((as.integer(end_time - start_time)) * 60) / 50



