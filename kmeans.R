args = commandArgs(trailingOnly=TRUE)

if (length(args) < 3) {
  stop("k-means <numberOfClusters> <input-file-name> <output-file-name>", call.=FALSE)
} else{
  
  # rm(list = ls())
  # setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
  
  # calculate euclidean distance
  euc_dist <- function(x,y){
    return(sqrt((x^2 + y^2)))
  }
  
  # kmean algorithm
  k_means <- function(k,iter_max){
    #get min and max values of x and y
    minx = min(test_data$x)
    miny = min(test_data$y)
    maxx = max(test_data$x)
    maxy = max(test_data$y)
    
    #generate centroids
    cent <- c()
    for(i in 1:k){
      cent <- rbind(cent,c(round(runif(1,min = minx, max = maxx),3)
                           ,round(runif(1,min = miny, max = maxy),3)))
    }
    
    cent <- cbind(cent,0)
    test_data[1,3] <- 0
    
    
    
    # calculate new centroid
    recalculate_centroid <- function(){
      new_centroid <- c()
      for(i in 1:k){
        points <- which(test_data[,3] %in% i)
        if(length(points) != 0){
          xval <- 0
          yval <- 0
          for(point in points){
            xval <- xval + test_data[point,1]
            yval <- yval + test_data[point,2]
          }
          new_centroid <- rbind(new_centroid,c(round(xval/length(points),3)
                                               , round(yval/length(points),3)))
        }
        else{
          new_centroid <- rbind(new_centroid, c(cent[i,1],cent[i,2]))
        }
      }
      return(new_centroid)
    }
    
    test_data_bak <- test_data
    
    iter <- 0
    
    repeat{
      
      iter <- iter + 1
      
      #Assign centroids
      for(i in 1:nrow(test_data)){
        eucd <- c()
        for(j in 1:k){
          eucd[j] <- round(euc_dist(test_data[i,1]-cent[j,1] , test_data[i,2] - cent[j,2]),3)
        }
        test_data[i,3] <- match(min(eucd),eucd)
      }
      
      # new centroids
      ncent <- recalculate_centroid()
      
      for(i in 1:k){
        if( (cent[i,1]!=ncent[i,1]) || (cent[i,2]!=ncent[i,2]) ){
          cent[i,1] = ncent[i,1]
          cent[i,2] = ncent[i,2]
          cent[i,3] <- 1
        }
      }
      
      if( (length(which(cent[,3] %in% 0)) == k) || iter == 25) break
      
      cent[,3] <- 0
    }
    #
    print(paste("Max Iterations :",iter))
    
    print("Final Centroids:")
    print(cent)
    
    res <- c()
    res$cent <- cent
    res$data <- test_data
    return(res)
  }
  
  SSE <- function(res){
    dist <- 0
    for(i in 1:k){
      points <- which(res$data$V3 %in% i)
      cdist <- 0
      for(point in points){
        val <- euc_dist(res$cent[i,1]-test_data[point,1]
                        ,res$cent[i,2]-test_data[point,2])
        # val <- (res$cent[i,1]-test_data[point,1]
        #                           +res$cent[i,2]-test_data[point,2])
        cdist <- cdist + (val ^ 2)
      }
      dist <- dist + cdist
    }
    return(dist)
  }
  
  
  
  #read data from text file
  test_data <- read.table(args[2], header=TRUE)
  
  #remove column id
  test_data <- test_data[-1]
  
  # number of centroids
  k <- as.integer(args[1])
  iter_max = 25
  
  res <- k_means(k,iter_max)
  
  dist <- SSE(res)
  
  # print(dist)
  
  
  final.res <- c()
  
  for(i in 1:k){
    final.res <- rbind(final.res,c(i,
                                   paste(which(res$data[,3] %in% i), collapse = ",")))
  }
  
  final.res <- rbind(final.res,c("SSE", dist))
  res.df<-  data.frame(Centroids = final.res[,1], Centroid_points = final.res[,2])
  # print(final.res)
  write.table(res.df,args[3],row.names = FALSE, col.names = FALSE, sep = " ")
}
