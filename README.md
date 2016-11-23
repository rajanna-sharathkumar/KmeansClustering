# KmeansClustering
k-means algorithm using Euclidean distance

Programming language used: R

Steps to run the code:
1.On the command line go the directory which has all the files 
2.Type or copy and paste the below command to run the python program on command line
       Rscript --vanilla kmeans 10 test_data.txt output.txt
       
Note:You can change the value of K in the above from 25 to any other number.
Also you can change the name of the output file if the output file with the name already exists.

Initialization:Based on the input parameter(k), randomly select k points as centroids.

Termination Condition:
The usual termination condition in k-means is when the centroids no longer move.Here, I also limited my update step to a maximum of 25 iterations.

Output:

<cluster-id> <List of points ids separated by comma>
For example,
1 2, 4, 7, 10

Validation:
The usual method of evaluating the goodness of clustering is used. It is the Sum of Squared Error function, defined as:
K
SSE = ∑∑dist2(mi,x)
i=1 x∈Ci
      
where mi is the centroid of the ith cluster.
SSE value for different K values

k=10 SSE=0.757581
k=14 SSE=0.487555
k=8  SSE= 0.903088
k=15 SSE=0.437382
k=5 SSE=1.550493  
