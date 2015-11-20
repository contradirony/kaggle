##########################################################################
# Description: Model using recipe-recipe similarity
# simple model that matches new recipe to existing recipes
# created: Nov 2015
# @author: samlam
###########################################################################

load("~/kaggle/whats-cooking/recipe-ingred-mat.RData")
dim(out)
object.size(out)

library(jsonlite)
test<-fromJSON("/media/sammi/Sammi/kaggle/whats-cooking/test.json",flatten=TRUE)
n.newrecipes <- dim(test)[1]

head(diff_cuisines)
head(ingred.list)

## test recipes' ingredient vectors
newrecipes <- Matrix(0,nrow=n.newrecipes,ncol=ningred, sparse = TRUE) # test <- Matrix(0,nrow=5,ncol=4, sparse = TRUE) 
for(r in 1:n.newrecipes){ 
  r1 <- test[r,]
  ingred1 <- unlist(r1$ingredients)
  newrecipes[i=r,j=which(ingred.list %in% ingred1)] <- 1
}

## find which recipes these new ones are closest to in the train matrix
# cosSim <- function(x,y){ x %*% t(y) / sqrt(x%*%t(x) * y%*%t(y))}

candidates <- NULL
topcandidate <- NULL
for(i in 1:n.newrecipes){
  a <- which(newrecipes[i,]>0)
    if( length(out[a,])/39774 > 1) {
      temp <- train[which(rowSums(out[a,]) == max(rowSums(out[a,]))),]$cuisine
    }
  temp <- train[which(out[a,] == max(out[a,])),]$cuisine
  candidates[[i]] <- temp
  topcandidate[i] <- names(sort(table(temp), decreasing=TRUE))[1]
}

submit <- data.frame(id = test$id, cuisine = topcandidate)
write.csv(submit, file = "/media/sammi/Sammi/kaggle/whats-cooking/submission1.csv", row.names = FALSE)

## scores: 0.24487 , +5% (out of max) improve on baseline of 0.19268
## 1b gives 0.17880 (swap [,a] to [a,])
## 1c gives 0.08870 (code fix: temp definition from only case when length(out[a,])/39774 < 1)



library(foreach)
library(doMC)
registerDoMC(cores=3)
ptm <- proc.time()

# candidates <- NULL
topcandidate <- NULL
for(i in 1:n.newrecipes){
  a <- which(newrecipes[i,]>0)
  relevant.rows <- unique(which(out[,a]>0, arr.ind=TRUE)[,1])
 # sim <- NULL
  sim        <- foreach(j=1:length(relevant.rows),.inorder=FALSE,.combine = rbind) %dopar% {
    tem <- length(intersect(which(out[relevant.rows[j],]>0),a))/( length(which(out[relevant.rows[j],]>0)) +length(a) - 
                                                            length(intersect(which(out[relevant.rows[j],]>0),a)) )
    return(c(relevant.rows[j],tem))
      }
  sim[which(sim[,2]==max(sim[,2])),][1]
#   for(j in relevant.rows){
#     # jaccard sim
#       tem <- length(intersect(which(out[j,]>0),a))/( length(which(out[j,]>0) +length(a) - 
#                                                               length(intersect(which(out[j,]>0),a))) )
#       sim <- c(sim, tem)
#     }
  
#   temp <-
#   candidates[[i]] <- temp
  topcandidate[i] <- train[sim[which(sim[,2]==max(sim[,2])),][1],]$cuisine
  if(i%%1000==0){print(i)}
}

ptm - proc.time()


submit <- data.frame(id = test$id, cuisine = topcandidate)
write.csv(submit, file = "/media/sammi/Sammi/kaggle/whats-cooking/submission2.csv", row.names = FALSE)
##

