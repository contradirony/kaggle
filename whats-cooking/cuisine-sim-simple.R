##########################################################################
# Description: Model using recipe-'cuisine' similarity
# For each cuisine, create an 'ideal' vector so when a new recipe comes in,
# see which type it falls under
#
# 2 versions possible: manual vs random forest
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

# library(Matrix)
# # library(foreach)
# # library(doMC)
# # registerDoMC(cores=3)
# 
# 
# ptm <- proc.time()
# out <- Matrix(0,nrow=nrecipes,ncol=ningred, sparse = TRUE) # test <- Matrix(0,nrow=5,ncol=4, sparse = TRUE) 
# for(r in 1:nrecipes){ 
#   r1 <- train[r,]
#   ingred1 <- unlist(r1$ingredients)
#   out[i=r,j=which(ingred.list %in% ingred1)] <- 1
# }
# proc.time()-ptm


## test recipes' ingredient vectors
newrecipes <- Matrix(0,nrow=n.newrecipes,ncol=ningred, sparse = TRUE) # test <- Matrix(0,nrow=5,ncol=4, sparse = TRUE) 
for(r in 1:n.newrecipes){ 
  r1 <- test[r,]
  ingred1 <- unlist(r1$ingredients)
  newrecipes[i=r,j=which(ingred.list %in% ingred1)] <- 1
}


# For each cuisine, create an 'ideal' vector so when a new recipe comes in,
# Description: Model using recipe-'cuisine' similarity

#   print(cuis)
#   all.cuis.ingredients <- unlist(cuis.recipes$ingredients)
#   cuis.recipes <- train[which(train$cuisine==cuis),]

diff_cuisines<-unique(train$cuisine)
cuis.mat.all<-NULL
for(i in diff_cuisines){
  cuis.mat <- out[which(train$cuisine==i),]
  cuis.mat.all <- rbind(cuis.mat.all, colSums(cuis.mat))
  print(i)
}

cuis.mat.all <- cuis.mat.all/colSums(cuis.mat.all)[col(cuis.mat.all)]
# cuis.mat.all[is.na(cuis.mat.all)]<-0

topcandidate <- NULL
#for(r in 1:1000){ 
  for(r in 1:n.newrecipes){ 
    corlist <- cov(newrecipes[r,],t(cuis.mat.all))
  temp <- diff_cuisines[which(max(corlist) == corlist)]
  topcandidate[r] <-temp
  if(r%%100==0){  print(r)}
}

submit <- data.frame(id = test$id, cuisine = topcandidate)
write.csv(submit, file = "/media/sammi/Sammi/kaggle/whats-cooking/submission2h.csv", row.names = FALSE)

## spearman cor 0.17498 bad
## cov 0.7145 ok

####
cuis.mat.all.test <- NULL
for(i in diff_cuisines){
  cuis.mat <- out[which(train$cuisine==i),]
  cuis.mat.all.test <- rbind(cuis.mat.all.test, colSums(cuis.mat))
  print(i)
}

cuis.mat.all.test <- rbind(cuis.mat.all.test, newrecipes)
newrecipes.norm <- cuis.mat.all.test/colSums(cuis.mat.all.test)[col(cuis.mat.all.test)]

newrecipes.norm.test <- newrecipes.norm[21:(n.newrecipes+20),]
####

topcandidate <- NULL
for(r in 1:n.newrecipes){ 
  corlist <- cor(newrecipes.norm.test[r,],t(cuis.mat.all))
  temp <- diff_cuisines[which(max(corlist) == corlist)]
  topcandidate[r] <-temp
  }

submit <- data.frame(id = test$id, cuisine = topcandidate)
write.csv(submit, file = "/media/sammi/Sammi/kaggle/whats-cooking/submission2e.csv", row.names = FALSE)

## scores: 0.72989 21
## big improvement from simple vec-vec highest ingredient overlap
## expect some marginal improvement through better ingredient matrix

## scores:0.53751 2b
## scores:0.68775 2c

## scores:0.59242 2e -- normalised test















