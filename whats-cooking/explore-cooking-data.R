##########################################################################
# Description: Explore recipe dataset
# created: Nov 2015
# @author: samlam
###########################################################################

library(jsonlite)
train<-fromJSON("/media/sammi/Sammi/kaggle/whats-cooking/train.json",flatten=TRUE)
diff_cuisines<-unique(train$cuisine)
# ftable<-table(train$cuisine)
# barplot(ftable)
# irish.recipes <- train[which(train$cuisine=='irish'),]
# 
# all.irish.ingredients <- unlist(irish.recipes$ingredients)
# head(sort(table(all.irish.ingredients), decreasing=TRUE))
# 
# chinese.recipes <- train[which(train$cuisine=='chinese'),]
# all.chinese.ingredients <- unlist(chinese.recipes$ingredients)
# head(sort(table(all.chinese.ingredients), decreasing=TRUE))

# for(cuis in names(ftable)){
#   cuis.recipes <- train[which(train$cuisine==cuis),]
#   all.cuis.ingredients <- unlist(cuis.recipes$ingredients)
#   print(cuis)
#   print(head(names(sort(table(all.cuis.ingredients), decreasing=TRUE))))
# }


#test <- fromJSON("/media/sammi/Sammi/kaggle/whats-cooking/test.json",flatten=TRUE)

# 20 types of cuisines
# length(diff_cuisines)
# [1] 20

# almost 500K ingredients all round
ingred.all <- unlist(train$ingredients)
length(unlist(train$ingredients))

# [1] 428275

# about 7K unique ingredients
ingred.list <- unique(unlist(train$ingredients))
ningred <- length(ingred.list)
#[1] 6714


# summary: 
# y = 20 cuisines
# x features = 6714 ingredients (note: need to factor in new ingredients)
# r recipes/rows
# create matrix where row is recipe, column are ingredients


# ingred.list
# ptm <- proc.time()
nrecipes <- length(train[,1])
# mat <- NULL
# for(r in 1:1000){
#       r1 <- train[r,]
#       ingred1 <- unlist(r1$ingredients)
#       row1 <- rep(0,length(ingred.list))
#       row1[which(ingred.list %in% ingred1)] <- 1
#       mat <- rbind(mat, row1)
# }
# proc.time()-ptm


library(Matrix)
# library(foreach)
# library(doMC)
# registerDoMC(cores=3)
ptm <- proc.time()
out <- Matrix(0,nrow=nrecipes,ncol=ningred, sparse = TRUE) # test <- Matrix(0,nrow=5,ncol=4, sparse = TRUE) 
for(r in 1:nrecipes){ 
      r1 <- train[r,]
      ingred1 <- unlist(r1$ingredients)
      out[i=r,j=which(ingred.list %in% ingred1)] <- 1
}
proc.time()-ptm


