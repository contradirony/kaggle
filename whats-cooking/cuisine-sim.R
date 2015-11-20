##########################################################################
# Description: Model using recipe-'cuisine' similarity
# For each cuisine, create an 'ideal' vector so when a new recipe comes in,
# see which type it falls under
#
# 2 versions possible: manual vs random forest
# created: Nov 2015
# @author: samlam
###########################################################################


# load("~/kaggle/whats-cooking/recipe-ingred-mat.RData")
# dim(out)
# object.size(out)
library(jsonlite)
train <- fromJSON("/media/sammi/Sammi/kaggle/whats-cooking/train.json",flatten=TRUE)
diff_cuisines<-unique(train$cuisine)

ingred.list <- unique(unlist(train$ingredients))
ningred <- length(ingred.list)

ingred.list <- gsub("\\s*\\([^\\)]+\\)","",ingred.list)
ingred.list[grep(",", ingred.list)]
ingred.list[grep("\\(", ingred.list)]

ingred.list.sep <- strsplit(ingred.list,' ')
ingred.lengths <- unlist(lapply(ingred.list.sep, length))

# sorted_words <- names(sort(table(strsplit(tolower(paste(readLines("/media/sammi/Sammi/kaggle/whats-cooking/foodtxt.txt"), collapse = " ")), "[^a-z]+")), decreasing = TRUE))
# correct <- function(word) { c(sorted_words[ adist(word, sorted_words) <= min(adist(word, sorted_words), 2)], word)[1] }

### create recipe-ingred matrix

ingred.list <- unique(tolower(ingred.list))

length(ingred.list)
ingred.list.sep <- strsplit(ingred.list,' ')
ingred.lengths <- unlist(lapply(ingred.list.sep, length))


# remove blank spaces
for(x in 1:length(ingred.list.sep)){
  ingred.list.sep[[x]] <- ingred.list.sep[[x]][which(ingred.list.sep[[x]]!="")]
  }

stopwords <- c('a','and','the','of', '&', 'in', 'to','lb.', 'lb', 1:100,
               'into', 'pieces', 'piece', '-', ',', 'better', 'than', 'or', 'recip', 'with',
               'made', 'inch')

ingred.split <- ingred.list.sep[which(ingred.lengths > 3) ]
length(table(unlist(ingred.split)))

for(x in 1:length(ingred.split)){
  #print(x)
  ingred.split[[x]] <- ingred.split[[x]][which(!ingred.split[[x]] %in% stopwords)]
  ingred.split[[x]] <- gsub(",","",ingred.split[[x]])
  ingred.split[[x]] <- gsub("-","",ingred.split[[x]])
  ingred.split[[x]] <- gsub("®","",ingred.split[[x]])
  ingred.split[[x]] <- gsub("™","",ingred.split[[x]])
  ingred.split[[x]] <- gsub("!","",ingred.split[[x]])
  ingred.split[[x]] <- gsub("almonds","almond",ingred.split[[x]])
  ingred.split[[x]] <- gsub("\\<mayonnais\\>","mayonnaise",ingred.split[[x]])
  ingred.split[[x]] <- gsub("\\<believ\\>","believe",ingred.split[[x]])
  ingred.split[[x]] <- gsub("breasts","breast",ingred.split[[x]])
  ingred.split[[x]] <- gsub("butter!","butter",ingred.split[[x]])
  ingred.split[[x]] <- gsub("\\<chees\\>","cheese",ingred.split[[x]])
  ingred.split[[x]] <- gsub("chilies","chili",ingred.split[[x]])
  ingred.split[[x]] <- gsub("chuck","chunk",ingred.split[[x]])
  ingred.split[[x]] <- gsub("chunks","chunk",ingred.split[[x]])
  ingred.split[[x]] <- gsub("\\<coars\\>","coarse",ingred.split[[x]])
  ingred.split[[x]] <- gsub("creami","creamy",ingred.split[[x]])
  ingred.split[[x]] <- gsub("\\<doubl\\>","double",ingred.split[[x]])
  ingred.split[[x]] <- gsub("\\<fatfre\\>","fatfree",ingred.split[[x]])
  ingred.split[[x]] <- gsub("foods","food",ingred.split[[x]])
  ingred.split[[x]] <- gsub("food's","food",ingred.split[[x]])
  ingred.split[[x]] <- gsub("hens","hen",ingred.split[[x]])
  ingred.split[[x]] <- gsub("\\<lettuc\\>","lettuce",ingred.split[[x]])
  ingred.split[[x]] <- gsub("mushrooms","mushroom",ingred.split[[x]])
  ingred.split[[x]] <- gsub("onions","onion",ingred.split[[x]])
  ingred.split[[x]] <- gsub("\\<penn\\>","penne",ingred.split[[x]])
  ingred.split[[x]] <- gsub("pineapples","pineapple",ingred.split[[x]])
  ingred.split[[x]] <- gsub("\\<rib\\>","ribs",ingred.split[[x]])
  ingred.split[[x]] <- gsub("reduc","reduced",ingred.split[[x]])
  ingred.split[[x]] <- gsub("\\<sauc\\>","sauce",ingred.split[[x]])
  ingred.split[[x]] <- gsub("tortillas","tortilla",ingred.split[[x]])
  ingred.split[[x]] <- gsub("\\<vinaigrettzz>","vinaigrette",ingred.split[[x]])
  ingred.split[[x]] <- gsub("yoghurt","yogurt",ingred.split[[x]])
}

head(table(unlist(ingred.split)))
length(table(unlist(ingred.split)))

ingred.new <- unique(unlist(c( ingred.list.sep[which(ingred.lengths <= 3) ], unique(unlist(ingred.split)))))
ningred.new <- length(ingred.new)
nrecipes <- length(train[,1])

library(Matrix)
# library(foreach)
# library(doMC)
# registerDoMC(cores=3)
ptm <- proc.time()
out <- Matrix(0,nrow=nrecipes,ncol=ningred.new, sparse = TRUE) # test <- Matrix(0,nrow=5,ncol=4, sparse = TRUE) 
 for(r in 1:nrecipes){ 
#  for(r in 1:1000){ 
    r1 <- train[r,]
  ingred1 <- unlist(r1$ingredients)
  ingred1 <- tolower(ingred1)
  ingred1 <- strsplit(ingred1, " ")
  ingred.lengths1 <- unlist(lapply(ingred1, length))
  
  for(x in 1:length(ingred1)){
    ingred1[[x]] <- ingred1[[x]][which(ingred1[[x]]!="")]
  }
  
  ingred.new1 <- unique(unlist(ingred1))
  
  if( length(which(ingred.lengths1 > 3)) > 0 ){
    ingred2 <- unlist(ingred1[which(ingred.lengths1 > 3) ])
      ingred2 <- ingred2[which(!ingred2 %in% stopwords)]
      ingred2 <- gsub(",","",ingred2)
      ingred2 <- gsub("-","",ingred2)
      ingred2 <- gsub("®","",ingred2)
      ingred2 <- gsub("™","",ingred2)
      ingred2 <- gsub("!","",ingred2)
      ingred2 <- gsub("almonds","almond",ingred2)
      ingred2 <- gsub("\\<mayonnais\\>","mayonnaise",ingred2)
      ingred2 <- gsub("\\<believ\\>","believe",ingred2)
      ingred2 <- gsub("breasts","breast",ingred2)
      ingred2 <- gsub("butter!","butter",ingred2)
      ingred2 <- gsub("\\<chees\\>","cheese",ingred2)
      ingred2 <- gsub("chilies","chili",ingred2)
      ingred2 <- gsub("chuck","chunk",ingred2)
      ingred2 <- gsub("chunks","chunk",ingred2)
      ingred2 <- gsub("\\<coars\\>","coarse",ingred2)
      ingred2 <- gsub("creami","creamy",ingred2)
      ingred2 <- gsub("\\<doubl\\>","double",ingred2)
      ingred2 <- gsub("\\<fatfre\\>","fatfree",ingred2)
      ingred2 <- gsub("foods","food",ingred2)
      ingred2 <- gsub("food's","food",ingred2)
      ingred2 <- gsub("hens","hen",ingred2)
      ingred2 <- gsub("\\<lettuc\\>","lettuce",ingred2)
      ingred2 <- gsub("mushrooms","mushroom",ingred2)
      ingred2 <- gsub("onions","onion",ingred2)
      ingred2 <- gsub("\\<penn\\>","penne",ingred2)
      ingred2 <- gsub("pineapples","pineapple",ingred2)
      ingred2 <- gsub("\\<rib\\>","ribs",ingred2)
      ingred2 <- gsub("reduc","reduced",ingred2)
      ingred2 <- gsub("\\<sauc\\>","sauce",ingred2)
      ingred2 <- gsub("tortillas","tortilla",ingred2)
      ingred2 <- gsub("\\<vinaigrettzz>","vinaigrette",ingred2)
      ingred2 <- gsub("yoghurt","yogurt",ingred2)
      
      ingred1[which(ingred.lengths1 <= 3) ]
      ingred.new1 <- unique(unlist(c( ingred1[which(ingred.lengths1 <= 3) ], unique(unlist(ingred2)))))
      
  }
   out[i=r,j=which(ingred.new %in% ingred.new1)] <- 1
}

proc.time()-ptm





######################## classify test

test<-fromJSON("/media/sammi/Sammi/kaggle/whats-cooking/test.json",flatten=TRUE)
n.newrecipes <- dim(test)[1]

head(diff_cuisines)
length(ingred.list)

# test recipes' ingredient vectors
newrecipes <- Matrix(0,nrow=n.newrecipes,ncol=ningred.new, sparse = TRUE) # test <- Matrix(0,nrow=5,ncol=4, sparse = TRUE) 
for(r in 1:n.newrecipes){ 
  r1 <- test[r,]
  ingred1 <- unlist(r1$ingredients)
  ingred1 <- tolower(ingred1)
  ingred1 <- strsplit(ingred1, " ")
  ingred.lengths1 <- unlist(lapply(ingred1, length))
  
  for(x in 1:length(ingred1)){
    ingred1[[x]] <- ingred1[[x]][which(ingred1[[x]]!="")]
  }
  
  ingred.new1 <- unique(unlist(ingred1))
  
  if( length(which(ingred.lengths1 > 3)) > 0 ){
    ingred2 <- unlist(ingred1[which(ingred.lengths1 > 3) ])
    ingred2 <- ingred2[which(!ingred2 %in% stopwords)]
    ingred2 <- gsub(",","",ingred2)
    ingred2 <- gsub("-","",ingred2)
    ingred2 <- gsub("®","",ingred2)
    ingred2 <- gsub("™","",ingred2)
    ingred2 <- gsub("!","",ingred2)
    ingred2 <- gsub("almonds","almond",ingred2)
    ingred2 <- gsub("\\<mayonnais\\>","mayonnaise",ingred2)
    ingred2 <- gsub("\\<believ\\>","believe",ingred2)
    ingred2 <- gsub("breasts","breast",ingred2)
    ingred2 <- gsub("butter!","butter",ingred2)
    ingred2 <- gsub("\\<chees\\>","cheese",ingred2)
    ingred2 <- gsub("chilies","chili",ingred2)
    ingred2 <- gsub("chuck","chunk",ingred2)
    ingred2 <- gsub("chunks","chunk",ingred2)
    ingred2 <- gsub("\\<coars\\>","coarse",ingred2)
    ingred2 <- gsub("creami","creamy",ingred2)
    ingred2 <- gsub("\\<doubl\\>","double",ingred2)
    ingred2 <- gsub("\\<fatfre\\>","fatfree",ingred2)
    ingred2 <- gsub("foods","food",ingred2)
    ingred2 <- gsub("food's","food",ingred2)
    ingred2 <- gsub("hens","hen",ingred2)
    ingred2 <- gsub("\\<lettuc\\>","lettuce",ingred2)
    ingred2 <- gsub("mushrooms","mushroom",ingred2)
    ingred2 <- gsub("onions","onion",ingred2)
    ingred2 <- gsub("\\<penn\\>","penne",ingred2)
    ingred2 <- gsub("pineapples","pineapple",ingred2)
    ingred2 <- gsub("\\<rib\\>","ribs",ingred2)
    ingred2 <- gsub("reduc","reduced",ingred2)
    ingred2 <- gsub("\\<sauc\\>","sauce",ingred2)
    ingred2 <- gsub("tortillas","tortilla",ingred2)
    ingred2 <- gsub("\\<vinaigrettzz>","vinaigrette",ingred2)
    ingred2 <- gsub("yoghurt","yogurt",ingred2)
    
    ingred1[which(ingred.lengths1 <= 3) ]
    ingred.new1 <- unique(unlist(c( ingred1[which(ingred.lengths1 <= 3) ], unique(unlist(ingred2)))))
    
  }
  newrecipes[i=r,j=which(ingred.new %in% ingred.new1)] <- 1
}


diff_cuisines<-unique(train$cuisine)
cuis.mat.all<-NULL
for(i in diff_cuisines){
  cuis.mat <- out[which(train$cuisine==i),]
  cuis.mat.all <- rbind(cuis.mat.all, colSums(cuis.mat))
  print(i)
}

cuis.mat.all <- cuis.mat.all/colSums(cuis.mat.all)[col(cuis.mat.all)]
#cuis.mat.all[is.na(cuis.mat.all)]<-0



topcandidate <- NULL
for(r in 1:n.newrecipes){ 
  corlist <- cor(newrecipes[r,],t(cuis.mat.all))
  temp <- diff_cuisines[which(max(corlist) == corlist)]
  topcandidate[r] <-temp
}

submit <- data.frame(id = test$id, cuisine = topcandidate)
write.csv(submit, file = "/media/sammi/Sammi/kaggle/whats-cooking/submission2d.csv", row.names = FALSE)

#0.69077





