

#- load all libs
library(jsonlite)

#- load the JSON and convert to list
train<-fromJSON("/media/sammi/Sammi/kaggle/whats-cooking/train.json",flatten=TRUE)
test<-fromJSON("/media/sammi/Sammi/kaggle/whats-cooking/test.json",flatten=TRUE)

nrow(test)

test$cuisine <- rep("italian", 9944)
submit <- data.frame(id = test$id, cuisine = test$cuisine)
write.csv(submit, file = "submission.csv", row.names = FALSE)
