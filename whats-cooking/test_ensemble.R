submission1 <- read.csv("~/kaggle/whats-cooking/submission1.csv")
submission1b <- read.csv("~/kaggle/whats-cooking/submission1b.csv")
submission1c <- read.csv("~/kaggle/whats-cooking/submission1c.csv")
submission21 <- read.csv("/media/sammi/Sammi/kaggle/whats-cooking/submission21.csv")
submission2b <- read.csv("/media/sammi/Sammi/kaggle/whats-cooking/submission2b.csv")
submission2c <- read.csv("/media/sammi/Sammi/kaggle/whats-cooking/submission2c.csv")
submission2d <- read.csv("/media/sammi/Sammi/kaggle/whats-cooking/submission2d.csv")
submission2e <- read.csv("/media/sammi/Sammi/kaggle/whats-cooking/submission2e.csv")
submission2h <- read.csv("/media/sammi/Sammi/kaggle/whats-cooking/submission2h.csv")

ensemble <- NULL
for(i in 1:dim(submission2d)[1]){
  temp <-c(
                    as.character(submission21[i,]$cuisine),
                    as.character(submission2h[i,]$cuisine),
                    as.character(submission2d[i,]$cuisine),
                    as.character(submission2e[i,]$cuisine))
  ensemble[i] <- names(sort(table(temp), decreasing=TRUE))[1]
  
}


submit <- data.frame(id = submission1$id, cuisine = ensemble)
write.csv(submit, file = "/media/sammi/Sammi/kaggle/whats-cooking/submission3g.csv", row.names = FALSE)

# 0.6963 for 21, 2c, 2d
# 0.7069 for 21, 2d
# 0.71832 for 21, 2d, 2e
# 0.71432 for 21, 2c, 2e
### 0.72526 for 21, 2d, 2h
# 0.71752 for 21, 2h
# 0.71822 for 21, 2h, 2d, 2e


### 0.72989 for 21