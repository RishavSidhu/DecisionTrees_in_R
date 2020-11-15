library(dplyr)
library(rpart)
library(rpart.plot)

set.seed(180)
eBayAuctions <-read.csv("eBayAuctions.csv")
str(eBayAuctions)

shuffle_index <- sample(1:nrow(eBayAuctions))
eBayAuctions <- eBayAuctions[shuffle_index, ]
head(eBayAuctions)

names(eBayAuctions)[names(eBayAuctions) == "Competitive."] <- "Competitive"

clean_eBayAuctions <- eBayAuctions %>%
  select(-c()) %>%
  mutate(Competitive = factor(Competitive, 
                              levels = c(0, 1), 
                              labels = c('No', 'Yes')),
         Duration = as.factor(Duration))

glimpse(clean_eBayAuctions)

create_train_test <- function(data, size = 0.6, train = TRUE) {
  n_row = nrow(data)
  total_row = size * n_row
  train_sample <- 1:total_row
  if (train == TRUE) {
    return (data[train_sample, ])
  } else {
    return (data[-train_sample, ])
  }
}

# function brings back a test set if train argument is false;
data_train <- create_train_test(clean_eBayAuctions, 0.6, train = TRUE)
data_test <- create_train_test(clean_eBayAuctions, 0.6, train = FALSE)
dim(data_train)
dim(data_test)
dim(clean_eBayAuctions)


fit <- rpart(Competitive~., 
             data = data_train, 
             method = 'class', 
             minbucket = 50,
             maxdepth = 7)
rpart.plot(fit, extra = 106)


#part 2 d
clean_eBayAuctions <- eBayAuctions %>%
  select(-c(currency,endDay,ClosePrice)) %>%
  mutate(Competitive = factor(Competitive, 
                              levels = c(0, 1), 
                              labels = c('No', 'Yes')),
         Duration = as.factor(Duration))

glimpse(clean_eBayAuctions)

data_train <- create_train_test(clean_eBayAuctions, 0.6, train = TRUE)
data_test <- create_train_test(clean_eBayAuctions, 0.6, train = FALSE)
dim(data_train)
dim(data_test)

fit <- rpart(Competitive~., 
             data = data_train, 
             method = 'class', 
             minbucket = 50,
             maxdepth = 7)
rpart.plot(fit, extra = "auto")

plot(clean_eBayAuctions$OpenPrice, 
     clean_eBayAuctions$sellerRating, 
     xlab="Open Price ", 
     ylab="Seller Rating ",
     col = clean_eBayAuctions$Competitive,
     pch = 19)
abline(lm(clean_eBayAuctions$sellerRating ~ clean_eBayAuctions$OpenPrice, data = clean_eBayAuctions), col = "blue")
lines(lowess(clean_eBayAuctions$sellerRating ~ clean_eBayAuctions$OpenPrice), col="green")
install.packages("car")
library(car)
scatterplot(clean_eBayAuctions$sellerRating ~ clean_eBayAuctions$OpenPrice | clean_eBayAuctions$Competitive, 
            data=clean_eBayAuctions,
            xlab="Open Price ", 
            ylab="Seller Rating ")
