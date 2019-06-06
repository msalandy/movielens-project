
###################################
# Create edx set and validation set
###################################

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- read.table(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                      col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data

set.seed(1) 
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set

validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set

removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)


###################################
# Methodology:Determining optimal value for lambda
###################################

# Cross-validation is used to choose the tuning parameter lambda.
lambdas<-seq(0,10,0.25)
rmses<-sapply(lambdas, function(l){
  mu<-mean(edx$rating)
  
  bi<-edx %>% group_by(movieId) %>% 
    summarize(bi=sum(rating-mu)/(n()+l))
  
  bu<-edx %>% left_join(bi, by="movieId") %>%
    group_by(userId) %>%
    summarize(bu=sum(rating-bi-mu)/(n()+l))
  
  yhat<-edx %>% left_join(bi, by="movieId") %>%
    left_join(bu, by="userId") %>%
    mutate(pred=mu+bi+bu) %>%
    .$pred
  
  return(RMSE(yhat, edx$rating))
})

plot(lambdas,rmses)


lambda<-lambdas[which.min(rmses)]


###################################
# Methodology:Computing the lowest RMSE
###################################

l<-0.5
mu<-mean(validation$rating)

bi<-validation %>% group_by(movieId) %>% 
  summarize(bi=sum(rating-mu)/(n()+l))

bu<-validation %>% left_join(bi, by="movieId") %>%
  group_by(userId) %>%
  summarize(bu=sum(rating-bi-mu)/(n()+l))

yhat<-validation %>% left_join(bi, by="movieId") %>%
  left_join(bu, by="userId") %>%
  mutate(pred=mu+bi+bu) %>%
  .$pred

RMSE(yhat, validation$rating)
