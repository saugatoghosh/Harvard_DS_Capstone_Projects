# RMSE function

RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}


################################
# Create edx set, validation set
################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

if(!exists("edx")){
  dl <- tempfile()
  download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

  ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

  movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
  colnames(movies) <- c("movieId", "title", "genres")
  movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))

  movielens <- left_join(ratings, movies, by = "movieId")

  # Validation set will be 10% of MovieLens data

  set.seed(1)
  # if using R 3.5 or earlier, use `set.seed(1)` instead
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
}


# Explore data
edx %>% as_tibble()
summary(edx)

#Distinct users and movies
edx %>% 
  summarize(n_users = n_distinct(userId),
            n_movies = n_distinct(movieId))

#Preprocess data 1 : Convert timestamp column to date

edx <- edx %>% mutate(date = round_date(as_datetime(timestamp), unit = "week"))
validation <- validation %>% mutate(date = round_date(as_datetime(timestamp), unit = "week"))
edx <- edx %>% select(-timestamp)
validation <- validation %>% select(-timestamp)

#Preprocess data 2 : Extract year of release from title and calculate age of movie

edx <- edx %>% mutate(release_year = as.numeric(str_sub(title,-5,-2)),
                      movie_title = str_replace(title, " \\(.*\\)", ""))
edx <- edx %>% mutate(movie_age = 2019 - release_year)
edx <-edx %>%select(-title)
validation <- validation %>% mutate(release_year = as.numeric(str_sub(title,-5,-2)),
                                    movie_title = str_replace(title, " \\(.*\\)", ""))
validation <- validation %>% mutate(movie_age = 2019 - release_year)
validation <- validation %>% select(-title)

# Visualize data

# Distribution of ratings

#create a dataframe "explore_ratings" which contains half star and whole star ratings  from the edx set : 

group <-  ifelse((edx$rating == 1 |edx$rating == 2 | edx$rating == 3 | 
                    edx$rating == 4 | edx$rating == 5) ,
                 "whole_star", 
                 "half_star") 

explore_ratings <- data.frame(edx$rating, group)

# histogram of ratings

ggplot(explore_ratings, aes(x= edx.rating, fill = group)) +
  geom_histogram( binwidth = 0.2) +
  scale_x_continuous(breaks=seq(0, 5, by= 0.5)) +
  scale_fill_manual(values = c("half_star"="purple", "whole_star"="brown")) +
  labs(x="rating", y="number of ratings") +
  ggtitle("histogram : number of ratings for each rating")

# Relationship with factors such as movie, user

# Movie vs ratings

# plot count rating by movie
edx %>% 
  count(movieId) %>% 
  ggplot(aes(n)) + 
  geom_histogram(bins = 30, color = "blue") + 
  scale_x_log10() + 
  ggtitle("Movies")

top_title <- edx %>%
  group_by(movie_title) %>%
  summarize(count=n(), avg_rating = mean(rating)) %>%
  top_n(20,count) %>%
  arrange(desc(count))

top_title %>% 
  ggplot(aes(x=reorder(movie_title, count), y=count)) +
  geom_bar(stat='identity', fill="blue") + coord_flip(y=c(0, 40000)) +
  labs(x="", y="Number of ratings") +
  geom_text(aes(label= avg_rating), hjust=-0.1, size=3) +
  labs(title="Average rating for Top 20 movies title based \n on number of ratings")

# User vs ratings

# plot count rating by user
edx %>% 
  count(userId) %>% 
  ggplot(aes(n)) + 
  geom_histogram(bins = 30, color = "orange") + 
  scale_x_log10() + 
  ggtitle("Users")

# Age of movie vs ratings

edx %>% group_by(movie_age) %>%
  summarize(rating = mean(rating)) %>%
  ggplot(aes(movie_age, rating)) +
  xlab("Age of Movie") +
  ylab("Average Ratings") +
  geom_point() +
  geom_smooth()

# Time of rating vs ratings

edx %>% group_by(date)%>%
  summarize(rating = mean(rating)) %>%
  ggplot(aes(date, rating)) +
  geom_point() +
  geom_smooth() +
  ggtitle("Timestamp, time unit : week")+
  labs(subtitle = "average ratings")

# Genre vs ratings

split_edx <- edx %>% separate_rows(genres, sep = "\\|")

top_genres <- split_edx %>%
  group_by(genres) %>%
  summarize(count = n()) %>%
  arrange(desc(count))


top_genres %>% as_tibble()

# We compute the average and standard error for each genre and plot these as error bar plots for genres with more than 100000 ratings.

split_edx %>%
  group_by(genres) %>%
  summarize(n = n(), avg = mean(rating), se = sd(rating)/sqrt(n())) %>%
  filter(n >= 100000) %>% 
  mutate(genres = reorder(genres, avg)) %>%
  ggplot(aes(x = genres, y = avg, ymin = avg - 2*se, ymax = avg + 2*se)) + 
  geom_point() +
  geom_errorbar() + 
  ylab("average ratings") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "error bar plots by genres" ) +
  theme(
    panel.background = element_rect(fill = "lightblue",
                                    colour = "lightblue",
                                    size = 0.5, linetype = "solid"),
    panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                    colour = "white"), 
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                    colour = "white")
  )

# Modeling approaches

# Simple model - Just The Average

mu_hat <- mean(edx$rating)
naive_rmse <- RMSE(validation$rating, mu_hat)

rmse_results <- tibble(method = "Just the average", RMSE = naive_rmse)
rmse_results %>% knitr::kable()

# Movie Effects

mu <- mean(edx$rating) 
movie_avgs <- edx %>% 
  group_by(movieId) %>% 
  summarize(b_i = mean(rating - mu))
movie_avgs %>% qplot(b_i, geom ="histogram", bins = 10, data = ., color = I("black"))

# create a results table with this and prior approaches
predicted_ratings <- mu + validation %>% 
  left_join(movie_avgs, by='movieId') %>%
  .$b_i

# create a results table with this and prior approach
model_1_rmse <- RMSE(predicted_ratings, validation$rating)
rmse_results <- bind_rows(rmse_results,
                          tibble(method="Movie Effect Model on test set",  
                                 RMSE = model_1_rmse ))
rmse_results %>% knitr::kable()

# Movie + User Effects

edx %>% 
  group_by(userId) %>% 
  summarize(b_u = mean(rating)) %>% 
  filter(n()>=100) %>%
  ggplot(aes(b_u)) + 
  geom_histogram(bins = 30, color = "black")

user_avgs <- edx %>% 
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_i))

predicted_ratings <- validation %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  mutate(pred = mu + b_i + b_u) %>%
  .$pred

model_2_rmse <- RMSE(predicted_ratings, validation$rating)
rmse_results <- bind_rows(rmse_results,
                          tibble(method="Movie and User Effects Model on test set",  
                                 RMSE = model_2_rmse ))
rmse_results %>% knitr::kable()

# Regularised Movie + User Effect

movie_titles <- edx %>% 
  select(movieId, movie_title) %>%
  distinct()

# 10 best movies

movie_avgs %>% left_join(movie_titles, by="movieId") %>%
  arrange(desc(b_i)) %>% 
  select(movie_title, b_i) %>% 
  slice(1:10) %>%  
  knitr::kable()

# 10 worst movies

movie_avgs %>% left_join(movie_titles, by="movieId") %>%
  arrange(b_i) %>% 
  select(movie_title, b_i) %>% 
  slice(1:10) %>%  
  knitr::kable()

# Number of times these movies rated

edx %>% count(movieId) %>% 
  left_join(movie_avgs) %>%
  left_join(movie_titles, by="movieId") %>%
  arrange(desc(b_i)) %>% 
  select(movie_title, b_i, n) %>% 
  slice(1:10) %>% 
  knitr::kable()

edx %>% count(movieId) %>% 
  left_join(movie_avgs) %>%
  left_join(movie_titles, by="movieId") %>%
  arrange(b_i) %>% 
  select(movie_title, b_i, n) %>% 
  slice(1:10) %>% 
  knitr::kable()

# Pick optimal value for lambda, the penalty term:

lambda <- seq(0, 10, 0.25)

rmses <- sapply(lambda, function(l){
  mu <- mean(edx$rating)
  
  b_i <- edx %>% 
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  
  b_u <- edx %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  
  predicted_ratings <- 
    edx %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(pred = mu + b_i + b_u) %>%
    .$pred
  
  return(RMSE(edx$rating, predicted_ratings))
})

qplot(lambda, rmses)  

lambda <- lambda[which.min(rmses)]

# compute movie effect with regularization on train set
b_i <- edx %>% 
  group_by(movieId) %>%
  summarize(b_i = sum(rating - mu)/(n()+lambda))

# compute user effect with regularization on train set
b_u <- edx %>% 
  left_join(b_i, by="movieId") %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - b_i - mu)/(n()+lambda))

# # compute predicted values on test set 
predicted_ratings <- 
  validation %>% 
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  mutate(pred = mu + b_i + b_u) %>%
  pull(pred)

# create a results table with this and prior approaches
model_3_rmse <- RMSE(validation$rating, predicted_ratings)

rmse_results <- bind_rows(rmse_results,
                          tibble(method="Reg Movie and User Effect Model on test set",  
                                 RMSE = model_3_rmse))
rmse_results %>% knitr::kable()

# Movie + User + Time Effects

# Calculate time effects ( b_t) using the training set
temp_avgs <- edx %>%
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  group_by(date) %>%
  summarize(b_t = mean(rating - mu - b_i - b_u))

# predicted ratings
predicted_ratings <- validation %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  left_join(temp_avgs, by='date') %>%
  mutate(pred = mu + b_i + b_u + b_t) %>%
  .$pred

model_4_rmse <- RMSE(predicted_ratings, validation$rating)
rmse_results <- bind_rows(rmse_results,
                          tibble(method="Movie, User and Time Effects Model on test set",  
                                 RMSE = model_4_rmse ))
rmse_results %>% knitr::kable()

# Movie + User + Movie_Age Effect

# Calculate age effects ( b_a) using the training set
age_avgs <- edx %>%
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  group_by(movie_age) %>%
  summarize(b_a = mean(rating - mu - b_i - b_u))

# predicted ratings
predicted_ratings <- validation %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  left_join(age_avgs, by='movie_age') %>%
  mutate(pred = mu + b_i + b_u + b_a) %>%
  .$pred

model_5_rmse <- RMSE(predicted_ratings, validation$rating)
rmse_results <- bind_rows(rmse_results,
                          tibble(method="Movie, User and Age Effects Model on test set",  
                                 RMSE = model_5_rmse ))
rmse_results %>% knitr::kable()

#Matrix Factorization with parallel stochastic gradient descent

#Create a copy of training(edx) and validation sets and retain only userId, movieId and rating features. Rename the three columns.

edx.copy <-  edx %>%
  select(c("userId","movieId","rating"))

names(edx.copy) <- c("user", "movie", "rating")


valid.copy <-  validation %>%
  select(c("userId","movieId","rating"))

names(valid.copy) <- c("user", "movie", "rating")

#as matrix
edx.copy <- as.matrix(edx.copy)
valid.copy <- as.matrix(valid.copy)


#write edx.copy and valid.copy tables on disk 
write.table(edx.copy , file = "trainset.txt" , sep = " " , row.names = FALSE, col.names = FALSE)
write.table(valid.copy, file = "validset.txt" , sep = " " , row.names = FALSE, col.names = FALSE)

rm(edx.copy, valid.copy)

#  data_file(): Specifies a data set from a file in the hard disk. 
if(!require(recosystem)) install.packages("recosystem", repos = "http://cran.us.r-project.org")

set.seed(123) # This is a randomized algorithm
train_set <- data_file("trainset.txt")
valid_set <- data_file("validset.txt")


#Next step is to build Recommender object
r = Reco()

# Matrix Factorization :  tuning training set

opts = r$tune(train_set, opts = list(dim = c(10, 20, 30), lrate = c(0.1, 0.2),
                                     costp_l1 = 0, costq_l1 = 0,
                                     nthread = 1, niter = 10))

# Matrix Factorization : trains the recommender model

r$train(train_set, opts = c(opts$min, nthread = 1, niter = 20))

#Making prediction on validation set and calculating RMSE:

pred_file = tempfile()

r$predict(valid_set, out_file(pred_file))  

#valid_set
scores_real <- read.table("validset.txt", header = FALSE, sep = " ")$V3
scores_pred <- scan(pred_file)


model_6_rmse <- RMSE(scores_pred, scores_real)
rmse_results <- bind_rows(rmse_results,
                          tibble(method="Matrix Factorization Model on test set",  
                                 RMSE = model_6_rmse ))
rmse_results %>% knitr::kable()

