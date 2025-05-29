#00.MOVIELENS_PROJECT_SANTIAGO_1975 #####

if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(mdatools)) install.packages("mdatools", repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")

library(caret)
library(mdatools)
library(tidyverse)

#01.HOLDOUT_DATA_SPLIT #####
# Create edx and final_holdout_test sets 
# Note: this process could take a couple of minutes

# if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
# if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
# 
# library(tidyverse)
# library(caret)

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

options(timeout = 120)

dl <- "ml-10M100K.zip"
if(!file.exists(dl))
  download.file("https://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings_file <- "ml-10M100K/ratings.dat"
if(!file.exists(ratings_file))
  unzip(dl, ratings_file)

movies_file <- "ml-10M100K/movies.dat"
if(!file.exists(movies_file))
  unzip(dl, movies_file)

ratings <- as.data.frame(str_split(read_lines(ratings_file), fixed("::"), simplify = TRUE),
                         stringsAsFactors = FALSE)
colnames(ratings) <- c("userId", "movieId", "rating", "timestamp")
ratings <- ratings %>%
  mutate(userId = as.integer(userId),
         movieId = as.integer(movieId),
         rating = as.numeric(rating),
         timestamp = as.integer(timestamp))

movies <- as.data.frame(str_split(read_lines(movies_file), fixed("::"), simplify = TRUE),
                        stringsAsFactors = FALSE)
colnames(movies) <- c("movieId", "title", "genres")
movies <- movies %>%
  mutate(movieId = as.integer(movieId))

movielens <- left_join(ratings, movies, by = "movieId")

# Final hold-out test set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding") # if using R 3.6 or later

# set.seed(1) # if using R 3.5 or earlier
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in final hold-out test set are also in edx set
final_holdout_test <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from final hold-out test set back into edx set
removed <- anti_join(temp, final_holdout_test)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)

#02.EXPLORATORY_DATA_ANALYSIS #####
#__01.write_data_to_file ####
# this step was instituted for development...
path_s <- paste(getwd(), "data", sep = "/")
dir.create(path_s)

saveRDS(edx, file = paste(path_s, "edx.rds", sep = "/")) # updated 2025-05-29...
saveRDS(final_holdout_test, file = paste(path_s, "final_holdout_test.rds", sep = "/")) # updated 2025-05-29...

rm(edx, final_holdout_test)

#__02.read_data_from_file ####
# this step was instituted for development...
path_r <- paste(getwd(), "data", sep = "/")

edx <- readRDS(file = paste(path_r, "edx.rds", sep = "/"))
final_holdout_test <- readRDS(file = paste(path_r, "final_holdout_test.rds", sep = "/"))

#__03.general_eda ####

dim(edx) # 9,000,055 observations of 6 variables...
names(edx)
str(edx)

# 10,677 unique movies in the edx training set...
edx %>%
  group_by(movieId) %>%
  slice(1) %>%
  nrow()

# 69,878 unique users in the edx training set...
edx %>%
  group_by(userId) %>%
  slice(1) %>%
  nrow()

mean_rating <- mean(edx$rating) # 3.51
sd_rating <- sd(edx$rating) # 1.06

edx %>%
  ggplot() +
  geom_histogram(aes(x = rating),
                 binwidth = 0.5,
                 color = "grey30", fill = "skyblue1") +
  geom_vline(xintercept = mean_rating, color = "red", linewidth = 1) +
  geom_vline(xintercept = c(mean_rating + sd_rating, mean_rating - sd_rating),
                            color = "orange", linewidth = 1, linetype = "dashed") +
  theme_bw()

# distribution of the n times a movie was rated...
# ~2/3 of movies have been rated less than 500 times...
# some movies have been rated more than 30000 times...
edx %>%
  group_by(movieId) %>%
  summarize(n = n()) %>%
  ggplot() +
  geom_histogram(aes(x = n),
                 binwidth = 500,
                 color = "grey30", fill = "skyblue1") +
  xlab("number of times a movie was rated") +
  ylab("count of movies") +
  scale_x_continuous(breaks = seq(0, 100000, by = 5000),
                     minor_breaks = seq(0, 100000, by = 1000)) +
  scale_y_continuous(breaks = seq(0, 10000, by = 1000),
                     minor_breaks = seq(0, 10000, by = 200)) +
  theme_bw()

# top 10 movies by number of ratings...
edx %>%
  group_by(movieId, title) %>%
  summarize(n = n()) %>%
  arrange(desc(n)) %>%
  head(., 10)

# distribution of the n times a user has rated a movie...
# a little less than half of user have rated less than 100 movies...
# 2x users have rated more than 6000 movies...
edx %>%
  group_by(userId) %>%
  summarize(n = n()) %>%
  ggplot() +
  geom_histogram(aes(x = n),
                 binwidth = 100,
                 color = "grey30", fill = "skyblue1") +
  xlab("number of times a user made a rating") +
  ylab("count of users") +
  scale_x_continuous(breaks = seq(0, 10000, by = 1000),
                     minor_breaks = seq(0, 10000, by = 200)) +
  scale_y_continuous(breaks = seq(0, 50000, by = 10000),
                     minor_breaks = seq(0, 50000, by = 1000)) +
  theme_bw()

# top 10 users by number of ratings...
edx %>%
  group_by(userId) %>%
  summarize(n = n()) %>%
  arrange(desc(n)) %>%
  head(., 10)

#__04.cleanup_prior_to_model_development ####

rm(mean_rating)
rm(sd_rating)

#03.MODEL_DEVELOPMENT #####
#__01.split_edx_into_train_and_test_sets ####

set.seed(7)
test_index <- createDataPartition(y = edx$rating,
                                  times = 1, p = 0.2,
                                  list = FALSE)

train_set <- edx[-test_index,]
temp <- edx[test_index,]

test_set <- temp %>%
  semi_join(train_set, by = "movieId") %>%
  semi_join(train_set, by = "userId")

# Add rows removed from final hold-out test set back into edx set
removed <- anti_join(temp, test_set)
train_set <- rbind(train_set, removed)

dim(train_set) # 7,200,076x observations of 6x variables...
dim(test_set) # 1,799,979x observations of 6x variables...

# cleanup before moving on...
rm(test_index)
rm(temp)
rm(removed)

#__02.define_loss_function ####

RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

#__03.mean_ratings_model (y = u + e) ####
# determine the mean rating for all movies...
mu <- mean(train_set$rating) # 3.512484...

# train_rsme...
true_ratings <- train_set %>%
  pull(rating)

predicted_ratings <- train_set %>%
  mutate(u = mu) %>%
  mutate(rating_hat = u) %>%
  pull(rating_hat)

rsme_train_1 <- RMSE(true_ratings, predicted_ratings)
rsme_train_1 # 1.0602545...

# test_rsme...
true_ratings <- test_set %>%
  pull(rating)

predicted_ratings <- test_set %>%
  mutate(u = mu) %>%
  mutate(rating_hat = u) %>%
  pull(rating_hat)

rsme_test_1 <- RMSE(true_ratings, predicted_ratings)
rsme_test_1 # 1.0606385...

#__04.movie_effects_model (y = u + bi + e) ####
# determine diff from the mean rating (u) for each movie...
movie_effects <- train_set %>%
  mutate(u = mu) %>%
  group_by(movieId) %>% 
  summarize(bi = mean(rating - u))

# train_rsme...
true_ratings <- train_set %>%
  pull(rating)

predicted_ratings <- train_set %>%
  mutate(u = mu) %>%
  left_join(movie_effects, by = 'movieId') %>%
  mutate(rating_hat = u + bi) %>%
  pull(rating_hat)

rsme_train_2 <- RMSE(true_ratings, predicted_ratings)
rsme_train_2 # 0.9420564...

# test_rsme...
true_ratings <- test_set %>%
  pull(rating)

predicted_ratings <- test_set %>%
  mutate(u = mu) %>%
  left_join(movie_effects, by = 'movieId') %>%
  mutate(rating_hat = u + bi) %>%
  pull(rating_hat)

rsme_test_2 <- RMSE(true_ratings, predicted_ratings)
rsme_test_2 # 0.9443225...

#__05.user_effects_model (y = u + bi + bu + e) ####
# determine diff from mean + movie effects rating (u + bi) for each user...
user_effects <- train_set %>%
  mutate(u = mu) %>%
  left_join(movie_effects, by = 'movieId') %>%
  group_by(userId) %>% 
  summarize(bu = mean(rating - (u + bi)))

# train_rsme...
true_ratings <- train_set %>%
  pull(rating)

predicted_ratings <- train_set %>%
  mutate(u = mu) %>%
  left_join(movie_effects, by = 'movieId') %>%
  left_join(user_effects, by = 'userId') %>%
  mutate(rating_hat = u + bi + bu) %>%
  pull(rating_hat)

rsme_train_3 <- RMSE(true_ratings, predicted_ratings)
rsme_train_3 # 0.8556421...

# test_rsme...
true_ratings <- test_set %>%
  pull(rating)

predicted_ratings <- test_set %>%
  mutate(u = mu) %>%
  left_join(movie_effects, by = 'movieId') %>%
  left_join(user_effects, by = 'userId') %>%
  mutate(rating_hat = u + bi + bu) %>%
  pull(rating_hat)

rsme_test_3 <- RMSE(true_ratings, predicted_ratings)
rsme_test_3 # 0.8664120

#__06.pca_effects_model (y = u + bi + bu + p1q1 + p2q2 + e) #####
# determine diff from mean + movie + user effects rating (u + bi + bu) for p1q1 and p2q2..
mxr <- train_set %>%
  mutate(u = mu) %>%
  left_join(movie_effects, by = 'movieId') %>%
  left_join(user_effects, by = 'userId') %>%
  mutate(residual = rating - (u + bi + bu)) %>%
  select(userId, movieId, residual) %>%
  pivot_wider(names_from = "userId",
              values_from = "residual",
              values_fill = 0) %>%
  remove_rownames %>%
  column_to_rownames("movieId") %>%
  as.matrix()

dim(mxr) # 10677 rows (movies) x 69878 columns (users)...

# check the formatting of the matrix...
mxr[1:10, 1:10] 

# randomized PCA approach...
m2 = pca(mxr, ncomp = 2, rand = c(5, 1))

# p movie "principal components"...
dim(m2$res$cal$scores) # 10677 rows (movies) x 2 columns...

p <- data.frame(movieId = rownames(m2$res$cal$scores),
                p1 = as.numeric(m2$res$cal$scores[,1]),
                p2 = as.numeric(m2$res$cal$scores[,2])) %>%
  mutate(movieId = as.numeric(movieId))
head(p)

# q user "loadings"...
dim(m2$loadings) # 69878 rows (users) x 2 columns...

q <- data.frame(userId = rownames(m2$loadings),
                q1 = as.numeric(m2$loadings[,1]),
                q2 = as.numeric(m2$loadings[,2])) %>%
  mutate(userId = as.numeric(userId))
head(q)

# train_rsme...
true_ratings <- train_set %>%
  pull(rating)

predicted_ratings <- train_set %>%
  mutate(u = mu) %>%
  left_join(movie_effects, by = 'movieId') %>%
  left_join(user_effects, by = 'userId') %>%
  left_join(p, by = 'movieId') %>%
  left_join(q, by = 'userId') %>%
  mutate(rating_hat = u + bi + bu + p1*q1 + p2*q2) %>%
  pull(rating_hat)

rsme_train_4 <- RMSE(true_ratings, predicted_ratings)
rsme_train_4 # 0.8434693...

# test_rsme...
true_ratings <- test_set %>%
  pull(rating)

predicted_ratings <- test_set %>%
  mutate(u = mu) %>%
  left_join(movie_effects, by = 'movieId') %>%
  left_join(user_effects, by = 'userId') %>%
  left_join(p, by = 'movieId') %>%
  left_join(q, by = 'userId') %>%
  mutate(rating_hat = u + bi + bu + p1*q1 + p2*q2) %>%
  pull(rating_hat)

rsme_test_4 <- RMSE(true_ratings, predicted_ratings)
rsme_test_4 # 0.8569457...

#__07.model_development_summary ####

df_mod_dev <- data.frame(model = c("mean_ratings",
                                   "movie_effects",
                                   "user_effects",
                                   "pca_effects"),
           equation = c("y = u + e",
                        "y = u + bi + e",
                        "y = u + bi + bu + e",
                        "y = u + bi + bu + p1q1 + p2q2 + e"),
           rsme_train = c(rsme_train_1,
                          rsme_train_2,
                          rsme_train_3,
                          rsme_train_4),
           rsme_test = c(rsme_test_1,
                         rsme_test_2,
                         rsme_test_3,
                         rsme_test_4))
df_mod_dev

#__08.cleanup_prior_to_training_on_edx ####

rm(train_set)
rm(test_set)
rm(mu)
rm(movie_effects)
rm(user_effects)
rm(mxr)
rm(m2)
rm(p)
rm(q)
rm(true_ratings)
rm(predicted_ratings)

#04.TRAIN_ON_COMPLETE_EDX_DATASET #####

# determine the mean rating for all movies...
mu <- mean(edx$rating) # 3.512465...

# determine diff from the mean rating (u) for each movie...
movie_effects <- edx %>%
  mutate(u = mu) %>%
  group_by(movieId) %>% 
  summarize(bi = mean(rating - u))

dim(movie_effects) # 10677 rows (movies) x 2 columns...

# determine diff from mean + movie effects rating (u + bi) for each user...
user_effects <- edx %>%
  mutate(u = mu) %>%
  left_join(movie_effects, by = 'movieId') %>%
  group_by(userId) %>% 
  summarize(bu = mean(rating - (u + bi)))

dim(user_effects) # 69878 rows (users) x 2 columns...

# determine diff from mean + movie + user effects rating (u + bi + bu) for p1q1 and p2q2..
mxr <- edx %>%
  mutate(u = mu) %>%
  left_join(movie_effects, by = 'movieId') %>%
  left_join(user_effects, by = 'userId') %>%
  mutate(residual = rating - (u + bi + bu)) %>%
  select(userId, movieId, residual) %>%
  pivot_wider(names_from = "userId",
              values_from = "residual",
              values_fill = 0) %>%
  remove_rownames %>%
  column_to_rownames("movieId") %>%
  as.matrix()

dim(mxr) # 10677 rows (movies) x 69878 columns (users)...

# check the formatting of the matrix...
mxr[1:10, 1:10] 

# randomized PCA approach...
m2 = pca(mxr, ncomp = 2, rand = c(5, 1))

# p movie "principal components"...
dim(m2$res$cal$scores) # 10677 rows (movies) x 2 columns...

p <- data.frame(movieId = rownames(m2$res$cal$scores),
                p1 = as.numeric(m2$res$cal$scores[,1]),
                p2 = as.numeric(m2$res$cal$scores[,2])) %>%
  mutate(movieId = as.numeric(movieId))
head(p)

# q user "loadings"...
dim(m2$loadings) # 69878 rows (users) x 2 columns...

q <- data.frame(userId = rownames(m2$loadings),
                q1 = as.numeric(m2$loadings[,1]),
                q2 = as.numeric(m2$loadings[,2])) %>%
  mutate(userId = as.numeric(userId))
head(q)

#05.TEST_ON_FINAL_HOLDOUT_TEST #####
#__01.test_on_edx ####

true_ratings <- edx %>%
  pull(rating)

predicted_ratings <- edx %>%
  mutate(u = mu) %>%
  left_join(movie_effects, by = 'movieId') %>%
  left_join(user_effects, by = 'userId') %>%
  left_join(p, by = 'movieId') %>%
  left_join(q, by = 'userId') %>%
  mutate(rating_hat = u + bi + bu + p1*q1 + p2*q2) %>%
  pull(rating_hat)

rsme_train_edx <- RMSE(true_ratings, predicted_ratings)
rsme_train_edx # 0.8428134

#__02.test_on_holdout ####

true_ratings <- final_holdout_test %>%
  pull(rating)

predicted_ratings <- final_holdout_test %>%
  mutate(u = mu) %>%
  left_join(movie_effects, by = 'movieId') %>%
  left_join(user_effects, by = 'userId') %>%
  left_join(p, by = 'movieId') %>%
  left_join(q, by = 'userId') %>%
  mutate(rating_hat = u + bi + bu + p1*q1 + p2*q2) %>%
  pull(rating_hat)

rsme_test_holdout <- RMSE(true_ratings, predicted_ratings)
rsme_test_holdout # 0.8539944

#__03.final_model_summary ####

df_mod_fin <- data.frame(model = c("pca_effects"),
                         equation = c("y = u + bi + bu + p1q1 + p2q2 + e"),
                         rsme_train = c(rsme_train_edx),
                         rsme_test = c(rsme_test_holdout))
df_mod_fin

#__04.cleanup_after_testing_on_holdout ####

rm(mu)
rm(movie_effects)
rm(user_effects)
rm(mxr)
rm(m2)
rm(p)
rm(q)
rm(true_ratings)
rm(predicted_ratings)
