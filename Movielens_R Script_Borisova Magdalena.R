#############################################################
## movielens dataset
#############################################################

# Note: this process could take a couple of minutes


if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)


if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

# The "fread" fucntion will be used in place of the "read.table" function because it runs faster 
# than the "read.table" function but it gives a similar result
# Disregard original code below which used the "read.table" function:
# ratings <- read.table(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))), col.names = c("userId", "movieId", "rating", "timestamp"))

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

# Check the stucture of the file 

str(ratings)

# Classes ‘data.table’ and 'data.frame':	10000054 obs. of  4 variables:
# $ userId   : int  1 1 1 1 1 1 1 1 1 1 ...
# $ movieId  : int  122 185 231 292 316 329 355 356 362 364 ...
# $ rating   : num  5 5 5 5 5 5 5 5 5 5 ...
# $ timestamp: int  838985046 838983525 838983392 838983421 838983392 838983392 838984474 838983653 838984885 838983707 ...


if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(stringr)) install.packages("stringr", repos = "http://cran.us.r-project.org")
if(!require(purrr)) install.packages("purrr", repos = "http://cran.us.r-project.org")

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")

# Check the structure of the file 

str(movies)

# ..$ : chr [1:3] "movieId" "title" "genres"

movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))

# Re-check the stucture of the file after formatting 

str(movies)

# 'data.frame':	10681 obs. of  3 variables:
# $ movieId: num  1 2 3 4 5 6 7 8 9 10 ...
# $ title  : chr  "Toy Story (1995)" "Jumanji (1995)" "Grumpier Old Men (1995)" "Waiting to Exhale (1995)" ...
# $ genres : chr  "Adventure|Animation|Children|Comedy|Fantasy" "Adventure|Children|Fantasy" "Comedy|Romance" "Comedy|Drama|Romance" ...


movielens <- left_join(ratings, movies, by = "movieId")


#############################################################
## 1. Descriptive statistics
#############################################################

if(!require(Hmisc)) install.packages("Hmisc", repos = "http://cran.us.r-project.org")

## 1.1 Numbers

dim(movielens)

# 10000054        6

colnames(movielens)

# "userId"    "movieId"   "rating"    "timestamp"  "title"   "genres"   

str(movielens)

# 'data.frame':	10000054 obs. of  6 variables:
# $ userId   : int  1 1 1 1 1 1 1 1 1 1 ...
# $ movieId  : num  122 185 231 292 316 329 355 356 362 364 ...
# $ rating   : num  5 5 5 5 5 5 5 5 5 5 ...
# $ timestamp: int  838985046 838983525 838983392 838983421 838983392 838983392 838984474 838983653 838984885 838983707 ...
# $ title    : chr  "Boomerang (1992)" "Net, The (1995)" "Dumb & Dumber (1994)" "Outbreak (1995)" ...
# $ genres   : chr  "Comedy|Romance" "Action|Crime|Thriller" "Comedy" "Action|Drama|Sci-Fi|Thriller" ...

head(movielens)

#   userId movieId rating timestamp                         title                        genres
# 1      1     122      5 838985046              Boomerang (1992)                Comedy|Romance
# 2      1     185      5 838983525               Net, The (1995)         Action|Crime|Thriller
# 3      1     231      5 838983392          Dumb & Dumber (1994)                        Comedy
# 4      1     292      5 838983421               Outbreak (1995)  Action|Drama|Sci-Fi|Thriller
# 5      1     316      5 838983392               Stargate (1994)       Action|Adventure|Sci-Fi
# 6      1     329      5 838983392 Star Trek: Generations (1994) Action|Adventure|Drama|Sci-Fi

movielens %>% as_tibble()

# 10,000,054 x 6
#    userId movieId rating timestamp title                        genres                                    
#     <int> <dbl>   <dbl>     <int> <chr>                         <chr>                                     
# 1      1     122      5 838985046 Boomerang (1992)              Comedy|Romance                            
# 2      1     185      5 838983525 Net, The (1995)               Action|Crime|Thriller                     
# 3      1     231      5 838983392 Dumb & Dumber (1994)          Comedy                                    
# 4      1     292      5 838983421 Outbreak (1995)               Action|Drama|Sci-Fi|Thriller              
# 5      1     316      5 838983392 Stargate (1994)               Action|Adventure|Sci-Fi                   
# 6      1     329      5 838983392 Star Trek: Generations (1994) Action|Adventure|Drama|Sci-Fi             
# 7      1     355      5 838984474 Flintstones, The (1994)       Children|Comedy|Fantasy                   
# 8      1     356      5 838983653 Forrest Gump (1994)           Comedy|Drama|Romance|War                  
# 9      1     362      5 838984885 Jungle Book, The (1994)       Adventure|Children|Romance                
# 10     1     364      5 838983707 Lion King, The (1994)         Adventure|Animation|Children|Drama|Musical
# … with 10,000,044 more rows

describe(movielens)

# userId 
#        n  missing distinct     
# 10000054        0    69878        

# movieId 
#        n  missing distinct     
# 10000054        0    10677  

# rating 
#        n  missing distinct      
# 10000054        0       10 
# 
# Value          0.5     1.0     1.5     2.0     2.5     3.0     3.5     4.0     4.5     5.0
# Frequency    94988  384180  118278  790306  370178 2356676  879764 2875850  585022 1544812
# Proportion   0.009   0.038   0.012   0.079   0.037   0.236   0.088   0.288   0.059   0.154

# timestamp 
#        n   missing  distinct      
# 10000054         0   7096905

# title 
#        n  missing distinct 
# 10000054        0    10676 

# genres 
# n          missing distinct 
# 10000054        0      797 

movielens %>% 
  summarise(n_users = n_distinct(userId),
            n_movies = n_distinct(movieId))

#  n_users n_movies
#  69,878  10,677

expected <- 69878 * 10677
expected

# 746,087,406

# 746,087,406 (expected number of ratings based on number of distinct users and distinct movies)
# vs. 10,000,054 (observed number of ratings based on distinct users and distinct movies in the 
# movielens dataset)

# Not every user rated every movie


## 1.2 Graphs
# log scale will help with skewness by producing a better visual representation of the data

movielens %>% 
  count(userId) %>% 
  ggplot(aes(n)) + 
  geom_histogram(bins = 30, color = "black") + 
  scale_x_log10() + 
  ggtitle("Users")

# Some users rated more movies than others 

movielens %>% 
  count(movieId) %>% 
  ggplot(aes(n)) + 
  geom_histogram(bins = 30, color = "black") + 
  scale_x_log10() + 
  ggtitle("Movies")

# Some movies had more ratings than others


#############################################################
## 2. edx set and validation set
#############################################################

## 2.1  Create edx set and validation set

# validation set (test set) will be 10% of the movieLens dataset
# edx set (train set) will be 90% of the movielens dataset

set.seed(1)
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Confirm that the userId and movieId in the validation set are also in the edx set

validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add the rows removed from the validation set back into edx set

removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)


## 2.2 Examine the sets

# Check the structure of the datasets

str(movielens)
str(edx)
str(validation)

# Check the dimensions of the datasets after the creation of the edx and validation sets

table(movielens$rating)

#   0.5       1     1.5       2     2.5       3     3.5       4     4.5       5 
# 94988  384180  118278  790306  370178 2356676  879764 2875850  585022 1544812 

table(edx$rating)

#  0.5       1      1.5       2     2.5       3     3.5       4     4.5       5 
# 85374  345679  106426  711422  333010 2121240  791624 2588430  526736 1390114

table(validation$rating)

# 0.5      1     1.5      2    2.5      3    3.5      4    4.5      5 
# 9614  38501  11852  78884  37168 235436  88140 287420  58286 154698 

# Keep the edx and validation datasets for modeling and the movielens dataset for regularization

rm(dl, ratings, movies, test_index, temp, removed)


#############################################################
## 3. RMSE
#############################################################

## Create RMSE
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}


#############################################################
## 4. Models
#############################################################

## 4.1 First model: "Modeling the average"

# Naively assume that all differences in the outcome (rating) are explained by random variation 
# (ε) and all movies received the average rating (μ) and all users gave the average rating 
# (μ) to all movies: Y = μ_hat + ε

# Find μ by calculating the the average of all ratings in the edx set

mu_hat <- mean(edx$rating)
mu_hat 

# 3.512465

# Predict all unknown ratings in the validation set

naive_rmse <- RMSE(validation$rating, mu_hat)
naive_rmse 

# 1.061202

# Create a results table from the naive model above 

rmse_results <- data_frame(method = "Just the Average Model", RMSE = naive_rmse)
rmse_results

rmse_results %>% knitr::kable()

# |method                 |     RMSE|
# |:----------------------|--------:|
# |Just the Average Model | 1.061202|


## 4.2 Second model: "Modeling the movie effects" 

# Observations from the graphs above: some movies are generally rated higher than others

# Add a term b_hat_m to represent the average rating for movie m: 
# Y_m = μ_hat + b_hat_m + ε_m

# fit_b_m <- lm(rating ~ as.factor(movieId), data = edx)
# Use the average of (Y_m − μ_hat) for each movie m instead of "fit_b_m" because the dataset 
# has a lot of observations 

movie_avgs <- edx %>% 
  group_by(movieId) %>% 
  summarise(b_hat_m = mean(rating - mu_hat))

# Plot b_hat_m

movie_avgs %>% 
  qplot(b_hat_m, geom ="histogram", bins = 10, data = ., color = I("black"))

# b_hat_m estimates vary 

# μ_hat = 3.5 (from the naive model in section 4.1) along with b_hat_m = -3 (from the graph 
# above) result in a 0.5 star rating (the lowest rating)

# μ_hat = 3.5 (from the naive model in section 4.1) along with b_hat_m = 1.5 (from the graph 
# above) result in a 5 star rating (the higest rating)

# Check if the prediction improves after b_hat_m is added to the model

predicted_ratings <- mu_hat + validation %>% 
  left_join(movie_avgs, by='movieId') %>%
  .$b_hat_m

model_2_rmse <- RMSE(predicted_ratings, validation$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method = "Movie Effect Model",  
                                     RMSE = model_2_rmse))
rmse_results %>% knitr::kable()

# |method             |      RMSE|
# |:------------------|---------:|
# |Just the average   | 1.0612018|
# |Movie Effect Model | 0.9439087|*


## 4.3 Third model: "Modeling the movie and user effects"

# Calculate and plot the average rating for user u, b_hat_u, for those that have rated 
# over 100 movies

edx %>% 
  group_by(userId) %>% 
  summarise(b_hat_u = mean(rating)) %>% 
  filter(n()>=100) %>%
  ggplot(aes(b_hat_u)) + 
  geom_histogram(bins = 30, color = "black")


# The skewness of the graph suggests that there is subjectivity in how a user rates movies
# which should be accounted for/added in the model

# Some users are generally rating movies more than others

# Add a term b_hat_u to represent the average ranking for user u: 
# Y_m,u = μ_hat + b_hat_m + b_hat_u + ε_m,u

# fit_b_u <- lm(rating ~ as.factor(movieId) + as.factor(userId), data = edx)
# Use the average of (Y_m,u − μ_hat - b_hat_m) for each user u instead of "fit_b_u" because 
# the dataset has a lot of observations

user_avgs <- edx %>%  
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarise(b_hat_u = mean(rating - mu_hat - b_hat_m))

# Plot b_hat_u 

user_avgs %>% 
  qplot(b_hat_u, geom ="histogram", bins = 10, data = ., color = I("black"))

# b_hat_u estimates vary

# μ_hat = 3.5 (from the naive model in section 4.1) along with b_hat_m = -3 (from the graph 
# in section 4.2) and b_hat_u = 0 (from the graph above) result in a 0.5 star rating (the 
# lowest rating)

# μ_hat = 3.5 (from the naive model in section 4.1) along with b_hat_m = 1 (from the graph 
# in section 4.2) and  b_hat_u = 0.5 (from the graph above) result in a 5 star rating (the 
# higest rating)

# Check if the prediction improves after b_hat_u is added to the model

predicted_ratings <- validation %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  mutate(pred = mu_hat + b_hat_m + b_hat_u) %>%
  .$pred

model_3_rmse <- RMSE(predicted_ratings, validation$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method = "Movie + User Effects Model",  
                                     RMSE = model_3_rmse))
rmse_results %>% knitr::kable()
# |method                     |      RMSE|
# |:--------------------------|---------:|
# |Just the average           | 1.0612018|
# |Movie Effect Model         | 0.9439087|
# |Movie + User Effects Model | 0.8653488|*

# RSME improved once both the movie and user effects are added to the model


#############################################################
## 5. Regularization
#############################################################

# Regularize the chosen final model above to prevent overfitting

## 5.1 Review the best and worst movies 

# Review the "Movie Effect Model" in the validation set

validation %>% 
  left_join(movie_avgs, by='movieId') %>%
  mutate(residual = rating - (mu_hat + b_hat_m)) %>%
  arrange(desc(abs(residual))) %>% 
  select(title, residual) %>% slice(1:10) 

#                               title  residual
# 1             Pokémon Heroes (2003)  3.970803
# 2  Shawshank Redemption, The (1994) -3.955131
# 3  Shawshank Redemption, The (1994) -3.955131
# 4  Shawshank Redemption, The (1994) -3.955131
# 5             Godfather, The (1972) -3.915366
# 6             Godfather, The (1972) -3.915366
# 7             Godfather, The (1972) -3.915366
# 8        Usual Suspects, The (1995) -3.865854
# 9        Usual Suspects, The (1995) -3.865854
#10        Usual Suspects, The (1995) -3.865854

# Take titles from the movielens dataset and create a dataset of the movieIds and titles

movie_titles <- movielens %>% 
  select(movieId, title) %>%
  distinct()

# Examine the best movies 

movie_avgs %>% 
  left_join(movie_titles, by="movieId") %>%
  arrange(desc(b_hat_m)) %>% 
  select(title, b_hat_m) %>% 
  slice(1:10) 

# A tibble: 10 x 2
# title                                                                              b_hat_i
# <chr>                                                                                <dbl>
# 1 Hellhounds on My Trail (1999)                                                       1.49
# 2 Satan's Tango (Sátántangó) (1994)                                                   1.49
# 3 Shadows of Forgotten Ancestors (1964)                                               1.49
# 4 Fighting Elegy (Kenka erejii) (1966)                                                1.49
# 5 Sun Alley (Sonnenallee) (1999)                                                      1.49
# 6 Blue Light, The (Das Blaue Licht) (1932)                                            1.49
# 7 Who's Singin' Over There? (a.k.a. Who Sings Over There) (Ko to tamo peva) (1980)    1.24
# 8 Human Condition II, The (Ningen no joken II) (1959)                                 1.24
# 9 Human Condition III, The (Ningen no joken III) (1961)                               1.24
# 10 Constantine's Sword (2007)                                                         1.24

# Examine the worst movies

movie_avgs %>% 
  left_join(movie_titles, by="movieId") %>%
  arrange(b_hat_m) %>% 
  select(title, b_hat_m) %>% 
  slice(1:10) 

# # A tibble: 10 x 2
# title                                       b_hat_m
# <chr>                                         <dbl>
#   1 Besotted (2001)                           -3.01
# 2 Hi-Line, The (1999)                         -3.01
# 3 Accused (Anklaget) (2005)                   -3.01
# 4 Confessions of a Superhero (2007)           -3.01
# 5 War of the Worlds 2: The Next Wave (2008)   -3.01
# 6 SuperBabies: Baby Geniuses 2 (2004)         -2.72
# 7 Hip Hop Witch, Da (2000)                    -2.69
# 8 Disaster Movie (2008)                       -2.65
# 9 From Justin to Kelly (2003)                 -2.61
# 10 Criminals (1996)                           -2.51


# Examine the best movies rating frequency in the edx set 

edx %>% count(movieId) %>% 
  left_join(movie_avgs) %>%
  left_join(movie_titles, by="movieId") %>%
  arrange(desc(b_hat_m)) %>% 
  select(title, b_hat_m, n) %>% 
  slice(1:10) 

# A tibble: 10 x 3
# title                                                                         b_hat_i    n
# <chr>                                                                          <dbl> <int>
# 1 Hellhounds on My Trail (1999)                                                 1.49     1
# 2 Satan's Tango (Sátántangó) (1994)                                             1.49     2
# 3 Shadows of Forgotten Ancestors (1964)                                         1.49     1
# 4 Fighting Elegy (Kenka erejii) (1966)                                          1.49     1
# 5 Sun Alley (Sonnenallee) (1999)                                                1.49     1
# 6 Blue Light, The (Das Blaue Licht) (1932)                                      1.49     1
# 7 Who's Singin' Over There? (a.k.a. Who Sings Over There) (Ko to tamo peva)…    1.24     4
# 8 Human Condition II, The (Ningen no joken II) (1959)                           1.24     4
# 9 Human Condition III, The (Ningen no joken III) (1961)                         1.24     4
# 10 Constantine's Sword (2007)                                                   1.24     2


# Examine the worst movies rating frequency in the edx set

edx %>% count(movieId) %>% 
  left_join(movie_avgs) %>%
  left_join(movie_titles, by="movieId") %>%
  arrange(b_hat_m) %>% 
  select(title, b_hat_m, n) %>% 
  slice(1:10)

# A tibble: 10 x 3
# title                                       b_hat_i     n
# <chr>                                         <dbl> <int>
# 1 Besotted (2001)                             -3.01     2
# 2 Hi-Line, The (1999)                         -3.01     1
# 3 Accused (Anklaget) (2005)                   -3.01     1
# 4 Confessions of a Superhero (2007)           -3.01     1
# 5 War of the Worlds 2: The Next Wave (2008)   -3.01     2
# 6 SuperBabies: Baby Geniuses 2 (2004)         -2.72    56
# 7 Hip Hop Witch, Da (2000)                    -2.69    14
# 8 Disaster Movie (2008)                       -2.65    32
# 9 From Justin to Kelly (2003)                 -2.61   199
# 10 Criminals (1996)                           -2.51     2

# For the most part, the best and worst movies were rated by very few users
# Less frequently rated movies can cause too much variablilty and increase the RMSE for the model 

# Regulariaztion presents a conservative method to deal with the variability and to control 
# the movie effects


## 5.2 Penalized Least Squares

# Use cross-validation to pick lambda 

# Select a range of lambdas 

lambdas <- seq(0, 10, 0.25)

# Create a loop to calculate rmses for the "Movie + User Effects Model"

rmses <- sapply(lambdas, function(l){
  
  mu_hat <- mean(edx$rating)
  
  b_hat_m <- edx %>% 
    group_by(movieId) %>%
    summarise(b_hat_m = sum(rating - mu_hat)/(n()+l))
  
  b_hat_u <- edx %>% 
    left_join(b_hat_m, by="movieId") %>%
    group_by(userId) %>%
    summarise(b_hat_u = sum(rating - b_hat_m - mu_hat)/(n()+l))
  
  predicted_ratings <- validation %>% 
    left_join(b_hat_m, by = "movieId") %>%
    left_join(b_hat_u, by = "userId") %>%
    mutate(pred = mu_hat + b_hat_m + b_hat_u) %>%
    pull(pred)
  
  return(RMSE(predicted_ratings, validation$rating))
})

# Plot the lambdas against rmses

qplot(lambdas, rmses)  

# Find the optimal lambda

lambda <- lambdas[which.min(rmses)]
lambda

# 5.25

# Check the RMSE after the regularization

rmse_results <- bind_rows(rmse_results,
                          data_frame(method = "Regularized Movie + User Effects Model",  
                                     RMSE = min(rmses)))
rmse_results %>% knitr::kable()

# |method                                     |      RMSE|
# |:------------------------------------------|---------:|
# |Just the Average Model                     | 1.0612018|
# |Movie Effect Model                         | 0.9439087|
# |Movie + User Effects Model                 | 0.8653488|
# |Regularized Movie + User Effects Model     | 0.8648170|*

# The regularized model produced a lower RMSE than the original "Movie and User Effects Model".
# This regularized model is not crowded with predictors and gives a similar RMSE to models with 
# more than two predictors (addintional models are included in section 6), which makes it an
# applealing choice for predicting the movie ratings.


## 5.3 Regularized movie effects estimates 

# Compute the regularized estimated for b_hat_m to compare to the least squared 
# estimates of b_hat_m

movie_reg_avgs <- edx %>% 
  group_by(movieId) %>% 
  summarise(b_hat_m = sum(rating - mu_hat)/(n()+lambda), n_hat_m = n()) 

# Plot the regularized estimates for b_hat_m versus the least squared estimates for b_hat_m
# to see the change in the estimates with regularization 

data_frame(original = movie_avgs$b_hat_m, 
           regularlized = movie_reg_avgs$b_hat_m, 
           n = movie_reg_avgs$n_hat_m) %>%
  ggplot(aes(original, regularlized, size=sqrt(n))) + 
  geom_point(shape=1, alpha=0.5)

# Examine the best movies with regularized estimates for b_hat_m

validation %>%
  count(movieId) %>% 
  left_join(movie_reg_avgs, by = "movieId") %>%
  left_join(movie_titles, by = "movieId") %>%
  arrange(desc(b_hat_m)) %>% 
  select(title, b_hat_m, n) %>% 
  slice(1:10)

# A tibble: 10 x 3
# title                                           b_hat_i     n
# <chr>                                             <dbl> <int>
# 1 Shawshank Redemption, The (1994)                0.942  3111
# 2 Godfather, The (1972)                           0.903  2067
# 3 Usual Suspects, The (1995)                      0.853  2389
# 4 Schindler's List (1993)                         0.851  2584
# 5 Casablanca (1942)                               0.808  1275
# 6 Rear Window (1954)                              0.806   890
# 7 Sunset Blvd. (a.k.a. Sunset Boulevard) (1950)   0.802   333
# 8 Third Man, The (1949)                           0.798   298
# 9 Double Indemnity (1944)                         0.796   249
# 10 Paths of Glory (1957)                          0.794   207

# Examine the worst movies with the regularized estimates for b_hat_m

validation %>%
  count(movieId) %>% 
  left_join(movie_reg_avgs, by = "movieId") %>%
  left_join(movie_titles, by="movieId") %>%
  arrange(b_hat_m) %>% 
  select(title, b_hat_m, n) %>% 
  slice(1:10)

# A tibble: 10 x 3
# title                                                 b_hat_i     n
# <chr>                                                   <dbl> <int>
# 1 From Justin to Kelly (2003)                           -2.54    17
# 2 SuperBabies: Baby Geniuses 2 (2004)                   -2.48     5
# 3 Pokémon Heroes (2003)                                 -2.39    19
# 4 Glitter (2001)                                        -2.30    41
# 5 Gigli (2003)                                          -2.28    39
# 6 Disaster Movie (2008)                                 -2.28     8
# 7 Pokemon 4 Ever (a.k.a. Pokémon 4: The Movie) (2002)   -2.28    31
# 8 Barney's Great Adventure (1998)                       -2.27    26
# 9 Carnosaur 3: Primal Species (1996)                    -2.25    11
# 10 Son of the Mask (2005)                               -2.14    13

# The results improved and the movie ratings for the best and worst movies seem
# more resonable with the penalized estimates for b_hat_m

# Concluding remarks:
# The "Regularized Movie and User Effects Model" is the final model chosen because there are 
# only two predictors in the model and the RMSE achieved is substantially lower (RMSE = 0.86) 
# than in the naive model (naive RMSE = 1.061), which assumed the same average rating for all 
# movies and users. The beauty of this model is that it is simple so it can be easily taken 
# and reproduced for another dataset because it is not too specific to this particular dataset, 
# which makes it more likely to be suitable for other datasets. More predictors were added 
# (as seen in section 6. below) but the RMSE did not improve substantially to allow for addition 
# of those predictors in the model with a good reason for doing so. The more predictors added
# to the model, the more specific the model becomes to this particular dataset and the harder it
# becomes to use it on a different dataset and expect to achieve the same low squared estimates. 

# Output RMSE 

rmse_results %>% knitr::kable()





#############################################################
## 6. Additional models (Appendix)
#############################################################

# These additional models did not make the cut but helped with choosing the final model


## 6.1 "Modeling the movie and user interaction effects"

# Add an interaction term to model 3 ("Movie + User Effects Model") from section 4.2: 
# Y_m,u = μ_hat + b_hat_m + b_hat_u + (b_hat_m * b_hat_u) + ε_m,u

# Check if the prediction improves after the interaction is added to the model

predicted_ratings <- validation %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  mutate(pred = mu_hat + b_hat_m + b_hat_u + (b_hat_m * b_hat_u)) %>%
  .$pred

model_5_rmse <- RMSE(predicted_ratings, validation$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method = "Movie * User Effects Interaction Model",  
                                     RMSE = model_5_rmse))
rmse_results %>% knitr::kable()
# |method                                 |      RMSE|
# |:--------------------------------------|---------:|
# |Just the average                       | 1.0612018|
# |Movie Effect Model                     | 0.9439087|
# |Movie + User Effects Model             | 0.8653488|
# |Movie * User Effects Interaction Model | 0.8978567|*

# The prediction did not improve 


## 6.2 "Modeling the movie, user and genre effects"

# Add genres to the model along with movieId and userId:
# Y_m,u,g = μ_hat + b_hat_m + b_hat_u + b_hat_g + ε_m,u,g

# Use the average of (Y_m − μ_hat - b_hat_m - b_hat_u) for each genre g because the dataset
# has a lot of observations 

genres_avgs <- edx %>%  
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  group_by(genres) %>%
  summarise(b_hat_g = mean(rating - mu_hat - b_hat_m - b_hat_u))

# Check if the prediction improves after adding genres to the model:

predicted_ratings <- validation %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  left_join(genres_avgs, by='genres') %>%
  mutate(pred = mu_hat + b_hat_m + b_hat_u + b_hat_g) %>%
  .$pred

model_6_rmse <- RMSE(predicted_ratings, validation$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method = "Movie + User + Genre Effects Model",  
                                     RMSE = model_6_rmse))
rmse_results %>% knitr::kable()

# |method                                 |      RMSE|
# |:--------------------------------------|---------:|
# |Just the Average Model                 | 1.0612018|
# |Movie Effect Model                     | 0.9439087|
# |Movie + User Effects Model             | 0.8653488|
# |Movie * User Effects Interaction Model | 0.8978567|
# |Movie + User + Genre Effects Model     | 0.8649469|*

# Adding the genres effect to the model improved the RMSE but not enough to justify adding 
# another predictor to the model and making it more complex


## 6.3 "Modeling user and genre effects"

# Create a new average of (Y_m − μ_hat - b_hat_u) for each genre g with only user u in the model:
# Y_u,g = μ_hat + b_hat_u + b_hat_g + ε_u,g

genres_avgs_u <- edx %>%  
  left_join(user_avgs, by='userId') %>%
  group_by(genres) %>%
  summarise(b_hat_g = mean(rating - mu_hat - b_hat_u))

# Check if the prediction improves with only userId and genres in the model:

predicted_ratings <- validation %>% 
  left_join(user_avgs, by='userId') %>%
  left_join(genres_avgs_u, by='genres') %>%
  mutate(pred = mu_hat + b_hat_u + b_hat_g) %>%
  .$pred

model_7_rmse <- RMSE(predicted_ratings, validation$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method = "User + Genre Effects Model", 
                          RMSE = model_7_rmse))
rmse_results %>% knitr::kable()

# |method                                 |      RMSE|
# |:--------------------------------------|---------:|
# |Just the Average Model                 | 1.0612018|
# |Movie Effect Model                     | 0.9439087|
# |Movie + User Effects Model             | 0.8653488|
# |Movie * User Effects Interaction Model | 0.8978567|
# |Movie + User + Genre Effects Model     | 0.8649469|
# |User + Genre Effects Model             | 0.9463636|*

# The prediction did not improve


## 6.4 "Modeling movie and genre interaction effects"

# Add a movie and genres interaction term to model 6 ("Movie + User + Genre Effects Model") above:
# Y_m,u,g = μ_hat + b_hat_m + b_hat_u + b_hat_g + (b_hat_m * b_hat_g) + ε_m,u,g 

# Check if the prediction improves after the interaction is added to the model

predicted_ratings <- validation %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  left_join(genres_avgs, by='genres') %>%
  mutate(pred = mu_hat + b_hat_m + b_hat_u + b_hat_g + (b_hat_m * b_hat_g)) %>%
  .$pred

model_8_rmse <- RMSE(predicted_ratings, validation$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method = "User + Movie * Genre Effects Interaction Model",  
                                     RMSE = model_8_rmse))
rmse_results %>% knitr::kable()

# |method                                         |      RMSE|
# |:----------------------------------------------|---------:|
# |Just the average                               | 1.0612018|
# |Movie Effect Model                             | 0.9439087|
# |Movie + User Effects Model                     | 0.8653488|
# |Movie * User Effects Interaction Model         | 0.8978567|
# |Movie + User + Genre Effects Model             | 0.8649469|
# |User + Genre Effects Model                     | 0.9463636|
# |User + Movie * Genre Effects Interaction Model | 0.8650310|*

# The prediction improved slightly from the prediction in model 3 ("Movie + User Effects Model") 
# from section 4.2 but model 6 ("Movie + User + Genre Effects Model") above improved the 
# prediction more


## 6.5 "Modeling user and genre interaction effects"

# Add a user and genres interaction term to model 6 ("Movie + User + Genre Effects Model") above: 
# Y_m,u,g = μ_hat + b_hat_m + b_hat_u + b_hat_g + (b_hat_u * b_hat_g) + ε_m,u,g

# Check if the prediction improves after the interaction is added to the model

predicted_ratings <- validation %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  left_join(genres_avgs, by='genres') %>%
  mutate(pred = mu_hat + b_hat_m + b_hat_u + b_hat_g + (b_hat_u * b_hat_g)) %>%
  .$pred

model_9_rmse <- RMSE(predicted_ratings, validation$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method = "Movie + User * Genre Effects Interaction Model",  
                                     RMSE = model_9_rmse))
rmse_results %>% knitr::kable()

# |method                                         |      RMSE|
# |:----------------------------------------------|---------:|
# |Just the average                               | 1.0612018|
# |Movie Effect Model                             | 0.9439087|
# |Movie + User Effects Model                     | 0.8653488|
# |Movie * User Effects Interaction Model         | 0.8978567|
# |Movie + User + Genre Effects Model             | 0.8649469|
# |User + Genre Effects Model                     | 0.9463636|
# |User + Movie * Genre Effects Interaction Model | 0.8650310|
# |Movie + User * Genre Effects Interaction Model | 0.8652007|*

# The prediction improved slightly from the prediction in model 3 ("Movie + User Effects Model") 
# from section 4.2 but model 6 ("Movie + User + Genre Effects Model") above improved the 
# prediction more


## 6.6 "Modeling movie, user and title effects" 

# Add title to model model 3 ("Movie + User Effects Model") from section 4.2: 
# Y_m,u,t = μ_hat + b_hat_m + b_hat_u + b_hat_t + ε_m,u,t

# Use the average of (Y_m − μ_hat - b_hat_m - b_hat_t) for each title because the dataset
# has a lot of observations 

title_avgs <- edx %>%  
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  group_by(title) %>%
  summarise(b_hat_t = mean(rating - mu_hat - b_hat_m - b_hat_u))

# Check if prediction improves after the title is added to the model 

predicted_ratings <- validation %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  left_join(title_avgs, by='title') %>%
  mutate(pred = mu_hat + b_hat_m + b_hat_u + b_hat_t) %>%
  .$pred

model_10_rmse <- RMSE(predicted_ratings, validation$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method = "Movie + User + Title Effects Model",  
                                     RMSE = model_10_rmse))
rmse_results %>% knitr::kable()

# |method                                         |      RMSE|
# |:----------------------------------------------|---------:|
# |Just the average                               | 1.0612018|
# |Movie Effect Model                             | 0.9439087|
# |Movie + User Effects Model                     | 0.8653488|
# |Movie * User Effects Interaction Model         | 0.8978567|
# |Movie + User + Genre Effects Model             | 0.8649469|
# |User + Genre Effects Model                     | 0.9463636|
# |User + Movie * Genre Effects Interaction Model | 0.8650310|
# |Movie + User * Genre Effects Interaction Model | 0.8652007|
# |Movie + User + Title Effects Model             | 0.8640972|*

# Adding the title term to the model improved the RMSE but not enough to justify 
# adding another predictor to the model and making it more complex


## 6.7 "Modeling movie and title interaction effects"

# Check if the prediction improves after a movie and title interaction is added to model 10
# ("Movie + User + Title Effects Model") above:
# Y_m,u,t = μ_hat + b_hat_m + b_hat_u + b_hat_t + (b_hat_m * b_hat_t) + ε_m,u,t

predicted_ratings <- validation %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  left_join(title_avgs, by='title') %>%
  mutate(pred = mu_hat + b_hat_m + b_hat_u + b_hat_t + (b_hat_m * b_hat_t)) %>%
  .$pred

model_11_rmse <- RMSE(predicted_ratings, validation$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method = "User + Movie * Title Interaction Effects Model",  
                                     RMSE = model_11_rmse))
rmse_results %>% knitr::kable()

# |method                                         |      RMSE|
# |:----------------------------------------------|---------:|
# |Just the average                               | 1.0612018|
# |Movie Effect Model                             | 0.9439087|
# |Movie + User Effects Model                     | 0.8653488|
# |Movie * User Effects Interaction Model         | 0.8978567|
# |Movie + User + Genre Effects Model             | 0.8649469|
# |User + Genre Effects Model                     | 0.9463636|
# |User + Movie * Genre Effects Interaction Model | 0.8650310|
# |Movie + User * Genre Effects Interaction Model | 0.8652007|
# |Movie + User + Title Effects Model             | 0.8640972|
# |User + Movie * Title Interaction Effects Model | 0.8645226|*

# The prediction improved from the prediction in model 3 ("Movie + User Effects Model")
# in section 4.2, but model 10 ("Movie + User + Title Effects Model") above improved the 
# prediction more


## 6.8 "Modeling user and title interaction effects"

# Check if the prediction improves after a user and title interaction is added to model 10
# ("Movie + User + Title Effects Model") above:
# Y_m,u,t = μ_hat + b_hat_m + b_hat_u + b_hat_t + (b_hat_u * b_hat_t) + ε_m,u,t

predicted_ratings <- validation %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  left_join(title_avgs, by='title') %>%
  mutate(pred = mu_hat + b_hat_m + b_hat_u + b_hat_t + (b_hat_u * b_hat_t)) %>%
  .$pred

model_12_rmse <- RMSE(predicted_ratings, validation$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method = "Movie + User * Title Interaction Effects Model",  
                                     RMSE = model_12_rmse))
rmse_results %>% knitr::kable()

# |method                                         |      RMSE|
# |:----------------------------------------------|---------:|
# |Just the average                               | 1.0612018|
# |Movie Effect Model                             | 0.9439087|
# |Movie + User Effects Model                     | 0.8653488|
# |Movie * User Effects Interaction Model         | 0.8978567|
# |Movie + User + Genre Effects Model             | 0.8649469|
# |User + Genre Effects Model                     | 0.9463636|
# |User + Movie * Genre Effects Interaction Model | 0.8650310|
# |Movie + User * Genre Effects Interaction Model | 0.8652007|
# |Movie + User + Title Effects Model             | 0.8640972|
# |User + Movie * Title Interaction Effects Model | 0.8645226|
# |Movie + User * Title Interaction Effects Model | 0.8647177|*

# # The prediction improved from the prediction in model 3 ("Movie + User Effects Model")
# in section 4.2, but model 10 ("Movie + User + Title Effects Model") above improved the 
# prediction more 


## 6.9 "Modeling movie, user, genre and title effects"

# Check if the prediction improves after genres and title are added to model 3 ("Movie + User 
# Effects Model") in section 4.2:
# Y_m,u,g,t = μ_hat + b_hat_m + b_hat_u + b_hat_g + b_hat_t + ε_m,u,g,t

predicted_ratings <- validation %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  left_join(genres_avgs, by='genres') %>%
  left_join(title_avgs, by='title') %>%
  mutate(pred = mu_hat + b_hat_m + b_hat_u + b_hat_g + b_hat_t) %>%
  .$pred

model_13_rmse <- RMSE(predicted_ratings, validation$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method = "Movie + User + Genre + Title Effects Model",  
                                     RMSE = model_13_rmse))
rmse_results %>% knitr::kable()

# |method                                         |      RMSE|
# |:----------------------------------------------|---------:|
# |Just the average                               | 1.0612018|
# |Movie Effect Model                             | 0.9439087|
# |Movie + User Effects Model                     | 0.8653488|
# |Movie * User Effects Interaction Model         | 0.8978567|
# |Movie + User + Genre Effects Model             | 0.8649469|
# |User + Genre Effects Model                     | 0.9463636|
# |User + Movie * Genre Effects Interaction Model | 0.8650310|
# |Movie + User * Title Interaction Effects Model | 0.8652007|
# |Movie + User + Title Effects Model             | 0.8640972|
# |User + Movie * Title Interaction Effects Model | 0.8645226|
# |Movie + User * Title Interaction Effects Model | 0.8647177|
# |Movie + User + Genre + Title Effects Model     | 0.8643771|*

# The prediction improved from the prediction in model model 3 ("Movie + User Effects Model")
# in section 4.2, but model 10 ("Movie + User + Title Effects Model") above improved the 
# prediction more 


## 6.10 "Modeling genre and title interaction effects"

# Check if the prediction improves after a genres and title interaction term is added to model
# 13 ("Movie + User + Genre + Title Effects Model") above
# Y_m,u,g,t = μ_hat + b_hat_m + b_hat_u + b_hat_g + b_hat_t + (b_hat_g * b_hat_t) + ε_m,u,g,t

predicted_ratings <- validation %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  left_join(genres_avgs, by='genres') %>%
  left_join(title_avgs, by='title') %>%
  mutate(pred = mu_hat + b_hat_m + b_hat_u + b_hat_g + b_hat_t + (b_hat_g * b_hat_t)) %>%
  .$pred

model_14_rmse <- RMSE(predicted_ratings, validation$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method = "Movie + User + Genre * Title Interaction Effects Model",  
                                     RMSE = model_14_rmse))
rmse_results %>% knitr::kable()

# |method                                                |      RMSE|
# |:-----------------------------------------------------|---------:|
# |Just the average                                      | 1.0612018|
# |Movie Effect Model                                    | 0.9439087|
# |Movie + User Effects Model                            | 0.8653488|
# |Movie * User Effects Interaction Model                | 0.8978567|
# |Movie + User + Genre Effects Model                    | 0.8649469|
# |User + Genre Effects Model                            | 0.9463636|
# |User + Movie * Genre Effects Interaction Model        | 0.8650310|
# |Movie + User * Genre Effects Interaction Model        | 0.8652007|
# |Movie + User + Title Effects Model                    | 0.8640972|
# |User + Movie * Title Interaction Effects Model        | 0.8645226|
# |Movie + User * Title Interaction Effects Model        | 0.8647177|
# |Movie + User + Genre + Title Effects Model            | 0.8643771|
# |Movie + User + Genre * Title Interaction Effects Model| 0.8643822|*

# The prediction improved from the prediction in model model 3 ("Movie + User Effects Model")
# in section 4.2, but model 10 ("Movie + User + Title Effects Model") above improved the 
# prediction more 

# These additonal models greatly contributed to the choice of the final model listed in section 5.2 
# above. Although model 10 ("Movie + User + Title Effects Model") from section 6.6 improved the RMSE
# prediction the most it incorporated an additional predictor and it did not improve the RMSE by 
# quite a bit to justify the addition of an extra term to model 3 ("Movie + User Effects Model"). 
# After model 3 was selected it was regularized to prevent overffiting and the final model, model 4,
# ("Regularized Movie + User Effects Model") was chosen.