##############################################################################################
## Setting the stage
##############################################################################################

setwd("C:/Users/migalej/OneDrive for Business/OnlineEducation/DataScienceSpecialization/10-Capstone/courseprojects")
library(quanteda); library(reshape2); library(data.table); library(dplyr)



##############################################################################################
## Reading in the data
##############################################################################################

## Read in each of the 3 files that will be used
cnxn.twit <-                    file(description = "./data/final/en_US/en_US.twitter.txt", open = "r")
twitter <-                      readLines(cnxn.twit)
close.connection(cnxn.twit)

cnxn.news <-                    file(description = "./data/final/en_US/en_US.newsEDIT.txt", open = "r")
news <-                         readLines(cnxn.news)
close.connection(cnxn.news)

cnxn.blogs <-                   file(description = "./data/final/en_US/en_US.blogs.txt", open = "r")
blogs <-                        readLines(cnxn.blogs)
close.connection(cnxn.blogs)


##############################################################################################
## Create the sampling indices for the training data and filter
##############################################################################################

##### 50% of the data #####
set.seed(1234)
index.twit <- rbinom(length(twitter), 1, 0.50)
twitter.sample <- twitter[which(index.twit == 1)]

set.seed(5678)
index.news <- rbinom(length(news), 1, 0.50)
news.sample <- news[which(index.news == 1)]

set.seed(9101112)
index.blogs <- rbinom(length(blogs), 1, 0.50)
blogs.sample <- blogs[which(index.blogs == 1)]

# Create the training data set and write to a file for later use (if necessary)
eng.sample <- c(twitter.sample, news.sample, blogs.sample)
write(eng.sample, file = "./data/samples/eng.sample50.txt")


##### 10% of the data #####
set.seed(1234)
index.twit <- rbinom(length(twitter), 1, 0.10)
twitter.sample <- twitter[which(index.twit == 1)]

set.seed(5678)
index.news <- rbinom(length(news), 1, 0.10)
news.sample <- news[which(index.news == 1)]

set.seed(9101112)
index.blogs <- rbinom(length(blogs), 1, 0.10)
blogs.sample <- blogs[which(index.blogs == 1)]

# Create the training data set and write to a file for later use (if necessary)
eng.sample <- c(twitter.sample, news.sample, blogs.sample)
write(eng.sample, file = "./data/samples/eng.sample10.txt")


##############################################################################################
## Create the sampling indices for the training data and filter
##############################################################################################

##### 20% of the remaining observations not in training #####
twitter.test <- twitter[which(index.twit == 0)]
set.seed(1234)
index.twit.test <- rbinom(length(twitter.test), 1, 0.20)
twitter.test <- twitter.test[which(index.twit.test == 1)]

news.test <- news[which(index.news == 0)]
set.seed(5678)
index.news.test <- rbinom(length(news.test), 1, 0.20)
news.test <- news.test[which(index.news.test == 1)]

blogs.test <- blogs[which(index.blogs == 0)]
set.seed(9101112)
index.blogs.test <- rbinom(length(blogs.test), 1, 0.20)
blogs.test <- blogs.test[which(index.blogs.test == 1)]

# Write to a file for later use (if necessary)
write(twitter.test, "./finalproject/benchmarking/tweets20.txt")
write(news.test, "./finalproject/benchmarking/news20.txt")
write(blogs.test, "./finalproject/benchmarking/blogs20.txt")

eng.test <- c(twitter.test, news.test, blogs.test)
write(eng.test, file = "./data/testdata/eng.test.txt")


##### Mocking up a similar test set to the one required in benchmark.R
twitter.test <- twitter[which(index.twit == 0)]
set.seed(1234)
index.twit.test <- rbinom(length(twitter.test), 1, 800/length(twitter.test))
twitter.test <- twitter.test[which(index.twit.test == 1)]

blogs.test <- blogs[which(index.blogs == 0)]
set.seed(9101112)
index.blogs.test <- rbinom(length(blogs.test), 1, 600/length(blogs.test))
blogs.test <- blogs.test[which(index.blogs.test == 1)]

# Write to a file for later use (if necessary)
write(twitter.test, "./finalproject/benchmarking/tweets_v2.txt")
write(blogs.test, "./finalproject/benchmarking/blogs_v2.txt")



##############################################################################################
## Data Cleaning/Prep
##############################################################################################

# If necessary, read in the training data from the first step
##### 10% Sample #####
con <- file(description = "./data/samples/eng.sample10.txt", open = "r")
eng.sample <- readLines(con, encoding = "UTF-8")
close.connection(con)

##### 50% Sample #####
con <- file(description = "./data/samples/eng.sample50.txt", open = "r")
eng.sample <- readLines(con, encoding = "UTF-8")
close.connection(con)


# Clean out all of the junk
eng.sample <- gsub("[^[:alnum:]^[:punct:]^[:space:]]", "", eng.sample)          %>%     # Remove everything that isn't an "ASCII" character
        gsub("'", "")                                                           %>%     # Remove apostrophes
        gsub("_(.*?)_", "")                                                     %>%     # Removes the instances of underscores strung together
        gsub("-(.*?)-", "")                                                             # Removes the instances of dashes strung together

# eng.sample <- gsub("'",
#                    "",
#                    eng.sample
#                    )                          # Remove apostrophes
# eng.sample <- gsub("_(.*?)_",
#                    "",
#                    eng.sample
#                    )                          # Removes the instances of underscores strung together
# eng.sample <- gsub("-(.*?)-",
#                    "",
#                    eng.sample
#                    )                          # Removes the instances of dashes strung together


##############################################################################################
## Create Document-Feature Matrices for 4-grams, 3-grams, 2-grams, 1-grams
##############################################################################################

# Unigrams
dfm.1 <- dfm(eng.sample,  toLower = TRUE, removePunct = TRUE, removeNumbers = TRUE,
             removeSeparators = TRUE, removeTwitter = TRUE, ngrams = 1
             )
dfm.1.df <- data.table(as.matrix(colSums(dfm.1)))               # Create a data frame using `data.table`
dfm.1.df$word <- rownames(as.matrix(colSums(dfm.1)))            # Bring in the words as a field
colnames(dfm.1.df) <- c("count", "uni.1")                       # Rename the columns
dfm.1.df$Prob1 <- round(dfm.1.df$count / sum(dfm.1.df$count),
                        5
                        )                                       # Calculate the MLE
############################################

# Bigrams
dfm.2 <- dfm(eng.sample,  toLower = TRUE, removePunct = TRUE, removeNumbers = TRUE,
             removeSeparators = TRUE, removeTwitter = TRUE, ngrams = 2
             )
dfm.2.df <- data.table(as.matrix(colSums(dfm.2)))               # Create a data frame using `data.table`
dfm.2.df$word <- rownames(as.matrix(colSums(dfm.2)))            # Bring in the words as a field
colnames(dfm.2.df) <- c("count", "bigram")                      # Rename the columns

############################################

# Trigrams
dfm.3 <- dfm(eng.sample,  toLower = TRUE, removePunct = TRUE, removeNumbers = TRUE,
             removeSeparators = TRUE, removeTwitter = TRUE, ngrams = 3
             )
dfm.3.df <- data.table(as.matrix(colSums(dfm.3)))               # Create a data frame using `data.table`
dfm.3.df$word <- rownames(as.matrix(colSums(dfm.3)))            # Bring in the words as a field
colnames(dfm.3.df) <- c("count", "trigram")                     # Rename the columns

############################################

# Quadgrams
dfm.4 <- dfm(eng.sample,  toLower = TRUE, removePunct = TRUE, removeNumbers = TRUE,
             removeSeparators = TRUE, removeTwitter = TRUE, ngrams = 4
             )
dfm.4.df <- data.table(as.matrix(colSums(dfm.4)))               # Create a data frame using `data.table`
dfm.4.df$word <- rownames(as.matrix(colSums(dfm.4)))            # Bring in the words as a field
colnames(dfm.4.df) <- c("count", "quadgram")                    # Rename the columns

############################################

# Optional: write the newly created data frames to .csv files for future use
##### 10% Sample #####
write.csv(dfm.1.df, file = "./data/modeldata/dfm.1.df_10sample.csv")
write.csv(dfm.2.df, file = "./data/modeldata/dfm.2.df_10sample.csv")
write.csv(dfm.3.df, file = "./data/modeldata/dfm.3.df_10sample.csv")
write.csv(dfm.4.df, file = "./data/modeldata/dfm.4.df_10sample.csv")

##### 50% Sample #####
write.csv(dfm.1.df, file = "./data/modeldata/dfm.1.df_50sample.csv")
write.csv(dfm.2.df, file = "./data/modeldata/dfm.2.df_50sample.csv")
write.csv(dfm.3.df, file = "./data/modeldata/dfm.3.df_50sample.csv")
write.csv(dfm.4.df, file = "./data/modeldata/dfm.4.df_50sample.csv")


##############################################################################################
## Manipulate the DFMs to calculate Maximum Likelihood Estimates for 4-grams, 3-grams, 2-grams
##############################################################################################

# Optional: Read in the data frames
##### 10% Sample #####
dfm.1.df <- fread("./data/modeldata/dfm.1.df_10sample.csv")
dfm.2.df <- fread("./data/modeldata/dfm.2.df_10sample.csv")
dfm.3.df <- fread("./data/modeldata/dfm.3.df_10sample.csv")
dfm.4.df <- fread("./data/modeldata/dfm.4.df_10sample.csv")

##### 50% Sample #####
dfm.1.df <- fread("./data/modeldata/dfm.1.df_50sample.csv")
dfm.2.df <- fread("./data/modeldata/dfm.2.df_50sample.csv")
dfm.3.df <- fread("./data/modeldata/dfm.3.df_50sample.csv")
dfm.4.df <- fread("./data/modeldata/dfm.4.df_50sample.csv")

# Define the threshold 
threshold <- 3

# Prune low-frequency n-grams
dfm.4.df <- dfm.4.df[count > threshold]

# Split the n-gram into its components
dfm.4.df <- transform(dfm.4.df, 
                      uni = colsplit(dfm.4.df$quadgram, 
                                     pattern = "_", 
                                     names = c("1", "2", "3", "4")
                                     )
                      )

# Create the "prefix"
dfm.4.df$trigram <- paste(dfm.4.df$uni.1, 
                          dfm.4.df$uni.2,
                          dfm.4.df$uni.3, 
                          sep = "_"
                          )

# Set the keys for joining n-grams with (n-1)-gram frequencies
setkey(dfm.4.df, trigram); setkey(dfm.3.df, trigram)

# Join the n-gram table with the (n-1)-gram table
dfm.4.df <- merge(dfm.4.df, dfm.3.df, all.x = TRUE)

# Clean up the field names
dfm.4.df <- dfm.4.df[, .(quadgram.count = count.x, quadgram = quadgram,
                         trigram.count = count.y, trigram = trigram,
                         uni.1 = uni.1, uni.2 = uni.2, uni.3 = uni.3, uni.4 = uni.4
                         )
                     ]

# Calculate the MLE for the n-gram and sort the table by the MLE
dfm.4.df$Prob4Given123 <- dfm.4.df$quadgram.count/dfm.4.df$trigram.count
dfm.4.df <- dfm.4.df[order(-rank(quadgram.count), -rank(Prob4Given123))]

############################################

# Prune low-frequency n-grams
dfm.3.df <- dfm.3.df[count > threshold]

# Split the n-gram into its components
dfm.3.df <- transform(dfm.3.df,
                      uni = colsplit(dfm.3.df$trigram, 
                                     pattern = "_", 
                                     names = c("1", "2", "3")
                                     )
                      )

# Create the "prefix"
dfm.3.df$bigram <- paste(dfm.3.df$uni.1, 
                         dfm.3.df$uni.2, 
                         sep = "_"
                         )

# Set the keys for joining n-grams with (n-1)-gram frequencies
setkey(dfm.3.df, bigram); setkey(dfm.2.df, bigram)

# Join the n-gram table with the (n-1)-gram table
dfm.3.df <- merge(dfm.3.df, dfm.2.df, all.x = TRUE)

# Clean up the field names
dfm.3.df <- dfm.3.df[, .(trigram.count = count.x, trigram = trigram,
                         bigram.count = count.y, bigram = bigram, 
                         uni.1 = uni.1, uni.2 = uni.2, uni.3 = uni.3
                         )
                     ]

# Calculate the MLE for the n-gram and sort the table by the MLE
dfm.3.df$Prob3Given12 <- dfm.3.df$trigram.count/dfm.3.df$bigram.count
dfm.3.df <- dfm.3.df[order(-rank(trigram.count), -rank(Prob3Given12))]

############################################

# Prune low-frequency n-grams
dfm.2.df <- dfm.2.df[count > threshold]

# Split the n-gram into its components
dfm.2.df <- transform(dfm.2.df,
                      uni = colsplit(dfm.2.df$bigram, 
                                     pattern = "_", 
                                     names = c("1", "2")
                                     )
                      )

# Set the keys for joining n-grams with (n-1)-gram frequencies
setkey(dfm.2.df, uni.1); setkey(dfm.1.df, uni.1)

# Join the n-gram table with the (n-1)-gram table
dfm.2.df <- merge(dfm.2.df, dfm.1.df, all.x = TRUE)

# Clean up the field names
dfm.2.df <- dfm.2.df[, .(bigram.count = count.x, bigram = bigram, 
                         uni.1 = uni.1, uni.1.count=count.y, 
                         uni.2 = uni.2
                         )
                     ]

# Calculate the MLE for the n-gram and sort the table by the MLE
dfm.2.df$Prob2Given1 <- dfm.2.df$bigram.count/dfm.2.df$uni.1.count
dfm.2.df <- dfm.2.df[order(-rank(bigram.count), -rank(Prob2Given1))]

############################################

# Optional: write the newly created data frames to .csv files for future use
##### 10% Sample #####
write.csv(dfm.1.df, file = "./data/modeldata/dfm.1.dfprobs_10sample.csv")
write.csv(dfm.2.df, file = "./data/modeldata/dfm.2.dfprobs_10sample.csv")
write.csv(dfm.3.df, file = "./data/modeldata/dfm.3.dfprobs_10sample.csv")
write.csv(dfm.4.df, file = "./data/modeldata/dfm.4.dfprobs_10sample.csv")

##### 50% Sample #####
write.csv(dfm.1.df, file = "./data/modeldata/dfm.1.dfprobs_50sample.csv")
write.csv(dfm.2.df, file = "./data/modeldata/dfm.2.dfprobs_50sample.csv")
write.csv(dfm.3.df, file = "./data/modeldata/dfm.3.dfprobs_50sample.csv")
write.csv(dfm.4.df, file = "./data/modeldata/dfm.4.dfprobs_50sample.csv")


##############################################################################################
## Create a function to implement a Stupid Backoff word prediction algorithm
##############################################################################################

# If starting clean, code to read in the manipulated DFMs that were previously saved to file
##### 10% Sample #####
dfm.4.df <- fread("./data/modeldata/dfm.4.dfprobs_10sample.csv")
dfm.3.df <- fread("./data/modeldata/dfm.3.dfprobs_10sample.csv")
dfm.2.df <- fread("./data/modeldata/dfm.2.dfprobs_10sample.csv")
dfm.1.df <- fread("./data/modeldata/dfm.1.dfprobs_10sample.csv")

##### 50% Sample #####
dfm.4.df <- fread("./data/modeldata/dfm.4.dfprobs_50sample.csv")
dfm.3.df <- fread("./data/modeldata/dfm.3.dfprobs_50sample.csv")
dfm.2.df <- fread("./data/modeldata/dfm.2.dfprobs_50sample.csv")
dfm.1.df <- fread("./data/modeldata/dfm.1.dfprobs_50sample.csv")

############################################
# Create the function 

score <- function(word1 = NULL, word2 = NULL, word3 = NULL, results = 5){
        ##Declare alpha for implementation of stupid backoff
        alpha <- 0.4
        
        ##If they enter all 3 words
        if(is.character(word1) == TRUE && 
           is.character(word2) == TRUE &&
           is.character(word3) == TRUE
           )
                {
                ##Filter the 4-gram data table to only keep the relevant data and then sort
                quad <- dfm.4.df[trigram == as.character(paste(word1,
                                                               word2,
                                                               word3,
                                                               sep = "_")
                                                         )
                                 ]
                quad <- quad[order(-rank(Prob4Given123))]
                
                ##For implementation of Stupid Backoff, create the lower-order data table
                ##Filter the 3-gram data table to only keep the relevant data and then sort
                tri <- dfm.3.df[bigram == as.character(paste(word2,
                                                             word3,
                                                             sep = "_")
                                                       )
                                ]
                tri <- tri[order(-rank(Prob3Given12))]
                
                ##For implementation of Stupid Backoff, create the lower-order data table
                ##Filter the 2-gram data table to only keep the relevant data and then sort
                bi <- dfm.2.df[uni.1 == as.character(word3)]
                bi <- bi[order(-rank(Prob2Given1))]
                
                ##For implementation of Stupid Backoff, create the lower-order data table
                ##Sort the 1-gram data table
                uni <- dfm.1.df[order(-rank(Prob1))]
                
                ##Create the prediction table by appending the top 5 results from each of the data tables
                pred.table <- rbind(data.table(input = quad$trigram[1:results], 
                                               source = "a.quadgram",
                                               prediction = quad$uni.4[1:results], 
                                               #Multiply the 'score' field by -1 so setkey() will sort how I want
                                               score = round(quad$Prob4Given123[1:results], 5) *-1
                                               ),
                                    data.table(input = tri$bigram[1:results],
                                               source = "b.trigram",
                                               prediction = tri$uni.3[1:results], 
                                               #Multiply the 'score' field by -1 so setkey() will sort how I want
                                               score = round(tri$Prob3Given12[1:results] * alpha, 5) * -1
                                               ),
                                    data.table(input = bi$uni.1[1:results],
                                               source = "c.bigram",
                                               prediction = bi$uni.2[1:results],
                                               #Multiply the 'score' field by -1 so setkey() will sort how I want
                                               score = round(bi$Prob2Given1[1:results] * (alpha^2), 5) * -1
                                               ),
                                    data.table(input = "None",
                                               source = "d.unigram",
                                               prediction = uni$uni.1[1:results],
                                               #Multiply the 'score' field by -1 so setkey() will sort how I want
                                               score = round(uni$Prob1[1:results] * (alpha^3), 5) * -1
                                               )
                                    )
                
                ##Remove NA values that have been pulled in through subsetting rows [1:results]
                pred.table <- pred.table[is.na(score) == FALSE]
                
                ##Setting the key so the unique values kept are the highest score
                setkey(pred.table, "prediction")
                pred.table <- unique(pred.table)
                
                ##Undoing the multiplying by -1 from above
                pred.table$score <- pred.table$score * -1
                
                ##Report the top options specified through the results argument
                pred.table[order(rank(source), -rank(score))][1:results]
                
                ############################################
                
                ##If they only enter 2 words
        } else if (is.character(word1) == TRUE &&
                   is.character(word2) == TRUE &&
                   is.null(word3) == TRUE
                   )
                {
                ##Filter the 3-gram data table to only keep the relevant data and then sort
                tri <- dfm.3.df[bigram == as.character(paste(word1, 
                                                             word2, 
                                                             sep = "_")
                                                       )
                                ]
                tri <- tri[order(-rank(Prob3Given12))]
                
                ##For implementation of Stupid Backoff, create the lower-order data table
                ##Filter the 2-gram data table to only keep the relevant data and then sort
                bi <- dfm.2.df[uni.1 == as.character(word2)]
                bi <- bi[order(-rank(Prob2Given1))]
                
                ##For implementation of Stupid Backoff, create the lower-order data table
                ##Sort the 1-gram data table
                uni <- dfm.1.df[order(-rank(Prob1))]
                
                ##Create the prediction table by appending the top 5 results from each of the data tables
                pred.table <- rbind(data.table(input = tri$bigram[1:results],
                                               source = "a.trigram",
                                               prediction = tri$uni.3[1:results], 
                                               #Multiply the 'score' field by -1 so setkey() will sort how I want
                                               score = round(tri$Prob3Given12[1:results], 5) * -1
                                               ),
                                    data.table(input = bi$uni.1[1:results],
                                               source = "b.bigram",
                                               prediction = bi$uni.2[1:results],
                                               #Multiply the 'score' field by -1 so setkey() will sort how I want
                                               score = round(bi$Prob2Given1[1:results] * alpha, 5) * -1
                                               ),
                                    data.table(input = "None",
                                               source = "c.unigram",
                                               prediction = uni$uni.1[1:results],
                                               #Multiply the 'score' field by -1 so setkey() will sort how I want
                                               score = round(uni$Prob1[1:results] * (alpha^2), 5) * -1
                                               )
                                    )
                
                ##Remove NA values that have been pulled in through subsetting rows [1:results]
                pred.table <- pred.table[is.na(score) == FALSE]
                
                ##Setting the key so the unique values kept are the highest score
                setkey(pred.table, "prediction")
                pred.table <- unique(pred.table)
                
                ##Undoing the multiplying by -1 from above
                pred.table$score <- pred.table$score * -1
                
                ##Report the top options specified through the results argument
                pred.table[order(rank(source), -rank(score))][1:results]
                
                ############################################
                
                ##If they only enter 1 words
                
        } else if (is.character(word1) == TRUE &&
                   is.null(word2) == TRUE &&
                   is.null(word3) == TRUE
                   )
                {
                
                ##Filter the 2-gram data table to only keep the relevant data and then sort
                bi <- dfm.2.df[uni.1 == as.character(word1)]
                bi <- bi[order(-rank(Prob2Given1))]
                
                ##For implementation of Stupid Backoff, create the lower-order data table
                ##Sort the 1-gram data table
                uni <- dfm.1.df[order(-rank(Prob1))]
                
                ##Create the prediction table by appending the top 5 results from each of the data tables
                pred.table <- rbind(data.table(input = bi$uni.1[1:results],
                                               source = "a.bigram",
                                               prediction = bi$uni.2[1:results],
                                               #Multiply the 'score' field by -1 so setkey() will sort how I want
                                               score = round(bi$Prob2Given1[1:results], 5) * -1
                                               ),
                                    data.table(input = "None",
                                               source = "b.unigram",
                                               prediction = uni$uni.1[1:results],
                                               #Multiply the 'score' field by -1 so setkey() will sort how I want
                                               score = round(uni$Prob1[1:results] * alpha, 5) * -1
                                               )
                                    )
                
                ##Remove NA values that have been pulled in through subsetting rows [1:results]
                pred.table <- pred.table[is.na(score) == FALSE]
                
                ##Setting the key so the unique values kept are the highest score
                setkey(pred.table, "prediction")
                pred.table <- unique(pred.table)
                
                ##Undoing the multiplying by -1 from above
                pred.table$score <- pred.table$score * -1
                
                ##Report the top options specified through the results argument
                pred.table[order(rank(source), -rank(score))][1:results]
                
                ############################################
                
                ##If they only enter no words
        } else {print("Enter a word")}
}

##############################################################################################
## For benchmarking, clean the test data similarly to how the training data was cleaned
##############################################################################################

blogs <- gsub("[^[:alnum:]^[:punct:]^[:space:]]",
              "",
              blogs
              )                          # Remove everything that isn't an "ASCII" character
blogs <- gsub("'",
              "",
              blogs
              )                          # Remove apostrophes
blogs <- gsub("_(.*?)_",
              "",
              blogs
              )                          # Removes the instances of underscores strung together
blogs <- gsub("-(.*?)-",
              "",
              blogs
              )                          # Removes the instances of dashes strung together

############################################

tweets <- gsub("[^[:alnum:]^[:punct:]^[:space:]]",
               "",
               tweets
               )                          # Remove everything that isn't an "ASCII" character
tweets <- gsub("'",
               "",
               tweets
               )                          # Remove apostrophes
tweets <- gsub("_(.*?)_",
               "",
               tweets
               )                          # Removes the instances of underscores strung together
tweets <- gsub("-(.*?)-",
               "",
               tweets
               )                          # Removes the instances of dashes strung together


##############################################################################################
## Other testing
##############################################################################################

##Read in the previously created test data
con <- file(description = "./data/testdata/eng.test.txt", open = "r")
eng.test <- readLines(con, encoding = "UTF-8")
close.connection(con)

##Clean out all of the junk
eng.test <- gsub("[^[:alnum:]^[:punct:]^[:space:]]",
                 "",
                 eng.test
                 )                          # Remove everything that isn't an "ASCII" character
eng.test <- gsub("'",
                 "",
                 eng.test
                 )                          # Remove apostrophes
eng.test <- gsub("_(.*?)_",
                 "",
                 eng.test
                 )                          # Removes the instances of underscores strung together

########################################################################################

dfmtest.1 <- dfm(eng.test, toLower = TRUE, removePunct = TRUE, removeNumbers = TRUE,
                 removeSeparators = TRUE, removeTwitter = TRUE, ngrams = 1
                 )
dfmtest.1.df <- data.table(as.matrix(colSums(dfmtest.1)))               # Create a data frame using `data.table`
dfmtest.1.df$word <- rownames(as.matrix(colSums(dfmtest.1)))            # Bring in the words as a field
colnames(dfmtest.1.df) <- c("count", "uni.1")                           # Rename the columns
dfmtest.1.df$Prob1 <- round(dfmtest.1.df$count / sum(dfmtest.1.df$count), 
                            5
                            )                                           # Calculate the MLE
############################################

dfmtest.2 <- dfm(eng.test,  toLower = TRUE, removePunct = TRUE, removeNumbers = TRUE,
                 removeSeparators = TRUE, removeTwitter = TRUE, ngrams = 2
                 )
dfmtest.2.df <- data.table(as.matrix(colSums(dfmtest.2)))               # Create a data frame using `data.table`
dfmtest.2.df$word <- rownames(as.matrix(colSums(dfmtest.2)))            # Bring in the words as a field
colnames(dfmtest.2.df) <- c("count", "bigram")                          # Rename the columns
dfmtest.2.df <- dfmtest.2.df[count > 1]                                 # Keep only n-grams with count greater than 1

############################################

dfmtest.3 <- dfm(eng.test,  toLower = TRUE, removePunct = TRUE, removeNumbers = TRUE,
                 removeSeparators = TRUE, removeTwitter = TRUE, ngrams = 3
                 )
dfmtest.3.df <- data.table(as.matrix(colSums(dfmtest.3)))               # Create a data frame using `data.table`
dfmtest.3.df$word <- rownames(as.matrix(colSums(dfmtest.3)))            # Bring in the words as a field
colnames(dfmtest.3.df) <- c("count", "trigram")                         # Rename the columns
dfmtest.3.df <- dfmtest.3.df[count > 1]                                 # Keep only n-grams with count greater than 1

############################################

dfmtest.4 <- dfm(eng.test,  toLower = TRUE, removePunct = TRUE, removeNumbers = TRUE,
                 removeSeparators = TRUE, removeTwitter = TRUE, ngrams = 4
                 )
dfmtest.4.df <- data.table(as.matrix(colSums(dfmtest.4)))               # Create a data frame using `data.table`
dfmtest.4.df$word <- rownames(as.matrix(colSums(dfmtest.4)))            # Bring in the words as a field
colnames(dfmtest.4.df) <- c("count", "quadgram")                        # Rename the columns
dfmtest.4.df <- dfmtest.4.df[count > 1]                                 # Keep only n-grams with count greater than 1

########################################################################################

##Manipulate the DFMs to calculate Maximum Likelihood Estimates for 4-grams, 3-grams, 2-grams

#Split the n-gram into its components
dfmtest.4.df <- transform(dfmtest.4.df, 
                          uni = colsplit(dfmtest.4.df$quadgram, 
                                         pattern = "_", 
                                         names = c("1", "2", "3", "4")
                                         )
                          )

#Create the "prefix"
dfmtest.4.df$trigram <- paste(dfmtest.4.df$uni.1, 
                              dfmtest.4.df$uni.2,
                              dfmtest.4.df$uni.3, 
                              sep = "_"
                              )

#Set the keys for joining n-grams with (n-1)-gram frequencies
setkey(dfmtest.4.df, trigram); setkey(dfmtest.3.df, trigram)

#Join the n-gram table with the (n-1)-gram table
dfmtest.4.df <- merge(dfmtest.4.df, dfmtest.3.df, all.x = TRUE)

#Clean up the field names
dfmtest.4.df <- dfmtest.4.df[, .(quadgram.count = count.x, quadgram = quadgram,
                                 trigram.count = count.y, trigram = trigram,
                                 uni.1 = uni.1, uni.2 = uni.2, uni.3 = uni.3, uni.4 = uni.4
                                 )
                             ]

#Calculate the MLE for the n-gram and sort the table by the MLE
dfmtest.4.df$Prob4Given123 <- dfmtest.4.df$quadgram.count/dfmtest.4.df$trigram.count
dfmtest.4.df <- dfmtest.4.df[order(-rank(quadgram.count), -rank(Prob4Given123))]

############################################

#Split the n-gram into its components
dfmtest.3.df <- transform(dfmtest.3.df,
                          uni = colsplit(dfmtest.3.df$trigram, 
                                         pattern = "_", 
                                         names = c("1", "2", "3")
                                         )
                          )

#Create the "prefix"
dfmtest.3.df$bigram <- paste(dfmtest.3.df$uni.1, 
                             dfmtest.3.df$uni.2, 
                             sep = "_"
                             )

#Set the keys for joining n-grams with (n-1)-gram frequencies
setkey(dfmtest.3.df, bigram); setkey(dfmtest.2.df, bigram)

#Join the n-gram table with the (n-1)-gram table
dfmtest.3.df <- merge(dfmtest.3.df, dfmtest.2.df, all.x = TRUE)

#Clean up the field names
dfmtest.3.df <- dfmtest.3.df[, .(trigram.count = count.x, trigram = trigram,
                                 bigram.count = count.y, bigram = bigram, 
                                 uni.1 = uni.1, uni.2 = uni.2, uni.3 = uni.3
                                 )
                             ]

#Calculate the MLE for the n-gram and sort the table by the MLE
dfmtest.3.df$Prob3Given12 <- dfmtest.3.df$trigram.count/dfmtest.3.df$bigram.count
dfmtest.3.df <- dfmtest.3.df[order(-rank(trigram.count), -rank(Prob3Given12))]

############################################

#Split the n-gram into its components
dfmtest.2.df <- transform(dfmtest.2.df,
                          uni = colsplit(dfmtest.2.df$bigram, 
                                         pattern = "_", 
                                         names = c("1", "2")
                                         )
                          )

#Set the keys for joining n-grams with (n-1)-gram frequencies
setkey(dfmtest.2.df, uni.1); setkey(dfmtest.1.df, uni.1)

#Join the n-gram table with the (n-1)-gram table
dfmtest.2.df <- merge(dfmtest.2.df, dfmtest.1.df, all.x = TRUE)

#Clean up the field names
dfmtest.2.df <- dfmtest.2.df[, .(bigram.count = count.x, bigram = bigram, 
                                 uni.1 = uni.1, uni.1.count=count.y, 
                                 uni.2 = uni.2
                                 )
                             ]

#Calculate the MLE for the n-gram and sort the table by the MLE
dfmtest.2.df$Prob2Given1 <- dfmtest.2.df$bigram.count/dfmtest.2.df$uni.1.count
dfmtest.2.df <- dfmtest.2.df[order(-rank(bigram.count), -rank(Prob2Given1))]

########################################################################################

##Optional: write the data frames to .csv files for future use
write.csv(dfmtest.4.df, file = "./data/testdata/dfmtest.4.dfprobs.csv")
write.csv(dfmtest.3.df, file = "./data/testdata/dfmtest.3.dfprobs.csv")
write.csv(dfmtest.2.df, file = "./data/testdata/dfmtest.2.dfprobs.csv")