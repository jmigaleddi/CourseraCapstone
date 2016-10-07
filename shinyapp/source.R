library(quanteda); library(reshape2); library(data.table)

dfm.4.df <- fread("modeldata/dfm.4.dfprobs_10sample.csv")
dfm.3.df <- fread("modeldata/dfm.3.dfprobs_10sample.csv")
dfm.2.df <- fread("modeldata/dfm.2.dfprobs_10sample.csv")
dfm.1.df <- fread("modeldata/dfm.1.dfprobs_10sample.csv")

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
                #pred.table[order(rank(source), -rank(score))][1:results]
                pred.table[order(-rank(score))][1:results]
                
                ############################################
                
                ##If they only enter 2 words
        } else if ((is.character(word1) == TRUE &&
                    is.character(word2) == TRUE &&
                    is.null(word3) == TRUE
                    ) ||
                   (is.character(word1) == TRUE &&
                    is.null(word2) == TRUE &&
                    is.character(word3) == TRUE
                    ) ||
                   (is.null(word1) == TRUE &&
                    is.character(word2) == TRUE &&
                    is.character(word3) == TRUE
                    )
                   )
                {
                ##Filter the 3-gram data table to only keep the relevant data and then sort
                tri <- dfm.3.df[bigram == paste(c(word1, word2, word3)[1],
                                                c(word1, word2, word3)[2],
                                                sep = "_"
                                                )
                                ]
                tri <- tri[order(-rank(Prob3Given12))]
                
                ##For implementation of Stupid Backoff, create the lower-order data table
                ##Filter the 2-gram data table to only keep the relevant data and then sort
                bi <- dfm.2.df[uni.1 == paste(c(word1, word2, word3)[2])]
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
                #pred.table[order(rank(source), -rank(score))][1:results]
                pred.table[order(-rank(score))][1:results]
                
                ############################################
                
                ##If they only enter 1 words
                
        } else if ((is.character(word1) == TRUE &&
                    is.null(word2) == TRUE &&
                    is.null(word3) == TRUE
                    ) ||
                   (is.null(word1) == TRUE &&
                    is.character(word2) == TRUE &&
                    is.null(word3) == TRUE
                    ) ||
                   (is.null(word1) == TRUE &&
                    is.null(word2) == TRUE &&
                    is.character(word3) == TRUE
                    )
                   )
                {
                
                ##Filter the 2-gram data table to only keep the relevant data and then sort
                bi <- dfm.2.df[uni.1 == paste(c(word1, word2, word3)[1])]
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
                #pred.table[order(rank(source), -rank(score))][1:results]
                pred.table[order(-rank(score))][1:results]
                
                ############################################
                
                ##If they only enter no words
        } else {print("Enter a word")}
}