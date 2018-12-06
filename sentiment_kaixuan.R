library(ggplot2)
#### read in files
pos <- read.csv("sentiment score_pos.csv")
neg <- read.csv("sentiment score_neg.csv")
##### assign headers
names(pos) <- c("score","counter")
names(neg) <- c("score","counter")
#### histogram for scores
qplot(pos$score, geom="histogram")
qplot(neg$score, geom="histogram")
#### do the percentage calculation
score_pos <- sum(pos$score)
score_neg <- sum(neg$score)
total <- abs(score_neg) + score_pos
pos_percentage <- score_pos/total
neg_percentage <- abs(score_neg)/total
pos_percentage
neg_percentage
