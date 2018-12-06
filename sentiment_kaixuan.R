library(ggplot2)
pos <- read.csv("sentiment score_pos.csv")
neg <- read.csv("sentiment score_neg.csv")
qplot(pos$score, geom="histogram")
qplot(neg$score, geom="histogram")
names(pos) <- c("score","counter")
names(neg) <- c("score","counter")
score_pos <- sum(pos$score)
score_neg <- sum(neg$score)
total <- abs(score_neg) + score_pos
pos_percentage <- score_pos/total
neg_percentage <- abs(score_neg)/total
pos_percentage
neg_percentage
