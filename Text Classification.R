# Libraries------
library(tidyverse)
library(tidytext)
library(quanteda)
library(quanteda.textmodels)
names(emails_1_) <- c("label", "text")

# Check how many ham, how many spam.
#label 0 indicates # of hams and label 1 indicates # of spams
emails_1_ %>% group_by(label) %>% summarise(length(label))

# Create corpus object for quanteda
emails_corpus <-corpus(emails_1_) 

#view corpus
summary(emails_corpus)

# Create dfm matrix for analysis
emailhamspam_dfm <- dfm(emails_corpus, tolower = TRUE)

# Get 75-25 train-test split.  Since 75% of our data
# is 4298, and data randomized, split at 4298 should do it
emailhamspam_dfm_train <- emailhamspam_dfm[1:4298,]
emailhamspam_dfm_test <- emailhamspam_dfm[4298:nrow(emailhamspam_dfm),]

#Splitting original as well for ham/spam labels.
emailhamspam_train <- emails_1_[1:4298,]
emailhamspam_test <- emails_1_[4298:nrow(emails_1_),]

# This checks split 
table(emailhamspam_train$label)

# Time to produce classifier
emailhamspam_classifier <- textmodel_nb(emailhamspam_dfm_train, emailhamspam_train$label)

# Look to see how the classifier does
emailhamspam_pred <- predict(emailhamspam_classifier,newdata = emailhamspam_dfm_test)
#Checking predictions against actual labels.

#98.32% accuracy

