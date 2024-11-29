################################################################################
# Lab #2 answer key
#
# This assignment we will focus on applying what we have learned on the Data4Good 
# case challenge. Bookmark these two links so you can easily find them later.
# 
# Competition: https://www.kaggle.com/competitions/the-purdue-data-4-good-case-competition-2024/overview
# Data Dictionary: https://docs.google.com/document/d/125pj9rUnRPTFCeq8MMrg1SSIeadfOwSrllFsWGDJ8HY/edit#heading=h.cghzriyx7fje
################################################################################
# If you have not watched and submitted your quiz scores for the first two
# training sessions, you're behind. If you have, I already have your scores and
# you do not need to submit again.
# I will pull those scores for each person for your points for questions 1 and 2
# below, so it's possible your Lab #2 scores will be different for each person
# on your team. Training videos are on website (https://bit.ly/2024data4good).
#
# Q1 (1 point): Kickoff Quiz submitted (https://bit.ly/data4goodkickoff)
# Q2 (1 point): RAG Quiz submitted (https://bit.ly/data4goodRAG)
#
################################################################################
# Business Problem Framing
# Q3 (6 points):
# Read through the Kaggle competition Overview, Data, and Rules sections on the
# Kaggle website. Also, you can find additional details about the problem from
# the Kickoff call from Dr. Chantel Dooley (https://bit.ly/2024data4good). Next,
# read through the INFORMS CAP JTA 7 Domains task (slide 18 in Data Mining
# Overview slides). For the six tasks listed provide a thoughtful but concise 
# response for each task. Usually, at the beginning on your project as you go 
# through your process you may not have a fully developed idea/answer. However,
# your goal is to continue to ask the right questions and seek out information
# to ensure you're working on the right problem, for the right people, in the
# right way and time.
#
# T1: TAPS wants a system to better identify what stage of the grief journey their survivors are on. The model should accurately match survivors to their corresponding step in the grief journey.
# T2: TAPS wants to better help their survivor network and military family survivors want the most appropriate care that they can receive
# T3: This problem is amenable to an analytics solution as we can use text and data mining to try and predict what stage of the grief journey the survivor is on based off of their survey results
# T4: Accurately map what part of the grief process survivors are on based off of their survey results.
# T5: This model will drastically cut down on evaluation time and allow TAPS to more efficiently connect survivors to the care that is most appropriate for them.
# T6: CAPS agrees that this framing is correct as they have come to us with this problem.
#
################################################################################
# Analytics Problem Framing
# Q4 (4 points):
# Review the five Analytics Problem Framing tasks and provide concise answers
# to T1-T4.
#
# T1: Use survey results (text and numbers) to accurately predict what step of the grief journey that survivors are on, maximizing F1.
# T2: Words like miserable, awful, depressed, terrible may have a higher frequency when it comes to someone in the grief stage. Furthermore, lower values on survey results may correlate to an earilier stage in the grief journey.
# T3: We are assuming that those surveyed are being honest and sharing their true feelings
# T4: A high level of accuracy when it comes to our predictions will mean success, in this case we are trying to maximize our F1.
#
################################################################################
# Data
# Q5:
# Review the six Data tasks in the table. Next we will begin to work on some of 
# those tasks.
################################################################################
# Data understanding and cleanup
#
# Q6 (1 point): 
# Read in the examples.csv kaggle dataset using the read.table() function.
# It should be in R as a data.frame called 'd'.
setwd("/Users/zhangtianyi/Desktop/200/code/kaggle")
d <- read.table("examples.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)


# Q7 (1 point): 
# Save the test.csv file from Kaggle as an Excel .xlsx spreadsheet. Read in
# Excel file using the read_xlsx() function. It should be in R as a data.frame 
# called 'test'.
library(readxl)
test <- read_xlsx("test.xlsx")

# Q8 (1 point): Review the structure of the loaded datasets using str() and names()
str(d)
names(d)
str(test)
names(test)
  
  
################################################################################
# Most of our potential input (X) variables are Likert-scale type responses that
# have an order to them (thus ordinal) but not all. Read through the survey 
# questions (link above in script). We see 
# > questions 1 and 2 have no inherit order (nominal)
# > questions 3-6 are:
#      0. Disagree
#      1. Neither agree nor disagree
#      2. Agree 
# > questions 7-8 are:
#      0. Extremely likely / Very likely
#      1. Somewhat likely 
#      2. Neither likely nor unlikely / Neutral
#      3. Somewhat unlikely
#      4. Extremely unlikely / Not at all likely
# > questions 9-20 are:
#      0. Strongly Disagree
#      1. Disagree
#      2. Agree
#      3. Strongly Agree
# > questions 21-22 are open text responses that appear both in English or
#   Spanish
#
# > Our target variable (Y) are integer values 1-6 each indicating a major
# stage in TAPS grief journey map. Pulled this directly from the kaggle site:
# 1. "Immediate Grief, Shock & Emotion": Overwhelmed, loss of purpose; shock 
#     and trauma emotions (isolation) present and challenging to understand. 
#     Individuals may struggle to deal with family responsibilities alone. 
#     Surviving Child: Feeling disconnected without guidance and attention from 
#     grieving adults.
# 2. "Navigating Family Relationships": Experiencing tension between individuals
#     within the family unit; lack of support from family members. Surviving 
#     Family Unit: Perception of other family members' grief experience. Each 
#     family member may be at different phases of their grief journey.
# 3. "Learning to Process Grief": Experiencing grief and learning to process 
#     those emotions. Surviving Child: Seeks guidance and acknowledgment of 
#     grief; benefit from opportunities to open up and process with kids in 
#     similar situations to normalize emotions.
# 4. "Moments That Matter": Renewed experience of grief around anniversaries of 
#     loss, holidays, and special moments. Surviving Family Unit: Navigating 
#     special moments (sports, school achievements, moments that matter).
# 5. "Feeling Immersed, Connected & Seen": Finding new purpose and goals to begin
#     moving towards Positive Integration. Surviving Family Unit: Connected to a 
#     broader community; support system; not the only person/family experiencing 
#     loss.
# 6. "New Growth & Purpose": Healthy point in grief journey; feeling capable to 
#    help others and a desire to do so. Surviving Family Unit: Ready to give 
#    back to the TAPS community through mentorship programs, volunteering at 
#    charity drives & events, etc.
################################################################################
# Now, when you framed your business problem into an analytics problem, you
# might have decided it is a classification problem because our target variable
# are classes (1, 2, ..., 6). That's indeed what this is. I want you to treat it
# like so for now, and later you can consider "breaking the rules" or adding a 
# bit of art with the science by trying to see if framing it as a regression-type
# problem, or even trying to cluster into six groups helps improve your results.
#
# The other thing I hope you're thinking about are how to address your input
# variables. Questions 3-20 could be kept as numeric vectors. However, you might
# also define them as ordered factor vectors (ordinal variables). Questions 1 and
# 2 will likely remain as just factor vectors. Lets proceed in our first set
# of experiments leaving 3-20 as numbers.
################################################################################
# Q9 (1 point): 
# we don't need Id column in examples data 'd' set so delete it
d$Id <- NULL

# Q10 (1 point): 
# Make the target variable the first column in your examples data.frame, 'd'
d <- d[, c(ncol(d), 1:(ncol(d)-1))]

# Q11 (1 point): 
# Make target variable column name "y" in your examples data.frame, 'd'
names(d)[1] <- "y"

# Q12 (2 points): 
# Use the str_trim() function from the stringr library to remove whitespace
# on both the left and right side of columns q21 and q22 in BOTH the 'd' and
# 'test' data.frames. One example is provided.
library(stringr)
d$q21 <- str_trim(d$q21, side="both")
d$q22 <- str_trim(d$q22, side="both")
test$q21 <- str_trim(test$q21, side="both")
test$q22 <- str_trim(test$q22, side="both")




# Q13 (2 points): 
# use ifelse() to make actual missing cells NAs for columns q21 and q22 
# in BOTH the 'd' and 'test' data.frames. One example is provided.
d$q21 <- ifelse(nchar(d$q21)==0,NA,d$q21)
d$q22 <- ifelse(nchar(d$q22)==0,NA,d$q22)
test$q21 <- ifelse(nchar(test$q21)==0,NA,test$q21)
test$q22 <- ifelse(nchar(test$q22)==0,NA,test$q22)




# Q14 (1 point): 
# use the factor() function to create your 'y' target variable in 'd' as an
# ordered factor vector
str(d)
d$y <- factor(d$y, levels = 1:6, ordered = TRUE)

# Q15 (1 point): 
# use the as.factor() function to coerce/change q1 and q2 in both the 'd'
# and 'test' data.frames to unordered factor vectors.
d$q1 <- as.factor(d$q1)
d$q2 <- as.factor(d$q2)
test$q1 <- as.factor(test$q1)
test$q2 <- as.factor(test$q2)





# Q16 (1 point): 
# use the table() function to see the counts for (y, q1, and q2) in 'd' and
# (q1 and q2) in 'test'. This is important to review because when you're using
# one dataset (e.g., 'd') to train a model that will predict another dataset 
# (e.g., 'test'), one dataset might not have observations for a certain factor
# level and thus when you try to predict using that level it will give you an
# error. To fix this in advance, within the ?as.factor() function you will see
# you can specify levels=c("1","2","3","4","5","6"). It doesn't appear we need
# to do this, but I think it's a common problem that you will likely need to
# address in many datasets.
table(d$y)
table(d$q1)
table(d$q2)
table(test$q1)
table(test$q2)





################################################################################
# EDA
# Below I have created a couple figures using ggplot2. These are somewhat similar
# to class R script examples but have customized them a bit more.
# Check out various coloring themes here: https://r-charts.com/ggplot2/themes/
################################################################################
str(d)
#install.packages("wesanderson")
#install.packages("RColorBrewer")
library(wesanderson)
library(RColorBrewer)
library(ggplot2)

# Figure 1: Reason to attend TAPS event (q1)
p1 <- ggplot() + geom_bar(data=d, aes(x=factor(q1), fill=factor(y))
                          , position="fill") 
p1 <- p1 + guides(fill=guide_legend(title="Grief Stages"))
# different coloring themes
# http://www.sthda.com/english/wiki/ggplot2-colors-how-to-change-colors-automatically-and-manually
p1 <- p1 + scale_fill_brewer(palette="Reds")
p1 <- p1 + labs(title = "Reason for Attendance by Grief Stage"
                , x="Reason to attend TAPS event", y="Percent") 
p1


# Figure 2: How did you hear about TAPS (q2)
library(stringr)
p1 <- ggplot() + geom_bar(data=d, aes(x=factor(q1), position="fill")) 
p1 <- p1 + labs(title = "Reason for Attendance by Grief Stage"
                , x="Reason to attend TAPS event", y="Percent") 
p1 <- p1 + theme(panel.background = element_rect(fill = "#67c9ff"),
                 plot.background = element_rect(fill = "gray86"),
                 plot.margin = margin(t = 1,  # Top margin
                                      r = 1,  # Right margin
                                      b = 0,  # Bottom margin
                                      l = 1,  # Left margin
                                      unit = "cm"),
                 axis.text.x = element_text(margin=margin(t=1, unit="cm")
                                            , angle=60, size=11, color="black")
                 #axis.title.y = element_text(size = rel(1.5), angle = 90))
                 )
p1 <- p1 + scale_x_discrete(labels=function(x) str_wrap(c(
                                      "0" = "Previous Regional/National seminar"
                                     ,"1" = "Connect w/ other survivors"
                                     ,"2" = "Learn more about TAPS resources"
                                     ,"3" = "Learn new tools and information"
                                     ,"4" = "Learn how to support adult family members"
                                     ,"5" = "Learn how to support my child(ren)"
                                     ,"6" = "Child(ren) attend Good Grief Camp"
                                     ,"7" = "Child(ren) connect w/ a Military Mentor"
                              ), width = 20))
p1

# Figure 3: How did you hear about TAPS (q2)
p1 <- ggplot() + geom_bar(data=d, aes(x=factor(q2), fill=factor(y))
                          , position="fill") 
p1 <- p1 + guides(fill=guide_legend(title="Grief Stages"))
# different coloring themes
# http://www.sthda.com/english/wiki/ggplot2-colors-how-to-change-colors-automatically-and-manually
p1 <- p1 + scale_fill_brewer(palette="Reds")
p1 <- p1 + labs(title = "How did you hear about TAPS by Grief Stage"
                , x="How did you hear about TAPS", y="Percent") 
p1

# Q17 (2 points): 
# Explore various visualizations and summarizations to help you identify at least
# two potentially strong relationships with what we are trying to predict. Only
# show these two graphics in your submitted R script but you can keep the other
# things you explored in another R script.

# Visualization 1: Relationship between q7 and y
ggplot(d, aes(x = factor(q7), fill = factor(y))) +
  geom_bar(position = "fill") +
  labs(title = "Relationship between q7 and Grief Stage",
       x = "q7: How likely are you to recommend TAPS?",
       y = "Proportion",
       fill = "Grief Stage") +
  theme_minimal()

# Visualization 2: Relationship between q15 and y
ggplot(d, aes(x = factor(q15), fill = factor(y))) +
  geom_bar(position = "fill") +
  labs(title = "Relationship between q15 and Grief Stage",
       x = "q15: I feel more hopeful about the future",
       y = "Proportion",
       fill = "Grief Stage") +
  theme_minimal()



################################################################################
# Can we detect the language the comments are in?
# Here I wrote a loop that created a new column in 'd' that detects what 
# language the comments are in.
################################################################################
library("textcat")
d$lang <- NA
for (i in 1:nrow(d)){
  if (!is.na(d[i,"q21"])) {
    d[i,"lang"] <- textcat(d[i,"q21"])
  } else if (!is.na(d[i,"q22"])) {
    d[i,"lang"] <- textcat(d[i,"q22"])
  } else {
    d[i,"lang"] <- "english"
  }
}

################################################################################
# Can we translate Spanish to English?
# Here I used Google Translate to convert the Spanish comments to English. This
# may not be a perfect translation but it's one option you could try.
################################################################################
library(polyglotr)
?google_translate
d$q21 <- google_translate(text=d$q21, target_language="en"
                           , source_language="auto")
d$q21 <- ifelse(d$q21=="THAT","",d$q21)

d$q22 <- google_translate(text=d$q22, target_language="en"
                          , source_language="auto")
d$q22 <- ifelse(d$q22=="THAT","",d$q22)

################################################################################
# Lets do some text processing/mining for questions q21 and q22
################################################################################
# make sure q21 and q22 are character vectors
d$q21 <- as.character(d$q21)
d$q22 <- as.character(d$q22)
str(d)

# we will save q21 and q22 as their own tibble data.frames to do our text mining
# on
library(dplyr)
q21 <- tibble(line=1:50, text=d$q21)
q22 <- tibble(line=1:50, text=d$q22)

# Q18 (1 point): 
# Split a column into tokens using the tokenizers package, splitting the table 
# into one-token-per-row for q21 and then q22 
library(tidytext)
q21 <- q21 %>%
  unnest_tokens(word, text)

q22 <- q22 %>%
  unnest_tokens(word, text)
  
  
  
  
# Q19 (1 point): 
# load the stop_words dictionary; note that you can always add words or remove
# words to this if you like based on your own problems context/domain. Then
# remove stop words from dataset for q21 and q22
data(stop_words)
  

# remove stop words from dataset
q21 <- q21 %>%
  anti_join(stop_words)

q22 <- q22 %>%
  anti_join(stop_words)
  
  

# Q20 (1 point): 
# find the most common words for q21 then q22
q21 %>%
count(word, sort = TRUE) %>%
head(10)

q22 %>%
count(word, sort = TRUE) %>%
head(10)
  
  
  

# Q22 (1 point): 
# Create a visual showing the most common words for q21 then q22
library(ggplot2)

q21 %>%
  count(word, sort = TRUE) %>%
  top_n(10) %>%
  ggplot(aes(x = reorder(word, n), y = n)) +
  geom_col() +
  coord_flip() +
  labs(title = "Top 10 Most Common Words in q21",
       x = "Word",
       y = "Count")

q22 %>%
  count(word, sort = TRUE) %>%
  top_n(10) %>%
  ggplot(aes(x = reorder(word, n), y = n)) +
  geom_col() +
  coord_flip() +
  labs(title = "Top 10 Most Common Words in q22",
       x = "Word",
       y = "Count")


################################################################################
# Lets build a predictive model using caret (see "caret demo.R") using just the
# numeric features for now (so columns 4 thru 21)
################################################################################
library(caret)
str(d)
names(d)

# Q23 (1 point): 
# Show how to identify correlated predictors with a correlation greater than
# +/- 0.85. You may or may not need to remove some variables.
descrCor <-  cor(d[,4:21])                           # correlation matrix
highlyCorrelated <- findCorrelation(descrCor, cutoff = 0.85)
print(highlyCorrelated)


# Q24 (1 point): 
# Identify and remove any features with limited variation. There may or may
# not be any
nzv <- nearZeroVar(d[,4:21], saveMetrics = TRUE)
print(nzv)

  
  

# Q25 (1 point): 
# Standardize your numeric features using a min-max normalization. Do this for
# the 'd' data.frame, then the 'test' data.frame
preProcValues <- preProcess(d[,4:21], method = c("center", "scale"))
d[,4:21] <- predict(preProcValues, d[,4:21])

test[,3:22] <- predict(preProcValues, test[,3:22])


names(test)

# make names for target if not already made
levels(d$y) <- make.names(levels(factor(d$y)))
levels(d$y)

# Q26 (2 points): 
# given the small size of our 'd' dataframe we will try leave-one-out cross
# validation and not split the data into separate train and test. Specify 
# you loocv cross-validation design in trainControl
library(MLmetrics)
ctrl <- trainControl(method="LOOCV",         # cross-validation set approach to use
                     classProbs = T,  # if you want probabilities
                     summaryFunction = multiClassSummary, # for multi-classification
                     allowParallel=T)

set.seed(123)
myModel1 <- train(y ~ q4+q5+q6+q7+q8+q9+q10+q11+q12+q13+q14+q15+q16+q17+q18+q19+q20,
                  data = d,            # train set used to build model
                  method = "rf",       # Random Forest model
                  trControl = ctrl,    # how you want to learn
                  metric = "Accuracy",  # performance measure
)
myModel1

# Q27 (1 point): 
# generate estimated probabilities and predicted classes
pred_probs <- predict(myModel1, newdata=d, type='prob')
pred_classes <- predict(myModel1, newdata=d)

# Q28 1 point): 
# use the confusionMatrix() function to generate model statistics
(cm <- confusionMatrix(pred_classes, d$y))

# Q29 (1 point): 
# generate predictions on your 'test' set and download them as a csv file to
# upload to kaggle for scoring. Report your score in comments below
preds <- predict(myModel1, newdata=test)
preds_df <- data.frame(Id=test$Id, label=as.numeric(preds))
write.csv(preds_df, file="kaggle_preds.csv", row.names=FALSE, quote=c(1))

# Kaggle score: [0.88393]

