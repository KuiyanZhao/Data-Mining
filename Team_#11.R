setwd("")
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



# Q3 add
q21 <- q21 %>% mutate(id = row_number())
q22 <- q22 %>% mutate(id = row_number())

q21_stages <- q21 %>%
  left_join(d %>% select(y) %>% mutate(id = row_number()), by = "id") %>%
  group_by(y, word) %>%
  summarise(n = n(), .groups = 'drop') %>%
  arrange(y, desc(n))

q22_stages <- q22 %>%
  left_join(d %>% select(y) %>% mutate(id = row_number()), by = "id") %>%
  group_by(y, word) %>%
  summarise(n = n(), .groups = 'drop') %>%
  arrange(y, desc(n))

print("Top 10 words for each grief stage in q21:")
print(q21_stages %>% group_by(y) %>% top_n(10, n))

print("Top 10 words for each grief stage in q22:")
print(q22_stages %>% group_by(y) %>% top_n(10, n))

ggplot(q21_stages %>% group_by(y) %>% top_n(5, n), 
       aes(x = reorder(word, n), y = n, fill = y)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~y, scales = "free_y") +
  coord_flip() +
  labs(title = "Top 5 Words for Each Grief Stage (q21)",
       x = "Word",
       y = "Count")

ggplot(q22_stages %>% group_by(y) %>% top_n(5, n), 
       aes(x = reorder(word, n), y = n, fill = y)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~y, scales = "free_y") +
  coord_flip() +
  labs(title = "Top 5 Words for Each Grief Stage (q22)",
       x = "Word",
       y = "Count")

word_proportions <- bind_rows(q21_stages, q22_stages) %>%
  group_by(y) %>%
  mutate(total = sum(n)) %>%
  ungroup() %>%
  mutate(proportion = n / total) %>%
  group_by(word) %>%
  summarise(
    max_prop = max(proportion),
    min_prop = min(proportion),
    diff_prop = max_prop - min_prop,
    .groups = 'drop'
  ) %>%
  arrange(desc(diff_prop))

print(head(word_proportions, 10))

top_words <- head(word_proportions$word, 3)

for(word in top_words) {
  d[[paste0(word, "_count")]] <- str_count(tolower(paste(d$q21, d$q22)), word)
  test[[paste0(word, "_count")]] <- str_count(tolower(paste(test$q21, test$q22)), word)
}



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
formula_string <- paste("y ~", paste(c(paste0("q", 4:20), paste0(top_words, "_count")), collapse = " + "))
myModel1 <- train(as.formula(formula_string),
                  data = d,            # train set used to build model
                  method = "glmnet",       # glmnet is for multi-classification
                  trControl = ctrl,    # how you want to learn
                  metric = "Accuracy"  # performance measure
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
write.csv(preds_df, file="Q3_kaggle_preds.csv", row.names=FALSE, quote=c(1))




# Q4 add
set.seed(123)
rfModel <- train(as.formula(formula_string),
                 data = d,
                 method = "rf",
                 trControl = ctrl,
                 metric = "Accuracy")

print(rfModel)

rf_preds <- predict(rfModel, newdata=test)
rf_preds_df <- data.frame(Id=test$Id, label=as.numeric(rf_preds))
write.csv(rf_preds_df, file="Q4_kaggle_preds_rf.csv", row.names=FALSE, quote=c(1))


set.seed(123)
nbModel <- train(as.formula(formula_string),
                 data = d,
                 method = "naive_bayes",  
                 trControl = ctrl,
                 metric = "Accuracy",
                 )

print(nbModel)

nb_preds <- predict(nbModel, newdata=test)
nb_preds_df <- data.frame(Id=test$Id, label=as.numeric(nb_preds))
write.csv(nb_preds_df, file="Q4_kaggle_preds_nb.csv", row.names=FALSE, quote=c(1))

orig_probs <- predict(myModel1, newdata=test, type='prob')
orig_preds <- predict(myModel1, newdata=test)

rf_probs <- predict(rfModel, newdata=test, type='prob')
rf_preds <- predict(rfModel, newdata=test)

nb_probs <- predict(nbModel, newdata=test, type='prob')
nb_preds <- predict(nbModel, newdata=test)

all_preds <- data.frame(
  Id = test$Id,
  orig = orig_preds,
  rf = rf_preds,
  nb = nb_preds
)

all_probs <- data.frame(
  Id = test$Id,
  orig_probs,
  rf_probs,
  nb_probs
)

write.csv(all_preds, file="Q4_all_predictions.csv", row.names=FALSE)
write.csv(all_probs, file="Q4_all_probabilities.csv", row.names=FALSE)

