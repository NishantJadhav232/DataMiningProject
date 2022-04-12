
# I have evaluated these predictions against the test set
# the success predictions have accuracy = 0.6045278
# the backers_count predictions have RMSE = 396.0263
# the big_hit predictions have AUC = 0.6303743


library(tidyverse)

#starter <- read.csv('ks_training_X.csv')

trainxs <- read.csv('small_training_X.csv')
trainys <- read.csv('small_training_Y.csv')

trains <- trainxs %>%
  left_join(trainys, by = "id") %>%
  mutate(success = as.factor(success),
         big_hit = as.factor(big_hit))

cleans <- trains%>%
  mutate(
    location_type = as.factor(location_type),
    region = as.factor(region),
    category_parent = as.factor(category_parent),
    color_foreground = as.factor(color_foreground),
    color_background = as.factor(color_background),
    accent_color = as.factor(accent_color),
    captions = as.factor(captions),
    deadline = as.Date(deadline)
  )

base_data1 <- cleans%>%
  select(goal,location_type,region,category_parent,numfaces_project,numfaces_creator,male_project,
male_creator,female_project,female_creator,smiling_project,smiling_creator,minage_project,minage_creator,
maxage_project,maxage_creator,num_words,avg_wordlengths,contains_youtube,sentence_counter,avgsentencelength,
avgsyls,grade_level,afinn_pos,afinn_neg,ADV,NOUN,ADP,PRT,DET,PRON,VERB,NUM,CONJ,ADJ,success)


# Base training

set.seed(1)
va_inds <- sample(nrow(base_data1), .3*nrow(base_data1))
tr <- base_data1[-va_inds,]
va <- base_data1[va_inds,]

log1 <- glm(success~., data = tr, family = "binomial")
probs_success <- predict(log1, newdata = va, type = "response")

classifications <- ifelse(probs_success > .5, "YES", "NO")
correct <- ifelse(classifications == va$success, 1, 0)
acc <- sum(correct)/length(correct)


## category_name, category_parent, captions, accent_color -- select top
## tag_names, reqard_amounts -- split