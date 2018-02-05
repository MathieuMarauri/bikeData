
# Several algorithms are benchmarked on the data. Using the mlr workflow a task
# is created with a pre-processed dataset (some columns are removed and other
# factorized). The algorithms tested are not tuned, their default
# hyperparameters are used. The one performing the best will be tuned later. 

# Input: the cleaned bikes dataset with the additional time variables.

# Outputs: the best learner and the task used.

# Packages ----------------------------------------------------------------

library('mlr') # machine learning workflow
library('data.table') # dataset manipulation
library('ggplot2') # data visualization
library('extrafont') # import fonts


# Task and available learners ---------------------------------------------

# The task is constructed ( a regression task on the bikes dataset with the
# count variabel as the target). Some leaners and a performance measure are
# selected from the list of all the available learners and performance measures.
# A leanrer can be applied on the task if it can predict numeric values with
# predictors being numeric and categorical without missing values. The columns
# datetime and date are removed as they are not predictors (they have been
# divided into several time variables). The columns holiday and day_month are
# removed because of their lack of impact on the count variable. The variable
# casual and registered are removed because they are subdivision of the count
# variable and therefore cannot be used as predictors.

# import data
bikes <- readRDS('data/clean/bikes_clean.rds')

# factorize time variables 
factor_cols <- c('year', 'hour_day', 'month', 'day_week')
bikes[, (factor_cols) := lapply(X = .SD, FUN = function(x) factor(x, ordered = FALSE)), 
      .SDcols = factor_cols]

# remove datetime, date, holiday, day_month, casual, registered 
remove_cols <- c('datetime', 'date', 'holiday', 'day_month', 'casual', 'registered')
bikes[, (remove_cols) := NULL]

# construct the task
task <- makeRegrTask(id = 'bikes', data = as.data.frame(bikes), target = 'count')

# list of available learners for the task
available_learners <- listLearners(task)

# List of performance measures
listMeasures(task)

# The larners that will be benchmarked are glm, gbm, svm, randomForest, cforest
# and deeplearning. The performance measure that will be used to compare then is
# the mean square error. The time to train the model and make the prediction is
# also used as a performance measure. 

# clean session
rm(factor_cols, remove_cols)


# Benchmark ---------------------------------------------------------------

# A learner object by algorithm is created. The resampling strategy choosed is
# holdout with a ratio of 0.25. It allows to computes the performance measure on
# the same test set and to train the different models on the same train set,
# ensuring correct comparison. 

# construct the learners to compare
learner_cforest <- makeLearner('regr.cforest', id = 'cforest')
learner_gbm <- makeLearner('regr.h2o.gbm', id = 'gbm', par.vals = list(ntrees = 200))
learner_glm <- makeLearner('regr.h2o.glm', id = 'glm')
learner_deeplearning <- makeLearner('regr.h2o.deeplearning', id = 'deeplearning')
learner_randomforest <- makeLearner('regr.h2o.randomForest', id = 'randomForest', par.vals = list(ntrees = 200))
learner_svm <- makeLearner('regr.ksvm', id = 'svm')

# resampling strategy
set.seed(123456)
train_set <- sample(nrow(bikes), (3 * nrow(bikes)) / 4)
test_set <- setdiff(1:nrow(bikes), train_set)
resampling <- makeFixedHoldoutInstance(train.inds = train_set, 
                                       test.inds = test_set, 
                                       size = task$task.desc$size)

# list of performance measures
perf_measures <- list(mse, timeboth)

# benchmark
benchmark <- benchmark(learners = list(learner_cforest, learner_gbm, learner_glm, 
                                       learner_deeplearning, learner_randomforest),
                       tasks = task,
                       resamplings = resampling,
                       measures = perf_measures, 
                       keep.pred = FALSE, 
                       models = FALSE)
benchmark <- getBMRAggrPerformances(bmr = benchmark, as.df = TRUE)

# save results and clean session
saveRDS(task, 'data/benchmark/task.rds')
saveRDS(benchmark, 'data/benchmark/benchmark.rds')
saveRDS(train_set, 'data/benchmark/train_set.rds')
saveRDS(test_set, 'data/benchmark/train_set.rds')
rm(learner_cforest, learner_gbm, learner_glm, learner_deeplearning, learner_randomforest, 
   learner_svm, resampling, perf_measures, available_learners, task)


# Performances ------------------------------------------------------------

# The performance measures of the different algorithm tested are visualized
# here. Models are first compared regarding the mean squared error then the
# processing time. 

# load font to add to ggplot
windowsFonts(century = "Century Gothic")
loadfonts(device = "win")

# import benchmark results
benchmark <- readRDS('data/benchmark/benchmark.rds')

# order models by performance
benchmark$learner.id <- factor(benchmark$learner.id, 
                               levels = benchmark$learner.id[order(benchmark$mse.test.mean)])

# plot the results of the benchmark, mse and time
ggplot(data = benchmark, mapping = aes(x = mse.test.mean, y = learner.id)) + 
  geom_point(colour = 'dodgerblue4', size = 3) + 
  geom_segment(mapping = aes(xend = 0, yend = learner.id), colour = 'dodgerblue4', size = 1) + 
  labs(x = 'Erreur', y = 'Algorithme', 
       title = 'Comparaison des algorithmes utilisés.',
       subtitle = 'La mesure utilisée est l\'erreur quadratique moyenne.') + 
  theme_bw() +
  theme(text = element_text(family = 'century'), 
        plot.title = element_text(size = 30, colour = 'grey10', face = 'bold'),
        plot.subtitle = element_text(size = 20, colour = 'grey10', face = 'italic'),
        axis.title = element_text(size = 25, colour = 'grey10'),
        axis.text = element_text(size = 20, colour = 'grey10'))

ggplot(data = benchmark, mapping = aes(x = timeboth.test.mean, y = learner.id)) + 
  geom_point(colour = 'dodgerblue4', size = 3) + 
  geom_segment(mapping = aes(xend = 0, yend = learner.id), colour = 'dodgerblue4', size = 1) + 
  labs(x = 'Temps de calcul', y = 'Algorithme', 
       title = 'Comparaison des algorithmes utilisés.',
       subtitle = 'La mesure utilisée est le temps de calcul.') + 
  theme_bw() +
  theme(text = element_text(family = 'century'), 
        plot.title = element_text(size = 30, colour = 'grey10', face = 'bold'),
        plot.subtitle = element_text(size = 20, colour = 'grey10', face = 'italic'),
        axis.title = element_text(size = 25, colour = 'grey10'),
        axis.text = element_text(size = 20, colour = 'grey10'))

# The gradient boosting machine algorithm has the best peformances. It has the
# lowest error and the second to lowest processing time.