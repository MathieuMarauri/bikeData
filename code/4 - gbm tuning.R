
# The gradient boosting models is tuned to try to find the best combination of
# hyperparameters. To perform the tuning the search space, an optimization algorithm, a
# resampling strategy and a performance measure must be defined.

# The parameters that wil be tuned are the number of trees, the maximum depth of each tree
# and the learning rate. First each hyperparameter is tuned on its own to find the best
# small set of values. Then they are tuned by pair and finally all together but with a
# search space as small as possible to avoid performing lots and lots of models and to see
# the exact impact of each parameter.

# More trees leads to better performance but it can also overfits and it increases the
# processing time. The maximum depth of a tree is used to control the ability of a tree to
# be (too?) precise. The learning rate defines how each iteration impacts the final result
# of the model. Value of 0.1 is said to be the best. Lower values need more trees to catch
# all the relations.

# Inputs: the bikes task.

# Outputs: the best combination of hyperparameters.

# Packages --------------------------------------------------------------------------

library('mlr') # machine learning workflow
library('ggplot2') # visualize performance
library('h2o') # close h2o cluster
library('extrafont') # add special font to plots


# Initialization of tuning ----------------------------------------------------------

# The different elements required to peform a tuning are defined here. Gridsearch is used
# as optimization algorithm. It will go through the entire search space and calculates the
# value of the performance measures for every parameter values defined in the
# hyperparameter space. The performance measures used are the means squared error and the
# time to train end predict. The resampling strategy used is CV 5-fold.

# import task
task <- readRDS('data/benchmark/task.rds')

# base learner
learner <- makeLearner('regr.h2o.gbm', 
                       id = 'gbm', 
                       par.vals = list(ntrees = 300, max_depth = 5, learn_rate = 0.1))

# resampling strategy
resampling <- makeResampleDesc("CV", iters = 5L)

# optimization algorithm
optim_algo <- makeTuneControlGrid()

# performance measures
measures <- list(mse, timeboth)

# font and plot theme for the visualization of the performances
# load font to add to ggplot
windowsFonts(century = "Century Gothic")
loadfonts(device = "win")

# theme
theme_plot <- theme_bw() + 
  theme(text = element_text(family = 'century'), 
        plot.title = element_text(size = 30, colour = 'grey10', face = 'bold'),
        plot.subtitle = element_text(size = 20, colour = 'grey10', face = 'italic'),
        axis.title = element_text(size = 25, colour = 'grey10'),
        axis.text = element_text(size = 20, colour = 'grey10'))


# Univariate tuning -----------------------------------------------------------------

# Hyperparameters are tuned one by one. A range of value around the default value is
# tested and the results are plotted to find the best value regarding the means squared
# error.

# Maximum depth of trees.
tune_max_depth <- readRDS('data/gbm/max_depth.rds')

# # parameter space 
# parameter_space <- makeParamSet(
#   makeDiscreteParam(id = 'max_depth', values = 2:20)
# )
# 
# # tuning
# tune_max_depth <- tuneParams(learner = learner, 
#                              task = task, 
#                              resampling = resampling,
#                              par.set = parameter_space, 
#                              control = optim_algo, 
#                              measures = measures)
# tune_max_depth <- generateHyperParsEffectData(tune_max_depth)$data
# 
# # close h2o cluster and save results
# h2o.shutdown(prompt = FALSE)
# saveRDS(tune_max_depth, 'data/gbm/max_depth.rds')

# plot
ggplot(data = tune_max_depth, mapping = aes(x = max_depth, y = mse.test.mean)) + 
  geom_line(color = 'dodgerblue4', size = 1) + 
  geom_point(color = 'dodgerblue4', size = 3) + 
  geom_point(data = tune_max_depth[tune_max_depth$mse.test.mean == min(tune_max_depth$mse.test.mean),], 
             mapping = aes(x = max_depth, y = mse.test.mean), 
             color = 'maroon4', size = 3) +  
  labs(x = 'Profondeur des arbres', y = 'Erreur quadratique moyenne',
       title = 'Evolution de l\'erreur quadratique moyenne en fonction \nde la profondeur des arbres.',
       subtitle = 'Le point correspond à la meilleure mesure de performane \nest d\'une couleur différente.') + 
  scale_x_continuous(breaks = seq(2, 20, by = 2)) + 
  theme_plot


# Mean squared error decreases when the maximum depth increases up to values around 6, 7,
# 8. It then increases a bit but stay low.

# Number of trees.
tune_ntrees <- readRDS('data/gbm/ntrees.rds')

# # parameter space
# parameter_space <- makeParamSet(
#   makeDiscreteParam(id = 'ntrees', values = seq(100, 1000, by = 50))
# )
# 
# # tuning
# tune_ntrees <- tuneParams(learner = learner, 
#                           task = task, 
#                           resampling = resampling,
#                           par.set = parameter_space, 
#                           control = optim_algo, 
#                           measures = measures)
# tune_ntrees <- generateHyperParsEffectData(tune_ntrees)$data
# 
# # close h2o cluster and save results
# h2o.shutdown(prompt = FALSE)
# saveRDS(tune_ntrees, 'data/gbm/ntrees.rds')

# plot the results
ggplot(data = tune_ntrees, mapping = aes(x = ntrees, y = mse.test.mean)) + 
  geom_line(color = 'dodgerblue4', size = 1) + 
  geom_point(color = 'dodgerblue4', size = 3) + 
  geom_point(data = tune_ntrees[tune_ntrees$mse.test.mean == min(tune_ntrees$mse.test.mean),], 
             mapping = aes(x = ntrees, y = mse.test.mean), 
             color = 'maroon4', size = 3) +  
  labs(x = "Nombre d'arbres", y = 'Erreur quadratique moyenne',
       title = "Evolution de l'erreur quadratique moyenne en fonction \ndu nombre d'arbres.",
       subtitle = 'Le point correspond à la meilleure mesure de performane \nest d\'une couleur différente.') + 
  scale_x_continuous(breaks = seq(100, 1000, by = 50)) + 
  theme_plot


# The mean square error decreases with the number of trees. Above 500 trees there is no
# gain in performance.

# Learning rate
tune_learn_rate <- readRDS('data/gbm/learn_rate.rds')

# parameter space
# parameter_space <- makeParamSet(
#   makeDiscreteParam(id = 'learn_rate', values = c(0.0001, 0.0005, 0.001, 0.005, 0.01, 0.05, 0.1, 0.2, 0.3, 0.4))
# )
# 
# # tuning
# tune_learn_rate <- tuneParams(learner = learner,
#                               task = task,
#                               resampling = resampling,
#                               par.set = parameter_space,
#                               control = optim_algo,
#                               measures = measures)
# tune_learn_rate <- generateHyperParsEffectData(tune_learn_rate)$data
# 
# # close h2o cluster and save results
# h2o.shutdown(prompt = FALSE)
# saveRDS(tune_learn_rate, 'data/gbm/learn_rate.rds')

# plot the results
ggplot(data = tune_learn_rate, mapping = aes(x = learn_rate, y = mse.test.mean)) + 
  geom_line(color = 'dodgerblue4', size = 1) + 
  geom_point(color = 'dodgerblue4', size = 3) + 
  geom_point(data = tune_learn_rate[tune_learn_rate$mse.test.mean == min(tune_learn_rate$mse.test.mean),], 
             mapping = aes(x = learn_rate, y = mse.test.mean), 
             color = 'maroon4', size = 3) + 
  labs(x = "Coefficeint de rétrécissement", y = 'Erreur quadratique moyenne',
       title = "Evolution de l'erreur quadratique moyenne en fonction \ndu paramètre d'ajustement.",
       subtitle = 'Le point correspondant à la meilleure mesure de performane \nest d\'une couleur différente.') + 
  scale_x_continuous(breaks = c(0, 0.05, 0.1, 0.2, 0.3, 0.4)) + 
  theme_plot

# Values of learning rate lower than 0.1 give poor results. Performance decreases a bit
# after 0.1.


# Bivariate tuning ------------------------------------------------------------------

# Hyperparameters are tuned two by two. Specific sets of values are tested to detect the
# relationship between the parameters.

# Maximum depth of trees and number of trees. 
tune_max_depth_ntrees <- readRDS('data/gbm/max_depth_ntrees.rds')

# # parameter space
# parameter_space <- makeParamSet(
#   makeDiscreteParam(id = 'max_depth', values = 3:10),
#   makeDiscreteParam(id = 'ntrees', values = seq(350, 600, by = 50))
# )
# 
# # tuning
# tune_max_depth_ntrees <- tuneParams(learner = learner, 
#                                     task = task, 
#                                     resampling = resampling,
#                                     par.set = parameter_space, 
#                                     control = optim_algo, 
#                                     measures = measures)
# tune_max_depth_ntrees <- generateHyperParsEffectData(tune_max_depth_ntrees)$data
# 
# # close h2o cluster and save results
# h2o.shutdown(prompt = FALSE)
# saveRDS(tune_max_depth_ntrees, 'data/gbm/max_depth_ntrees.rds')

# plot the results
ggplot(data = tune_max_depth_ntrees, 
       mapping = aes(x = max_depth, y = mse.test.mean, color = as.factor(ntrees))) + 
  geom_line() + 
  geom_point() + 
  scale_color_brewer(palette = 'Dark2', name = "Nombre d'arbres") + 
  labs(title = "Evolution de l'erreur quadratique moyenne en fonction \ndu nombre d'arbres et la profondeur des arbres.",
       x = 'Profondeur des arbres', y = 'Erreur quadratique moyenne') + 
  theme_plot+ 
  theme(legend.position = c(0.85, 0.8), 
        legend.background = element_rect(colour = 'grey20'),
        legend.text = element_text(size = 20), 
        legend.title = element_text(size = 25))

# The optimal maximum depth value is 6 or 7. The higher the number of trees, the better
# the performance for every values of maximum depth. As the maximum depht increases the
# trees are getting less and less weak hence the number of trees has no impact.

# Learning rate and number of trees. These two hyperparameters are supposed to be linked,
# low values of learning rate requiring high number of trees. 
tune_learn_rate_ntrees <- readRDS('data/gbm/ntrees_learn_rate.rds')

# # parameter space
# parameter_space <- makeParamSet(
#   makeDiscreteParam(id = 'learn_rate', values = c(0.005, 0.01, 0.05, 0.1, 0.2, 0.3)),
#   makeDiscreteParam(id = 'ntrees', values = c(400, 500, 600, 700, 800))
# )
# 
# # tuning
# tune_learn_rate_ntrees <- tuneParams(learner = learner, 
#                                     task = task, 
#                                     resampling = resampling,
#                                     par.set = parameter_space, 
#                                     control = optim_algo, 
#                                     measures = measures)
# tune_learn_rate_ntrees <- generateHyperParsEffectData(tune_learn_rate_ntrees)$data
# 
# # close h2o cluster and save results
# h2o.shutdown(prompt = FALSE)
# saveRDS(tune_learn_rate_ntrees, 'data/gbm/ntrees_learn_rate.rds')

# plot the results
ggplot(data = tune_learn_rate_ntrees,
       mapping = aes(x = ntrees, y = mse.test.mean, color = as.factor(learn_rate))) +
  geom_line() +
  geom_point() + 
  scale_color_brewer(palette = 'Dark2', name = "Coefficeint de rétrécissement") + 
  labs(title = "Evolution de l'erreur quadratique moyenne en fonction \ndu nombre d'arbres et du paramètre d'ajustement.",
       x = "Nombre d'arbres", y = 'Erreur quadratique moyenne') + 
  theme_plot+ 
  theme(legend.position = c(0.8, 0.8), 
        legend.background = element_rect(colour = 'grey20'),
        legend.text = element_text(size = 20), 
        legend.title = element_text(size = 25))

# It is clear that the lower the learning rate value the higher the number of trees needed
# to reach the same performance. A learning rate of 0.1 gives the best results for every
# number of trees but it is constant. At 0.05 the performance measure is decreasing with
# the number of trees. Another search space is created with values between 0.05 and 0.1
# and greater numbers of trees.

tune_learn_rate_ntrees2 <- readRDS('data/gbm/ntrees_learn_rate2.rds')

# # parameter space
# parameter_space <- makeParamSet(
#   makeDiscreteParam(id = 'learn_rate', values = c(0.05, 0.06, 0.07, 0.08, 0.09, 0.1)),
#   makeDiscreteParam(id = 'ntrees', values = c(300, 500, 700, 900))
# )
# 
# # tuning
# tune_learn_rate_ntrees2 <- tuneParams(learner = learner,
#                                     task = task,
#                                     resampling = resampling,
#                                     par.set = parameter_space,
#                                     control = optim_algo,
#                                     measures = measures)
# tune_learn_rate_ntrees2 <- generateHyperParsEffectData(tune_learn_rate_ntrees2)$data
# 
# # close h2o cluster and save results
# h2o.shutdown(prompt = FALSE)
# saveRDS(tune_learn_rate_ntrees2, 'data/gbm/ntrees_learn_rate2.rds')

# plot the results
ggplot(data = tune_learn_rate_ntrees2,
       mapping = aes(x = ntrees, y = mse.test.mean, color = as.factor(learn_rate))) +
  geom_line() +
  geom_point() + 
  scale_color_brewer(palette = 'Dark2', name = "Coefficeint de rétrécissement") + 
  labs(title = "Evolution de l'erreur quadratique moyenne en fonction \ndu nombre d'arbres et du paramètre d'ajustement.",
       x = "Nombre d'arbres", y = 'Erreur quadratique moyenne') + 
  theme_plot+ 
  theme(legend.position = c(0.8, 0.8), 
        legend.background = element_rect(colour = 'grey20'),
        legend.text = element_text(size = 20), 
        legend.title = element_text(size = 25))

# The best value for the learning rate is 0.07 with a number of trees of at least 500.

# Learning rate and maximum depth of the trees. 
tune_learn_rate_max_depth <- readRDS('data/gbm/max_depth_learn_rate.rds')

# # parameter space
# parameter_space <- makeParamSet(
#   makeDiscreteParam(id = 'learn_rate', values = c(0.005, 0.01, 0.05, 0.1, 0.2, 0.3)),
#   makeDiscreteParam(id = 'max_depth', values = 4:9)
# )
# 
# # tuning
# tune_learn_rate_max_depth <- tuneParams(learner = learner, 
#                                      task = task, 
#                                      resampling = resampling,
#                                      par.set = parameter_space, 
#                                      control = optim_algo, 
#                                      measures = measures)
# tune_learn_rate_max_depth <- generateHyperParsEffectData(tune_learn_rate_max_depth)$data
# 
# # close h2o cluster and save results
# h2o.shutdown(prompt = FALSE)
# saveRDS(tune_learn_rate_max_depth, 'data/gbm/max_depth_learn_rate.rds')

# plot the results
ggplot(data = tune_learn_rate_max_depth, 
       mapping = aes(x = learn_rate, y = mse.test.mean, color = as.factor(max_depth))) + 
  geom_line() + 
  geom_point() + 
  scale_color_brewer(palette = 'Dark2', name = "Profondeur de l'arbre") + 
  labs(title = "Evolution de l'erreur quadratique moyenne en fonction \ndu nombre d'arbres et du paramètre d'ajustement.",
       x = "Coefficeint de rétrécissement", y = 'Erreur quadratique moyenne') + 
  theme_plot+ 
  theme(legend.position = c(0.8, 0.8), 
        legend.background = element_rect(colour = 'grey20'),
        legend.text = element_text(size = 20), 
        legend.title = element_text(size = 25))

# The best performances are obtained with a value of learning rate of 0.05 and 0.1 and a
# maximum depth higher or equal to 6.


# Final tuning ----------------------------------------------------------------------

# Considering the results of the previous section the hyperparameters are tuned at the
# same time. The search space is reduced to few values.

# # parameter space
# parameter_space <- makeParamSet(
#   makeDiscreteParam(id = 'learn_rate', values = c(0.06, 0.07, 0.08, 0.09)),
#   makeDiscreteParam(id = 'max_depth', values = 6:8),
#   makeDiscreteParam(id = 'ntrees', values = c(450, 500, 550, 600))
# )
# 
# # tuning
# tune_all <- tuneParams(learner = learner,
#                        task = task,
#                        resampling = resampling,
#                        par.set = parameter_space,
#                        control = optim_algo,
#                        measures = measures)
# tune_all <- generateHyperParsEffectData(tune_all)$data
# 
# # close h2o cluster and save results
# h2o.shutdown(prompt = FALSE)
saveRDS(tune_all, 'data/gbm/tune_all.rds')

