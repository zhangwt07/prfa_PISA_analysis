learn_mc <- function(student_china,parent_china){


  require(h2o)



  student_subset <- student_china[CNT=="Macao-China", .(CNT,STRATUM, STIDSTD, MATHEFF, ANXMAT, ESCS,OPENPS)]
  parent_subset <- parent_china[CNT=="Macao-China", .(STRATUM, STIDSTD, PARINVOL)]

  setkey(student_subset, STRATUM, STIDSTD)
  setkey(parent_subset, STRATUM, STIDSTD)

  student_subset[parent_subset, PARINVOL := PARINVOL]


  h2o.init(nthreads = -1, max_mem_size = "2G")
  h2o.removeAll() ## clean slate - just in case the cluster was already running


  h2o_data <- na.omit(student_subset) %>%
    as.h2o %>%
    h2o.splitFrame(ratios = c(0.8),  #partition data into 80% and 20% chunks
                   destination_frames = c("train", "test"),
                   seed = 1234) %>% #setting a seed will guarantee reproducibility
    `names<-`(c("train", "test"))

  y <- "PARINVOL"
  x <- setdiff(names(h2o_data$train), c(y, "STRATUM", "STIDSTD"))  # x is all the cols except price and id
  print(x)


  hyperparams <- list(ntrees = c(1000),
                      max_depth = c(2, 5, 10),
                      learn_rate = c(0.01, 0.005, 0.001))

  gbm_grid <- h2o.grid("gbm",
                       y = y,
                       x = x,
                       training_frame = h2o_data$train,
                       grid_id = "gbm_models",
                       nfolds = 5,
                       hyper_params = hyperparams,
                       min_split_improvement = 1e-04,
                       stopping_rounds = 10,
                       col_sample_rate = 0.5,
                       seed = 1234)

  sorted_grid <- h2o.getGrid(grid_id = "gbm_models", sort_by = "mae")

  best_model <- h2o.getModel(sorted_grid@model_ids[[1]])

  #summary(best_model)

  vi <- h2o.varimp(best_model)

  pp <- h2o.partialPlot(best_model, h2o_data$train, cols = c("MATHEFF", "ANXMAT", "ESCS","OPENPS"))

  h2o.shutdown(prompt = F)

  pp <- lapply(pp, function(x) setDT(x) %>% .[, variable := names(x)[1]]) %>%
    rbindlist %>%
    setnames("MATHEFF", "value")

  ggplot(pp, aes(value, mean_response)) +
    geom_ribbon(aes(ymin = mean_response - (stddev_response / 2),
                    ymax = mean_response + (stddev_response / 2)),
                fill = "lightgrey") +
    geom_line(colour = "blue", size = 1) +
    facet_wrap(~ variable, scale = "free_x") +
    theme_bw()



}
