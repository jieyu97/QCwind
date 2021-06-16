#' @name eqm_bc_5foldcv_simple
#' @title Calibrating the bias of observed data by empirical quantile
#' mapping method with 5-fold cross-validation
#' @description Performing empirical quantile distribution mapping to correct bias in the observation data,
#' applying the 5-fold cross-validation;
#' the "truth" quantiles are given for a reference.
#' @param data.obs a numeric vector of observation data needs to be corrected.
#' @param data.datetime a sequence of timestamps in `data.obs`.
#' @param true.quantiles a list of six numeric vectors of the "truth" quantiles to build the
#' bias correction model, the corresponding cumulative probability stamps must be evenly spreaded
#' between 0 and 1. If not satisfied,
#' a pseudo "truth" observation numeric vector satisfying the quantiles should be provided.
#' We suggest using quantiles with cumulative probability from 0.01 to 1 with interval length 0.01
#' (100 quantiles in total).
#' We sort these quantiles to avoid crossovers.
#' @return a list of two parts: one is the output dataframe `bc_output` that includes both
#' input and bias-corrected observation data; the other one is a large list `bc.eqm_model`
#' contains all the empirical quantile mapping models in each turn of cross-validation
#' during different season/period.
#' @import stats
#' @import tidyverse
#' @importFrom dplyr tibble %>%
#' @importFrom lubridate month hour is.POSIXct
#' @export

eqm_bc_5foldcv_simple = function(data.obs, data.datetime, true.quantiles)
{
  stopifnot(length(data.obs) == length(data.datetime))
  stopifnot(is.numeric(data.obs))
  stopifnot(is.POSIXct(data.datetime))

  ## split datetime by season and by day/night time
  data.datetime_split = tibble(datetime = data.datetime,
                               month = month(data.datetime),
                               hour = hour(data.datetime),
                               season = 'summer', # summer: 6,7,8,9; winter: 12,1,2,3; trans: 4,5,10,11;
                               daytime = 'night', # day: 7--18; night: 19--6;
                               period = 'day_s',
                               cv_fold = NA)
  data.datetime_split$season[ which( data.datetime_split$month %in% c(12,1,2,3) ) ] = 'winter'
  data.datetime_split$season[ which( data.datetime_split$month %in% c(4,5,10,11) ) ] = 'trans'
  data.datetime_split$daytime[ which( data.datetime_split$hour %in% c(7:18) ) ] = 'day'
  data.datetime_split$period = paste0(data.datetime_split$season,'_',data.datetime_split$daytime)

  valid.data_label = which(!is.na(data.obs))
  # zero.data_label = which(data.obs == 0)
  split_6period = c("winter_night","winter_day","trans_night","trans_day","summer_night","summer_day")
  for (period in 1:6) {
    each.period_label = which(data.datetime_split$period == split_6period[period])
    each.period.valid_label = intersect(valid.data_label, each.period_label)
    cv.fold.length = round(length(each.period.valid_label) / 5)
    each.period.valid.fold1_label = each.period.valid_label[1:cv.fold.length]
    each.period.valid.fold2_label = each.period.valid_label[(cv.fold.length+1):(2*cv.fold.length)]
    each.period.valid.fold3_label = each.period.valid_label[(2*cv.fold.length+1):(3*cv.fold.length)]
    each.period.valid.fold4_label = each.period.valid_label[(3*cv.fold.length+1):(4*cv.fold.length)]
    each.period.valid.fold5_label = each.period.valid_label[(4*cv.fold.length+1):length(each.period.valid_label)]
    data.datetime_split$cv_fold[each.period.valid.fold1_label] = paste0(split_6period[period],"_","cvfold1")
    data.datetime_split$cv_fold[each.period.valid.fold2_label] = paste0(split_6period[period],"_","cvfold2")
    data.datetime_split$cv_fold[each.period.valid.fold3_label] = paste0(split_6period[period],"_","cvfold3")
    data.datetime_split$cv_fold[each.period.valid.fold4_label] = paste0(split_6period[period],"_","cvfold4")
    data.datetime_split$cv_fold[each.period.valid.fold5_label] = paste0(split_6period[period],"_","cvfold5")
  }

  bc_output = data.datetime_split %>%
    dplyr::mutate(obs_before_BC = data.obs, data_after_BC = NA) %>%
    dplyr::select(datetime, period, cv_fold, obs_before_BC, data_after_BC)

  bc.eqm_model = list()
  for (period in 1:6) {
    estimate_true_quantile = true.quantiles[[period]]
    sorted_quantile = sort(estimate_true_quantile)

    percentiles = seq(0,1,0.01)
    percentiles_value = c(0,sorted_quantile)
    # kriging_smooth_weibull = fitdist(estimate_true_quantile,'weibull',method = "mle")
    for (cv in 1:5) {
      training.class = paste0(split_6period[period],"_","cvfold",setdiff(1:5,cv))
      validation.class = paste0(split_6period[period],"_","cvfold",cv)
      training.label = which(bc_output$cv_fold %in% training.class)
      validation.label = which(bc_output$cv_fold == validation.class)
      training.data = bc_output$obs_before_BC[training.label]
      validation.data = bc_output$obs_before_BC[validation.label]

      model.number = 5*(period-1)+cv
      if (length(training.data) > 1000) {
        ecdf_training.data = ecdf(training.data)
        cumu.prob_validation.data = ecdf_training.data(validation.data)

        # piecewise linear function based on percentiles and percentiles_values, as the true cdf:
        quantiles_validation.data = unlist( lapply(cumu.prob_validation.data, function(x){
          if (is.na(x)) {
            y = NA
          } else if (x %in% percentiles) {
            label = which(percentiles == x)
            y = percentiles_value[label]
          } else {
            label2 = which(abs(percentiles - x) < 0.01)
            label2 = sort(label2)
            x1 = percentiles[label2[1]]
            y1 = percentiles_value[label2[1]]
            y2 = percentiles_value[label2[2]]
            y = y1 + (x - x1) * (y2 - y1) / 0.01
          }
          return(y)
        }) )

        bc.eqm_model[[model.number]] = quantiles_validation.data
        attr(bc.eqm_model[[model.number]], "cross_validation_detail") = paste0("training data: ",training.class[1]," + ",
                                                                               training.class[2]," + ",training.class[3]," + ",
                                                                               training.class[4],"; validation data: ",
                                                                               validation.class)

        bc_output$data_after_BC[validation.label] = quantiles_validation.data
      } else {
        bc.eqm_model[[model.number]] = "not enough training data"
        attr(bc.eqm_model[[model.number]], "cross_validation_detail") = paste0("training data: ",training.class[1]," + ",
                                                                               training.class[2]," + ",training.class[3]," + ",
                                                                               training.class[4],"; validation data: ",
                                                                               validation.class)

        bc_output$data_after_BC[validation.label] = NA
      }
    }
  }

  # bc_output$data_after_BC[zero.data_label] = 0

  return(list(bc_output, bc.eqm_model))
}
