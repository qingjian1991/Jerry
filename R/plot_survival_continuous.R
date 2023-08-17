#' plot_survival_continuous
#'
#' Plot survival for continuous variables. The continuous variables were divided into high and low groups by setting methods including "median", "mean" or "best".
#'
#' @param data the input data.
#' @param variables the variable to be explored.
#' @param cutoff cutoff methods. cutoff methods is setting either "median", "mean", "best" or the certain values. When the variables is numerical, we divide the variables into two groups according to the inputed values.
#' @param minprop Setting minprop values if cutoff methods is "best"
#' @param time columns names specifying time.
#' @param status columns names specifying status.
#' @param palette colors for different groups.
#' @param return.data whether return the data frame, Default is FALSE.
#' @param ... parameters from survminer::ggsurvplot() to control the styles of figures
#'
#' @import survminer
#' @import survival
#'
#'
#' @examples
#' library(survival)
#' library(ggsurvfit)
#' library(gtsummary)
#' library(ezcox)
#'
#' head(lung )
#'
#' #get HR values.
#' zz = lung |>
#'   mutate(
#'     status = recode(status, `1` = 0, `2` = 1),
#'     time = time
#'   ) |>
#'   ezcox(
#'     covariates = c("pat.karno", "meal.cal", "wt.loss"),
#'     controls = c("age", "sex"),
#'     time = "time",
#'     status = "status",
#'     return_models = TRUE
#'   )
#'
#' mds = get_models(zz)
#'
#' show_models(mds, drop_controls = TRUE,  merge_models = T)
#'
#' #Table of summary
#' tbl_regression(mds$`Surv ~ pat.karno + age + sex` , exp = TRUE)
#'
#'
#' #Plot Survival
#' surv.list = plot_survival_continuous(
#' lung |>
#'   mutate(
#'     status = recode(status, `1` = 0, `2` = 1),
#'     time = time
#'   ),
#' variables = c("pat.karno", "age"),
#' time = "time",
#' status = "status",
#' legend.labs = c("High","Low"),
#' cutoff = "best",
#' minprop = 0.2,
#' return.data = T
#' )
#'
#' surv.list$plot.list
#'
#' #The median survival days.
#' gtsummary::tbl_survfit(
#'   surv.list$surv.fit$pat.karno.g,
#'  probs  = 0.5,
#'   label_header = "**Median survival (95% CI)**"
#' )
#'
#' @export


plot_survival_continuous = function(data,
                         variables,
                         cutoff,
                         time = "time",
                         status = "status",
                         palette = NULL,
                         minprop = 0.4,
                         return.data = FALSE,
                         ...
                         ){
  if(is.null(palette)){
    palette = set.colors(8)
  }


  #add data
  data$time <- data[[time]]
  data$status <- data[[status]]
  data <- dplyr::filter(data, !is.na(.data$time), !is.na(.data$status))

  #cutoffs: setting the cutoffs for the continuous variables

  #see data types.
  types = lapply( variables, function(x) is.numeric(data[[x]]) ) %>%
    purrr::reduce(c)

  if(!is.null(cutoff)){

    if(sum(types) > 0 ){

      message("Set cutoffs for numerical variables by the methods: ", cutoff )

      if(cutoff == "mean"){

        if(length(variables[types]) >=2 ){
          cutoff_values = apply(data[, variables[types]] , 2, mean, na.rm = T)
        }else{
          cutoff_values = setNames( mean(data[, variables[types]], na.rm = T) , nm = variables[types])
        }
      }else if(cutoff == "median"){

        if(length(variables[types]) >=2 ){
          cutoff_values = apply(data[, variables[types]] , 2, median, na.rm = T)
        }else{
          cutoff_values = setNames( median(data[, variables[types]], na.rm = T) , nm = variables[types])
        }

      }else if(cutoff == "best"){
        cutoff_values = survminer::surv_cutpoint(
          data,
          time = time,
          event = status,
          variables = variables[types],
          minprop = minprop,
          progressbar = TRUE
        )
        cutoff_values = setNames(cutoff_values$cutpoint[, 1], nm = variables[types] )
      }else if(is.numeric(cutoff)){
        cutoff_values = setNames(cutoff, nm = variables[types])

      }

      #divided variables into two groups.
      for(i in variables[types]){
        data[[paste0(i, ".g")]] = ifelse(data[[i]] > cutoff_values[i], "High","Low")
      }
    }

  }

  variables[types] = str_c(variables[types], ".g")


  fm = lapply(variables, function(x) as.formula(sprintf("Surv(time, status) ~ %s", x) )
              )

  #fm = as.formula(sprintf("Surv(time, status) ~ %s",  str_c(variables, collapse = " + ") ))
  #model <- coxph(fm , data = data)
  #broom::tidy(model, exp = T) %>% print()

  fit = lapply(fm, function(x) survminer::surv_fit(x, data = data ) )

  names(fit) = variables

  plt = lapply(fit,
               function(x){
                 plt.surv = survminer::ggsurvplot(x ,
                                       data = data,
                                       censor = TRUE,
                                       risk.table = T,
                                       risk.table.height = 0.3,
                                       linetype = 1,
                                       pval = TRUE,
                                       palette = palette,
                                       #legend.labs = c("High","Low"),
                                       ylab = "Overall Survival (OS)",
                                       #conf.int = TRUE, # Add confidence interval
                                       ncensor.plot = FALSE,
                                       ...
                 )

                 patchwork::wrap_plots(
                   plt.surv$plot,
                   plt.surv$table,
                   ncol = 1,
                   heights = c(1, 0.25)
                 )


    }
  )

  names(plt) = variables

  if(return.data){
    list(
      data = data,
      surv.fit = fit,
      plot.list = plt
    )
  }else{
    plt
  }

}

