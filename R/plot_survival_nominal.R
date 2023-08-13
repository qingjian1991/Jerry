#' plot_survival_nominal
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
#'
#' @examples
#' library(survival)
#' library(ggsurvfit)
#' library(gtsummary)
#' library(ezcox)
#'
#' head(lung )
#' plot_survival_nominal(lung, variables = "ph.ecog")
#'
#' @export


plot_survival_nominal = function(data,
                                 variables,
                                 time = "time",
                                 status = "status",
                                 palette = NULL,
                                 return.data = TRUE,
                                 ...
){
  if(is.null(palette)){
    palette = set.colors(8)
  }


  #add data
  data$time <- data[[time]]
  data$status <- data[[status]]
  data <- dplyr::filter(data, !is.na(.data$time), !is.na(.data$status))

  if( !all(variables %in% colnames(data))){
    stop("checking the inputed variable")
  }


  fm = lapply(variables, function(x) as.formula(sprintf("Surv(time, status) ~ %s", x) )
  )

  fit = lapply(fm, function(x) survminer::surv_fit(x, data = data ) )

  names(fit) = variables

  plt = lapply(fit, function(x){
    survminer::ggsurvplot(x ,
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




