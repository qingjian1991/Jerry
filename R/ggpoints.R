#' ggpoints
#'
#' plot the x and y points using ggplot. The annotations can be correlation coefficient and linear regression function.
#'
#'
#'
#' @param data: data frame: if non, using x and y to plot.
#' @param x: x
#' @param y: y
#' @param annotations: annotations text in the plot.
#' @param linetypes: add lines into the plot.
#'
#' @param label.x.cor: label.x.cor of cor annotation text
#' @param label.y.cor: label.y.cor of cor annotation text
#'
#' @param label.x.npc: label.x.npc of lm annotation text
#' @param label.y.npc: label.x.npc of lm annotation text
#'
#' @details
#' The types of annotation:
#' @details 3. cor: add cor.test of R and p-value.
#' @details 4. lm: lm information, y ~ x and R^2.
#'
#'The types of linetypes:
#' @details 1. xy.line: add y = x dashed lines.
#' @details 2. lm.line: add y ~ x lines.
#'
#' @import ggplot2
#' @import ggpmisc
#'
#' @examples
#' #data
#' set.seed(100)
#' x = rnorm(20)
#' y = x*1.1 + 1 + rnorm(20, mean = 0.01)
#' data = data.frame(x = x, y = y)
#'
#' #plot all data
#' ggpoints(x = x, y = y)
#' ggpoints(data = data, x = "x", y = "y")
#'
#' #plot lines
#' ggpoints(x = x, y = y, annotations = c("lm"), linetypes = "lm.line" )
#'
#' #adjust the position of label
#' ggpoints(x = x, y = y, annotations = c("lm"),linetypes = "lm.line", label.x.npc = 0.1, label.y.npc = 0.9)
#'
#'
#' @export

ggpoints = function(data = NULL, x, y, xlab = NULL, ylab = NULL,
                    annotations = c("lm","cor"),
                    linetypes = c("xy.line", "lm.line"),
                    cor.method = "spearman",
                    show.cor.pvalue = TRUE,
                    label.x.cor = 0.01, label.y.cor = 0.9,
                    label.x.npc = "right", label.y.npc = "top",
                    point.pch = 21,
                    point.size = 2,
                    point.fill = "#3b518be5",
                    point.col = "#3b518be5",
                    xy.line.col = "grey50",
                    lm.line.col = 'blue',
                    lm.line.size = 1.1
                    ){

  if(is.null(data)){
    data = data.frame(
      x = x,
      y = y
    )
  }else{
    data$x = data[,x]
    data$y = data[,y]
  }

  annotate_text = data |>
    filter( !(is.na(x) | is.na(y)) ) |>
    summarise(p.value = (cor.test(x, y, method = cor.method)$p.value ),
              rho = (cor.test(x, y, method = cor.method)$estimate ),
              x.pos = min(x)+ ((max(x)-min(x)) *label.x.cor ),
              y.pos = min(y)+ ((max(y)-min(y))*label.y.cor ))

  if(show.cor.pvalue){
    annotate_text = annotate_text |>
      mutate(text = sprintf('R[sp]~"="~%.2f~","~italic(P)~"="~%.1e', rho, p.value  ) )
  }else{
    annotate_text = annotate_text |>
      mutate(text = sprintf('R[sp] == %.2f', rho  ) )
  }


  p1 = data %>%
    ggplot(aes(x = x, y = y ) ) + ggpubr::theme_pubr() +
    geom_point(pch=point.pch , fill= point.fill,
               size= point.size, color = point.col )+
    labs(x = xlab, y = ylab)

  if("xy.line" %in% linetypes){
    p1 = p1+
      geom_abline(slope = 1, intercept = 0, linetype = 2,
                  size = 1.1, col = xy.line.col )
  }

  if("lm.line" %in% linetypes){
    p1 = p1 +
      geom_smooth(method = "glm", formula = "y ~ x", se = F,
                  col= lm.line.col, linetype = 2, size = lm.line.size)
  }

  if("cor" %in% annotations){
    p1 = p1+
      geom_text(aes(x=x.pos, y= y.pos, label = text) , hjust = 0, vjust = 1, data= annotate_text, parse = T )
  }

  if( "lm" %in% annotations){
    formula <- y ~ poly(x, 1, raw = TRUE)
    p1 = p1 +
      stat_poly_eq(use_label(c("eq", "adj.R2")) ,
                   label.x.npc = label.x.npc, label.y.npc = label.y.npc,
                   formula = formula, parse = TRUE, size = 4)

    p1

  }

  p1

}

