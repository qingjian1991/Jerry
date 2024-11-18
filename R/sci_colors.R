#' Color setting
#'
#' @param n number of colors to select. A total of 36 colors are saved.
#' @param rev reverse output the colors.
#' @param random random select number of colors.
#'
#' @export

sci_colors = function( n = 1 , plot = F){

  message("the total colors list is 14")

  if(n <1 | n >14){
     message("the range of n is in 1 to 14")
  }

  colorlist1 =  c("#FF420E", "#FFBB00", "#4CB5F5", "#89DA59", "#878787", "#B037C4")

  colorlist2 =c('#3cb44b','#1f77b4', '#000075','#ff7f0e','#aa40fc','#d62728','#c49c94')

  colorlist3 = c("#8c510a", "#bf812d", "#dfc27d", "#c7eae5", "#80cdc1", "#35978f", "#01665e")

  colorlist4 =  c('#d4de9c','#94c58f','#86c7b4','#9cd2ed','#a992c0',                '#ea9994','#f2c396','#bb82b1')

  colorlist5 =c("#843C39", "#8C6D31", "#E6550D", "#3182BD","#54990F", "#E41A1C","#E7298A", "#C3BC3F","#FF7F00", "#F1788D")

  colorlist6 =  c("#ea5c6f","#f7905a","#e187cb","#fb948d", "#e2b159","#ebed6f","#b2db87","#7ee7bb", "#64cccf","#a9dce6","#a48cbe","#e4b7d6")

  colorlist7 <- c('#007ABA','#8B58A4','#DF75AE','#00B7CA','#A8C7E9','#E91E25','#925047','#F37121','#FBB36E','#F58D93','#B4BE50','#00A163','#8CCA7C')

  colorlist8 =  c("#223D6C","#D20A13","#FFD121","#088247","#B037C4", "#58CDD9","#7A142C","#5D90BA","#431A3D","#91612D", "#6E568C","#E0367A","#D8D155","#64495D","#7CC767")


  colorlist9 = c("#4E79A7","#A0CBE8","#8CD17D","#499894","#F28E2B", "#FFBE7D","#B6992D","#E15759","#FF9D9A","#79706E",  "#D37295","#FABFD2","#B07AA1","#D4A6C8", "#9D7660","#E58606","#5D69B1","#24796C","#499894", '#DAA51B','#000000','#99C945','#ED645A')

  colorlist10 = c('#8c564b','#e377c2','#b5bd61','#17becf','#aec7e8', '#ffbb78','#98df8a','#ff9896','#c5b0d5','#c49c94','#f7b6d2','#dbdb8d','#9edae5','#aa40fc','#3cb44b',               '#d62728','#1f77b4','#ff7f0e','#000075','#ad494a', '#8c6d31','#000000','#279e68','#ffe119','#808080')

  colorlist11 =c('#cb4936','#ff0000','#5ac1b3','#1616c8','#0D63A5',                       '#A12568','#F499C1','#F7C394','#B2A157','#ade87c','#000000','#03C4A1','#bb4316','#7382BC','#F0E442','#3B185F','#0d6c0d','#FEC260','#FD7014','#a67063','#1B9B8A','#D0EBE7','#713045','#F6E0EA','#AD6D28','#EAB67D','#5ac1b3','#EE4590')


  colorlist12= c("#843C39", "#8C6D31", "#E6550D", "#3182BD", "#54990F", "#BD9E39", "#E7BA52", "#31A354", "#E41A1C", "#6BAED6", "#9ECAE1", "#AD494A", "#E7CB94", "#74C476", "#A1D99B", "#C7E9C0", "#99600F", "#E7298A", "#C3BC3F", "#D6616B", "#FF7F00", "#1B9E77", "#FDAE6B", "#B3823E", "#66A61E", "#F1788D","#C6DBEF","#E6550D","#E7969C")


  colorlist13 <-c('#E5D2DD', '#53A85F', '#F1BB72', '#F3B1A0', '#D6E7A3', '#57C3F3',                '#476D87', '#E95C59', '#E59CC4', '#AB3282', '#23452F', '#BD956A',               '#8C549C', '#585658', '#9FA3A8', '#E0D4CA', '#5F3D69', '#C5DEBA',               '#58A4C3', '#E4C755', '#F7F398', '#AA9A59', '#E63863', '#E39A35',  '#C1E6F3', '#6778AE', '#91D0BE', '#B53E2B', '#712820', '#DCC1DD', '#CCE0F5','#CCC9E6','#625D9E','#68A180','#3A6963','#968175')



  colorlist14= c('#4b6aa8','#3ca0cf','#c376a7','#ad98c3','#cea5c7', '#53738c','#a5a9b0','#a78982','#696a6c','#92699e',             '#d69971','#df5734','#6c408e','#ac6894','#d4c2db',             '#537eb7','#83ab8e','#ece399','#405993','#cc7f73',             '#b95055','#d5bb72','#bc9a7f','#e0cfda','#d8a0c0',             '#e6b884','#b05545','#d69a55','#64a776','#cbdaa9',             '#efd2c9','#da6f6d','#ebb1a4','#a44e89','#a9c2cb',             '#b85292','#6d6fa0','#8d689d','#c8c7e1','#d25774',             '#c49abc','#927c9a','#3674a2','#9f8d89','#72567a',   '#63a3b8','#c4daec','#61bada','#b7deea','#e29eaf','#4490c4','#e6e2a3','#de8b36','#c4612f','#9a70a8', '#76a2be','#408444','#c6adb0','#9d3b62','#2d3462')


  colorlist = get(sprintf("colorlist%s", n))

  if(plot){

    print(plot_colors(colorlist))

  }

  return(colorlist)

}


#' plot_colors
#'
#' plot the inputted colors.
#'
#' @param colorlist colorlist to show
#'
#' @export
#'
plot_colors = function(colorlist){

  if(length(colorlist) >60 ){
    stop(sprintf("the number of colors should be less than 60, the input is %s"), length(colorlist))
  }

  df <- data.frame(x=c( rep(1:10, 6) ),
                   y=c(rep(6:1, each = 10)),
                   color = 1:60
  )

  ggplot(df[df$color <= length(colorlist),], aes(x=x, y=y)) +
    geom_point(aes(color=factor(color)), size=8)+#geom_point绘制散点图
    geom_text(aes(label = color)
    ) +
    scale_colour_manual(values = colorlist) + #scale_colour_manual自定义配色
    theme_void() +
    theme(legend.position="none") +
    theme(    legend.position = "none",                    # 图例也不展示
              plot.margin = unit(c(1, 2, 1, 2), "cm")      # 设置上下左右的边距
    )
}

