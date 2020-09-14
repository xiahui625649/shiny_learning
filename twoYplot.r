#test data
#dat <- data.frame(clone=rep(c("#1","x3"),each=3),
                  time=rep(1:3,2),
                  qpcr=runif(6,1,100),qpcr_sd=runif(6,1,5),
                  rna=runif(6,1,10),rna_sd=runif(6,0,1))
#example
#Z(data = dat,group = "clone",factor = "time",
  leftY = "qpcr",leftY_sd = "qpcr_sd",
  rightY = "rna",rightY_sd = "rna_sd")
#function
Z <- function(data,leftY,leftY_sd,rightY,rightY_sd,group,factor,main=NULL,
         xlab_groupName=NULL,show.legend=TRUE,col_sd="red",legend.position=NULL,
         fillCol=c('orange', 'steelblue'),
         xlab=NULL,ylab=NULL,...){
  clone <- group;time <- factor
  ncol <- length(unique(data[,clone]))
  nrow <- length(unique(data[,time]))
  width <- 1#柱子宽度强制为1
  xgroup1 <- c("group1","group2")
  xgroup2 <- as.character(unique(data[,clone]))
  if(is.null(xlab_groupName)){xlab_groupName <- xgroup2}else{
    xlab_groupName <- xgroup1}
  par(mar=c(5.1,4.1,4.1,4.1))  #图边距
  barplot(height = matrix(c(data[,leftY]), ncol = ncol, nrow = nrow),  # 绘图数据（矩阵）
          width = width,#每个柱子的宽度，包括中间空白的
          # space = space,#间隔，指定每个柱子左边的空白区域的宽度，这个值为一个百分比，默认值为0.2， 实际的间隔 = 0.2 * 1（所有柱子的平均宽度） = 0.2; 这个参数的值和width 参数类似，可以只设置一个值，也可以指定不同的柱子间隔不同
          names.arg = xlab_groupName,  # 柱子名称
          col = rep(fillCol,each=nrow),  # 填充颜色
          border = '#ffffff',   # 轮廓颜色
          xlab = ifelse(is.null(xlab),clone,NULL),  # X轴名称
          ylab = ifelse(is.null(ylab),leftY,NULL),  # Y轴名称
          main = main,  # 主标题
          horiz = FALSE,  # 是否为水平放置
          ylim = c(-1,ceiling(range(dat[,leftY])[2])+0.1*ceiling(range(data[,leftY])[2])), # Y轴取值范围
          legend.text = xlab_groupName,  # 图例文本
          args.legend = list(fill=fillCol,x=ifelse(is.null(legend.position),"topright",legend.position),box.col="transparent"),#图例
          beside = TRUE ,... # 是否平行排列
  )
  #误差线函数
  plot_error <- function(x, y, sd, len = 1, col = "black") {
    len <- len * 0.05
    arrows(x0 = x, y0 = y, x1 = x, y1 = y - sd, col = col, angle = 90, length = len)
    arrows(x0 = x, y0 = y, x1 = x, y1 = y + sd, col = col, angle = 90, length = len)
  }
  #柱子标签
  mtext(text = unique(data[,time])[1],side = 1,at = 1.5)
  mtext(text = unique(data[,time])[2],side = 1,at = 2.5)
  mtext(text = unique(data[,time])[3],side = 1,at = 3.5)
  mtext(text = unique(data[,time])[1],side = 1,at = 5.5)
  mtext(text = unique(data[,time])[2],side = 1,at = 6.5)
  mtext(text = unique(data[,time])[3],side = 1,at = 7.5)
  #添加柱状图的误差线
  plot_error(x = c(seq(1.5,3.5),seq(5.5,7.5)),y = data[,leftY],sd = data[,leftY_sd])
  # 原坐标
  u <- par("usr")
  ymin <- range(data[,rightY])[1]-0.1*ceiling(range(data[,rightY])[1])
  ymax <- range(data[,rightY])[2]+0.1*ceiling(range(data[,rightY])[2])
  # 重新设置y坐标范围
  par(usr=c(u[1:2],as.integer(ymin),ceiling(ymax)))
  
  points(x=c(seq(1.5,3.5),seq(5.5,7.5)),y=data[,rightY],pch=19,col="red")
  
  lines(x=c(seq(1.5,3.5)),y=data[,rightY][1:3],col="red")
  lines(x=c(seq(5.5,7.5)),y=data[,rightY][4:6],col="red")
  
  #添加线的误差线
  plot_error(x = c(seq(1.5,3.5),seq(5.5,7.5)),y = data[,rightY],sd = data[,rightY_sd],col=col_sd)
  
  #添加外围边框
  box()
  #右边y轴
  axis(4)
  mtext(rightY,side = 4,line = 2)
}
