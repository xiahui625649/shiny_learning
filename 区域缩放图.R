#http://houyun.xyz/post/2019/12/22/ggcor/
#
just_tools <- function(x, y, just){
  rl <- just[1]
  bt <- just[2]
  if(!(rl %in% c('left', 'right')))
    stop("'just[1]' should be 'left' or 'right'.", call. = FALSE)
  if(!(bt %in% c('bottom', 'top')))
    stop("'just[2]' should be 'bottom' or 'top'.", call. = FALSE)
  if(rl == 'left'){
    x <- x
  }else{
    x <- grid::unit(1, 'npc') - x
  }
  if(bt == 'bottom'){
    y <- y
  }else{
    y <- grid::unit(1, 'npc') - y
  }
  invisible(list(x = x, y = y))
}
draw_gg <- function(gg, # “ggplot”图形对象
                    x = grid::unit(4, "mm"), # 偏离定位坐标点的宽度
                    y = grid::unit(4, "mm"), # 偏离定位坐标点的长度
                    just = c('left', 'bottom'), # 定位坐标点
                    box = FALSE, # 缩放小图是否有边框
                    space = NULL, # 边框和缩放小图的空白
                    #（上下左右是均是space的一半，默认4mm
                    width = NULL, # 缩放小图宽度，须是units类
                    height = NULL, # 缩放小图高度，须是units类
                    box_lty = NULL, # 边框线型类型
                    box_lwd = NULL, # 边框线宽度
                    box_lcol = NULL, # 边框线颜色
                    box_fill = NULL # 边框矩形填充色
){
  if(!is.ggplot(gg))
    stop("'gg' is not a 'ggplot' object.", call. = FALSE)
  xy <- just_tools(x = x, y = y, just = just)
  xx <- xy[[1]]
  yy <- xy[[2]]
  if(is.null(width))
    width = grid::widthDetails(gg)
  if(is.null(height))
    height = grid::heightDetails(gg)
  if(box){
    
    if(is.null(box_lty)) box_lty <- grid::get.gpar('lty')[[1]]
    if(is.null(box_lwd)) box_lwd <- grid::get.gpar('lwd')[[1]]
    if(is.null(box_lcol)) box_lcol <- grid::get.gpar('col')[[1]]
    if(is.null(box_fill)) box_fill <- grid::get.gpar('fill')[[1]]
    if(is.null(space)) space <- unit(4, 'mm')
    grid::pushViewport(
      grid::viewport(x = xx,
                     y = yy,
                     width = width + space,
                     height = height + space,
                     just = just,
                     name = 'gg-box'
      ))
    
    grid::grid.rect(
      gp = gpar(
        lty = box_lty,
        lwd = box_lwd,
        col = box_lcol,
        fill = box_fill
      )
    )
    grid::upViewport()
  }
  
  
  if(box){
    ggxy <- just_tools(x = x + 1/2 * space, y = y + 1/2 * space, just = just)
    xx <- ggxy[[1]]
    yy <- ggxy[[2]]
  }
  grid::pushViewport(
    grid::viewport(x = xx,
                   y = yy,
                   width = width,
                   height = height,
                   just = just,
                   name = 'gg-draw'
    ))
  print(gg, newpage = FALSE)
  grid::upViewport()
  
}
#用grid::downViewport('panel.7-5-7-5')定位到了ggplot的绘图区，不同的绘图函数绘图区的名字不一样，需要自己去找。
#找的技巧是在grid::current.vpTree()函数返回值中查看，绘图区视图以panel开头，后面接一串数字