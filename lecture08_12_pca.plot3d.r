pca_plot_3d <- function(
  eigenvector,
  eigenvalue,
  group,
  key,
  outdir,
  xyz = "xyz",
  width = 8,
  height = 6,
  angle = 45,
  inset = c(-0.1,0),
  mar = c(3.5,3.5,3.5,6),
  legend.pos = "topright",
  horiz = F,
  cex.symbols = 1.5,
  cex.axis = 1,
  cex.lab = 1.5,
  cex.leg = 1.5,
  cex.names = 0.3,
  font.axis = 2,
  font.lab = 2,
  scale.y = 1,
  pch = 19,
  box = T,
  type = "p",
  use_grid = T,
  add_label = F
){
  library(scatterplot3d)
  library(grDevices)
  setwd(outdir)
  gg_color_hue <- function(n) {
    hues = seq(15, 375, length = n + 1)
    hcl(h = hues, l = 65, c = 100)[1:n]
  }
  egv <- read.table(eigenvector, header = T, stringsAsFactors = F)
  colnames(egv)[1] <- "sample"
  colnames(egv)[2:ncol(egv)] <- paste0("PC", 1:(ncol(egv) - 1))
  eva <- read.table(eigenvalue, header = T, stringsAsFactors = F)
  names(eva)[1] <- "PCs"
  eva$PCs <- paste0("PC", 1:nrow(eva))
  pop <- read.table(group, header = T, stringsAsFactors = F, check.names = F, comment.char = "")
  egv_order <- egv[match(pop[,3], egv[,1]),]
  pop_order <- pop[match(egv_order[,1], pop[,3]),]
  egv_order$group <- pop_order[,4]
  egv_order$group <- as.factor(egv_order$group)
  if(ncol(pop_order) == 5 | ncol(pop_order) == 6){
    egv_order$color <- pop_order[,5]
    col_df <- unique(pop_order[,4:5])
    col_df <- col_df[order(col_df[,1]),]
  } else if(ncol(pop_order) == 4){
    g_name <- unique(pop_order[,4])
    g_name <- sort(g_name)
    use_col <- gg_color_hue(length(g_name))
    col_df <- data.frame(group = g_name, color = use_col, stringsAsFactors = F)
    col_match <- match(egv_order$group, col_df$group)
    egv_order$color <- col_df[col_match, 2]
  }
  if(xyz == "xyz"){
    xdata <- egv_order$PC1
    ydata <- egv_order$PC3
    zdata <- egv_order$PC2
    x_lab <- paste0("PC1 (", round(eva[eva$PCs == "PC1", 3], 2),"%)")
    y_lab <- paste0("PC3 (", round(eva[eva$PCs == "PC3", 3], 2),"%)")
    z_lab <- paste0("PC2 (", round(eva[eva$PCs == "PC2", 3], 2),"%)")
  }
  if(xyz == "xzy"){
    xdata <- egv_order$PC1
    ydata <- egv_order$PC2
    zdata <- egv_order$PC3
    x_lab <- paste0("PC1 (", round(eva[eva$PCs == "PC1", 3], 2),"%)")
    y_lab <- paste0("PC2 (", round(eva[eva$PCs == "PC2", 3], 2),"%)")
    z_lab <- paste0("PC3 (", round(eva[eva$PCs == "PC3", 3], 2),"%)")
  }
  if(pch == "table"){
    pch <- pop_order[,6]
    pop_col_pch <- unique(pop_order[,4:6])
    pop_col_pch <- pop_col_pch[match(col_df[,1], pop_col_pch[,1]),]
    uniq_pch <- pop_col_pch[,3]
  } else {
    pch <- pch
    uniq_pch <- pch
  }
  ## pdf
  pdf(paste0(key, ".PCA.3d.pdf"), width = width, height = height)
  s3d <- scatterplot3d(x = xdata, y = ydata, z = zdata,
                       xlab=x_lab, ylab=y_lab, zlab=z_lab,
                       pch = pch,
                       angle=angle, mar = mar, type = type, grid = use_grid, box = box,
                       color = egv_order$color, col.axis = "blue",
                       cex.symbols = cex.symbols, cex.axis = cex.axis, cex.lab = cex.lab,
                       font.axis = font.axis, font.lab = font.lab, scale.y = scale.y)
  if(add_label == T){
    text(s3d$xyz.convert(x = xdata, y = ydata, z = zdata), labels = egv_order[,1],
         cex= cex.names, col = "black")
  }
  if(nrow(col_df) > 1){
    legend(legend.pos, legend = col_df[,1], col = col_df[,2], horiz = horiz,
           pch = uniq_pch, cex = cex.leg, xpd=TRUE, inset = inset, bty = "n",
           text.font=2)
  }
  dev.off()
  ## png
  png(paste0(key, ".PCA.3d.png"), width = width, height = height, units = "in", res = 300)
  s3d <- scatterplot3d(x = xdata, y = ydata, z = zdata,
                       xlab=x_lab, ylab=y_lab, zlab=z_lab,
                       pch = pch,
                       angle=angle, mar = mar, type = type, grid = use_grid, box = box,
                       color = egv_order$color, col.axis = "blue",
                       cex.symbols = cex.symbols, cex.axis = cex.axis, cex.lab = cex.lab,
                       font.axis = font.axis, font.lab = font.lab, scale.y = scale.y)
  if(add_label == T){
    text(s3d$xyz.convert(x = xdata, y = ydata, z = zdata), labels = egv_order[,1],
         cex= cex.names, col = "black")
  }
  if(nrow(col_df) > 1){
    legend(legend.pos, legend = col_df[,1], col = col_df[,2], horiz = horiz,
           pch = uniq_pch, cex = cex.leg, xpd=TRUE, inset = inset, bty = "n",
           text.font=2)
  }
  dev.off()
}
