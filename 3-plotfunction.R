draw_pca = function(exp,group){
  dat=as.data.frame(t(exp))
  if(!require(FactoMineR))install.packages("FactoMineR")
  if(!require(factoextra))install.packages("factoextra")
  library(FactoMineR)
  library(factoextra) 
  dat.pca <- PCA(dat, graph = FALSE)
  fviz_pca_ind(dat.pca,
               geom.ind = "point", # show points only (nbut not "text")
               col.ind = group_list, # color by groups
               addEllipses = TRUE, # Concentration ellipses
               legend.title = "Groups")
}

draw_heatmap = function(n,group_list){
  n = log2(n+1)
  n = t(scale(t(n)))
  n[n>4] = 4
  n[n< -4] = -4
  annotation_col=data.frame(group=group_list)
  rownames(annotation_col)=colnames(n) 
  library(pheatmap)
  library(ggplotify)
  as.ggplot(pheatmap(n,
                     show_colnames =F,
                     show_rownames = F,
                     # scale = "row",
                     annotation_col=annotation_col,
                     legend = F,
                     annotation_legend = F,
                     annotation_names_col = F
                    
  ))
}




draw_volcano <- function(test,pvalue_cutoff = 0.05,logFC_cutoff= 1,pkg = 1){
  this_tile <- paste0(nrow(test[test$change =='DOWN',]),
                      'down gene,',
                      nrow(test[test$change =='UP',]),
                      'up gene'
                      )
  lab = c("DESeq2","edgeR","limma(voom)","limma")[pkg]
  if(!require(ggplot2))install.packages("ggplot2")
  colnames(test)[1:2]=c("logFC","P.value")
  #logFC_cutoff <- with(test,mean(abs(logFC)) + 2*sd(abs(logFC)) )
  ggplot(data = test, 
         aes(x = logFC, 
             y = -log10(P.value))) +
    geom_point(alpha=0.4, size=3.5, 
               aes(color=change)) +
    scale_color_manual(values=c("blue", "grey","red"))+
    geom_vline(xintercept=c(-logFC_cutoff,logFC_cutoff),lty=4,col="black",lwd=0.8) +
    geom_hline(yintercept = -log10(pvalue_cutoff),lty=4,col="black",lwd=0.8) +
    theme_bw()+
    labs(title=this_tile , x=lab, y="")+
    theme(plot.title = element_text(hjust = 0.5))
    
}

venn <- function(x,y,z,name){
  if(!require(VennDiagram))install.packages('VennDiagram')
  library (VennDiagram)
  venn.diagram(x= list(Deseq2 = x,edgeR = y,limma = z),
               #imagetype ="tiff",
               filename=NULL,
               lwd=1,#圈线粗度
               lty=1, #圈线类型
               col=c('#0099CC','#FF6666','#FFCC99'), #圈线颜色
               fill=c('#0099CC','#FF6666','#FFCC99'), #填充颜色
               cat.col=c('#0099CC','#FF6666','#FFCC99'),#A和B的颜色
               cat.cex = 1.5,# A和B的大小
               rotation.degree = 0,#旋转角度
               main = name,#主标题内容
               main.cex = 1.5,#主标题大小
               cex=1.5,#里面交集字的大小
               alpha = 0.5,#透明度
               reverse=TRUE)
}


