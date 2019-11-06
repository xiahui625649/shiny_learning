hat.plot <- function(fit){
  p <- length(coefficients(fit))
  n <- length(fitted(fit))
  plot(hatvalues(fit),main = "Index Plot of Hat Values")
  abline(h=c(2,3)*p/n,col="red",lty=2)#观测点的帽子值大于帽子均值的2或3倍，即视为高杠杆点
  identify(1:n,hatvalues(fit),names(hatvalues(fit)))
}
##高于两条红色线即为大于2或3倍的高杠杆点
##生产图片后需要双击哪些点才会显示点的标签