###Tpm
countToTpm <- function(counts, effLength){
  rate <- log(counts)-log(effLength)
  denom <- log(sum(exp(rate)))
  exp(rate-denom+log(1e6))
}
###Fpkm
countToFpkm <- function(counts,effLength){
  N <- sum(counts)
  exp(log(counts)+log(1e9)-log(effLength)-log(N))
}
###fpkm to Tpm
fpkmToTpm <- function(fpkm){
  exp(log(fpkm)-log(sum(fpkm))+log(1e6))
}
##Effcounts
countToEffCounts <- function(counts,len,effLength){
  counts*(len/effLength)
}
##an example##
# cnts <- c(4250,3300,200,1750,50,0)
# lens <- c(900,1020,2000,770,3000,1777)
# countDf <- data.frame(count=cnts,length=lens)
# ###assume a mean(FLD)=203.7
# countDf$effLength <- countDf$length-203.7+1
# countDf$Tpm <- with(countDf,countToTpm(count,effLength))
# countDf$fpkm <- with(countDf,countToFpkm(count,effLength))
# with(countDf,all.equal(Tpm,fpkmToTpm(fpkm)))#验证fpkmToTpm函数
# countDf$effCounts <- with(countDf,countToEffCounts(count,length,effLength))

