#blast2go_go_table <- read.delim("C:/Users/XH/Desktop/blast2go_go_table.txt",stringsAsFactors = F)
#利用blast2go注释后生成的table文件生成geneName和Go_id、Go_term的对应关系
gene2Go_id2Go_term <- function(blast2go_go_table){
df <- data.frame(blast2go_go_table$SeqName,blast2go_go_table$GO.IDs,blast2go_go_table$GO.Names,blast2go_go_table$Enzyme.Codes,blast2go_go_table$X.GO)
colnames(df) <- c("seqName","GO_id","Go_name","Enzyme_code","hits")
# for (i in 1:4) {
# df[,i] <- as.character(df[,i])
# }
require(tidyverse)
####Go_id
dat <- df[1,2] %>% str_replace_all(" ","") %>% str_split(";",simplify = T) %>% t()
colnames(dat) <- "GO_id"
for (i in 2:nrow(df)) {
  dt <- df[i,2] %>% str_replace_all(" ","") %>% str_split(";",simplify = T) %>% t()
  dat <- rbind(dat,dt)#第二行开始最前面有个空格字符，需删除空格
}

dat[1] <- dat[1] %>% str_sub(3,nchar(dat[1]))
for (i in 2:nrow(dat)) {
  dat[i] <- dat[i] %>% str_sub(3,nchar(dat[i]))
}
dat <- as.data.frame(dat)
####geneNames
d <- rep(df[1,1],df$hits[1]) %>% as.data.frame()
colnames(d) <- "geneNames"
for (i in 2:nrow(df)) {
  s <- rep(df[i,1],df$hits[i]) %>% as.data.frame()
  colnames(s) <- "geneNames"
  d <- rbind(d,s)
}
dat$geneNames <- d
####Go_term
f <- df[1,3] %>% str_split(";",simplify = T) %>% 
  t() %>% str_replace(pattern = " ",replacement = "") %>% str_sub(3,nchar(as.character(df[1,3]))) %>% as.data.frame()
colnames(f) <- "Go_term"
for (i in 2:nrow(df)) {
  e <- df[i,3] %>% str_split(";",simplify = T) %>% 
    t() %>% str_replace(pattern = "^ ",replacement = "") %>% str_sub(3,nchar(as.character(df[i,3]))) %>% as.data.frame()
  colnames(e) <- "Go_term"
  f <- rbind(f,e)
}
dat <- data.frame(geneNames=dat$geneNames,Go_id=dat$GO_id,Go_term=f)
dat
}
