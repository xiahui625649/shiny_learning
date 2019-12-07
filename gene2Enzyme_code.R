#blast2go_go_table <- read.delim("C:/Users/XH/Desktop/blast2go_go_table.txt",stringsAsFactors = F)
#利用blast2go注释后生成的table文件生成geneName和Enzyme_code的对应关系,
#如需KEGG分析，还需到KEGG官网根据Enzyme_code查找KEGG号。
gene2Enzyme_code <- function(blast2go_go_table){
df <- data.frame(blast2go_go_table$SeqName,blast2go_go_table$Enzyme.Codes)
colnames(df) <- c("seqName","Enzyme_code")
# for (i in 1:4) {
# df[,i] <- as.character(df[,i])
# }
df[which(df$Enzyme_code==""),"Enzyme_code"]=NA
df <- df[complete.cases(df),]
require(tidyverse)
####geneNames
d <- rep(df[1,1],(str_count(df[1,2],";")+1)) %>% as.data.frame()
colnames(d) <- "geneNames"
for (i in 2:nrow(df)) {
  s <- rep(df[i,1],(str_count(df[i,2],";")+1)) %>% as.data.frame()
  colnames(s) <- "geneNames"
  d <- rbind(d,s)
}
####Enzyme_code
s <- df[1,2] %>% str_replace_all(" ","") %>% str_split(";",simplify = T) %>% t() %>% as.data.frame()
for (i in 2:nrow(df)) {
  a <- df[i,2] %>% str_replace_all(" ","") %>% str_split(";",simplify = T) %>% t() %>% as.data.frame()
  s <- rbind(s,a)
}
colnames(s) <- "Enzyme_code"
dat <- data.frame(geneNames=d,Enzyme_code=s)
dat
}

