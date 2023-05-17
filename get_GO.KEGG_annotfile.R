#get_GO.KEGG_annotfile函数：输入文件包括egg注释文件和GO基础注释信息；返回list
get_GO.KEGG_annotfile <- function(eggnog_mapper.file,GO_basic_Description.file){
  # eggnog_mapper.file <- "MM_qazoigs5.emapper.annotations (15).tsv"
  #读取eggnog-mapper网页注释结果文件
  egg <- readr::read_tsv(file = eggnog_mapper.file,col_names = T,comment = "#")
  #将—替换为NA
  egg[egg=="-"] <- NA
  
  #GO
  #提取基因与GO的对应
  gterms <- egg %>%
    dplyr::select(query, GOs) %>% na.omit()
  
  # 将gterms的每一行中GO按照逗号分开
  all_go_list <- str_split(gterms$GOs,",")
  gene2go <- data.frame(GID = rep(gterms$query,
                                  times = sapply(all_go_list, length)),
                        GO = unlist(all_go_list))
  #去除重复行
  gene2go <- gene2go[!duplicated(gene2go),c(2,1)]
  #每一列转换为字符型
  for(i in 1:ncol(gene2go)) gene2go[,i] <- as.character(gene2go[,i])
  
  #term2name
  # GO_basic_Description.file <- "GO_basic_Description.txt"
  #读取GO_basic_Description.txt文件
  dt <- data.table::fread(input = GO_basic_Description.file,data.table = F)
  #根据GO匹配注释信息
  term2name1 <- dt[match(gene2go$GO,dt$GO_IDs),c(2,3)]
  term2name1 <- na.omit( term2name1);colnames(term2name1) <- c("GO","NAME")
  #GO号未注释到功能的GID
  geneID <- as.character(gene2go[which(!gene2go$GO%in%term2name1$GO),"GID"])
  gene2go <- gene2go[!gene2go$GID%in%geneID,]
  
  #KEGG
  # 提取基因ID与KEGG的对应
  gene2ko <- egg %>%
    dplyr::select(GID = query, KO = KEGG_ko) %>%
    na.omit()
  # 将gene2ko的每一行中KO按照逗号分开
  all_ko_list <- str_split(gene2ko$KO,",")
  gene2ko <- data.frame(GID = rep(gene2ko$GID,
                                  times = sapply(all_ko_list, length)),
                        KO = unlist(all_ko_list))
  gene2ko$KO=str_replace(gene2ko$KO,"ko:","")
  gene2ko$GID <- as.character(gene2ko$GID)
  
  ##加载拟南芥kegg注释信息，里面包含Ko2name和ko2pathway
  if(file.exists("kegg_info.RData")){
    load("kegg_info.RData")}else{
      stop("Please provide kegg_info.RData")
    }
  
  #pathway2gene
  pathway2gene <- gene2ko[,c(2,1)]
  pathway2gene$ko <- as.character(ko2pathway[match(as.character(pathway2gene$KO),ko2pathway$Ko),"Pathway",drop=T])
  pathway2gene <- na.omit(pathway2gene)
  pathway2gene <- pathway2gene[,c(3,2)]
  
  #pathway2name
  pathway2name <- pathway2gene[!duplicated(pathway2gene$ko),"ko",drop=F]
  pathway2name$pathway <- as.character(Ko2name[match(as.character(pathway2name$ko),Ko2name$Pathway),"Name",drop=T])
  
  #返回term2go和term2name
  return(list(term2gene=gene2go,term2name=term2name1,
              pathway2gene=pathway2gene,pathway2name=pathway2name))
}