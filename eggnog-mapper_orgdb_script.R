
# 读取eggnog-mapper注释后生成的原始数据 -----------------------------------------------
setwd("D:/eggnog_mapper")
#此结果为eggnog-mapper网页在线工具生成http://eggnogdb.embl.de/#/app/emapper
egg_f <- "Lchi_pep.fas.emapper.annotations"
egg <- read.csv(egg_f, sep = "\t",header = F)
#参考徐州更简书自定义的表头https://www.jianshu.com/p/e646c0fa6443
colnames(egg) <- c("query_name","sedd_eggNOG_ortholog","seed_orholog_evalue","seed_ortolog_score",
                   "predicted_gene_name","GO_terms","KEGG_KOs","BiGG_Reactions","Annotation_tax_scope",
                   "Matching_OGs","best_OG|evalue|score","COG functional categories","eggNOG_HMM_model_annotation")
#eggnog-mapper软件本地下机结果数据表头
# colnames(egg) <- c("query_name","seed_eggNOG_ortholog","seed_ortholog_evalue",
#                    "seed_ortholog_score","predicted_gene_name","GO_terms	KEGG_KOs",
#                    "BiGG_reactions","Annotation_tax_scope","OGs","bestOG|evalue|score","COG cat","eggNOG annot")

# 处理eggnog-mapper注释之后的数据 --------------------------------------------------


egg[egg==""]<-NA #(将空行变成NA，方便下面的去除)

#eggNOG_HMM_model_annotation为最后一列，软件下机数据为eggNOG annot
gene_info <- egg %>%
  dplyr::select(GID = query_name, GENENAME = `eggNOG_HMM_model_annotation`) %>% na.omit()

gterms <- egg %>%
  dplyr::select(query_name, GO_terms) %>% na.omit()

gene2go <- data.frame(GID = character(),
                      GO = character(),
                      EVIDENCE = character())
df_temp <- list()
for (row in 1:nrow(gterms)) {
  the_gid <- gterms[row, "query_name"][[1]]
  the_gos <- str_split(gterms[row,"GO_terms"], ",", simplify = FALSE)[[1]]
  
  df_temp[[row]] <- tibble(GID = rep(the_gid, length(the_gos)),
                           GO = the_gos,
                           EVIDENCE = rep("IEA", length(the_gos)))
}
gene2go <- bind_rows(df_temp) %>% .[!duplicated(.),]

# extract kegg pathway annotation from emapper ----------------------------
kos <- egg %>%
  dplyr::select(query_name, KEGG_KOs) %>%
  na.omit()

gene2ko = data.frame(GID = character(),
                     Ko = character())

df_temp <- list()
for (row in 1:nrow(kos)) {
  the_gid <- kos[row, "query_name"][[1]]
  the_kos <- str_split(kos[row,"KEGG_KOs"], ",", simplify = FALSE)[[1]]
  
  df_temp[[row]] <- data_frame(GID = rep(the_gid, length(the_kos)),
                               Ko = the_kos)
}
gene2ko <- bind_rows(df_temp) %>% .[!duplicated(.),]


# 下载 json文件（2019），这是kegg官网下载的ko信息，时间长了需更新 ---------------------------------

  # 需要下载 json文件(这是是经常更新的)
  # https://www.genome.jp/kegg-bin/get_htext?ko00001
  # 代码来自：http://www.genek.tv/course/225/task/4861/show
  library(jsonlite)
  library(purrr)
  library(RCurl)
  
  update_kegg <- function(json = "ko00001.json") {
    pathway2name <- tibble(Pathway = character(), Name = character())
    ko2pathway <- tibble(Ko = character(), Pathway = character())
    
    kegg <- fromJSON(json)
    
    for (a in seq_along(kegg[["children"]][["children"]])) {
      A <- kegg[["children"]][["name"]][[a]]
      
      for (b in seq_along(kegg[["children"]][["children"]][[a]][["children"]])) {
        B <- kegg[["children"]][["children"]][[a]][["name"]][[b]] 
        
        for (c in seq_along(kegg[["children"]][["children"]][[a]][["children"]][[b]][["children"]])) {
          pathway_info <- kegg[["children"]][["children"]][[a]][["children"]][[b]][["name"]][[c]]
          
          pathway_id <- str_match(pathway_info, "ko[0-9]{5}")[1]
          pathway_name <- str_replace(pathway_info, " \\[PATH:ko[0-9]{5}\\]", "") %>% str_replace("[0-9]{5} ", "")
          pathway2name <- rbind(pathway2name, tibble(Pathway = pathway_id, Name = pathway_name))
          
          kos_info <- kegg[["children"]][["children"]][[a]][["children"]][[b]][["children"]][[c]][["name"]]
          
          kos <- str_match(kos_info, "K[0-9]*")[,1]
          
          ko2pathway <- rbind(ko2pathway, tibble(Ko = kos, Pathway = rep(pathway_id, length(kos))))
        }
      }
    }
    
    save(pathway2name, ko2pathway, file = "kegg_info.RData")
  }
  
  update_kegg(json = "ko00001.json")
  
###gene2pathway这里出现了重复的行，故需删除重复行,若最后不能生成org.db可能是之前的数据有重复行
load(file = "kegg_info.RData")
gene2pathway <- gene2ko %>% left_join(ko2pathway, by = "Ko") %>% 
  dplyr::select(GID, Pathway) %>% .[!duplicated(.),] %>%
  na.omit()


# 制作org.db ----------------------------------------------------------------

tax_id = "3414"
genus = "Liriodendron" 
species = "Liriodendron chinense (Hemsl.) Sarg."
require(makeOrgPackage)
makeOrgPackage(gene_info=gene_info,
               go=gene2go,
               ko=gene2ko,
               pathway=gene2pathway,
               version="0.0.1",
               outputDir = ".",
               tax_id=tax_id,
               maintainer = "Xia Hui <xiahui625649@163.com>",
               author = "Xia Hui <xiahui625649@163.com>",
               genus=genus,
               species=species,
               goTable="go")
###之后会在工作目录下生成一个.sqlite文件,这就是orgdb的本质数据
Lchi.orgdb <- AnnotationDbi::loadDb("org.LLiriodendron chinense (Hemsl.) Sarg..eg.sqlite")

keys(Lchi.orgdb) %>% length()#基因总个数
columns(Lchi.orgdb)#列名
keys(Lchi.orgdb,keytype = c("GID")) %>% head()#查看基因ID的前6个
select(Lchi.orgdb,keys = keys(Lchi.orgdb,keytype = "GID")[1:6],
       columns = c("GID","GO","Ko","Pathway"),keytype = 'GID')#查看基因GID前六个对应的其他ID信息


# GO富集分析 ------------------------------------------------------------------

geneid <- read.table("CAD_confirmed_idlist.txt",header = F) 
colnames(geneid) <- "GID"
ego <- enrichGO(gene = geneid$GID,
                #模式物种
                #OrgDb = org.Mm.eg.db,
                #非模式物种，例如芝麻
                keyType = "GID",
                OrgDb = Lchi.orgdb,
                ont = "BP", #或MF或CC
                pAdjustMethod = "BH",
                #pvalueCutoff  = 0.01,
                qvalueCutoff  = 0.01)
barplot(ego)
dotplot(ego)
