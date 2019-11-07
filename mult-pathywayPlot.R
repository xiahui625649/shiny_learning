df <- read.csv("趋势分析与GO富集分析.csv",header = T,sep = ",")
fix(df)
library(ggplot2)
df1 <- df[1:10,]
df2 <- df[11:20,]
df3 <- df[21:30,]
df4 <- df[31:40,]
p_plot <- function(df){
p1 <- ggplot(data = df1,aes(rich.factor,Pathway))+
  geom_point(aes(size=count,color=p.adjust))
p1+scale_colour_gradient(low="red",high="green")+
  labs(color="qvalue",
       size="Gene number",x="Rich factor",y="Pathway name",title="Top10 of pathway enrichment")+
  theme_bw()+
  scale_y_discrete(limits=c(as.character(df1$Pathway)))
}
p1 <- p_plot(df1)
p1
ggsave("profile2.pdf",width = 5,height = 4)
p2 <- p_plot(df2)
p2
ggsave("profile5.pdf",width = 5,height = 4)
p3 <- p_plot(df3)
p3
ggsave("profile6.pdf",width = 5,height = 4)
p4 <- p_plot(df4)
p4
ggsave("profile7.pdf",width = 5,height = 4)
cowplot::plot_grid(p1,p2,p3,p4,labels = c("profile2","profile5","profile6","profile7"))
ggsave("profile.pdf",width = 12,height = 10)
