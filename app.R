library(shiny)
library(shinydashboard)
library(magrittr)
library(dashboardthemes)
library(ggplot2)
library(plotly)
library(breedR)
ui <- dashboardPage(
  dashboardHeader(title = "Forestry Data Analysis",titleWidth="100%"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Upload Data", tabName = "a",icon = icon("file-upload")),
      br(),
      menuItem("Data Observation", icon = icon("list-alt"),
               menuSubItem("observed values",tabName = "subitem1"),
               menuSubItem("missing values",tabName = "subitem2"),
               menuSubItem("outlier obversation",tabName = "out"),
               menuSubItem("mean values",tabName = "subitem3"),
               menuSubItem("phenotype heatmap",tabName = "subitem4")
      ),
      br(),
      menuItem("Data Test",icon =icon("bar-chart-o"),
               menuSubItem("Normality test",tabName = "t1"),
               menuSubItem("Homogeneity test for variance",tabName = "t2")),
      br(),
      menuItem("Variation Parameter", tabName = "b",icon = icon("stream")),
      br(),
      menuItem("ANOVA",tabName = "aov",icon = icon("chart-line")),
      br(),
      menuItem("Blup Values",tabName = "bp", icon = icon("th")),
      br(),
      menuItem("Comprehensive Evaluation",tabName = "ce", icon = icon("table")),
      br(),
      menuItem("Excellent families selection", tabName = "efs",icon = icon("hand-pointer"))
    )),
  dashboardBody(tags$audio(src = "http://other.web.nc01.sycdn.kuwo.cn/resource/n3/95/92/2685144667.mp3",loop= -1,type = "audio/mp3", autoplay = T, controls = F),#BGM,loop=-1表示循环播放
    #tags$iframe(src = "https://www.opendrive.com/player/NjVfMzQ3ODMzMzVfZGtGTHU",height = 25, width = 297, autoplay = T,loop= -1,scrolling = "no", seamless = FALSE),#永久地址但是卡，height = 0, width =0隐藏播放器
    shinyDashboardThemes(theme = "poor_mans_flatly" ),#设置主题
    tabItems(
      tabItem(tabName = "a", fileInput("dat","Choose CSV File",accept = ".csv"),helpText(
        "Please select csv file to upload or transform xls file to csv file before uploading! Please refer to the sample data for the format of the uploaded data. 
        In addition, please contact the app author for sample data. Email: xiahui625649@163.com")),
      tabItem(tabName = "subitem1",DT::dataTableOutput("head")),
      tabItem(tabName = "subitem2",fluidRow(
        tabBox(
          title = "Missing value visualization",width = 600,
          tabPanel("Table view",DT::dataTableOutput("view",width = "100%", height = "400px")),
          tabPanel("Plot visualization", plotOutput("visualization",width = "100%", height = "400px"))
        ))),
      tabItem(tabName = "out",fluidPage(titlePanel("Boxplot for different traits"),
                                        #numericInput("traits", "Selected trait:",value=1,min = 1,max = 10),
                                        #helpText( "Please select which trait to input, for example, the first trait is 1 !"),         
                                        textInput("traits", "Selected trait:",placeholder = 
                                                    "Please select which trait to input, for example, the first trait is 1 !",width = "450px"),
                                        box(plotly::plotlyOutput("boxplot",width = "100%"),title = "Boxplot for different traits of families",collapsible = T,width = "100%",solidHeader = T,status = "success")
      )),
      tabItem(tabName = "subitem3",fluidPage(DT::dataTableOutput("mean"),plotOutput("tm_plot"))),
      tabItem(tabName = "subitem4",
              box(title = "Phenotype heatmap",status = "success",width = 600,solidHeader = TRUE,
                  collapsible = TRUE,background = NULL,
                  plotOutput("heatmap",width = "100%", height = "600px"))
      ),
      tabItem(tabName = "t1",fluidRow(
        tabBox(
        title = "Normality test",
        side = "right",width = 600,height = 600,
        tabPanel("Q-Q plot for trait", icon = icon("bar-chart-o"),plotOutput("tr1",width = "100%", height = "600px")),
        tabPanel("Histogram plot for trait",icon = icon("bar-chart-o"), plotOutput("tr2",width = "100%", height = "600px"))
      ))),
      tabItem(tabName = "t2",
              box(title = "Homogeneity test for variance", status = "success",width = "100%", height = "600px",solidHeader = TRUE,
                  collapsible = TRUE,
                  DT::dataTableOutput("homogeneity"),helpText("Note:Levene's Test for Homogeneity of Variance (center = median), p>0.05, the variance is homogeneous."))),
      tabItem(tabName = "b",fluidPage(DT::dataTableOutput("Parameter")),fluidRow(height="100%",
              box(title = "Traits of different families on different blocks",status = "success",solidHeader = T,plotOutput("parallel_plot1")),
              box(title = "Values of different traits among different families",status = "success",solidHeader = T,plotOutput("parallel_plot2")))),
      tabItem(tabName = "aov",fluidRow(
        tabBox(title = "ANOVA analysis for different families",width = 600,
               tabPanel("Single trait",icon = icon("th"),fluidPage(
                 column(box(textOutput("ts", inline = F), status = "success",solidHeader = T,collapsible = T,width = "100%"),width = 4),
                 br(),
                 DT::dataTableOutput("aov1",width = "100%", height = "400px"
                 ))),
               tabPanel("Summary",icon = icon("calendar"),DT::dataTableOutput("aov2",width = "100%", height = "500px")))
      )),
      tabItem(tabName = "bp",fluidRow(
        tabBox(title = "Blup values rank for different families among traits",width = "100%",
               tabPanel("Table for ranks", icon = icon("table"),DT::dataTableOutput("tb_blup",width = "100%"),downloadButton("download_blup", "Download")),
               tabPanel("Heatmap for ranks",fluidPage(title = "Blup values rank heatmap",plotOutput("hp_blup",width = "100%", height = "600px"))))
      )),
      tabItem(tabName = "ce",fluidPage(title = "Comprehensive evaluation based on Qi value for different families",
                                       DT::dataTableOutput("qi",width = "100%"))),
      tabItem(tabName = "efs",fluidRow(tabBox(title = "Excellent families selection and genetic gains for different traits",width = "100%",
         tabPanel("selection based on blup rank",icon = icon("calendar"),                                    
        box(title="Selected excellent families and genetic gains",status = "success",solidHeader = T,collapsible = T,width = 8,DT::dataTableOutput("se_table")),
        box(title = "selection ratio",status = "success",solidHeader = T,collapsible = T, width = 4,sliderInput("sr","Selection ratio input:",0,1,0.1),
            helpText("Excellent families selected are:",textOutput("te")))
         ),tabPanel("selection based on Qi value",icon = icon("table"),fluidPage(
           DT::dataTableOutput("se_qi",width = "100%")
         ))
        )
      ))
    ))
)
server <- function(input, output){
  d1 <- reactive({#导入数据
    inFile1 <- input$dat
    if(is.null(inFile1)) return(NULL)
    df=data.table::fread(inFile1$datapath,na.strings = "NA",data.table = F)#data.table,TRUE返回data.table，FALSE返回data.frame
    traits <- vector()
    for (i in 1:(ncol(df)-2)) {
      traits[i] <-paste("trait",i,sep = "")
    }
    colnames(df)<-c("block","family",traits)#修改数据列名
    df$block <-as.factor(df$block)
    df$family<- as.factor(df$family)
    df
  })
  d2 <- reactive({
    dat=d1()
    dat <-dat[complete.cases(dat),]
    a<-function(i){
      i=3:ncol(dat)
      dat[,i]
    }
    a(i)
  })#去除前两列之后的数据
  output$head <- DT::renderDataTable(DT::datatable({#输入的原始数据
    dat=d1()
    dat
  },rownames = F))
  output$view <- DT::renderDataTable(DT::datatable({
    dat = d1()
    dat[!complete.cases(dat),]#给出数据集中包含缺失值的行
  },rownames = F,options = list(pageLength = 10)))
  output$visualization<- renderPlot({
    dat = d1()
    VIM::aggr(dat,prop=F,numbers=T)#prop=F表示输出的是计数不是频数，numbers=T表示右图输出数字
  })
  output$boxplot <- plotly::renderPlotly({
    dat=d1()
    dat <-dat[complete.cases(dat),]
    values <- d2()[,as.numeric(input$traits)]
    trait <- colnames(dat)[as.numeric(input$traits)+2]
    #graphics::boxplot(t)
    #p<-ggplot(data = d2(),aes(y=values,x=paste("",trait)))+labs(title = paste("Boxplot for",trait,sep = " "),x=paste("",trait))+
    #  geom_boxplot(width=0.5,outlier.colour="red", outlier.shape=7,outlier.size=3,na.rm = T,
     #              outlier.alpha =0.3,outlier.stroke = 0.5,varwidth =T)+geom_jitter(width = 0.25,height = 0.02)+
      #theme_classic()
    p <- plotly::plot_ly(dat,y=values,type = "box",color = trait,alpha = 0.7,colors = "green",width = "100%")
    p
  })
  d4 <- reactive({#生成不含缺失值和离群值的数据
    dat =d1()
    require(tidyr)
    require(stats)
    drop_outliers<-function(df_name, dep_col){
      a <- df_name[,dep_col]
      iqr <- IQR(a,na.rm = T)
      q1 <- as.numeric(quantile(a,0.25,na.rm = T))
      q3 <- as.numeric(quantile(a,0.75,na.rm = T))
      bottom <- q1-1.5*iqr
      top <- q3+1.5*iqr
      df_return <- df_name %>% dplyr::filter(df_name[,dep_col]>= bottom & df_name[,dep_col]<= top)
      return(df_return)
    }
    f<-list()
    for (i in (3:ncol(dat))){
      d<- drop_outliers(dat,i)
      f[i]<-list(d)
    }
    s<- do.call(rbind,f)#通过行合并for循环生产的几个data.frame
    s<-s[!duplicated(s),]#去除重复的行
    s<-s[complete.cases(s),] %>% as.data.frame()#去除含缺失值的行
    s
  })
  mean_data <-reactive({
    dat =d4()
    d <- dat[,-(1:2)]
    a <- list()
    for (i in c(1:ncol(d))){
      values <- d[,i]
      test_l<- stats::aggregate(values~family,dat,mean)
      colnames(test_l)[2]<-paste("trait",i,sep = "")
      a[i] <-list(test_l)
    }
    t <-do.call(cbind, a) %>% as.data.frame()
    traits <- vector()
    for (i in c(1:ncol(d))){
      traits[i] <- paste("trait",i,sep = "")
    }
    b<- t[,traits]
    b$family <-t[,1]
    b<-b[,c(ncol(b),(1:ncol(b)-1))]
    b
  })
  output$mean <- DT::renderDataTable(DT::datatable({
    dat =mean_data()
    dat
  },rownames = F)%>% DT::formatRound(c(2:ncol(dat)),2))
  output$tm_plot <- renderPlot({
    require(lattice)
    dat <- mean_data()
    parallelplot(~dat[,2:ncol(dat)],dat,horizontal.axis = FALSE,groups = as.factor(dat$family),
                 xlab = "Traits",ylab = "Mean value of different families",scales = list(x = list(rot = 90)))
  })
  output$heatmap <- renderPlot({
    dat=mean_data()
    row.names(dat) <- paste(rep("Family",nrow(dat)),dat[,1],sep="_")
    dat<- dat[,-1]
    pheatmap::pheatmap(dat, cluster_row = TRUE,cluster_col =F, scale ="column",treeheight_row = 100,
                       main="phenotype heatmap of different families on traits",cellwidth = 100, cellheight = 15)
  })
  output$tr1<- renderPlot({
    dat = d4()
    values <- d2()[,as.numeric(input$traits)]
    trait <- colnames(dat)[as.numeric(input$traits)+2]
    car::qqPlot(values,main=paste("Q-Q plot",trait,sep = " "),id=F,ylab=paste("",trait),grid=F)
  })
  output$tr2<- renderPlot({
    dat = d4()
    d<-dat[,-(1:2)]
    values <- d[,as.numeric(input$traits)]
    trait <- colnames(dat)[as.numeric(input$traits)+2]
    hist_data <- graphics::hist(values, col = "red", xlab = paste("",trait),main = paste("Histogram of",trait,sep = " "))
    new_mar <- old_mar <- par()$mar
    new_mar[2] <- new_mar[1] 
    par(mar = new_mar)
    pos <- pretty(hist_data$density, n = 5)
    freq <- round(pos * length(na.omit(values)) * with(hist_data, breaks[2] - breaks[1]))
    graphics:::plot.histogram(hist_data, freq = FALSE, col="grey", main="",
                              xlab =paste("",trait), ylab="Frequeny", cex.lab = 2.0,
                              border="black", yaxt='n', cex.axis=1.5)
    Axis(side = 2, at = pos, labels = freq, cex.axis=1.5, cex.lab = 2.0)
    lines(density(na.omit(values)), col="blue", lwd = 2)
  })
  output$homogeneity<- DT::renderDataTable(DT::datatable({
    dat<-d4()
    d <- dat[,-(1:2)]
    test <- list()
    for (i in c(1:ncol(d))){
      values <- d[,i]
      test_l<- car::leveneTest(values~family,dat)
      test_l<-test_l[-2,]
      row.names(test_l)<-c(paste("Trait",i,sep = " "))
      test[i]<-list(test_l)
    }
    test_all <-do.call(rbind, test)
    test_all
  },rownames = T) %>% DT::formatRound(c(2:ncol(test_all)),3)
  )
  output$Parameter <- DT::renderDataTable(DT::datatable({
    dat =d4()
    d<-tidyr::gather(dat,key="variable",value = "value",-c(1:2))
    func <- function(x)(c(mean=mean(x,na.rm = T),
                          min = min(x,na.rm = T),
                          max = max(x,na.rm = T), 
                          sd=stats::sd(x,na.rm = T),
                          "cv(%)"=stats::sd(x,na.rm = T)/mean(x,na.rm = T)*100))
    data<-stats::aggregate(value~variable,d,func)
    re<-cbind("Variation sources"=data$variable,as.data.frame(data$value))
    re
    Shannon_index<-function(df,i){
      require(stats)
      hdata <- df[complete.cases(df),]
      hdata <-hdata[,-c(1:2)]
      hdata[,'level']<-cut(hdata[,i],breaks=c(-Inf,(mean(hdata[,i])-2*sd(hdata[,i])),
                                              (mean(hdata[,i])-1.5*sd(hdata[,i])),
                                              (mean(hdata[,i])-1*sd(hdata[,i])),
                                              (mean(hdata[,i])-0.5*sd(hdata[,i])),
                                              (mean(hdata[,i])+0*sd(hdata[,i])),
                                              (mean(hdata[,i])+0.5*sd(hdata[,i])),
                                              (mean(hdata[,i])+1*sd(hdata[,i])),
                                              (mean(hdata[,i])+1.5*sd(hdata[,i])),
                                              (mean(hdata[,i])+2*sd(hdata[,i])),
                                              Inf),
                           labels=c(letters[1:10]))
      f<-as.data.frame(table(hdata$level))#计算各分级频数
      f<-cbind(f,rep(length(hdata[,i])))#合并数据框
      f$pi<-f$Freq/f$`rep(length(hdata[, i]))`#计算各分级pi值（各分级频数占总数百分比）
      ifelse((f$pi)[1]==0,f<-f[-1,],f)
      H<--1*sum(f$pi*log(f$pi))#Shannon-Wiener多样性指数
      return(H)
    }
    #  -Inf -0.002211662  0.017665113 (mean(hdata[,i])-2*sd(hdata[,i]))小于0时，删除为0的第一行
    H<-vector()
    for (i in 1:(ncol(dat)-2)) {
      H[i]<-Shannon_index(dat,i) 
    } 
    re$"Shannon index"<-H
    re<-cbind(re,h2())
    re
  },rownames = F) %>% DT::formatRound(c(2:ncol(re)-2),2))
  output$parallel_plot1 <- renderPlot({
    dat<-d4()
    require(lattice)
    parallelplot(~dat[,3:ncol(dat)]| as.factor(block),data = dat,horizontal.axis = FALSE,xlab = "Traits",
                 ylab = "values of families among different traits in each block",scales = list(x = list(rot = 90)))
  })
  output$parallel_plot2 <- renderPlot({
    dat<-d4()
    require(lattice)
    parallelplot(~dat[,3:ncol(dat)],data = dat,horizontal.axis = FALSE,xlab = "Traits",
                 ylab = "values of families among different traits",scales = list(x = list(rot = 90)))
  })
  output$aov1 <- DT::renderDataTable(DT::datatable({
    dat=d4()
    d<-dat[,-(1:2)]
    values <- d[,as.numeric(input$traits)]
    t<- stats::aov(values~family*block,data = dat) %>%summary()
    aov_analysis <-t[[1]]
    aov_analysis
  },rownames = T)%>% DT::formatRound(c(2:ncol(aov_analysis)),3))
  d3 <- reactive({
    dat=d4()
    d<-dat[,-(1:2)]
    t <- list()
    for (i in c(1:ncol(d))){
      values <- d[,i]
      a<- stats::aov(values~family*block,data = dat) %>% summary()
      a[[1]]$Traits <- c(colnames(d)[i],NA,NA,NA)
      t[i]<-a
    }
    t
  })
  output$aov2 <- DT::renderDataTable(DT::datatable({
    t<-d3()
    all_t <-do.call(rbind,t)
    all_t$"Variation sources" <-c(rep(c("family","block","family:block","residuals"),ncol(d2())))
    all_t <- all_t[,c(ncol(all_t)-1,ncol(all_t),(1:(ncol(all_t)-2)))]
    all_t
  },rownames = F,options = list(pageLength = 12))%>% DT::formatRound(c(4:ncol(all_t)),3))
  output$ts <- renderText({
    paste(paste("Trait you've selected is:", input$traits, sep = " ")," trait",sep = "")
  })
  blup_data <-reactive({
    require(breedR)
    df<-d4()
    df$family<-as.factor(df$family)
    Blup_value <- list()
    for (i in 1:(ncol(df)-2)) {
      mvres<-function(object){
        df<-data.frame(mv=ranef(object)$family,mv.se=attr(ranef(object)$family,'se'))
        df$Family<-row.names(df)
        df<-dplyr::arrange(df,desc(mv))
        df$mv.rank<-1:nrow(df)
        return(df)
      }
      dat<-df[,-(1:2)]
      fit<-remlf90(fixed = dat[,i]~1+block,
                   random = ~family,
                   data = df)
      fM<-mvres(fit) %>% dplyr::arrange(Family)
      colnames(fM) <- c(paste("Blup", i, sep = "_"),paste("Blup.se", i, sep = "_"),paste("family", i, sep = "_"),
                        paste("Rank", i, sep = "_"))
      Blup_value[i]<-list(fM)
    }
    Blup <-do.call(cbind,Blup_value)
    Blup$Family<-Blup$family_1 %>% as.numeric()
    Blup <- dplyr::select(Blup,-dplyr::contains("family",ignore.case = FALSE))# contains忽略了大小写，有个参数可以改变
    Blup <-Blup[,c(ncol(Blup),1:(ncol(Blup)-1))] %>% dplyr::arrange(Family)
    m<-dplyr::select(Blup,dplyr::contains("rank"))
    f<-vector()
    for (g in 1:nrow(m)) {
      f[g]<-sum(m[g,])
    }
    Blup$Rank <-f
    Blup
  })
  output$tb_blup <-DT::renderDataTable(DT::datatable({
  dat<-blup_data()
  dat
  },rownames = F))
  output$download_blup <- downloadHandler(
    filename = function() {
      paste("blup rank-",Sys.time(),".docx", sep = "")
    },
    content = function(file) {
      dat <-blup_data()
      data.table::fwrite(dat, file, row.names=F)
    }
  )
  output$hp_blup <-renderPlot({
    dat<-blup_data() 
    row.names(dat) <- paste("family",dat$Family,sep = "_")
    dat<- dat[,-1] %>% dplyr::select(dplyr::contains("rank_",ignore.case = T))
    colnames(dat)<- paste(colnames(d1())[3:ncol(d1())],"rank",sep = "_")
    pheatmap::pheatmap(dat, cluster_row = TRUE,cluster_col =F, scale ="none",treeheight_row = 100,
                       color = colorRampPalette(c("red","white","blue"))(256),
                       main="Blup values rank heatmap for different families among traits",cellwidth = 100, cellheight = 15)
  })
  qi_data <- reactive({
    t<-mean_data()
    m_data <-t
    Qi <-vector()
    for (i in 1:nrow(m_data)) {
      p<-list()
      for (j in 1:ncol(m_data[,-1])) {
        a<-(t[i,j+1]/max(t[,j+1]))
        p[j] <-a
      }
      s<- do.call(sum,p) %>% sqrt()
      Qi[i] <-s
    }
    z <- data.frame(Family=m_data$family,Qi)
    z
  })
  output$qi <-DT::renderDataTable(DT::datatable({
   q<- qi_data()
   q
  },rownames = F)%>% DT::formatRound(2,3))
  h2 <- reactive({
    df <- d4()
    nb = length(levels(as.factor(df$block)))
    nf = length(levels(as.factor(df$family)))
    a<-data.frame(table(df[,1:2]))
    b <- dplyr::filter(a,!Freq ==0)
    c <-tail(cumsum(1/b$Freq),1)
    nk = (nb*nf)/c#单地点的调和平均株数
    dat<-df[,-(1:2)]
    require(breedR)
    hi <- vector()
    hf <- vector()
    for (i in 1:ncol(dat)) {
      fit<-remlf90(fixed = dat[,i]~1+block,
                   random = ~family,
                   data = df)
      d<-breedRPlus::var(fit)
      hi[i] <-round(4*d[1,2]/(d[1,2]+d[2,2]),4)#单株遗传力，breedRPlus::pin(fit1, h2~4*V1/(V1+V2))
      hf[i] <-round(d[1,2]/(d[1,2]+d[2,2]/(nk*nb)),4)#家系遗传力，breedRPlus::pin(fit1, h2~V1/(V1+V2/nk))
    }
    h2_result <- data.frame("Indivual heritability"=hi, "Family heritability"=hf)
    h2_result
  })
  blup_select_result <- reactive({
    Blup <-blup_data()
    h2_result <-h2()
    t <- mean_data()
    select_ratio <- input$sr
    select_number <- select_ratio * nrow(Blup)
    select_family <- (head(dplyr::arrange(Blup,Rank),select_number))[,1]
    select_data <- dplyr::filter(t,family %in% select_family)
    w1 <- tidyr::gather(select_data,key="Traits",value="Mean_value",2:4)
    w2<- stats::aggregate(Mean_value~Traits,w1,mean)
    w3 <- tidyr::gather(t,key="Traits",value="Total_Mean_value",2:4)
    w4 <-stats::aggregate(Total_Mean_value~Traits,w3,mean)
    w <- merge(w2,w4,by="Traits") %>% cbind(h2_result)
    w$select_difference <- w$Mean_value-w$Total_Mean_value
    w$Genetic_gains <- (w$Family.heritability * w$select_difference/w$Total_Mean_value)*100
    f<-as.data.frame(matrix(c("Genetic gains (%)",w$Genetic_gains),ncol = 4,nrow = 1,byrow=T))
    colnames(f)<-colnames(select_data)
    e <- rbind(select_data,f)
    e
  })
  output$se_table <-DT::renderDataTable(DT::datatable({#根据Blup值排名进行优良家系选择
  a<-blup_select_result()
  a
  },rownames = F)%>% DT::formatRound(c(2:ncol(a)),2))
  output$te <- renderText({
    dat <- blup_select_result()
    a<-paste("Family",as.character(dat[1:(nrow(dat)-1),1]),sep = " ")
    b<-paste(a[1:length(a)-1],",",sep="")#不能用中文字符、
    d<-c(b,a[length(a)])
    d
  })
  output$se_qi <- DT::renderDataTable(DT::datatable({
    Q <-qi_data()
    h2_result <-h2()
    t <- mean_data()
    select_ratio <- input$sr
    select_number <- select_ratio * nrow(Q)
    select_family <- (head(dplyr::arrange(Q,desc(Qi)),select_number))[,1]
    select_data <- dplyr::filter(t,family %in% select_family)
    w1 <- tidyr::gather(select_data,key="Traits",value="Mean_value",2:4)
    w2<- stats::aggregate(Mean_value~Traits,w1,mean)
    w3 <- tidyr::gather(t,key="Traits",value="Total_Mean_value",2:4)
    w4 <-stats::aggregate(Total_Mean_value~Traits,w3,mean)
    w <- merge(w2,w4,by="Traits") %>% cbind(h2_result)
    w$select_difference <- w$Mean_value-w$Total_Mean_value
    w$Genetic_gains <- (w$Family.heritability * w$select_difference/w$Total_Mean_value)*100
    f<-as.data.frame(matrix(c("Genetic gains (%)",w$Genetic_gains),ncol = 4,nrow = 1,byrow=T))
    colnames(f)<-colnames(select_data)
    e <- rbind(select_data,f)
    e 
  },rownames = F)%>% DT::formatRound(c(2:ncol(e)),2))
}
shinyApp(ui=ui, server = server)
#runApp("C:/Users/HP/Desktop/R_learning_files/shiny包/forestry data analysis/app.R",display.mode = "showcase")#可展示代码
