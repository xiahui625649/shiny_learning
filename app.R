library(shiny)
library(shinydashboard)
library(magrittr)
ui <- dashboardPage(
  dashboardHeader(title = "Forestry Data Analysis"),
  dashboardSidebar(
    sidebarMenu(
    menuItem("Upload Data", tabName = "a",icon = icon("th")),
    br(),
    menuItem("Data Observation", icon = icon("list-alt"),
             menuSubItem("head result",tabName = "subitem1"),
             menuSubItem("summary result",tabName = "subitem2"),
             menuSubItem("mean value",tabName = "subitem3"),
             menuSubItem("phenotype heatmap",tabName = "subitem4")
             ),
    br(),
    menuItem("Data Test",icon =icon("bar-chart-o"),
             menuSubItem("Normality test",tabName = "t1"),
             menuSubItem("Homogeneity test for variance",tabName = "t2")),
    br(),
    menuItem("Variation Parameter", tabName = "b",icon = icon("table"))
  )),
  dashboardBody(
    tabItems(
      tabItem(tabName = "a", fileInput("dat","Upload csv File",accept = ".csv")),
      tabItem(tabName = "subitem1",DT::dataTableOutput("head")),
      tabItem(tabName = "subitem2",fluidRow(
        tabBox(
          title = "Missing value visualization",width = 600,
          tabPanel("Table view",DT::dataTableOutput("view",width = "100%", height = "400px")),
          tabPanel("Plot visualization", plotOutput("visualization",width = "100%", height = "400px"))
        ))),
      tabItem(tabName = "subitem3",DT::dataTableOutput("mean")),
      tabItem(tabName = "subitem4",
        box(title = "Phenotype heatmap",status = "primary",width = 600,solidHeader = TRUE,
            collapsible = TRUE,
            plotOutput("heatmap",width = "100%", height = "400px"))
              ),
      tabItem(tabName = "t1",fluidRow(tabBox(
        title = "Normality test",
        side = "right",width = 600,height = 600,
        tabPanel("Trait 1", plotOutput("trait1",width = "100%", height = "400px")),
        tabPanel("Trait 2", plotOutput("trait2",width = "100%", height = "400px")),
        tabPanel("Trait 3", plotOutput("trait3",width = "100%", height = "400px"))
      ))),
      tabItem(tabName = "t2",
              box(title = "Homogeneity test for variance", status = "primary",width = "100%", height = "400px",solidHeader = TRUE,
                  collapsible = TRUE,
                  plotOutput("homogeneity"))),
      tabItem(tabName = "b",DT::dataTableOutput("Parameter"))
))
)
server <- function(input, output){
  d1 <- reactive({
    inFile1 <- input$dat
    if(is.null(inFile1)) return(NULL)
    data.table::fread(inFile1$datapath,na.strings = "NA")
  })
  output$head <- DT::renderDataTable(DT::datatable({
    dat = d1()
    head(dat,nrow(dat))
  },rownames = F)%>% formatRound(c(3:ncol(dat)),2))
  output$view <- DT::renderDataTable(DT::datatable({
    dat = d1()
    dat[!complete.cases(dat),]#给出数据集中包含缺失值的行
  },options = list(pageLength = 10)))
  output$visualization<- renderPlot({
    dat = d1()
    VIM::aggr(dat,prop=F,numbers=T)#prop=F表示输出的是计数不是频数，numbers=T表示右图输出数字
  })
  output$mean <- DT::renderDataTable(DT::datatable({
    dat =d1()
    dat <- dat[,-1] %>% dplyr::group_by(家系) %>% dplyr::summarise(trait1 = mean(性状1,na.rm=T),
                                                   trait2 = mean(性状2,na.rm=T),
                                                   trait3 = mean(性状3,na.rm=T)) %>% as.data.frame()
    dat
  },rownames = F)%>% formatRound(c(2:ncol(dat)),2))
  output$heatmap <- renderPlot({
    dat=d1()
    dat <- dat[,-1] %>% dplyr::group_by(家系) %>%  dplyr::summarise(trait1 = mean(性状1,na.rm=T),
                                                                  trait2 = mean(性状2,na.rm=T),
                                                                  trait3 = mean(性状3,na.rm=T)) %>% as.data.frame()
    row.names(dat) <- paste(rep("Family",nrow(dat)),dat[,1],sep="_")
    dat<- dat[,-1]
    pheatmap::pheatmap(dat, cluster_row = TRUE,cluster_col =F, scale ="column",
                       main="phenotype heatmap of different families on traits",cellwidth = 50, cellheight = 10)
  })
  output$trait1<- renderPlot({
    dat <- d1()
    fit1<- stats::lm(性状1~家系,data = dat)
    car::qqPlot(fit1,main="tarit1 Q-Q plot",labels=F)
  })
  output$trait2<- renderPlot({
    dat <- d1()
    fit2<- stats::lm(性状2~家系, dat)
    car::qqPlot(fit2, main="tarit2 Q-Q plot",labels=F)
  })
  output$trait3<- renderPlot({
    dat <-d1()
    fit3<- stats::lm(性状3~家系,dat)
    car::qqPlot(fit3,main="tarit3 Q-Q plot",labels=F)
  })
  output$homogeneity<- renderPlot({
    dat<-d1()
    fit<- stats::lm(家系~性状1+性状2+性状3, dat)
    car::spreadLevelPlot(fit)
  })
  output$Parameter <- DT::renderDataTable(DT::datatable({
    dat =d1()
    d<-tidyr::gather(dat,key="variable",value = "value",-c(1:2))
    func <- function(x)(c(mean=mean(x,na.rm = T),
                          min = min(x,na.rm = T),
                          max = max(x,na.rm = T), 
                          sd=stats::sd(x,na.rm = T),
                          "cv(%)"=stats::sd(x,na.rm = T)/mean(x,na.rm = T)*100))
    data<-stats::aggregate(value~variable,d,func)
    re<-cbind("variation sources"=data$variable,as.data.frame(data$value))
    re
  },rownames = F) %>% formatRound(c(2:ncol(re)),2))
}
shinyApp(ui=ui, server = server)
#runApp("./General_Data_Analysis/app.R",display.mode = "showcase")可展示代码
