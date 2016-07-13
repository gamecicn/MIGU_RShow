library(shiny)
library(DT)
library(markdown)
library(ggplot2)
library(datasets)
library(shinyBS)

# Define UI for application that plots random distributions 
shinyUI(fluidPage(
  
  titlePanel = ("Migu风控系统"),
  
  #Title
  fluidRow(
      h1("Migu风控系统",align="center")
  ),
  #汇总信息
  fluidRow(
      column(2, h4('汇总信息'))
  ),
  fluidRow(
    column(2, h4('总计费次数:',align="left")),
    column(1, h4(textOutput("e_billingsum")),align="left"),
    column(2, h4('总计费价格:',align="left")),
    column(1, h4(textOutput("e_totalsum")),align="left"),
    column(5, dateRangeInput('dateRange',
                          label = "",
                          start = Sys.Date()-50 , end = Sys.Date(),
                          separator = " - "
           ),align="right"),
    column(1,
           br(),
           submitButton("ok"),align="middle")
    
  ),
  br(),
  fluidRow(
    # side bar
    sidebarLayout(
      sidebarPanel( 

        fluidRow(h3("筛选条件")),
                 fluidRow(radioButtons("radiofilter", label = "",
                     choices = c('应用ID', '维度', '省份', '号段', 'CP'), 
                     selected = '应用ID')),
        ("------------------------------------------"),
        fluidRow(h3("详细信息")),
        fluidRow(
        h5("已选 App"),
        verbatimTextOutput('select_id'),
        
        selectInput("gourp rule", label = h5("分组方式"), 
                    choices = c('渠道', '省份', '城市') , 
                    selected = "渠道"),
        
        h5("查看应用详细统计信息"),
        actionButton("go", "查看"),
        
        bsModal("modalExample", 
                "应用详细统计信息", 
                "go", 
                size = "large", 
                  
                  verbatimTextOutput("应用详细统计信息")
                  #DT::dataTableOutput('app_billing_detail')

            )
        
      )),
      
    
       #main nav bar pannel
       mainPanel(

                 tabsetPanel(
                   id='tabpanel',
                   tabPanel('汇总表',
                              downloadButton('downloadData', 'Download'),
                              DT::dataTableOutput('ex2')
                   ),
                   tabPanel('柱状图',
                            plotOutput(outputId = "hist_plot", height = "300px")
                            #plotOutput("histPlot")  
                   ),
                   tabPanel('饼图'),
                   tabPanel('折线图')
                 )
        ) 
    )
  )
)
)

