library(shiny)
library(ggplot2)
library(RMySQL)
library(sqldf)
library(datasets)
# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
  # Expression that generates a histogram. The expression is
  # wrapped in a call to renderPlot to indicate that:
  #
  #  1) It is "reactive" and therefore should be automatically
  #     re-executed when inputs change
  #  2) Its output type is a plot
 

  datasetBillingDataInput <- reactive({
    
      data_range <- input$dateRange
    
      # Connect to MySQL to get data 
      mysql_con <- dbConnect(MySQL(), user="root", password="irdetogame", dbname='DATACENTER', host="10.86.51.13")
      
      sql_query <- sprintf("select *  from REVENUE where date between '%s' and '%s';", data_range[1], data_range[2])
      
      billing_data <- fetch(dbSendQuery(mysql_con, sql_query), n=-1)
      
     # billing_data <- fetch(dbSendQuery(mysql_con, "select *  from REVENUE;"))
      billing_data$appid<-as.character(billing_data$appid)
      billing_data$cid<-as.character(billing_data$cid)
      billing_data$cpid<-as.character(billing_data$cpid)
      billing_data$chid<-as.character(billing_data$chid)
      billing_data$pid<-as.character(billing_data$pid)
      billing_data$exdid<-as.character(billing_data$exdid)
      billing_data$ctype<-as.character(billing_data$ctype)
      billing_data$ms<-as.character(billing_data$ms)
      billing_data$provcode<-as.character(billing_data$provcode)
      billing_data$isself<-as.character(billing_data$isself)
      billing_data$isn6<-as.character(billing_data$isn6)
      
      billing_data
  })
  
  #appid
  get_appid_aggregated_table <- function(billing_data){
       sp<-NULL
      # billing count
       sp<-split(billing_data,billing_data[,c("appid","exdid","cid","cpid","chid","pid","ctype","ms","provcode","isself","isn6")],drop = TRUE)
      
      #fee sum

      sum <- data.frame(0, 0)
      meta_max <- data.frame(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0)
      
      if(length(sp) > 0 ){
        meta_max <- t(sapply(sp, FUN=function(rd) 
          c(rd$appid[1], rd$exdid[1], rd$cid[1], rd$cpid[1], rd$chid[1], rd$pid[1], rd$ctype[1], 
            rd$ms[1],rd$provcode[1], rd$isself[1], 
            rd$isn6[1], nrow(rd)), simplify = TRUE))
        sum <- t(sapply(sp,FUN=function(rd)c(rd$appid[1], rd$exdid[1], rd$cid[1], rd$cpid[1], rd$chid[1], rd$pid[1], rd$ctype[1], 
                                             rd$ms[1],rd$provcode[1], rd$isself[1], 
                                             rd$isn6[1], sum(rd$price)),simplify=TRUE))
      }
      
      # fix matrix
      rownames(meta_max) <- NULL
      colnames(meta_max) <- c("appid", 'exdid', 'cid', 'cpid', 'chid', 'pid', 'ctype', 'ms', 'provcode','isself','isn6','count')
      tempTable <- data.frame(meta_max)
      tempTable$appid <- as.character(tempTable$appid)
      tempTable$exdid <- as.character(tempTable$exdid)
      tempTable$cid <- as.character(tempTable$cid)
      tempTable$cpid <- as.character(tempTable$cpid)
      tempTable$chid <- as.character(tempTable$chid)
      tempTable$pid <- as.character(tempTable$pid)
      tempTable$ctype <- as.character(tempTable$ctype)
      tempTable$ms <- as.character(tempTable$ms)
      tempTable$provcode<-as.character(tempTable$provcode)
      tempTable$isself<-as.character(tempTable$isself)
      tempTable$isn6<-as.character(tempTable$isn6)
      tempTable$count <- as.character(as.character(tempTable$count))
      
      rownames(sum) <- NULL
      colnames(sum) <- c("appid", 'exdid', 'cid', 'cpid', 'chid', 'pid', 'ctype', 'ms', 'provcode','isself','isn6','sum')
      tempTable1 <- data.frame(sum)
      tempTable1$appid <- as.character(tempTable1$appid)
      tempTable1$exdid <- as.character(tempTable1$exdid)
      tempTable1$cid <- as.character(tempTable1$cid)
      tempTable1$cpid <- as.character(tempTable1$cpid)
      tempTable1$chid <- as.character(tempTable1$chid)
      tempTable1$pid <- as.character(tempTable1$pid)
      tempTable1$ctype <- as.character(tempTable1$ctype)
      tempTable1$ms <- as.character(tempTable1$ms)
      tempTable1$provcode<-as.character(tempTable1$provcode)
      tempTable1$isself<-as.character(tempTable1$isself)
      tempTable1$isn6<-as.character(tempTable1$isn6)
      tempTable1<- data.frame(sum)

      
      tempTableSum <-merge(tempTable,tempTable1,by=c("appid", 'exdid', 'cid', 'cpid', 'chid', 'pid', 'ctype', 'ms', 'provcode','isself','isn6'),all.x = T)
      tempTableSum$sum <- as.character(tempTableSum$sum)
      #统计信息
      output$e_totalsum <-renderText({
        paste(" ", as.character(max( cumsum(as.numeric(tempTableSum$sum)))))
      })
      
      output$e_billingsum <-renderText({
        paste0(" ", as.character(nrow(billing_data)))
      })
      
      return (tempTableSum)
    
  }
  
  #四维度
  get_dimension_aggregated_table <- function(billing_data){
    sp<-NULL
    # billing count
    sp<-split(billing_data,billing_data[,c("cid","cpid","chid","pid","ctype")],drop = TRUE)
    
    #fee sum
    
    sum <- data.frame(0, 0)
    meta_max <- data.frame(0, 0, 0, 0, 0)
    
    if(length(sp) > 0 ){
      meta_max <- t(sapply(sp, FUN=function(rd) 
        c( rd$cid[1], rd$cpid[1], rd$chid[1], rd$pid[1], rd$ctype[1], 
          nrow(rd)), simplify = TRUE))
      sum <- t(sapply(sp,FUN=function(rd)c(rd$cid[1], rd$cpid[1], rd$chid[1], rd$pid[1], rd$ctype[1], sum(rd$price)),simplify=TRUE))
    }
    
    # fix matrix
    rownames(meta_max) <- NULL
    colnames(meta_max) <- c( 'cid', 'cpid', 'chid', 'pid', 'ctype','Num')
    tempTable <- data.frame(meta_max)
    tempTable$cid <- as.character(tempTable$cid)
    tempTable$cpid <- as.character(tempTable$cpid)
    tempTable$chid <- as.character(tempTable$chid)
    tempTable$pid <- as.character(tempTable$pid)
    tempTable$ctype <- as.character(tempTable$ctype)
   
    tempTable$Num <- as.character(as.character(tempTable$Num))
    
    rownames(sum) <- NULL
    colnames(sum) <- c( 'cid', 'cpid', 'chid', 'pid', 'ctype','price')
    tempTable1<- data.frame(sum)
    tempTable1$cid <- as.character(tempTable1$cid)
    tempTable1$cpid <- as.character(tempTable1$cpid)
    tempTable1$chid <- as.character(tempTable1$chid)
    tempTable1$pid <- as.character(tempTable1$pid)
    tempTable1$ctype <- as.character(tempTable1$ctype)
    
    tempTableSum <-merge(tempTable,tempTable1,by=c( 'cid', 'cpid', 'chid', 'pid', 'ctype'),all.x = T)
    tempTableSum$price <- as.character(tempTableSum$price)
    #统计信息
    output$e_totalsum <-renderText({
      paste(" ", as.character(max( cumsum(as.numeric(tempTableSum$price)))))
    })
    
    output$e_billingsum <-renderText({
      paste0(" ", as.character(nrow(billing_data)))
    })
    
    return (tempTableSum)
    
  }
  #分省
  get_ProvCode_aggregated_table <- function(billing_data){
    sp<-NULL
    # group by provcode
    sp<-split(billing_data,billing_data[,c("provcode")],drop = TRUE)
    
    count <- data.frame(0, 0)
    sum <- data.frame(0, 0)
    #fee sum
    if(length(sp) > 0 ){
      count <- t(sapply(sp, FUN=function(rd) 
        c(rd$provcode[1], nrow(rd)), simplify = TRUE))
      sum <- t(sapply(sp,FUN=function(r)c(r$provcode[1], sum(r$price)),simplify=TRUE))
    }
    
    # fix matrix
    rownames(count) <- NULL
    colnames(count) <- c( 'provcode','count')
    tempTable_pro_count <- data.frame(count)
    rownames(sum) <- NULL
    colnames(sum) <- c( 'provcode','sum')
    tempTable_pro_sum<- data.frame(sum)
    
    tempTableSum<-NULL
    tempTableSum <-merge(tempTable_pro_count,tempTable_pro_sum,by='provcode',all.x = T)
    tempTableSum$sum <- as.character(tempTableSum$sum)
    
    #统计信息
    output$e_totalsum <-renderText({
      paste(" ", as.character(max( cumsum(as.numeric(tempTableSum$sum)))))
    })
    
    output$e_billingsum <-renderText({
      paste0(" ", as.character(nrow(billing_data)))
    })
    
    #colnames(tempTableSum) <-c('号段','总交易条数','总收入')
    return (tempTableSum)
    
  }
  
  #CP
  get_CP_aggregated_table <- function(billing_data){
    sp<-NULL
    # group by ctype
    sp<-split(billing_data,billing_data[,c("ctype")],drop = TRUE)
    
    count <- data.frame(0, 0)
    sum <- data.frame(0, 0)
    #fee sum
    if(length(sp) > 0 ){
      count <- t(sapply(sp, FUN=function(rd) 
        c(rd$ctype[1], nrow(rd)), simplify = TRUE))
      sum <- t(sapply(sp,FUN=function(r)c(r$ctype[1], sum(r$price)),simplify=TRUE))
    }
    
    # fix matrix
    rownames(count) <- NULL
    colnames(count) <- c( 'ctype','count')
    tempTable_pro_count <- data.frame(count)
    rownames(sum) <- NULL
    colnames(sum) <- c( 'ctype','sum')
    tempTable_pro_sum<- data.frame(sum)
    
    tempTableSum<-NULL
    tempTableSum <-merge(tempTable_pro_count,tempTable_pro_sum,by='ctype',all.x = T)
    tempTableSum$sum <- as.character(tempTableSum$sum)
    
    #统计信息
    output$e_totalsum <-renderText({
      paste(" ", as.character(max( cumsum(as.numeric(tempTableSum$sum)))))
    })
    
    output$e_billingsum <-renderText({
      paste0(" ", as.character(nrow(billing_data)))
    })
    
    #colnames(tempTableSum) <-c('号段','总交易条数','总收入')
    return (tempTableSum)
    
  }
  
  #号段
  get_Phone_aggregated_table <- function(billing_data){
    sp<-NULL
    billing_data1<-billing_data
    billing_data1$phone<-substr(billing_data$ms,1,7)
    # group by provcode
   sp<-split(billing_data1,billing_data1[,c("phone")],drop = TRUE)
    
    count <- data.frame(0, 0)
    sum <- data.frame(0, 0)
    #fee sum
    if(length(sp) > 0 ){
      count <- t(sapply(sp, FUN=function(rd) 
        c(rd$phone[1], nrow(rd)), simplify = TRUE))
      sum <- t(sapply(sp,FUN=function(r)c(r$phone[1], sum(r$price)),simplify=TRUE))
    }
    
    # fix matrix
    rownames(count) <- NULL
    colnames(count) <- c( 'phone','count')
    tempTable_pro_count <- data.frame(count)
    rownames(sum) <- NULL
    colnames(sum) <- c( 'phone','sum')
    tempTable_pro_sum<- data.frame(sum)
    
    tempTableSum<-NULL
    tempTableSum <-merge(tempTable_pro_count,tempTable_pro_sum,by='phone',all.x = T)
    tempTableSum$sum <- as.character(tempTableSum$sum)
    
    #统计信息
    output$e_totalsum <-renderText({
      paste(" ", as.character(max( cumsum(as.numeric(tempTableSum$sum)))))
    })
    
    output$e_billingsum <-renderText({
      paste0(" ", as.character(nrow(billing_data)))
    })
    
    return (tempTableSum)
    
  }
  observeEvent(input$go, {
    
    app_billing <- datasetBillingDataInput()
    
    output$app_billing_detail <- renderDataTable({
      
      dt<-DT::datatable(app_billing,  selection = "single", options = list(pageLength = 25))
    })
    
  })
  
  # row selection
  output$select_id = renderPrint(input$ex1_rows_selected)
  
 
  #summary information
  output$e_company  <- renderText({
      paste0("migu", " company")
  })

  
  
 
  # display 10 rows initially

  output$ex1 <- renderDataTable({

    #DT::renderDataTable(DT::datatable
                        
    if (input$radiofilter == '应用ID') {
        dt<-DT::datatable(get_appid_aggregated_table(datasetBillingDataInput()),  selection = "single", options = list(pageLength = 25))
    }
    if (input$radiofilter == '维度') {
        dt<- DT::datatable(get_dimension_aggregated_table(datasetBillingDataInput()),  selection = "single", options = list(pageLength = 50))
    }
    if (input$radiofilter == "省份") {
        dt<-DT::datatable(get_ProvCode_aggregated_table(datasetBillingDataInput()),   selection = "single", options = list(pageLength = 25))
    }
    if (input$radiofilter == "号段") {
      dt<-DT::datatable(get_Phone_aggregated_table(datasetBillingDataInput()),   selection = "single", options = list(pageLength = 25))
    }
    if (input$radiofilter == "CP") {
      dt<-DT::datatable(get_CP_aggregated_table(datasetBillingDataInput()),   selection = "single", options = list(pageLength = 25))
    }
    
    dt
  })

  output$ex2 <- renderDataTable({
    
   
    
    if (input$radiofilter == '应用ID') {
      dt<-DT::datatable(get_appid_aggregated_table(datasetBillingDataInput()),  selection = "single", options = list(pageLength = 25))
    }
    if (input$radiofilter == '维度') {
      dt<- DT::datatable(get_dimension_aggregated_table(datasetBillingDataInput()),  selection = "single", options = list(pageLength = 50))
    }
    if (input$radiofilter == "省份") {
      dt<-DT::datatable(get_ProvCode_aggregated_table(datasetBillingDataInput()),   selection = "single", options = list(pageLength = 25))
    }
    if (input$radiofilter == "号段") {
      dt<-DT::datatable(get_Phone_aggregated_table(datasetBillingDataInput()),   selection = "single", options = list(pageLength = 25))
    }
    if (input$radiofilter == "CP") {
      dt<-DT::datatable(get_CP_aggregated_table(datasetBillingDataInput()),   selection = "single", options = list(pageLength = 25))
    }
    
    dt
    
    
  })

  
  output$hist_plot <- renderPlot({
    hist(get_appid_aggregated_table$appid,
         probability = TRUE,
        breaks = 1,
         xlab = "appid",
         main = "billing")
  })
  
  
  #download
  output$downloadData <- downloadHandler(
    filename = function() { 
      paste('appid', '.csv', sep='') 
    },
    content = function(file) {
      write.csv(billing_data, file)
    }
  )
  
})
