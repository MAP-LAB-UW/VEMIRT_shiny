library(shiny)
library(shinydashboard)
# library(psych)
# library(testit)
# library(Matrix)
# library(gtools)
# library(MASS)
library(mirt)
# library(cacIRT)
# library(mvtnorm)
# library(graphics)
# library(dmutate)
library(abind)
source('mbr.R')
source('gvemm.R')
source('iwgvemm.R')

#####shiny app######
options(shiny.maxRequestSize = 30*1024^2)
# Define UI for data upload app ----

ui <- dashboardPage(skin="purple",
  dashboardHeader(title = "Regularized DIF"),
  dashboardSidebar(sidebarMenu(
    ############################
    # Input: Data file u ----
    ############################
    fileInput("file1", "Choose Data CSV File",
              multiple = TRUE,
              accept = c("text/csv",
                         "text/comma-separated-values,text/plain",
                         ".csv")),
    
    # Horizontal line ----
    tags$hr(),
    
    # Input: Checkbox if file has header ----
    checkboxInput("header1", "Header", TRUE),
    
    # Input: Select separator ----
    radioButtons("sep1", "Separator",
                 choices = c(Comma = ",",
                             Semicolon = ";",
                             Tab = "\t"),
                 selected = ","),
    # Horizontal line ----
    tags$hr(),
    
    # Input: Select number of rows to display ----
    radioButtons("disp1", "Display",
                 choices = c(Head = "head",
                             All = "all"),
                 selected = "head"),
    
    # Horizontal line ----
    tags$hr(),
    
    ############################
    # Input: Data file Group ----
    ############################
    fileInput("file2", "Choose group indicator CSV File",
              multiple = TRUE,
              accept = c("text/csv",
                         "text/comma-separated-values,text/plain",
                         ".csv")),
    # Horizontal line ----
    tags$hr(),
    # Input: Checkbox if file has header ----
    checkboxInput("header2", "Header", TRUE),
    
    # Input: Select separator ----
    radioButtons("sep2", "Separator",
                 choices = c(Comma = ",",
                             Semicolon = ";",
                             Tab = "\t"),
                 selected = ","),
    
    
    # Horizontal line ----
    tags$hr(),
    
    # Input: Select number of rows to display ----
    radioButtons("disp2", "Display",
                 choices = c(Head = "head",
                             All = "all"),
                 selected = "head"),
    
    # Horizontal line ----
    tags$hr(),
    
    
    
    
    ############################
    # Input: Data file indic ----
    ############################
    fileInput("file4", "Choose loading indicator CSV file",
              multiple = TRUE,
              accept = c("text/csv",
                         "text/comma-separated-values,text/plain",
                         ".csv")),
    
    # Horizontal line ----
    tags$hr(),
    
    # Input: Checkbox if file has header ----
    checkboxInput("header4", "Header", TRUE),
    
    # Input: Select separator ----
    radioButtons("sep4", "Separator",
                 choices = c(Comma = ",",
                             Semicolon = ";",
                             Tab = "\t"),
                 selected = ","),
    
    
    # Horizontal line ----
    tags$hr(),
    
    
    
    #Input: Select Reg_DIF methods ----
    selectInput("method", 
                label = "Choose a regularization algorithm",
                choices = list("Lasso EM"='EM', 
                               "Lasso EMM"='EMM',
                               "Adaptive lasso EM"="Adapt",
                               "Lasso GVEMM" = "GVEMM",
                               "Lasso IW-GVEMM" = "IW-GVEMM",
                               "Likelihood Ratio Test"="LRT"),
                selected = "EMM"),
    
    
    #Input: Select information criteria ----
    uiOutput('ic'),
    
    # Horizontal line ----
    tags$hr(),
    actionButton("go1", "Run"),
    
    
    # Horizontal line ----
    tags$hr(),
    radioButtons("checkGroup1", "Download Results",
                 choices = list("All Results" = "all",
                                "Item Parameters" = "item",
                                "Covariance Matrix" = "cov"),
                 selected = "all"),
    # Button
    downloadButton("downloadData", "Download Results")
    
  )),
  dashboardBody(tags$style(".skin-purple .sidebar .shiny-download-link { color: #444; }"),
    mainPanel(
      
      uiOutput("tab"),
      uiOutput("tab2"),
      
      h2("Data"),
      # Output: Data file ----
      tableOutput("contents1"),
      
      h2("Group Indicator"),
      # Output: Data file ----
      tableOutput("contents2"),
      
      h2("Loading indicator"),
      # Output: Data file ----
      tableOutput("contents4"),
      
      #warning
      h3(span(textOutput("warn"),style="color:red")),
      
      h2("Item Parameter Results"),
      tableOutput("par1"),
      
      h2("Mean Vector"),
      tableOutput("mean1"),
      
      h2("Covariance Matrix"),
      tableOutput("cov1"),
      
      h2("Information Criteria"),
    ),
    fluidRow(
      box(status="primary",plotOutput("plot",width = "100%", height = "300px"))
    )
  )
)


# Define server logic to read selected file ----
server <- function(input, output,session) {
  url <- a("Regularized DIF (GVEM algorithms)", href="https://www.google.com/")
  output$tab <- renderUI({
    tagList("Other Functions:", url)
  })
  
  # data matrix
  u<-reactive({req(input$file1)
    df <- data.matrix(read.csv(input$file1$datapath,
                               header = input$header1,
                               sep = input$sep1))
    return(df)
  })
  output$contents1 <- renderTable({
    if(input$disp1 == "head") {
      return(u()[1:6,])
    }
    else {
      return(u())
    }
    
  })
  # group indicator 
  Group<-reactive({req(input$file2)
    df <- read.csv(input$file2$datapath,
                   header = input$header2,
                   sep = input$sep2)[,1]
    if (is.character(df)) df <- as.factor(df)
    if (!is.integer(df)) df <- as.integer(df)
    return(df)
  })
  output$contents2 <- renderTable({
    if(input$disp2 == "head") {
      return(Group()[1:6])
    }
    else {
      return(Group())
    }
    
  })
  
  
  # indic
  indic<-reactive({req(input$file4)
    df <- data.matrix(read.csv(input$file4$datapath,
                               header = input$header4,
                               sep = input$sep4))
    return(df)
  })
  output$contents4 <- renderTable({
    return(indic())
  })
  
  output$ic <- renderUI({
    if (input$method %in% c('GVEMM', 'IW-GVEMM'))
      sliderInput('gic_c', 'Penalty constant for GIC', 0, 1, 0.7, 0.1)
    else
      selectInput("Type", 
                  label = "Choose a DIF type",
                  choices = list("Uniform"='T', 
                                 "Non-uniform"='F'),
                  selected = "T")
  })
  
  result0 <- eventReactive(input$go1, {
    if (input$method %in% c('GVEMM', 'IW-GVEMM')) {
      progress <- shiny::Progress$new()
      progress$set(message = "Trying different lambdas: ", value = 0)
      on.exit(progress$close())
      
      method <- if (input$method == 'GVEMM') gvemm else iwgvemm
      method(u(), t(indic()), Group(), input$gic_c, progress)
    } else {
      # Create a Progress object
      progress <- shiny::Progress$new()
      progress$set(message = "Iteration times", value = 0)
      # Close the progress when this reactive exits (even if there's an error)
      on.exit(progress$close())
      updateProgress <- function(value = NULL, detail = NULL) {
        if (is.null(value)) {
          value <- progress$getValue()
          value <- value + (progress$getMax() - value) /4
        }
        progress$set(value = value, detail = detail)
      }
      if (input$method == "EM") {
        Method="EM"
      } 
      if (input$method == "EMM") {
        Method="EMM"
      } 
      if (input$method == "Adapt") {
        Method="Adapt"
      } 
      if (input$method == "GVEMM") {
        Method="GVEMM"
      } 
      if (input$Type == "T") {
        Unif=T
      } 
      if (input$Type == "F") {
        Unif=F
      } 
      if (input$method == "LRT") {
        result=LRT_function(u(),indic(),as.character(Group()),Unif)
      } else {
        result=reg_DIF_alllbd(u(),indic(),as.character(Group()),Method,Unif,updateProgress)
      }
      return(result)
    }
  })
  
  output$warn<-renderText({
    input$go1
    isolate({
      if (input$method == "LRT") {
        return(NULL)
      } else if (!(input$method %in% c('GVEMM', 'IW-GVEMM'))) {
        if(result0()$lbd == min(result0()$lbd.vec) || result0()$lbd == max(result0()$lbd.vec)){
          return("Warning: The optimal penalty parameter may be out of range")
        }else{
          return(NULL)
        }
      }
    })
  })
  
  #(lbd=lbd,Gamma=Gamma,Beta=Beta,Amat=Amat,Dmat=Dmat,Sig=Sig,ICs=ICs)
  output$par1<-renderTable({
    input$go1
    isolate({
      if (input$method %in% c('GVEMM', 'IW-GVEMM')) {
        result <- result0()[[1]]
        out.a <- as.data.frame(result$a)
        colnames(out.a) <- paste0('a', 1:ncol(out.a))
        out.gamma <- aperm(result$gamma[-1, , , drop = F], c(2, 3, 1))
        dim.gamma <- dim(out.gamma)
        dim(out.gamma) <- c(dim.gamma[1], prod(dim.gamma[-1]))
        colnames(out.gamma) <- paste0('gamma', rep(2:(dim.gamma[3] + 1), each = dim.gamma[2]), 1:dim.gamma[2])
        out.beta <- t(result$beta[-1, , drop = F])
        colnames(out.beta) <- paste0('beta', 2:(dim.gamma[3] + 1))
        out <- cbind(out.a, b = result$b, out.gamma, out.beta)
        rownames(out) <- paste0('Item', 1:nrow(out))
        out
      } else {
        if (input$method == "LRT"){
          domain=result0()$domain
          y=result0()$y
          m<-cbind(result0()$Amat,result0()$Dmat)
          if (y==2){
            for (r in 1:domain){
              m<-cbind(m,result0()$Gamma[,r,])
            }
          } else {
            for (r in 1:domain){
              m<-cbind(m,t(result0()$Gamma[,r,]))
            }
          }
          m<-cbind(m,result0()$Beta)
          gp=NULL
          #for(yy in 1:(y-1)){
          #  gp=c(gp,rep(yy,domain))
          #}
          #colnames(m)<-c(paste("a",1:(result0()$domain),sep=""),"b",paste(rep(paste("gamma",1:domain,sep=""),(y-1)),gp,sep=""),paste("beta",1:(y-1),sep=""))
          #rownames(m)<-c(paste("Item",1:ncol(indic),sep=""))
          for(r in 1:domain){
            gp=c(gp,rep(r,y-1))
          }
          colnames(m)<-c(paste("a",1:(result0()$domain),sep=""),"b",paste(rep(paste("gamma",1:(y-1),sep=""),result0()$domain),gp,sep=""),paste("beta",1:(y-1),sep=""))
        } else {
          domain=result0()$domain
          y=result0()$y
          m<-cbind(result0()$Amat,result0()$Dmat)
          if (y==2){
            for (r in 1:domain){
              m<-cbind(m,result0()$Gamma[,r,,1])
            }
          } else {
            for (r in 1:domain){
              m<-cbind(m,t(result0()$Gamma[,r,,1]))
            }
          }
          m<-cbind(m,result0()$Beta)
          gp=NULL
          #for(yy in 1:(y-1)){
          #  gp=c(gp,rep(yy,domain))
          #}
          #colnames(m)<-c(paste("a",1:(result0()$domain),sep=""),"b",paste(rep(paste("gamma",1:domain,sep=""),(y-1)),gp,sep=""),paste("beta",1:(y-1),sep=""))
          #rownames(m)<-c(paste("Item",1:ncol(indic),sep=""))
          for(r in 1:domain){
            gp=c(gp,rep(r,y-1))
          }
          colnames(m)<-c(paste("a",1:(result0()$domain),sep=""),"b",paste(rep(paste("gamma",1:(y-1),sep=""),result0()$domain),gp,sep=""),paste("beta",1:(y-1),sep=""))
        }
        return(m)
      }
    })
  }, rownames = T)
  
  output$mean1<-renderTable({
    input$go1
    isolate({
      if (input$method %in% c('GVEMM', 'IW-GVEMM')) {
        result <- result0()[[1]]
        mu <- result$Mu
        rownames(mu) <- paste0('Group', 1:nrow(mu))
        colnames(mu) <- paste0('Dim', 1:ncol(mu))
        mu
      } else {
        m<-matrix(result0()$Mu)
        rownames(m)<-paste("group",rep(1:(result0()$y),each=result0()$domain)," dimension",rep(1:(result0()$domain),result0()$y),sep="")
        return(m)
      }
    })
  }, rownames = T)
  
  output$cov1<-renderTable({
    input$go1
    isolate({
      if (input$method %in% c('GVEMM', 'IW-GVEMM')) {
        result <- result0()[[1]]
        sigma <- aperm(result$Sigma, c(2, 3, 1))
        dim.sigma <- dim(sigma)
        dim(sigma) <- c(dim.sigma[1], prod(dim.sigma[-1]))
        sigma <- t(sigma)
        rownames(sigma) <- paste0('Group', rep(1:dim.sigma[3], each = dim.sigma[1]), ' Dim', 1:dim.sigma[1])
        colnames(sigma) <- paste0('Dim', 1:dim.sigma[1])
        sigma
      } else {
        m<-result0()$Sig
        colnames(m)<-paste("dimension",1:(result0()$domain),sep="")
        rownames(m)<-paste("group",rep(1:(result0()$y),each=result0()$domain)," dimension",rep(1:(result0()$domain),result0()$y),sep="")
        return(m)
      }
    })
  }, rownames = T)
  
  output$plot <- renderPlot({
    input$go1
    isolate({
      if (input$method %in% c('GVEMM', 'IW-GVEMM')) {
        ic <- result0()[[2]]
        plot(ic ~ lambda0, ic, xlab = expression(lambda[0]), ylab = 'GIC', type = 'b')
      } else if (input$method == "LRT"){
        return(NULL)
      } else {
        bic=result0()$ICs
        eta=result0()$lbd.vec
        plot(eta,bic)
      }
    })
  })
  #Downloadable csv of selected dataset ----
  
  output$downloadData <- downloadHandler(
    filename = function() {
      if(input$checkGroup1=="all"){
        paste("RegDIF",input$checkGroup1 ,"results.rds",sep="")}else{
          paste("RegDIF",input$checkGroup1 ,"results.csv",sep="")}
      
    },
    content = function(file) {
      if(input$checkGroup1=="all"){
        saveRDS(result0(), file)
      }else if(input$checkGroup1=="item"){
        if (input$method %in% c('GVEMM', 'IW-GVEMM')) {
          isolate({
            result <- result0()[[1]]
            out.a <- as.data.frame(result$a)
            colnames(out.a) <- paste0('a', 1:ncol(out.a))
            out.gamma <- aperm(result$gamma[-1, , , drop = F], c(2, 3, 1))
            dim.gamma <- dim(out.gamma)
            dim(out.gamma) <- c(dim.gamma[1], prod(dim.gamma[-1]))
            colnames(out.gamma) <- paste0('gamma', rep(2:(dim.gamma[3] + 1), each = dim.gamma[2]), 1:dim.gamma[2])
            out.beta <- t(result$beta[-1, , drop = F])
            colnames(out.beta) <- paste0('beta', 2:(dim.gamma[3] + 1))
            out <- cbind(out.a, b = result$b, out.gamma, out.beta)
            rownames(out) <- paste0('Item', 1:nrow(out))
            write.csv(out, file)
          })
        } else {
          if (input$method == "LRT"){
            domain=result0()$domain
            y=result0()$y
            m<-cbind(result0()$Amat,result0()$Dmat)
            if (y==2){
              for (r in 1:domain){
                m<-cbind(m,result0()$Gamma[,r,])
              }
            } else {
              for (r in 1:domain){
                m<-cbind(m,t(result0()$Gamma[,r,]))
              }
            }
            m<-cbind(m,result0()$Beta)
            gp=NULL
            #for(yy in 1:(y-1)){
            #  gp=c(gp,rep(yy,domain))
            #}
            #colnames(m)<-c(paste("a",1:(result0()$domain),sep=""),"b",paste(rep(paste("gamma",1:domain,sep=""),(y-1)),gp,sep=""),paste("beta",1:(y-1),sep=""))
            #rownames(m)<-c(paste("Item",1:ncol(indic),sep=""))
            for(r in 1:domain){
              gp=c(gp,rep(r,y-1))
            }
            colnames(m)<-c(paste("a",1:(result0()$domain),sep=""),"b",paste(rep(paste("gamma",1:(y-1),sep=""),result0()$domain),gp,sep=""),paste("beta",1:(y-1),sep=""))
            
          } else {
            domain=result0()$domain
            y=result0()$y
            m<-cbind(result0()$Amat,result0()$Dmat)
            if (y==2){
              for (r in 1:domain){
                m<-cbind(m,result0()$Gamma[,r,,1])
              }
            } else {
              for (r in 1:domain){
                m<-cbind(m,t(result0()$Gamma[,r,,1]))
              }
            }
            m<-cbind(m,result0()$Beta)
            gp=NULL
            #for(yy in 1:(y-1)){
            #  gp=c(gp,rep(yy,domain))
            #}
            #colnames(m)<-c(paste("a",1:(result0()$domain),sep=""),"b",paste(rep(paste("gamma",1:domain,sep=""),(y-1)),gp,sep=""),paste("beta",1:(y-1),sep=""))
            #rownames(m)<-c(paste("Item",1:ncol(indic),sep=""))
            for(r in 1:domain){
              gp=c(gp,rep(r,y-1))
            }
            colnames(m)<-c(paste("a",1:(result0()$domain),sep=""),"b",paste(rep(paste("gamma",1:(y-1),sep=""),result0()$domain),gp,sep=""),paste("beta",1:(y-1),sep=""))
          }
          rownames(m)<-c(paste("Item",1:ncol(indic()),sep=""))
          write.csv(m,file)
        }
      }else if(input$checkGroup1=="cov"){
        if (input$method %in% c('GVEMM', 'IW-GVEMM')) {
          isolate({
            result <- result0()[[1]]
            sigma <- aperm(result$Sigma, c(2, 3, 1))
            dim.sigma <- dim(sigma)
            dim(sigma) <- c(dim.sigma[1], prod(dim.sigma[-1]))
            sigma <- t(sigma)
            rownames(sigma) <- paste0('Group', rep(1:dim.sigma[3], each = dim.sigma[1]), ' Dim', 1:dim.sigma[1])
            colnames(sigma) <- paste0('Dim', 1:dim.sigma[1])
            write.csv(sigma, file)
          })
        } else {
          m1<-matrix(result0()$Mu)
          m2<-result0()$Sig
          m=cbind(m1,m2)
          colnames(m)<-c(paste("Mean"),paste("Var dimension",1:(result0()$domain),sep=""))
          rownames(m)<-paste("group",rep(1:(result0()$y),each=result0()$domain)," dimension",rep(1:(result0()$domain),result0()$y),sep="")
          write.csv(m,file)
        }
      }
    }
  )
}
# Run the app ----
shinyApp(ui, server)
