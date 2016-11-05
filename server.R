library(shiny)
library(ggplot2)

function(input, output) {
  
  
  setwd("E:/GitHub/R-FB-PG3")
  train_ini <- read.csv("ZUP_M_h10.csv",header=TRUE, sep=";", row.names=NULL)
  train_ini$sl <- 0 
  test <- read.csv("ZUP_M_h16.csv",header=TRUE, sep=";", row.names=NULL)
  test$sl <- 1 
  train <- rbind(train_ini, test)
  
  # there is an incorrect prediction where is only target product
  train=train[!(train$isTarget == 1 & train$isNotOnlyTarget ==0),] #excuding the clients with the only target product
  
  anColName = 'isTarget'
  idColName = 'id'
  
  # #for Server
  # train$isLicense <- NULL #Home, CHANGE IT!
  # train$isServer <- NULL #Target, CHANGE IT!
  # train$name <- NULL #client's name
  # # context Server
  # train$isBS <- NULL #context
  # train$isRep <- NULL #context
  
  #for ZUP
  train$isBuh <- NULL #Home, CHANGE IT!
  train$isZUP <- NULL #Target, CHANGE IT!
  
  # # context ZUP
  train=train[train$cEmpl >= 2,] #2 for ZUP all
  train$isEmpl20 <- 0
  train$isEmpl20[train$cEmpl >= 22] <- 1
  
  # train$cEmpl[train$cEmpl == 0] = NA
  # cEmpl <- rpart(cEmpl ~ branch + sS + sG, data=train[!is.na(train$cEmpl),], method="anova")
  # train$cEmpl[is.na(train$cEmpl)] <- predict(cEmpl, train[is.na(train$cEmpl),])
  
  #for everything
  train$name <- NULL #client's name
  train$cCust <- NULL #number of clients
  
  
  #restrict the dataset with Head or|and something else 
  # train=train[train$sGS >10000,]
  train=train[train$isHead == 1,]
  train$isHead <- NULL #client's name
  # train=train[train$sTotal !=0,]
  # train=train[train$isBuh !=0,]
  # train=train[train$sGS >30000,]
  
  #find the clients with Head and Target together 
  train=train[train$isSameOrNever == 1,]
  
  # доля купивших 
  nrow(train[train$isTarget == 1,])/nrow(train)
  
  # доля купивших вместе с Head среди купивших
  nrow(train[train$isSameDay,]) / nrow(train[train$isTarget == 1,])
  
  train$branch0 <- as.numeric(train$branch)
  train$branch0 <- paste0(as.character(train$branch0))
  train$branch0 <- factor(train$branch0)
  
  train$branchN <- paste(train$branch0,train$branch, sep = '=')
  branches <- data.frame(table(train$branchN))
  train$branch <- NULL 
  train$branchN <- NULL 
  
  #mask the prediction colomns
  train$isNotOnlyTarget <- NULL #=Target
  train$cDaysBetween <- NULL #Days between Head & Target
  train$isSameDay <- NULL #Days between Head & Target
  train$isLater <- NULL #Days between Head & Target
  train$isLaterOrNever <- NULL #Days between Head & Target
  train$isSameOrNever <- NULL #Days between Head & Target
  train$isEarlier <- NULL #Days between Head & Target
  
  #split back the test and train sets
  test <- train[train$sl == 1,]
  train <- train[train$sl == 0,]
  test$sl <- NULL
  train$sl <- NULL
  
  #Split in-train and in-test
  set.seed(222)
  split <- createDataPartition(train[,anColName], p = 1/2, list = FALSE)
  train0 <- slice(train, split)
  train1 <- slice(train, -split)
  
  ########################################################
  
  # Prediction
  
  
  all_var_PA = sort(names(train[colSums(is.na(train)) == 0]))
  all_var_NA = sort(names(train[colSums(is.na(train)) != 0]))
  all_var_PA <- all_var_PA[!is.element(all_var_PA,c(anColName,idColName,'sTarget'))]
  
  output$featureControls <- renderUI({
    checkboxGroupInput("features", "Choose features", all_var_PA, selected = all_var_PA)
  })
  
  
  output$plot <- renderPlot({
    if (is.null(input$features)) {return()} 
    parseText_eval <- paste0('as.factor(', anColName,') ~ ', paste0(input$features, collapse = " + "))
    
    fit <- randomForest(eval(parse(text=parseText_eval)), data=train1[!is.na(train1[,anColName]),], importance=TRUE, ntree=200)
    # fit <- randomForest(as.factor(isTarget) ~ cEmpl + branch0  + isUT, data=train1, importance=TRUE, ntree=200)
    
    varImpPlot(fit)
    

  }, height=700)
  
  
  output$summary <- renderPrint({
    paste0('as.factor(', anColName,') ~ ', paste0(input$features, collapse = " + "))
  })

  
}
