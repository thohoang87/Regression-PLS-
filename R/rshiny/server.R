
# Define server logic to read selected file ----
server <- function( input, output,session) {
  ################ Pour l'onglet "Data view":###################
# read the first data file which is used in training:  
  data1 <- eventReactive(input$file1,{
    inFile <- input$file1
    if (is.null(inFile)) {return(NULL)
    } else {
      tryCatch(
        {
          if(stringr::str_ends(input$file1$datapath, "csv")) {
            read.table(input$file1$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote)
          } else if (stringr::str_ends(input$file1$datapath, "(xlsx|xls)")) {
            readxl::read_excel(input$file1$datapath,
                               col_names = input$header,
                               sheet=input$sheet1)
          }
        },
        error = function(e) {
          # return a safeError if a parsing error occurs
          stop(safeError(e))
        }
        )
    }
  })
  
  # read the second data file which is used in prediction:  
  data2 <- eventReactive(input$file2,{
    inFile2 <- input$file2
    if (is.null(inFile2)) {return(NULL)
    } else {
      tryCatch(
        {
          if(stringr::str_ends(input$file2$datapath, "csv")) {
            read.table(input$file2$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote)
          } else if (stringr::str_ends(input$file2$datapath, "(xlsx|xls)")) {
            readxl::read_excel(input$file2$datapath,
                               col_names = input$header,
                               sheet=input$sheet2)
          }
        },
        error = function(e) {
          # return a safeError if a parsing error occurs
          stop(safeError(e))
        }
        )
    }
  })
 
# view 10 first rows of data frame or summary of data frame for the first data file
  output$contents1 <- renderReactable({
    if(input$disp1 == "head_training") {
      return(reactable(data1(), 
                       resizable = TRUE, wrap = FALSE, bordered = TRUE
                       ))
    }
    else if (input$disp1 == "summary_training"){
      return(reactable(summary(data1())))
    }
  })

  # view 10 first rows of data frame or summary of data frame for the second data file 
  output$contents2 <- renderReactable({
    if(input$disp2 == "head_prediction") {
      return(reactable(data2()))
    }
    else if (input$disp2 == "summary_prediction"){
      return(reactable(summary(data2())))
    } 
  })
  
  ################# Pour l'onglet "Define status": ##########################
# update the dropdown to choose columns from uploaded data:
  observeEvent(data1(),{
    updateSelectInput(session,
                         "x",
                      label = "Select x variable",
                         choices = colnames(data1()))
    updateSelectInput(session,
                         "y",
                      label = "Select y variable",
                         choices = colnames(data1()))
  })
  
  #define X and Y variables for training:
  X <- eventReactive(input$viewx,{
    X <- data1() %>%
    select(input$x)
    return(X)
})
  Y <- eventReactive(input$viewy,{
    Y <- data1() %>%
      select(input$y)
    return(Y)
  })
  
  #view data frame of X:
  output$definex <- renderReactable({
    reactable(X(),
              resizable = TRUE, wrap = FALSE, bordered = TRUE)
  })
  
  #view data frame of Y:
  output$definey <- renderReactable({
    reactable(Y(),
              resizable = TRUE, wrap = FALSE, bordered = TRUE)
  })
  
  
  # define X for predicion (for the second data file)
  X_pred <- eventReactive(input$viewx,{
    X_pred <- data2() %>%
      select(input$x)
    return(X_pred)
  })
  
  #################### Pour l'onglet "Fit values": ##########################
  # split data1 on train:
  train <- eventReactive(input$viewy,{
    train <- data1()%>%select(input$x,input$y)
    train<-train_test_split(train)$Df_Train
    names(train)[names(train)==input$y] <- "class"
    return(train)})

  # split data1 on test:
  test <- eventReactive(input$viewy,{
    test <- data1()%>%select(input$x,input$y)
    test <- train_test_split(data1())$Df_Test
    names(test)[names(test)==input$y] <- "class"
    return(test)})
  
  # split data1 on test which contains explained variables:
  test_x <- eventReactive(input$viewy,{
    test_x <- test() %>% select(input$x)
    return(test_x)})
  
  # split data1 on test which contains target:
  test_y <- eventReactive(input$viewy,{
    test_y <- test() %>% select("class")
    return(test_y)})
 
  # transform cible to class:
  data_base <- eventReactive(input$viewy,{
    data = data1()
    names(data)[names(data)==input$y] <- "class"
    return(data)})
  
  # Fit for training data:
  OutFit <- eventReactive(input$viewfit,{
    
      if (input$Select_Train_data == "Train"){
        
        fit_launch <- fit(class~.,train(),var.select = TRUE,
                          algorithm=input$algorithm,
                          threshold.algorithm=input$threshold.algorithm,
                          iter.max=input$iter.max,
                          threshold.vip=input$threshold.vip,
                          ncomp = input$ncomponents)
      }else {
        fit_launch <- fit(class~.,data_base(),var.select = TRUE,
                          algorithm=input$algorithm,
                          threshold.algorithm=input$threshold.algorithm,
                          iter.max=input$iter.max,
                          threshold.vip=input$threshold.vip,
                          ncomp = input$ncomponents)
      }
      
    
    if (input$fit=="Classification") {
      res <- fit_launch$calc$classification 
    } else if (input$fit=="Xweights") {
      res <-  fit_launch$calc$x_weights
    } else if (input$fit=="Yweights") {
      res <- fit_launch$calc$y_weights
    } else if (input$fit=="Xscores") {
      res <-  fit_launch$calc$Scores_X
    } else if (input$fit=="Yscores") {
      res <- fit_launch$calc$Scores_Y
    } else if (input$fit=="Xloadings") {
      res <-  fit_launch$calc$Loadings_X
    } else if (input$fit=="Yloadings") {
      res<- fit_launch$calc$Loadings_Y
    } else {res <- NULL}
    error = function(e) {
      # return a safeError if a parsing error occurs
      stop(safeError(e))
    }
    return(res)
  })
  
  
  output$fitvalue<- renderReactable({
    reactable(OutFit(),resizable = TRUE, wrap = FALSE, bordered = TRUE)
  })
  
  
  #################### Pour l'onglet "Prediction": ##########################
  # prediction for training data:
  OutPred <- eventReactive(input$viewpred,{
    fit_launch <- fit(class~.,train(),var.select = TRUE,
                      algorithm=input$algorithm,
                      threshold.algorithm=input$threshold.algorithm,
                      iter.max=input$iter.max,
                      threshold.vip=input$threshold.vip,
                      ncomp = input$ncomponents)
    # return class:
    pred_train <- predict(fit_launch,test_x(),type="class")
    # return probability:
    pred_train_pos <- predict(fit_launch,test_x(),type="posterior")
    # confusion matrix:
    t <- table(test()$class,pred_train$Class)
    # Accuracy of the model:
    acc <- sum(diag(t))/sum(t)
    # Precision:
    precision <- diag(t) / colSums(t)
    # Recall:
    recall <- diag(t) / rowSums(t)
    # return f1 score of the model:
    f1 <- ifelse(precision + recall == 0, 0, 2 * precision * recall / (precision + recall))
    if (input$pred1=="Class") {
      res <- pred_train$Class
    } else if (input$pred1=="Probability") {
      res <- pred_train_pos$Probability
    } else if (input$pred1=="Confusion_Matrix") {
      res <- t
    } else if (input$pred1=="Accuracy") {
      res <- acc
    } else if (input$pred1=="Precision") {
      res <- precision
    } else if (input$pred1=="F1_Score") {
      res <-  f1
    } else if (input$pred1=="Recall") {
      res<- recall
    } else {res <- NULL}
    error = function(e) {
      # return a safeError if a parsing error occurs
      stop(safeError(e))
    }
    return(res)
  })
  
  
  output$predictvalues1<- renderReactable({
    reactable(as.data.frame(OutPred()),resizable = TRUE, wrap = FALSE, bordered = TRUE)
  })
  
  #prediction for prediction data:
  result <- eventReactive(input$class,{
    fit_launch <- fit(class~.,train(),var.select = TRUE,
                      algorithm=input$algorithm,
                      threshold.algorithm=input$threshold.algorithm,
                      iter.max=input$iter.max,
                      threshold.vip=input$threshold.vip,
                      ncomp = input$ncomponents)
    pred <- predict(fit_launch,X_pred(),type="class")
    res_result <- pred$Class
    error = function(e) {
      # return a safeError if a parsing error occurs
      stop(safeError(e))
    }
    return(res_result)
  })
  
  output$predictvalues2 <- renderReactable({
    reactable(as.data.frame(result()),resizable = TRUE, wrap = FALSE, bordered = TRUE)
  })
  
  #Downloadable csv of result
  output$downloadcsv <- downloadHandler(
    filename = function() {
      paste("result", ".csv", sep = "")},
    content = function(file) {
      write.csv(result(), file, row.names = TRUE)}
  )
  
  #Downloadable excel of result
  output$downloadexcel <- downloadHandler(
    filename = function() {
      paste("result", ".xlsx", sep = "")},
    content = function(file) {
      xlsx::write.xlsx(result(), file, row.names = TRUE)}
  )
  #################### Pour l'onglet "Graphics": ############################
  
  
  #choose the first component
  observeEvent(data_base(),{
    
    updateSelectInput(session,
                      "comp1",
                      label = "Select the first component",
                      choices = c(1:(length(colnames(data_base()))-1)),
                      selected = 1)
  })
  
  comp1 <- eventReactive(input$comp1,{
    
    return(as.numeric(input$comp1))
  })
  
  
  #choose the second component
  observeEvent(data_base(),{
    
    updateSelectInput(session,
                      "comp2",
                      label = "Select the second component",
                      choices = c(1:(length(colnames(data_base()))-1)),
                      selected = 2)
  })
  
  comp2 <- eventReactive(input$comp2,{
    
    return(as.numeric(input$comp2))
  })
  
  
  
  #choose th number of components
  observeEvent(data_base(),{
    
    updateSelectInput(session,
                      "ncomp",
                      label = "Select number of components",
                      choices = c(2:(length(colnames(data_base()))-1)),
                      selected = 2)
  })
  
  ncomp <- eventReactive(input$ncomp,{
    
    return(as.numeric(input$ncomp))
  })
  
  
  #plot the first graph list 
  graph_1 <- eventReactive(input$viewgraph_1,{
    
    fit_launch <- fit(class~.,data_base(),var.select = FALSE, ncomp = length(colnames(X())))
    
    if(input$Graphic_1 == "Circle_Correlation_plot"){
      
      graph1 <- plotVar.PlsDA(fit_launch,comp1 = comp1(),comp2 = comp2())
    }
    else if (input$Graphic_1 == "Individu_plot"){
      
      graph1 <- plotInd.PlsDA(fit_launch,comp1 = comp1(),comp2 = comp2())
    }
    else if (input$Graphic_1 == "Correlation_plot"){
      
      graph1 <- correlationplot_plsDA(fit_launch,usedcomp = comp1())
    }
    
    return(graph1)
  })
  
  
  #plot the second graph list
  graph_2 <- eventReactive(input$viewgraph_2,{

      fit_launch <- fit(class~.,data_base(),var.select = FALSE, ncomp = ncomp())
    
    if(input$Graphic_2 == "Cos2plot"){
      
      graph2 <- Cos2.PlsDA(fit_launch)
    }
    else if (input$Graphic_2 == "Contribution_variable"){
      
      graph2 <- contribvar.PlsDA(fit_launch)
    }
    else if (input$Graphic_2 == "Screeplot"){
      
      graph2 <- screeplot_plsDA(fit_launch)
    }
    
    else if (input$Graphic_2 == "Plot_Vip"){
      
      fit_launch <- fit(class~.,data_base(),var.select = T, ncomp = ncomp())
      graph2 <- plot.vip(fit_launch$VIP)
    }
    
    return(graph2)
  })
  
  
  output$plotly1 <- renderPlotly({
    graph_1()
  })
  
  output$plotly2 <- renderPlotly({
    graph_2()
  })
}

