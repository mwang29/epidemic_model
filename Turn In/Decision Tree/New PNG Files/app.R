knitr::opts_chunk$set(echo=FALSE, message=FALSE)

library(dplyr)
library(rpart)
library(rpart.plot)
library(rpart.utils)

HIGHLIGHT_COLOR <- "red"
#setwd("E:/DecisionTreeShiny")
DATA_DIR <- "E:/DecisionTreeShiny"

DATA_MODELS <- gsub("\\.Rds$", "", list.files(DATA_DIR, pattern="*.Rds")) # Returns list of all files with a .Rds extension in the directory
DATA_MODELS1<-c(DATA_MODELS[1], DATA_MODELS[12], DATA_MODELS[22],DATA_MODELS[29], DATA_MODELS[30], DATA_MODELS[31],  # Reordered list due to selectedInput misordering
               DATA_MODELS[32], DATA_MODELS[33], DATA_MODELS[34],  DATA_MODELS[2], DATA_MODELS[3], DATA_MODELS[4], DATA_MODELS[5],
               DATA_MODELS[6], DATA_MODELS[7], DATA_MODELS[8], DATA_MODELS[9], DATA_MODELS[10], DATA_MODELS[11],
               DATA_MODELS[13],DATA_MODELS[14],DATA_MODELS[15], DATA_MODELS[16], DATA_MODELS[17],DATA_MODELS[18],
               DATA_MODELS[19],DATA_MODELS[20],DATA_MODELS[35],
               DATA_MODELS[21], DATA_MODELS[36], DATA_MODELS[23],DATA_MODELS[24], DATA_MODELS[25],
               DATA_MODELS[26], DATA_MODELS[27], DATA_MODELS[28])

#DATA_MODELS[23],
loadTree <- function(data_file){ # Function for loading the Rds files
  tree <- readRDS(file.path(DATA_DIR, paste0(data_file, ".Rds")))
  tree
}

best_cp <- function(TREE){
  best_cp_row <- which(TREE$cptable[,'xerror'] == min(TREE$cptable[,'xerror'])) # If there are ties, we'll take the last one
  TREE$cptable[max(best_cp_row),"CP"]
}

tree_performance_stats <- function(myTree){ 
  #here for numobs2 n_test was changed to n
  num_obs <- myTree$frame[1,"n"]
  num_obs2 <- myTree$frame[1,"n"]
  
  leaf_values <- myTree$frame[myTree$frame$var=="<leaf>", c("var","n","wt","dev", "yval","complexity", "ncompete", "nsurrogate")]
  leaf_values$percent <- 100 * leaf_values$n / num_obs
  leaf_values$percent_test <- 100 * leaf_values$n / num_obs2
  
  leaf_values <- leaf_values[order(leaf_values$yval, decreasing=TRUE),] # Order by model scores
  
  leaf_values <- transform(leaf_values, 
                           TP=n * yval, 
                           FP=n * (1 - yval),
                           TP_test = n * yval,
                           FP_test = n * (1 - yval))
  
  leaf_values <- transform(leaf_values,
                           pop_fraction = cumsum(n)/sum(n),
                           TPR=cumsum(TP)/sum(TP), 
                           FPR=cumsum(FP)/sum(FP),
                           pop_fraction_test = cumsum(n)/sum(n),
                           TPR_test=cumsum(TP)/sum(TP), 
                           FPR_test=cumsum(FP)/sum(FP))
  #num_obs2 <- leaf_values$[1,"n"]
  
  leaf_values
}

### 
get_node_id_by_liftclick <- function(click, leaf_val_df, dataset){
  pop_column <- if (dataset=="TEST") "pop_fraction_test" else "pop_fraction"
  selected_node <- row.names(leaf_val_df)[min(which((100*leaf_val_df[[pop_column]]) > click$x))]
  yval_column <- if (dataset=="TEST") "yval_test" else "yval"
  # if (100*leaf_val_df[selected_node, yval_column] < click$y) selected_node <- character(0) ## triggers redraw (???)
  if (is.na(selected_node)) selected_node <- character(0)
  return(selected_node)
}

get_node_id_by_treeclick <- function(plotted_tree, click){
  node_idx <- with(plotted_tree$boxes,(
    (x1 < click$x) & 
      (x2 > click$x) &
      (y1 < click$y) & 
      (y2 > click$y) 
  ))
  nodes <- row.names(plotted_tree$obj$frame)
  nodes[node_idx]
}

plot_tree <- function(pTree, dataset, selectedNode=character(0)){
  library(rpart.plot)
  # Copy test results over training results. Be aware that this only works for methods that
  # take all their numbers from the tree frame. Note that rpart.plot with extra=100 does NOT
  # compute percents correctly when we lie to it like this, but the counts seem to work right.
  if (dataset=="TEST"){ 
    pTree$frame$n <- pTree$frame$n
    pTree$frame$yval <- pTree$frame$yval
  }
  rpart.plot(pTree, 
             extra=1, #  display number, not percent in each node; percent is incorrect for test set.
             nn=TRUE, xflip=TRUE, # border.col
             shadow.col= if (length(selectedNode) == 0)
               'black'
             else
               ifelse(row.names(pTree$frame)==selectedNode, 
                      HIGHLIGHT_COLOR, 'black')) 
}

get_node_info <- function(nodeID, plotted_tree, dataset){
  leaf_val_df <- tree_performance_stats(plotted_tree)
  node_row <- plotted_tree$frame[nodeID,] # leaf_val_df
  if (dataset == "TEST"){
    num_cases <- node_row$n
    outcome <- node_row$yval
    total_cases <- sum(leaf_val_df$n)
    total_positive <- sum(leaf_val_df$TP)
  } else {
    num_cases <- node_row$n
    outcome <- node_row$yval
    total_cases <- sum(leaf_val_df$n)
    total_positive <- sum(leaf_val_df$TP)
  }
  paste0("nodeID=", nodeID,
         "\nnumber of cases=", num_cases, 
         " (", round(100*num_cases/total_cases, 2), "% of total)",
         "\npercent positive=", round(100 * outcome, 2),
         " (", outcome * num_cases, " positive cases)",
         "\noverall percent positive=", round(100 * total_positive/total_cases, 2),
         " (relative lift=", round(100*outcome/(total_positive/total_cases), 2),"%)",
         "\nrule=", node_row$rule
  )
}

plot_gain <- function(fullTree, prunedTree, dataset="TRAIN", selectedNode="None"){
  leaf_values_full <- tree_performance_stats(fullTree)
  leaf_values_pruned <- tree_performance_stats(prunedTree)
  
  # Cumulative gain curve
  if (dataset=="TEST"){
    test_vars <- grep("_test$", names(leaf_values_pruned), value=TRUE)
    training_vars <- gsub("_test$", "", test_vars)
    names(training_vars) <- test_vars
    
    for (var_name in names(training_vars)){
      leaf_values_full[[training_vars[var_name]]] <- leaf_values_full[[var_name]]
      leaf_values_pruned[[training_vars[var_name]]] <- leaf_values_pruned[[var_name]]
    }
  }
  
  with(leaf_values_full,{ 
    # Performance of full model
    plot(c(0, 100*pop_fraction), c(0, 100*TPR), type='l', lwd=2, col="orange", 
         main="Cumulative Gain Curve",
         xlab="Percent of population", ylab="Percent of all positive cases")
    # Ideal performance
    lines(c(0, 100*sum(TP)/sum(n), 100), c(0, 100, 100), lwd=2, lty=3, col="gray")
  })
  highlighted_point <- c(FALSE, row.names(leaf_values_pruned) == selectedNode)
  with(leaf_values_pruned,{
    lines(c(0, 100*pop_fraction), c(0, 100*TPR), 
          col=ifelse(highlighted_point, HIGHLIGHT_COLOR, "black"), 
          pch=ifelse(highlighted_point, 19, 1),
          lwd=2, lty=2, type='b')
    if(any(highlighted_point)){
      x0 <- 100*c(0,pop_fraction)[which(highlighted_point) - 1]
      y0 <- 100*c(0,TPR)[which(highlighted_point) - 1]
      lines(c(0, x0, x0, x0), c(y0, y0, 0, 0), col="darkgreen", lty=3)
      x <- 100*c(0,pop_fraction)[highlighted_point]
      y <- 100*c(0,TPR)[highlighted_point]
      lines(c(0, x, x, x), c(y, y, 0, 0), col=HIGHLIGHT_COLOR, lty=3)
    }
  })
  abline(0, 1, col="blue")  
  
}


plot_lift <- function(prunedTree, dataset="TRAIN", selectedNode=character(0)){
  lvp <- tree_performance_stats(prunedTree) # leaf_values, pruned
  bar_colors <- ifelse( row.names(lvp) == selectedNode, HIGHLIGHT_COLOR, "darkgray")
  if (length(bar_colors) == 0) bar_colors <- "darkgray"
  
  # Lift plot
  if (dataset=="TEST"){
    num_obs <-  sum(lvp$n_test)
    
    overall_value <- 100*sum(lvp$n_test * lvp$yval_test)/num_obs
    height_vec <- 100*lvp$yval_test
    width_vec <- lvp$percent_test
  } else {
    num_obs <- sum(lvp$n)
    
    overall_value <- 100*sum(lvp$n * lvp$yval)/num_obs
    height_vec <- 100*lvp$yval
    width_vec <- lvp$percent
  }
  
  barplot(height=height_vec, width=width_vec, space=0, col=bar_colors, xlim=c(0,100),
          main="Lift Plot", ylab="Percent cases positive", xlab="Percent of population")
  abline(h=overall_value, col="blue")
  
}

get_complexity_map <- function(rpart_tree){
  function(relative_complexity){
    opt_cp <- best_cp(rpart_tree)  
    min_cp <- min(rpart_tree$frame$complexity[rpart_tree$frame$complexity!=0])
    max_cp <- max(rpart_tree$frame$complexity)
    
    if ((opt_cp <= min_cp) | (opt_cp >= max_cp)){
      opt_cp <- (min_cp + max_cp)/2
      ### warn("tree may be undertrained")
    }
    # cp_levels - unique(TREE$frame$complexity)
    
    cp_map <- data.frame(
      log_cp = c(seq(log(max_cp), log(opt_cp), length=100), 
                 log(opt_cp),
                 seq(log(opt_cp), log(min_cp), length=100)),
      std_cp = seq(-100, 100, by=1)
    )
    exp(cp_map[cp_map$std_cp==relative_complexity, "log_cp"])
  }
}

library(shiny)

TREE <- loadTree(DATA_MODELS1[1])


ui <- fluidPage(
  fluidRow(
    column(12,
           inputPanel(
             selectInput("data_model", label = "Model file:",
                         choices = DATA_MODELS1),
             sliderInput("std_complexity", label = "Complexity adjustment:",
                         min = -100, max = 100, 
                         value = 0, 
                         step = 1),
             radioButtons("dataset", "data set to display:", 
                          choices = list(test="TEST", training="TRAIN"), 
                          selected = "TEST", inline = TRUE, width = NULL)
           )
    )
  ),
  fluidRow(
    column(8,
           fluidRow(plotOutput("treeplot", click = "treeplot_click")),
           fluidRow(verbatimTextOutput("node_info"))
    ),
    column(4,
           fluidRow(plotOutput("gainplot")),
           fluidRow(plotOutput("liftplot", click = "liftplot_click"))
    )
  )
)

# input <- list(data_model="big_spender", std_complexity=50, dataset="TRAIN")
server <- function(input, output) {
  
  CP <- NA
  SELECTED_NODE <- character(0)
  PLOTTED_TREE <- NULL
  PRUNED_TREE <- NULL
  STD_CP_FUN <- get_complexity_map(TREE)
  
  get_clicks <- reactive({
    # dataFile <- file.path(DATA_DIR, paste0(input$data_model, ".Rds"))
    TREE <<- loadTree(input$data_model)
    STD_CP_FUN <<- get_complexity_map(TREE)
    CP <<- STD_CP_FUN(input$std_complexity)
    PRUNED_TREE <<- prune(TREE, cp=CP)
    
    if (!is.null(input$liftplot_click)){
      prunedTree <- PRUNED_TREE
      leaf_val_df <- tree_performance_stats(prunedTree)
      SELECTED_NODE <<- get_node_id_by_liftclick(input$liftplot_click, leaf_val_df, input$dataset)
    } else if (!is.null(input$treeplot_click)){
      SELECTED_NODE <<- get_node_id_by_treeclick(PLOTTED_TREE, input$treeplot_click)
    }
    
  })
  
  
  output$treeplot <- renderPlot({
   if(!is.null(input$dataset)){
    get_clicks()
    prunedTree <- PRUNED_TREE  # prune(TREE, cp=CP)
    PLOTTED_TREE <<- plot_tree(prunedTree, input$dataset, SELECTED_NODE)
    # PRUNED_TREE <<- prunedTree  # save separately; PLOTTED_TREE$obj reorders segments (???)
  }
  })
  
  output$gainplot <- renderPlot({
    if(!is.null(input$dataset)){
     get_clicks()
     prunedTree <- PRUNED_TREE
     plot_gain(TREE, prunedTree, dataset=input$dataset, SELECTED_NODE)
    }
  })
  
  output$liftplot <- renderPlot({
    get_clicks()
    prunedTree <- PRUNED_TREE
    plot_lift(prunedTree, dataset=input$dataset, SELECTED_NODE)
  })
  
  output$node_info <- renderText({
    get_clicks()
    prunedTree <- PRUNED_TREE
    get_node_info(SELECTED_NODE, prunedTree, input$dataset)
  })
}
 
options <- list(width = 1000, height = 1000)

shinyApp(ui, server,  options=options)
