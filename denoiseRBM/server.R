
server <- function(input, output, session){
  #############################################################################
  # user arguments
  #############################################################################
  s_X <- reactive({
    if (input$X == "Corrupted (X_c)") "X_c" else "X_z"
  })
  s_A <- reactive({
    if (input$A == "Corrupted (A_c)") "A_c" else "A_z"
  })
  s_split <- reactive({
    if (input$split == "Validation") "val" else "test"
  })
  #############################################################################
  # MLP
  #############################################################################
  output$MLP_plot <- renderPlotly({
    # Get list
    if (input$dataset == "ogbn-arxiv"){
      df <- o_MLP_files
    } else if (input$dataset == "WikiCS"){
      df <- w_MLP_files
    } else if (input$dataset == "WikiCS-AWGN"){
      df <- wa_MLP_files
    }
    
    # Get sub-list
    suffix <- glue("{s_X()}_A_c_{s_split()}")
    x <- if (s_X() == "X_c") ~X_c else ~X_z
    
    # Plot sub-list
    if(!is.null(df[[suffix]])){
      fig <- plot_ly(data = df[[suffix]], x = x)
      if("MLP_x" %in% colnames(df[[suffix]]) && !is.null(df[[suffix]]$MLP_x)){
        fig <- fig %>% add_trace(y = ~MLP_x, name = "MLP:z0", mode = "lines+markers")
      }
      if("MLP_z0" %in% colnames(df[[suffix]]) && !is.null(df[[suffix]]$MLP_z0)){
        fig <- fig %>% add_trace(y = ~MLP_z0, name = "MLP:~z0", mode = "lines+markers")
      }
      if("MLP_z1" %in% colnames(df[[suffix]]) && !is.null(df[[suffix]]$MLP_z1)){
        fig <- fig %>% add_trace(y = ~MLP_z1, name = "MLP:~z1", mode = "lines+markers")
      }
      if("MLP_z2" %in% colnames(df[[suffix]]) && !is.null(df[[suffix]]$MLP_z2)){
        fig <- fig %>% add_trace(y = ~MLP_z2, name = "MLP:~z2", mode = "lines+markers")
      }
      if("MLP_z3" %in% colnames(df[[suffix]]) && !is.null(df[[suffix]]$MLP_z3)){
        fig <- fig %>% add_trace(y = ~MLP_z3, name = "MLP:~z3", mode = "lines+markers")
      }
    } else {
      fig <- plot_ly()
    }
    
    # Layout the plot
    fig <- fig %>% layout(title = glue("Full adjacency matrix"),
                          xaxis = list(title = glue("Distortion in {tolower(input$split)} node feature matrix ({s_X()})")),
                          yaxis = list (title = "Prediction accuracy"))
    fig
  })
  #############################################################################
  # n2v
  #############################################################################
  output$n2v_plot <- renderPlotly({
    # Get list
    if (input$dataset == "ogbn-arxiv"){
      df <- o_n2v_files
    } else if (input$dataset == "WikiCS"){
      df <- w_n2v_files
    } else if (input$dataset == "WikiCS-AWGN"){
      df <- wa_n2v_files
    }
    
    # Get sub-list
    suffix <- glue("{s_X()}_{s_A()}_{s_split()}")
    x <- if (s_X() == "X_c") ~X_c else ~X_z
    if (!is.null(df[[suffix]])){
      if (s_A() == "A_c"){
        reqd_df <- subset(df[[suffix]], A_c == input$n_A) 
      } else {
        reqd_df <- subset(df[[suffix]], A_z == input$n_A)
      }
    }
    
    # Plot sub-list
    if (exists("reqd_df")){
      fig <- plot_ly(data = reqd_df, x = x)
      if("n2v_x" %in% colnames(reqd_df) && !is.null(reqd_df$n2v_x)){
        fig <- fig %>% add_trace(y = ~n2v_x, name = "n2v:z0", mode = "lines+markers")
      }
      if("n2v_z0" %in% colnames(reqd_df) && !is.null(reqd_df$n2v_z0)){
        fig <- fig %>% add_trace(y = ~n2v_z0, name = "n2v:~z0", mode = "lines+markers")
      }
      if("n2v_z1" %in% colnames(reqd_df) && !is.null(reqd_df$n2v_z1)){
        fig <- fig %>% add_trace(y = ~n2v_z1, name = "n2v:~z1", mode = "lines+markers")
      }
      if("n2v_z2" %in% colnames(reqd_df) && !is.null(reqd_df$n2v_z2)){
        fig <- fig %>% add_trace(y = ~n2v_z2, name = "n2v:~z2", mode = "lines+markers")
      }
      if("n2v_z3" %in% colnames(reqd_df) && !is.null(reqd_df$n2v_z3)){
        fig <- fig %>% add_trace(y = ~n2v_z3, name = "n2v:~z3", mode = "lines+markers")
      }
    } else {
      fig <- plot_ly()
    }
    
    # Layout the plot
    if (input$n_A == 0){
      title <- glue("Full adjacency matrix")
    } else{
      second <- if (s_A() == "A_c") "corrupted" else "blanked out"
      if((s_A() == "A_z") && input$n_A > 40){
        title <- "node2vec embeddings are not generated because some nodes are isolated."
      } else {
        title <- glue("{input$n_A}% {second} adjacency matrix") 
      }  
    }
    fig <- fig %>% layout(title = title,
                          xaxis = list(title = glue("Distortion in {tolower(input$split)} node feature matrix ({s_X()})")),
                          yaxis = list (title = "Prediction accuracy"))
    fig
  })
  #############################################################################
  # GCN
  #############################################################################
  output$GCN_plot <- renderPlotly({
    # Get list
    if (input$dataset == "ogbn-arxiv"){
      df <- o_GCN_files
    } else if (input$dataset == "WikiCS"){
      df <- w_GCN_files
    } else if (input$dataset == "WikiCS-AWGN"){
      df <- wa_GCN_files
    }
    
    # Get sub-list
    suffix <- glue("{s_X()}_{s_A()}_{s_split()}")
    x <- if (s_X() == "X_c") ~X_c else ~X_z
    if(!is.null(df[[suffix]])){
      if (s_A() == "A_c"){
        reqd_df <- subset(df[[suffix]], A_c == input$n_A) 
      } else {
        reqd_df <- subset(df[[suffix]], A_z == input$n_A)
      }
    }
    
    # Plot sub-list
    if (exists("reqd_df")){
      fig <- plot_ly(data = reqd_df, x = x)
      if("GCN_x" %in% colnames(reqd_df) && !is.null(reqd_df$GCN_x)){
        fig <- fig %>% add_trace(y = ~GCN_x, name = "GCN:z0", mode = "lines+markers")
      }
      if("GCN_z0" %in% colnames(reqd_df) && !is.null(reqd_df$GCN_z0)){
        fig <- fig %>% add_trace(y = ~GCN_z0, name = "GCN:~z0", mode = "lines+markers")
      }
      if("GCN_z1" %in% colnames(reqd_df) && !is.null(reqd_df$GCN_z1)){
        fig <- fig %>% add_trace(y = ~GCN_z1, name = "GCN:~z1", mode = "lines+markers")
      }
      if("GCN_z2" %in% colnames(reqd_df) && !is.null(reqd_df$GCN_z2)){
        fig <- fig %>% add_trace(y = ~GCN_z2, name = "GCN:~z2", mode = "lines+markers")
      }
      if("GCN_z3" %in% colnames(reqd_df) && !is.null(reqd_df$GCN_z3)){
        fig <- fig %>% add_trace(y = ~GCN_z3, name = "GCN:~z3", mode = "lines+markers")
      }
    } else {
      fig <- plot_ly()
    }
    
    # Layout the plot
    if (input$n_A == 0){
      title <- glue("Full adjacency matrix")
    } else{
      second <- if (s_A() == "A_c") "corrupted" else "blanked out"
      title <- glue("{input$n_A}% {second} adjacency matrix")
    }
    fig <- fig %>% layout(title = title,
                          xaxis = list(title = glue("Distortion in {tolower(input$split)} node feature matrix ({s_X()})")),
                          yaxis = list (title = "Prediction accuracy"))
    fig
  })
  #############################################################################
  # SAGE
  #############################################################################
  output$SAGE_plot <- renderPlotly({
    # Get list
    if (input$dataset == "ogbn-arxiv"){
      df <- o_SAGE_files
    } else if (input$dataset == "WikiCS"){
      df <- w_SAGE_files
    } else if (input$dataset == "WikiCS-AWGN"){
      df <- wa_SAGE_files
    }
    
    # Get sub-list
    suffix <- glue("{s_X()}_{s_A()}_{s_split()}")
    x <- if (s_X() == "X_c") ~X_c else ~X_z
    if(!is.null(df[[suffix]])){
      if (s_A() == "A_c"){
        reqd_df <- subset(df[[suffix]], A_c == input$n_A) 
      } else {
        reqd_df <- subset(df[[suffix]], A_z == input$n_A)
      }
    }
    
    # Plot sub-list
    if (exists("reqd_df")){
      fig <- plot_ly(data = reqd_df, x = x)
      if("SAGE_x" %in% colnames(reqd_df) && !is.null(reqd_df$SAGE_x)){
        fig <- fig %>% add_trace(y = ~SAGE_x, name = "SAGE:z0", mode = "lines+markers")
      }
      if("SAGE_z0" %in% colnames(reqd_df) && !is.null(reqd_df$SAGE_z0)){
        fig <- fig %>% add_trace(y = ~SAGE_z0, name = "SAGE:~z0", mode = "lines+markers")
      }
      if("SAGE_z1" %in% colnames(reqd_df) && !is.null(reqd_df$SAGE_z1)){
        fig <- fig %>% add_trace(y = ~SAGE_z1, name = "SAGE:~z1", mode = "lines+markers")
      }
      if("SAGE_z2" %in% colnames(reqd_df) && !is.null(reqd_df$SAGE_z2)){
        fig <- fig %>% add_trace(y = ~SAGE_z2, name = "SAGE:~z2", mode = "lines+markers")
      }
      if("SAGE_z3" %in% colnames(reqd_df) && !is.null(reqd_df$SAGE_z3)){
        fig <- fig %>% add_trace(y = ~SAGE_z3, name = "SAGE:~z3", mode = "lines+markers")
      }
    } else {
      fig <- plot_ly()
    }
    
    # Layout the plot
    if (input$n_A == 0){
      title <- glue("Full adjacency matrix")
    } else{
      second <- if (s_A() == "A_c") "corrupted" else "blanked out"
      title <- glue("{input$n_A}% {second} adjacency matrix")
    }
    fig <- fig %>% layout(title = title,
                          xaxis = list(title = glue("Distortion in {tolower(input$split)} node feature matrix ({s_X()})")),
                          yaxis = list (title = "Prediction accuracy"))
    fig
  })
  #############################################################################
  # Playground
  #############################################################################
  output$compare_plot <- renderPlotly({
    # Get lists
    if (input$dataset == "ogbn-arxiv") {
      MLP_df <- o_MLP_files
      n2v_df <- o_n2v_files
      GCN_df <- o_GCN_files
      SAGE_df <- o_SAGE_files
    } else if (input$dataset == "WikiCS") {
      MLP_df <- w_MLP_files
      n2v_df <- w_n2v_files
      GCN_df <- w_GCN_files
      SAGE_df <- w_SAGE_files
    } else if (input$dataset == "WikiCS-AWGN") {
      MLP_df <- wa_MLP_files
      n2v_df <- wa_n2v_files
      GCN_df <- wa_GCN_files
      SAGE_df <- wa_SAGE_files
    }
    
    # Get sub-lists
    MLP_suffix <- glue("{s_X()}_A_c_{s_split()}")
    rest_suffix <- glue("{s_X()}_{s_A()}_{s_split()}")
    x <- if (s_X() == "X_c") ~X_c else ~X_z
    MLP_df <- MLP_df[[MLP_suffix]]
    if(!is.null(n2v_df[[rest_suffix]])){
      if (s_A() == "A_c"){
        n2v_df <- subset(n2v_df[[rest_suffix]], A_c == input$n_A)
      } else {
        n2v_df <- subset(n2v_df[[rest_suffix]], A_z == input$n_A)
      }
    }
    if(!is.null(GCN_df[[rest_suffix]])){
      if (s_A() == "A_c"){
        GCN_df <- subset(GCN_df[[rest_suffix]], A_c == input$n_A)
      } else {
        GCN_df <- subset(GCN_df[[rest_suffix]], A_z == input$n_A)
      }
    }
    if(!is.null(SAGE_df[[rest_suffix]])){
      if (s_A() == "A_c"){
        SAGE_df <- subset(SAGE_df[[rest_suffix]], A_c == input$n_A)
      } else {
        SAGE_df <- subset(SAGE_df[[rest_suffix]], A_z == input$n_A)
      }
    }
    
    # Plot sub-lists
    fig <- plot_ly()
    
    if (!is.null(MLP_df) && "MLP_x" %in% colnames(MLP_df) && "MLP:z0" %in% input$playground){
      fig <- fig %>% add_trace(data = MLP_df, x = x, y = ~MLP_x, name = "MLP:z0", mode = "lines+markers")
    }
    if (!is.null(MLP_df) && "MLP_z0" %in% colnames(MLP_df) && "MLP:~z0" %in% input$playground){
      fig <- fig %>% add_trace(data = MLP_df, x = x, y = ~MLP_z0, name = "MLP:~z0", mode = "lines+markers")
    } 
    if (!is.null(MLP_df) && "MLP_z1" %in% colnames(MLP_df) && "MLP:~z1" %in% input$playground){
      fig <- fig %>% add_trace(data = MLP_df, x = x, y = ~MLP_z1, name = "MLP:~z1", mode = "lines+markers")
    }
    if (!is.null(MLP_df) && "MLP_z2" %in% colnames(MLP_df) && "MLP:~z2" %in% input$playground){
      fig <- fig %>% add_trace(data = MLP_df, x = x, y = ~MLP_z2, name = "MLP:~z2", mode = "lines+markers")
    }
    if (!is.null(MLP_df) && "MLP_z3" %in% colnames(MLP_df) && "MLP:~z3" %in% input$playground){
      fig <- fig %>% add_trace(data = MLP_df, x = x, y = ~MLP_z3, name = "MLP:~z3", mode = "lines+markers")
    }
    
    if (exists("n2v_df") && "n2v_x" %in% colnames(n2v_df) && "n2v:z0" %in% input$playground){
      fig <- fig %>% add_trace(data = n2v_df, x = x, y = ~n2v_x, name = "n2v:z0", mode = "lines+markers")
    }
    if (exists("n2v_df") && "n2v_z0" %in% colnames(n2v_df) && "n2v:~z0" %in% input$playground){
      fig <- fig %>% add_trace(data = n2v_df, x = x, y = ~n2v_z0, name = "n2v:~z0", mode = "lines+markers")
    }
    if (exists("n2v_df") && "n2v_z1" %in% colnames(n2v_df) && "n2v:~z1" %in% input$playground){
      fig <- fig %>% add_trace(data = n2v_df, x = x, y = ~n2v_z1, name = "n2v:~z1", mode = "lines+markers")
    }
    if (exists("n2v_df") && "n2v_z2" %in% colnames(n2v_df) && "n2v:~z2" %in% input$playground){
      fig <- fig %>% add_trace(data = n2v_df, x = x, y = ~n2v_z2, name = "n2v:~z2", mode = "lines+markers")
    }
    if (exists("n2v_df") && "n2v_z3" %in% colnames(n2v_df) && "n2v:~z3" %in% input$playground){
      fig <- fig %>% add_trace(data = n2v_df, x = x, y = ~n2v_z3, name = "n2v:~z3", mode = "lines+markers")
    }
    
    if (exists("GCN_df") && "GCN_x" %in% colnames(GCN_df) && "GCN:z0" %in% input$playground){
    fig <- fig %>% add_trace(data = GCN_df, x = x, y = ~GCN_x, name = "GCN:z0", mode = "lines+markers")
    }
    if (exists("GCN_df") && "GCN_z0" %in% colnames(GCN_df) && "GCN:~z0" %in% input$playground){
      fig <- fig %>% add_trace(data = GCN_df, x = x, y = ~GCN_z0, name = "GCN:~z0", mode = "lines+markers")
    }
    if (exists("GCN_df") && "GCN_z1" %in% colnames(GCN_df) && "GCN:~z1" %in% input$playground){
      fig <- fig %>% add_trace(data = GCN_df, x = x, y = ~GCN_z1, name = "GCN:~z1", mode = "lines+markers")
    }
    if (exists("GCN_df") && "GCN_z2" %in% colnames(GCN_df) && "GCN:~z2" %in% input$playground){
      fig <- fig %>% add_trace(data = GCN_df, x = x, y = ~GCN_z2, name = "GCN:~z2", mode = "lines+markers")
    }
    if (exists("GCN_df") && "GCN_z3" %in% colnames(GCN_df) && "GCN:~z3" %in% input$playground){
      fig <- fig %>% add_trace(data = GCN_df, x = x, y = ~GCN_z3, name = "GCN:~z3", mode = "lines+markers")
    }
    
    if (exists("SAGE_df") && "SAGE_x" %in% colnames(SAGE_df) && "SAGE:z0" %in% input$playground){
      fig <- fig %>% add_trace(data = SAGE_df, x = x, y = ~SAGE_x, name = "SAGE:z0", mode = "lines+markers")
    }
    if (exists("SAGE_df") && "SAGE_z0" %in% colnames(SAGE_df) && "SAGE:~z0" %in% input$playground){
      fig <- fig %>% add_trace(data = SAGE_df, x = x, y = ~SAGE_z0, name = "SAGE:~z0", mode = "lines+markers")
    }
    if (exists("SAGE_df") && "SAGE_z1" %in% colnames(SAGE_df) && "SAGE:~z1" %in% input$playground){
      fig <- fig %>% add_trace(data = SAGE_df, x = x, y = ~SAGE_z1, name = "SAGE:~z1", mode = "lines+markers")
    }
    if (exists("SAGE_df") && "SAGE_z2" %in% colnames(SAGE_df) && "SAGE:~z2" %in% input$playground){
      fig <- fig %>% add_trace(data = SAGE_df, x = x, y = ~SAGE_z2, name = "SAGE:~z2", mode = "lines+markers")
    }
    if (exists("SAGE_df") && "SAGE_z3" %in% colnames(SAGE_df) && "SAGE:~z3" %in% input$playground){
      fig <- fig %>% add_trace(data = SAGE_df, x = x, y = ~SAGE_z3, name = "SAGE:~z3", mode = "lines+markers")
    }
    
    # Layout the plot
    if (input$n_A == 0){
      title <- glue("Full adjacency matrix")
    } else{
      second <- if (s_A() == "A_c") "corrupted" else "blanked out"
      title <- glue("{input$n_A}% {second} adjacency matrix")
    }
    fig <- fig %>% layout(title = title,
                          xaxis = list(title = glue("Distortion in {tolower(input$split)} node feature matrix ({s_X()})")),
                          yaxis = list (title = "Prediction accuracy"))
    fig
  })
  #############################################################################
  # MI
  #############################################################################
  #############################################################################
}