
server <- function(input, output, session){
  #############################################################################
  # data frame
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
    df <- if (input$dataset == "ogbn-arxiv") o_MLP_files else w_MLP_files
    suffix <- glue("{s_X()}_A_c_{s_split()}")
    x <- if (s_X() == "X_c") ~X_c else ~X_z
      
    fig <- plot_ly(df[[suffix]], x = x)
    fig <- fig %>% add_trace(y = ~MLP_x, name = "MLP:z0", mode = "lines+markers")
    fig <- fig %>% add_trace(y = ~MLP_z0, name = "MLP:~z0", mode = "lines+markers")
    fig <- fig %>% add_trace(y = ~MLP_z1, name = "MLP:~z1", mode = "lines+markers")
    fig <- fig %>% add_trace(y = ~MLP_z2, name = "MLP:~z2", mode = "lines+markers")
    fig <- fig %>% add_trace(y = ~MLP_z3, name = "MLP:~z3", mode = "lines+markers")
    fig <- fig %>% layout(title = glue("Full adjacency matrix"),
                          xaxis = list(title = glue("Distortion in {tolower(input$split)} node feature matrix ({s_X()})")),
                          yaxis = list (title = "Prediction accuracy"))
    fig
  })
  #############################################################################
  # n2v
  #############################################################################
  output$n2v_plot <- renderPlotly({
    df <- if (input$dataset == "ogbn-arxiv") o_n2v_files else w_n2v_files
    suffix <- glue("{s_X()}_{s_A()}_{s_split()}")
    x <- if (s_X() == "X_c") ~X_c else ~X_z
    
    reqd_df <- df[[suffix]]
    final_df <- if (s_A() == "A_c") subset(reqd_df, A_c == input$n_A) else subset(reqd_df, A_z == input$n_A)
    
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
    
    fig <- plot_ly(final_df, x = x)
    fig <- fig %>% add_trace(y = ~n2v_x, name = "n2v:z0", mode = "lines+markers")
    fig <- fig %>% add_trace(y = ~n2v_z0, name = "n2v:~z0", mode = "lines+markers")
    fig <- fig %>% add_trace(y = ~n2v_z1, name = "n2v:~z1", mode = "lines+markers")
    fig <- fig %>% add_trace(y = ~n2v_z2, name = "n2v:~z2", mode = "lines+markers")
    fig <- fig %>% add_trace(y = ~n2v_z3, name = "n2v:~z3", mode = "lines+markers")
    fig <- fig %>% layout(title = title,
                          xaxis = list(title = glue("Distortion in {tolower(input$split)} node feature matrix ({s_X()})")),
                          yaxis = list (title = "Prediction accuracy"))
    fig
  })
  #############################################################################
  # GCN
  #############################################################################
  output$GCN_plot <- renderPlotly({
    df <- if (input$dataset == "ogbn-arxiv") o_GCN_files else w_GCN_files
    suffix <- glue("{s_X()}_{s_A()}_{s_split()}")
    x <- if (s_X() == "X_c") ~X_c else ~X_z
    
    reqd_df <- df[[suffix]]
    final_df <- if (s_A() == "A_c") subset(reqd_df, A_c == input$n_A) else subset(reqd_df, A_z == input$n_A)
    
    if (input$n_A == 0){
      title <- glue("Full adjacency matrix")
    } else{
      second <- if (s_A() == "A_c") "corrupted" else "blanked out"
      title <- glue("{input$n_A}% {second} adjacency matrix")
    }
    
    fig <- plot_ly(final_df, x = x)
    fig <- fig %>% add_trace(y = ~GCN_x, name = "GCN:z0", mode = "lines+markers")
    fig <- fig %>% add_trace(y = ~GCN_z0, name = "GCN:~z0", mode = "lines+markers")
    fig <- fig %>% add_trace(y = ~GCN_z1, name = "GCN:~z1", mode = "lines+markers")
    fig <- fig %>% add_trace(y = ~GCN_z2, name = "GCN:~z2", mode = "lines+markers")
    fig <- fig %>% add_trace(y = ~GCN_z3, name = "GCN:~z3", mode = "lines+markers")
    fig <- fig %>% layout(title = title,
                          xaxis = list(title = glue("Distortion in {tolower(input$split)} node feature matrix ({s_X()})")),
                          yaxis = list (title = "Prediction accuracy"))
    fig
  })
  #############################################################################
  # SAGE
  #############################################################################
  output$SAGE_plot <- renderPlotly({
    df <- if (input$dataset == "ogbn-arxiv") o_SAGE_files else w_SAGE_files
    suffix <- glue("{s_X()}_{s_A()}_{s_split()}")
    x <- if (s_X() == "X_c") ~X_c else ~X_z
    
    reqd_df <- df[[suffix]]
    final_df <- if (s_A() == "A_c") subset(reqd_df, A_c == input$n_A) else subset(reqd_df, A_z == input$n_A)
    
    if (input$n_A == 0){
      title <- glue("Full adjacency matrix")
    } else{
      second <- if (s_A() == "A_c") "corrupted" else "blanked out"
      title <- glue("{input$n_A}% {second} adjacency matrix")
    }
    
    fig <- plot_ly(final_df, x = x)
    fig <- fig %>% add_trace(y = ~SAGE_x, name = "SAGE:z0", mode = "lines+markers")
    fig <- fig %>% add_trace(y = ~SAGE_z0, name = "SAGE:~z0", mode = "lines+markers")
    fig <- fig %>% add_trace(y = ~SAGE_z1, name = "SAGE:~z1", mode = "lines+markers")
    fig <- fig %>% add_trace(y = ~SAGE_z2, name = "SAGE:~z2", mode = "lines+markers")
    fig <- fig %>% add_trace(y = ~SAGE_z3, name = "SAGE:~z3", mode = "lines+markers")
    fig <- fig %>% layout(title = title,
                          xaxis = list(title = glue("Distortion in {tolower(input$split)} node feature matrix ({s_X()})")),
                          yaxis = list (title = "Prediction accuracy"))
    fig
  })
  #############################################################################
  # Playground
  #############################################################################
  output$compare_plot <- renderPlotly({
    if (input$dataset == "ogbn-arxiv") {
      MLP_df <- o_MLP_files
      n2v_df <- o_n2v_files
      GCN_df <- o_GCN_files
      SAGE_df <- o_SAGE_files
    } else {
      MLP_df <- w_MLP_files
      n2v_df <- w_n2v_files
      GCN_df <- w_GCN_files
      SAGE_df <- w_SAGE_files
    }
    
    MLP_suffix <- glue("{s_X()}_A_c_{s_split()}")
    rest_suffix <- glue("{s_X()}_{s_A()}_{s_split()}")
    x <- if (s_X() == "X_c") ~X_c else ~X_z
    
    MLP_df <- MLP_df[[MLP_suffix]]
    
    n2v_reqd_df <- n2v_df[[rest_suffix]]
    n2v_df <- if (s_A() == "A_c") subset(n2v_reqd_df, A_c == input$n_A) else subset(n2v_reqd_df, A_z == input$n_A)
    
    GCN_reqd_df <- GCN_df[[rest_suffix]]
    GCN_df <- if (s_A() == "A_c") subset(GCN_reqd_df, A_c == input$n_A) else subset(GCN_reqd_df, A_z == input$n_A)
    
    SAGE_reqd_df <- SAGE_df[[rest_suffix]]
    SAGE_df <- if (s_A() == "A_c") subset(SAGE_reqd_df, A_c == input$n_A) else subset(SAGE_reqd_df, A_z == input$n_A)
    
    if (input$n_A == 0){
      title <- glue("Full adjacency matrix")
    } else{
      second <- if (s_A() == "A_c") "corrupted" else "blanked out"
      title <- glue("{input$n_A}% {second} adjacency matrix")
    }
    
    fig <- plot_ly()
    
    fig <- fig %>% add_trace(data = MLP_df, x = x, y = ~MLP_x, name = "MLP:z0", mode = "lines+markers")
    if ("MLP:~z0" %in% input$playground){
      fig <- fig %>% add_trace(data = MLP_df, x = x, y = ~MLP_z0, name = "MLP:~z0", mode = "lines+markers")
    } else if ("MLP:~z1" %in% input$playground){
      fig <- fig %>% add_trace(data = MLP_df, x = x, y = ~MLP_z1, name = "MLP:~z1", mode = "lines+markers")
    } else if ("MLP:~z2" %in% input$playground){
      fig <- fig %>% add_trace(data = MLP_df, x = x, y = ~MLP_z2, name = "MLP:~z2", mode = "lines+markers")
    } else if ("MLP:~z3" %in% input$playground){
      fig <- fig %>% add_trace(data = MLP_df, x = x, y = ~MLP_z3, name = "MLP:~z3", mode = "lines+markers")
    }
    
    fig <- fig %>% add_trace(data = n2v_df, x = x, y = ~n2v_x, name = "n2v:z0", mode = "lines+markers")
    if ("n2v:~z0" %in% input$playground){
      fig <- fig %>% add_trace(data = n2v_df, x = x, y = ~n2v_z0, name = "n2v:~z0", mode = "lines+markers")
    } else if ("n2v:~z1" %in% input$playground){
      fig <- fig %>% add_trace(data = n2v_df, x = x, y = ~n2v_z1, name = "n2v:~z1", mode = "lines+markers")
    } else if ("n2v:~z2" %in% input$playground){
      fig <- fig %>% add_trace(data = n2v_df, x = x, y = ~n2v_z2, name = "n2v:~z2", mode = "lines+markers")
    } else if ("n2v:~z3" %in% input$playground){
      fig <- fig %>% add_trace(data = n2v_df, x = x, y = ~n2v_z3, name = "n2v:~z3", mode = "lines+markers")
    }
    
    fig <- fig %>% add_trace(data = GCN_df, x = x, y = ~GCN_x, name = "GCN:z0", mode = "lines+markers")
    if ("GCN:~z0" %in% input$playground){
      fig <- fig %>% add_trace(data = GCN_df, x = x, y = ~GCN_z0, name = "GCN:~z0", mode = "lines+markers")
    } else if ("GCN:~z1" %in% input$playground){
      fig <- fig %>% add_trace(data = GCN_df, x = x, y = ~GCN_z1, name = "GCN:~z1", mode = "lines+markers")
    } else if ("GCN:~z2" %in% input$playground){
      fig <- fig %>% add_trace(data = GCN_df, x = x, y = ~GCN_z2, name = "GCN:~z2", mode = "lines+markers")
    } else if ("GCN:~z3" %in% input$playground){
      fig <- fig %>% add_trace(data = GCN_df, x = x, y = ~GCN_z3, name = "GCN:~z3", mode = "lines+markers")
    }
    
    fig <- fig %>% add_trace(data = SAGE_df, x = x, y = ~SAGE_x, name = "SAGE:z0", mode = "lines+markers")
    if ("SAGE:~z0" %in% input$playground){
      fig <- fig %>% add_trace(data = SAGE_df, x = x, y = ~SAGE_z0, name = "SAGE:~z0", mode = "lines+markers")
    } else if ("SAGE:~z1" %in% input$playground){
      fig <- fig %>% add_trace(data = SAGE_df, x = x, y = ~SAGE_z1, name = "SAGE:~z1", mode = "lines+markers")
    } else if ("SAGE:~z2" %in% input$playground){
      fig <- fig %>% add_trace(data = SAGE_df, x = x, y = ~SAGE_z2, name = "SAGE:~z3", mode = "lines+markers")
    } else if ("SAGE:~z3" %in% input$playground){
      fig <- fig %>% add_trace(data = SAGE_df, x = x, y = ~SAGE_z3, name = "SAGE:~z3", mode = "lines+markers")
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