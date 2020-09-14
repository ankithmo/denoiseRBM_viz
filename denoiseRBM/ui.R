
header <- dashboardHeader(
  title = "denoiseRBM",
  titleWidth = 300,
  
  dropdownMenu(
    type = "notifications",
    headerText = strong("HELP"),
    icon = icon("question"),
    badgeStatus = NULL,
    notificationItem(
      text = steps$text[1],
      icon = icon("calendar")
    ),
    notificationItem(
      text = steps$text[2],
      icon = icon("columns")
    ),
    notificationItem(
      text = steps$text[3],
      icon = icon("xing")
    ),
    notificationItem(
      text = steps$text[4],
      icon = icon("adn")
    ),
    notificationItem(
      text = steps$text[5],
      icon = icon("sliders-h")
    ),
    notificationItem(
      text = steps$text[6],
      icon = icon("chart-line")
    )
  ),
  
  tags$li(
    a(
      strong("About"),
      height = 40,
      href = "https://github.com/ankithmo/denoiseRBM_viz/blob/master/README.md",
      title = "",
      target = "_blank"
    ),
    class = "dropdown"
  )
)

sidebar <- dashboardSidebar(
  width = 300,
  sidebarMenu(
    menuItem(
      "Dashboard",
      tabName = "dashboard",
      icon = icon("dashboard"),
      startExpanded = TRUE,
      selectInput("dataset",
                  "Dataset",
                  choices = c("ogbn-arxiv", "WikiCS"),#, "WikiCS-AWGN"
                  selected = "ogbn-arxiv"),
      selectInput("split",
                  "Split",
                  choices = c("Validation", "Test"),
                  selected = "Validation"),
      selectInput("X",
                  "Node feature matrix (X)",
                  choices = c("Corrupted (X_c)", "Blanked out (X_z)"),
                  selected = "Corrupted (X_c)"),
      selectInput("A",
                  "Adjacency matrix (A)",
                  choices = list("Corrupted (A_c)", "Blanked out (A_z)"),
                  selected = "Corrupted (A_c)"),
      sliderInput("n_A",
                  "Distortion in adjacency matrix (n_A)",
                  min = 0,
                  max = 100,
                  value = 0,
                  step = 10),
      selectInput("playground",
                  "Choose reconstructions",
                  list("MLP" = list("MLP:z0", "MLP:~z0", "MLP:~z1", "MLP:~z2", "MLP:~z3"),
                       "n2v" = list("n2v:z0", "n2v:~z0", "n2v:~z1", "n2v:~z2", "n2v:~z3"),
                       "GCN" = list("GCN:z0", "GCN:~z0", "GCN:~z1", "GCN:~z2", "GCN:~z3"),
                       "SAGE" = list("SAGE:z0", "SAGE:~z0", "SAGE:~z1", "SAGE:~z2", "SAGE:~z3")
                       ),
                  selected = c("MLP:z0", "n2v:z0", "GCN:z0", "SAGE:z0"),
                  multiple = TRUE)
    ),
    menuItem(
      "Info",
      icon = icon("info"),
      menuItem(
        "Neural networks",
        icon = icon("project-diagram"),
        helpText("1. MLP: Multi-layer perceptron"),
        helpText("2. n2v: MLP with node feature matrix"),
        helpText("        concatenated with node2vec embeddings"),
        helpText("3. GCN: Graph convolution networks"),
        helpText("4. SAGE: GraphSAGE")
      ),
      menuItem(
        "Reconstructions",
        icon=icon("wrench"),
        helpText("z0: Noisy data"),
        helpText("~z0: Reconstructions of noisy data"),
        helpText("~z1: Reconstructions of first hidden"),
        helpText("     layer representations"),
        helpText("~z2: Reconstructions of second hidden"),
        helpText("     layer representations"),
        helpText("~z3: Reconstructions of third hidden"),
        helpText("     layer representations")
      ),
      menuItem(
        "Notes",
        icon=icon("comment-dots"),
        helpText("1. Performance of MLP is independent of"),
        helpText("distortions in the adjacency matrix."),
        helpText("2. node2vec embeddings are not generated"),
        helpText("when the adjacency matrix is more than 40%"),
        helpText("blanked out because nodes start becoming"),
        helpText("increasingly isolated.")
      )
    )
  )
)

body <- dashboardBody(
  fluidRow(
    tabsetPanel(
      tabPanel(
        "MLP",
        value="MLP",
        plotlyOutput("MLP_plot")
      ),
      tabPanel(
        "n2v",
        value="n2v",
        plotlyOutput("n2v_plot")
      ),
      tabPanel(
        "GCN",
        value="GCN",
        plotlyOutput("GCN_plot")
      ),
      tabPanel(
        "SAGE",
        value="SAGE",
        plotlyOutput("SAGE_plot")
      )
    )
  ),
  fluidRow(
    tabsetPanel(
      tabPanel(
        "Playground",
        value="Playground",
        plotlyOutput("compare_plot")   
      )
    )
  )
)

ui <- dashboardPage(
  skin = "purple",
  title = "denoiseRBM",
  header,
  sidebar,
  body
)