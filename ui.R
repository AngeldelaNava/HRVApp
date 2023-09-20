user_interface <- fluidPage(
  titlePanel("Heart Rate Variability Analysis"),
  sidebarLayout(
    sidebarPanel(
      fileInput("upload", "Choose a BEATS file with HRV data",
                buttonLabel = "Upload", multiple = FALSE, accept = c(".beats")),
      checkboxInput("filter_button", "Filter Data", value = FALSE),
      tabsetPanel(id = "t1", type = "hidden",
                  tabPanel("Time Analysis",
                           numericInput("size",
                                        "Choose the size of the Time Analysis",
                                        min = 0, max = 1000, value = 300),
                           numericInput("interval",
                                        "Choose the interval of the Time Analyisis",
                                        value = 7.8125, min = 0.0, max = 20.0),
                           actionButton("csv_button",
                                        "Download time_analysis.csv")
                  ),
                  tabPanel("Frequency Analysis",
                           numericInput("freqhr", "Choose the sample frequency",
                                        min = 1, max = 10, value = 4),
                           strong("Choose the ULF band values for 
                                       Fourier Analysis"),
                           splitLayout(
                             numericInput("ULF1", "", value = 0.0,
                                          min = 0.0, max = 1.0),
                             numericInput("ULF2", "", value = 0.03,
                                          min = 0.0, max = 1.0)
                           ),
                           strong("Choose the VLF band values for 
                                       Fourier Analysis"),
                           splitLayout(
                             numericInput("VLF1", "", value = 0.03,
                                          min = 0.0, max = 1.0),
                             numericInput("VLF2", "", value = 0.05,
                                          min = 0.0, max = 1.0)
                           ),
                           strong("Choose the LF band values for 
                                       Fourier Analysis"),
                           splitLayout(
                             numericInput("LF1", "", value = 0.05,
                                          min = 0.0, max = 1.0),
                             numericInput("LF2", "", value = 0.15,
                                          min = 0.0, max = 1.0)
                           ),
                           strong("Choose the HF band values for 
                                       Fourier Analysis"),
                           splitLayout(
                             numericInput("HF1", "", value = 0.15,
                                          min = 0.0, max = 1.0),
                             numericInput("HF2", "", value = 0.4,
                                          min = 0.0, max = 1.0)
                           ),
                           actionButton("csv_button_f",
                                        "Download freq_analysis.csv")
                  ),
                  tabPanel("Embedding Dimension & Timelag",
                           numericInput("lagMax", "Choose the maximum lag",
                                        value = 100, min = 0, max = 1000),
                           numericInput("numberPoints",
                                        "Choose the number of points to use",
                                        value = 10000, min = 2000, max = 50000),
                           numericInput("maxEmbeddingDim",
                                        "Choose the maximum embedding dimension",
                                        value = 15, min = 1, max = 100)
                  ),
                  tabPanel("Correlation Dimension",
                           actionButton("start_cd",
                                        "Start Correlation dimension (WARNING: Takes a long time)"),
                           actionButton("reg_correlation",
                                        "Estimate the regression line"),
                           p("\n"),
                           strong("Choose the range of distance to compute the correlation sum"),
                           splitLayout(
                             numericInput("minRadius", "", value = 1, min = 1,
                                          max = 500),
                             numericInput("maxRadius", "", value = 50, min = 1,
                                          max = 500)
                           ),
                           numericInput("pointsRadius",
                                        "Choose the number of radius to compute the correlation sum",
                                        value = 15, min = 1, max = 500),
                           numericInput("theilerWindow",
                                        "Choose the value of the Theiler window",
                                        value = 100, min = 1, max = 1000),
                           strong("Choose the Regression Range: "),
                           splitLayout(
                             numericInput("minRegC", "", value = 20, min = 1,
                                          max = 100),
                             numericInput("maxRegC", "", value = 50,
                                          min = 1, max = 100)
                           ),
                           actionButton("csv_button_c",
                                        "Download corr_analysis.csv")
                  ),
                  tabPanel("Maximum Lyapunov",
                           actionButton("start_lya",
                                        "Start Max. Lyapunov exponent (WARNING: Takes a long time)"),
                           actionButton("reg_lya",
                                        "Estimate the regression line"),
                           numericInput("radius",
                                        "Choose the radius of the analysis",
                                        value = 50, min = 1, max = 5000),
                           numericInput("theilerWindowLya",
                                        "Choose the value of the Theiler window",
                                        value = 100, min = 1, max = 1000),
                           strong("Choose the Regression Range: "),
                           splitLayout(
                             numericInput("minRegL", "", value = 10, min = 0,
                                          max = 20),
                             numericInput("maxRegL", "", value = 20,
                                          min = 0, max = 20)),
                           actionButton("csv_button_ml",
                                        "Download lya_analysis.csv")
                  )
      ),
      actionButton("downloadButton", "Download analysis.csv")
    ),
    mainPanel(
      tabsetPanel(id = "t2", type = "pills",
                  tabPanel("Time Analysis",
                           h3("Heart Rate Plot"),
                           plotOutput("graphic"),
                           tableOutput("time_analysis")
                  ),
                  tabPanel("Frequency Analysis",
                           h3("Frequency Analysis"),
                           plotOutput("freq_analysis"),
                           tableOutput("freq_table")
                  ),
                  tabPanel("Nonlinear Analysis",
                           actionButton("start_nla",
                                        "Start Nonlinear Analysis (WARNING: takes a long time)"),
                           tabsetPanel(id = "t3", type = "tabs",
                                       tabPanel(("Embedding Dimension & Timelag"),
                                                h3("Nonlinear Analysis: Embedding Dimension and Timelag estimations"),
                                                strong("Time Lag Estimation"),
                                                plotOutput("time_lag"),
                                                strong("Embedding Dimension Estimation"),
                                                plotOutput("emb_dim")
                                       ),
                                       tabPanel("Correlation Dimension",
                                                h3("Nonlinear Analysis by Correlation Dimension"),
                                                plotOutput("corr_plot"),
                                                p("Value of the statistic: ",
                                                  textOutput("corr_text"))
                                       ),
                                       tabPanel("Maximum Lyapunov",
                                                h3("Nonlinear Analysis by Maximum Lyapunov Exponent"),
                                                plotOutput("lya_plot"),
                                                p("Value of the statistic: ",
                                                  textOutput("lya_text"))
                                       )
                           )
                  )
      )
    )
  )
)