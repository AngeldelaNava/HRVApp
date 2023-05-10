library(shiny)
library(RHRV)
library(nonlinearTseries)

source("hrv.R")



ui <- fluidPage(
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
                           p("Value of SDNN (in msec.):", textOutput("text")),
                           actionButton("csv_button",
                                        "Download time_analysis.csv"),
                           p(textOutput("download_message"))),
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
                                        "Download freq_analysis.csv"),
                           p(textOutput("download_message_f"))
                           ),
                  tabPanel("Embedding Dimension & Timelag",
                           c("Embedding Dimension & Timelag"),
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
                           c("Correlation Dimension")
                           ),
                  tabPanel("Maximum Lyapunov",
                           c("Maximum Lyapunov"))
                  ),
      actionButton("downloadButton", "Download analysis.csv"),
      p(textOutput("download_general"))
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
                                        tabPanel(h3("Nonlinear Analysis: Embedding Dimension and Timelag estimations"),
                                                 strong("Time Lag Estimation"),
                                                 plotOutput("time_lag"),
                                                 strong("Embedding Dimension Estimation"),
                                                 plotOutput("emb_dim")
                                        ),
                                        tabPanel("Correlation Dimension",
                                                  h3("Nonlinear Analysis by Correlation Dimension")
                                                  ),
                                         tabPanel("Maximum Lyapunov",
                                                 h3("Nonlinear Analysis by Maximum Lyapunov Exponent")
                                        )))
      )
    )
  )
)




server <- function(input, output, session) {
  
  observe({
    tab <- input$t2 == "Frequency Analysis"
    tab2 <- input$t2 == "Time Analysis"
    if (tab) {
      updateTabsetPanel(session, inputId = "t1",
                        selected = "Frequency Analysis")
    } else if (tab2) {
      updateTabsetPanel(session, inputId = "t1", selected = "Time Analysis")
    } else {
      tab <- input$t3 == "Embedding Dimension & Timelag"
      tab2 <- input$t3 == "Correlation Dimension"
      if (tab){
        updateTabsetPanel(session, inputId = "t1",
                          selected = "Embedding Dimension & Timelag")
      } else if (tab2) {
        updateTabsetPanel(session, inputId = "t1",
                          selected = "Correlation Dimension")
      } else {
        updateTabsetPanel(session, inputId = "t1",
                          selected = "Maximum Lyapunov")
      }
    }
  })
  
  output$download_general <- renderText({
    req(input$upload)
    
    tryCatch({
      hrv.data = LoadBeatAscii(hrv.data, input$upload$datapath)
      hrv.data = BuildNIHR(hrv.data)
      if(input$filter_button){
        hrv.data = FilterNIHR(hrv.data)
      }
      
      tryCatch({
        hrv.data = CreateTimeAnalysis(hrv.data, size = input$size,
                                      interval = input$interval)
        CHARACTERISTICS = c("Size", "Interval", "SDNN (msec.)", "SDANN (msec.)",
                            "SDNNIDX (msec.)", "pNN50 (%)", "SDSD (msec.)",
                            "r-MSSD (msec.)", "IRRR (msec.)", "MADRR (msec.)",
                            "TINN (msec.)", "HRV index")
        VALUES = c(input$size, input$interval, hrv.data$TimeAnalysis[[1]]$SDNN,
                   hrv.data$TimeAnalysis[[1]]$SDANN,
                   hrv.data$TimeAnalysis[[1]]$SDNNIDX,
                   hrv.data$TimeAnalysis[[1]]$pNN50,
                   hrv.data$TimeAnalysis[[1]]$SDSD,
                   hrv.data$TimeAnalysis[[1]]$rMSSD,
                   hrv.data$TimeAnalysis[[1]]$IRRR,
                   hrv.data$TimeAnalysis[[1]]$MADRR,
                   hrv.data$TimeAnalysis[[1]]$TINN,
                   hrv.data$TimeAnalysis[[1]]$HRVi)
        hrv.data = InterpolateNIHR(hrv.data, freqhr = input$freqhr)
        hrv.data = CreateFreqAnalysis(hrv.data)
        hrv.data = CalculatePSD(hrv.data, indexFreqAnalysis = 1, doPlot = FALSE)
        BANDS = c("ULF", "VLF", "LF", "HF")
        ENERGY = CalculateEnergyInPSDBands(hrv.data, indexFreqAnalysis = 1,
                                           ULFmax = (input$ULF2),
                                           VLFmin = (input$VLF1),
                                           VLFmax = (input$VLF2),
                                           LFmin = (input$LF1),
                                           LFmax = (input$LF2),
                                           HFmin = (input$HF1),
                                           HFmax = (input$HF2))
        
        req(input$downloadButton)
        tryCatch({
          A = c(CHARACTERISTICS, BANDS)
          B = c(VALUES, ENERGY)
          write.csv(data.frame(A, B), "analysis.csv")
          c <- "analysis.csv downloaded succesfully\n"
        }, error = function(e){
          cat("Error: analysis.csv not downloaded\n")
          c <- "Error: analysis.csv not downloaded"
          stop(safeError(e))
        })
        c
      },
      error = function(e) {
        cat("Error: data filtering failed\n")
        stop(safeError(e))
      })
    },
    error = function(e) {
      cat("Error: upload failed\n")
      stop(safeError(e))
    })
  })
  
  output$graphic <- renderPlot({
    req(input$upload)
    
    tryCatch({
      hrv.data = LoadBeatAscii(hrv.data, input$upload$datapath)
      hrv.data = BuildNIHR(hrv.data)
      if(input$filter_button){
        hrv.data = FilterNIHR(hrv.data)
      }
      
      tryCatch({
        PlotNIHR(hrv.data)
      },
      error = function(e) {
        cat("Error: plot failed\n")
        stop(safeError(e))
      })
      
    },
    error = function(e) {
      cat("Error: upload failed\n")
      stop(safeError(e))
    })
  })
  
  output$time_analysis <- renderTable({
    req(input$upload)
    
    tryCatch({
      hrv.data = LoadBeatAscii(hrv.data, input$upload$datapath)
      hrv.data = BuildNIHR(hrv.data)
      
      tryCatch({
        if(input$filter_button){
          hrv.data = FilterNIHR(hrv.data)
        }
        hrv.data = CreateTimeAnalysis(hrv.data, size = input$size,
                                      interval = input$interval)
        CHARACTERISTICS = c("Size", "Interval", "SDNN (msec.)", "SDANN (msec.)",
                            "SDNNIDX (msec.)", "pNN50 (%)", "SDSD (msec.)",
                            "r-MSSD (msec.)", "IRRR (msec.)", "MADRR (msec.)",
                            "TINN (msec.)", "HRV index")
        VALUES = c(input$size, input$interval, hrv.data$TimeAnalysis[[1]]$SDNN,
                   hrv.data$TimeAnalysis[[1]]$SDANN,
                   hrv.data$TimeAnalysis[[1]]$SDNNIDX,
                   hrv.data$TimeAnalysis[[1]]$pNN50,
                   hrv.data$TimeAnalysis[[1]]$SDSD,
                   hrv.data$TimeAnalysis[[1]]$rMSSD,
                   hrv.data$TimeAnalysis[[1]]$IRRR,
                   hrv.data$TimeAnalysis[[1]]$MADRR,
                   hrv.data$TimeAnalysis[[1]]$TINN,
                   hrv.data$TimeAnalysis[[1]]$HRVi)
        df <- t(data.frame(VALUES))
        colnames(df) <- CHARACTERISTICS
        df
      },
      error = function(e) {
        cat("Error: data filtering failed\n")
        stop(safeError(e))
      })
      
    },
    error = function(e) {
      cat("Error: upload failed\n")
      stop(safeError(e))
    })
  })
  
  output$text <- renderText({
    req(input$upload)
    
    tryCatch({
      hrv.data = LoadBeatAscii(hrv.data, input$upload$datapath)
      hrv.data = BuildNIHR(hrv.data)
      
      if(input$filter_button){
        hrv.data = FilterNIHR(hrv.data)
      }
      
      tryCatch({
        hrv.data = CreateTimeAnalysis(hrv.data, size = input$size,
                                      interval = input$interval)
        hrv.data$TimeAnalysis[[1]]$SDNN
      },
      error = function(e) {
        cat("Error: data filtering failed\n")
        stop(safeError(e))
      })
      
    },
    error = function(e) {
      cat("Error: upload failed\n")
      stop(safeError(e))
    })
  })
    
  output$download_message <- renderText({
    req(input$upload)
    
    tryCatch({
      hrv.data = LoadBeatAscii(hrv.data, input$upload$datapath)
      hrv.data = BuildNIHR(hrv.data)
      
      if(input$filter_button){
        hrv.data = FilterNIHR(hrv.data)
      }
      
      tryCatch({
        hrv.data = CreateTimeAnalysis(hrv.data, size = input$size,
                                      interval = input$interval)
        CHARACTERISTICS = c("Size", "Interval", "SDNN (msec.)", "SDANN (msec.)",
                            "SDNNIDX (msec.)", "pNN50 (%)", "SDSD (msec.)",
                            "r-MSSD (msec.)", "IRRR (msec.)", "MADRR (msec.)",
                            "TINN (msec.)", "HRV index")
        VALUES = c(input$size, input$interval, hrv.data$TimeAnalysis[[1]]$SDNN,
                   hrv.data$TimeAnalysis[[1]]$SDANN,
                   hrv.data$TimeAnalysis[[1]]$SDNNIDX,
                   hrv.data$TimeAnalysis[[1]]$pNN50,
                   hrv.data$TimeAnalysis[[1]]$SDSD,
                   hrv.data$TimeAnalysis[[1]]$rMSSD,
                   hrv.data$TimeAnalysis[[1]]$IRRR,
                   hrv.data$TimeAnalysis[[1]]$MADRR,
                   hrv.data$TimeAnalysis[[1]]$TINN,
                   hrv.data$TimeAnalysis[[1]]$HRVi)
        
        req(input$csv_button)
        tryCatch({
          write.csv(data.frame(CHARACTERISTICS, VALUES), "time_analysis.csv")
          c <- "time_analysis.csv downloaded succesfully\n"
        }, error = function(e){
          cat("Error: time_analysis.csv not downloaded\n")
          c <- "Error: time_analysis.csv not downloaded"
          stop(safeError(e))
        })
        c
      },
      error = function(e) {
        cat("Error: data filtering failed\n")
        stop(safeError(e))
      })
      
    },
    error = function(e) {
      cat("Error: upload failed\n")
      stop(safeError(e))
    })
  })
  
  output$download_message_f <- renderText({
    req(input$upload)
    
    tryCatch({
      hrv.data = LoadBeatAscii(hrv.data, input$upload$datapath)
      hrv.data = BuildNIHR(hrv.data)
      
      if(input$filter_button){
        hrv.data = FilterNIHR(hrv.data)
      }
      
      tryCatch({
        hrv.data = InterpolateNIHR(hrv.data, freqhr = input$freqhr)
        hrv.data = CreateFreqAnalysis(hrv.data)
        hrv.data = CalculatePSD(hrv.data, indexFreqAnalysis = 1, doPlot = FALSE)
        BANDS = c("ULF", "VLF", "LF", "HF")
        ENERGY = CalculateEnergyInPSDBands(hrv.data, indexFreqAnalysis = 1,
                                           ULFmax = (input$ULF2),
                                           VLFmin = (input$VLF1),
                                           VLFmax = (input$VLF2),
                                           LFmin = (input$LF1),
                                           LFmax = (input$LF2),
                                           HFmin = (input$HF1),
                                           HFmax = (input$HF2))
        req(input$csv_button_f)
        
        tryCatch({
          write.csv(data.frame(BANDS, ENERGY), "freq_analysis.csv")
          c <- "freq_analysis.csv downloaded succesfully\n"
        }, error = function(e){
          cat("Error: freq_analysis.csv not downloaded\n")
          c <- "Error: _analysis.csv not downloaded"
          stop(safeError(e))
        })
      },
      error = function(e) {
        cat("Error: data filtering failed\n")
        stop(safeError(e))
      })
    },
    error = function(e) {
      cat("Error: upload failed\n")
      stop(safeError(e))
    })
  })
  
  output$freq_analysis <- renderPlot({
    req(input$upload)
    
    tryCatch({
      hrv.data = LoadBeatAscii(hrv.data, input$upload$datapath)
      hrv.data = BuildNIHR(hrv.data)
      if(input$filter_button){
        hrv.data = FilterNIHR(hrv.data)
      }
      
      tryCatch({
        hrv.data = InterpolateNIHR(hrv.data, freqhr = input$freqhr)
        hrv.data = CreateFreqAnalysis(hrv.data)
        a = c(input$ULF1, input$ULF2, input$VLF1,
              input$VLF2, input$LF1, input$LF2,
              input$HF1, input$HF2)
        
        hrv.data = CalculatePSD(hrv.data, indexFreqAnalysis = 1, doPlot = FALSE)
        PlotPSD(hrv.data, indexFreqAnalysis = 1, ULFmin = a[1], ULFmax = a[2],
                VLFmin = a[3], VLFmax = a[4], LFmin = a[5], LFmax = a[6],
                HFmin = a[7], HFmax = a[8])
      },
      error = function(e) {
        cat("Error: data filtering failed\n")
        stop(safeError(e))
      })
    },
    error = function(e) {
      cat("Error: upload failed\n")
      stop(safeError(e))
    })
  })

  output$freq_table <- renderTable({
    req(input$upload)
    
    tryCatch({
      hrv.data = LoadBeatAscii(hrv.data, input$upload$datapath)
      hrv.data = BuildNIHR(hrv.data)
      if(input$filter_button){
        hrv.data = FilterNIHR(hrv.data)
      }
      
      
      tryCatch({
        hrv.data = InterpolateNIHR(hrv.data, freqhr = input$freqhr)
        hrv.data = CreateFreqAnalysis(hrv.data)
        hrv.data = CalculatePSD(hrv.data, indexFreqAnalysis = 1, doPlot = FALSE)
        BANDS = c("ULF", "VLF", "LF", "HF")
        ENERGY = CalculateEnergyInPSDBands(hrv.data, indexFreqAnalysis = 1,
                                           ULFmax = (input$ULF2),
                                           VLFmin = (input$VLF1),
                                           VLFmax = (input$VLF2),
                                           LFmin = (input$LF1),
                                           LFmax = (input$LF2),
                                           HFmin = (input$HF1),
                                           HFmax = (input$HF2))
        
        df <- t(data.frame(ENERGY))
        colnames(df) <- BANDS
        df
      },
      error = function(e) {
        cat("Error: data filtering failed\n")
        stop(safeError(e))
      })
    },
    error = function(e) {
      cat("Error: upload failed\n")
      stop(safeError(e))
    })
  })
  
  output$time_lag <- renderPlot({
    req(input$upload)
    tryCatch({
      hrv.data = LoadBeatAscii(hrv.data, input$upload$datapath)
      hrv.data = BuildNIHR(hrv.data)
      if(input$filter_button){
        hrv.data = FilterNIHR(hrv.data)
      }
      req(input$start_nla)
      tryCatch({
        hrv.data = CreateNonLinearAnalysis(hrv.data)
        hrv.data = SurrogateTest(hrv.data, significance = 0.05,
                                 useFunction = timeAsymmetry2, tau = 4,
                                 doPlot = FALSE)
        CalculateTimeLag(hrv.data, method = "first.minimum", lagMax = input$lagMax)
      },
      error = function(e) {
        cat("Error: non-linear analysis failed\n")
        stop(safeError(e))
      })
    },
    error = function(e) {
      cat("Error: upload failed\n")
      stop(safeError(e))
    })
  })
  
  output$emb_dim <- renderPlot({
    req(input$upload)
    tryCatch({
      hrv.data = LoadBeatAscii(hrv.data, input$upload$datapath)
      hrv.data = BuildNIHR(hrv.data)
      if(input$filter_button){
        hrv.data = FilterNIHR(hrv.data)
      }
      req(input$start_nla)
      tryCatch({
        hrv.data = CreateNonLinearAnalysis(hrv.data)
        hrv.data = SurrogateTest(hrv.data, significance = 0.05,
                                 useFunction = timeAsymmetry2, tau = 4,
                                 doPlot = FALSE)
        kTimeLag = CalculateTimeLag(hrv.data, method = "first.minimum", lagMax = input$lagMax,
                                    doPlot = FALSE)
        CalculateEmbeddingDim(hrv.data, numberPoints = input$numberPoints,
                              timeLag = kTimeLag,
                              maxEmbeddingDim = input$maxEmbeddingDim)
      },
      error = function(e) {
        cat("Error: non-linear analysis failed\n")
        stop(safeError(e))
      })
    },
    error = function(e) {
      cat("Error: upload failed\n")
      stop(safeError(e))
    })
  })
}


shinyApp(ui = ui, server = server)