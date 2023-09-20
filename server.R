source("hrv.R")
source("tabsControl.R")

server <- function(input, output, session) {
  
  observe({
    tabsControl(input, output, session)
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
  
  observe({
    
    req(input$upload)
    tryCatch({
      hrv.data = LoadBeatAscii(hrv.data, input$upload$datapath)
      hrv.data = BuildNIHR(hrv.data)
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
      if(input$csv_button){
        write.csv(data.frame(CHARACTERISTICS, VALUES), "time_analysis.csv")
      }
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
      if(input$csv_button_f){
        write.csv(data.frame(BANDS, ENERGY), "freq_analysis.csv")
      }
      
      A = c(CHARACTERISTICS, BANDS)
      B = c(VALUES, ENERGY)
      
      
      
      if(input$start_nla){
        tryCatch({
          hrv.data = CreateNonLinearAnalysis(hrv.data)
          kTimeLag = CalculateTimeLag(
            hrv.data, method = "first.minimum", lagMax = input$lagMax,
            doPlot = FALSE)
          output$time_lag <- renderPlot({
            CalculateTimeLag(
              hrv.data, method = "first.minimum", lagMax = input$lagMax,
              doPlot = TRUE)
          })
          kEmbeddingDim = CalculateEmbeddingDim(
            hrv.data, numberPoints = input$numberPoints, timeLag = kTimeLag,
            maxEmbeddingDim = input$maxEmbeddingDim, doPlot = FALSE)
          output$emb_dim <- renderPlot({
            CalculateEmbeddingDim(
              hrv.data, numberPoints = input$numberPoints, timeLag = kTimeLag,
              maxEmbeddingDim = input$maxEmbeddingDim, doPlot = TRUE)
          })
          if(input$start_cd){
            tryCatch({
              hrv.data = CalculateCorrDim(
                hrv.data, indexNonLinearAnalysis = 1,
                minEmbeddingDim = kEmbeddingDim - 1,
                maxEmbeddingDim = kEmbeddingDim + 2, timeLag = kTimeLag,
                minRadius = input$minRadius, maxRadius = input$maxRadius,
                pointsRadius = input$pointsRadius,
                theilerWindow = input$theilerWindow, doPlot = FALSE)
              output$corr_plot <- renderPlot({
                PlotCorrDim(hrv.data, indexNonLinearAnalysis = 1)
              })
              if(input$reg_correlation){
                hrv.data = EstimateCorrDim(
                  hrv.data, indexNonLinearAnalysis = 1,
                  regressionRange = c(input$minRegC, input$maxRegC),
                  useEmbeddings = (kEmbeddingDim - 1):(kEmbeddingDim + 2),
                  doPlot = FALSE)
                A = c(A, "Correlation Statistic")
                B = c(B, hrv.data$NonLinearAnalysis[[1]]$correlation$statistic)
                output$corr_plot <- renderPlot({
                  EstimateCorrDim(
                    hrv.data, indexNonLinearAnalysis = 1,
                    regressionRange = c(input$minRegC, input$maxRegC),
                    useEmbeddings = (kEmbeddingDim - 1):(kEmbeddingDim + 2),
                    doPlot = TRUE)
                })
                output$corr_text <- renderText({
                  hrv.data$NonLinearAnalysis[[1]]$correlation$statistic
                })
                if(input$csv_button_c){
                  C = hrv.data$NonLinearAnalysis[[1]]$correlation$statistic
                  write.csv(data.frame(c("Correlation Statistic"), c(C)),
                            "corr_analysis.csv")
                }
              }
            },
            error = function(e) {
              cat("Error: Correlation dimension analysis failed")
            })
          }
          if(input$start_lya){
            tryCatch({
              hrv.data = CalculateMaxLyapunov(
                hrv.data, indexNonLinearAnalysis = 1,
                minEmbeddingDim = kEmbeddingDim,
                maxEmbeddingDim = kEmbeddingDim + 2, timeLag = kTimeLag,
                radius = input$radius, theilerWindow = input$theilerWindowLya,
                doPlot = FALSE)
              output$lya_plot <- renderPlot({
                PlotMaxLyapunov(hrv.data, indexNonLinearAnalysis = 1)
              })
              if(input$reg_lya){
                hrv.data = EstimateMaxLyapunov(
                  hrv.data, indexNonLinearAnalysis = 1,
                  regressionRange = c(input$minRegL, input$maxRegL),
                  useEmbeddings = (kEmbeddingDim):(kEmbeddingDim + 2),
                  doPlot = FALSE)
                A = c(A, "Max. Lyapunov Statistic")
                B = c(B, hrv.data$NonLinearAnalysis[[1]]$lyapunov$statistic)
                output$lya_plot <- renderPlot({
                  EstimateMaxLyapunov(
                    hrv.data, indexNonLinearAnalysis = 1,
                    regressionRange = c(input$minRegL, input$maxRegL),
                    useEmbeddings = (kEmbeddingDim):(kEmbeddingDim + 2),
                    doPlot = TRUE)
                })
                output$lya_text <- renderText({
                  hrv.data$NonLinearAnalysis[[1]]$lyapunov$statistic
                })
                if(input$csv_button_ml){
                  C = hrv.data$NonLinearAnalysis[[1]]$lyapunov$statistic
                  write.csv(data.frame(c("Max. Lyapunov Statistic"), c(C)),
                            "lya_analysis.csv")
                }
              }
            }, error = function(e) {
              cat("Error: Maximum Lyapunov exponent calculation failed")
            })
          }
        },
        error = function(e) {
          cat("Error: non-linear analysis failed\n")
          stop(safeError(e))
        })
      }
      if(input$downloadButton){
        write.csv(data.frame(A, B), "analysis.csv")
      }
    },
    error = function(e) {
      cat("Error: upload failed\n")
      stop(safeError(e))
    })
  })
}