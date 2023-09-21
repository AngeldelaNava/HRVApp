source("timeAnalysis.R")
source("frequencyAnalysis.R")
source("nonlinearAnalysis.R")
source("csv.R")

inputOutputControl <- function(input, output, session, hrv.data){
    
    req(input$upload)
    
    tryCatch({
      hrv.data = LoadBeatAscii(hrv.data, input$upload$datapath)
      hrv.data = BuildNIHR(hrv.data)
      if(input$filter_button){
        hrv.data = FilterNIHR(hrv.data)
      }
      hrv.data = CreateTimeAnalysis(hrv.data, size = input$size,
                                    interval = input$interval)
      timeAnalysis(input, output, session, hrv.data)
      
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
        csvDownload(data.frame(CHARACTERISTICS, VALUES), "time_analysis.csv")
      }
      A <- csvSum(CHARACTERISTICS, c())
      B <- csvSum(VALUES, c())
      tryCatch({
        hrv.data = InterpolateNIHR(hrv.data, freqhr = input$freqhr)
        hrv.data = CreateFreqAnalysis(hrv.data)
        
        hrv.data <- frequencyAnalysis(input, output, session, hrv.data)
        
        BANDS = c("ULF", "VLF", "LF", "HF")
        ENERGY = CalculateEnergyInPSDBands(hrv.data, indexFreqAnalysis = 1,
                                           ULFmax = (input$ULF2),
                                           VLFmin = (input$VLF1),
                                           VLFmax = (input$VLF2),
                                           LFmin = (input$LF1),
                                           LFmax = (input$LF2),
                                           HFmin = (input$HF1),
                                           HFmax = (input$HF2))
        if(input$csv_button){
          csvDownload(data.frame(CHARACTERISTICS, VALUES), "time_analysis.csv")
        }
        A <- csvSum(A, BANDS)
        B <- csvSum(B, ENERGY)
        
        if(input$start_nla){
          tryCatch({
            hrv.data = CreateNonLinearAnalysis(hrv.data)
            kTimeLag = CalculateTimeLag(
              hrv.data, method = "first.minimum", lagMax = input$lagMax,
              doPlot = FALSE)
            timeLag(input, output, session, hrv.data, kTimeLag)
            
            kEmbeddingDim = CalculateEmbeddingDim(
              hrv.data, numberPoints = input$numberPoints, timeLag = kTimeLag,
              maxEmbeddingDim = input$maxEmbeddingDim, doPlot = FALSE)
            embeddingDim(input, output, session, hrv.data, kTimeLag,
                         kEmbeddingDim)
            
            if(input$start_cd){
              tryCatch({
                hrv.data = CalculateCorrDim(
                  hrv.data, indexNonLinearAnalysis = 1,
                  minEmbeddingDim = kEmbeddingDim - 1,
                  maxEmbeddingDim = kEmbeddingDim + 2, timeLag = kTimeLag,
                  minRadius = input$minRadius, maxRadius = input$maxRadius,
                  pointsRadius = input$pointsRadius,
                  theilerWindow = input$theilerWindow, doPlot = FALSE)
                correlationDimensionCalculation(output, hrv.data)
                
                if(input$reg_correlation){
                  hrv.data = EstimateCorrDim(
                    hrv.data, indexNonLinearAnalysis = 1,
                    regressionRange = c(input$minRegC, input$maxRegC),
                    useEmbeddings = (kEmbeddingDim - 1):(kEmbeddingDim + 2),
                    doPlot = FALSE)
                  A <- csvSum(A, "Correlation Statistic")
                  C <- hrv.data$NonLinearAnalysis[[1]]$correlation$statistic
                  B <- csvSum(B, C)
                  correlationDimensionEstimation(input, output, kEmbeddingDim,
                                                 hrv.data)
                  correlatonStatistic(output, hrv.data)
                  if(input$csv_button_c){
                    C = hrv.data$NonLinearAnalysis[[1]]$correlation$statistic
                    csvDownload(data.frame(c("Correlation Statistic"), c(C)),
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
                lyapunovCalculation(output, hrv.data)
                
                if(input$reg_lya){
                  hrv.data = EstimateMaxLyapunov(
                    hrv.data, indexNonLinearAnalysis = 1,
                    regressionRange = c(input$minRegL, input$maxRegL),
                    useEmbeddings = (kEmbeddingDim):(kEmbeddingDim + 2),
                    doPlot = FALSE)
                  A <- csvSum(A, "Max. Lyapunov Statistic")
                  C = hrv.data$NonLinearAnalysis[[1]]$lyapunov$statistic
                  B <- csvSum(B, C)
                  lyapunovEstimation(input, output, kEmbeddingDim, hrv.data)
                  lyapunovStatistic(output, hrv.data)
                  if(input$csv_button_ml){
                    csvDownload(data.frame(c("Max. Lyapunov Statistic"), c(C)),
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
      },
      error = function(e) {
        cat("Error: data filtering failed\n")
        stop(safeError(e))
      })
      
      if(input$downloadButton){
        csvDownload(data.frame(A,B), "analysis.csv")
      }
    },
    error = function(e) {
      cat("Error: upload failed\n")
      stop(safeError(e))
    })
}