# Copyright 2015 Nicola Romano (romano.nicola@gmail.com)
# 
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License, version 2, as 
# published by the Free Software Foundation.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
  
library(shiny)

# NOTE: Click handling as in http://stackoverflow.com/a/28035861/176923

# Little helper function for outputting stats
fmt <- function(num, dig = 2)
  {
  format(num, nsmall = dig, digits = dig, scientific = FALSE)
  }

shinyServer(function(input, output, clientData, session)
  {
  # Read experiment file 
  read.data <- reactive(
    {
    if (input$experiment != "Select an experiment")
      {
      tmp <- read.csv(input$experiment)
      calcium$traces <- as.matrix(tmp[, !(colnames(tmp) %in% c("Background", "Time"))])

      if (is.null(tmp$Background))
        calcium$bg <- 1:nrow(calcium$traces)
      else
        calcium$bg <- tmp$Background
      
      if (is.null(tmp$Time))
        calcium$time <- rep(0, nrow(calcium$traces))
      else
        calcium$time <- tmp$Time

      treatments.file <- paste(substr(input$experiment, 1, nchar(input$experiment) - 4), "- treatments.csv")
      
      if (file.exists(treatments.file))
        calcium$treatments <- read.csv(treatments.file)
      else
        calcium$treatments <- NULL
      
      calcium$baseline <- NULL
      calcium$baselinesd <- NULL
      
      updateSliderInput(session, "traceNum", max = ncol(calcium$traces))
      updateNumericInput(session, "timeTo", value = max(calcium$time)/60)
      
      calcium$peaks.start <- NULL
      calcium$peaks.end <- NULL
      calcium$peaks <- NULL
      }
    }) # end read.data

  calcium <- reactiveValues()
  clicksValues <- reactiveValues(
    click1 = NULL,  # Represents the first mouse click, if any
    range = NULL    # After two clicks, this stores the range of x
    )
  
  # Handle clicks on the plot
  observeEvent(input$plotClick, 
              {
              if (is.null(clicksValues$click1))
                  {
                  # We don't have a first click, so this is the first click
                  clicksValues$click1 <- max(1, input$plotClick$x) * 60
                  clicksValues$range <- NULL
                  }
              else 
                  {
                  # We already had a first click, so this is the second click.
                  # Make a range from the previous click and this one.
                  clicksValues$range <- range(clicksValues$click1, max(1, input$plotClick$x) * 60)
                  # And clear the first click so the next click starts a new
                  # range.
                  clicksValues$click1 <- NULL
                  }
              })
  
  output$plotStats <- renderPrint(
    {
    trace <- calcium$traces[,input$traceNum]

    basecv <- trace[1:calcium$treatments$From[1]] - 
              calcium$bg[1:calcium$treatments$From[1]]
    
    basecv <- (basecv - min(basecv)) / diff(range(basecv))
    basecv <- sd(basecv) / mean(basecv)
    
#    cat(paste0("Baseline CV [0;", calcium$treatments$From[1]/60, "] : ", 
#               fmt(basecv), "\n"))

    if (is.null(clicksValues$range)) 
      {
      cat("Select a region of the plot (two clicks) to get stats.")
      }
    else
      {
      rg <- clicksValues$range
      
      region <- trace[rg[1]:rg[2]] - calcium$bg[rg[1]:rg[2]]
      # Scale to full trace 
      region.scaled <- (region - min(trace-calcium$bg)) / diff(range(trace-calcium$bg))
      
      output <- paste0("Length: ", fmt(diff(rg) / 60), " (", fmt(rg[1] / 60), 
                          " to ", fmt(rg[2] / 60) ,")",
                       "\nMean value: ", fmt(mean(region)),
                       "\nMax value: ", fmt(max(region)), 
                          " at ", fmt((which.max(region) + rg[1]) / 60),
                       "\nCV: ", fmt(sd(region)/mean(region)),
                       "\nAUC: ", fmt(sum(region) - sum(calcium$baseline[rg[1]:rg[2]])),
                       "\n%Time active: ", fmt(sum(region.scaled > input$activityThresh)/
                                                 length(region.scaled) * 100)
                       )

      cat(output)
      }
    })
  
  output$treatmentTable <- renderTable(
    {
    if (input$experiment == "Select an experiment")
      {
      NULL
      }
    else if (!is.null(calcium$treatments))
      {
      data.frame("Treatment" = calcium$treatments$Treatment,
                 "From" = calcium$treatments$From / 60,
                 "Duration" = calcium$treatments$Duration / 60)
      }
    else
      {
      NULL
      } 
    })
  
  output$plotCoords <- renderPrint(
    if (!is.null(input$plot_hover) & input$visualization == "single")
      paste("X:", format(input$plot_hover$x, digits = 2, nsmall = 1), 
            "- Y:", format(input$plot_hover$y, digits = 2, nsmall = 1))
    else
      cat(" ")
    )
  
  output$peakStats <- renderPrint(
    cat(paste("Number of peaks: ", length(calcium$peaks)))
    )
  
  plotTraceFn <- function()
      {
      read.data()
      
      if (input$experiment == "Select an experiment")
      {
        plot(0, 0, "n", axes = F, xlab = "", ylab = "")
      }
      else
      {
        if (!is.null(calcium$traces))
        {
          ############# MULTIPLE TRACES ###############
          if (input$visualization == "traces")
          {
            plot(0, 0, "n", ylim = c(1, ncol(calcium$traces) + 1), bty = "n", 
                 xlab = "Time (min)", ylab = "", yaxt = "n", 
                 xlim = c(input$timeFrom, input$timeTo))
            
            i <- 1
            apply(calcium$traces, 2, function(t)
            {
              t <- t - calcium$bg
              t <- (t - min(t)) / diff(range(t))
              lines(calcium$time / 60, t + i)
              i <<- i + 1
            })
            
            if (!is.null(calcium$treatments))
            {
              tr <- calcium$treatments
              rect(tr$From/60, 0, tr$From/60 + tr$Duration/60, ncol(calcium$traces) * 2, 
                   border = 0, col = rgb(0, 0, 0, 0.2))
            }
          }
          ############# SINGLE TRACE ###############
          else if (input$visualization == "single")
          {
            if (input$bgSub)
            {
              trace <- calcium$traces[,input$traceNum] - calcium$bg
            }
            else
            {
              trace <- calcium$traces[,input$traceNum] 
            }
            
            times <- calcium$time / 60
            
            if (input$normalise)
            {
              plot(times, (trace-min(trace)) / max(trace), t = "l", las = 1, 
                   xlim = c(input$timeFrom, input$timeTo),
                   xlab = "Time (min)", ylab = "", bty = "n")
            }
            else if (input$autoscaleY)
              plot(times, trace, t = "l", las = 1, xlim = c(input$timeFrom, input$timeTo),
                   xlab = "Time (min)", ylab = "", bty = "n")
            else
              plot(times, trace, t = "l", las = 1, xlim = c(input$timeFrom, input$timeTo),
                   xlab = "Time (min)", ylab = "", bty = "n", ylim = c(input$yFrom, input$yTo))
            
            if (!is.null(calcium$treatments) & input$showTreatments)
            {
              tr <- calcium$treatments
              rect(tr$From/60, -100, tr$From/60 + tr$Duration/60, max(trace) * 2, 
                   border = 0, col = rgb(0, 0, 0, 0.2))
            }
            
            if (input$showBase)
            {
              q <- quantile(trace, input$centile/100)
              baseline <- trace
              baseline[baseline >= q] <- q
              ls <- loess(baseline ~ times, span = input$span)
              calcium$baseline <- predict(ls)
              lines(times, calcium$baseline, col = "darkgreen", lwd = 2)
            }
            
            if (input$showActThr)
            {
              if (input$normalise)
                abline(h = input$activityThresh, lty = "dashed", col = "orange")
              else
                abline(h = min(trace) + input$activityThresh * diff(range(trace)),
                       lty = "dashed", col = "orange")
            }
            
            if (input$showStats)
            {
              rn <- clicksValues$range
              
              if (!is.null(clicksValues$click1))
              {
                abline(v = clicksValues$click1/60, lwd = 2, col = "darkorange")
              }
              
              if (!is.null(rn))
              {
                rect(rn[1]/60, -100, rn[2]/60, max(trace) * 2, 
                     col = rgb(.9, .4, 0, .1), border = "darkorange")
                
                region <- trace[rn[1]:rn[2]]
                
                # polygon(c(times[rn[1]], times[rn[1]:rn[2]], times[rn[2]]),
                #         c(min(trace), region, min(trace)), col = rgb(0, 0.4, 0.8, 0.5),
                #         border = NA)
                if (!is.null(calcium$baseline))
                  polygon(c(times[rn[1]:rn[2]], times[rn[2]:rn[1]]),
                          c(region, calcium$baseline[rn[2]:rn[1]]), col = rgb(0, 0.4, 0.8, 0.5),
                          border = NA)
              }
            }          
          }
          ############# FIND PEAKS ###############
          else if (input$visualization == "peaks")
          {
            if (input$bgSub)
            {
              trace <- calcium$traces[,input$traceNum] - calcium$bg
            }
            else
            {
              trace <- calcium$traces[,input$traceNum] 
            }
            
            times <- calcium$time / 60
            
            if (input$autoscaleY)
              plot(times, trace, t = "l", las = 1, xlim = c(input$timeFrom, input$timeTo),
                   xlab = "Time (min)", ylab = "", bty = "n")
            else
              plot(times, trace, t = "l", las = 1, xlim = c(input$timeFrom, input$timeTo),
                   xlab = "Time (min)", ylab = "", bty = "n", ylim = c(input$yFrom, input$yTo))
            
            q <- quantile(trace, input$centile/100)
            baseline <- trace
            baseline[baseline >= q] <- q
            ls <- loess(baseline ~ times, span = input$span)
            calcium$baseline <- predict(ls)
            
            if (input$showBase)
            {
              lines(times, calcium$baseline, col = "darkgreen", lwd = 2)
            }
            
            calcium$baselinesd <- sd(trace[trace <= quantile(trace, input$centile/100)])
            
            threshold <- baseline + input$minheight * calcium$baselinesd
            
            calcium$peaks <- NULL
            calcium$peaks.start <- NULL
            calcium$peaks.end <- NULL
            
            t <- 2
            last.end <- 1
            
            while (t < length(trace))
            {
              # Find points that are x SD over baseline
              if (trace[t] > threshold[t])
              {
                start <- which((trace[last.end:t] - baseline[last.end:t]) <= 0)
                if (length(start) > 0)
                  start <- max(start) + last.end
                else
                {
                  t <- t + 1
                  next
                }
                # Find the next crossing with the threshold
                end <- which((trace[-1:-t] - baseline[-1:-t]) <= 0)
                if (length(end) > 0)
                  end <- min(end) + t
                else
                {
                  t <- t + 1
                  next
                }
                
                last.end <- end
                
                peak <- which.max(trace[start:last.end]) + start - 1
                
                if ((end - start) >= input$peakwidth[1] &
                    (end - start) <= input$peakwidth[2])
                {
                  calcium$peaks.start <- c(calcium$peaks.start, start)
                  calcium$peaks.end <- c(calcium$peaks.end, end)
                  calcium$peaks <- c(calcium$peaks, peak)
                  
                  t <- tail(calcium$peaks.end, 1) + 1
                }
              }
              
              t <- t + 1
            }
            
            if (input$showPeaks)
            {
              points(times[calcium$peaks], trace[calcium$peaks], col = "purple", pch = 20, cex = 2)
              points(times[calcium$peaks.start], trace[calcium$peaks.start], col = "blue", pch = 20, cex = 1)
              points(times[calcium$peaks.end], trace[calcium$peaks.end], col = "orange", pch = 20, cex = 1)
            }
          }
        }
      }
    }

  output$mainPlot <- renderPlot(
    {
    plotTraceFn()
    })
  
  output$downloadPlot <- downloadHandler(
    contentType = "application/pdf",
    filename = "plot.pdf",
    content = function(file) 
      {
      pdf(file, width = 10, height = 8)
      plotTraceFn()
      dev.off()
      }
    )
  
  }) # end shinyServer
