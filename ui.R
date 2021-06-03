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

setwd("/lab/Lab/CalciumAnalysis/Traces/")
files <- dir(pattern = "csv$")
files <- files[grep("treatments", files, invert = T)]

shinyUI(
  fluidPage
      (
      titlePanel("Calcium Imaging Analysis"),
  
      sidebarLayout
        (
        sidebarPanel
            (
            selectInput("experiment", "Experiment", choices = c("Select an experiment", files)), 
            selectInput("visualization", label = "Plot type", choices = c("All traces" = "traces",
                                      "Single trace" = "single"
                                      #"Find peaks" = "peaks"
                                      )),
            
            fluidRow(
              column(width = 3,
                numericInput("timeFrom", "Min time", min = 0, max = 1000, step = 0.5, value = 0,
                             width = 100)),
              column(width = 3,
                numericInput("timeTo", "Max time", min = 0, max = 1000, step = 0.5, value = 120,
                             width = 100)),
              
              column(width = 3,
                numericInput("yFrom", "Min y", step = 0.5, value = 0, 
                             width = 100)),
              column(width = 3,
                numericInput("yTo", "Max y", step = 0.5, value = 1000,
                             width = 100)),

              column(width = 5,
                checkboxInput("autoscaleY", "autoscale y", value = TRUE)),
            
              column(width = 5,
                     checkboxInput("normalise", "normalise y", value = FALSE))
               ),
              
          downloadButton('downloadPlot', 'Download Plot (PDF)'),
              
          conditionalPanel(
                "(input.visualization == 'single' || input.visualization == 'peaks') && input.experiment != 'Select an experiment'",
                sliderInput("traceNum", label = "Trace #", min = 1, max = 10, step = 1, value = 1,
                            round = TRUE, ticks = FALSE),
                checkboxInput("bgSub", label = "Background subtracted", value = TRUE),
                checkboxInput("showBase", label = "Show baseline", value = TRUE),
                checkboxInput("showActThr", label = "Show activity threshold", value = FALSE),
                sliderInput("centile", label = "Centile", min = 1, max = 100, step = 1, value = 10),
                sliderInput("span", label = "Span", value = 0.2, min = 0.01, max = 2, step = 0.01),
                sliderInput("activityThresh", label = "Activity threshold", value = 0.15, min = 0.01, max = 1, step = 0.01)
            ), # conditionalPanel
    
            # conditionalPanel(
            #     "input.visualization == 'peaks' && input.experiment != 'Select an experiment'",
            #     checkboxInput("showPeaks", label = "Show peaks", value = TRUE),
            #     sliderInput("peakwidth", label = "Peak length (s)", min = 3, max = 600, step = 1, value = c(10, 300)),
            #     sliderInput("minheight", label = "Minimum peak height (SD over baseline)", value = 20, min = 2, max = 100, step = 0.1)
            #     ), # conditionalPanel

            conditionalPanel(
                  "input.visualization == 'single' && input.experiment != 'Select an experiment'",
                  checkboxInput("showTreatments", "Show treatments", value = TRUE),
                  checkboxInput("showStats", "Show statistics", value = TRUE)
              ), # conditionalPanel
            
            conditionalPanel(
                "input.showStats == 1 && input.visualization == 'single' 
                  && input.experiment != 'Select an experiment'",
                verbatimTextOutput("plotStats")
                )
            ), # sidebarPanel
          
          mainPanel
              (
              plotOutput("mainPlot", height = "500px", click = "plotClick", 
                         hover = hoverOpts("plot_hover", delay = 100, delayType = "throttle")),
              conditionalPanel(
                  "input.visualization == 'single' 
                  && input.experiment != 'Select an experiment'",
                  verbatimTextOutput("plotCoords")),
              conditionalPanel(
                  "input.visualization != 'peaks' 
                  && input.experiment != 'Select an experiment'",
                  tableOutput("treatmentTable"))
              # conditionalPanel(
              #     "input.visualization == 'peaks' 
              #     && input.experiment != 'Select an experiment'",
              #     tableOutput("peakStats"))
              ) # mainPanel
      ) # sidebarLayout
    ) # fluidPage
  ) # shinyUI