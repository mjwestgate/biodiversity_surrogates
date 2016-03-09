library(shiny)

shinyUI(fluidPage(
  sidebarPanel(
    selectInput('dataset', 'Assemblage Metric:', c('composition', 'richness')),
	selectInput('method', 'Method:', c('method1', 'method2', 'method3')),
	selectInput('target', 'Target:', c('allSpecies', 'inverts', 'plants', 'verts')),
    sliderInput("budget",
                  "Number of groups (budget):",
                  min = 1,
                  max = 12,
                  value = 1,
				  step=1, sep="", animate=TRUE),
	selectInput('line.breaks', 'Line break attributes:', c('absolute', 'quantile')),
	selectInput('line.cols', 'Line colours:', c('Greys', 'Blues', 'Purples', 'Reds', 'Oranges')),
	width=3
   ),
  mainPanel(plotOutput("circleplot", height="600px"), width=9)
))