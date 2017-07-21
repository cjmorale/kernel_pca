#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Kernel PCA"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectInput("kernel",
                  "select kernel:",
                  c("Polynomial" = "poly",
                    "Gaussian" = "normal",
                    "hyperbolic tangent" = "tan",
                    "custom" = "custom")),
      conditionalPanel("input.kernel == 'poly'",
                       
         sliderInput("degree_poly",
                     "degree of polynomial kernel:",
                     min = 1,
                     max = 10,
                     value = 2),
         sliderInput("scale_poly",
                     "scale of polynomial kernel:",
                     min = 1,
                     max = 10,
                     value = 1),
         sliderInput("offset_poly",
                     "offset of polynomial kernel:",
                     min = -10,
                     max = 10,
                     value = 0)
      ),
      conditionalPanel("input.kernel == 'normal'",
                       textInput('sigma_normal', 'Sigma of gaussian kernel:', 
                                 value = ".1")

      ),
      conditionalPanel("input.kernel == 'tan'",
                       sliderInput("scale_tan",
                                   "scale of hyperbolic tangent kernel:",
                                   min = 1,
                                   max = 10,
                                   value = 1),
                       sliderInput("offset_tan",
                                   "offset of hyperbolic tangent kernel:",
                                   min = -10,
                                   max = 10,
                                   value = 0)
                       
                       
      )
       
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
       plotOutput("distPlot")
    )
  )
))
