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
shinyUI(fluidPage(theme = "bootstrap.css", 
  
  # Application title
  titlePanel("Kernel PCA"),
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(

      selectInput("kernel",
                  "select kernel:",
                  c("Polynomial kernel function" = "poly",
                    "Radial Basis kernel function (Gaussian)" = "normal",
                    "Hyperbolic tangent kernel function" = "tan",
                    "Laplacian kernel function" = "laplace",
                    "Bessel kernel function" = 'bessel',
                    "ANOVA RBF kernel function" = 'anova',
                    'Spline kernel function' = 'spline')
                  ),

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
                                 value = "0.1")
      ),
      
      conditionalPanel("input.kernel == 'laplace'",
                       textInput('sigma_laplace', 'Sigma of laplace kernel:', 
                                 value = "0.1")
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
                       ),
      conditionalPanel("input.kernel == 'bessel'",

                       sliderInput("degree_bessel",
                                   "Degree of bessel kernel:",
                                   min = 1,
                                   max = 10,
                                   value = 1),
                       sliderInput("order_bessel",
                                   "Order of bessel kernel:",
                                   min = 1,
                                   max = 10,
                                   value = 1),
                       textInput('sigma_bessel', 'Sigma of bessel kernel:', 
                                 value = "0.1")
      ),
      conditionalPanel("input.kernel == 'anova'",
                       sliderInput("degree_anova",
                                   "Degree of anova kernel:",
                                   min = 1,
                                   max = 10,
                                   value = 1),
                       textInput('sigma_anova', 'Sigma of anova kernel:', 
                                 value = "0.1")
      ),

      downloadButton("report", "Download Plots")
      
       
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      conditionalPanel("input.kernel == 'anova'",
                       withMathJax(
                         helpText('The kernel function is defined as $$k(x,y)=\\sum_{i=1}^n exp(−\\sigma(x−y)^2)^{degree}$$ ')
                       )
      ),
      conditionalPanel("input.kernel == 'bessel'",
                       withMathJax(
                         helpText('The kernel function is defined as $$k(x,y)=−Bessel^n_{(ν+1)}(\\sigma‖x−y‖2)$$ ')
                       )
      ),
      conditionalPanel("input.kernel == 'laplace'",
                       withMathJax(
                         helpText('The kernel function is defined as  $$k(x, y) = (\\exp(-\\sigma \\lVert x - y \\rVert)$$ ')
                       )
      ),

      conditionalPanel("input.kernel == 'poly'",
                       withMathJax(
                         helpText('Th kernel function is defined as  $$k(x, y) = (scale \\cdot x^Ty + offset)^{degree}$$ ')
                       )
      ),
      conditionalPanel("input.kernel == 'normal'",
                       withMathJax(
                         helpText('The kernel function is defined as  $$k(x, y) = \\exp(-\\sigma|| x - y ||^2 $$ ')
                       )
      ),
      conditionalPanel("input.kernel == 'spline'", 
                       withMathJax(
                         helpText('The kernel function is defined as $$k(x,y)=\\prod_{i=1}^d \\left(1+xy+xy\\cdot\\min(x,y)−\\frac{x+y}{2} \\min(x,y)+\\frac{1}{3}\\cdot\\min(x,y)^3\\right)$$ ')
                       )
      ),
      conditionalPanel("input.kernel == 'tan'",
                       withMathJax(
                         helpText('The kernel function is defined as  $$k(x, y) = \\tanh(scale\\cdot x^T y + offset)$$ ')
                       )
      ),
       plotOutput("pc_plots"),
       withTags({
        div(class="header", checked=NA,
            p("Source code is avaliable on github"),
            a(href="https://github.com/cjmorale/kernel_pca", "https://github.com/cjmorale/kernel_pca")
        )
      })
      
    )
  )
))
