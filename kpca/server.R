#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.


library(shiny)
kernel_matrix <- function(kernel, data){
  # Calculate N 
  num_obs  <- nrow(data)
  data <- as.matrix(data)
  # Make an empty matrix of size NxN
  K <- matrix(nrow = num_obs, ncol = num_obs)
  # loop is optimized since the matrix is symmetric.
  for(row in 1:num_obs){
    for(col in row:num_obs){
      K[row, col] <- kernel(data[row,], data[col,])
      K[col, row] <- K[row, col]
    }
  }
  return(K)
}

kernel_pca <- function(kernel_matrix, thres = 0.0001){
  N <- nrow(kernel_matrix)
  # Create the mean operator matrix
  I_1_n <- matrix(1, nrow = N, ncol = N) * 1 / N
  # Create a NxN identity matrix
  I <- diag(N)
  # Center kernel matrix K
  K_star <- (I - I_1_n) %*% (kernel_matrix) %*% (I - I_1_n)
  
  # Solve the eigenvalue problem
  de = eigen(K_star)
  eigenvalues <- de$values / N
  
  # Find the eigenvalues whch are greater than the threshold
  index <- eigenvalues > thres
  eigenvalues <- eigenvalues[index]
  N_comp <- length(eigenvalues)
  
  # Standardized the eigenvectors
  pc <- t(t(de$vectors[, 1:N_comp]) / sqrt(eigenvalues[1:N_comp]))
  # Rotated PCs
  rpc <- K_star %*% pc
  return(list('pc'= pc, 'rpc' = rpc, 'eigen' = eigenvalues))
}

# Create a kernel function

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  data <- as.data.frame(apply(as.matrix(iris[,-5]), 2, scale))
  
  kernel_function <- reactive({
    if(input$kernel == 'poly'){
      k_x_y <- function(x, y, 
                        scale=input$scale_poly, 
                        degree=input$degree_poly, 
                        offset=input$offset_poly){
        return((scale*(t(x) %*% (y))+offset)^degree)
      }
    }
    if(input$kernel == 'normal'){
      k_x_y <- function(x, y, 
                        sigma=input$sigma_normal){
        # input is text needs to be converted to numeric.
        sigma <- as.numeric(sigma)
        norm <- (2*crossprod(x,y) - crossprod(x) - crossprod(y))
        return(exp(-sigma * norm))
      }
    }
    if(input$kernel == 'tan'){
      k_x_y <- function(x, y, 
                        scale=input$scale_tan, 
                        offset=input$offset_tan){
        return(tanh(scale*(t(x) %*% (y))+offset))
      }
    }
    
    if(input$kernel == 'laplace'){
      k_x_y <- function(x, y, 
                        sigma = input$sigma_laplace){
        sigma <- as.numeric(sigma)
        return(exp(-sigma*sqrt(-(round(2*crossprod(x,y) - crossprod(x) - crossprod(y),9)))))
      }
    }
    
    if(input$kernel == 'bessel'){
      k_x_y <- function(x, y, 
                        order = input$order_bessel,
                        sigma = input$sigma_bessel,
                        degree = input$degree_bessel){
        sigma <- as.numeric(sigma)
        lim <- 1 / (gamma(order+1) * 2 ^ (order))
        
        bkt <- sigma * sqrt(-(round(2*crossprod(x,y) - crossprod(x) - crossprod(y),9)))
        if(bkt < 10e-5){
          res <- lim
        }
        else{
          res <- besselJ(bkt, order) * (bkt ^ (-order))
        }

        return(as.numeric((res / lim) ^ degree))
      }
    }
    
    if(input$kernel == 'anova'){
      k_x_y <- function(x, y, 
                        degree = input$degree_anova,
                        sigma = input$sigma_anova){
        sigma <- as.numeric(sigma)  
       
        res <- sum(exp(-sigma * (x - y)^2))

        return((res) ^ degree)
      }
    }
    
    if(input$kernel == 'spline'){
      k_x_y <- function(x, y){
        min_val <- pmin(x,y)
        res <- 1 + x * y * (1 + min_val) - ((x+y) / 2)*min_val^2 + (min_val^3) / 3
        return(prod(res))
      }
    }
    

    k_x_y
  })
  
  # Create a kernel matrix
  PC <- reactive({
    
    K <- kernel_matrix(kernel_function(), data)
    print(K[1,2])
    KPrinComp <- kernel_pca(K, thres=0.0001)
    print(KPrinComp$eigen[1:3])
    KPrinComp
  })
  
  # Compute PC of kernel matrix

  output$pc_plots <- renderPlot({
    # Make a cool plot
    par(mfrow = c(1,2))
    
    plot(PC()$pc[, 1], PC()$pc[, 2], col = iris[, 5], cex = 0.5, 
         xlab = 'Principal Component 1',  ylab = 'Principal Component 2',
         main = 'Principal Component Scores Plot')
    
    plot(1:length(PC()$eigen), PC()$eigen, xlab = 'Component Number',  
         ylab = 'Eigenvalue', type = 'l', main = 'Principal Component Scree Plot')
  })

  
  
  output$report <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = "report.html",
    content = function(file) {
      
      rmarkdown::render("report.Rmd", output_file = file)

    }
  )
  
})

