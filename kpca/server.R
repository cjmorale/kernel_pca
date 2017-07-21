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
      print(input$degree_poly)
      print(k_x_y)
      k_x_y
    }
    if(input$kernel == 'normal'){
      k_x_y <- function(x, y, 
                        sigma=input$sigma_normal){
        #return((scale*(t(x) %*% (y))+offset)^degree)
      }

    }
    if(input$kernel == 'tan'){
      k_x_y <- function(x, y, 
                        scale=input$scale_tan, 
                        offset=input$offset_tan){
        return(tanh(scale*(t(x) %*% (y))+offset))
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


  output$distPlot <- renderPlot({
    # Make a cool plot
    plot(PC()$pc[, 1], PC()$pc[, 2], col = iris[, 5], cex = 0.5)
  })
  
})

