schedule <- function(pv, apr, ppy, years, extra = 0, summary = FALSE, graph = FALSE){
  
  
  # pv = present value of loan
  # apr = annual percentage rate
  # ppy = number of interest periods per year
  # years = loan period in years
  # extra = extra payment per period
  # summary = return full table or summary
  
  
  # Invalidate a negative extra payment
  
  if(extra < 0){
    return("Invalid Extra Payment Amount")
  }
  
  
  # Variables
  
  apr <- apr/100
  r <- apr/ppy
  n <- years * ppy
  p <- (pv*r) / (1 - (1 + r)^(-n))
  tp <- p + extra # total payment per month
  
  
  # Making the loan amortisation table
  
  df <- na.omit(data.frame(NA,NA,NA,NA,NA))
  
  for(j in 1:n){
    
    # Interest incurred during the i'th period
    int <- pv*r
    
    # When extra payments > 0, the last payment is usually less than the others
    if(tp > pv + int){ 
      tp <- pv + int
      df <- rbind(df, c(pv, tp, int, tp - int, pv + int - tp))
      break
    }
    
    # Update the loan table and balance
    df <- rbind(df, c(pv, tp, int, tp - int, pv + int - tp))
    pv <- pv + int - tp
  }
  
  
  # Total Payments Made and Total Interest, at every payment period
  
  totalPaid <- c(0); totalInt <- c(0)
  for(i in 1:length(df[,1])){
    totalPaid[i+1] <- df[i,2] + totalPaid[i]
    totalInt[i+1] <- df[i,3] + totalInt[i]
  }
  
  
  # Loan Summary Graph
  
  if(graph){
    
    l <- length(df[,1]) # Number of payments made
    
    # Chebyshev Nodes for x-axis
    a <- 1
    b <- l
    n <- floor(log(l))
    k <- 1:n
    x_k <- round(0.5*(a + b) + 0.5*(b - a)*cos((2*k - 1)*pi/(2*n)))
    x_k <- x_k[-c(1,length(x_k))] # Remove endpoints to avoid label overlap
    
    # Creating the graph
    m <- round(totalPaid[l+1], 2)
    i <- round(totalInt[l+1], 2)
    plot(NULL, xaxt = 'n', yaxt = 'n', xlim = c(1,l+1), ylim = c(0,m), main = "Loan Summary", xlab = "Payment Period", ylab = "")
    axis(side = 1, at = c(1,x_k,l+1), labels = c(1,x_k,l+1) - 1, cex.axis = 0.8, las = 2) # was starting at x = 1, so "labels" corrects this
    axis(side = 2, at = c(0,df[1,1],i,m), cex.axis = 0.8, las = 2) # y labels are 0, starting loan amount, total interest and total payments made
    lines(c(df[1,1],df[,5]), col = "red")
    lines(totalPaid, col = "blue")
    lines(totalInt, col = "green")
    legend("topleft", legend = c("Balance","Paid","Interest"), fill = c("red","blue","green"), cex = 0.6, inset = c(0, -0.18), xpd = TRUE)
  
  }
  
  
  # Return the full loan table, or a summary of the table
  
  if(!summary){
    display <- round(df, 2)
    colnames(display) <- c("Start Balance", "Total Payment", "Interest", "Principal", "End Balance")
  } else {
    display <- data.frame(round(df[1,2],2),length(df[,1]),round(sum(df[,2]),2),round(sum(df[,3]),2))
    colnames(display) <- c("Scheduled Payment", "Number of Payments", "Total Spent", "Total Interest")
  }
  
  return(display)
  
}


# Example: £3000 loan, 8% interest pa, twice-yearly payments over 5 years, no extra payments, display whole loan table and graph

schedule(3000, 8, 2, 5, graph = TRUE)

# Example: £6000 loan, 6% interest pa, monthly payments over 30 years, £15 extra payment each month, display loan summary and graph

schedule(6000, 6, 12, 30, extra = 15, summary = TRUE, graph = TRUE)