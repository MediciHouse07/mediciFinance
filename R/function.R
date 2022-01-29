# PV, present value of money
# FV, future ...
# r, interest rate
# n, number of years
# option, decide you want to calculate PV or FV

tvalue <- function(PV,FV,r,n,option){
  if(option=='PV'){
    result <- FV/((1+r)^n)
  }
  if(option=='FV'){
    result <- PV*((1+r)^n)
  }
  if(option=='r'){
    result <- ((FV/PV)^(1/n)) - 1
  }
  if(option=='n'){
    result <- log(FV/PV)/log(1+r)
  }
  else print("Incorrect input, please double check.")
  return(result)
}

sum_geo <- function(f_item,ratio,n){
  s <- (((1-(1/(1+ratio))^n)*f_item)/(1-(1/(1+ratio))))/(1+ratio)
  return(s)
}

# par - face value
# c_rate - coupon rate
# m_year - year of maturity
# ymt - yield to maturity
# np_year - number of payments by year

b_evaluation <- function(par,c_rate,m_year,ymt,np_year){
  value <- sum_geo((par*c_rate),ymt,m_year) + (par/((1+ymt)^m_year))
  print(paste0('PIVFA is ',sum_geo((par*c_rate),ymt,m_year)))
  print(paste0('Evaluated priciple is ',(par/((1+ymt)^m_year))))
  return(value)
}
