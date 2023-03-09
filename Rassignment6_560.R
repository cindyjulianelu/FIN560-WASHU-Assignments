#####################################################################
# R Assignment 6
#
# Cindy Lu
# Group 4
#
# Last modified: 01-May-2021
#####################################################################

###############
# Load packages
###############
library(dplyr)
library(GRS.test)

################
# Read data file
################
data_assignment6 <- read.csv("~/Downloads/dataAssignment06.csv", 
                             na.strings = c('', 'NA'))

######################
# Processing data file
######################

crsp_mini <- data_assignment6 

# Convert date variable to date format
crsp_mini$date = as.Date(as.character(crsp_mini$date),
                    format = '%m/%d/%Y')

# Convert return to numeric format
crsp_mini$RET = as.numeric(crsp_mini$RET)

# Remove dashes before price
crsp_mini$PRC = abs(data$PRC)

names(crsp_mini) <- tolower(names(crsp_mini)
                            
                            # Common shares only
                            data = data[ data$SHRCD == 10 | data$SHRCD == 11 , ]
                            
                            # NYSE, AMEX, NASDAQ
                            data = data[ data$EXCHCD == 1 | data$EXCHCD == 2 | data$EXCHCD == 3 , ]
                            
                            # Drop micro caps
                            data = data[ data$PRC >= 1 , ]
                            
                            # Define new variables
                            data = transform(data,
                                             LMVE = log(PRC*SHROUT),
                                             LPRC = log(PRC))
                            
                            # Omit rows with missing data
                            data = na.omit(data)
                            
                            # Vector of unique dates
                            date = unique(data$date)

######################
# Q3: Mutate variables 
######################

# Market Equity, log market equity, log share price, log volume, and bid-ask spread
crsp_mini <- crsp_mini %>%
  dplyr::mutate(me = abs(prc) * shrout, 
                log_me = log(me), 
                log_prc = log(abs(prc)), 
                log_vol = log(vol), 
                bid_ask = (ask-bid)/((ask_bid)/2)
                ) %>%
  dplyr::select(permno, date, ret, log_me, log_prc, log_vol, bid_ask)

###########################
# Q4: Fama-MacBeth approach
###########################

# Factor observations
x = xFf3

# Size of rolling window for estimating betas
roll = 60

# Intercept for monthly CSRs
gamma = matrix(NA, nrow(rx), 1)

# Slope coefficients for monthly CSRs
lambda = matrix(NA, nrow(rx), ncol(x))

# Iterate over months

for ( t in roll:(nrow(rx)-1) ){
  
  cat(sprintf('%s\n', date[ t+1 ]))
  
  # Excess return observations for estimating betas
  rxTsr = rx[ (t-(roll-1)):t ,  ]
  
  # Factor observations for estimating betas
  xTsr = x[ (t-(roll-1)):t ,  ]
  
  # Storage matrix for betas
  betaRoll = matrix(NA, ncol(rx), ncol(x))
  
  # Iterate over test assets
  
  for ( i in 1:ncol(rx) ){
    
    # Estimate TSR via OLS
    fitTsr = lm(rxTsr[ , i ] ~ xTsr)
    
    # Store beta estimates
    betaRoll[ i , ] = fitTsr$coefficients[ -1 ]
    
  }
  
  # Test asset returns for CSR
  rxCsr = rx[ t+1 , ]
  
  # Explanatory variables for CSR
  xCsr = betaRoll
  
  # Estimate CSR via OLS
  fitCsr = lm(rxCsr ~ xCsr)
  
  # Intercept estimate for CSR
  gamma[ t+1 ] = fitCsr$coefficients[ 1 ]
  
  # Slope coefficient estimates for CSR
  lambda[ t+1 , ] = fitCsr$coefficient[ -1 ]
  
}

# Drop rows with no values

gamma = gamma[ -(1:roll) ]

lambda = lambda[ -(1:roll) , ]

# Time-series averages

tsaGamma = mean(gamma)

tsaLambda = apply(lambda, 2, mean)

# Standard errors

seGamma = sd(gamma)/sqrt(length(gamma))

seLambda = apply(lambda, 2, sd)/sqrt(nrow(lambda))

# t-statistics

tstatGamma = tsaGamma/seGamma

tstatLambda = tsaLambda/seLambda

# Print results

cat('Fama-MacBeth estimation\n\n')
cat('gamma-hat = ', tsaGamma, '\n')
cat('t-stat = ', tstatGamma, '\n\n')

lambda = as.table(round(cbind(tsaLambda, tstatLambda), 2))

colnames(lambda) = c('coef', 't-stat')
rownames(lambda) = colnames(xFf3)

cat('Risk premia\n\n')
print(lambda)
