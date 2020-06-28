library(PortfolioAnalytics)
library(foreach)
library(iterators)
library(ROI)
library(ROI.plugin.quadprog)
library(ROI.plugin.glpk)
library(tidyverse)
library(tidyquant)
library(PerformanceAnalytics)
library(PortfolioAnalytics)
library(dplyr)

data(edhec)
data(edhec)
returns <- edhec[, 1:4]

colnames(returns) <- c("CA", "CTAG", "DS", "EM")
print(head(returns, 5))


# Get a character vector of the fund names
funds <- colnames(returns)

# Create portfolio object

portf_maxret <- portfolio.spec(assets=funds)

# Add constraints to the portfolio object
portf_maxret <- add.constraint(portfolio=portf_maxret, type="full_investment")

portf_maxret <- add.constraint(portfolio=portf_maxret, type="box",
                               min=c(0.02, 0.05, 0.03, 0.02), 
                               max=c(0.55, 0.6, 0.65, 0.5))



# Add objective to the portfolio object
portf_maxret <- add.objective(portfolio=portf_maxret, type="return", name="mean")


summary(portf_maxret)



# Run the optimization

opt_maxret <- optimize.portfolio(R=returns, portfolio=portf_maxret,
                                 optimize_method="ROI", trace=TRUE)



print(opt_maxret)
opt_maxret
summary(opt_maxret)




names(opt_maxret)



extractStats(opt_maxret)



extractWeights(opt_maxret)


#The plot method charts of the optimal weights with the box constraints along with the
#optimal portfolio in risk-return space. The blue dots are the optimal weights and the gray
#triangles are the min and max of the box constraints.

plot(opt_maxret, chart.assets=TRUE, xlim=c(0.02, 0.18))



#The optimal portfolio can be plotted in risk-return space along with other feasible
#portfolios. The return metric is defined in the return.col argument and the risk metric
#is defined in the risk.col argument. The scatter chart includes the optimal portfolio (blue
#dot) and other feasible portfolios (gray circles) to show the overall feasible space given the
#constraints. By default, if rp is not passed in, the feasible portfolios are generated with
#random_portfolios to satisfy the constraints of the portfolio object.
#Volatility as the risk metric

chart.RiskReward(opt_maxret,return.col="mean", risk.col="sd",
                 chart.assets=TRUE, xlim=c(0.01, 0.05), main="Maximum Return")



#Backtesting
#An out of sample backtest is run with optimize.portfolio.rebalancing. In this example, an initial training period of 36 months is used and the portfolio is rebalanced
#quarterly

bt_maxret <- optimize.portfolio.rebalancing(R=returns, portfolio=portf_maxret,
                                             optimize_method="ROI",
                                             rebalance_on="quarters",
                                             training_period=36)


bt_maxret
summary(bt_maxret)






