StockPrice <- replicate(10000,NA)

for (id in 1:10000){
Year <- 1:9

EconomicGrowth <- 0.02*rnorm(1,1,0.1)

NetIncomeGrowth1 <- 0.1925*rnorm(1,1,0.1)
NetIncomeGrowth <- NetIncomeGrowthRate1-((Year-1)/9)*
    (NetIncomeGrowthRate1-EconomicGrowth)
NetIncome <- replicate(9,208.4*(1+NetIncomeGrowth1))
i <- 2
while (i<=9) {
    NetIncome[i] <- NetIncome[i-1]*(1+NetIncomeGrowth[i])
    i <- i+1
}

EquityGrowth1 <- 0.1233*rnorm(1,1,0.1)
EquityGrowth <- EquityGrowth1-((Year-1)/9)*
    (EquityGrowth1-EconomicGrowth)
Equity <- replicate(9,1919.7*(1+EquityGrowth1))
i <- 2
while (i<=9) {
    Equity[i] <- Equity[i-1]*(1+EquityGrowth[i])
    i <- i+1
}

MarketPremium <- 0.064*rnorm(1,1,0.1)
RiskFree <- 0.004*rnorm(1,1,0.1)
Beta <- 1.0*rnorm(1,1,0.1)
CostOfEquity1 <- 0.089*rnorm(1,1,0.1)
CostOfEquity <- CostOfEquity1-((Year-1)/9)*
    (CostOfEquity1-(Beta*MarketPremium+RiskFree))

CostOfEquityEmployed <- CostOfEquity*Equity

ResidualIncome <- NetIncome-CostOfEquityEmployed

TerminalValue <- ResidualIncome[9]/(CostOfEquity[9]-EconomicGrowth)

FutureValue <- c(ResidualIncome[1:8],ResidualIncome[9]+TerminalValue)

PresentValue <- replicate(9,0)
for (i in 9:1) {
    PresentValue[i] <- FutureValue[i]/
        (1+ifelse(i>=9,1,0)*CostOfEquity[9])/
        (1+ifelse(i>=8,1,0)*CostOfEquity[8])/
        (1+ifelse(i>=7,1,0)*CostOfEquity[7])/
        (1+ifelse(i>=6,1,0)*CostOfEquity[6])/
        (1+ifelse(i>=5,1,0)*CostOfEquity[5])/
        (1+ifelse(i>=4,1,0)*CostOfEquity[4])/
        (1+ifelse(i>=3,1,0)*CostOfEquity[3])/
        (1+ifelse(i>=2,1,0)*CostOfEquity[2])/
        (1+ifelse(i>=1,1,0)*CostOfEquity[1])
}

NPV <- sum(PresentValue)

SharesOutstanding <- 80.1

StockPrice[id] <- NPV/SharesOutstanding
}

library(tidyverse)
ggplot(data = data.frame(StockPrice)) +
    geom_histogram(aes(x=StockPrice))+
    geom_vline(xintercept=median(StockPrice),color="darkblue") +
    theme_light()

write.csv(StockPrice,"MonteCarlo.csv")
    