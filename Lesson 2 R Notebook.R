# Problem 1#
#Variables: R = x[1] = rebate ($)
#           P = Profit = obj = 1500 ($)
#           S = Sales = R/100 * .15
#Assumptions: As rebate increases $100, sales increase 15%

#a: What amount of rebate will maximize profit? Use one-variable optimization #
library(ma391kvasnak)
p = function (x){
  return((1500-100*x)*(1+.15*x))
}
x = seq(0,15,1)
plot(x,profit(x),type="o")

dProfit = function(x){fprime(profit,x)}
bisection(dProfit,0,20)
#The amount of rebate that will maximize profit is $4.16 #

#b: Compute the Sensitivity of your answer to the 15% assumption. Consider the amount of rebate and the resulting profit#
sales = seq(0.05,0.5,0.06)
ans = 0
ans.profit=0
for (i in 1:length(sales)){
  profit = function (x){
    return((1500-100*x)*(1+sales[i]*x))
  }
  dProfit = function(x){fprime(profit,x)}
  ans[i] = bisection(dProfit,-10,20)
  ans.profit[i]=profit(ans[i])
}
result = data.frame(salesIncrease=sales,optimalRebate=ans,profit=ans.profit)
print(result)
plot(sales,ans,"o",xlab="Sales Increase",ylab="x (# of rebates)")
#  salesIncrease optimalRebate   profit
1          0.05     -2.500000 1531.250
2          0.11      2.954547 1596.023
3          0.17      4.558823 1853.309
4          0.23      5.326085 2152.446
5          0.29      5.775863 2467.457
6          0.35      6.071428 2790.179
7          0.41      6.280488 3117.226
8          0.47      6.436171 3446.941

#c: What is the effect if rebates actually generated a 10% increase in sales per $100? What if the response is somewhere between 10 and 15% per $100 rebate?

profit = function (x){
  return((1500-200*x)*(1+.05*x))
}
x = seq(0,15,1)
plot(x,profit(x),type="o",xlab="x(# of rebates)")
# If rebates actually generated a 10% increase in sales per $100, the number of rebates would decrease at a constant rate. If the response was somewhere between 10 to 15% per $100 rebate, the number of rebates would increase at a slower rate exponentially as profit decreases.


#Problem 2#
#Sensitivity Analysis for Pig Problem

feed = seq(0.3,0.6,0.05)
pr = array(0,length(feed))
ans = array(0,length(feed))
for (i in 1:length(feed)){
  profit = function (x){
    return((0.65-0.01*x)*(200+5*x)-feed[i]*x)
  }
  dProfit = function(x){fprime(profit,x,)}
  ans[i] = bisection(dProfit,-100,50,0.0001)
  pr[i] = profit(round(ans[i]))
}
print(ans)
print(pr)
plot(feed,ans,"o",xlab="feed cost",ylab="x(Days to Sell)")
title("Sensitivity of Feed Cost of the Pig")
# If a new feed costing 60 cents/day would let the pig grow at a rate of 7 lbs/day, would it be worth switching feed? What is the minimum improvement in growth rate that would make this new feed worthwile?
# .60 per day feeding / growth rate increase of 7 lbs per day / assume 132.8 was previous profit
library(ma391kvasnak)
profit = function (x){
  return((0.65-0.01*x)*(200+7*x)-0.6*x)
}
x = seq(0,20,1)
plot(x,profit(x),type="o",xlab="x(days)")
abline(h=132.8) #represents the max profits for previous feed
dProfit = function(x){fprime(profit,x)}
ans=bisection(dProfit,0,20)
print(ans)
#The minimum improvement in growth rate that would make this feed worthwile is ##.

#Problem 3#
#From Pig Problem (problem 2) now consider P = 0.65-0.01*t-0.00004*t^2
# P = the price for pigs (cents/lb) after t days
#a: graph P with original price equation (profit = function (x){
#return((0.65-0.01*x)*(200+7*x)-0.6*x)}
p1 = function(x)(0.65-0.01*x)*(200+7*x)-0.6*x
p2 = function(x)0.65-0.01*x+0.00004*x^2
t = seq(0,20)

plot(t,p1(t),"l",lwd=3)
points(t,p2(t),"o",col="red")
#The original price equation could be considered as an approximation to Eq. (1.5) for values of t near zero because it does not take into consideration the second part of the equation, which must be a more accurate model of the price of pigs.

#b: Find the best time to sell the pig.
