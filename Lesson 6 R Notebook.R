#Problem 5
#Color TV problem without constraints
#Now has tariff of $25 per unit
#Variables: s = x[1]  = number of 19-inch TVs sold (per year)
#           t = x[2] = number of 21-inch TVs sold (per year)
#           p  = selling price of a 19 in set ($)
#           q  = selling price of a 21 in set ($)
#           C  = cost of manufacturing sets ($/yr)
#           R  = revenue from sale of sets ($/yr)
#           P  = profit from the sale of sets ($/yr)
#Assumptions: p  - selling price of 19in set is affected by the number of 19in & 21in TV sets sold:  p=339−0.01s−0.003t
#             q  - selling price of 21in set is affected by the number of 19in & 21in TV sets sold:  q=339−0.004s−0.01t
#             R  - revenue is made only from selling these two TV sets:  R=ps+qt
#             C  - there is a fixed cost and enough of other materials not to cause any additional cost for making more sets:  C=400000+195s+225t
#             P=R−C
#             s≥0
#             t≥0
#a: Find the optimal production levels, taking the tariff into consideration
library(MASS);library(NlcOptim)
f = function(x){((339-0.01*x[1]-0.003*x[2])*x[1]
                   +(399-0.004*x[1]-0.01*x[2])*x[2]
                   -(400000+195*x[1]+225*x[2]))*(-1)-(25*x[1])-(25*x[2])
  }
x = c(4700,7042)
ans = optim(x,f,method="BFGS")
return(ans)
Z = Outer(f,x)
A = matrix(c(1,0,0,1))
B = matrix(c(0,0))
abline(h=0,col="red",lwd=3)
abline(v=0,col="red",lwd=3)
ans = solnl(x,objfun=f)
print(ans)
#The optimal production levels are s = 5660.96 and t = 7968.66.
s = 5660.96
t = 7968.66
25*t+25*s
#The tariff costs the company $340,740.5 annually.
#All of this tariff cost is paid directly to the government.

#b:Would it be worthwhile for the company to relocate production facilities to the U.S. in order to avoid the tariff?
#Assumptions: The overseas facility can be leased to another manufacturer for $200,000 per year
#             and that the cost of constructing and operating a new facility in the US would ammount
#             to $550,000 annually.
#             C = cost of moving to U.S. = 200,000*x - 550,000*x
#             x = number of years
#Find the price that it would cost to move to the U.S.
ans = 200000-550000
print(ans)
#Add the price that the company would not be paying in tariffs.
my.ans = ans+340740.5
print(my.ans)
#The company would be losing $9,259 by moving the facility to the U.S. It would not be worthwhile for the company to relocate.

#c: The purpose of the tariff is to motivate manufacturing companies to operate
#   plants in the U.S. What is the minimum tariff that would make it worthwhile
#   for the company to relocate its facility?
T = function(t){
  f = function(x){((339-0.01*x[1]-0.003*x[2])*x[1]
                 +(399-0.004*x[1]-0.01*x[2])*x[2]
                 -(400000+195*x[1]+225*x[2]))*(-1)-(t*x[1])-(t*x[2])
}
x = c(4700,7042)

#I tried this and it didn't work
g = seq(0,25,5)
ans = array(0,length(g))
for (i in 1:length(g)){
  profit = function (x){
    return(function(t){
      f = function(x){((339-0.01*x[1]-0.003*x[2])*x[1]
                       +(399-0.004*x[1]-0.01*x[2])*x[2]
                       -(400000+195*x[1]+225*x[2]))*(-1)-(t*x[1])-(t*x[2])
      }

#d: Given the tariff is large enough to motivate the company to move its facility,
#   how important is the action tariff amount?
#Pretty important. There is a difference between two numbers of whether or not it
#it is worth it to move to another country to cut down on costs. However, the
#price difference isn't that important, since the company wouldn't be making any
#money by moving unless the tariff was super huge. I guess the exact number isn't
#that important unless it is a huge number.

#Problem 7
#Variables: p = price($/week) = 1.5
#           s = x[1] = subscribers = 80000
#           a = x[2] = advertising revenue($/page) = 250
#           n = pages sold/week(pages/week) = 350
#           P = profit = s*p + a*n
#Assumptions:
#     1. an increase of 10 cents/week in subscription price will cause a drop in
#        circulation of 5,000 subscribers
#                - p = 1.6 ($/week); s = 75000
#     2. increasing the price of advertising by 100($/page) will cause the paper
#        to lose approximately 50 pages of advertising per week
#         - a = 350($/page); n = 300(pages/week); s = 79000
#a: find the weekly subscription rate and advertising price that will maximize the profit
library(ma391kvasnak)
fprime = function (f,a,h=0.0001){(f(a+h)-f(a-h))/(2*h)}
P = function(x)1.5*x[1]+350*x[2]
x = seq(1,)


)
}
x = seq(0,15,1)
plot(x,profit(x),type="o")

dProfit = function(x){fprime(profit,x)}
bisection(dProfit,0,20)
