#Problem 4#
#Variables: d = distance(miles)=200
#           t = time(days)=14
#           c1 = cost of local clean up crew at rate of cleanup($/day)=500
#           c2 = cost of additional clean up crew at rate of cleanup($+$/day)=$18000+$800/day
#           a = # of additional crews(#)
#           f = cost of fine after 14 days($/day)=10,000
#           r = rate of cleanup(miles/day)
#Assumptions: All crews clean up spill at same rate; number of days
#             to complete only depends on rate/# of crews
#cost = c1+c2+f
#c1 = 500*t
#c2 = 18000a+(800*t*a)
#f = step function thing:10,000*t when t>14
#                           t     when t<14
#t = 200/r*(a+1)
#a: How many additional crews should be brought in to minimize the total cost to the company?

library(ma391kvasnak)
months = seq(1,10)
ans.time=0
ans.profit=0
for (i in 1:length(months)){
m = months[i]
p = function(t){(0.65-0.01*t)*(5/m)*((m*t-t^2/60)+200-0.45*t}
dP = function(t){fprime(p,t)}
ans.time[i] = newton(dP,10)
ans.profit[i] = p(ans.time[i])
result =(months = months,time=ans.time,profit=ans.profit)
r = 5/7
fT = function(x){500*(200/(r*(x+1)))+(18000+800*200/(r*(x+1)))*x+(200/(r*(x+1))>14)*(10000*(200/(r*(x+1))-14))}
dfT = function(x){fprime(fT,x)}
x = seq(0,20,1)
plot(x,fT(x),type="o")
print(fT(11))
#The minimum cost with crews is $508,333.30
#b: Examine the sensitivity to the rate at which a crew can clean up the shoreline.
ans.crew=0
ans.cost=0
ans.cost1 <- na.omit(ans.cost)
r = seq(2/7,8/7,1/7)
for (i in 1:length(r)){
  ## Change r to the variable r[i] to iterate the changing rate
  fT = function(x){return(500*(200/(r[i]*(x+1)))+(18000+800*200/(r[i]*(x+1)))*x+(200/(r[i]*(x+1))>14)*(10000*(200/(r[i]*(x+1))-14)))}
  dfT = function(x){fprime(fT,x)}
  ans.crew[i] = bisection(dfT,0,20)
  ans.cost1[i] = fT(ans.crew[i])
}
result = data.frame(rate = r, crew = ans.crew, cost = ans.cost1)
print(result)
#As rate increases from 2/7 to 8/7 the amount of crews needed decreases as well as the cost.
#c: Examine the sensitivity to the amount of fine.
ans.days=0
ans.cost=0
ans.cost1 <- na.omit(ans.cost)
f = seq(0,50000,10000)
for (i in 1:length(f)){
  r=5/7
  fT = function(x){return(500*(200/(r*(x+1)))+(18000+800*200/(r*(x+1)))*x+(200/(r*(x+1))>14)*f[i])}
  dfT = function(x){fprime(fT,x)}
  ans.days[i] = bisection(dfT,14,10000)
  ans.cost1[i] = fT(ans.days[i])
}
result = data.frame(fine = f, days = ans.days, cost = ans.cost1)
print(result)
# The derivative will never go to Zero#

#d: Is the fine excessive?
#Yes, it does appear that the fine is excessive.

#Problem 5
#Variables: g = growth rate= r*x(1-x[2]/k)
#           r = intrinsic growth rate = 0.08
#           K = maximum sustainable population = 400000
#           x = x[2] = current population = 70,000
#           n = number of whales harvested per year = 0.00001*E*x
#           E = x[1] = level of fishing effort in boat(days)
#a: What level of effort will maximize the sustained harvest rate?
Blue = function(x){(0.05*x[1]*(1-x[1]/150000)-1/100000000*x[1]*x[2])}
Fin = function(x){(0.08*x[2]*(1-x[2]/400000)-1/100000000*x[1]*x[2])}

Rev = function(x){(12*(0.05*x[1]*(1-x[1]/150000)-1/100000000*x[1]*x[2])+6*(0.08*x[2]*(1-x[2]/400000)-1/100000000*x[1]*x[2]))*(-1)}
x=c(50000,50000)
ans = optim(x,Rev,method = "L-BFGS-B")
Blue(ans$par)
Fin(ans$par)
# The level of effort that will maximize the sustained harvest rate is when Blue = 1731 and Fin = 7856.

#b: Examine the sensitivity to the intrinsic growth rate
R2 = function(r2){
  fr2=function(x){(12*(0.05*x[1]*(1-x[1]/150000)-1/100000000*x[1]*x[2]) +

                     6*(r2*x[2]*(1-x[2]/400000)-1/100000000*x[1]*x[2]))*(-1)}
  x=c(50000,50000)
  ans=optim(x,Rev,method="L-BFGS-B")
  return(ans)
}
R2(0.08)
r = seq(0.06,0.1,0.01)
ans.x1=0
ans.x2=0
ans.rev=0
for (i in 1:length(r)){
  ans = R2(r[i])
  ans.x1[i] = ans$par[1]
  ans.x2[i] = ans$par[2]
  ans.rev[i] = -ans$value
}
result=data.frame(growth_rate=r,x1=ans.x1,x2=ans.x2,revenue=ans.rev
)
print(result)

#c: Examine the sensitivity to the maximum sustainable population
K2 = function(k2){
  fr2=function(x){(12*(0.05*x[1]*(1-x[1]/150000)-1/100000000*x[1]*x[2])+
                    6*(0.08*x[2]*(1-x[2]/k2)-1/100000000*x[1]*x[2]))*(-1)}
K2(400000)
r = seq(0.06,0.1,0.01)
ans.x1=0
ans.x2=0
ans.rev=0
for (i in 1:length(r)){
  ans = K2(r[i])
  ans.x1[i] = ans$par[1]
  ans.x2[i] = ans$par[2]
  ans.rev[i] = -ans$value
}
result=data.frame(sustainable.population = K,x1=ans.x1,x2=ans.x2,revenue=ans.rev
)
print(result)

#Problem 6
#Variables: c = cost of whaling($/day)=500
#           d = number of days
#           f = price of a fin whale carcass($) = 6,000
#           n = x[1] = number of fin whale carcasses sold
#           P = x[2] = profit = f*n-c*d or 6000*n-500*d of 6000*x[1]-500*x[2]
#a: Find the level of effort that will maximize profit over the long term
fprime = function (f,a,h=0.0001){(f(a+h)-f(a-h))/(2*h)}
P = function(x)6000*x[1]-500*x[2]
x = c(500,500)
dP = function(x){fprime(P,x,)}

ans = optim(x,dP,method = "L-BFGS-B")
print(ans)
#It appears that the level of effort that will maximize profit over the long term is 5500.

#b: Sensitivity to the cost of whaling
cost = function(c){
  P = function(x)6000*x[1]-c*x[2]}
  x = c(500,500)
  dP = function(x){fprime(P,x,)}



