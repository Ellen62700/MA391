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
r = 5/7
cost = function(a)500*(200/r*(a+1))+18000*a+(800*a*(200/r*(a+1)))+((200/(r*(a+1))>14)*(10000*(200/(r*(a+1))-14))*(-1)
p=function(t){(0.65*0.01*t)*(200+5*t-t^2/60)-0.45*t}
dP = function(t){fprime(p,t)}
t = seq(0,20)
ans = newton(dP,10)
print(ans)
print(p(ans))
library(ma391laporte)

months = seq(1,10)
ans.time=0
ans.profit=0
for (i in 1:length(months)){
m = months[i]
p = function(t){(0.65-0.01*t)*(5/m)*((m*t-t^2/60)+200-0.45*t}
dP = function(t){fprime(p,t)}
ans.time[i] = newton(dP,10)
ans.profit[i] = p(ans.time[i])
}
result = (months = months,time=ans.time,profit=ans.profit)






