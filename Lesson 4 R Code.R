#Problem 7
#Pig problem again. Maximize profit rate ($/day). Already owned pig for 90 days
#   and have invested $100 in this pig to date
#Variables: t  = time (days)
#           w  = weight of the pig(lbs)
#           p  = price for pigs ($/lb)
#           C  = cost of keeping pig  t  days ($)
#           R  = revenue obtained by selling pig ($)
#           P  = profit from selling pig ($)
#Assumptions: w=200+5t
#             p=0.65−0.01t
#             C=0.45t
#             R=p⋅w
#             P=R−C
#             t>=0
#a: Find the best time to sell the pig.
library(ma391kvasnak)
fprime = function (f,a,h=0.0001){(f(a+h)-f(a-h))/(2*h)}

dX = function(x){.05*x*(1-(x/150000))-((10^-8)*x*y)}
dY = function(x){.08*y*(1-(y/400000))-((10^-8)*x*y)}

G = function(x){(.05*x*(1-(x/150000))-(a*x*y))+
    (.08*y*(1-(y/450000))-(a*x*y))}
dGdX = fprime(G,x)
dGdY = fprime(G,y)

x=0
G(0)
bisection(G,0,100000)
print(G(69103.7))

P = function(r1){
  f = function(x){(r1*x*(1-(x/150000))-((10^-8)*x*y))+
      (.08*y*(1-(y/400000))-((10^-8)*x*y))*-1
  }
  x = c(69103,196544)
  ans = optim(x,f,method="BFGS")
  ## Choose which of these that we will return ##
  print(ans$par)
  print(ans$value)
}
P(0.05)

#Problem 8
