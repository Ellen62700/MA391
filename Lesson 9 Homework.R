library(MASS);library(NlcOptim)
library(ma391kvasnak)
obj = function(x){((600-3*x[1]+x[2])*x[1]+(800-2*x[2]+x[1])*x[2])*(-1)}
X = list(x=seq(0,500),y=seq(0,500))
Z = Outer(obj,X)
## contour plot ##
contour(X$x,X$y,-Z)
abline(a=1000,b=-12/5,col="red",lwd=3)
Aeq = matrix(c(12,5),nrow = 1)
Beq = matrix(2500)
x0 = c(0,500)
ans = solnl(x0,objfun=obj,Aeq=Aeq,Beq=Beq)
print(ans)


obj = function(x){x[1]^2+x[2]^2}
X = list(x=seq(0,100),y=seq(0,100))
Z = Outer(obj,X)
contour(X$x,X$y,-Z)
abline(a=1,b=1/2,col="red",lwd=3)
abline(h=0,col="red",lwd=3)
abline(v=0,col="red",lwd=3)


plot(x,dF(x),type = "l",col = "red")
points(x,f(x),type = "l")
### Test Bisection ##
f = function(x){x^2-4}
bisection(f,1,10,0.000000000000001)

dF = function(x){fprime(f,x)}
x = seq(-3,3,.1)

plot(x,dF(x),type = "l",col = "red")
points(x,f(x),type = "l")
abline(v=0,col = "darkgreen")
abline(h=2,col = "gray")



