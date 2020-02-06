f = function(x){(x[1]*x[2]-2*x[1]-2*x[2]-x[1]^2-x[2]^2)*(-1)}
x = c(0,0)
ans = optim(x,f)
print(ans$par)
Blue = function(x){(0.05*x[1]*(1-x[1]/150000)-1/100000000*x[1]*x[2])}
Fin = function(x){(0.08*x[2]*(1-x[2]/400000)-1/100000000*x[1]*x[2])}
Rev = function(x){(12*(0.05*x[1]*(1-x[1]/150000)-1/100000000*x[1]*x[2])+
                     6*(0.08*x[2]*(1-x[2]/400000)-1/100000000*x[1]*x[2]))*(-1)
x = c(50000,50000)
ans = optim(x,Rev,method = "L-BFGS-B")
Blue(ans$par)
Fin(ans$par)
R2 = function(r2){
  fr2 = function(x){(12*(0.05*x[1]*(1-x[1]/150000)-1/100000000*x[1]*x[2])+
                     6*(r2*x[2]*(1-x[2]/400000)-1/100000000*x[1]*x[2]))*(-1)}
x = c(50000,50000)
ans = optim(x,Rev,method = "L-BFGS-B")
return(ans)
R2(0.08)
r = seq(0.06,.1,.01)
ans.x1 = 0
ans.x2 = 0
ans.rev = 0
for (i in 1:length(r)){
  ans = R2(r[i])
  ans.x1 = ans$par[1]
  ans.x2[i] = ans$par[2]
  ans.rev[i] = ans$value
}

result = data.frame(growth_rate = r, x1 = ans.x1, x2 = ans.x2, rev = ans.rev)
print(result)
