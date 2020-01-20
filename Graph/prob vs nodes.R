library(igraph)
make_random <- function(num,prob)
{
  while(prob <= 1)
  {
    x = make_empty_graph(n=num)
    for(i in 1:(num-1))
    {
      for(j in (i+1):num)
      {
        r = runif(1)
        if(r<=prob)
          x = x + edge(i,j)
      }
    }
    if(is.connected(x))
      return (prob)
    else
      prob = prob + 0.01
  }
}
num = 2
xx = c()
yy = c()
while(num <= 100)
{
  p = make_random(num,0.001)
  xx = c(xx,num)
  #print(xx)
  yy = c(yy,p)
 # print(yy)
  num = num + 1
}

plot(xx,yy, xlab="Nodes", ylab="Probability", main="Probability vs Nodes", ylim=c(0,0.35), xlim=c(0,100), pch=15, col="blue",type = "l")