LAGF<-function(x,y,x0){
  l<-length(x)
  sum<-0
  for(i in 1:l){
    numer<-prod(x0-x[-i])   #product of all pairs of x0-x[j] except j≠i
    denom<-prod(x[i]-x[-i])  #product of all pairs of x[i]-x[j] except j≠i
    sum<-sum+(numer/denom)*y[i]
  }
  return(sum)
}
# An example
x<-seq(.51, .57, .01)
y<-c(.5292437, .5378987, .5464641, .5549392, .5633233, .5716157, .5798158)
LAGF(x, y, .515)	
# R output	
[1] 	0.5335823542
