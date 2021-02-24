library(numDeriv)
#weibull probability density function
weibull<-function(x,lambda,k){
  ifelse(x>=0,k/lambda*(x/lambda)^(k-1),0)
}

x_0<-seq(.1,3,.1) #x_0 should be the independent variable
y_0<-weibull(x_0,.5,2) #for purposes of this exercise
z_0<-jitter(y_0) #this should be dependent variable

weibull_cost<-function(lst){
  sum((z_0-weibull(x_0,lst[1],lst[2]))^2)
}

lst<-matrix(nrow=101,ncol=2) #create an empty matrix to fill
lst[1,1]=1 #set starting value for lambda
lst[1,2]=1 #set starting value for k
costs<-matrix(nrow=1001,ncol=1) #empty matrix to fill for cost function

weibull_fit<-function(lambda_0,k_0,iterations,step_size){
  lst<-matrix(nrow=iterations+1,ncol=2) #create an empty matrix to fill
  lst[1,1]=lambda_0 #set starting value for lambda
  lst[1,2]=k_0 #set starting value for k
  costs<-matrix(nrow=iterations+1,ncol=1) #empty matrix to fill for cost function
  
  for(i in 1:iterations){
    if(sum(grad(weibull_cost,lst[i,])^2)<.1){ 
      break #stops the loop when the gradient gets close to 0
    }
    lst[i+1,]<-lst[i,]-step_size*grad(weibull_cost,lst[i,]) #update by subtracting a fraction of the gradient
    costs[i]<-weibull_cost(lst[i,])
  }
  print(lst[iterations+1,])
}

#plot results
ggplot()+
  geom_line(aes(x=x_0,y=weibull(x_0,lambda=weibull_fit(2,.5,1000,.00001)[1],k=weibull_fit(2,.5,1000,.00001)[2])))+ #weibull fit
  geom_line(aes(x=x_0,y=z_0)) #actual data
