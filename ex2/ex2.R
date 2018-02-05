# Programming Exercise 2: Logistic Regression

# remove all list
rm(list = ls())

library(ggplot2)
# SET WORK FOLDER
setwd("~/Desktop/Coursera_MSLN/recreate_r/ex2")

# BRING IN DATA
data1<-read.table("ex2data1.txt",sep=",",header = F)
colnames(data1)<-c('s1','s2','decision')
# number of training examples

head(data1)

X<-data1[,1:2]
y<-data1[,3]
m <- nrow(data1)
n<-ncol(X)

# numeric to discrete factor for graphic
data1$decisionf<-as.factor(data1$decision)
# Set color by cond
ggplot(data1, aes(x=s1, y=s2, color=decisionf, shape=decisionf)) + geom_point()+scale_shape_manual(values=c(1,2))


# sigmoid function
sigmoid<-function(z){
  # %SIGMOID Compute sigmoid functoon
  # %   J = SIGMOID(z) computes the sigmoid of z.
  enez = exp(-z);
  g = 1/(1+enez);
  return(g)
}

# test on sigmoid function and plot it
tst_seq<-seq(-500,500,by=5)
# sigseq<-c()
for(i in 1:length(tst_seq)){
  sigval<-sigmoid(tst_seq[i])
  if(i==1){
    sigseq<-c(sigval)
  }else{sigseq<-rbind(sigseq,sigval)}
}
tstplot<-as.data.frame(cbind(tst_seq,sigseq))
colnames(tstplot)<-c("tst_seq","sig_vals")
ggplot(tstplot, aes(x=tst_seq, y=sig_vals)) + geom_point()

# Cost function and gradient
costFunction<-function(theta,X,y){
  # %COSTFUNCTION Compute cost and gradient for logistic regression
  # %   J = COSTFUNCTION(theta, X, y) computes the cost of using theta as the
  # %   parameter for logistic regression and the gradient of the cost
  # %   w.r.t. to the parameters.
  
  #  number of training examples
  m<-length(y)
  yhat<-sigmoid(X%*%theta)
  J<-(1/m)*sum(-y*log(yhat)-(1-y)*log(1-yhat))
  
  grad<-array(data=0,dim=c(dim(theta)))
  
  for(i in 1:length(theta)){
    grad[i]= (1/m)*sum((yhat-y)*X[,i])
  }
  results<- list("J"=J,"grad"=grad)
  return(results)
}

# Add intercept term to x 
# % Add intercept term to X
ones<-array(data=1,dim=c(m,1))
X<-cbind(ones,X)
# convert X to matrix for matrix cal in functions
X<-as.matrix(X)

# % Initialize fitting parameters
initial_theta = array(data=0,dim=c(n+1,1))

# % Compute and display initial cost and gradient
cf.rs<-costFunction(initial_theta, X, y);
cost<-cf.rs$J
grad<-cf.rs$grad

# define fr and grr seperately from costFunction
fr_cost<-function(theta_v,data1){
  theta<-as.matrix(theta_v)
  X<-data1[,1:2]
  y<-data1[,3]
  m <- nrow(data1)
  n<-ncol(X)
  # % Add intercept term to X
  ones<-array(data=1,dim=c(m,1))
  X<-cbind(ones,X)
  # convert X to matrix for matrix cal in functions
  X<-as.matrix(X)
  
  cf.rs<-costFunction(theta, X, y);
  cost<-cf.rs$J
  return(cost)
}
grr_grad<-function(theta_v,data1){
  theta<-as.matrix(theta_v)
  X<-as.matrix(data1[,1:2])
  y<-data1[,3]
  m <- nrow(data1)
  n<-ncol(X)
  # % Add intercept term to X
  ones<-array(data=1,dim=c(m,1))
  X<-cbind(ones,X)
  # convert X to matrix for matrix cal in functions
  # X<-as.matrix(X)
  cf.rs<-costFunction(theta, X, y);
  grad<-cf.rs$grad
  return(grad)
}

grr_grad(c(0,0,0),data1)
optim(c(0,0,0), fr_cost,data1=data1)
op<-optim(par=c(0,0,0), fn=fr_cost, gr=grr_grad, data1=data1,
          method = "BFGS",control = list("maxit"=400))

# plot boundary
int<- - op$par[1]/op$par[3]
slp<- - op$par[2]/op$par[3]
p + geom_abline(intercept = int, slope = slp)


