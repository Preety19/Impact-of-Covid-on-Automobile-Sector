plot(density(data_7_$feb))
plot(density(data_7_$oct))

shapiro.test(data_7_$oct)
shapiro.test(data_7_$feb)

library("boot")
set.seed(1001)
####quantiles and ci for feb####
x=data_7_$feb
x=as.vector(x)
b <- function(x,i){
  quantile = quantile(x[i])
  return(quantile)
}

bs = boot(x,b,R=1000)
bs
bs[["t"]] # view bootstrap samples
s=summary(bs[["t"]]) #to find mean of each quantile for bootstrap samples
s


v50=var(bs$t[,3])
v25=var(bs$t[,2])
v75=var(bs$t[,4])

quantile(x)
ci25=c(7817.550-1.96*sqrt(v25),7817.550+1.96*sqrt(v25))
ci25

ci50=c(7973.225-1.96*sqrt(v50),7973.225+1.96*sqrt(v50))
ci50

ci75=c(8149.975-1.96*sqrt(v75),8149.975+1.96*sqrt(v75))
ci75

####quantiles and ci for oct####
y=data_7_$oct
y=as.vector(y)
b <- function(y,i){
  quantile = quantile(y[i])
  return(quantile)
}

ps = boot(y,b,R=1000)
ps
ps[["t"]] # view bootstrap samples
s2=summary(ps[["t"]]) #to find mean of each quantile for bootstrap samples
s2


vp50=var(ps$t[,3])#var of 50th percentile



vp50


vp25=var(ps$t[,2]) #var of 25th percentile
vp25


vp75=var(ps$t[,4]) #var of 75th percentile
vp75

quantile(y)
ci25=c(8034.125-1.96*sqrt(vp25),8034.125+1.96*sqrt(vp25))
ci50=c(8093.800-1.96*sqrt(vp50),8093.800+1.96*sqrt(vp50))
ci75=c(8147.438-1.96*sqrt(vp75),8147.438+1.96*sqrt(vp75))
ci25
ci50
ci75
####ci for diff between 2 1st quartiles####
quantile(x)
quantile(y)
v25
vp25
ci_diff_25=c((7817.550-8034.125)-1.96*sqrt(v25+vp25),(7817.550-8034.125)+1.96*sqrt(v25+vp25))
ci_diff_25

v50
vp50
ci_diff_50=c((7973.225-8093.800)-1.96*sqrt(v50+vp50),(7973.225-8093.800)+1.96*sqrt(v50+vp50))
ci_diff_50

v75
vp75
ci_diff_75=c((8149.975-8147.438)-1.96*sqrt(v75+vp75),(8149.975-8147.438)+1.96*sqrt(v75+vp75))
ci_diff_75

#identity matrix for bootstrap sample
t=diag(x=1,nrow=1000,ncol=1000)

#feb
set.seed(1001)
m=bs[["t"]]
Combined=c(m[,2],m[,3],m[,4])
q_n=matrix(data = Combined,ncol=3,byrow = FALSE)
q_n
#transpose

qt_n=t(q_n)
qt_n
dim(qt_n)

IB=diag(x=1,nrow = 1000,ncol = 1000)
IB

B=1000

I=rep(1,1000)
E=matrix(I, nrow = 1000, ncol = 1)
E1=matrix(I, nrow = 1, ncol = 1000)

E2=E%*%E1
E2

E3=E2/1000
E3

#INSIDE BRACKET
E4=IB-E3

E5=qt_n%*%E4%*%q_n
E5/999
V=E5/999
V

#oct
set.seed(1001)
n=ps[["t"]]
Combined=c(n[,2],n[,3],n[,4])
p_n=matrix(data=Combined,ncol=3,byrow = FALSE)
p_n
dim(p_n)
#transpose
pt_n=t(p_n)
pt_n
dim(pt_n)

#check!!
pt_n1=matrix(data = pt_n,ncol = 3,byrow = FALSE)
pt_n1
dim(pt_n1)

IB=diag(x=1,nrow = 1000,ncol = 1000)
IB

B=1000

I=rep(1,1000)
P=matrix(I, nrow = 1000, ncol = 1)
P1=matrix(I, nrow = 1, ncol = 1000)

P2=P%*%P1
P2

P3=P2/1000
P3

#INSIDE BRACKET
P4=IB-P3

P5=pt_n%*%P4%*%p_n
P5/999
VP=P5/999
VP

#
B=rep(0,9)
Q=matrix(data=B,ncol=3)
Q

C1=cbind(V,Q)
C2=cbind(Q,VP)
M=rbind(C1,C2)
M
class(M)
a1=c(1,0,0,-1,0,0)
a2=c(0,1,0,0,-1,0)
a3=c(0,0,1,0,0,-1)
A=matrix(data=c(a1,a2,a3),nrow=3,byrow =TRUE)
A

xq=quantile(x)
yq=quantile(y)
xq
yq
q_1=matrix(data=c( 7817.550, 7973.225, 8149.97,8034.125 ,8093.800, 8147.438),nrow=1)
q_1
#transpose
A1=t(A)
A1
q=t(q_1)
q
# wald test
N=A%*%M%*%A1 
N

library(matlib)
N1=inv(N)


N1
N%*%N1

W=q_1%*%A1%*%N1%*%A%*%q
W

q=t(q_1)
q

am1=matrix(a1,nrow = 1)
am2=matrix(a2,nrow = 1)
am3=matrix(a3,nrow = 1)
at1=t(am1)
at2=t(am2)
at3=t(am3)

ciaq25=c(am1%*%q-1.96*(am1%*%M%*%at1)^(1/2),am1%*%q+1.96*(am1%*%M%*%at1)^(1/2))
ciaq50=c(am2%*%q-1.96*(am2%*%M%*%at2)^1/2,am2%*%q+1.96*(am2%*%M%*%at2)^1/2)
ciaq75=c(am3%*%q-1.96*(am3%*%M%*%at3)^1/2,am3%*%q+1.96*(am3%*%M%*%at3)^1/2)
