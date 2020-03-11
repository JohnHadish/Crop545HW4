myGD=read.table(file="http://zzlab.net/GAPIT/data/mdp_numeric.txt",head=T)
myGM=read.table(file="http://zzlab.net/GAPIT/data/mdp_SNP_information.txt",head=T)
source("http://zzlab.net/StaGen/2020/R/G2P.R")
source("http://zzlab.net/StaGen/2020/R/GWASbyCor.R")
X=myGD[,-1] # Remove the first column
index1to5=myGM[,2]<6 # Find out fiirst 5 chromosome genes
X1to5 = X[,index1to5] # Get those genes
set.seed(99164)
mySim=G2P(X= X1to5,h2=.75,alpha=1,NQTN=10,distribution="norm") # Pick 10 genes to make significant out of first 5 chromosomes. Then Simulate the phenotypes for each of these.
p= GWASbyCor(X=X,y=mySim$y) # Perform the GWAS with the simulated data. p is simulated GWAS results


#False Positives Manhatten Plot
color.vector <- rep(c("deepskyblue","orange","forestgreen","indianred3"),10) # Color
m=nrow(myGM) # nrow of OG myGM
plot(t(-log10(p))~seq(1:m),col=color.vector[myGM[,2]]) # plot the OG data with the phenotype generated from the first 5. seq is used for locations
abline(v=mySim$QTN.position, lty = 2, lwd=2, col = "black")


# QQ plot
p.obs=p[!index1to5] # remove simulated data from 1 to 5
m2=length(p.obs) # mind length of remaining (6 onwards) for plotting purposes
p.uni=runif(m2,0,1) # r uniform distribution from 0 to 1 for the number of samples not used in mySIM
order.obs=order(p.obs) # Obsevred data
order.uni=order(p.uni) # Expected data (uniform distribution)

plot(-log10(p.uni[order.uni]),-log10(p.obs[order.obs])) # plot expected on y, observed on x
abline(a = 0, b = 1, col = "red")


# GOAL OF GLM
# Minimize the line to go to x = y

# Phenotypes by Genotypes
order.obs=order(p.obs) # repeat
X6to10=X[,!index1to5] # OG genotype ratings for chromosomes 6:10
Xtop=X6to10[,order.obs[1]] # top SNP of each observation I AM NOT SURE WHY WE DO THIS!!!!

boxplot(mySim$y~Xtop) # simulated phenotype (y axis) genotype x axis

# Correlation
PCA=prcomp(X) # Principle component analysis of origional genotypes


plot(mySim$y,PCA$x[,2]) # plot simulated phenotype on the x axis, PCA 2 of genotypes on the y
plot(mySim$y,PCA$x[,1]) # PCA 1 vs simulated phenotype
plot(PCA$x[,1],PCA$x[,2]) # PCA 1 vs 2
cor(mySim$y,PCA$x[,2]) # determine correlation between the two

# Linear Regression
set.seed(99164)
s=sample(length(mySim$y),10) # take a sample of 10 of the phenotypes
plot(mySim$y[s],PCA$x[s,2]) # plot 10 phenotype samples with corresponding genotype samples
cor(mySim$y[s],PCA$x[s,2]) # determine correlation of these samples
# y = a + cx + e
# phenotype equals genotype plus principle component( of ghenotype) plus environment

# Example from the ten individuals
cbind(mySim$y[s],1, PCA$x[s,2],Xtop[s]) # For ten samples, this shows the "Simulated phenotype, 1, principle component 2, and genotype)

# Action in R
y=mySim$y # rename simulated phenotype data
X=cbind(1, PCA$x[,2],Xtop) # Bind mean = 1, PCA2 and first genotype
LHS=t(X)%*%X # Matrix multiplication of x by itself
C=solve(LHS) # Inverse of LHS
RHS=t(X)%*%y # X by y
b=C%*%RHS
yb=X%*%b # Phenotype
e=y-yb # I think this is phenotype with one snp and pca2
n=length(y)
ve=sum(e^2)/(n-1)
vt=C*ve
t=b/sqrt(diag(vt))
p=2*(1-pt(abs(t),n-2))

# Phenotypes by genotype for the first SNP
LM=cbind(b, t, sqrt(diag(vt)), p)
rownames(LM)=cbind("Mean", "PC2","Xtop")
colnames(LM)=cbind("b", "t", "SD","p")
LM

# Loop through genome
G=myGD[,-1]
n=nrow(G) # all samples
m=ncol(G) # All genes
P=matrix(NA,1,m)
for (i in 1:m){ # For each gene, calculate new
  x=G[,i]
  if(max(x)==min(x)){
    p=1}else{
      X=cbind(1, PCA$x[,2],x)
      LHS=t(X)%*%X
      C=solve(LHS)
      RHS=t(X)%*%y
      b=C%*%RHS
      yb=X%*%b
      e=y-yb
      n=length(y)
      ve=sum(e^2)/(n-1)
      vt=C*ve
      t=b/sqrt(diag(vt))
      p=2*(1-pt(abs(t),n-2))
    } #end of testing variation
  P[i]=p[length(p)]
} #end of looping for markers

# QQ Plot

p.obs=P[!index1to5]
m2=length(p.obs)
p.uni=runif(m2,0,1)
order.obs=order(p.obs)
order.uni=order(p.uni)

plot(-log10(p.uni[order.uni]),
     -log10(p.obs[order.obs]), ylim=c(0,7))
abline(a = 0, b = 1, col = "red")

# Using three PCs Loop through the genome
G=myGD[,-1]
n=nrow(G)
m=ncol(G)
P=matrix(NA,1,m)
for (i in 1:m){
  x=G[,i]
  if(max(x)==min(x)){
    p=1}else{
      X=cbind(1, PCA$x[,1:3],x)
      LHS=t(X)%*%X
      C=solve(LHS)
      RHS=t(X)%*%y
      b=C%*%RHS
      yb=X%*%b
      e=y-yb
      n=length(y)
      ve=sum(e^2)/(n-1)
      vt=C*ve
      t=b/sqrt(diag(vt))
      p=2*(1-pt(abs(t),n-2))
    } #end of testing variation
  P[i]=p[length(p)]
} #end of looping for markers

# QQ plots
p.obs=P[!index1to5]
m2=length(p.obs)
p.uni=runif(m2,0,1)
order.obs=order(p.obs)
order.uni=order(p.uni)

plot(-log10(p.uni[order.uni]),
     -log10(p.obs[order.obs]), ylim=c(0,7))
abline(a = 0, b = 1, col = "red")

# QQ Plots
p.obs=P
m2=length(p.obs)
p.uni=runif(m2,0,1)
order.obs=order(p.obs)
order.uni=order(p.uni)

plot(-log10(p.uni[order.uni]),
     -log10(p.obs[order.obs]), )
abline(a = 0, b = 1, col = "red")


# Manhatten Plots
color.vector <- rep(c("deepskyblue","orange","forestgreen","indianred3"),10)
m=nrow(myGM)
plot(t(-log10(P))~seq(1:m),col=color.vector[myGM[,2]])
abline(v=mySim$QTN.position, lty = 2, lwd=2, col = "black")


