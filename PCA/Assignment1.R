# load data
data=read.table('labor21_33.dat',header=T)
head(data)

# 3. Exploring data with graphs and checking assumptions

# (1) histogram
win.graph()
par(mfrow=c(3,4))

# cols: column index of 11 variables
cols=c(3:12,16)

# (1) histogram
for (i in cols){hist(data[,i],main="Histogram of variables",xlab=colnames(data)[i])}

# (2) qq-plot
win.graph()
par(mfrow=c(3,4))
for(i in cols){
  qqnorm(data[,i],xlab="Quantiles of Standard Normal",ylab=colnames(data)[i]);
  qqline(data[,i])}

# (3) probability plotting technique-chi-square plot

# only include 11 variables of living expenses
data.2=data[cols]

# get covariance matrix of data.2
cov = cov(data.2)
di2 = mahalanobis(data.2, colMeans(data.2), cov)

# Draw the chi-square probability plot
library(lattice)
p<-(c(1:nrow(data.2))-0.5)/nrow(data.2)
plot(x=sort(di2),y=qchisq(p,df=ncol(data.2)),main="Chi-Square plot of data",
     xlab="Ordered Distance",ylab="Chi-Square Quantile with df 11")
abline(a=0,b=1)

# (4) Explore other remaining variables

# column index of remaining variables
cols.2=c(2,13,14,15)
win.graph()
par(mfrow=c(1,4))

# draw histogram
for (i in cols.2){hist(data[,i],main="Histogram",xlab=colnames(data)[i])}

# correlation between remaining variables vs 11 living expense variables
cor(data$gender,data.2)
cor(data$age,data.2)
cor(data$h212102,data.2)
cor(data$h212402,data.2)

# 4~6. PCA with 11 variables

# check variances of each variables
diag(var(data.2))

# conduct PCA
pca= princomp(covmat = cov, cor = T)
pca$loadings

# get lambdas
pca$sdev^2

# proportion of variables
summary(pca)

# scree diagram
screeplot(pca,type='l')

# draw scatterplot matrix
pairs(as.matrix(data.2) %*% pca$loadings[,c(1,2)])

# 7.Is PCA useful in data?
pairs(data.2)

