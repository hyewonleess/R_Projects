# Report #2 codes

# 3. Understand and explore the data
data=read.table('survey_33.dat',header=T, sep='\t')
head(data)
dim(data)

# check missing values
sum(is.na(data)) # ->the result of this code is 0, but there were missing values.

# check 'Guardian' distribution
table(data$Guardian)
barplot(table(data$Guardian),col=c('green'),main='Guardians',
        names=c('Father','Mother','Others'),ylim=c(0,50))

# check 'Gender' distribution
table(data$Gender)
barplot(table(data$Gender),col=c('green'),main='Gender',
        names=c('Female','Male'),ylim=c(0,40))

# check answer types in each group of questions(positive, negative, vague)
# important note: when I exerted this code, I could find missing values.
positive=c('Q1','Q3','Q5','Q6','Q7','Q11','Q12','Q15','Q17','Q21','Q22','Q25')
for (i in positive) {print(table(data[i]))}

negative=c('Q2','Q4','Q8','Q9','Q10','Q13','Q14','Q16','Q18','Q19','Q20','Q23','Q24')
for (i in negative) {print(table(data[i]))}


# drop rows that contains missing values
data2=data[-c(6,35), ] 

# 4. Conduct factor analysis
# make correlation matrix

# replace values to points(0~3)
data2[positive][data2[positive]=='Very like']<-3
data2[positive][data2[positive]=='Moderately like']<-2
data2[positive][data2[positive]=='Moderately unlike']<-1
data2[positive][data2[positive]=='Very unlike']<-0

data2[negative][data2[negative]=='Very like']<-0
data2[negative][data2[negative]=='Moderately like']<-1
data2[negative][data2[negative]=='Moderately unlike']<-2
data2[negative][data2[negative]=='Very unlike']<-3

# create correlation matrix
data2[, -c(1,2,3)] <- sapply(data2[, -c(1,2,3)], as.numeric)
cor=cor(data2[,-c(1,2,3)])

# Method 1: principal factor method
library(psych)
cor.pca=princomp(covmat=cor,cor=T)
summary(cor.pca)
cor.pca$sdev^2
screeplot(cor.pca, npcs=6,type="l")

fac.pf1 = fa(cor, nfactors=2, fm="pa", rotate="none")
fac.pf1

fac.pf2 = fa(cor, nfactors=2, fm="pa", rotate="varimax")
fac.pf2

# plot factor loadings
fa.plot(fac.pf1,xlim=c(-1,1), ylim=c(-1,1),)
fa.plot(fac.pf2,xlim=c(-1,1), ylim=c(-1,1),)

# Method 2: maximum likelihood
fac.ml1 = fa(cor, nfactors=1, n.obs=50, fm="ml", rotate="varimax")
fac.ml1

fac.ml2= fa(cor, nfactors=2, n.obs=50, fm="ml", rotate="varimax")
fac.ml2

fa.plot(fac.ml1,xlim=c(-1,1), ylim=c(-1,1),)
fa.plot(fac.ml2,xlim=c(-1,1), ylim=c(-1,1),)

# 5. comparison between principal component analysis
cor.pca=princomp(covmat=cor,cor=T)
summary(cor.pca)
cor.pca$sdev^2
screeplot(cor.pca, npcs=6,type="l")
cor.pca$loadings

# 6. Gender and Guardians
# Gender (Female/Male)
female=data2[data2$Gender=='Female',]
male=data2[data2$Gender=='Male',]
dim(female)
dim(male)
cor_fm=cor(female[,-c(1,2,3)])
cor_m=cor(male[,-c(1,2,3)])

# female
cor_fm.pca=princomp(covmat=cor_fm,cor=T)
cor_fm.pca$sdev^2
screeplot(cor_fm.pca,npcs=6,type='l')
fac.fm=fa(cor_fm,nfactors=2,fm='pa',rotate='varimax')
fac.fm
fa.plot(fac.fm,xlim=c(-1,1), ylim=c(-1,1),)

# male
cor_m.pca=princomp(covmat=cor_m,cor=T)
cor_m.pca$sdev^2
screeplot(cor_m.pca,npcs=6,type='l')
fac.m=fa(cor_m,nfactors=2,fm='pa',rotate='varimax')
fac.m
fa.plot(fac.m,xlim=c(-1,1), ylim=c(-1,1),)

# Guardians
father=data2[data2$Guardian=='Father',]
mother=data2[data2$Guardian=='Mother',]
dim(father)
dim(mother)
cor_fa=cor(father[,-c(1,2,3)])
cor_mo=cor(mother[,-c(1,2,3)])

# father
cor_fa.pca=princomp(covmat=cor_fa,cor=T)
cor_fa.pca$sdev^2
screeplot(cor_fa.pca,npcs=6,type='l')
fac.fa=fa(cor_fa,nfactors=3,fm='pa',rotate='varimax') # -> There was an error

# pca
cor_fa.pca$loadings

# mother
cor_mo.pca=princomp(covmat=cor_mo,cor=T)
cor_mo.pca$sdev^2
screeplot(cor_mo.pca,npcs=6,type='l')
fac.mo=fa(cor_mo,nfactors=2,fm='pa',rotate='varimax')
fac.mo
fa.plot(fac.mo,xlim=c(-1,1), ylim=c(-1,1),)
