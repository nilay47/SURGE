#for GINI
g1=ginif[3:267,52:57] 
g2=ginif[3:267,1]
giniold=cbind(g2,g1)

#for GDP
G1=gdpf[3:267,52:57]
G2=gdpf[3:267,1]
gdpold=cbind(G2,G1)

#for UNEMPLOYMENT
u1=unempf[3:267,52:57] 
u2=unempf[3:267,1]
unempld=cbind(u2,u1)

#for GROSS SAVING RATE
gs1=gsavsf[3:267,52:57] 
gs2=gsavsf[3:267,1]
gsavsold=cbind(gs2,gs1)

#for FERTILITY
f1=ferf[3:267,52:57] 
f2=ferf[3:267,1]
ferold=cbind(f2,f1)

#for gdp of 74 countries
k=2
gdpnew=gdpold[1:75,]
a=c(9,10,14,25,17,32,28,24,29,21,47,45,42,35,45,48,53,54,58,59,66,211,71,75,77,82,55,89,85,97,101,114,111,115,116,120,122,129,145,131,143,144,169,150,163,162,148,176,173,177,184,185,195,186,190,194,201,202,214,221,222,70,223,37,234,246,233,237,244,248,81,251,250,196)
for(i in a){
  gdpnew[k,]=gdpold[i,]
  k=k+1
}

#for gdpchange
gdp2012=as.numeric(gdpnew[2:75,6])
gdp2007=as.numeric(gdpnew[2:75,2])
gdpchange=(gdp2012-gdp2007)/gdp2007*100

#for fer2007 of 74 countries
k=2
fernew=ferold[1:75,]
a=c(9,10,14,25,17,32,28,24,29,21,47,45,42,35,45,48,53,54,58,59,66,211,71,75,77,82,55,89,85,97,101,114,111,115,116,120,122,129,145,131,143,144,169,150,163,162,148,176,173,177,184,185,195,186,190,194,201,202,214,221,222,70,223,37,234,246,233,237,244,248,81,251,250,196)
for(i in a){
  fernew[k,]=ferold[i,]
  k=k+1
}
fer2007=as.numeric(fernew[2:75,2])

#for gini2007 of 74 countries
k=2
gininew=giniold[1:75,]
a=c(9,10,14,25,17,32,28,24,29,21,47,45,42,35,45,48,53,54,58,59,66,211,71,75,77,82,55,89,85,97,101,114,111,115,116,120,122,129,145,131,143,144,169,150,163,162,148,176,173,177,184,185,195,186,190,194,201,202,214,221,222,70,223,37,234,246,233,237,244,248,81,251,250,196)
for(i in a){
  gininew[k,]=giniold[i,]
  k=k+1
}

#since gini has some unfilled columns, analysis for corrected columns
gini=gininew[2:75,2:6]
gini1=as.numeric(gini[,1])
gini2=as.numeric(gini[,2])
gini3=as.numeric(gini[,3])
gini4=as.numeric(gini[,4])
gini5=as.numeric(gini[,5])
gini=cbind(gini1,gini2)
gini=cbind(gini,gini3)
gini=cbind(gini,gini4)
gini=cbind(gini,gini5)

for(i in 1:ncol(gini)){
  gini[,i]=ifelse(is.na(gini[,i]),
                  ave(gini[,i],FUN=function(y) mean(y, na.rm = TRUE)),
                  gini[,i])
}
gini2007=as.numeric(gini[,1])


#for gsavs2007 of 74 countries
k=2
gsavsnew=gsavsold[1:75,]
a=c(9,10,14,25,17,32,28,24,29,21,47,45,42,35,45,48,53,54,58,59,66,211,71,75,77,82,55,89,85,97,101,114,111,115,116,120,122,129,145,131,143,144,169,150,163,162,148,176,173,177,184,185,195,186,190,194,201,202,214,221,222,70,223,37,234,246,233,237,244,248,81,251,250,196)
for(i in a){
  gsavsnew[k,]=gsavsold[i,]
  k=k+1
}
gsavs2007=as.numeric(gsavsnew[2:75,2])
gsavs2007[29]=20

#for unemp2007 of 74 countries
k=2
unempnew=unempold[1:75,]
a=c(9,10,14,25,17,32,28,24,29,21,47,45,42,35,45,48,53,54,58,59,66,211,71,75,77,82,55,89,85,97,101,114,111,115,116,120,122,129,145,131,143,144,169,150,163,162,148,176,173,177,184,185,195,186,190,194,201,202,214,221,222,70,223,37,234,246,233,237,244,248,81,251,250,196)
for(i in a){
  unempnew[k,]=unempold[i,]
  k=k+1
}
unemp2007=as.numeric(unempnew[2:75,2])

#for defining dev
dev=gdpchange
for(i in 1:75){
  if(gdp2007[i]>=12000){
    dev[i]=1
  }else{
    dev[i]=0
  }
}

#for model1(ONLY GINI2007)
model1=lm(gdpchange~gini2007)
summary1=summary(model1)
print(model1)

#for model2(ONLY GINI2007, GSAVS2007)
model2=lm(gdpchange~gini2007+gsavs2007)
summary2=summary(model2)
print(model2)

#for model3(ONLY GINI2007, GSAVS2007, UNEMP2007)
model3=lm(gdpchange~gini2007+gsavs2007+unemp2007)
summary3=summary(model3)
print(model3)

#for model4(ONLY GINI2007, GSAVS2007, UNEMP2007, FER2007)
model4=lm(gdpchange~gini2007+gsavs2007+unemp2007+fer2007)
summary4=summary(model4)
print(model4)

#for model5(ONLY GINI2007, GSAVS2007, FER2007)
model5=lm(gdpchange~gini2007+gsavs2007+fer2007)
summary5=summary(model5)
print(model5)

#for model6(ONLY GINI2007, GSAVS2007, FER2007, DEV)
model6=lm(gdpchange~gini2007+gsavs2007+fer2007+dev)
summary6=summary(model6)
print(model6)


X=cbind(gini2007,unemp2007,fer2007,gsavs2007)
round(cor(X),2)
model=lm(gsavs2007~gini2007+unemp2007+fer2007)


install.packages("car")
library(car)
vif(model)

condition_number=kappa(X)

install.packages("ridge")
library(ridge)
model_ridge=linearRidge(gdpchange~gini2007+gsavs2007+unemp2007+fer2007+dev)


#for tidy tables for all models
tidy1=tidy(model1)
tidy2=tidy(model2)
tidy3=tidy(model3)
tidy4=tidy(model4)
tidy5=tidy(model5)
tidy6=tidy(model6)

#plot of residuals for models
par(mfrow=c(3,2))
plot(density(resid(model1)))
plot(density(resid(model2)))
plot(density(resid(model3)))
plot(density(resid(model4)))
plot(density(resid(model5)))
plot(density(resid(model6)))


 
