###############################
#ROR of 20 mg vs 20-40-80 mg###
###############################

##OR of 20 mg by Drug
MyData = read_excel("20(vs20-40-80).xlsx",  na = "NA")
MyData = as.data.frame(MyData)


###OR of response
MyDataregression = MyData[!is.na(MyData$Study_year) & !is.na(MyData$Respond_e),]
MyDataregression = subset(MyDataregression, Drug!=c("mirtazapine") & Drug!=c("venlafaxine"))
MyDataregression$Study_year = (MyDataregression$Study_year-2000)

ORFixed20regression = metabin(Respond_e, N_rand_e, Respond_c, N_rand_c, data=MyDataregression,
                              studlab=StudyID,
                              comb.fixed=F,
                              comb.rand=T,
                              method.tau="REML",
                              hakn=T,
                              incr=0.1,
                              sm="OR",
                              byvar=Drug)

MyMetareg=metareg(ORFixed20regression,Study_year+Drug)
LnORFixed2000=c(MyMetareg$beta[1], MyMetareg$beta[1]+MyMetareg$beta[3:6])
names(LnORFixed2000)<-unique(MyDataregression$Drug)
LnORFixed2000
#calculate the SE for the combinations of coefficients
vcov<-MyMetareg$vb
seLnORFixed2000<-sqrt(c(vcov[1,1],vcov[1,1]+diag(vcov[3:6,3:6])+2*vcov[1,3:6]))
names(seLnORFixed2000)<-unique(MyDataregression$Drug)
seLnORFixed2000




##OR of 20-40-80 mg by Drug
MyData2 = read_excel("20-40-80.xlsx",  na = "NA")
MyData2 = as.data.frame(MyData2)


###OR of response
MyData2regression = MyData2[!is.na(MyData2$Study_year) & !is.na(MyData2$Respond_e),]
MyData2regression = subset(MyData2regression, Drug!=c("mirtazapine") & Drug!=c("venlafaxine"))
MyData2regression$Study_year = (MyData2regression$Study_year-2000)

ORFlexible20_80regression = metabin(Respond_e, N_rand_e, Respond_c, N_rand_c, data=MyData2regression,
                                    studlab=StudyID,
                                    comb.fixed=F,
                                    comb.rand=T,
                                    method.tau="REML",
                                    hakn=T,
                                    incr=0.1,
                                    sm="OR",
                                    byvar=Drug)

MyMetareg2=metareg(ORFlexible20_80regression,Study_year+Drug)
LnORFlexible2000=c(MyMetareg2$beta[1], MyMetareg2$beta[1]+MyMetareg2$beta[3:6])
names(LnORFlexible2000)<-unique(MyData2regression$Drug)
LnORFlexible2000
#calculate the SE for the combinations of coefficients
vcov<-MyMetareg2$vb
seLnORFlexible2000<-sqrt(c(vcov[1,1],vcov[1,1]+diag(vcov[3:6,3:6])+2*vcov[1,3:6]))
names(seLnORFlexible2000)<-unique(MyData2regression$Drug)
seLnORFlexible2000




##Calculate ROR
LnROR2000 = LnORFlexible2000 - LnORFixed2000
seLnROR2000 = sqrt(seLnORFixed2000^2 + seLnORFlexible2000^2)


pooledROR2000=metagen(LnROR2000,seLnROR2000,
                      studlab=names(LnROR2000),
                      comb.fixed=F,
                      comb.rand=T,
                      method.tau="REML",
                      hakn=T,
                      prediction=T,
                      sm="OR")
pooledROR2000
forest(pooledROR2000,
       xlim=c(0.5,2.0),
       leftcols=c("studlab"),
       leftlabs=c("Drug"),
       rightlabs=c("ROR","95%CI","weight"),
       smlab="ROR",
       print.i2=T, print.i2.ci=T,
       label.right="Favours flexible dose", label.left="Favours fixed low dose",
       prediction=F,
       fs.lr=9,
       col.square="blue",col.diamond="red",
       colgap.forest="1.5cm",
       fs.axis=9)



