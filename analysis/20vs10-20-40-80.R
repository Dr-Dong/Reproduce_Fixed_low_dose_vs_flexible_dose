###############################
#ROR of 20 mg vs 10-20-40-80 mg###
###############################

##OR of 20 mg by Drug
MyData = read_excel("20(vs10-20-40-80).xlsx",  na = "NA")
MyData = as.data.frame(MyData)

###OR of response
ORFixed20 = metabin(Respond_e, N_rand_e, Respond_c, N_rand_c, data=MyData,
             studlab=StudyID,
             comb.fixed=F,
             comb.rand=T,
             method.tau="REML",
             hakn=T,
             incr=0.1,
             sm="OR",
             byvar=Drug)
ORFixed20$TE.random.w


LnORFixed = ORFixed20$TE.random.w
seLnORFixed = ORFixed20$seTE.random.w
names(LnORFixed) = unique(ORFixed20$byvar)
names(seLnORFixed) = unique(ORFixed20$byvar)
#create SSRI, MIR, VEN
LnORFixedSSRI = LnORFixed[c("citalopram","escitalopram","fluoxetine","paroxetine","sertraline")]
seLnORFixedSSRI = seLnORFixed[c("citalopram","escitalopram","fluoxetine","paroxetine","sertraline")]
LnORFixedMIR = LnORFixed[c("mirtazapine")]
seLnORFixedMIR = seLnORFixed[c("mirtazapine")]
LnORFixedVEN = LnORFixed[c("venlafaxine")]
seLnORFixedVEN = seLnORFixed[c("venlafaxine")]
LnORFixedSSRI
LnORFixedMIR
LnORFixedVEN

##OR of 10-20-40-80 mg by Drug
MyData2 = read_excel("10-20-40-80.xlsx",  na = "NA")
MyData2 = as.data.frame(MyData2)

###OR of response
ORFlexible20_80 = metabin(Respond_e, N_rand_e, Respond_c, N_rand_c, data=MyData2,
              studlab=StudyID,
              comb.fixed=F,
              comb.rand=T,
              method.tau="REML",
              hakn=T,
              incr=0.1,
              sm="OR",
              byvar=Drug)
ORFlexible20_80

LnORFlexible = ORFlexible20_80$TE.random.w
seLnORFlexible = ORFlexible20_80$seTE.random.w
names(LnORFlexible) = unique(ORFlexible20_80$byvar)
names(seLnORFlexible) = unique(ORFlexible20_80$byvar)
#create SSRIs, MIR, VEN
LnORFlexibleSSRI = LnORFlexible[c("citalopram","escitalopram","fluoxetine","paroxetine","sertraline")]
seLnORFlexibleSSRI = seLnORFlexible[c("citalopram","escitalopram","fluoxetine","paroxetine","sertraline")]
LnORFlexibleMIR = LnORFlexible[c("mirtazapine")]
seLnORFlexibleMIR = seLnORFlexible[c("mirtazapine")]
LnORFlexibleVEN = LnORFlexible[c("venlafaxine")]
seLnORFlexibleVEN = seLnORFlexible[c("venlafaxine")]
LnORFlexibleSSRI
LnORFlexibleMIR
LnORFlexibleVEN


##Calculate ROR
LnRORSSRI = LnORFlexibleSSRI - LnORFixedSSRI
seLnRORSSRI = sqrt(seLnORFixedSSRI^2 + seLnORFlexibleSSRI^2)
LnRORMIR = LnORFlexibleMIR - LnORFixedMIR
seLnRORMIR = sqrt(seLnORFixedMIR^2 + seLnORFlexibleMIR^2)
LnRORVEN = LnORFlexibleVEN - LnORFixedVEN
seLnRORVEN = sqrt(seLnORFixedVEN^2 + seLnORFlexibleVEN^2)

pooledRORSSRI=metagen(LnRORSSRI,seLnRORSSRI,
            studlab=names(LnRORSSRI),
            comb.fixed=F,
            comb.rand=T,
            method.tau="REML",
            hakn=T,
            prediction=T,
            sm="OR")
pooledRORSSRI
forest(pooledRORSSRI,
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
       col.by="black")

###mirtazapine
pooledRORMIR=metagen(LnRORMIR,seLnRORMIR,
                      studlab=names(LnRORMIR),
                      comb.fixed=F,
                      comb.rand=T,
                      method.tau="REML",
                      hakn=T,
                      prediction=T,
                      sm="OR")
pooledRORMIR
forest(pooledRORMIR,
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
       col.by="black")

###venlafaxine
if(F){
pooledRORVEN=metagen(LnRORVEN,seLnRORVEN,
                     studlab=names(LnRORVEN),
                     comb.fixed=F,
                     comb.rand=T,
                     method.tau="REML",
                     hakn=T,
                     prediction=T,
                     sm="OR")
pooledRORVEN
forest(pooledRORVEN,
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
       col.by="black")
}

#drawing a forest plot of ORs of individual fixed and flexible dose studies
forest(ORFixed20,
       xlim=c(0.5,2.0),
       leftcols=c("studlab"),
       leftlabs=c("Drug"),
       rightlabs=c("OR","95%CI","weight"),
       smlab="OR",
       print.i2=T, print.i2.ci=T,
       label.right="Favours drug", label.left="Favours placebo",
       prediction=F,
       fs.lr=9,
       col.square="blue",col.diamond="red",
       colgap.forest="1.5cm",
       overall=F,
       sortvar=Drug,
       fs.axis=9,
       col.by="black")

forest(ORFlexible20_80,
       xlim=c(0.5,2.0),
       leftcols=c("studlab"),
       leftlabs=c("Drug"),
       rightlabs=c("OR","95%CI","weight"),
       smlab="OR",
       print.i2=T, print.i2.ci=T,
       label.right="Favours drug", label.left="Favours placebo",
       prediction=F,
       fs.lr=9,
       col.square="blue",col.diamond="red",
       colgap.forest="1.5cm",
       overall=F,
       sortvar=Drug,
       fs.axis=9,
       col.by="black")


#drawing a forest plot of ORs of fixed dose studies for three subgroups together
LnORFixedSSRI
LnORFixedVEN
LnORFixedMIR

LnORFixed = c(LnORFixedSSRI, LnORFixedVEN, LnORFixedMIR)
seLnORFixed = c(seLnORFixedSSRI, seLnORFixedVEN, seLnORFixedMIR)
DataframeORFixed = cbind.data.frame(Class=c("SSRI", "SSRI", "SSRI", "SSRI", "SSRI", "VEN", "MIR"), 
                                Drug=c("citalopram", "escitalopram", "fluoxetine", "paroxetine", "sertraline", "venlafaxine", "mirtazapine"),
                                LnORFixed=LnORFixed,seLnORFixed=seLnORFixed)
DataframeORFixed
pooledORFixed=metagen(LnORFixed,seLnORFixed,data=DataframeORFixed,
                  studlab=Drug,
                  comb.fixed=F,
                  comb.rand=T,
                  method.tau="REML",
                  hakn=T,
                  prediction=T,
                  sm="OR",
                  byvar=Class)
pooledORFixed
forest(pooledORFixed,
       xlim=c(0.5,2.0),
       leftcols=c("studlab"),
       leftlabs=c("Drug"),
       rightlabs=c("OR","95%CI","weight"),
       smlab="OR",
       print.i2=T, print.i2.ci=T,
       label.right="Favours drug", label.left="Favours placebo",
       prediction=F,
       fs.lr=9,
       col.square="blue",col.diamond="red",
       colgap.forest="1.5cm",
       overall=F,
       sortvar=c("citalopram", "escitalopram", "fluoxetine", "paroxetine", "sertraline", "venlafaxine", "mirtazapine"),
       fs.axis=9,
       col.by="black")



#drawing a forest plot of ORs of flexible dose studies for three subgroups together
LnORFlexibleSSRI
LnORFlexibleVEN
LnORFlexibleMIR

LnORFlexible = c(LnORFlexibleSSRI, LnORFlexibleVEN, LnORFlexibleMIR)
seLnORFlexible = c(seLnORFlexibleSSRI, seLnORFlexibleVEN, seLnORFlexibleMIR)
DataframeORFlexible = cbind.data.frame(Class=c("SSRI", "SSRI", "SSRI", "SSRI", "SSRI", "VEN", "MIR"), 
                                    Drug=c("citalopram", "escitalopram", "fluoxetine", "paroxetine", "sertraline", "venlafaxine", "mirtazapine"),
                                    LnORFlexible=LnORFlexible,seLnORFlexible=seLnORFlexible)
DataframeORFlexible
pooledORFlexible=metagen(LnORFlexible,seLnORFlexible,data=DataframeORFlexible,
                      studlab=Drug,
                      comb.fixed=F,
                      comb.rand=T,
                      method.tau="REML",
                      hakn=T,
                      prediction=T,
                      sm="OR",
                      byvar=Class)
pooledORFlexible
forest(pooledORFlexible,
       xlim=c(0.5,2.0),
       leftcols=c("studlab"),
       leftlabs=c("Drug"),
       rightlabs=c("OR","95%CI","weight"),
       smlab="OR",
       print.i2=T, print.i2.ci=T,
       label.right="Favours drug", label.left="Favours placebo",
       prediction=F,
       fs.lr=9,
       col.square="blue",col.diamond="red",
       colgap.forest="1.5cm",
       overall=F,
       sortvar=c("citalopram", "escitalopram", "fluoxetine", "paroxetine", "sertraline", "venlafaxine", "mirtazapine"),
       fs.axis=9,
       col.by="black")


#drawing a forest plot of RORs with three subgroups together
LnRORSSRI
LnRORVEN
LnRORMIR

LnROR = c(LnRORSSRI, LnRORVEN, LnRORMIR)
seLnROR = c(seLnRORSSRI, seLnRORVEN, seLnRORMIR)
DataframeROR = cbind.data.frame(Class=c("SSRI", "SSRI", "SSRI", "SSRI", "SSRI", "VEN", "MIR"), 
                                Drug=c("citalopram", "escitalopram", "fluoxetine", "paroxetine", "sertraline", "venlafaxine", "mirtazapine"),
                                LnROR=LnROR,seLnROR=seLnROR)
DataframeROR
pooledROR=metagen(LnROR,seLnROR,data=DataframeROR,
             studlab=Drug,
             comb.fixed=F,
             comb.rand=T,
             method.tau="REML",
             hakn=T,
             prediction=T,
             sm="OR",
             byvar=Class)
pooledROR
forest(pooledROR,
       xlim=c(0.5,2.0),
       leftcols=c("studlab"),
       leftlabs=c("Drug"),
       rightlabs=c("ROR","95%CI"),
       smlab="ROR",
       print.i2=T, print.i2.ci=T,
       label.right="Favours flexible dose",
       label.left="Favours fixed low dose",
       prediction=F,
       fs.lr=9,
       col.square="blue",col.diamond="red",
       colgap.forest="1.5cm",
       overall=F,
       sortvar=c("citalopram", "escitalopram", "fluoxetine", "paroxetine", "sertraline", "venlafaxine", "mirtazapine"),
       fs.axis=9,
       col.by="black")





