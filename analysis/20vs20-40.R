###############################
#ROR of 20 mg vs 20-40 mg###
###############################

##OR of 20 mg by Drug
MyData = read_excel("20(vs20-40).xlsx",  na = "NA")
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
ORFixed20


LnORFixed = ORFixed20$TE.random.w
seLnORFixed = ORFixed20$seTE.random.w
names(LnORFixed) = unique(ORFixed20$byvar)
names(seLnORFixed) = unique(ORFixed20$byvar)
#create SSRI, MIR, VEN
LnORFixedSSRI = LnORFixed[c("escitalopram","citalopram","fluoxetine","paroxetine","sertraline")]
seLnORFixedSSRI = seLnORFixed[c("escitalopram","citalopram","fluoxetine","paroxetine","sertraline")]
LnORFixedMIR = LnORFixed[c("mirtazapine")]
seLnORFixedMIR = seLnORFixed[c("mirtazapine")]
LnORFixedVEN = LnORFixed[c("venlafaxine")]
seLnORFixedVEN = seLnORFixed[c("venlafaxine")]
LnORFixedSSRI
LnORFixedMIR
LnORFixedVEN

##OR of 20-40 mg by Drug
MyData2 = read_excel("20-40.xlsx",  na = "NA")
MyData2 = as.data.frame(MyData2)
###OR of response
ORFlexible20_40 = metabin(Respond_e, N_rand_e, Respond_c, N_rand_c, data=MyData2,
              studlab=StudyID,
              comb.fixed=F,
              comb.rand=T,
              method.tau="REML",
              hakn=T,
              incr=0.1,
              sm="OR",
              byvar=Drug)
ORFlexible20_40

LnORFlexible = ORFlexible20_40$TE.random.w
seLnORFlexible = ORFlexible20_40$seTE.random.w
names(LnORFlexible) = unique(ORFlexible20_40$byvar)
names(seLnORFlexible) = unique(ORFlexible20_40$byvar)
#create SSRIs, MIR, VEN
LnORFlexibleSSRI = LnORFlexible[c("escitalopram","citalopram","fluoxetine","paroxetine","sertraline")]
seLnORFlexibleSSRI = seLnORFlexible[c("escitalopram","citalopram","fluoxetine","paroxetine","sertraline")]
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
forest(pooledRORSSRI)

###venlafaxine
pooledRORVEN=metagen(LnRORVEN,seLnRORVEN,
                     studlab=names(LnRORVEN),
                     comb.fixed=F,
                     comb.rand=T,
                     method.tau="REML",
                     hakn=T,
                     prediction=T,
                     sm="OR")
pooledRORVEN
forest(pooledRORVEN)


#drawing a forest plot of RORs with three subgroups together
LnRORSSRI
LnRORVEN
LnRORMIR

LnROR = c(LnRORSSRI, LnRORVEN)
seLnROR = c(seLnRORSSRI, seLnRORVEN)
DataframeROR = cbind.data.frame(Class=c("SSRI", "SSRI", "SSRI", "SSRI", "SSRI", "VEN"), 
                                Drug=c("citalopram", "escitalopram", "fluoxetine", "paroxetine", "sertraline", "venlafaxine"),
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
       rightlabs=c("ROR","95%CI","weight"),
       smlab="ROR",
       print.i2=T, print.i2.ci=T,
       label.right="Favours flexible dose", 
       label.left="Favours fixed low dose",
       prediction=F,
       fs.lr=9,
       col.square="blue",col.diamond="red",
       colgap.forest="1.5cm",
       overall=F,
       sortvar=c("citalopram", "escitalopram", "fluoxetine", "paroxetine", "sertraline", "venlafaxine"),
       fs.axis=9,
       col.by="black")

