###############################
#ROR of 20 mg vs 20-40 mg###
###############################

##OR of 20 mg by Drug
MyData = read_excel("20(vs20-40).xlsx",  na = "NA")
MyData = as.data.frame(MyData)
###OR of response
ORFixed20 = metabin(DropAE_e, N_rand_e, DropAE_c, N_rand_c, data=MyData,
             studlab=StudyID,
             comb.fixed=F,
             comb.rand=T,
             method.tau="REML",
             hakn=T,
             incr=0.1,
             sm="OR",
             byvar=Drug)
ORFixed20

## names(ORFixed20)
## summary(ORFixed20)
## help(metabin)

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
ORFlexible20_40 = metabin(DropAE_e, N_rand_e, DropAE_c, N_rand_c, data=MyData2,
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
forest(pooledRORMIR)

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


