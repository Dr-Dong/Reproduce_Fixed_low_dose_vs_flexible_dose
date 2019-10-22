###Table 1###

MyData = read_excel("20(vs20-40-80).xlsx",  na = "NA")
MyData = as.data.frame(MyData)
MyData2 = read_excel("20-40-80.xlsx",  na = "NA")
MyData2 = as.data.frame(MyData2)


if (F) {
MyData$Study_year[is.na(MyData$Study_year) & MyData$Drug=="citalopram"]=1998
MyData$Study_year[is.na(MyData$Study_year) & MyData$Drug=="escitalopram"]=2002
MyData$Study_year[is.na(MyData$Study_year) & MyData$Drug=="fluoxetine"]=1986
MyData$Study_year[is.na(MyData$Study_year) & MyData$Drug=="paroxetine"]=1992
MyData$Study_year[is.na(MyData$Study_year) & MyData$Drug=="sertraline"]=1991
MyData$Study_year[is.na(MyData$Study_year) & MyData$Drug=="mirtazapine"]=1996
MyData$Study_year[is.na(MyData$Study_year) & MyData$Drug=="venlafaxine"]=1993

MyData2$Study_year[is.na(MyData2$Study_year) & MyData2$Drug=="citalopram"]=1998
MyData2$Study_year[is.na(MyData2$Study_year) & MyData2$Drug=="escitalopram"]=2002
MyData2$Study_year[is.na(MyData2$Study_year) & MyData2$Drug=="fluoxetine"]=1986
MyData2$Study_year[is.na(MyData2$Study_year) & MyData2$Drug=="paroxetine"]=1992
MyData2$Study_year[is.na(MyData2$Study_year) & MyData2$Drug=="sertraline"]=1991
MyData2$Study_year[is.na(MyData2$Study_year) & MyData2$Drug=="mirtazapine"]=1996
MyData2$Study_year[is.na(MyData2$Study_year) & MyData2$Drug=="venlafaxine"]=1993
}

length(MyData$StudyID)   ###number of comparisons for the fixed dose comparisons against placebo
length(MyData2$StudyID)   ### number of comparisons for the flexible dose comparisons against placebo
length(unique(c(unique(MyData$StudyID),unique(MyData2$StudyID))))   ###number of included studies


###Descriptives for the fixed dose studies
names(MyData)
table(MyData$Drug)
tapply(MyData$Study_year, MyData$Drug, median, na.rm=T)
tapply(MyData$Study_year, MyData$Drug, min, na.rm=T)
tapply(MyData$Study_year, MyData$Drug, max, na.rm=T)
tapply(MyData$Age, MyData$Drug, mean, na.rm=T)
tapply(MyData$Women, MyData$Drug, mean, na.rm=T)
tapply(MyData$Weeks, MyData$Drug, mean, na.rm=T)

MyData3 = read_excel("baseline.xlsx", na="*")
MyData3 = as.data.frame(MyData3)
MyData3 = subset(MyData3, MyData3$Drug=="placebo")
MyData3$Baseline_mean = as.numeric(MyData3$Baseline_mean)

MyData13 = merge(MyData, MyData3, by="StudyID", all=F)   ###creating a file with baseline severity
table(MyData13$Scale)
MyData13_hamd17 = subset(MyData13, Scale=="HAMD17")
tapply(MyData13_hamd17$Baseline_mean, MyData13_hamd17$Drug.x, mean, na.rm=T)
tapply(MyData13_hamd17$Baseline_mean, MyData13_hamd17$Drug.x, function(x) sum(!is.na(x)))
MyData13_hamd21 = subset(MyData13, Scale=="HAMD21")
tapply(MyData13_hamd21$Baseline_mean, MyData13_hamd21$Drug.x, mean, na.rm=T)
tapply(MyData13_hamd21$Baseline_mean, MyData13_hamd21$Drug.x, function(x) sum(!is.na(x)))


###Descriptives for the flexible dose studies
names(MyData2)
table(MyData2$Drug)
tapply(MyData2$Study_year, MyData2$Drug, median, na.rm=T)
tapply(MyData2$Study_year, MyData2$Drug, min, na.rm=T)
tapply(MyData2$Study_year, MyData2$Drug, max, na.rm=T)
tapply(MyData2$Age, MyData2$Drug, mean, na.rm=T)
tapply(MyData2$Women, MyData2$Drug, mean, na.rm=T)
tapply(MyData2$Weeks, MyData2$Drug, mean, na.rm=T)

MyData23 = merge(MyData2, MyData3, by="StudyID", all=F)   ###creating a file with baseline severity
table(MyData23$Scale)
MyData23_hamd17 = subset(MyData23, Scale=="HAMD17")
tapply(MyData23_hamd17$Baseline_mean, MyData23_hamd17$Drug.x, mean, na.rm=T)
tapply(MyData23_hamd17$Baseline_mean, MyData23_hamd17$Drug.x, function(x) sum(!is.na(x)))
MyData23_hamd21 = subset(MyData23, Scale=="HAMD21")
tapply(MyData23_hamd21$Baseline_mean, MyData23_hamd21$Drug.x, mean, na.rm=T)
tapply(MyData23_hamd21$Baseline_mean, MyData23_hamd21$Drug.x, function(x) sum(!is.na(x)))

MyData4 = read_excel("minmax20-40.xlsx", na="*")   ###creating a file with min & max intended doses
MyData4 = as.data.frame(MyData4)
MyData5 = read_excel("minmax20-80.xlsx", na="*")
MyData5 = as.data.frame(MyData5)
MyData45 = rbind.data.frame(MyData4, MyData5)
MyData45 = subset(MyData45,MyData45$Drug!="placebo")
MyData45$Drug_intended_min = as.numeric(MyData45$Dose_intended_min)
MyData45$Drug_intended_max = as.numeric(MyData45$Dose_intended_max)
tapply(MyData45$Dose_intended_min, MyData45$Drug, min, na.rm=T)
tapply(MyData45$Dose_intended_min, MyData45$Drug, max, na.rm=T)
tapply(MyData45$Dose_intended_max, MyData45$Drug, min, na.rm=T)
tapply(MyData45$Dose_intended_max, MyData45$Drug, max, na.rm=T)
tapply(MyData2$Dose_delivered, MyData2$Drug, mean, na.rm=T)



###all the names of the included studies for the appendix
unique(c(unique(MyData$StudyID),unique(MyData2$StudyID)))


###examination of association between Study_year and regimen
MyData = transform(MyData, regimen="Fixed")
MyData2 = transform(MyData2, regimen="Flexible")
MyData6 = rbind.data.frame(MyData, MyData2)
wilcox.test(Study_year ~ regimen, MyData6)
t.test(Study_year ~ regimen, MyData6)
MyRegression = glm(regimen ~ Study_year+Drug, family=binomial(link="logit"), MyData6)
summary(MyRegression)
boxplot(Study_year ~ regimen, MyData6)

###counting published and unpublished studies
MyData6 = distinct(MyData6, StudyID, .keep_all=T)   ###need dplyr
table(MyData6$Unpublished, exclude=NULL)
table(MyData6$Study_year, exclude=NULL)
