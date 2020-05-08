## These data is too large to upload on github
## So I show the code to process this data

age2=read.csv("raw_data-2.csv",header=TRUE)
age65risk=age2[2:52,9]
bed=read.csv("raw_data.csv",header=TRUE)
hospbed=bed[2:52,5]
state_name=bed[2:52,1]
kffdata=data.frame(state_name,hospbed,age65risk*100)
write.csv(kffdata,"kffdata.csv")

acs=read.csv("usa_00009.csv",header=TRUE)
acs2=acs%>%group_by(STATEFIP)%>%summarise(metrocity=mean(METRO>1,na.rm=TRUE)*100,age25=mean(AGE<=25)*100,ins=mean(HCOVANY==1)*100)
write.csv(acs2,"acs.csv")
