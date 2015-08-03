## TO DO
# cells?
# am i grabbing the right data?

## NOTE ON RESULTS
# most seem to be niche specialization (more suitable in their respective polygons and equally unsuitable in the other two polygons)
# spring inland shows niche conservatism with barrier (more suitable in both their polygon and that of the other subspecies)

id="spring"
id2="inland"

## setwd, read data in and load libraries
setwd("~/PhD/Analysis/Connectivity-Analysis")
data=read.csv(paste("C:/Users/Kira Delmore/Dropbox/Files for Julie/SuitScores_allThrushes_",id,".csv",sep=""),stringsAsFactors = FALSE,strip.white = TRUE, na.strings = c("NA",""))
library(visreg)

## select taxonomic group you're running and rearrange dataframe to run lm

# this will get pure populations
temp=subset(data,data$species==id2)
temp2=stack(temp,select=c(paste("aveSuitability_coastal_",id,"_mcp_hinge",sep=""),paste("aveSuitability_inland_",id,"_mcp_hinge",sep="")))
names(temp2)=c("suitability","range")

# this will get other taxonomic groups in intermediate area
temp3=subset(data,data$species%in%c("Veery","Gray-cheeked Thrush","Hermit Thrush","American Robin", "Western Bluebird", "Mountain Bluebird"))
temp4=subset(temp3,temp3$MigratoryRoute=="intermediate_route")
temp5=as.data.frame(temp4[,paste("aveSuitability",id2,id,"mcp_hinge",sep="_")])
temp5[,2]="inter_other" ## something about using cell column to remove duplicates here
names(temp5)=c("suitability","range")

# this will get swainsons in intermediate area
temp6=subset(data,data$species=="swainson")
temp7=as.data.frame(temp6[,paste("aveSuitability",id2,id,"mcp_hinge",sep="_")])
temp7[,2]="inter_swth"
names(temp7)=c("suitability","range")

# put em all together
data_stack=rbind(temp2,temp7,temp5)
rm(list=ls(pattern="temp"))
data_stack$range<-gsub(paste("aveSuitability_coastal_",id,"_mcp_hinge",sep=""),"coastal",data_stack$range)
data_stack$range<-gsub(paste("aveSuitability_inland_",id,"_mcp_hinge",sep=""),"inland",data_stack$range)
data_stack$range<-factor(data_stack$range,levels=c("coastal","inter_swth","inter_other","inland"))

## run lm

z<-lm(suitability~range,data=data_stack)
anova(z)
#summary(z)
visreg(z,ylab=paste(id2,"suitability",id,sep=" "),ylim=c(0,1))
a1<-aov(suitability~range,data=data_stack)
posthoc<-TukeyHSD(x=a1,'range',conf.level=0.95)
posthoc

## write table
# write.table(data_stack,paste0("habitat_suitability_by_taxon_",id,"_",id2,".csv",sep=""),quote=FALSE,sep=",",row.names=FALSE,col.names=TRUE)
