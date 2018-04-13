#script to calculate the SACS measures from AGLTresults table

# SACS = Z(%)-Z(RT) Speed accuracy composite scores
# high score--> efficient performance, low score --> poor performance

# calculate this for each congruency separation condition  and all trials 

library("dplyr", lib.loc="/usr/local/lib/R/site-library")
AGLTresults<-data.frame(AGLTresults)
######### L_con ##############

ACC<-AGLTresults[,1]/40
RT<-AGLTresults[,2]
ACC[ACC==1]<-0.95
ACC[ACC==0]<-0.0001
SACS_Lcon=qnorm(ACC)-qnorm(RT)
rm(ACC,RT)

######### L_incon ##############

ACC<-AGLTresults[,3]/40
RT<-AGLTresults[,4]
ACC[ACC==1]<-0.95
ACC[ACC==0]<-0.0001
SACS_Linc=qnorm(ACC)-qnorm(RT)
rm(ACC,RT)


######### G_con ##############

ACC<-AGLTresults[,5]/40
RT<-AGLTresults[,6]
ACC[ACC==1]<-0.95
ACC[ACC==0]<-0.0001
SACS_Gcon=qnorm(ACC)-qnorm(RT)
rm(ACC,RT)


######### G_incon ##############

ACC<-AGLTresults[,7]/40
RT<-AGLTresults[,8]
ACC[ACC==1]<-0.95
ACC[ACC==0]<-0.0001
SACS_Ginc=qnorm(ACC)-qnorm(RT)
rm(ACC,RT)


######### all ###############

ACC<-(AGLTresults[,1]+AGLTresults[,3]+AGLTresults[,5]+AGLTresults[,7])/160
RT<-(AGLTresults[,2]+AGLTresults[,4]+AGLTresults[,6]+AGLTresults[,8])/4
ACC[ACC==1]<-0.95
ACC[ACC==0]<-0.0001
SACS_all=qnorm(ACC)-qnorm(RT)
rm(ACC,RT)

AGLT_SACS<-cbind(SACS_Lcon,SACS_Linc,SACS_Gcon,SACS_Ginc,SACS_all)
AGLT_SACS<-data.frame(AGLT_SACS)
colnames(AGLT_SACS)<-c("aud_SACS_Lcon","aud_SACS_Linc","aud_SACS_Gcon","aud_SACS_Ginc","aud_SACS_all")
rownames(AGLT_SACS)<-rownames(AGLTresults)

#ggf

AGLTresults<-cbind(AGLTresults,AGLT_SACS)
