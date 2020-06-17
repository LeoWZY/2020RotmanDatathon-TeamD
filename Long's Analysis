# write data into R file
getwd()
setwd("C:\\Datathon")
hockey<-read.csv("Rotman MMA Summer Datathon.csv")

#please remember to install and call out lib ggplot2, dplyr,ggpubr.
# remove unimportant columns
hockey$game_date<-NULL
hockey$game_name<-NULL
hockey$opp_team_name<-NULL



# index
hockey$indexnum=c(1:18117)
# filter dataframe for women teams data only
data1<-hockey[hockey$team_name=="Olympic (Women) - Canada",]
data1<-data.frame(data1)

#filter for event type== "Play"(passing)
passdata<-data1[data1$event_type=="Play",]

#creating important vars.
passdata$passdatasuccess<-ifelse(passdata$event_successful=='t',1,0)

#passdistance(as the square root of the sum of squares of vertical& horizontal differences)
passdata$passdistance<-sqrt((passdata$receiver_x-passdata$x_event)^2+
                     (passdata$receiver_y-passdata$y_event)^2)

#passangle- higher the angle the flatter the pass, and more downward it becomes.
passdata$passangle<-acos((passdata$receiver_x-passdata$x_event)/(passdata$passdistance))*(180/pi)

#passdirections- 1 if upward, 0 if downward pass
passdata$passdirection<-ifelse(passdata$receiver_x-passdata$x_event>0,1,0)

#factoring
passdata$period<-factor(passdata$period)

# model 1: logistic model
passmodel<-glm(passdata$passdatasuccess ~ passdata$x_event + passdata$y_event +
                 passdata$passdistance+passdata$passangle,data = passdata,
                 family = "binomial")
summary(passmodel)
  passdata$pass_predictions<-predict(passmodel,passdata,type="response")

# analyse fit of model 1
for (i in 1:nrow(passdata)){
  passdata$pred_log[i]<-ifelse(passdata$pass_predictions[i]>0.5,1,0)
}

# precentage of "right" fitted values.
valuefit1<-passdata$pred_log==passdata$passdatasuccess
table(valuefit1)["TRUE"]/nrow(passdata)

#model 2: Decision Tree model.
library(rpart)

tree<-rpart(passdatasuccess~x_event+y_event+
              passdistance+passangle,data=passdata,
            control=rpart.control(cp=0.005))
#plot-tree
plot(tree,uniform=TRUE,margin=0.07)
text(tree,cex=0.65)
tree

passdata$pred_tree<-predict(tree)
#analyse fit model 2
for (i in 1:nrow(passdata)){
  passdata$pred_tree_log[i]<-ifelse(passdata$pred_tree[i]>0.5,1,0)
}
# precentage of "right" fitted values.
valuefit2<-passdata$pred_tree_log==passdata$passdatasuccess
table(valuefit2)["TRUE"]/nrow(passdata)

#find top players in passing criteria
playerpass<-passdata %>% group_by(player_name,event_successful) %>% summarise(n=n())%>% mutate(total=sum(n))%>%mutate(rate=n/sum(n))%>% arrange(desc(rate))%>% filter(event_successful=="t")
mean(playerpass$total)
quantile(playerpass$total)
# top players with most passes(meaningful data).(100 and over)

playerpass<-passdata %>% group_by(player_name,event_successful) %>% summarise(n=n())%>% mutate(total=sum(n))%>%mutate(rate=n/sum(n))%>% arrange(desc(rate))%>% filter(event_successful=="t"&total>=100&rate>=.75)
playerpass
candidatepass<-passdata%>%filter(passdata$player_name==playerpass$player_name)
candidatepass



#plot for overall population

p1<-ggplot(data=passdata,aes(x=x_event, y=pass_predictions))+geom_smooth(fill=NA)

p1


plot1<-p1+theme(legend.title=element_text(size=1),legend.text=element_text(size=1))
plot1
p1+facet_grid(.~passdirection)+ylim(0.50,1)




p2<-ggplot(data=passdata,aes(passangle, pass_predictions))+geom_point()
p2
p3<-ggplot(data=passdata,aes(receiver_x, pass_predictions))+geom_smooth()
p3
p4<-ggplot(data=passdata,aes(x_event, pass_predictions))+geom_smooth()
p4

# creating column 
#Pass to Pass
passdata[,"nextactsuccess"]<-NA

for (i in 1:(nrow(passdata)-1)){if(passdata$indexnum[i+1]==passdata$indexnum[i]+1){
  if(passdata$event_successful[i+1]=="t"){passdata$nextactsuccess[i]="1"}else{passdata$nextactsuccess[i]="0"}
}
}
candidatenextact<-passdata%>%group_by(player_name,nextactsuccess)%>% summarise(n=n())%>% mutate(total=sum(n))%>%mutate(rate=n/sum(n))%>% arrange(desc(rate))%>% filter(nextactsuccess=="1")
candidatenextact

mean(candidatenextact$total)
quantile(playerpass$total)

candidatenextact2<-candidatenextact%>%filter(total>=120,rate>=0.444)
candidatenextact2
ggplot(data=candidatenextact2,aes(x=player_name,y=rate),color="green")+geom_bar(stat="identity",fill="lightblue")

#situations:)
passdata[,"situation"]<-NA
candidatepass[,"situation"]<-NA

levels(passdata$situation_type)
for (i in 1:(nrow(passdata))){
  if(passdata$situation_type[i]=="3 on 3"|passdata$situation_type[i]=="4 on 4"|passdata$situation_type[i]=="5 on 5"){
  passdata$situation[i]="Equal"}}
for (i in 1:nrow(passdata)){
  if(passdata$situation_type[i]=="3 on 4"|passdata$situation_type[i]=="3 on 5"|passdata$situation_type[i]=="4 on 5"|
                                     passdata$situation_type[i]=="5 on 6"){passdata$situation[i]="Penalty Kill"}}
for (i in 1:nrow(passdata)){
  if(passdata$situation_type[i]=="4 on 3"|passdata$situation_type[i]=="5 on 3"|passdata$situation_type[i]=="5 on 4"|
    passdata$situation_type[i]=="6 on 4"|passdata$situation_type[i]=="6 on 5"){passdata$situation[i]="Power Play"}}

for (i in 1:(nrow(candidatepass))){
  if(candidatepass$situation_type[i]=="3 on 3"|candidatepass$situation_type[i]=="4 on 4"|candidatepass$situation_type[i]=="5 on 5"){
    candidatepass$situation[i]="Equal"}}
for (i in 1:nrow(candidatepass)){
  if(candidatepass$situation_type[i]=="3 on 4"|candidatepass$situation_type[i]=="3 on 5"|candidatepass$situation_type[i]=="4 on 5"|
     candidatepass$situation_type[i]=="5 on 6"){candidatepass$situation[i]="Penalty Kill"}}
for (i in 1:nrow(candidatepass)){
  if(candidatepass$situation_type[i]=="4 on 3"|candidatepass$situation_type[i]=="5 on 3"|candidatepass$situation_type[i]=="5 on 4"|
     candidatepass$situation_type[i]=="6 on 4"|candidatepass$situation_type[i]=="6 on 5"){candidatepass$situation[i]="Power Play"}}

#Pass to Shoot
data2<-hockey[hockey$team_name=="Olympic (Women) - Canada",]
data2<-data.frame(data2)

shootpassdata<-data2[data2$event_type=="Play"|data2$event_type=="Shoot",]
levels(shootpassdata$event_type)
shootpassdata[,"nextactsuccess"]<-NA

for (i in 1:(nrow(shootpassdata)-1)){if(shootpassdata$indexnum[i+1]==shootpassdata$indexnum[i]+1&shootpassdata$event_type[i]=="Play"&shootpassdata$event_type[i+1]=="Shoot"){
  if(shootpassdata$event_successful[i+1]=="t"){shootpassdata$nextactsuccess[i]="1"}else{shootpassdata$nextactsuccess[i]="0"}
}
}
