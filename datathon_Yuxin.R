## read data
data <- read.csv(file = "~/Desktop/Rotman MMA Summer Datathon (men&women olympic).csv", header = T)


## convert data set to a data frame
df.data <- as.data.frame(data)
## extract women's data
library(dplyr)
library(ggplot2)
df.women <- filter(df.data, team_name == "Olympic (Women) - Canada")


## check situation types
plot1 <- ggplot(df.women, aes(x = factor(situation_type), y = event_successful))
print(plot1 + geom_boxplot(aes(fill = factor(situation_type))) + 
        stat_summary(fun = mean, colour="darkred", geom = "point", 
                     shape=18, size=2,show.legend = FALSE))





## convert 4 on 3, 6 on 5, 5 on 3, 6 on 4, 6 on 5 to 5 on 4 situation, since they 
## all belong to power play cases



for(i in 1:nrow(df.women)){  
  if (df.women$situation_type[i] == "6 on 5"){
    df.women$situation_type[i] = "5 on 4"
  }
}

filter(df.women, situation_type == "6 on 5")


for(i in 1:nrow(df.women)){  
  if (df.women$situation_type[i] == "5 on 3"){
    df.women$situation_type[i] = "5 on 4"
  }
}

filter(df.women, situation_type == "5 on 3")


for(i in 1:nrow(df.women)){  
  if (df.women$situation_type[i] == "4 on 3"){
    df.women$situation_type[i] = "5 on 4"
  }
}

filter(df.women, situation_type == "4 on 3")

for(i in 1:nrow(df.women)){  
  if (df.women$situation_type[i] == "6 on 4"){
    df.women$situation_type[i] = "5 on 4"
  }
}

filter(df.women, situation_type == "6 on 4")

for(i in 1:nrow(df.women)){  
  if (df.women$situation_type[i] == "6 on 5"){
    df.women$situation_type[i] = "5 on 4"
  }
}


filter(df.women, situation_type == "6 on 5")

##############################################################################################
#
#      POWER PLAY ANALYSIS
#
###############################################################################################
## Case 1 : play position (both home and away venue) 5 on 4 & 5 on 3 & 6 on 5
## create a new data frame for finding play specialists 
power.women.1 <-  df.women[(df.women$situation_type == "5 on 4") &
                             (df.women$event_type == "Play"),]


## sum of faliure and success of play event for each female athelet
sum.play.1 <- power.women.1 %>% count(player_name, sort = TRUE)




## count the success play events for each female athelet
succ.women.1 <- df.women[(df.women$situation_type == "5 on 4") &
                           (df.women$event_successful == "t") &
                           (df.women$event_type == "Play"),]

succ.count.1 <- succ.women.1 %>% count(player_name, sort = TRUE)



## combine both data to generate the rate of success of play event for each female athelet
play.data.1 <- merge(succ.count.1, sum.play.1, by = "player_name" )
colnames(play.data.1)[2] <- "success_count"
colnames(play.data.1)[3] <- "sum_of_play_times"


play.data.1$success_rate <- (play.data.1$success_count/ play.data.1$sum_of_play_times)



## generate candidates for play position
all.candidata <- arrange(play.data.1, desc(success_rate))
all.candidata


## filter condition
sum.mean <- sum(play.data.1$sum_of_play_times / nrow(play.data.1))

## generate top 5 power play specialists
candidate.play <- arrange(filter(play.data.1, sum_of_play_times >= round(sum.mean)), 
                          desc(success_rate))
top5.play <- candidate.play[1:5,]
top5.play
################################################################################################









####################################################################################################
## Case 2 : shot position, 5 on 4 & 5 on 3 & 6 on 5
## create a new data frame for finding shot specialists 
power.women.2 <-  df.women[(df.women$situation_type == "5 on 4") &
                             (df.women$event_type == "Shot"),]

a <- filter(power.women.2, player_name == "Haley Irwin")
a

## sum of faliure and success of shot event for each female athelet
sum.shot.2 <- power.women.2 %>% count(player_name, sort = TRUE)




## count the success shot events for each female athelet
succ.women.2 <- df.women[(df.women$situation_type == "5 on 4") &
                           (df.women$event_successful == "t") &
                           (df.women$event_type == "Shot"),]

succ.count.2 <- succ.women.2 %>% count(player_name, sort = TRUE)



## combine both data to generate the rate of success of shot event for each female athelet
shot.data.2 <- merge(succ.count.2, sum.shot.2, by = "player_name" )
colnames(shot.data.2)[2] <- "success_count"
colnames(shot.data.2)[3] <- "sum_of_shot_times"


shot.data.2$success_rate <- (shot.data.2$success_count/ shot.data.2$sum_of_shot_times)




## generate candidates for shot position 
candidate.shot <- arrange(shot.data.2, desc(success_rate))
top5.shot <- candidate.shot[1:5,]
top5.shot
####################################################################################################


## convert 5 on 6 and to 4 on 5 situation, since they all belong to penalty kill cases
for(i in 1:nrow(df.women)){  
  if (df.women$situation_type[i] == "5 on 6"){
    df.women$situation_type[i] = "4 on 5"
  }
}

d <- filter(df.women, situation_type == "5 on 6", event_type == "Play")
d




####################################################################################################
#
#      PENALTY KILL ANALYSIS
#
#####################################################################################################
## Case 3 :  takeaway position, 4 on 5 & 5 on 6
## create a new data frame for finding penalty kill specialists at home venue
penalty.women.3 <-  df.women[(df.women$situation_type == "4 on 5") &
                               (df.women$event_type == "Takeaway"),]


## sum of faliure and success of takeaway event for each female athelet
sum.take.3 <- penalty.women.3 %>% count(player_name, sort = TRUE)




## count the success takeaway events for each female athelet
succ.women.3 <- df.women[(df.women$situation_type == "4 on 5") &
                           (df.women$event_successful == "t") &
                           (df.women$event_type == "Takeaway"),]

succ.count.3 <- succ.women.3 %>% count(player_name, sort = TRUE)



## combine both data to generate the rate of success of takeaway event for each female athelet
penalty.data.3 <- merge(succ.count.3, sum.take.3, by = "player_name" )
colnames(penalty.data.3)[2] <- "success_count"
colnames(penalty.data.3)[3] <- "sum_of_takeaway"


penalty.data.3$success_rate <- (penalty.data.3$success_count/ penalty.data.3$sum_of_takeaway)




## generate candidates for takeaway position 
candidate.take <- arrange(penalty.data.3 ,desc(success_rate))
candidate.take
top4.take <- candidate.take[1:4,]
top4.take
#############################################################################################










#############################################################################################
## Case 4 : play position (both home and away venue) 4 on 5 & 5 on 6
## create a new data frame for finding play specialists 
power.women.4 <-  df.women[(df.women$situation_type == "4 on 5") &
                             (df.women$event_type == "Play"),]


## sum of faliure and success of play event for each female athelet
sum.play.4 <- power.women.4 %>% count(player_name, sort = TRUE)




## count the success play events for each female athelet
succ.women.4 <- df.women[(df.women$situation_type == "4 on 5") &
                           (df.women$event_successful == "t") &
                           (df.women$event_type == "Play"),]

succ.count.4 <- succ.women.4 %>% count(player_name, sort = TRUE)




## combine both data to generate the rate of success of play event for each female athelet
play.data.4 <- merge(succ.count.4, sum.play.4, by = "player_name" )
colnames(play.data.4)[2] <- "success_count"
colnames(play.data.4)[3] <- "sum_of_play_times"


play.data.4$success_rate <- (play.data.4$success_count/ play.data.4$sum_of_play_times)



## generate candidates for play position
all.candidata.2 <- arrange(play.data.4, desc(success_rate))
all.candidata.2

 
## filter condition
sum.mean.2 <- sum(play.data.4$sum_of_play_times / nrow(play.data.4))


candidate.play.2 <- arrange(filter(play.data.4, sum_of_play_times >= round(sum.mean.2)), 
                            desc(success_rate))
top4.play <- candidate.play.2[1:4,]
top4.play

new <- arrange(inner_join(candidate.play.2, candidate.take, by = "player_name"),
               desc(success_rate.x))
new

new.top4 <- select(new[1:4,], -success_count.y,-sum_of_takeaway, -success_rate.y)
new.top4








library("ggplot2")
df1 <- filter(df.women, event_type == "Play", situation_type == "5 on 4")
p1 <- ggplot(df1, aes(x = x_event, y = y_event)) + ggtitle("Play Events for Power Specialists")
print(p1 + geom_point(aes(shape = factor(event_successful), color = factor(event_successful))))



df2 <- filter(df.women, event_type == "Shot", situation_type == "5 on 4")
p2 <- ggplot(df2, aes(x = x_event, y = y_event)) + ggtitle("Shot Events for Power Specialists")
print(p2 + geom_point(aes(shape = factor(event_successful), color = factor(event_successful))))



df3 <- filter(df.women, event_type == "Takeaway", situation_type == "4 on 5")
p3 <- ggplot(df3, aes(x = x_event, y = y_event)) + ggtitle("Takeaway Events for Penalty Kill Specialists")
print(p3 + geom_point(aes(shape = factor(event_successful), color = factor(event_successful))))




df4 <- filter(df.women, event_type == "Play", situation_type == "5 on 4")
p1 <- ggplot(df4, aes(x = receiver_x, y = receiver_y)) + ggtitle("Play Events for Receivers")
print(p1 + geom_point(aes(shape = factor(event_successful), color = factor(event_successful))))








##########################################################################################
#
#  Predictions
#
###########################################################################################
df.data <- as.data.frame(data)
df.women <- filter(df.data, team_name == "Olympic (Women) - Canada")

## logistic model for pass 
df.pass1 <- filter(df.women, event_type == "Play")
df.pass1
## create new variables 
df.pass1$pass_success <- ifelse(df.pass1$event_successful == 't', 1, 0)
df.pass1
df.pass1$pass_distance <- sqrt((df.pass1$receiver_x - df.pass1$x_event)^2 +
                                 (df.pass1$receiver_y - df.pass1$y_event)^2)
df.pass1$pass_angle <- acos((df.pass1$receiver_x - df.pass1$x_event)/
                              (df.pass1$pass_distance))*(180/pi)


## create logistic model 
pass.model.1 <- glm(pass_success ~ x_event + y_event + pass_distance + pass_angle + 
                      receiver_x + receiver_y, data = df.pass1, family = "binomial")
summary(pass.model.1)



## split dataset into train and test datasets
library(caTools)
set.seed(101)
pass.split.1= sample.split(df.pass1$pass_success, SplitRatio = 0.70)
pass.train.1 = filter(subset(df.pass1, pass.split.1 == TRUE), pass_distance > 0)
pass.test.1 = filter(subset(df.pass1, pass.split.1 == FALSE), pass_distance > 0)


final.pass.1 <- glm(pass_success ~ x_event + y_event + pass_distance + pass_angle + 
                      receiver_x + receiver_y, data = pass.train.1, family = "binomial")
summary(final.pass.1)



## calculate predicition accurary of the model
pass.prob.1 <- predict(final.pass.1, newdata = pass.test.1, type = 'response')
pass.results.1 <- ifelse(pass.prob.1 > 0.5,1,0)
pass.error.1 <- mean(pass.results.1 != pass.test.1$pass_success)
print(paste('Accuracy',1-pass.error.1))   ## 0.738



## add prediciotns for pass in the dataset
df.pass1$pass_predictions <- predict(pass.model.1, df.pass1,type = "response")
df.pass1$pass_predictions <- ifelse(df.pass1$pass_distance == 0,1,
                                    predict(pass.model.1, df.pass1,type = "response"))


## plot boxplot: situation types vs pass predictions
options(digits = 3)
pass_means1 <- aggregate(pass_predictions ~  situation_type, df.pass1, mean)
pass_means1
pass.p1 <- ggplot(df.pass1, aes(x = factor(situation_type), y = pass_predictions))
print(pass.p1 + geom_boxplot(aes(fill = factor(situation_type))) + 
        stat_summary(fun = mean, colour="darkred", geom = "point", 
                     shape=18, size=2,show.legend = FALSE))




## plot top 5: pass angles or pass distance vs pass prediciotns
pass.data <- inner_join(top5.play, df.pass1, by = "player_name")
pass.p2 <- ggplot(data = pass.data , aes(x = pass_distance, y = pass_predictions,
                                      color = player_name)) 
print(pass.p2 + geom_smooth(fill = NA))



pass.p3 <- ggplot(data = pass.data , aes(x = pass_angle, y = pass_predictions,
                                         color = player_name)) 
print(pass.p3 + geom_smooth(fill = NA))



## plot boxplot: periods vs pass predictions
pass_means2 <- aggregate(pass_predictions ~  period, df.pass1, mean)
pass_means2
df.pass1$period = factor(df.pass1$period)
pass.p4 <- ggplot(df.pass1, aes(x = factor(period), y = pass_predictions))
print(pass.p4 + geom_boxplot(aes(fill = factor(period))) + 
        stat_summary(fun = mean, colour="darkred", geom = "point", 
                     shape=18, size=2,show.legend = FALSE))



## pass distance vs pass predictions with factor pass success
pass.p5 <- ggplot(df.pass1, aes(x = pass_distance, y = pass_predictions)) 
print(pass.p5 + geom_point(aes(shape = factor(pass_success), color = factor(pass_success))))



## pass angle vs pass predictions with factor pass success
pass.p6 <- ggplot(df.pass1, aes(x = pass_angle, y = pass_predictions)) 
print(pass.p6 + geom_point(aes(shape = factor(pass_success), color = factor(pass_success))))



##
pass.data2 <- inner_join(candidate.play, df.pass1, by = "player_name")
pass.data2
library(reshape)
pass.melt <- melt(select(df.pass1, pass_predictions, pass_distance, pass_angle, pass_success))
head(pass.melt)
pass.data$pass_distance = factor(pass.data$pass_distance)
pass.p7 <- ggplot(df.pass1, aes(x = pass_distance, y = pass_distance)) 
print(pass.p7 + geom_tile(aes(fill = pass_predictions))
      + scale_fill_gradient(low = "white", high = "blue"))

###########################################################################################





###########################################################################################
## logistic model for shot 
df.shot1 <- filter(df.women, event_type == "Shot")
## create new variables 
df.shot1$goal <- ifelse(df.shot1$event_successful == 't', 1, 0)
goal.net <- c(200,85/2)
df.shot1$shot_distance <- sqrt((goal.net[1] - df.shot1$x_event)^2 + 
                                (goal.net[2] - df.shot1$y_event)^2)
df.shot1$shot_angle <- acos((goal.net[1] - df.shot1$x_event)/
                              (df.shot1$shot_distance))*(180/pi)




## create logistic model 
shot.model.1 <- glm(goal ~ x_event + y_event + shot_distance + shot_angle + shot_type, 
                    data = df.shot1, family = "binomial")
summary(shot.model.1)


## split dataset into train and test datasets
library(caTools)
set.seed(101)
shot.split.1= sample.split(df.shot1$goal, SplitRatio = 0.70)
shot.train.1 = subset(df.shot1, shot.split.1 == TRUE)
shot.test.1 = subset(df.shot1, shot.split.1 == FALSE)


final.shot.1 <- glm(goal ~ x_event + y_event + shot_distance + shot_angle + shot_type + period,
                    data = shot.train.1, family = "binomial")
summary(final.shot.1)



## calculate predicition accurary of the model
shot.prob.1 <- predict(final.shot.1, newdata = shot.test.1, type = 'response')
shot.results.1 <- ifelse(shot.prob.1 > 0.5,1,0)
shot.error.1 <- mean(shot.results.1 != shot.test.1$goal)
print(paste('Accuracy',1-shot.error.1))   ## 0.9628



## add prediciotns for shot in the dataset
df.shot1$shot_predictions <- predict(shot.model.1, df.shot1,type = "response")



## plot boxplot: situation types vs shot predictions
options(digits = 3)
shot_means1 <- aggregate(goal ~  situation_type, df.shot1, mean)
shot_means1
shot.p1 <- ggplot(df.shot1, aes(x = factor(situation_type), y = goal))
print(shot.p1 + geom_boxplot(aes(fill = factor(situation_type))) + 
        stat_summary(fun = mean, colour="darkred", geom = "point", 
                     shape=18, size=2,show.legend = FALSE))


## plot boxplot: period vs shot predictions
shot_means2 <- aggregate(goal ~  period, df.shot1, mean)
shot_means2
shot.p1 <- ggplot(df.shot1, aes(x = factor(period), y = goal))
print(shot.p1 + geom_boxplot(aes(fill = factor(period))) + 
        stat_summary(fun = mean, colour="darkred", geom = "point", 
                     shape=18, size=2,show.legend = FALSE))




## plot top 5: shot angles or shot distance vs shot prediciotns
shot.data <- inner_join(top5.shot, df.shot1, by = "player_name")
shot.p3 <- ggplot(data = shot.data , aes(x = shot_distance, y = shot_predictions,
                                         color = player_name)) 
print(shot.p3 + geom_smooth(fill = NA))



shot.p4 <- ggplot(data = shot.data , aes(x = shot_angle, y = shot_predictions,
                                         color = player_name)) 
print(shot.p4 + geom_smooth(fill = NA))




## shot distance vs shot predictions with factor goal
shot.p5 <- ggplot(df.shot1, aes(x = shot_distance, y = shot_predictions)) 
print(shot.p5 + geom_point(aes(shape = factor(goal), color = factor(goal))))



## shot angle vs shot predictions with factor goal
shot.p6 <- ggplot(df.shot1, aes(x = shot_angle, y = shot_predictions)) 
print(shot.p6 + geom_point(aes(shape = factor(goal), color = factor(goal))))

############################################################################################






###########################################################################################
## logistic model for takeaway 
df.take1 <- filter(df.women, event_type == "Takeaway")
a <- filter(df.women, event_type == "Takeaway")
a
## create new variables 
df.take1$take_success <- ifelse(df.take1$event_successful == 't', 1, 0)
df.take1$take_distance <- sqrt((df.take1$x_event)^2 + (df.take1$y_event)^2)
df.take1$take_angle <- acos((df.take1$x_event)/(df.take1$take_distance))*(180/pi)
df.take1



## create logistic model 
take.model.1 <- glm(take_success ~ x_event + y_event + take_distance + take_angle,
                    data = df.take1, family = "binomial")
summary(take.model.1)


## split dataset into train and test datasets
library(caTools)
set.seed(101)
take.split.1= sample.split(df.take1$take_success, SplitRatio = 0.70)
take.train.1 = subset(df.take1, take.split.1 == TRUE)
take.test.1 = subset(df.take1, take.split.1 == FALSE)


final.take.1 <- glm(take_distance ~ x_event + y_event + take_distance + take_angle,
                    data = take.train.1, family = "binomial")
summary(final.take.1)



## calculate predicition accurary of the model
take.prob.1 <- predict(final.take.1, newdata = take.test.1, type = 'response')
take.results.1 <- ifelse(take.prob.1 > 0.5,1,0)
take.error.1 <- mean(take.results.1 != take.test.1$pass_success)
print(paste('Accuracy',1-take.error.1))   



## add prediciotns for shot in the dataset
df.shot1$shot_predictions <- predict(shot.model.1, df.shot1,type = "response")
df.shot1



## decision tree 
library(rpart)
take.tree <- rpart(take_success ~ x_event + y_event + take_distance + take_angle,
                   data = df.take1, control=rpart.control(cp=.005))
plot(take.tree, uniform=TRUE, margin=.05)
text(take.tree)

take_predictions <- predict(take.tree)
for (i in 1:nrow(df.take1)){
  if(take_predictions[i,1]>=0.5){df.take1[i,"take_predictions"] = 0}
  else{df.take1[i,"take_predictions"] = 1}
}
sum(df.take1$take_predictions == df.take1$sucess)/nrow(df.take1)  
take.tree
#########################################################################################














