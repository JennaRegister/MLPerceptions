library(ggplot2)
library(dplyr)

data <- read.csv("MLPercept.csv")

#remove some fluff and clean
data <- data[,-(1:14)]
data <- data %>%
  select(AssignmentId,WorkerId,Answer.AIinvolve,Answer.Age,Answer.Gender,Answer.InternetUsage,Answer.Race,Answer.SocialMedia,Answer.comments,Answer.q1,Answer.q2,Answer.q3,Answer.q4,Answer.q5,Answer.q6,Answer.q7)
colnames(data) <- c("AssignmentID","WorkerID","Involvement","Age","Gender","InternetUsage","Race","SocialMedia","Comments","Q1","Q2","Q3","Q4","Q5","Q6","Q7")
View(data)

data$Q1=ordered(data$Q1, levels=c("Strongly Disagree","Disagree","Neither Agree or Disagree","Agree","Strongly Agree"))
data$Q2 = ordered(data$Q1, levels=c("Strongly Disagree","Disagree","Neither Agree or Disagree","Agree","Strongly Agree"))

questions = c(data$Q1,data$Q2,data$Q3,data$Q4,data$Q5,data$Q6,data$Q7)

#why is this all effed up
reorder <- function(q){
  q=factor(q,levels(q)[c(6,3,4,2,5)])
  return(q)
  
}

data$Q3<-reorder(data$Q3)
data$Q4<-reorder(data$Q4)
data$Q5<-reorder(data$Q5)
data$Q6<-reorder(data$Q6)
data$Q7<-reorder(data$Q7)




#Demographics
plt = ggplot(data,aes(Age,Race,color=Gender))+
  geom_point()+
  theme_bw()
plt




plt = ggplot(na.omit(data[data$Q1!="",]),aes(Q1,..count../sum(..count..)))+
  geom_point(stat="count")+
  theme_bw()+
  xlab("AI will take over the world.")+
  ylab("Proportion of Participants")+
  ylim(0,1)
  
plt 

plt = ggplot(na.omit(data[data$Q2!="",]),aes(Q2,..count../sum(..count..)))+
  geom_point(stat="count")+
  theme_bw()+
  xlab("AI will steal our jobs.")+
  ylab("Proportion of Participants")+
  ylim(0,1)
plt 

plt = ggplot(na.omit(data[data$Q3!="",]),aes(Q3,..count../sum(..count..)))+
  geom_point(stat="count")+
  theme_bw()+
  xlab("AI helps people.")+
  ylab("Proportion of Participants")+
  ylim(0,1)
plt 

plt = ggplot(na.omit(data[data$Q4!="",]),aes(Q4,..count../sum(..count..)))+
  geom_point(stat="count")+
  theme_bw()+
  xlab("AI is necessary in today's society.")+
  ylab("Proportion of Participants")+
  ylim(0,1)
plt 

plt = ggplot(na.omit(data[data$Q5!="",]),aes(Q5,..count../sum(..count..)))+
  geom_point(stat="count")+
  theme_bw()+
  xlab("Targeted ads help me choose products.")+
  ylab("Proportion of Participants")+
  ylim(0,1)
plt 

plt = ggplot(na.omit(data[data$Q6!="",]),aes(Q6,..count../sum(..count..)))+
  geom_point(stat="count")+
  theme_bw()+
  xlab("Targeted ads are creepy.")+
  ylab("Proportion of Participants")+
  ylim(0,1)
plt 

data$Q7=factor(data$Q7,levels(data$Q7)[c(2,3,1,4)])
plt = ggplot(na.omit(data[data$Q7!="",]),aes(Q7,..count../sum(..count..)))+
  geom_point(stat="count")+
  theme_bw()+
  xlab("Are you interested in more education about AI?")+
  ylab("Proportion of Participants")+
  ylim(0,1)
plt 

data=data[data$Q3!="",]
plt = ggplot(na.omit(data[data$Q7!="",]),aes(Q7,..count../sum(..count..)))+
  facet_wrap(~Q3)+
  geom_point()+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ggtitle("AI helps people")+
  xlab("Are you interested in more education about AI?")+
  ylab("Proportion of Participants")+
  ylim(0,.3)
plt 
ggsave("helpwanted.pdf",plt)
ggsave("helpwanted.png",plt)


data=data[data$Q2!="",]
plt = ggplot(na.omit(data[data$Q7!="",]),aes(Q7,..count../sum(..count..)))+
  facet_wrap(~Q2)+
  geom_point()+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ggtitle("AI will steal our jobs.")+
  xlab("Are you interested in more education about AI?")+
  ylab("Proportion of Participants")+
  ylim(0,.3)
plt 
ggsave("stealwant.pdf",plt)
ggsave("stealwant.png",plt)

data=data[data$Q5!="",]
plt = ggplot(na.omit(data[data$Q7!="",]),aes(Q7,..count../sum(..count..)))+
  facet_wrap(~Q5)+
  geom_point()+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ggtitle("Targeted ads help me to choose products.")+
  xlab("Are you interested in more education about AI?")+
  ylab("Proportion of Participants")+
  ylim(0,.3)
plt 
ggsave("targetedwant.png",plt)
comments = data.frame(data$Comments)

wannabeusers=data[data$Involvement=="User",]


plt = ggplot(wannabeusers[wannabeusers$Q7!="",],aes(Q7,..count../sum(..count..)))+
  geom_point(stat="count")+
  theme_bw()+
  xlab("Are you interested in more education about AI?")+
  ylab("Proportion of Participants")+
  ylim(0,1)
plt 

wannabeeng=data[data$Involvement=="Engineer",]
plt = ggplot(wannabeeng[wannabeeng$Q7!="",],aes(Q7,..count../sum(..count..)))+
  geom_point(stat="count")+
  theme_bw()+
  xlab("Are you interested in more education about AI?")+
  ylab("Proportion of Participants")+
  ylim(0,1)
plt 

chisq.test(data$Involvement,data$Q7)
chisq.test(data$Q7,data$Q1)

chisq.test(data$Q3,data$Q7)

