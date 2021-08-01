#total tasks to perform on matches data is 8
library(ggplot2)    #for elegant data visualization
library(ggsci)      #for color palettes to use on ggplot2 package 
library(tidyr)      #tools to create tidy data
library(dplyr)      #grammar of data manipulation
library(tidyverse)  #load multiple tidyr package
library(lubridate)  #dates and time made easy with lubricate
library(ggpubr)     #for theme 
library(stringr)
library(cowplot)    #help with creating publication-quality figures with 'ggplot2'


matches<-data.frame(read.csv(file.choose(),header=T))
teams<-data.frame(read.csv(file.choose(),header=T))
attach(matches)
attach(teams)
summary(matches)
summary(teams)

matches$date<-as.Date(matches$date,"%d-%m-%Y")
typeof(matches$date)

matches%>%select(c(-1))->matches
view(matches)

#Total matches played per season
matches%>%group_by(season)%>%summarize(match_played=n())%>%
  ggplot(aes(y=match_played,x=factor(season)))+
  geom_bar(stat="identity",fill="pink",col="red")+
  coord_flip()+
  labs(x="Season",y="Matches Played",
  title="Number of matches played per season")+
  geom_text(aes(label=match_played),hjust=1.8)+
  scale_y_continuous()+
  theme(plot.title=element_text(hjust=0.5,face="bold"))+
  theme(plot.background = element_rect(fill="lightcoral"))+
  theme(panel.background = element_rect(fill="peachpuff"))

mp<-matches%>%group_by(season)%>%summarize(match_played=n())
median(mp$match_played)

###maximum number of matches was played in the IPL 2013 season 
###median of matches played is 60.


#IPL title winner in each season vs the venue
md<-group_by(matches,season)%>%
  summarise(date=max(date))
md

ven<-c()
twin<-c()
title_winner<-for(i in 1:nrow(matches)){
  for(j in 1:nrow(md)){
    if(matches$date[i]==md$date[j]){twin[j]<-matches$winner[i]
    ven[j]<-matches$venue[i]}}}

data<-add_column(md,twin,ven);data

ggplot(data,aes(x=factor(season),y=ven,col=twin))+
  geom_point(position="identity",pch=16,cex=4.2)+
  labs(x="Year",y="Venue",title="IPL title winner each season")+
  theme(plot.title=element_text(hjust=0.5,face="bold"))+
  geom_text(fontface="bold",position=position_jitter(width=0.5),aes(label=twin))+
  theme(plot.background = element_rect(fill="aliceblue"))+
  theme(panel.background = element_rect(fill="aquamarine"))



#Most successful IPL title winners
data%>%group_by(twin)%>%summarize(total_title_won=n())%>%
  ggplot(aes(x=twin,y=total_title_won))+
  geom_bar(stat="identity",aes(fill=twin))+
  labs(x="Team",y="Number of times title won",title="Most Successful IPL Title Winner",fill="Team")+
  geom_text(fontface="bold",aes(label=total_title_won))+
  theme(plot.title=element_text(hjust=0.5,face="bold"))+
  theme(plot.background = element_rect(fill="lightpink"))+
  theme(panel.background = element_rect(fill="linen"))

###most successful IPL title winner is Mumbai Indians with number of wins equals to 4


#most successful teams according to the number of wins
group_by(matches,winner)%>% summarize(wins= n())%>% 
  ggplot(aes(x=winner,y=wins,fill=winner))+
  geom_bar(stat="identity")+
  coord_flip()+
  scale_y_continuous()+
  geom_text(aes(label=wins),hjust=1.25)+
  ggtitle("Most number of wins in the IPL")+
  labs(x="Team",y="Total matches won",fill="Team")+
  theme_bw()+
  theme(plot.background = element_rect(fill="lightblue"))+
  theme(panel.background = element_rect(fill="linen"))

###Mumbai India also has most number of wins(109) in the IPL from season 2008-2019


#most man of the match award winners
matches %>% 
  group_by(player_of_match)%>%
  summarize(awards=n())%>%
  top_n(10)%>%
  ggplot(aes(x=reorder(player_of_match,awards),y=awards))+
  geom_bar(stat="identity",col="yellow",fill="khaki")+
  coord_flip()+
  ggtitle("Most number of Man Of the Match Awards")+
  labs(x="Team",y="Total MOM awards won",fill="Player")+
  geom_text(aes(label=awards),hjust=1.25)+
  theme(plot.background = element_rect(fill="lightgrey"))+
  theme(panel.background = element_rect(fill="linen"))

###Chris Gayle has won the most number of Man of the Match Awards(21) followed by AB de Villiers


#most played venue
matches%>%
  group_by(venue)%>%
  summarize(tmatches=n())%>%
  top_n(15)%>%
  ggplot(aes(x=reorder(venue,tmatches),y=tmatches,fill=venue))+
  geom_bar(stat= "identity")+
  coord_flip()+
  scale_y_continuous("Total Matches Played")+
  geom_text(aes(label=tmatches),hjust=1.25)+
  ggtitle("Top 10 Venues")+
  labs(x="Venue",y="Total matches played",fill="Venue")+
  geom_text(aes(label=tmatches),hjust=1.25)+
  theme(plot.background = element_rect(fill="lightgoldenrod"))+
  theme(panel.background = element_rect(fill="linen"))

###Most played venue=== Eden Gardens => 77 matches 


#most number of times toss won and the toss decision
a<-matches%>%
  group_by(toss_winner,toss_decision)%>%
  summarize(total_toss_win=n())%>%
  ggplot(aes(x=toss_winner,y=total_toss_win,fill=toss_decision))+
  geom_bar(stat="identity")+
  coord_flip()+
  scale_fill_jama()+
  scale_y_continuous()+
  geom_text(aes(label=total_toss_win),hjust=0.25,position=position_stack(vjust=0.5))+
  ggtitle("Teams Vs the number of times toss won")+
  labs(x="Team",y="Total toss won")+
  theme_light()+
  theme(plot.background = element_rect(fill="lightgoldenrod"))+
  theme(panel.background = element_rect(fill="mistyrose"))

b<-matches%>%group_by(toss_winner)%>%
  summarize(total_toss_win=n())%>%
  ggplot(aes(x=toss_winner,y=total_toss_win))+
  geom_bar(stat="identity",position="dodge",fill="aquamarine")+
  coord_flip()+
  scale_fill_distiller()+
  scale_y_continuous()+
  geom_text(aes(label=total_toss_win),hjust=0.25,position=position_stack(vjust=0.5))+
  ggtitle("Teams Vs the number of times toss won")+
  labs(x="Team",y="Total toss won")+
  theme_light()+
  theme(plot.background = element_rect(fill="lightgoldenrod"))+
  theme(panel.background = element_rect(fill="mistyrose"))

plot_grid(a,b)

###Mumbai Indians has won most number of tosses(98) 
#and all the teams, after winning the toss, has decided to field first more than batting first 


#number of teams participated each season
d1<-data.frame(matches$season,matches$team1)
d2<-data.frame(matches$season,matches$team2)
colnames(d2)<-colnames(d1)
cbar<-rbind(d1,d2)

cbar%>%group_by(matches.season)%>%
  summarize(teamspart=length(unique(matches.team1)))%>%
  ggplot(aes(x=factor(matches.season),y=teamspart))+
  geom_bar(stat="identity",fill="snow")+
  scale_y_continuous()+
  geom_text(aes(label=teamspart),hjust=0.25,position=position_stack(vjust=0.5))+
  ggtitle("Number of teams participated each season")+
  labs(x="Season",y="Total teams participated")+
  theme_light()+
  theme(plot.background = element_rect(fill="wheat"))+
  theme(panel.background = element_rect(fill="tomato1"))

###10 teams had participated in the year 2011
###median of the number of teams participated each year is 8


##########################################################################################################

#number of matches played and number of matches won

c<-teams%>%select(team,total_matches_played,matches_won)%>%
  pivot_longer(.,cols=c(total_matches_played,matches_won),names_to="var",values_to="value")%>%
  ggplot(aes(team,value,fill=var))+
  geom_bar(stat="identity",position="dodge")+
  coord_flip()+
  geom_text(aes(label=value),hjust=0.05,position=position_dodge(width=0.9))+
  ggtitle("Number of matches played and matches won by each team in IPL")+
  labs(x="Team",y="Total matches won/played",fill="Bar colors")+
  scale_fill_ordinal()+
  theme_update()+
  theme(plot.background = element_rect(fill="lightgreen"))+
  theme(panel.background = element_rect(fill="lightblue"))


d<-teams%>%select(team,match_won_percentage)%>%
  ggplot(aes(sort(team),match_won_percentage))+
  geom_bar(stat="identity",fill="green",col="darkgreen")+
  coord_flip()+
  ggtitle("Percent of matches won by each team based on toss decision")+
  geom_text(aes(label=match_won_percentage*100),hjust=1.1)+
  labs(x="Team",y="Match winning percent")+
  theme_update()+
  scale_y_continuous(labels=scales::percent)+
  theme(plot.background = element_rect(fill="lightblue"))+
  theme(panel.background = element_rect(fill="lightgreen"))

plot_grid(c,d)

###though the match winning percentage of Delhi Capitals is 62.5%(maximum)
###but they only had played 16 matches
###so Chennai Capitals' match winning percentage 60.98 is maximum among all


#Percent of matches won by each team based on their toss decision.
teams%>%select(team,X.twfmw,X.twbmw)%>%
  pivot_longer(.,cols=c(X.twbmw,X.twfmw),names_to="var",values_to="value")%>%
  ggplot(aes(sort(team),value*100,fill=var))+
  geom_bar(stat="identity",position="dodge")+
  coord_flip()+
  ggtitle("Percent of matches won by each team based on toss decision")+
  labs(x="Team",y="Match winning percent",fill="Toss Win ->Bat/Field ->Match Win")+
  scale_y_continuous()+
  theme_update()+
  scale_fill_aaas()+
  theme(plot.background = element_rect(fill="lightblue"))+
  theme(panel.background = element_rect(fill="lightpink"))

###teams such as Kochi Tuskers Kerala and Rising Pune Supergiants had played only few matches(14 and 29 resp.)
###and their toss decisions was same each time(fielding first) so there is no blue bar for them
###most teams match win percentage by fielding first is greater than match win percentage by batting first.


########################################################################################################
del<-read.csv(file.choose(),header=T)
view(del)

#Top 10 batsman with most number of sixes
del %>% 
  group_by(batsman)%>%
  filter(batsman_runs==6)%>%
  summarize(sixes=n())%>%
  top_n(10)%>%
  ggplot(aes(x=reorder(batsman,sixes),y=sixes))+
  geom_bar(stat="identity",fill="violetred")+
  coord_flip()+
  ggtitle("Top 10 Batsman with Most Number of 6s in IPL")+
  geom_text(aes(label=sixes),hjust=1.25)+
  labs(x="Batsman",y="Sixes")+
  theme_classic()+
  theme(plot.background = element_rect(fill="lightpink"))+
  theme(panel.background = element_rect(fill="violet"))

###Chris gale has hit most number of sixes(327)


#Top 10 teams which scored the maximum runs
maxrun<-del%>%group_by(match_id,batting_team)%>%
  summarize(total_run=sum(total_runs))
maxrun<-arrange(maxrun,-total_run) 
maxrun<-maxrun[1:10,]
maxrun

agnst<-c()
for (i in 1:nrow(maxrun)){
  for(j in 1:nrow(del)){
  if(maxrun$match_id[i]==del$match_id[j]){
    agnst[i]=del$bowling_team[j]
    break}
    }}
print(agnst)
maxrun<-cbind(maxrun,as.data.frame(agnst))
maxrun


maxrun%>%mutate(agnst=factor(agnst,levels=unique(agnst)))%>%
  ggplot(aes(interaction(batting_team,agnst,sep=" against "),total_run,fill=batting_team))+
  geom_bar(stat="identity" )+
  coord_flip()+
  ggtitle("Top 10 teams which has scored maximum runs in IPL")+
  geom_text(aes(label=total_run),hjust=1.25)+
  labs(x="Batting team against Bowling team", y="Runs scored")+
  theme_dark()+
  theme(plot.background = element_rect(fill="grey"))+
  theme(panel.background = element_rect(fill="black"))

###Royal Challengers Bengalore had scored maximum runs against Pune Warriors


#most run scored by a batsman
maxbr<-del%>%group_by(batsman,match_id)%>%summarize(run=sum(batsman_runs))
maxbr<-arrange(maxbr,-maxbr$run) 
maxbr<-maxbr[1:10,]
maxbr

against<-c()
for (i in 1:nrow(maxbr)){
  for(j in 1:nrow(del)){
    if(maxbr$match_id[i]==del$match_id[j]){
      against[i]=del$bowling_team[j]
      break}
    against[i]=del$bowling_team[j]
  }}
print(against)
maxbr<-cbind(maxbr,as.data.frame(against))
maxbr

ggplot(maxbr,aes(interaction(batsman,against,sep=" runs against "),run,fill=batsman))+
  geom_bar(stat="identity" )+
  coord_flip()+
  scale_fill_hue()+
  ggtitle("Top 10 batsman who has scored maximum number of runs in a match in IPL")+
  geom_text(aes(label=run),hjust=1.25)+
  labs(x="Batsman agaisnt the bowling team", y="Runs scored")+
  theme_half_open()+
  theme(plot.background = element_rect(fill="linen"))


###Chris Gayle had scored maximum runs(175) against Pune warriors


#Most wickets takers in IPL
plydis<-del%>%group_by(bowler)%>%
filter(is.na(player_dismissed)=="FALSE")%>%
  summarize(pdis=length(player_dismissed))%>%
  arrange(-pdis)
plydis<-plydis[1:10,]
ggplot(plydis,aes(reorder(bowler,pdis),pdis,fill=pdis))+
  geom_bar(stat="identity")+
  theme_classic2()+
  labs(x="Bowler",y="Number of player dismissed",title="Most Wickets takers in IPL",fill="Number")+
  geom_text(aes(label=pdis),hjust=0.5,vjust=1.9)+
  theme(plot.background = element_rect(fill="skyblue"))+
  theme(panel.background = element_rect(fill="snow"))

###SL Malinga has taken most number of wickets in the IPL


#Most stumps taken
st<-del%>%
  filter(is.na(player_dismissed)=="FALSE")%>%
  filter(dismissal_kind=="stumped")%>%
  group_by(fielder)%>%
  summarize(stumpd=length(player_dismissed))%>%
  arrange(-stumpd)
st<-st[1:10,]
st
ggplot(st,aes(reorder(fielder,stumpd),stumpd))+
  geom_bar(stat="identity",fill="red4")+
  theme_classic2()+
  labs(x="Fielder",y="Number of player dismissed",title="Most Stumped takers in IPL")+
  geom_text(aes(label=stumpd),hjust=0.5,vjust=1.9)+
  theme(plot.background = element_rect(fill="tomato"))+
  theme(panel.background = element_rect(fill="peru"))

###MS Dhoni has taken most number of stumps(38) followed by RV Uthappa(32)


#Most caughts taken
ct<-del%>%
  filter(is.na(player_dismissed)=="FALSE")%>%
  filter(dismissal_kind=="caught")%>%
  group_by(fielder)%>%
  summarize(ncaughts=length(player_dismissed))%>%
  arrange(-ncaughts)
ct<-ct[1:10,]
ct
ggplot(ct,aes(reorder(fielder,ncaughts),ncaughts))+
  geom_bar(stat="identity",fill="blue4")+
  theme_classic2()+
  labs(x="Fielder",y="Number of player dismissed",title="Most Caught takers in IPL")+
  geom_text(aes(label=ncaughts),hjust=0.5,vjust=1.9)+
  theme(plot.background = element_rect(fill="powderblue"))+
  theme(panel.background = element_rect(fill="linen"))

###KD Karthik had taken maximum number of catches in the IL followed by SK Raina


#number of centuries hit
ch<-del%>%group_by(match_id,batsman)%>%
  summarize(runs=sum(batsman_runs))%>%
  filter(runs>=100)
ch

ch%>%group_by(batsman)%>%summarize(noc=n())%>%
  top_n(5)%>%
  ggplot(aes(reorder(batsman,noc),noc))+
  geom_bar(stat="identity",fill="orangered2")+
  theme_classic2()+
  labs(x="Fielder",y="Number of player dismissed",title="Most Century scored by a batsman in IPL")+
  theme(plot.background = element_rect(fill="peachpuff"))+
  theme(panel.background = element_rect(fill="linen"))

###CH Gayle has scored maximum number of centuries in the IPL history.






