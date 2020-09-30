#======================#
#### Data Cleansing ####
#======================#

# Date    : 2017/05/08
# Note    : This code contains preprocessing koweps panel data. 
#           Mostly correcting logical errors and dropping missing values.
# Status  : Passed
rm(list=ls())
# Load Dataset

# Select Relevant variables : We only consider paid workers
data11=dataRaw_hp[,c("h_pid","year","p02_8","p02_9","h_g8","h_g3",
                     "h_inc2_2","h_pers_income1","h_inc3_2","h_pers_income2", "p02_6", "p02_7",
                     "h_g4","h_med2","p02_4",
                     "h_eco9","h_eco10","h_eco4","h_g6","h_g7",
                     "np06_4", "np06_5", #일자리 관련
                     "np06_10","np06_11", #일자리A
                     "np06_13","np06_14",
                     "np06_16","np06_17",
                     "np06_19","np06_20",
                     "np06_22","np06_23")] 
#,#                    "h_pers_income3","h_pers_income4","h_pers_income5")]
table(dataRaw_hp$h_eco9)

head(data11,40)
head(cbind(dataRaw_hp$p02_4,dataRaw_hp$year),100)

# Remove data with no income data : only salary worker
data11.clean = data11[!(is.na(data11$h_inc2_2) &is.na(data11$h_pers_income1) & is.na(data11$h_inc3_2) & is.na(data11$h_pers_income2)),]
data11.clean = data11.clean[!((data11.clean$h_inc2_2) == 0  & (data11.clean$h_pers_income1)==0 & 
                                (data11.clean$h_inc3_2)==0 & (data11.clean$h_pers_income2)==0),]
head(data11.clean)
#data11.clean[(!is.na(data11.clean$h_inc2_2) & !is.na(data11.clean$h_pers_income1) & !is.na(data11.clean$h_inc3_2) & !is.na(data11.clean$h_pers_income2)),]
data11.clean = data11.clean[!((data11.clean$h_pers_income1==0) & (data11.clean$h_pers_income2==0) & !(is.na(data11.clean$p02_8) & is.na(data11.clean$p02_9))), ]

# fill-in NA to 0 for counterparts
data11.clean$h_inc2_2[data11.clean$h_pers_income1 == 0] = 0
data11.clean$h_inc3_2[data11.clean$h_pers_income2 == 0] = 0
data11.clean[,7:10][is.na(data11.clean[,7:10])] = 0

# Impute working months if observed p02_6 but not on either hinc2_2 or hinc3_2 then, use p02_6 for proper month of income.
data11.clean[!is.na(data11.clean$p02_6) & (data11.clean$h_inc2_2==0) & (data11.clean$h_inc3_2==0) & (data11.clean$h_pers_income1==0),]$h_inc3_2 =
  data11.clean[!is.na(data11.clean$p02_6) & (data11.clean$h_inc2_2==0) & (data11.clean$h_inc3_2==0) & (data11.clean$h_pers_income1==0),]$p02_6

data11.clean[!is.na(data11.clean$p02_6) & (data11.clean$h_inc2_2==0) & (data11.clean$h_inc3_2==0) & (data11.clean$h_pers_income2==0),]$h_inc2_2 =
  data11.clean[!is.na(data11.clean$p02_6) & (data11.clean$h_inc2_2==0) & (data11.clean$h_inc3_2==0) & (data11.clean$h_pers_income2==0),]$p02_6

data11.clean[is.na(data11.clean$p02_6) & (data11.clean$h_inc2_2==0),]$p02_6 = 
  data11.clean[is.na(data11.clean$p02_6) & (data11.clean$h_inc2_2==0),]$h_inc3_2

data11.clean[is.na(data11.clean$p02_6) & (data11.clean$h_inc3_2==0),]$p02_6 = 
  data11.clean[is.na(data11.clean$p02_6) & (data11.clean$h_inc3_2==0),]$h_inc2_2

# Remove total working months(p02_6) are 0
data11.clean = data11.clean[data11.clean$p02_6!=0,]
#impute end above

### Remove where id's data11.clean$year is observed once.
sum(table(data11.clean$h_pid)==1) # n = 2720 id is observed only once

table(data11.clean$h_pid)[2]==1
data11.clean=data11.clean[(!is.na(data11.clean$h_pid)),] 

# Leave only observed more than one times   
idtab = table(data11.clean$h_pid)
id.notone = names(idtab)[idtab != 1]
data11.clean = data11.clean[data11.clean$h_pid %in% as.integer(id.notone),]

length(data11.clean$h_pid) #total n=59865 left
length(unique(data11.clean$h_pid)) # id = 10372 left

head(data11.clean)
##########################################################################################
################################# 2017.05.07 end #########################################
##########################################################################################

#data11.clean[pmax(data11.clean$h_inc2_2,data11.clean$h_inc3_2) > data11.clean$p02_6, ]

data11=data11.clean
data11$h_g8=ifelse(data11.clean$h_g8==0,0,1)
data11.clean$h_g8=ifelse(data11.clean$h_g8==0,0,1)
table(data11.clean$h_g8) # 비장애인과 장애인 비율은 56734 : 3131 임 
head(data11.clean,50) 

data11.clean$p02_6 = ifelse(data11.clean$h_inc2_2 == 12 | data11.clean$h_inc3_2 == 12, 12, data11.clean$p02_6)
# wage변수 생성: 상용소득과 임시소득의 합
data11.clean$wage=(data11.clean$h_pers_income1*data11.clean$h_inc2_2 +
                     data11.clean$h_pers_income2*data11.clean$h_inc3_2) / (data11.clean$p02_6)
head(data11.clean,2)

# 필요없는 변수 제거
head(data11.clean)
welfare =  data11.clean[,c("h_pid","year","h_g3","h_g8","h_g4","h_med2","p02_4","h_eco10","h_eco4","h_g6","h_g7","h_eco9", "wage")]
colnames(welfare)=c("ID","year","sex","handicap","birth","health","exp","scale","status","eduyear","edustatus","job","wage")
head(welfare)

#newstatus = 종사상 지위인 status 변수를 상용직은 1, 임시직과일용직은 2로 바꾼 변수
welfare=welfare[welfare$status %in% c(1,2,3),]
welfare$newstatus=ifelse(welfare$status==1,1,2)

with(welfare,{
  interaction.plot(x.factor=year, trace.factor=ID, response=wage,
                   xlab="YEAR",ylab="WAGE",legend=F,
                   col=c(1:10372), las=1)
})
max(welfare$wage)
sum(welfare$wage==999999) 
#wage에 이상치 삭제할것
welfare=welfare[welfare$wage!=999999,]

max(welfare$wage) #그래도 여전히 82000 과같이 큰값들은 있음 


welfare[which(welfare$wage==max(welfare$wage)),]
welfare[welfare$ID==49901,] # data error
data11.clean$h_pers_income1[data11.clean$h_pid==49901 & data11.clean$year==2009] = 8200
welfare$wage[welfare$ID==49901 & welfare$year==2009] = 8200

# OUTLIER ID = 92901 : high wage ## WARNING
welfare = welfare[welfare$ID!=92901,]

## Data Cleanse : if wage > 40000 then assume as wrong input.
threshold.clean = 40000
length(which(welfare$wage>threshold.clean))
welfare[welfare$wage>threshold.clean,"wage"] = welfare[welfare$wage>threshold.clean,"wage"]/10

## Data Cleanse : Check peaks on 2009 and 2013
welfare[welfare$wage>20000 & welfare$year==2009, "wage"] = welfare[welfare$wage>20000 & welfare$year==2009, "wage"]/10

welfare[welfare$wage>20000 & welfare$year==2013, ]
welfare[rownames(welfare) %in% c("3359","16086","24600","33467","35650","65650","99221","128361"), "wage"] =
  welfare[rownames(welfare) %in% c("3359","16086","24600","33467","35650","65650","99221","128361"), "wage"]/10
# NOT 84701

# DELETE case 65649 : ambiguous
welfare = welfare[rownames(welfare)!="65649",]

## DATA Cleanse : Peak value
welfare[welfare$wage==max(welfare[welfare$year=="2008","wage"]),"wage"] =
  welfare[welfare$wage==max(welfare[welfare$year=="2008","wage"]),"wage"]/10

length(welfare$ID)

## Check whether income is 0
noincomeList = unique(welfare[welfare$wage==0,]$ID)
welfare[welfare$ID==noincomeList[9],]
head(welfare)

# newscale = scale(사업장규모) 변수 범주화
welfare$newscale=ifelse(0<welfare$scale&welfare$scale<5,0,welfare$scale)
welfare$newscale=ifelse(4<welfare$scale&welfare$scale<7,1,welfare$newscale)
welfare$newscale=ifelse(welfare$scale==7,2,welfare$newscale)
welfare$newscale=ifelse(7<welfare$scale&welfare$scale<11,3 ,welfare$newscale)
welfare=welfare[welfare$newscale!=11,]
welfare=welfare[welfare$newscale!=99,]

#newedustatus = edustatus (교육수준)
welfare$eduyear.new=ifelse(welfare$edustatus%in%c(1,2,3) & welfare$eduyear==3, 3,welfare$eduyear)
welfare$eduyear.new=ifelse(welfare$edustatus%in%c(4,5) & welfare$eduyear==3, 6,welfare$eduyear.new)
welfare$eduyear.new=ifelse(welfare$edustatus%in%c(1,2,3) & welfare$eduyear==4, 7.5,welfare$eduyear.new)
welfare$eduyear.new=ifelse(welfare$edustatus%in%c(4,5) & welfare$eduyear==4, 9,welfare$eduyear.new)
welfare$eduyear.new=ifelse(welfare$edustatus%in%c(1,2,3) & welfare$eduyear==5, 10.5,welfare$eduyear.new)
welfare$eduyear.new=ifelse(welfare$edustatus%in%c(4,5) & welfare$eduyear==5, 12,welfare$eduyear.new)
welfare$eduyear.new=ifelse(welfare$edustatus%in%c(4,5) & welfare$eduyear==6, 14,welfare$eduyear.new)
welfare$eduyear.new=ifelse(welfare$edustatus%in%c(0:3) & welfare$eduyear==7,14,welfare$eduyear.new)
welfare$eduyear.new=ifelse(welfare$edustatus%in%c(4,5) & welfare$eduyear==7, 16,welfare$eduyear.new)
welfare$eduyear.new=ifelse(welfare$eduyear%in%c(8,9), 18,welfare$eduyear.new)
#무학=0
welfare$eduyear.new=ifelse(welfare$eduyear==2, 0,welfare$eduyear.new)
table(welfare$eduyear.new)

#health.new = health 변수 역코딩
table(welfare$health)
welfare=welfare[!welfare$health==9,]
table(welfare$health)

welfare$health.new=ifelse(welfare$health==1, 5,welfare$health)
welfare$health.new=ifelse(welfare$health==2, 4,welfare$health.new)
welfare$health.new=ifelse(welfare$health==4, 2,welfare$health.new)
welfare$health.new=ifelse(welfare$health==5, 1,welfare$health.new)
table(welfare$health.new)

# age 변수 생성
welfare$age=welfare$year-welfare$birth

# job.new = job 가 비전문 생산직 = 0 / 전문 사무직 = 1
welfare$job.new=ifelse(welfare$job<400,1,welfare$job)
welfare$job.new=ifelse(400<welfare$job & welfare$job<1013, 0, welfare$job.new)
welfare=welfare[welfare$job.new!=9999,]

###########################################################################
###########################################################################
###########################################################################



# Monthly income final
#data11.clean$h_inc_mon = (data11.clean$h_inc2_2*data11.clean$h_pers_income1 + data11.clean$h_inc3_2*data11.clean$h_pers_income2)/12



# Only use after 4th study
data11.clean = data11.clean[data11.clean$year>2006,]


sum(table(data11$h_g8)[-1])
head(data11,50)

temp = data11[!(is.na(data11$p02_8) & is.na(data11$p02_9)),]
temp = temp[!is.na(temp$p01_6),]
all.equal(dataRaw_hp$h_pers_income1, dataRaw_hp$h_inc2)
cbind(dataRaw_hp$h_pers_income1, dataRaw_hp$h_inc2)

length(data11$h_pid)
sum(!is.na(dataRaw_hp$p02_8aq2))
sum(!is.na(dataRaw_hp$h_g3))

# 각 연도별로 sample이 몇개 남아있는지?
table(data11$year) 
table(data11$p02_8aq2)

#일한날의 시간당 임금이 결측인 관측치 제거
data11=data11[data11$p02_8aq2!=9999,] 
temp = table(data11$h_pid)
length(which(temp!=1))

# 장애/비장애인 비율
table(data11$h_g8)
sum(table(data11$h_g8)[-1])

# id 