#################################### Load data ###########################################
setwd("~/Mobile Game")

# table 1: users
users <- read.csv("users_copy.csv")

# table 2: iaps (in-app-purchases)
# "rev" is the target variable, the users paid to buy 'prod_type'
iaps <- read.csv("iapscopy.csv")
# 按照时间来排列，有重复的用户

# table 3: sessions
sessions <- read.csv("sessions_copy.csv")
# last session termination type是NA

# table 4: spendevents
spendevents <- read.csv("spendevents_copy.csv")
# 按照时间来排列，有重复的用户

# library packages
library(dplyr)
library(lubridate)
library(stringr)
library(ggplot2)

library(caTools)

########################### check missing values and bad values ############################
# 1. users
str(users)
# country 有bad value，需要后期改正
colSums(is.na(users))
summary(users)
# country 有5个missing value, 并且7个blank values
prop.table(table(users$country))
# country的bad value有_C, _P, _U, 15, 后期需要改成NA

# 2. iaps
str(iaps)
# udid只有1526，说明有很多重复用户
# ts有5592，证明ts有重复的
colSums(is.na(iaps))
# 没有missing value
summary(iaps)
# rev有7000，可能是个outlier，后期要处理一下

# 3. sessions
str(sessions)
# udid只有22544，说明有很多重复的用户
# ts有90719，说明ts也有重复的
colSums(is.na(sessions))
# last session termination type一整列都是missing value，后期需要一整列删掉
summary(sessions)
# session num有1919，可能是个outlier，后期要处理一下

# 4. spendevents
str(spendevents)
# udid有18017，说明有很多重复用户
# ts有32832，说明有重复ts
colSums(is.na(spendevents))
# 没有missing value
summary(spendevents)
# chapter max是89，可能是个outlier，后期要处理一下
# amount min是-999999，可能是个outlier，后期需要处理一下

############################## data pre-processing #####################################
# 1. users
# 把country 5个missing values, 7个blank values 和 bad values (_C, _P, _U, 15) 改成NA
users$country = factor(ifelse(users$country == "", "NA", as.character(users$country)))
users[is.na(users)] <- "NA"
col = c("_C","_P","_U","15")
users$country = factor(ifelse(users$country %in% col, "NA", as.character(users$country)))

# change data type: install date
users$install_date <- mdy(as.character(users$install_date))

# split version
hw_ver = as.data.frame(str_split_fixed(users$hw_ver,',',2))
users['hw_ever_type'] = hw_ver[,1]

# 2. iaps
plot(iaps$rev)
# 有两个7000，三个3000-4000之间(3个3500)
iaps %>% select(rev) %>% filter(rev > 3000) %>% arrange(rev) %>% head
# 用mean代替5个outlier？
# iaps$rev = ifelse(iaps$rev > 3000, mean(iaps$rev),iaps$rev )

# change data type: ts
iaps$ts <- mdy_hm(as.character(iaps$ts))
# change data type" date
iaps$date <- mdy(as.character(iaps$date))

# split ts in year, month, day, and time
iaps_ts = as.data.frame(str_split_fixed(iaps$ts,' ',2))
iaps['iaps_ts'] = iaps_ts[,2]

iaps_tss = as.data.frame(str_split_fixed(iaps$iaps_ts,':',3))
iaps['iaps_ts_h'] = iaps_tss[,1]
# 这就可以知道用户都喜欢在什么时间段购买

iaps$iaps_ts = NULL

iaps_md = as.data.frame(str_split_fixed(iaps_ts$V1,'-',3))
iaps['iaps_ts_m'] = iaps_md[,2]
iaps['iaps_ts_d'] = iaps_md[,3]
# 这就可以知道用户都喜欢在什么月份和日子购买

# split prod name
prod_name = as.data.frame(str_split_fixed(iaps$prod_name,'_',4))
iaps['prod_name_1'] = prod_name[,1]
iaps['prod_name_2'] = prod_name[,2]
iaps['prod_name_3'] = prod_name[,3]
iaps$prod_names = str_c(iaps$prod_name_1,iaps$prod_name_2,iaps$prod_name_3,sep='_')
iaps$prod_names = as.factor(iaps$prod_names)

iaps$prod_name_1 = NULL
iaps$prod_name_2 = NULL
iaps$prod_name_3 = NULL


# 3. sessions
# 删去last session termination type整个column
sessions$last_session_termination_type = NULL
# plot(sessions$session_num)

# change data type: ts
sessions$ts <- mdy_hm(as.character(sessions$ts))
# change data type" date
sessions$date <- mdy(as.character(sessions$date))

# split ts in year, month, day, and time
sessions_ts = as.data.frame(str_split_fixed(sessions$ts,' ',2))
sessions['sessions_ts'] = sessions_ts[,2]

sessions_date = as.data.frame(str_split_fixed(sessions$date,'-',3))
sessions['sessions_m'] = sessions_date[,2]

sessions_tss = as.data.frame(str_split_fixed(sessions$sessions_ts,':',3))
sessions['sessions_ts_h'] = sessions_tss[,1]


# 4. spendevents
plot(spendevents$chapter)
# chapter有一个是89，跟其他很不一样，用mean替换掉
spendevents$chapter = ifelse(spendevents$chapter > 80, mean(spendevents$chapter), spendevents$chapter)
plot(spendevents$amount)

# change data type: ts
spendevents$ts <- mdy_hm(as.character(spendevents$ts))
# change data type" date
spendevents$date <- mdy(as.character(spendevents$date))

# split ts in year, month, day, and time
spendevents_ts = as.data.frame(str_split_fixed(spendevents$ts,' ',2))
spendevents['spendevents_ts'] = spendevents_ts[,2]

spendevents_tss = as.data.frame(str_split_fixed(spendevents$spendevents_ts,':',3))
spendevents['spendevents_ts_h'] = spendevents_tss[,1]

spendevents$story_chapter = str_c(spendevents$story, spendevents$chapter, sep = '_')
spendevents$story_chapter = as.factor(spendevents$story_chapter)


########################## Feature Engineering #########################################

iaps_udid = distinct(iaps,udid)
converted = iaps_udid$udid
spendevents_udid = distinct(spendevents, udid)
potential_users = setdiff(spendevents_udid$udid,iaps_udid$udid)

users$status = factor(ifelse(users$udid %in% converted, 'Converted',
                      ifelse(users$udid %in% potential_users, 'Potential', 'No Converted')))

# converted users only have 6.76%

# 1. DAU 日活跃用户数
DAU = sessions %>% group_by(date) %>% distinct(udid) %>% count()
plot(DAU$date,DAU$n)
# 3/1 - 3/7 之间dau上升很快，但是之后就一直下降

# MAU def
Month <- function(date){
  if (date >= '2016-03-01' & date <= '2016-03-31'){
    return('3')
  }else if (date >= '2016-04-01' & date <= '2016-04-30'){
    return('4')
  }else if (date >= '2016-05-01' & date <= '2016-05-31') {
    return('5')
  }
}

DAU$MAU_month <- sapply(DAU$date, Month)
DAU$MAU_month <- as.factor(DAU$MAU_month)


# 2. MAU 月活跃用户数
# split DAU's date in year, month, day
MAU = sessions %>% group_by(sessions_m) %>% distinct(udid) %>% count()
plot(MAU$sessions_m,MAU$n)

MAU$MAU_month = factor(ifelse(MAU$sessions_m == '03', '3',
                              ifelse(MAU$sessions_m == '04','4','5')))

MAU$sessions_m = NULL

Active_Users = left_join(DAU,MAU, by = 'MAU_month')
Active_Users = Active_Users %>% group_by(MAU_month) %>% mutate(stickness = DAU_n/n)
plot(Active_Users$date,Active_Users$stickness)

# 用户黏度每个月都在下降，除了三月份第一个星期在上涨之外。说明那个星期可能在搞活动。整体趋势与dau趋势符合。

# 3. 留存率
# 月留存率(Cohort Analysis)

# 创建活跃月份字段
sessions$active_period = format(sessions$date,'%Y-%m')

# 创建用户首次活跃字段
CohortGroup = sessions %>% group_by(udid) %>% summarize(CohortGroup = min(date))
CohortGroup$CohortGroup <- CohortGroup$CohortGroup %>% format('%Y-%m')
CohortGroup$CohortGroup <- as.factor(CohortGroup$CohortGroup)
sessions <- sessions %>% left_join(CohortGroup, by = 'udid') # 将首购日期与原始活跃合并对齐

# 分组(按照首次活跃日期，活跃日期)计算总用户数(用户id要去重)
chorts <- sessions %>% group_by(CohortGroup, active_period) %>%
  summarize(udid = n_distinct(udid)) %>% rename(TotalUsers = udid)

# 创建一个名为CohortPeriod的数据列，代表活跃月份与首次月份之间相差的期数
chorts <- chorts %>% arrange(CohortGroup, active_period) %>%
  group_by(CohortGroup) %>%
  mutate(CohortPeriod = row_number())

# 计算当月活跃用户数
cohort_group_size <- chorts %>% 
  filter(CohortPeriod == 1) %>%
  select(CohortGroup, active_period, TotalUsers)

user_retention <- chorts %>% 
  select(CohortGroup, CohortPeriod, TotalUsers) %>%
  spread(CohortGroup, TotalUsers) # 长表转换为宽表

# 将具体用户数转换成占基准月份比率
user_retention[,-1] <- user_retention[,-1] %>% t() %>% `/`(cohort_group_size$TotalUsers) %>% t() %>% as.data.frame()

# 宽表转为长表
# install.packages("reshape2")
library(reshape2)
user_retention1 <- user_retention %>% select(1:2) %>%
  melt(
    id.vars = 'CohortPeriod',
    variable.name = 'CohortGroup',
    value.name = 'TotalUsers'
  )

# 留存图
ggplot(user_retention1,aes(CohortPeriod,TotalUsers)) +
  geom_line(aes(group = CohortGroup,colour = CohortGroup)) +
  scale_x_continuous(breaks = 1:15) +
  scale_colour_brewer(type = 'div')

# 3月首次活跃的用户，到了4月只剩下22%，到5月就只剩5%了。说明游戏的质量和保留用户的能力有待提高。

# 4. 抓出potential player

# 计算每一个users的sessions 总次数
sessions_count = sessions %>% group_by(udid) %>% count()
sessions_count = rename(sessions_count, sessions_n = n)

# 抓出user里的potential users
potential_users = users[which(users$status == 'Potential'),]

# potential users' sessions count
potential_users_sessions = sessions_count[sessions_count$udid %in% potential_users$udid,]

# 把sessions count 与 users information 合并
potential_users = left_join(potential_users,potential_users_sessions, by = 'udid')

# potential users' sessions whole information
potential_users_sessions_total = sessions[sessions$udid %in% potential_users$udid,]

# sessions last date
potential_users_sessions_LastDate = potential_users_sessions_total %>% group_by(udid) %>% arrange(date) %>% filter(row_number()==n()) %>% select(udid, date)
potential_users_sessions_LastDate = rename(potential_users_sessions_LastDate, LastDate = date)
potential_users = left_join(potential_users, potential_users_sessions_LastDate, by = 'udid')

# sessions first date
potential_users_sessions_FirstDate = potential_users_sessions_total %>% group_by(udid) %>% arrange(date) %>% filter(row_number()==1) %>% select(udid, date)
potential_users_sessions_FirstDate = rename(potential_users_sessions_FirstDate, FirstDate = date)
potential_users = left_join(potential_users, potential_users_sessions_FirstDate, by = 'udid')

# 抓出spendevents里的potential users
potential_users_spendevents = spendevents[spendevents$udid %in% potential_users$udid,]

# 计算出他们的total amount
potential_users_spendevents_count = potential_users_spendevents %>% group_by(udid) %>% summarise(total_amount = sum(amount))

# 把total amount 与 users information 合并
potential_users = left_join(potential_users, potential_users_spendevents_count, by = 'udid')

# 计算potential users的spend的次数
potential_users_spendevents_spendcount = potential_users_spendevents %>% group_by(udid) %>% count()
potential_users_spendevents_spendcount = rename(potential_users_spendevents_spendcount, SpendCount = n)

# 把spend count 与 users information 合并
potential_users = left_join(potential_users, potential_users_spendevents_spendcount, by = 'udid')

# spendevents first date
potential_users_spendevents_FirstDate = potential_users_spendevents %>% group_by(udid) %>% arrange(date) %>% filter(row_number()==1) %>% select(udid, date)
potential_users_spendevents_FirstDate = rename(potential_users_spendevents_FirstDate, Spend_FirstDate = date)
potential_users = left_join(potential_users, potential_users_spendevents_FirstDate, by = 'udid')

# spendevents last date
potential_users_spendevents_LastDate = potential_users_spendevents %>% group_by(udid) %>% arrange(date) %>% filter(row_number()==n()) %>% select(udid, date)
potential_users_spendevents_LastDate = rename(potential_users_spendevents_LastDate, Spend_LastDate = date)
potential_users = left_join(potential_users, potential_users_spendevents_LastDate, by = 'udid')

# 计算活跃的间隔天数
potential_users$sessions_day = potential_users$LastDate - potential_users$FirstDate

# 计算消费的间隔天数
potential_users$spend_day = potential_users$Spend_LastDate - potential_users$Spend_FirstDate

# 计算安装到首次活跃的天数
potential_users$active_day = potential_users$FirstDate - potential_users$install_date

# table(potential_users$active_day)
#     0     1     2     5    18 
# 16504    10     6     1     1 
# 99% 的用户安装的当天就活跃了

# write.csv(potential_users,"~/BA/HW/HW/Week 8-9/Game_Challenge/potential_users.csv")

# visualization 
# sessions_n
summary(potential_users$sessions_n)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 1.00    3.00   10.00   33.38   33.00 1697.00      20
ggplot(potential_users,aes(x=sessions_n)) + geom_histogram()
# 活跃了100次左右的用户占大多数

# total amount
summary(potential_users$total_amount)
#       Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
# -13999991       -10        -5      -913        -5        35 

potential_users = potential_users[-which(potential_users$total_amount == '-13999991'),]
potential_users = potential_users[-which(potential_users$total_amount == '-1000004'),]

#       Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
# -2608.000   -10.000    -5.000    -6.681    -5.000    35.000 

# total amount 有正数，但是这些用户在iaps里没有记录，说明他们不是用钱买的。
# 这有可能是通过分享给朋友之类的赚取了积分，而不是通过玩游戏而积累的积累。所以在花这些钱的时候，会显示正数。
# 可能是为了跟通过游戏赚取积分的方式区分开来

# 用tableau做的可视化

# 1. install date
# 从3/1一直到3/5逐渐上升，3/5达到顶峰，之后3/6就下降

# 2. lang
# 语言是以英语为主，81.12%

# 3. country
# 以US为主(51.68%)，其次是GB英国(10.91%)，接着是CA(4.30%)和AU澳大利亚(3.88%)

# 4. hw ver
# 手机型号主要以iphone为主

# 5. os ver
# mac os版本主要以9.2.1为主(66.30%)

# 6. sessions n 每个users一共活跃了多少次
# 大部分都是在3-33次之间
boxplot(potential_users$sessions_n, data = potential_users)

# 7. total amount 
plot(potential_users$total_amount)
potential_users = potential_users[-which(potential_users$total_amount == '-2608'),]
#  Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
# -38.000 -10.000   -5.000      -6.523  -5.000      35.000
boxplot(potential_users$total_amount, data = potential_users)
# 75% 的人都在-10和-5之间

# 8. spend count 购买次数
# 大部分都购买一次

# 9. sessions day
# 0 day(当天)有29.97%, 1 day有8.22%，证明大部分用户都是活跃周期很短

# 10. spend day
# 0 day有70.33%，证明大部分用户都是只消费了一次

# 11. active day
table(potential_users$active_day)
#     0     1     2     5    18 
# 16501    10     6     1     1 
# 大部分玩家都死安装的当天就进行了首次活跃

# 分析spend events
# 把没用的column删去
potential_users_spendevents$spendevents_ts = NULL
potential_users_spendevents$spendevents_ts_h = NULL
# write.csv(potential_users_spendevents,"~/BA/HW/HW/Week 8-9/Game_Challenge/potential_users_spendevents.csv")

# spendevents date
# 从3/1一直上升到3/6，从3/7开始就走下坡

# spendevents story
# 最喜欢在Demi_M_Closet(43.02%)消费，其次是Demi_Master(33.85%)， Mean_Girls_Version_D(21.77%)
# 说明这几个系列比较火

# spendevents chapter
# 有72%的用户在第0章就开始消费

# spendevents spendtype
# 94.39%的用户花费类型是earnGemsCounter

# spendevents story chapter
# 有43%的人在Demi_M_Closet_0上消费了，但是之后就没有在这个系列上面里消费过了
# 说明这个系列一开始很吸引用户，但是可能之后的章节没做好，所以导致用户都没在这个系列里继续花钱，后期要对这个系列的内容修改一下
# Demi_Master好几个chapter都有人在消费，说明这个系列做得不错


# converted players
# 抓出user里的converted users
converted_users = users[which(users$status == 'Converted'),]

# 提取iaps的weekday
iaps$wd = wday(iaps$date, label = TRUE)
plot(iaps$wd)
# 星期六日消费的人比较多

plot(iaps$iaps_ts_h)
# 12点到下午6点消费的人最多

# 转换率：6.76%

iaps$iaps_month <- sapply(iaps$date, Month)
iaps$iaps_month <- as.factor(iaps$iaps_month)
iaps$iaps_ts_m = NULL

# PUR 付费率=付费人数/活跃人数   PUR = APA/AU
PUR = iaps %>% group_by(iaps_month) %>% distinct(udid) %>% count()
PUR$MAU_n = MAU$n
PUR = PUR %>% group_by(iaps_month) %>% summarise(PUR = n/MAU_n)
plot(PUR$iaps_month,PUR$PUR)
# 付费率逐月下降

# ARPPU 每付费用户平均收益   ARPPU = Revenue/APA

# Revenue
iaps %>% summarise(rev_total = sum(rev))
#     rev_total
# 1   1445477

# APA
iaps %>% distinct(udid) %>% count()
#       n
#  1  1526

1445477/1526
# ARPPU =  947.2326

# ARPU 平均每用户收入 (活跃用户对游戏产生对平均收入)
# ARPU = Revenue/AU

# AU
sessions %>% distinct(udid) %>% count()
#     n
# 1 22544
1445477/22544
# ARPU = 64.11804

# ARPU低，ARPPU高，说明大R付费能力强。针对这一点，我们做一些付费功能调整和优化，甚至专属大R客服，让大R玩的更开心

# converted users' sessions information
converted_users_sessions = sessions[sessions$udid %in% converted_users$udid,]
converted_users_sessions_n = converted_users_sessions %>% group_by(udid) %>% count()

# 把sessions count 与 users information 合并
converted_users = left_join(converted_users,converted_users_sessions_n, by = 'udid')
converted_users = rename(converted_users, sessions_n = n)

# sessions last date
converted_users_sessions_LastDate = converted_users_sessions %>% group_by(udid) %>% arrange(date) %>% filter(row_number()==n()) %>% select(udid, date)
converted_users_sessions_LastDate = rename(converted_users_sessions_LastDate, LastDate = date)
converted_users = left_join(converted_users, converted_users_sessions_LastDate, by = 'udid')

# sessions first date
converted_users_sessions_FirstDate = converted_users_sessions %>% group_by(udid) %>% arrange(date) %>% filter(row_number()==1) %>% select(udid, date)
converted_users_sessions_FirstDate = rename(converted_users_sessions_FirstDate, FirstDate = date)
converted_users = left_join(converted_users, converted_users_sessions_FirstDate, by = 'udid')


# 抓出spendevents里的converted users
converted_users_spendevents = spendevents[spendevents$udid %in% converted_users$udid,]

# 计算出他们的total amount
converted_users_spendevents_count = converted_users_spendevents %>% group_by(udid) %>% summarise(total_amount = sum(amount))

# 把total amount 与 users information 合并
converted_users = left_join(converted_users, converted_users_spendevents_count, by = 'udid')

# 计算converted users的spend的次数
converted_users_spendevents_spendcount = converted_users_spendevents %>% group_by(udid) %>% count()
converted_users_spendevents_spendcount = rename(converted_users_spendevents_spendcount, SpendCount = n)

# 把spend count 与 users information 合并
converted_users = left_join(converted_users, converted_users_spendevents_spendcount, by = 'udid')

# spendevents first date
converted_users_spendevents_FirstDate = converted_users_spendevents %>% group_by(udid) %>% arrange(date) %>% filter(row_number()==1) %>% select(udid, date)
converted_users_spendevents_FirstDate = rename(converted_users_spendevents_FirstDate, Spend_FirstDate = date)
converted_users = left_join(converted_users, converted_users_spendevents_FirstDate, by = 'udid')

# spendevents last date
converted_users_spendevents_LastDate = converted_users_spendevents %>% group_by(udid) %>% arrange(date) %>% filter(row_number()==n()) %>% select(udid, date)
converted_users_spendevents_LastDate = rename(converted_users_spendevents_LastDate, Spend_LastDate = date)
converted_users = left_join(converted_users, converted_users_spendevents_LastDate, by = 'udid')

# 计算活跃的间隔天数
converted_users$sessions_day = converted_users$LastDate - converted_users$FirstDate

# 计算消费的间隔天数
converted_users$spend_day = converted_users$Spend_LastDate - converted_users$Spend_FirstDate

# 计算安装到首次活跃的天数
converted_users$active_day = converted_users$FirstDate - converted_users$install_date

table(converted_users$active_day)
#   0     1    6 
# 1522    3    1
# 99% 的用户安装的当天就活跃了

# iaps
TotalRev = iaps %>% group_by(udid) %>% summarise(total_rev = sum(rev))
converted_users = left_join(converted_users, TotalRev, by = 'udid')

# iaps count
iaps_count = iaps %>% group_by(udid) %>% count()
iaps_count = rename(iaps_count, iaps_count = n)
converted_users = left_join(converted_users, iaps_count, by = 'udid')

# iaps first date
converted_users_iaps_FirstDate = iaps %>% group_by(udid) %>% arrange(date) %>% filter(row_number()==1) %>% select(udid, date)
converted_users_iaps_FirstDate = rename(converted_users_iaps_FirstDate, iaps_FirstDate = date)
converted_users = left_join(converted_users, converted_users_iaps_FirstDate, by = 'udid')

# iaps last date
converted_users_iaps_LastDate = iaps %>% group_by(udid) %>% arrange(date) %>% filter(row_number()==n()) %>% select(udid, date)
converted_users_iaps_LastDate = rename(converted_users_iaps_LastDate, iaps_LastDate = date)
converted_users = left_join(converted_users, converted_users_iaps_LastDate, by = 'udid')

# 计算首次活跃到首次付款的天数
converted_users$pay_day = converted_users$iaps_FirstDate - converted_users$FirstDate

# 比较首次花费(SpendEvent)是否比首次付款早
# 想知道是否首次花费(SpendEvent)是否比首次付款早，若是的话，证明游戏还蛮有吸引力
converted_users$Spend_FirstDate <= converted_users$iaps_FirstDate
# 大部分都是true

# write.csv(converted_users,"~/BA/HW/HW/Week 8-9/Game_Challenge/converted_users.csv")

# visualization 
# sessions_n
summary(converted_users$sessions_n)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1.00   19.00   42.00   86.27   97.00 1939.00
ggplot(converted_users,aes(x=sessions_n)) + geom_histogram()
# 活跃了100次左右的用户占大多数

# total amount
summary(converted_users$total_amount)
#      Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
# -1477.00   -16.00    -7.00   -17.08    -3.00   145.00       51
# -16 ～ -3 说明converted了的用户里，赚的都是少量的钱

# 用tableau做的可视化

# 1. install date
# 从3/1一直到3/4很平稳，用户的下载量增长不明显，3/5激增，3/6达到顶峰，之后3/7下降了好多，回到跟以前差不多的下载量

# 2. lang
# 语言是以英语为主，93.71%

# 3. country
# 以US为主(68.87%)，其次是GB英国(11.53%)，接着是AU澳大利亚(4.78%)和CA(4.00%)

# 4. hw ver
# 手机型号主要以iphone为主,接着是ipad

# 5. os ver
# mac os版本主要以9.2.1为主(73.39%)

# 6. spend count 购买次数
summary(converted_users$SpendCount)
# 平均购买11次

# 7. sessions day
# 60 day有8.716%, 0 day有6.029%, 日期跨越很大，说明后期搞活动刺激上线了

# 10. spend day: 第一次消费距离最有一次消费的天数
# 0 day有22.80%, 1 day有10.22%，证明大部分用户都是只消费了一次

# 11. active day
table(converted_users$active_day)
#     0    1    6 
#  1522    3    1
# 大部分玩家都死安装的当天就进行了首次活跃

# 分析spend events
# 把没用的column删去
converted_users_spendevents$spendevents_ts = NULL
converted_users_spendevents$spendevents_ts_h = NULL
# write.csv(converted_users_spendevents,"~/BA/HW/HW/Week 8-9/Game_Challenge/converted_users_spendevents.csv")

# spendevents date
# 从3/1一直上升到3/6，从3/7开始就走下坡

# spendevents story
# 最喜欢在Mean_Girls_Version_D(37.46%)消费，其次是Demi_M_Closet(25.85%), Demi_Master(24.27%)， 
# 说明这几个系列比较火

# spendevents chapter
# 有36.65%的用户在第0章就开始消费

# spendevents spendtype
# 45.78%的用户花费类型是earnGemsCounter, premium choice 是36.15%，iap是17.92%，很少有人买valuepack(0.14%)

# spendevents story chapter
# 有25.55%的人在Demi_M_Closet_0上消费了，但是之后就没有在这个系列上面里消费过了
# 说明这个系列一开始很吸引用户，但是可能之后的章节没做好，所以导致用户都没在这个系列里继续花钱，后期要对这个系列的内容修改一下
# Demi_Master和Mean Girls Version D好几个chapter都有人在消费，说明这个系列做得不错

