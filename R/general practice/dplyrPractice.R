library(dplyr)
# Facebook mock interview
"
Event-level data: an attendance log for every student in a school district
date | student_id | attendance

Dimension-level data: a summary table with demographics for each student in the district
student_id | school_id | grade_level | date_of_birth | hometown

Using this data, you could answer questions like the following:
    What was the overall attendance rate for the school district yesterday?

SELECT SUM(CASE WHEN a.attendance = 1 THEN 1 ELSE 0)/COUNT(b.student_id)
FROM attendance a
RIGHT JOIN student b ON
(a.student_id = b.student_id)
WHERE a.date = dateadd(day,datediff(day,1,GETDATE()),0)

// datediff, calculate days diff between day 1 to GETDATE()
// add this date diff to day 0

    Which grade level currently has the most students in this school district?
SELECT TOP 1 grade_level, COUNT(student_id) AS count
FROM student
GROUP BY grade_level
ORDER BY count DESC

    Which school had the highest attendance rate? The lowest?

SELECT TOP 1 a.school_id, SUM(CASE WHEN b.attendance=1 THEN 1 ELSE 0)/COUNT(a.student_id) AS rate
FROM student a
LEFT JOIN attendance b ON
(a.student_id = b.student_id)
GROUP BY a.school_id
ORDER BY rate DESC|ASC
"
att <-attendance %>% filter(date = Sys.Date()-1) %>%
  summarise(count = sum(attendance==1))
total <- nrow(student)
att/total

student %>% group_by(grade_level) %>%
  summarise(count = n()) %>%
  filter(count=max(count))

attendance %>% filter(date = Sys.Date()-1) %>%
  right_join(student, by='student_id') %>%
  group_by(school_id) %>%
  summarise(count = n(),
            attend = sum(attendance==1),
            rate = attend/count) %>%
  filter(rate == max(rate) | rate == min(rate)) # most or least attendance rate school
"
Q1
t1: name, category; # player name and category 
t2: id, name, date; # player details
t3: id, follower, date # player and follower
SELECT t1.category, COUNT(t3.followers) AS followers, COUNT(DISTINCT t3.follower) AS users
FROM t2 LEFT JOIN t1 ON
(t2.name = t1.name)
LEFT JOIN t3 ON
(t2.id = t3.id)
GROUP BY t1.category
"
t1 <- data.frame(name=c('a', 'b', 'c', 'd','e', 'f'),
                category = c('NFL', 'NFL', 'NFL', 'NBA', 'NBA', 'MLB'))
t2 <- data.frame(id = c(1:6),
                 name = c('a', 'b', 'c', 'd','e', 'f'))
t3 <- data.frame(id = c(1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6),
                 follower = c(2, 3, 4, 5, 6, 1, 2, 3, 4, 1, 2, 3))
"How many followers in each category"
df1<-t1 %>% 
  left_join(t2, by = 'name') %>%
  left_join(t3, by = 'id')%>%
  select(name, category, follower)

df1 %>% group_by(category) %>%
  summarise(
    count = n_distinct(follower)
  ) %>%
  filter(category == 'NBA')

"How many NBA followers also follow NFL" 
t1 %>%
  filter(category %in% c('NBA', 'NFL')) %>%
  left_join(t2, by = 'name') %>%
  left_join(t3, by = 'id') %>%
  select(name, category, follower) %>%
  group_by(follower) %>%
  summarise(
    count = n_distinct(category)) %>% 
  filter(count ==2) %>%
  summarise(count = n())


"How many NBA celetrities follows NFL"
all <- t2 %>%
  left_join(t1, by='name')
nba <- all %>% filter(category == 'NBA')
nfl <- all %>% filter(category == 'NFL')

inner_join(nfl,t3, by = 'id') %>%
  inner_join(nba, by=c('follower' = 'id')) %>%
  summarise(count = n_distinct(follower))

"
Q2
content: userid, content_id, content_type, target_id 
  content_type = {'comment', 'post'}
if content_type = 'post', no target_id
target_id: post owner's id
content_id: post_id or comment_id
"

"Total # of comments and total # of posts
SELECT content_type, COUNT(*) as total
FROM content
GROUP BY content_type

"
content %>% group_by(content_type) %>%
  summarise(count = n_distinct(content_id))

"distribution of comments over post
SELECT comments, COUNT(post_id) AS posts_count
FROM
  (SELECT a.content_id AS post_id, ISNULL(COUNT(b.content_id), 0) AS comments
  FROM content a LEFT JOIN content b ON
  (b.target_id = a.userid)
  WHERE a.content_type = 'post' AND b.content_type = 'comment'
  GROUP BY a.content_id)
GROUP BY comments
"
comments <- content %>% filter(content_type == 'post') %>%
  left_join(content, by = c('target_id' = 'userid')) %>%
  group_by(content_id) %>%
  summarise(count = n_distinct(content_id.y))
# method 1: number of posts by each comment count
comments %>% group_by(count) %>%
  summarise(post_count = n())
# method 2: density function of the comment count
library(graphics)
d <- density(comments$count)
plot(d)
hist(comments$count)
library(ggplot2)
ggplot(comments, aes(count))+geom_density()

"
Q4
table friending：date，action{‘send’，‘accept’}，user_id，target_id
计算acceptance rate treding by time
每个用户能发送一次，每个用户也只能接受一次，a发了request给b，b就不能发送给a，只能接受或忽略。
如果a发送request给b，b接受了才能认为是accept，否则不算。
我当时问了时间长度的问题，被反问觉得应该选择多长的时间？
我直觉说了一句monthly，问为什么是monthly，想了想说应该算average frequency for send or accept，再根据这个定时间。
追问用现有表格如何算average frequency？说了一下。
最后让用一个月为期限，算acceptance rate。

SELECT a.date, COUNT(a.user_id) AS total, SUM(CASE WHEN (date.accept-date.send <= 1) THEN 1 ELSE 0) AS accept, accept/total AS rate
FROM (
  SELECT a.user_id, b.user_id, a.date AS date_send, b.date AS date.accept 
  FROM table a LEFT JOIN table b ON
  (a.user_id = b.target_id AND a.target_id = b.user_id)
  WHERE a.action ='send' AND b.action='accept'
)
GROUP BY a.date
  
"
request <- table %>% filter(action=='send')
accept <- table %>% filter(action=='accept')

"Accept rate by day"
time.window = 1 # 1 day as max gap
left_join(request, accept, by = c('target_id' = 'user_id', 'user_id' = 'target_id')) %>%
  select(sender = user_id.x, target_id = target_id.x,  date.send = date.x, date.accept = date.y) %>%
  mutate(success = (!is.na(date.accpet)) & ((date.accpet -date.send) <= time.window) ) %>%
  group_by(date.send) %>%
  summarise(request.total = n(),
            accept.total = sum(success))

"
Q5
-- table: country, search_cat, num_search, zero_result_pct
-- result: country, num_search, zero_result_pct

SELECT country, SUM(num_search) AS num_search, ISNULL(SUM(num_search*zero_result_pct)/SUM(num_search), 0) AS zero_result_pct
FROM table
GROUP BY country
"
table %>% group_by(country) %>%
  summarise(num_search = sum(num_search), 
            zero_result_pct = sum(num_search*zero_result_pct)/sum(num_search))

"
Q6
-- userid, action_id, target_id, action{'sent', 'accept', 'unfriend'), date
"
"how many friends each person has

SELECT userid, SUM(CASE WHEN action='accept' THEN 1 ELSE -1) AS total
FROM(
  SELECT userid, target_id, action FROM table WHERE action IN ('accpet', 'unfriend)
  UNION ALL
  SELECT target_id, userid, action FROM table WHERE action IN ('accept', 'unfriend')
)
GROUP BY userid
"
tb1 <- table %>% filter(action %in% c('accept', 'unfriend')) %>%
  select(user1 = userid, user2=target_id, action, date)

tb2 <- table %>% filter(action %in% c('accept', 'unfriend')) %>%
  select(user1 = target_id, user2=userid, action, date)

union_all(tb1, tb2) %>% group_by(user1) %>%
  summarise(friends = sum(action=='accept')-sum(action=='unfriend'))

"
how to decide if 2 people are friends or NOT
SELECT userid, target_id, action, ROW_NUMBER() OVER(PARTITION BY userid, target_id ORDER BY date DESC) AS rank
FROM(
  SELECT userid, target_id, action FROM table WHERE action IN ('accpet', 'unfriend)
  UNION ALL
  SELECT target_id, userid, action FROM table WHERE action IN ('accept', 'unfriend')
)
WHERE rank = 1
"
# if the last status was accept then they are friend.
union_all(tb1, tb2) %>% group_by(user1) %>%
  filter(date == max(date))

"
Q: 7
-- how do you calculate monthly active users, churned users and resurrected users from a user activity log
-- log {userid, act_time)  

-- user active (last month yes, this month yes)
-- user churned (last month yes, this month no)
-- user revived (last month no, this month yes)

SELECT yearmonth, status, COUNT(userid) AS count FROM(
  SELECT userid, DATE_FORMAT(act_time, '%Y-%m') as yearmonth,
  (CASE  WHEN a.userid IS NOT NULL AND b.userid IS NOT NULL THEN 'active'
  WHEN a.userid IS NULL AND b.userid IS NOT NULL THEN 'churned'
  WHEN a.userid IS NOT NULL AND b.userid IS NULL THEN 'revived') AS status
  FROM table a
  OUTER JOIN (
  SELECT userid, DATE_FORMAT(act_time, '%Y-%m') as yearmonth, COUNT(userid) AS act
  FROM table
  ) b ON 
  (a.userid = b.userid AND a.yearmonth = b.yearmonth+1)
)
GROUP BY yearmonth, status

"

library(lubridate)
log<- mutate(log, month = month(act_time)) %>% group_by(userid, month) %>%
  summarise(count=n())
last_month <- filter(log, month==(month(Sys.Date()-1)))
this_month <- filter(log, month==(month(Sys.Date())))
log2<-this_month %>% full_join(last_month, by='userid')

"user active"
filter(log2, !is.na(act_time.x) & !is.na(act_time.y))
"user churned"
filter(log2, !is.na(act_time.y) & is.na(act_time.x))
"user revived"
filter(log2, !is.na(act_time.x) & is.na(act_time.y))

# Another way
summarise(log2, active_users = sum(!is.na(act_time.x) & !is.na(act_time.y)),
                churned_users = sum(is.na(act_time.x) & !is.na(act_time.y)),
                revived_users = sum(!is.na(act_time.x) & is.na(act_time.y)))
"
Q8
-- dialog{userid, appid, type, flag('im', 'cl'), time}
-- how to compute click through rate
# Based on action
SELECT appid, SUM(CASE THEN flag='cl' THEN 1 ELSE 0)/COUNT(userid)
FROM dialog
GROUP BY appid
"
group_by(dialog, appid) %>%
  summarise(cli_through_rate = SUM(ifelse(flag =='cl', 1, 0))/n())

"
# Based on user
SELECT a.appid, b.cl/a.total AS cli_through_rate
FROM
(SELECT appid, COUNT(DISTINCT userid) AS total FROM dialog) a LEFT JOIN
(SELECT appid COUNT(DISTINCT userid) AS cl FROM dialog WHERE flag = 'cl') b ON
(a.appid = b.appid)
GROUP BY a.appid
"
cli <-filter(flag == 'cl') %>%
  group_by(appid) %>%
  summarise(cli_count = n_distinct(userid))

tot <- group_by(dialog, appid) %>%
  summarise(total_count = n_distinct(userid))

left_join(cli, tot, by='appid') %>%
  mutate(ctr = cli_count/total_count)

"
Q9:
table1: date, carrier, country, phone_number, type {friend_notice, confirmation, other}
table2 (Phone numbers who have responded to the confirmation): date, phone_number
"
"
How many numbers were sent confirmation message yesterday?
(tip: duplicate number)

SELECT COUNT(DISTINCT phone_number) FROM table1
WHERE date = (Date()-1) AND type='confirmation'
"
filter(table1, date == (Sys.Date()-1) & type == 'confirmation') %>%
  summarise(count = n_distinct(phone_number))

"
confirmation rate/day
(tip: duplicate number, time window)

SELECT  date,
        COUNT(DISTINCT phone_number) AS sent, 
        SUM(CASE WHEN (b.date - a.date) <= AVG(DATEDIFF(a.date, b.date)) THEN 1 ELSE 0) AS confirmed, 
        confirmed/sent AS rate
FROM table1
LEFT JOIN table2 AS b ON
(a.phone_number = b.phone_number AND a.type = 'confirmation')
GROUP BY date
"
sent <- filter(table1, type = 'confirmation') %>%
  group_by(date) %>%
  summarise(count = n_distinct(phone_number))

confirmed <- group_by(table2, date) %>%
  summarise(count = n_distinct(phone_number))

# Use the 90% quantile of time gap as the condition
left_join(sent, confirmed, by='phone_number') %>%
  mutate(qualified = ((date.y - date.x) <=quantile(date.y - date.x, 0.9))) %>%
  group_by(date.x) %>%
  summarise(date.x, rate = sum(qualified)/n())

"
SQL Q10:
--table A: advertiser_id, ad_id, spend
--table B: ad_id, user_id, spend 
q1 : Calculate ROI for each advertiser_id 
SELECT advertiser_id, ISNULL(return,0)/spend
FROM
(SELECT advertiser_id, SUM(spend) AS spend 
  FROM A 
  GROUP BY advertiser_id) a
LEFT JOIN(
  SELECT advertiser_id, SUM(b.spend) AS return
  FROM A a LEFT JOIN B b ON
  (a.ad_id = b.ad_id)
  GROUP BY advertiser_id
) c ON
(a.advertiser_id = c.advertiser_id)
"
return <- left_join(B, A, by='ad_id') %>% 
  group_by(advertiser_id, ad_id) %>%
  summarise(return = sum(spend))

ad_roi <- left_join(A, return, by=c('advertiser_id', 'ad_id')) %>%
  mutate(roi = ifelse(is.na(return), 0, return)/spend)

advertiser_roi <- group_by(ad_roi, advertiser_id) %>%
  summarise(roi = sum(ifelse(is.na(return), 0, return))/sum(spend))

"
SQL Q11:
Given the following tables how would you know who has the most friends 
REQUESTS { date, sender_id,  accepter_id }
ACCEPTED { accepted_at,  accepter_id,  sender_id }

If 2 users have the following history: request, accept, request. It means they are not friend in the end. So only the last action of a pair of users is meaningful

SELECT TOP 1 id1, COUNT(id2) AS friends
FROM
  (SELECT id1, type, date, ROW_NUMBER() OVER(PARTITION BY id1, id2 ORDER BY date DESC) AS rank
  FROM  
    (SELECT date, sender_id AS id1, accepter_id AS id2, 'request' AS type FROM REQUEST
    UNION ALL
    SELECT date, accepter_id AS id1, sender_id AS id2, 'request' AS type FROM REQUEST
    UNION ALL
    SELECT date, accepter_id AS id1, sender_id AS id2, 'accept' AS type FROM ACCEPTED
    UNION ALL
    SELECT date, sender_id AS id1, accepter_id AS id2, 'accept' AS type FROM ACCEPTED)
)
WHERE rank = 1 AND type = 'accept'
GROUP BY id1
ORDER BY friends DESC
"
log<-rbind(
  select(REQUESTS, date, id1 = sender_id, id2 = accepter_id, type = 'request'),
  select(REQUESTS, date, id1 = accepter_id, id2 = sender_id, type = 'request'),
  select(ACCEPTED, date = accepted_at, id1 = sender_id, id2 = accepter_id, type = 'accept'),
  select(ACCEPTED, date = accepted_at, id1 = accepter_id, id2 = sender_id, type = 'accept')
)

# rank by date, most recent date as rank 1
group_by(log, id1, id2)%>%
  mutate(rank = rank(desc(date))) %>%
  filter(rank ==1 & type == 'accept') %>%
  group_by(id1) %>%
  summarise(friends = n_distinct(id2)) %>%
  arrange(desc(friends)) %>% slice(1)
# OR filter(friends == max(friends))

"
Table {date, u1, u2, n_msg}
It contains the number of messages sent between all unique pair of users
1. What we could learn from this table?
Total amount of messages sent a day
How many contancts a user usually talk to
number of active users per day
Social network
How close between users
Who is the important hub user (connect to a lot other users)

2. Get the distribution of number of fiends who communicate. 
SELECT n_msg, COUNT(u1) AS users
FROM
  (SELECT u1, SUM(n_msg) as n_msg 
    FROM
      (SELECT u1, u2, date, n_msg FROM table
      UNION ALL
      SELECT u2, u1, date, n_msg FROM table)
    GROUP BY u1)
GROUP BY n_msg

3. Get the pair who send the most messages and percentage of the total
SELECT date, MAX(n_msg)/SUM(n_msg) AS pct
FROM table
GROUP BY date
"
msg<-union_all(select(table, u1, u2, date, n_msg), select(table, u1=u2, u2=u1, date, n_msg)) %>%
  group_by(u1) %>%
  summarise(n_msg = sum(n_msg)) 
group_by(msg, n_msg) %>%
  summarise(n_user = count(u1))
# Density plot
ggplot(msg, aes(n_msg))+geom_density()

mutate(msg, rate = n_msg/sum(n_msg)) %>%
  filter(rate == max(rate))

"
Q13
The table is given as TABLE {date,  actor_uid, target_uid, action}
action = {'send_request', 'accept_request', 'unfriend'}
actor_uid = the person who takes the action

# a.action='send' filte out other joint result; b.action='accept', only join the record in b with 'accept' to a
# in outer join, put the condition in ON like b.action='accept' or a.action='send', the total rows in result won't be changed. But the number of a matched is different
# If the put the condition in WHERE clause, it works as a filter and happens after the join done. Then it will filter out the records based on the condition says

# In inner join, it doesn't matter where to put the condition in. Because any unmatched records will be exlcuded by inner join
Q1: How is the overall friending acceptance rate changing over time?
SELECT date, SUM(CASE WHEN b.date < a.date + x THEN 1 ELSE 0)/COUNT(a.actor_uid) AS rate
FROM table a LEFT JOIN table b ON
(a.actor_uid = b.target_uid AND a.target_uid = b.action_uid AND b.action='accept')
WHERE a.action = 'send'
GROUP BY date
"

accept <- filter(table, action=='accept')
request <- filter(table, action=='send')
left_join(request, accept, by=c('actor_uid'='target_uid', 'target_uid'='actor_uid')) %>%
  mutate(overtime = (date.y-date.x>=max_time)) %>%
  group_by(date)%>%
  summarise(rate = sum(!overtime)/n())

"
Q2: Who has the most number of friends?
SELECT id, SUM(CASE WHEN action='accept' THEN 1 ELSE -1) AS friends
FROM(  
  SELECT actor_uid AS id, action FROM table WHERE action IN ('accept', 'unfriend')
  UNION ALL
  SELECT target_uid AS id, action FROM table WHERE action IN ('accept', 'unfriend')
) 
GROUP BY id

"
df <- filter(table, action %in% c('unfriend', 'accept'))
union_all(select(df, id = actor_uid, action), select(df, id = target_uid, action))%>%
  mutate(change = ifelse(action=='accept', 1, -1)) %>%
  group_by(id) %>%
  summarise(friends = sum(change))
  