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
WHERE a.date = date()-1

    Which grade level currently has the most students in this school district?
SELECT TOP 1 grade_level, COUNT(student_id) AS count
FROM student
GROUP BY grade_level
ORDER BY COUNT(student_id) DESC

    Which school had the highest attendance rate? The lowest?

SELECT TOP 1 a.school_id, SUM(CASE WHEN b.attendance=1 THEN 1 ELSE 0)/COUNT(a.student_id) AS rate
FROM student a
LEFT JOIN attendance b ON
(a.student_id = b.student_id)
GROUP BY a.school_id
ORDER BY rate DESC|ASC
"
att <-attendance %>% filter(date = Sys.Date()-1) %>%
  summarise(count = sum(ifelse(attendance==1, 1, 0)))
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
  filter(rate == max(rate) & rate == min(rate))
"
Q1
t1: name, category; # player name and category 
t2: id, name, date; # player details
t3: id, follower, date # player and follower
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

"Total # of comments and total # of posts"
content %>% group_by(content_type) %>%
  summarise(count = n_distinct(content_id))

"distribution of comments over post"
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

SELECT a.date, COUNT(a.user_id) AS total, SUM(CASE WHEN (date.accept-date.send <= 1) AND (b.action='accept') THEN 1 ELSE 0) AS accept, accept/total AS rate
FROM (
  SELECT a.user_id, b.user_id, a.date AS date_send, b.date AS date.accept 
  FROM table a LEFT JOIN table b ON
  (a.user_id = b.target_id AND a.target_id = b.user_id AND a.action ='send' AND b.action='accept')
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
            zero_result_pct = num_search*zero_result_pct/sum(num_search))

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
union_all(tb1, tb2) %>% group_by(user1) %>%
  filter(date = max(date))
