library(dplyr)

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
Q3
table frieding：date，action（‘send’，‘accept’），user_id，target_id
计算acceptance rate treding by time
每个用户能发送一次，每个用户也只能接受一次，a发了request给b，b就不能发送给a，只能接受或忽略。
如果a发送request给b，b接受了才能认为是accept，否则不算。
我当时问了时间长度的问题，被反问觉得应该选择多长的时间？
我直觉说了一句monthly，问为什么是monthly，想了想说应该算average frequency for send or accept，再根据这个定时间。
追问用现有表格如何算average frequency？说了一下。
最后让用一个月为期限，算acceptance rate。
"

  