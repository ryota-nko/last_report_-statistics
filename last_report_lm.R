install.packages("lfe")
library(lfe)

rm(list=ls())
setwd("/Users/nakaoryouta/Desktop/last_report/data")
use_data <- read.csv ("omid_T_data.csv")


#######################   win_dummy (優勝ダミー)　ロジスティック回帰　###################### 

result_1 <-lm(win_dummy ~ s_span_of_control ,data = use_data)#単回帰

result_2 <-lm(win_dummy ~ s_span_of_control + span_of_contorl_2 +height + weight + pro_player_proportion + spring_s_span 
  + K_dummy + W_dummy + M_dummy + R_dummy + H_dummy,data = use_data)#重回帰

result_3 <-lfe::felm(win_dummy ~ s_span_of_control  +height + weight+ span_of_contorl_2  + pro_player_proportion + spring_s_span | fe_id, data = use_data)#個人+年度+大学FE

result_4 <- glm(win_dummy ~ s_span_of_control , data=use_data,family=poisson)#ロジスティック回帰

result_5 <- glm(win_dummy ~ s_span_of_control + span_of_contorl_2 +height + weight + pro_player_proportion + spring_s_span 
  + K_dummy + W_dummy + M_dummy + R_dummy + H_dummy, data=use_data,family=poisson)#ロジスティック回帰


result_plain = stargazer::stargazer(result_1,result_2,result_3,result_4,result_5,type = "html" )
result_escape = stringr::str_replace_all(result_plain, "\\((.+?)\\)", "'\\(\\1\\)")
cat(result_escape,file="win_dummy.html",sep="\n")


#######################   win_ratio (勝率)　重回帰分析・単回帰分析　###################### 

result_1 <-lm(win_ratio ~ s_span_of_control ,data = use_data)#単回帰

result_2 <-lm(win_ratio ~ s_span_of_control + span_of_contorl_2 +height + weight + pro_player_proportion + spring_s_span 
  + K_dummy + W_dummy + M_dummy + R_dummy + H_dummy,data = use_data)#重回帰

result_3 <-lfe::felm(win_ratio ~ s_span_of_control  +height + weight+ span_of_contorl_2  + pro_player_proportion + spring_s_span | fe_id, data = use_data)#個人+年度+大学FE


result_plain = stargazer::stargazer(result_1,result_2,result_3,type = "html" )
result_escape = stringr::str_replace_all(result_plain, "\\((.+?)\\)", "'\\(\\1\\)")
cat(result_escape,file="/Users/nakaoryouta/Desktop/lab_2020/autumn/推計結果/分析結果/win_ratio.html",sep="\n")

