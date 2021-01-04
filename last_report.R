install.packages("dplyr")
install.packages("psych")
library(dplyr)
library(psych)

rm(list=ls())

setwd("/Users/nakaoryouta/Desktop/lab_2020/autumn/code/data")
df_original <- read.csv("data.csv")

#説明変数

"部員数の変数名→ span of control"

df_original = df_original %>%
  dplyr::group_by(group_id)%>%
  dplyr::mutate(span_of_control = n())%>%
  dplyr::ungroup()


df_original = df_original %>%
  dplyr::mutate(span_of_contorl_2 =span_of_control**2)

df_original = df_original %>%
  dplyr::group_by(university)%>%
  dplyr::mutate(s_span_of_control = scale(span_of_control))%>%
  dplyr::mutate(s_span_of_control_2 = scale(span_of_contorl_2))%>%
  dplyr::ungroup()


#コントロール変数


"プロ野球選手比率"


df_original = df_original %>%
  dplyr::group_by(group_id)%>%
  dplyr::mutate(pro_player_proportion = mean(pro_player, na.rm=TRUE))%>%#pro_player
  dplyr::ungroup()


"大学ダミー"

df_original = df_original %>%
  dplyr::mutate(K_dummy = if_else(university == "K" , true = 1, false = 0))%>%
  dplyr::mutate(W_dummy = if_else(university == "W" , true = 1, false = 0))%>%
  dplyr::mutate(R_dummy = if_else(university == "R" , true = 1, false = 0))%>%
  dplyr::mutate(M_dummy = if_else(university == "M" , true = 1, false = 0))%>%
  dplyr::mutate(H_dummy = if_else(university == "H" , true = 1, false = 0))%>%
  dplyr::mutate(T_dummy = if_else(university == "T" , true = 1, false = 0))


df_original = df_original %>%
  dplyr::mutate(fe_id = paste(university, as.character(time), sep="_"))


"omid T processing〜東大を除去〜"

omid_T_data = df_original %>%
  dplyr::filter(university!= "T")




"cor(data)_相関行列 "

df_original <- df_original[, !(colnames(df_original) %in% c("mst_id", "university", "time", "n", "season", "position"
  ,"group_id","name","fe_id","ikusei_dummy","win_point","s_span_of_control","s_span_of_control_2"
    ,"rank_drafuto"))]

# install.packages("nbastats")
# install.packages("tidyverse")
# install.packages("GGally")
# library(nbastats)
# library(tidyverse)
# library(GGally)


correlation_matrix <- function(df_original) {
  # gets columns names
  colnames <- names(df_original)
  
  # change data.frame into matrix
  # and if there are non-numeric columns , convert them into numeric
  mat <- df_original %>% 
    sapply(as.numeric)
  
  # create correlation matrix
  cormat <- cor(mat)
  
  # prepare parameters for ggplot
  n <- ncol(cormat)
  x <- c()
  for (i in 1:n) {
    x <- append(x, rep(i, n))
  }
  y <- rep(1:n, n)
  
  # convert matrix into dataframe
  cormat <- data.frame(cormat) %>%
    gather(key = "type", value = "value") %>% 
    mutate(x = x, y = y)
  
  # create correlation matrix with color scale
  cormat %>% ggplot() +
    geom_tile(aes(x = x, y = y, fill = value)) +
    geom_text(aes(x = x, y = y, label = round(cormat$value, 2)), size = 3) +
    scale_x_continuous(breaks = 1:n, labels = colnames) +
    scale_y_continuous(breaks = 1:n, labels = colnames) +
    theme(axis.title.x = element_blank(),
      axis.title.y = element_blank()) +
    ggtitle("Correlation Matrix") +
    scale_fill_gradient(low = "white", high = "red", name = "scale")
}

correlation_matrix(df_original)


#記述統計

dat_1 <- psych::describe(omid_T_data)
write.csv(dat_1, file = "/Users/nakaoryouta/Desktop/lab_2020/autumn/記述統計/記述統計表/Descriptive statistics_omid_T.csv")


"Estimate_data ~推計用データ（csv）~"

write.csv(omid_T_data, file = "/Users/nakaoryouta/Desktop/lab_2020/autumn/code/data/estimate_data/omid_T_data.csv")




