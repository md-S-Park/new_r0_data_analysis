library(tidyverse)
library(readxl)



###Data 불러오기 
dat <- read_excel("G:\\내 드라이브\\dev\\new_r0_data_analysis\\vein_230206.xlsx", sheet="vein_selected_final")
dat1 <- dat 

#IQR(Inter-Quartile Range)를 기준으로 이상치를 제거
boxplot(dat1$depth)$stats 
boxplot(dat1$d_vertical)$stats 
boxplot(dat1$d_horizontal)$stats 
dat1_sensitivity <- dat1 %>% 
  filter(depth <= 9.672, d_vertical <= 5.941, d_horizontal <= 8.290) 


### Indentify data structure
table(is.na(dat1)) # 결측치 확인하기



### Table 2 with appendix (sensitivity analysis: depth, d_vertical, d_horizontal에서 outlier를 제외한 통계)
dat1 %>%   #tibble 형태의 data는 기본적으로 유효숫자 
  summarise(mean_deapth = mean(depth), sd_depth = sd(depth),
            mean_d_vert = mean(d_vertical), sd_d_vert = sd(d_vertical),
            mean_d_hori = mean(d_horizontal), sd_d_hori = sd(d_horizontal))

dat1_sensitivity %>%   #tibble 형태의 data는 기본적으로 유효숫자 
  summarise(mean_deapth = mean(depth), sd_depth = sd(depth),
            mean_d_vert = mean(d_vertical), sd_d_vert = sd(d_vertical),
            mean_d_hori = mean(d_horizontal), sd_d_hori = sd(d_horizontal))

dat1 %>% 
  group_by(sex) %>% 
  summarise(mean_deapth = mean(depth), sd_depth = sd(depth),
            mean_d_vert = mean(d_vertical), sd_d_vert = sd(d_vertical),
            mean_d_hori = mean(d_horizontal), sd_d_hori = sd(d_horizontal))

dat1_sensitivity %>% 
  group_by(sex) %>% 
  summarise(mean_deapth = mean(depth), sd_depth = sd(depth),
            mean_d_vert = mean(d_vertical), sd_d_vert = sd(d_vertical),
            mean_d_hori = mean(d_horizontal), sd_d_hori = sd(d_horizontal))

#normality 확인하기 for depth
with(dat1, shapiro.test(depth[sex=='M']-depth[sex=='F']))

male <- dat1 %>% 
  filter(sex=='M')
result <-shapiro.test(male$depth)
result$statistic
result$p.value

female <- dat1 %>% 
  filter(sex=='F')
result <-shapiro.test(female$depth)
result$statistic
result$p.value

#둘 다 normality를 만족하지 않음 -> Mann-Whitney U test
wilcox.test(male$depth, female$depth, alternative = "two.sided")
wilcox.test(male$d_vertical, female$d_vertical, alternative = "two.sided")
wilcox.test(male$d_horizontal, female$d_horizontal, alternative = "two.sided")


#S: normality 확인하기 for depth
male_s <- dat1_sensitivity %>% 
  filter(sex=='M')
result <-shapiro.test(male_s$depth)
result$statistic
result$p.value

female_s <- dat1_sensitivity %>% 
  filter(sex=='F')
result <-shapiro.test(female_s$depth)
result$statistic
result$p.value

#둘 다 normality를 만족하지 않음 -> Mann-Whitney U test
wilcox.test(male_s$depth, female_s$depth, alternative = "two.sided")
wilcox.test(male_s$d_vertical, female_s$d_vertical, alternative = "two.sided")
wilcox.test(male_s$d_horizontal, female_s$d_horizontal, alternative = "two.sided")


