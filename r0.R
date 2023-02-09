library(tidyverse)
library(readxl)


dat <- read_excel("G:\\내 드라이브\\dev\\new_r0_data_analysis\\vein_230206.xlsx", sheet="vein_selected_final")
dat1 <- dat


### Indentify data structure
glimpse(dat1)


### Table 1. Demographics
dat1 %>% 
  group_by(sex) %>% 
  summarise(n_distinct(ID)) %>% 
  summarise(percentage_M=199/458 * 100)

  
### Table 2. 
dat1 %>% 
  summarise(mean_deapth = round(mean(depth),3), sd_deapth = sd(depth),
            mean_d_opt = mean(d_opt), sd_d_opt = sd(d_opt),
            mean_d_max = mean(d_max), sd_d_max = sd(d_max),
            mean_d_min = mean(d_min), sd_d_min = sd(d_min),
            mean_d_vert = mean(d_vertical), sd_d_vert = sd(d_vertical),
            mean_d_hori = mean(d_horizontal), sd_d_hori = sd(d_horizontal),
            mean_area = mean(area), sd_area = sd(area))

dat1 %>% 
  summarise(mean_depth = round(mean(depth), 3))


dat1 %>% 
  group_by(sex) %>% 
  summarize(mean_dept=mean(depth)) %>% 
  print()
  


dat1 %>% 
  filter(age<20)
