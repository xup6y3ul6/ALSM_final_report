## transform rawdata to Data for lm

library(dplyr)

# referendum 14
data <- readRDS("rawdata.rds")
referendum14 <- data %>% 
  select(1:10) %>% 
  mutate(agree_rate = 同意票數/有效票數)
referendum14$agree_rate <- round(referendum14$agree_rate, digits = 3)

# vote rate
voteRate <- data %>% 
  select(投票權人數, 投票人數) %>% 
  mutate(vote_rate = 投票人數/投票權人數)

# gender
male_number <- data %>% 
  select_if(grepl("_m", names(.))) %>% 
  apply(1, sum)
female_number <- data %>% 
  select_if(grepl("_f", names(.))) %>% 
  apply(1, sum)
gender_ratio <- male_number / female_number
gender <- data.frame(male_number, female_number, gender_ratio)

# age
total_number <- data %>% 
  select(people_age_018_m:people_age_100up_f) %>% 
  apply(1, sum)
order_number <- data %>% 
  select(people_age_065_m:people_age_100up_f) %>% 
  apply(1, sum)
young_number <- data %>% 
  select(people_age_018_m:people_age_030_f) %>% 
  apply(1, sum)
old_ratio <- order_number / total_number
young_ratio <- young_number / total_number
young_old_ratio <- young_ratio / old_ratio
age <- data.frame(old_ratio, young_ratio, young_old_ratio) 

# married
married <- data %>% 
  select(site_id, 喪偶:離婚) %>% 
  group_by(site_id) %>% 
  mutate(married_ratio = (喪偶+有偶+離婚) / (喪偶+有偶+未婚+離婚))

# education
total_edu_number <- data %>% 
  select(博畢:高中畢) %>% 
  apply(1, sum)
college_edu_number <- data %>% 
  select(博畢, 大畢, 專畢, 碩畢) %>% 
  apply(1, sum)
college_ratio <- college_edu_number / total_edu_number
education <- data.frame(total_edu_number, college_edu_number, college_ratio)

# salary
salaryMed <- data %>%
  select(salary_mid)

# municipality
.county <- substring(data$site_id, 1, 3)
.is_municipality <- .county %in% c("臺北市", "新北市", "桃園市", "臺中市", "臺南市", "高雄市")
.is_municipality_fac <- factor(.is_municipality, levels = c(TRUE, FALSE), labels = c(1, 0))

# offshore_island
.is_offshoreIsland <- .county %in% c("澎湖縣", "金門縣", "連江縣")
.is_offshoreIsland_fac <- factor(.is_offshoreIsland, levels = c(TRUE, FALSE), labels = c(1, 0))

.location <- .is_municipality + .is_offshoreIsland*2
.location_fac <- factor(.location, levels = c(0, 1, 2), labels = c("county", "special municipality", "offshore island"))


# combine all data
Data <- data.frame(agree_rate = referendum14$agree_rate,
                   vote_rate = voteRate$vote_rate,
                   is_municipality = .is_municipality_fac,
                   is_offshoreIsland = .is_offshoreIsland_fac,
                   location = .location_fac,
                   gender_ratio = gender$gender_ratio,
                   young_ratio = age$young_ratio,
                   old_ratio = age$old_ratio,
                   young_old_ratio = age$young_old_ratio,
                   married_ratio = married$married_ratio,
                   college_ratio = education$college_ratio,
                   salary_med = salaryMed$salary_mid,
                   row.names = data$site_id)

saveRDS(Data, "Data.rds")
write.csv(Data, "Data.csv")








