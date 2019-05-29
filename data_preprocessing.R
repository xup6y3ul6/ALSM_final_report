library(dplyr)
library(tidyr)

# referendum data
.referendum <- read.csv("referendum14.csv", header = TRUE, stringsAsFactors = TRUE)
referendum <- .referendum %>% 
  slice(23:390) %>% 
  select(-`村里`, -`投開票所`) %>% 
  mutate(site_id = paste0(`縣市`, `行政區`))

View(referendum)
dim(referendum) #368

# age data
.age <- read.csv("age.csv", header = TRUE, stringsAsFactors = FALSE)
.a <- .age %>% 
  slice(-1) %>% 
  select(-(statistic_yyymm:district_code), -(village:people_age_017_f))
.a[-1] <- sapply(.a[-1], as.numeric)
age <- .a %>% 
  group_by(site_id) %>% 
  summarise_all(sum) 
dim(age) # 370

age$site_id <- gsub("　", "", age$site_id)

rmReplicat <- function(data) {
  .sanmin <- data %>% 
    filter(grepl("三民", site_id)) %>%
    select(-site_id) %>% 
    apply(2, sum)
  data <- data[-which(grepl("三民", data$site_id))[2], ]
  data[which(grepl("三民", data$site_id))[1], 1] <- "高雄市三民區"
  data[which(grepl("三民", data$site_id))[1], -1] <- .sanmin
  
  .fengshan <- data %>% 
    filter(grepl("鳳山", site_id)) %>%
    select(-site_id) %>% 
    apply(2, sum)
  data <- data[-which(grepl("鳳山", data$site_id))[2], ]
  data[which(grepl("鳳山", data$site_id))[1], 1] <- "高雄市鳳山區"
  data[which(grepl("鳳山", data$site_id))[1], -1] <- .fengshan
  
  return(data)
}

age <- rmReplicat(age)
dim(age) # 368
View(age)

# married & education data
.married<- read.csv("married.csv", header = TRUE, stringsAsFactors = FALSE)
.m <- .married %>% slice(-1)
.m$population <- .m$population %>% as.numeric()

.me <- .m%>% 
  select(-statistic_yyy, -district_code, -village) %>% 
  group_by(site_id, marital_status, edu) %>% 
  summarise(population = sum(population)) 
.me$site_id <- gsub("　", "", .me$site_id)

married <- .me %>% 
  spread(marital_status, population) %>% 
  select(-edu) %>% 
  group_by(site_id) %>% 
  summarise_all(sum)

education <- .me %>% 
  spread(edu, population) %>% 
  ungroup() %>% 
  select(-marital_status) %>% 
  group_by(site_id) %>% 
  summarise_all(sum)

unique(married$site_id)

married <- rmReplicat(married)
dim(married)

education <- rmReplicat(education)
dim(education)



salary <- read.csv("salary_med.csv", header = TRUE, stringsAsFactors = FALSE)
View(salary)
names(salary)[4] <- "salary_med"

# combine all data
data <- referendum %>% 
  left_join(age, by = "site_id") %>% 
  left_join(married, by = "site_id") %>% 
  left_join(education, by = "site_id") %>% 
  left_join(salary[, c(4, 5)], by = "site_id")
  
is.na(data) %>% sum()
View(data)

saveRDS(data, file = "rawdata.rds")
write.csv(data, file = "rawdata.csv")



