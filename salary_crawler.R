library(xml2)
library(XML)
library(dplyr)

salary <- list()
i <- 1

for (l in LETTERS) {
  filepath <- paste0("isa105/105_165-", l, ".html")
  if(file.exists(filepath) == FALSE) next
  
  .x <- read_html(filepath)
  .county <- xml_find_all(.x, "//td/p/span")
  county <- xml_text(.county[3])
  county <- substring(county, 5) %>% 
    gsub(" ", "", .)
  
  
  .t <- readHTMLTable(filepath)
  district <- .t[[1]]$V2 %>% grep(pattern = "^[^縣]*[區鄉鎮市]$", value = TRUE)
  mid <- .t[[1]] %>% 
    filter(V3 == "合　計") %>% 
    select(V6) %>% 
    slice(1:length(district))
  
  salary_county <- data.frame(county, district, mid)
  salary[[i]] <- salary_county
  i <- i + 1
}

salary_df <- do.call(rbind, salary)

.site_id <- paste0(salary_df$county, salary_df$district) %>% 
  gsub("　", "", .)
salary_df$site_id <- .site_id

View(salary_df)
dim(salary_df)

write.csv(salary_df, "salary_med.csv")
