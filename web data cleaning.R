library(dplyr)
library(stringr)

station_summary <- function(df, name){
  df <- df[c(1,3)]
  colnames(df)<- c("run", "station")
  
  dt <- tibble(run = df$run,
               start = NA,
               end = NA)
  
  for( i in 1:nrow(df)){
    temp_sta <- unlist(strsplit(toString(df$station[i]), "\\-"))
    start_sta <- str_trim(temp_sta[1])
    end_sta <- str_trim(temp_sta[2])
    dt$start[i] <- start_sta
    dt$end[i] <- end_sta
  }
  
  dt2 <- dt %>% group_by(end) %>% tally()
  dt2[nrow(dt2) + 1,] = c("每日班次", sum(dt2$n))
  
  full_name <- paste("/Users/chengyahan/Desktop/", name, sep="")
  full_name <- paste(full_name, ".csv", sep="")
  write.csv(dt2, full_name, row.names = FALSE, fileEncoding = "UTF-8")
}



