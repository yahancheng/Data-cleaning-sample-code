# 北京內部比較
rm(list = ls())
library(plyr)
library(dplyr)
library(ggplot2)
library(tidyr)
land_deal <- read.csv("/Users/chengyahan/Desktop/Work/Paulson/北京土地出讓明細_2007_2012.csv")
colnames(land_deal) <- c("公告日期", "城市", "區域", "土地面積", "規劃建築面積", "成交總價",
                         "溢價率", "成交樓面均價", "成交日期")
land_deal$'成交年' <- as.numeric(substring(land_deal$成交日期, 1, 4))
land_deal$'成交月' <- substring(land_deal$成交日期, 6, 7)
land_deal$"高鐵" <- ifelse((land_deal$區域 == "东城区" | land_deal$區域 == "丰台区") & (land_deal$成交年 > 2010), 1, 0)

growth_rate <- data.frame("區域" = unique(land_deal$區域))
for (i in 2009:2013){
  growth_rate[, toString(i)] <- NA
}

for(district in growth_rate$區域){
  for (year in 2009:2013){
    train <- subset(land_deal, 區域 == district & 成交年 == toString(year))
    train_past <- subset(land_deal, 區域 == district & 成交年 == toString(year-1))
    avg_p_current <- mean(as.numeric(train$`成交樓面均價`))
    avg_p_past <- mean(as.numeric(train_past$`成交樓面均價`))
    growth_rate[which(growth_rate$區域 == district), 
                which(colnames(growth_rate)==year)] <- (avg_p_current - avg_p_past)/avg_p_past
  }
}

grow_rate_2 <- tibble(year = 2009:2013,
                      no_HSR = NA,
                      Yes_HSR = NA)

train_hr <- subset(land_deal, (區域 == "东城区" | 區域 == "丰台区"))
train_nhr <- subset(land_deal, (區域 != "东城区" & 區域 != "丰台区"))

for (year in 2009:2013){
  train <- subset(train_nhr, 成交年 == toString(year))
  train_past <- subset(train_nhr, 成交年 == toString(year-1))
  avg_p_current <- mean(as.numeric(train$`成交樓面均價`))
  avg_p_past <- mean(as.numeric(train_past$`成交樓面均價`))
  grow_rate_2[which(grow_rate_2$year == year), 2] <- (avg_p_current - avg_p_past)/avg_p_past
}

for (year in 2009:2013){
  train <- subset(train_hr, 成交年 == toString(year))
  train_past <- subset(train_hr, 成交年 == toString(year-1))
  avg_p_current <- mean(as.numeric(train$`成交樓面均價`))
  avg_p_past <- mean(as.numeric(train_past$`成交樓面均價`))
  grow_rate_2[which(grow_rate_2$year == year), 3] <- (avg_p_current - avg_p_past)/avg_p_past
}

ggplot(data=grow_rate_2, mapping = aes(x=factor(year), y=no_HSR, group=1), color="#0066cc") + 
  geom_line(data=grow_rate_2, mapping = aes(x=factor(year), y=no_HSR, group=1), color="#0066cc") +
  ylab("growth rate") + xlab("year") +
  geom_line(data=grow_rate_2, mapping = aes(x=factor(year), y=Yes_HSR, group=1), color = "#cc0033")

rownames(growth_rate) <- growth_rate[,1]
growth_rate <- growth_rate[, -1]
growth_rate <- as.data.frame(t(as.matrix(growth_rate)))

d <- gather(growth_rate)
d$year <- rep(2009:2013, 16)

base <- ggplot(data=d, aes(x=factor(year), y=value, group=1))

for (district in unique(land_deal$區域)){
  if (district == "平谷区" | district=="门头沟区"){next}
  train <- subset(d, d$key==district)
  base <- base + geom_line(data=train, aes(x=factor(year), y=value, group=1), size=0.25)
}
print(base)

base +
  geom_line(data=subset(d, d$key=="东城区"), aes(x=factor(year), y=value, group=1), size=0.5, color="red") +
  geom_line(data=subset(d, d$key=="丰台区"), aes(x=factor(year), y=value, group=1), size=0.5, color="blue") +
  xlab("year") + ylab("growth rate (%)")


# 平均地價
avg_price <- read.csv("/Users/chengyahan/Desktop/Work/Paulson/平均地價.csv", stringsAsFactors = FALSE)
rownames(avg_price) <- avg_price[,1]
avg_price <- avg_price[, -1]
avg_price <- as.data.frame(t(as.matrix(avg_price)))

for (i in 1:length(row.names(avg_price))){
  rownames(avg_price)[i] <- unlist(strsplit(rownames(avg_price)[i],"\\."))[1]
}

avg_price_year <- data.frame('區域' = rownames(avg_price),
                             "京滬線" = NA)
for (year in 2000:2019){
  avg_price_year[,toString(year)] <- NA
}

for (district in avg_price_year$區域){  
  for (year in 2000:2019){
    train <- avg_price[,which(substring(colnames(avg_price), 1, 4) == toString(year))]
    train <- subset(train, rownames(train)==district)
    avg_price_year[which(avg_price_year$"區域"==district), toString(year)] <- as.numeric(rowMeans(train))
  }
}

jh_line <- c("北京", "河北", "天津", "山东", "江苏", "安徽", "上海")
avg_price_year$京滬線 <- ifelse(avg_price_year$區域 %in% jh_line, 1, 0)

# 京滬線內比較
jh_d <- subset(avg_price_year, avg_price_year$京滬線 == 1)
colnames(jh_d)[1] <- "city"
jh_d$city <- c("Beijing", "Shanghai", "Tianjin")
jh_d %>% gather(key="key", value="value",-c('city',"京滬線")) %>%
  ggplot(aes(x=factor(key), y=value, fill=city)) +
  geom_bar(stat="identity", position=position_dodge()) +
  scale_fill_brewer(palette="Paired")+
  xlab("year") + ylab("price") 

# 京滬線/非京滬線 比較
combine_d <- data.frame("jh_line" = c(0, 1))

for (i in c(0, 1)){
  for (year in 2000:2019){
    train <- subset(avg_price_year, avg_price_year$京滬線==i)
    combine_d[which(combine_d$jh_line == i), toString(year)] <- mean(train[,toString(year)], na.rm = T)
  }
}

combine_d %>% gather(key="key", value="value",-c('jh_line')) %>%
  ggplot(aes(x=factor(key), y=value, fill=factor(jh_line))) +
  geom_bar(stat="identity", position=position_dodge()) +
  scale_fill_brewer(palette="Paired")+
  xlab("year") + ylab("price")


  