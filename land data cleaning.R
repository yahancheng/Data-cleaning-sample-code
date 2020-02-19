rm(list = ls())
# 土地溢價率
price_rate <- read.csv("/Users/chengyahan/Desktop/Work/Paulson/土地溢價率.csv")
price_rate <- price_rate[c(-13,-14), ]
rownames(price_rate) <- price_rate[,1]
price_rate <- price_rate[, -1]
price_rate <- as.data.frame(t(as.matrix(price_rate)))

for (i in 1:length(row.names(price_rate))){
  rownames(price_rate)[i] <- unlist(strsplit(rownames(price_rate)[i],"\\."))[1]
}


# 成交面積與筆數
deal <- read.csv("/Users/chengyahan/Desktop/Work/Paulson/成交土地資料.csv")
deal <- deal[c(-13, -14), ]
rownames(deal) <- deal[, 1]
deal <- deal[, -1]
deal <- as.data.frame(t(as.matrix(deal)))

deal_area <- deal[which(grepl("占地", rownames(deal))),]
deal_num <- deal[which(grepl("量", rownames(deal))),]

for (i in 1:length(row.names(deal_area))){
  name <- unlist(strsplit(rownames(deal_area)[i],"\\."))
  rownames(deal_area)[i] <- gsub(" ","",paste(name[1], name[3]))
}

for (i in 1:length(row.names(deal_num))){
  name <- unlist(strsplit(rownames(deal_num)[i],"\\."))
  rownames(deal_num)[i] <- gsub(" ","",paste(name[1], name[3]))
}


# 北京detail成交資料
land_deal <- read.csv("/Users/chengyahan/Desktop/Work/Paulson/北京土地出讓明細_2007_2012.csv")
colnames(land_deal) <- c("公告日期", "城市", "區域", "土地面積", "規劃建築面積", "成交總價",
                         "溢價率", "成交樓面均價", "成交日期")
land_deal$'成交年' <- as.numeric(substring(land_deal$成交日期, 1, 4))
land_deal$'成交月' <- substring(land_deal$成交日期, 6, 7)
land_deal$"高鐵" <- ifelse((land_deal$區域 == "东城区" | land_deal$區域 == "丰台区") & (land_deal$成交年 > 2010), 1, 0)

# 平均地價
avg_price <- read.csv("/Users/chengyahan/Desktop/Work/Paulson/平均地價.csv", stringsAsFactors = FALSE)
rownames(avg_price) <- avg_price[,1]
avg_price <- avg_price[, -1]
avg_price <- as.data.frame(t(as.matrix(avg_price)))

for (i in 1:length(row.names(avg_price))){
  rownames(avg_price)[i] <- unlist(strsplit(rownames(avg_price)[i],"\\."))[1]
}



