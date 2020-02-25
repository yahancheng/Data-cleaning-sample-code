library(tidyverse)
library(ggplot2)
library(cowplot)

paste_path <- function(file_name){
  path <- paste("/Users/chengyahan/Desktop/Work/Paulson/HSR aggregate data/", file_name, sep="")
  path <- paste(path, ".csv", sep="")
  return(path)
}

clean_df <- function(dataframe){
  colnames(dataframe) <- c("time", "num_deal", "deal_area","building_area",
                           "price", "price_per_sq", "premium_rate")
  dataframe <- dataframe[complete.cases(dataframe[, 2:6]), ]
  dataframe$year <- substring(dataframe$time, 7)
  return(dataframe)
}


aggregate_data <- function(yes_live, yes_business, no_live, no_business){
  yes_live_df <- clean_df(read.csv(paste_path(yes_live), stringsAsFactors = FALSE))
  yes_live_df$HSR <- "HSR district"
  yes_live_df$live <- "live"
  yes_bus_df <- clean_df(read.csv(paste_path(yes_business), stringsAsFactors = FALSE))
  yes_bus_df$HSR <- "HSR district"
  yes_bus_df$live <- "business"
  no_live_df <- clean_df(read.csv(paste_path(no_live), stringsAsFactors = FALSE))
  no_live_df$HSR <- "no HSR"
  no_live_df$live <- "live"
  no_bus_df <- clean_df(read.csv(paste_path(no_business), stringsAsFactors = FALSE))
  no_bus_df$HSR <- "no HSR"
  no_bus_df$live <- "business"
  
  agg <- rbind(yes_live_df, yes_bus_df, no_bus_df, no_live_df)
  return(agg)
}

agg_plot <- function(agg_df, plot_title){
  gplot <- ggplot(data=agg_df, aes(x=factor(as.numeric(year)), y=price_per_sq)) +
    geom_bar(stat="identity", position=position_dodge(), fill="steelblue") +
    facet_wrap(~ live + HSR, ncol=2) +
    xlab("year") + ylab("Average Price per sq") +
    theme(text = element_text(family='Kai'),
          plot.title = element_text(hjust = 0.5, size=20)) +
    ggtitle(plot_title)
  
  return(gplot)
}

price_ratio_plot <- function(agg, plot_title){
  live_df <- agg[which(agg$live == "live"), ]
  bus_df <- agg[which(agg$live == "business"), ]
  live_pr_df <- data.frame(year = sort(as.numeric(unique(live_df$year))),
                           price_ratio = NA)
  bus_pr_df <- data.frame(year = sort(as.numeric(unique(bus_df$year))),
                          price_ratio = NA)
  
  for (y in live_pr_df$year){
    avg_pr_yes <- live_df[which(live_df$year==y & 
                                live_df$HSR == "HSR district"), 
                          "price_per_sq"]
    avg_pr_no <- live_df[which(live_df$year==y &
                               live_df$HSR == "no HSR"), 
                         "price_per_sq"]
    if(is_empty(avg_pr_yes) | is_empty(avg_pr_no)){next}
    pr <- avg_pr_yes / avg_pr_no
    live_pr_df$price_ratio[which(live_pr_df$year == y)] <- pr
  }
  
  for (y in bus_pr_df$year){
    avg_pr_yes <- bus_df[which(bus_df$year==y & 
                               bus_df$HSR == "HSR district"), 
                          "price_per_sq"]
    avg_pr_no <- bus_df[which(bus_df$year==y &
                              bus_df$HSR == "no HSR"), 
                         "price_per_sq"]
    if(is_empty(avg_pr_yes) | is_empty(avg_pr_no)){next}
    pr <- avg_pr_yes / avg_pr_no
    bus_pr_df$price_ratio[which(bus_pr_df$year == y)] <- pr
  }
  
  live_gplot <- ggplot(data=live_pr_df, aes(x=factor(year), y=price_ratio)) +
    geom_bar(stat="identity", position=position_dodge(), fill="steelblue") +
    xlab("year") +
    theme(text = element_text(family='Kai'),
          plot.title = element_text(hjust = 1, size=20)) +
    ggtitle(plot_title)
    
  bus_gplot <- ggplot(data=bus_pr_df, aes(x=factor(year), y=price_ratio)) +
    geom_bar(stat="identity", position=position_dodge(), fill="steelblue") +
    xlab("year")
  plots <- plot_grid(live_gplot, bus_gplot, align="h", labels=c("live", "business"))
  
  return(plots)
}


# ggsave(filename, plot = last_plot(), device = NULL, path = NULL,
#scale = 1, width = NA, height = NA, units = c("in", "cm", "mm"),
#dpi = 300, limitsize = TRUE, ...)

all_func <- function(yes_live, yes_business, no_live, no_business, plot_title){
  agg_df <- aggregate_data(yes_live, yes_business, no_live, no_business)
  plot_1 <- agg_plot(agg_df, plot_title)
  plot_2 <- price_ratio_plot(agg_df, plot_title)
  
  file_name_1 <- paste(plot_title, "agg_plot", sep="_")
  file_name_2 <- paste(plot_title, "pr_plot", sep="_")
  
  ggsave(file_name_1, plot=plot_1, device = jpeg, width=650, height=500, limitsize = FALSE,
         path="/Users/chengyahan/Desktop/Work/Paulson/aggregate plot")
  ggsave(file_name_2, plot=plot_2, device = jpeg, width=800, height=500, limitsize = FALSE,
         path="/Users/chengyahan/Desktop/Work/Paulson/aggregate plot")
}


all_func("南京_有高铁_住宅", "南京_有高铁_商服", "南京_没高铁_住宅", "南京_没高铁_商服", "南京")
all_func("天津_有高铁_住宅", "天津_有高铁_商服", "天津_没高铁_住宅", "天津_没高铁_商服", "天津")
all_func("常州_有高铁_住宅", "常州_有高铁_商服", "常州_没高铁_住宅", "常州_没高铁_商服", "常州")
all_func("廊坊_有高铁_住宅", "廊坊_有高铁_商服", "廊坊_没高铁_住宅", "廊坊_没高铁_商服", "廊坊")
all_func("徐州_有高铁_住宅", "徐州_有高铁_商服", "徐州_没高铁_住宅", "徐州_没高铁_商服", "徐州")
all_func("无锡_有高铁_住宅", "无锡_有高铁_商服", "无锡_没高铁_住宅", "无锡_没高铁_商服", "无锡")
all_func("济南_有高铁_住宅", "济南_有高铁_商服", "济南_没高铁_住宅", "济南_没高铁_商服", "济南")
all_func("苏州_有高铁_住宅", "苏州_有高铁_商服", "苏州_没高铁_住宅", "苏州_没高铁_商服", "苏州")
all_func("南京_有高铁_住宅", "南京_有高铁_商服", "南京_没高铁_住宅", "南京_没高铁_商服", "南京")





