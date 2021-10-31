# Activated Library
library(shinydashboard)
library(tidyverse)
library(shiny)
library(DT)
library(readxl)
library(glue)
library(scales)
library(plotly)
library(plyr)
library(arules)
library(arulesViz)
library(visNetwork)
library(lubridate)


#===PREPROCESSING DATA===
#Read Dataset
online_retail <- readRDS("online_retail.rds")

#remove missing value
online_retail <- na.omit(online_retail)

#change some data to data type factor
online_retail <-
  online_retail %>%
  mutate_at(vars(InvoiceNo, StockCode, Description, CustomerID, Country),funs(as.factor))

#filter quantity > 0
online_retail <-
  online_retail %>%
  filter(Quantity > 0)

#filter date
online_retail <- 
  online_retail %>% 
  filter(InvoiceDate >= "2010-12-01" & InvoiceDate <= "2011-11-30")

#make column for date and hour
online_retail$Date <- as.Date(online_retail$InvoiceDate)
online_retail$TransactionTime <- format(online_retail$InvoiceDate,"%H:%M:%S")
online_retail <- online_retail[order(online_retail$Description),]

#Plot Background
background_plot <- readRDS("background_plot.rds")


#===EXPLORATORY DATA===
#Plot 0
data0_plot <- readRDS("data0_plot.rds")

#Plot 1
data1_plot <- readRDS("data1_plot.rds")

#Plot 2
data2_plot <- readRDS("data2_plot.rds")

#Plot 3
data3_plot <- readRDS("data3_plot.rds")

#Plot 4
data4_plot <- readRDS("data4_plot.rds")

#Plot5
data5_plot <- readRDS("data5_plot.rds")


#===MARKET BASKET ANALYSIS===
#TABEL
rules_tabel <- readRDS("rules_tabel.rds")


#VISUALIZATION
#PlotScatter
scater_plot_apriori <- readRDS("scater_plot_apriori.rds")

#PlotGraph
graph_plot_apriori <- readRDS("graph_plot_apriori.rds")



#===PRODUCT RECOMENDATION===
#dataframe
rules_tabel_df <- readRDS("rules_tabel_df.rds")

