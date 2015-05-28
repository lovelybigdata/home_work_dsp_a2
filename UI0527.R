

# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(markdown)

library(data.table) # data ETL 套件 
library(dplyr)      # data ETL 套件
library(reshape2)   # data ETL 套件
library(ggplot2)    # 視覺化套件
library(scales)     # 改變ggplot2座標軸刻度
library(ggfortify)
###載入資料####
raw_n <- fread("hypermall_H1_utf8.csv", data.table=FALSE)
raw_m <- fread("hypermall_T1_utf8.csv", data.table=FALSE)
raw_s <- fread("hypermall_S1_utf8.csv", data.table=FALSE)
sup <- fread("hypermall_supplement_utf8.csv", data.table=FALSE)

raw <- rbind(raw_n,raw_m,raw_s) #北中南合併
rm(raw_m) # 刪掉不再需要的變數，節省記憶體空間
rm(raw_n)
rm(raw_s)


# 觀察資格格式，其中日期資料
#str(raw)

#'data.frame':  1021033 obs. of  9 variables:
# $ Store       : chr  "H1" "H1" "H1" "H1" ...
# $ Dept        : chr  "Mwg-6Hp2" "Mwg-6Hp2" "Mwg-6Hp2" "Mwg-6Hp2" ...
# $ Date        : chr  "01/01/2009" "01/01/2009" "01/01/2009" "01/01/2009" ...
# $ Family      : chr  "螺旋形一" "鹼3-電池" "燈管一一" "球形一一" ...
# $ Sales_Value : int  27790 10500 9237 7608 6999 5498 5111 4463 3686 3045 ...
# $ Sales_Qty   : int  183 70 73 50 41 15 4 63 10 5 ...
# $ Sales_Number: int  67 68 24 21 40 13 4 35 8 5 ...
# $ Item_Type   : int  29 9 14 8 8 7 2 16 4 5 ...
# $ Price_Mode  : int  256 161 98 199 161 540 1880 107 444 404 ...



# 直接使用 dplyr 套件中的 mutate 來定義資料格式
# 設定日期格式 、 將銷售額、數量轉為數字；不過不清楚為什麼要把類別項目轉成數字呢?

raw <- mutate(raw, 
              Date=as.Date(Date, format="%m/%d/%Y"),
              Sales_Value=as.numeric(Sales_Value), 
              Sales_Qty=as.numeric(Sales_Qty),
              Sales_Number=as.numeric(Sales_Number), 
              Item_Type=as.numeric(Item_Type),
              Price_Mode=as.numeric(Price_Mode)
)

## 開始進行資料探索
## 先設法將了解各分店的銷售總額

# 搭配 group_by -- summarise 語法，類似 SQL group by 用法
# 如果要產生新變數時，是使用 mutate 
# 使用 n() 會有等於在 sql 底下 count 相同的效果，而且不用指定引數
# 選擇變數 --> group by -->集群函數 --> 排列
# 產生新變數時可以另外命名

all_3_sales_values <-select(raw, Sales_Value , Store )  %>% group_by(Store) %>%  summarise(Sales=sum( Sales_Value) ,AVG_Sales =  mean(Sales_Value) , count=n() ) %>%   arrange( desc(Sales) )

all_3_sales_values <- as.data.frame(all_3_sales_values) 

shinyUI(fluidPage(

  # Application title
  titlePanel("DSP Hypermall Team #2"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      
       
      selectInput("my1", "Label",
                  choices = names(all_3_sales_values[2:4] ),
                  selected = names(all_3_sales_values[3] )  )
      
    ),

    # Show a plot of the generated distribution
    
    mainPanel(
      
      includeMarkdown("intro.md") , 
      plotOutput("distPlot") , 
     
    # R 的程式碼得到的結果輸出在畫面上，並且指定 id  
    verbatimTextOutput("print1") , 
    textOutput("mytext1") ,
    # 畫第2張圖試試
    plotOutput("distPlot2")  
  
    
    )
  )
))
