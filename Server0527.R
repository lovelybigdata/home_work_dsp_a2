

# This is the server logic for a Shiny web application.
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

# 應該只需要讀進資料一次 




####  第2組 先載入資料進行研究

# 以 mac 使用者為例
# 使用 getwd() 確認目前工作目錄  "/Users/LaiR/Desktop/myRDataSet/DSP_2015_APP"
# ray 的設定 DSP_2015_APP
# 設定目前的工作目錄，要先確認所有的 csv 是否都已經存入
#setwd( "/Users/LaiR/Desktop/myRDataSet/DSP_2015_APP")
# 上傳到網路上時，setwd 要記得註解掉 


 

####載入資料####
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
# 轉成一般的 data.frame 就好，不要附加 tbl_df 屬性

#  得到的結果如下，可以發現總銷售額、平均銷售額、單一品項的單日銷售次數 依次是 北(h) 中(t)  南(s) 
#
#  Store sum(Sales_Value) mean(Sales_Value)  count
#1    H1       1325835530          3364.023 394122
#2    S1        556331658          2076.515 267916
#3    T1       1132277672          3154.021 358995    

#將資料畫成長條圖試試  如果y軸要數值而非計次的話要特別指定 stat="identity"


shinyServer(function(input, output) {

 output$distPlot <- renderPlot({

    # generate bins based on input$bins from ui.R
    #x    <- raw_n[, 5] # 讀入的資料需要是數值變數，而非類別變數
    #bins <- seq(min(x), max(x), length.out = input$bins + 1)
    # draw the histogram with the specified number of bins
    #hist(x, breaks = bins, col = 'darkgray', border = 'white')
    # Y 替換為變數    
    #  ggplot(data = all_3_sales_values , 
    #      aes(x=Store , y =   count   )) + geom_bar( stat ='identity')
  s <- ggplot(data = all_3_sales_values ,aes_string(x="Store", y = input$my1 ))+geom_bar(stat ="identity" ,fill="green"  )  
  
 
  
  print(s)
  print(s)
  print(s) # 也只會出現第一次
  }) 
  
  
# 圖形物件與圖形物件彼此不用區隔 
# 直接填上紅色試試
  
 output$distPlot2 <- renderPlot({
    
     
    w <-  ggplot(data = all_3_sales_values,aes_string(x="Store", y = input$my1 ))+geom_bar(stat ="identity" ,fill="red"  )    
     
    print(w)
    
  }) 
  
 # 直接在畫面上秀出文字

  output$print1 <- renderPrint({
    print(str(all_3_sales_values ))
  summary(raw)
})
  
  
    
    
   
    
    
 

})
