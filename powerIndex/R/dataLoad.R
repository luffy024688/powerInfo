getwather <- function(fromYear, toYear)
{
  ## http://blog.bryanbigdata.com/2014/05/r.html
  library(XML)
  library(RCurl)

  ##generate the list of url

  #產生空白的list存放欲抓網站的URL
  url_list <- list()

  #觀察原始網站如"http://lishi.tianqi.com/taibei/201101.html"
  #月份'01'、'02'是字串格式,預先製作一個陣列存放
  month <- c('01','02','03','04','05','06','07', '08', '09', '10', '11', '12')

  #利用迴圈自動產生月份網址
  for (year in fromYear:toYear){
    url <- paste('http://lishi.tianqi.com/taibei/',year,month,'.html',sep='')
    url_list <- rbind(url_list,url)
  }

  #將產生的網址去格式化，方便讀取
  url_list <- unlist(url_list)


  ##Get the table online

  #核心程式
  myTemp <- function(url){
    #抓取url
    get_url = getURL(url,encoding = "UTF-8")
    #將url解析
    get_url_parse = htmlParse(get_url, encoding = "UTF-8")
    #抓取關鍵的變項，我們需要的變項夾在一個div的class=tqtongji2，裡面<ul>中的<li>標籤裡面
    #標籤裡面還有一些沒有用到的東西沒關係，事後再一併移除
    tablehead <- xpathSApply(get_url_parse, "//div[@class='tqtongji2']/ul/li", xmlValue)
    #將擷取到的關鍵字轉成容易閱讀的矩陣格式
    table <- matrix(tablehead, ncol = 6, byrow = T)
    #回傳值
    return(table)
  }

  #lapply是個好用的指令，在SAS要做類似的事情都會使用MACRO變項
  #但是R是向量語言，把要跑的變項放在一個向量(url_list) ，把公式放在第二個參數位置
  #R會自動為第一個向量中的每個元素代入到公式之中
  #產生出的的Temp_Total是個list，存放每一次的結果
  Temp_Total <- lapply(url_list, myTemp)

  ##Transform the data from list to matrix
  ##將結果的LIST轉成矩陣方便分析
  #建立一個有六個欄位的矩陣
  Temp <- matrix(ncol = 6)

  #分別取出list中的每個元素，貼到矩陣裡面
  for (i in 1:length(Temp_Total[1])){
    tmp <- Temp_Total[[i]]
    Temp <- rbind(Temp,tmp)
  }

  #write.csv(Temp,file = "Temp.csv" ,sep = ",", row.names = F)
  wather = as.data.frame(Temp[-c(1:2),c(1:3)])
  names(wather) = c("date", "high", "low")
  return(wather)
}

test <- function()
{
  print("I got it!!")
}

loadPower <- function(building, dateFrom, dateTo)
{
  # http://140.112.166.97/power/index.aspx
  library(magrittr)
  library(httr)
  library(rvest)
  library(XML)  # readHTMLTable
  library(dplyr) # data manipulation & pipe line
  library(stringr)
  library(plyr)

  # target = paste0("N",1)
  
  #building = "01A_P1_14"
  #dateFrom = "2016/12/1"
  #dateTo = "2016/12/1 23:00:00"

  powerdata = data.frame()
  res = POST("http://140.112.166.97/power/fn2/dataq.aspx",
              body = list(dtype = "h",
                          build= building,
                          dt1 = dateFrom,
                          dt2 = dateTo))
    
  node = content(res, "parsed", encoding = "big5") %>% html_nodes("table table")
  power = html_table(node,header = T)[[1]]
  write.csv(power,file = "power.csv" ,sep = ",", row.names = F)
  
  return(power)
}
test2 <- function()
{
  print("OK!!")
}