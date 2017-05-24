getwather <- function(fromYear, toYear)
{
  ## http://blog.bryanbigdata.com/2014/05/r.html
  library(XML)
  library(RCurl)

  ##generate the list of url

  #���ͪťժ�list�s����������URL
  url_list <- list()

  #�[���l�����p"http://lishi.tianqi.com/taibei/201101.html"
  #���'01'�B'02'�O�r��榡,�w���s�@�@�Ӱ}�C�s��
  month <- c('01','02','03','04','05','06','07', '08', '09', '10', '11', '12')

  #�Q�ΰj��۰ʲ��ͤ�����}
  for (year in fromYear:toYear){
    url <- paste('http://lishi.tianqi.com/taibei/',year,month,'.html',sep='')
    url_list <- rbind(url_list,url)
  }

  #�N���ͪ����}�h�榡�ơA��KŪ��
  url_list <- unlist(url_list)


  ##Get the table online

  #�֤ߵ{��
  myTemp <- function(url){
    #���url
    get_url = getURL(url,encoding = "UTF-8")
    #�Nurl�ѪR
    get_url_parse = htmlParse(get_url, encoding = "UTF-8")
    #������䪺�ܶ��A�ڭ̻ݭn���ܶ����b�@��div��class=tqtongji2�A�̭�<ul>����<li>���Ҹ̭�
    #���Ҹ̭��٦��@�ǨS���Ψ쪺�F��S���Y�A�ƫ�A�@�ֲ���
    tablehead <- xpathSApply(get_url_parse, "//div[@class='tqtongji2']/ul/li", xmlValue)
    #�N�^���쪺����r�ন�e���\Ū���x�}�榡
    table <- matrix(tablehead, ncol = 6, byrow = T)
    #�^�ǭ�
    return(table)
  }

  #lapply�O�Ӧn�Ϊ����O�A�bSAS�n���������Ʊ����|�ϥ�MACRO�ܶ�
  #���OR�O�V�q�y���A��n�]���ܶ���b�@�ӦV�q(url_list) �A�⤽����b�ĤG�ӰѼƦ�m
  #R�|�۰ʬ��Ĥ@�ӦV�q�����C�Ӥ����N�J�줽������
  #���ͥX����Temp_Total�O��list�A�s��C�@�������G
  Temp_Total <- lapply(url_list, myTemp)

  ##Transform the data from list to matrix
  ##�N���G��LIST�ন�x�}��K���R
  #�إߤ@�Ӧ�������쪺�x�}
  Temp <- matrix(ncol = 6)

  #���O���Xlist�����C�Ӥ����A�K��x�}�̭�
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