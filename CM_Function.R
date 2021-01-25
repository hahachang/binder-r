##################### Manipulate ##############################
drop.na.cols<-function(datasets){
  datasets %<>% 
    select_if(~all(!is.na(.)))
}

## big5 to utf8 
## On Mac, Prem.Raw's colnames display garbled due to encoding.
big5_to_utf8 <- function(datasets){
  for (col in colnames(datasets)){
    Encoding(datasets[[col]]) <- "Big5"
  }
  
  temp = colnames(datasets)
  
  
  temp <- iconv(temp, "big5", "utf8")
  
  
  colnames(datasets)<-temp
  
  
}


utf8_to_big5 <- function(datasets){
  for (col in colnames(datasets)){
    Encoding(datasets[[col]]) <- "utf8"
  }
  
  temp = colnames(datasets)
  
  
  temp <- iconv(temp, "utf8", "big5")
  
  
  colnames(datasets)<-temp
  
  
}    



show.comma <- function(datasets){
  
  datasets %<>%
    mutate_if(is.numeric,funs(comma2(.,0)))
  
}

show.percent <- function(datasets){
  
  datasets %<>%
    mutate_if(is.numeric,funs(percent(.,0)))
  
}


sum.row <- function(datasets){
  datasets %<>% 
    mutate(total = rowSums(select_if(., is.numeric), na.rm = TRUE))
}

sum.col <- function(datasets){
  datasets %<>% 
    bind_rows(summarise_all(., funs(if(is.numeric(.)) sum(.) else "all")))
}  

sum.col.wd <- function(datasets){
  datasets %<>% 
    bind_rows(summarise_all(., funs(if(is.numeric(.)) sum(.) else "業界")))
}   

#################### like Bash ##############################
haha.progressbar <- function(end,i){
  cat('\r',
      "Program::",
      i/end*100,
      '% |',
      rep('=', i / (end/50)),
      ifelse(i == end, '|\n',  '>'), sep = '')
  Sys.sleep(.01)
}



pwd <-function(){
  getwd()
  
}

ls.files<-function(){
  list.files()
}


cd <-function(path){
  setwd(path)
  
  
}


dirs <-function(path){
  list.dirs(path,recursive=F,full.names = F)
}




haha.string.replace <- function(filename, name.old, name.new, filename.new){
  x <- readLines(filename)
  y <- gsub(name.old,name.new,x)
  
  if (missing(filename.new)){
    cat(y, file=filename, sep="\n")
  }else{
    cat(y, file=filename.new, sep="\n")
  }
  
  
}




##################### Function ##############################

as.vector.df <-function(datasets){
  as.character(unlist(datasets))
}

## add.date
add.date <-function(x){
  x %<>%
    mutate(Date=paste0((Year-1911)*100+Month)) 
  
}

## add.nickname
add.nickname <- function(datasets){
  
  datasets %<>% 
    merge(Hash.Company.Unique %>% 
            filter(!Name %in% c("大型公司","中型公司","小型公司")) %>% 
            mutate(nickname = LETTERS[1:nrow(.)]) %>% 
            select(Name,nickname),by="Name"
    )
  
}




## add.order
add.order <- function(x){
  
  if (isMac()){
    path = "./Data_Insurance/"
  } else{
    path = ".\\Data_Insurance\\"    
  }
  
  load(paste(path,"Hash_Company_Unique.Rdata",sep="")) 
  
  x %<>% 
    merge(select(Hash.Company.Unique,Name,Order),by="Name") %>% 
    data.frame()
  
}


add.order.all <- function(x){
  x %<>%
    mutate(Order = 23) %>% 
    mutate(Name="業界")
  
}


add.order.size <- function(x){
  x %<>%
    mutate(Order = case_when(Size=="large"~8.5,
                             Size=="medium"~15.5,
                             Size=="small"~22.5)) %>% 
    rename(Name=Size)
  
}

add.order.size.chinese <- function(x){
  x %<>%
    mutate(Order = case_when(Size=="大型公司"~8.5,
                             Size=="中型公司"~15.5,
                             Size=="小型公司"~22.5)) %>% 
    rename(Name=Size)
  
}

## add.name
add.name <- function(x){
  
  if (isMac()){
    path = "./Data_Insurance/"
  } else{
    path = ".\\Data_Insurance\\"    
  }
  
  load(paste(path,"Hash_Company.Rdata",sep="")) 
  
  x %<>% 
    merge(select(Hash.Company,CorpCode,Name),by="CorpCode") %>% 
    select(-CorpCode) %>% 
    data.frame()
  
}

add.name.rbc <- function(x){
  if (isMac()){
    path = "./Data_Insurance/"
  } else{
    path = ".\\Data_Insurance\\"    
  }
  
  load(paste(path,"Hash_Company_RBC.Rdata",sep="")) 
  
  x %<>% 
    merge(select(Hash.Company.RBC,companyID,shortname),by="companyID") %>% 
    select(-companyID) %>%
    rename(Name=shortname) %>% 
    data.frame()
  
  
  
}



## add.name
add.order.fv <- function(x){
  
  if (isMac()){
    path = "./Data_Insurance/"
  } else{
    path = ".\\Data_Insurance\\"    
  }
  
  load(paste(path,"Hash_Company_FV.Rdata",sep="")) 
  
  x %<>% 
    merge(select(Hash.Company.FV %>% filter(Order!=99),Name,Order),by="Name") %>% 
    data.frame()
  
}

add.name.english <- function(x){
  
  if (isMac()){
    path = "./Data_Insurance/"
  } else{
    path = ".\\Data_Insurance\\"    
  }
  
  load(paste(path,"Hash_Company_Unique.Rdata",sep="")) 
  
  x %<>% 
    merge(select(Hash.Company.Unique,Name,Name_English),by="Name") %>% 
    data.frame()
  
  
}

## add.size
add.size <- function(x){
  
  if (isMac()){
    path = "./Data_Insurance/"
  } else{
    path = ".\\Data_Insurance\\"    
  }
  
  load(paste(path,"Hash_Company_Unique.Rdata",sep="")) 
  
  x %<>% 
    merge(select(Hash.Company.Unique,Name,Size),by="Name") %>% 
    data.frame()
  
}  


arrange.order<- function(datasets){
  datasets %<>% 
    arrange(Order)
  
}


arrange.size<- function(datasets){
  datasets %<>% 
    arrange(match(Size,c("large","medium","small")))
  
}

arrange.com.colname <- function(datasets){
  
  name = colnames(datasets)
  
  sort.name = c("國泰", 
                "南山", 
                "富邦", 
                "新光", 
                "中國", 
                "台灣", 
                "三商美邦", 
                "全球", 
                "中華郵政", 
                "臺銀", 
                "遠雄", 
                "安聯", 
                "宏泰", 
                "法國巴黎", 
                "元大", 
                "保誠", 
                "保德信", 
                "合庫", 
                "安達", 
                "友邦",
                "第一金",
                "康健")
  
  
  datasets[, c(setdiff(name,sort.name),sort.name)]
  
}


arrange.rbc.com.colname <- function(datasets){
  
  name = colnames(datasets)
  
  sort.name = c("國泰",
                "原國泰",
                "原國寶幸福",
                "南山",
                "原南山",
                "原朝陽",
                "富邦", 
                "新光", 
                "中國", 
                "台灣", 
                "三商美邦", 
                "全球",
                "原全球",
                "原國華",
                "中華郵政", 
                "臺銀", 
                "遠雄", 
                "安聯", 
                "宏泰", 
                "法國巴黎", 
                "元大", 
                "保誠", 
                "保德信", 
                "合庫", 
                "安達", 
                "友邦",
                "第一金",
                "康健")
  
  
  datasets[, c(setdiff(name,sort.name),sort.name)]
  
}

arrange.property.com.colname <- function(datasets){
  
  name = colnames(datasets)
  
  sort.name = c("臺灣產物", 
                "兆豐產物", 
                "富邦產物", 
                "和泰產物", 
                "泰安產物", 
                "明台產物", 
                "南山產物", 
                "第一產物", 
                "旺旺友聯產物", 
                "新光產物", 
                "華南產物", 
                "國泰世紀產物", 
                "新安東京海上產物", 
                "台壽保產物", 
                "裕利安宜產物", 
                "美國國際產物", 
                "科法斯產物", 
                "安達產物", 
                "亞洲產物", 
                "安盛產物", 
                "法國巴黎產物",
                "漁保社")
  
  
  datasets[, c(setdiff(name,sort.name),sort.name)]
  
}





## as.numeric.factor
as.numeric.factor <- function(x) {
  as.numeric(as.character.factor(x))
}




#change data's fomrat from character to number
char2num = function(datasets){
  if("DataDate" %in% colnames(datasets)){
    datasets %<>% 
      mutate(DataDate=as.numeric(DataDate))
  }  
  
  if("Date" %in% colnames(datasets)){
    datasets %<>% 
      mutate(Date=as.numeric(Date))
  }
  
  if("Year" %in% colnames(datasets)){
    datasets %<>% 
      mutate(Year=as.numeric(Year))
  }    
  
  if("Month" %in% colnames(datasets)){
    datasets %<>% 
      mutate(Year=as.numeric(Month))
  }    
  
  if("Order" %in% colnames(datasets)){
    datasets %<>% 
      mutate(Order=as.numeric(Order))
  }    
}  




#Change Date Format
change_date = function(datasets){
  if("DataDate" %in% colnames(datasets)){
    datasets %>%
      tidyr::extract(DataDate,c("Year","Month"), "(^\\w{2,3})(\\w{2}+)$",remove=F) %>%
      mutate(Year = as.numeric(Year)+1911) %>%
      mutate(Month = as.numeric(Month))
  }else if("Date" %in% colnames(datasets)){
    datasets %>%
      tidyr::extract(Date,c("Year","Month"), "(^\\w{2,3})(\\w{2}+)$",remove=F) %>%
      mutate(Year = as.numeric(Year)+1911) %>%
      mutate(Month = as.numeric(Month))
  }
}

crawl.ptt.query <- function(keyword,collection){
  library(mongolite)
  
  if(missing(collection)){
    collection="Gossiping"
  }
  
  
  dmd<- mongo(collection =collection, db = 'ptt',url =   "mongodb://localhost:27017")
  
  query.keyword = paste0('{"title":{"$regex":','"', keyword,'"', ",",'"$options" : "i"}}')
  
  result <- dmd$find(query.keyword)
  
  crawl.ptt.query<-result %>%
    arrange(desc(score)) 
  
}




comma2 <-function(input,digit){
  if(missing(digit)){
    digit=0  
  }
  output = comma(round(input,digit))
}



data2map <- function(datasets,zoom){
  map <- get_map(location = 'Taiwan', zoom = zoom,language = "zh-TW", maptype = "roadmap")
  ggmap(map) + 
    geom_point(aes(x = lon, y = lat), data = datasets)
}





rename.remove.prefix <- function(datasets,prefix){
  datasets %<>% 
    rename_at(.vars = vars(starts_with(prefix)),
              .funs = funs(sub(prefix, "", .)))
  
}

sleep.random<-function(wait.sec.start,wait.sec.end){
  
  sleeptime = runif(1,wait.sec.start,wait.sec.end)
  Sys.sleep(sleeptime)
}




##################### Plot ##############################  
haha.plot.window.new <-function(){
  dev.new(width=9,height=6,noRStudioGD = TRUE,xpos=1540,ypos=0)  
}



haha.plot.gif <-function(path,gif.name){
  library(gifski)
  png_files <- list.files(path, pattern = ".*png$", full.names = TRUE)
  gifski(png_files, gif_file = gif.name , width = 800, height = 600, delay = 0.1)
}








haha.plot.todevs <- function(...){
  #plot2devs("p1","p2","p3","p4","p5")
  graphics.off()
  loops =list(...) # THIS WILL BE A LIST STORING EVERYTHING:
  
  for (loop_plot in loops){
    loop.position = match(loop_plot,loops)
    dev.new(width=5,height=4,noRStudioGD = TRUE,xpos=1540+500*((loop.position-1)%%3),ypos=0+((loop.position-1)%/%3)*400)
    print(eval(parse(text=loop_plot)))
    
    }  
    
    # Example of inbuilt function
  }

  
  
  
  
  #Function to shift x-axis to 0 adapted from link shown above
  timeline2<-function(data,start,end){
    shift_axis <- function(p, xmin, xmax, y=0){
      g <- ggplotGrob(p)
      dummy <- data.frame(y=y)
      ax <- g[["grobs"]][g$layout$name == "axis-b"][[1]]
      p + annotation_custom(grid::grobTree(ax, vp = grid::viewport(y=1, height=sum(ax$height))), 
                            ymax=y, ymin=y) +
        ggplot2::annotate("segment", y = 0, yend = 0, x = xmin, xend = xmax, 
                          arrow = arrow(length = unit(0.1, "inches"))) +
        theme(axis.text.x = element_blank(), 
              axis.ticks.x=element_blank())
      
    }
    
    #Tidy Data
    data%<>% 
      arrange(start_date) %>% 
      mutate(rank=1:nrow(.)) %>% 
      mutate(upordown = ifelse(rank%%2==0,1,-1)) %>%
      mutate(line_height = (upordown*displ*rnorm(nrow(.)))) %>% #create different longer lollipop 
      filter(start_date>=ymd(start),start_date<=ymd(end))
    
    
    #Conditionally set whether text will be above or below the point
    vjust = ifelse(data$displ > 0, -1, 1.5)
    
    
    #font size ON/OFF
    
    
    #plot
    p1 <- data %>% 
      #filter(start_date>=ymd(start),start_date<=ymd(end)) %>% 
      ggplot(aes(start_date, line_height,color=group)) +
      geom_lollipop(point.size = 1) +
      geom_text(aes(x = start_date, y = line_height, label = event), data = data,
                hjust = 0, vjust = vjust, size = 3) +
      theme(axis.title = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            axis.line = element_blank(),
            axis.text.x = element_text(size = 8)) +
      expand_limits(x = c(ymd(start), ymd(end)), y = 1.2) +
      scale_x_date(breaks = scales::pretty_breaks(n = 9))+
      theme_legend_bottom()+
      theme_legend_title_blank()+
      theme_backgroud_white()
    
    
    shift_axis(p1, ymd(start), ymd(end))
    
  }
  
  
  
  
  
  
  
  
  ##################### Output Data ############################## 
  
  
  line<-function(message,plot){
    
    library(lineNotify)
    line_token_info = "apfSt2nGRwzBuCuFWtH9qRCX8BGATHEyCiBAIQzAwup"
    Sys.setenv(LINE_API_TOKEN=line_token_info)
    
    
    if(missing(plot)){
      notify_msg(message)    
    }else{
      
      notify_ggplot(message, plot = plot)      
      
      
    }
    
    

    
  }
  
  
  
  
  
  #
  write.csv3<-function(datasets,path,file.name){
    datasets %>%   
      write.csv(.,paste0(path,file.name,".csv"))
  }
  
  
  ggsave2 <- function(path,file.name,width,height){
    
    ggsave(paste0(path, file.name ,".png"), width=width, height=height,dpi=100)
    
  }
  
 
  
  ##################### Date ##############################  
  datechange.char_no_hypen2date <- function(date_char_no_hypen){
    
    date = as.Date(ISOdate(substr(date_char_no_hypen,1,4),
                           substr(date_char_no_hypen,5,6),
                           substr(date_char_no_hypen,7,8)))
  }
  
  
  date2time.excel = function(datasets){
    datasets %<>% 
      mutate(date = as.Date(date, origin = "1899-12-30"))
    
    
  }
  
  #date2actime其實用ymd可以取代
  date2actime = function(datasets,colname){
    if (missing(colname)) {
      
      datasets %<>% 
        mutate(year = as.numeric(substr(date,1,4))) %>% 
        mutate(month = as.numeric(substr(date,5,6))) %>% 
        mutate(day   = as.numeric(substr(date,7,8)))%>% 
        mutate(time  = ISOdate(year,month,day) ) %>% 
        select(time,year,month,day,everything())
      
    }else{
    
    datasets %<>% 
      mutate(year = as.numeric(substr(colname,1,4))) %>% 
      mutate_(month = as.numeric(substr(colname,5,6))) %>% 
      mutate_(day   = as.numeric(substr(colname,7,8)))%>% 
      mutate(time  = ISOdate(year,month,day) ) %>% 
      select(time,year,month,day,everything())
    
    }  
  }
 
  #應更名twdate.ym2ac.ym
  date2ac = function(datasets){
    if("DataDate" %in% colnames(datasets)) {
      
      datasets %<>% 
        mutate(year = as.numeric(substr(DataDate,1,3))+1911) %>% 
        mutate(month = as.numeric(substr(DataDate,4,5))) %>% 
        mutate(date.ac = year*100+month  ) %>% 
        select(date.ac,everything(),-year,-month,-DataDate)
      
    }else if("Date" %in% colnames(datasets)){
      
      datasets %<>% 
        mutate(year = as.numeric(substr(Date,1,3))+1911) %>% 
        mutate(month = as.numeric(substr(Date,4,5))) %>% 
        mutate(date.ac = year*100+month  ) %>% 
        select(date.ac,everything(),-year,-month,-Date)
      
    }  
  }
  

  
  
   
  date2stocktime = function(datasets,colname){
    if (missing(colname)) {
      
      datasets %<>% 
        mutate(year = as.numeric(substr(date,1,4))) %>% 
        mutate(month = as.numeric(substr(date,5,6))) %>% 
        mutate(day   = as.numeric(substr(date,7,8)))%>% 
        mutate(hour  = 13) %>% 
        mutate(min   = 30 ) %>% 
        mutate(time  = ISOdate(year,month,day,hour,min) ) %>% 
        select(time,year,month,day,everything())
      
    }else{
      
      datasets %<>% 
        mutate(year = as.numeric(substr(colname,1,4))) %>% 
        mutate_(month = as.numeric(substr(colname,5,6))) %>% 
        mutate_(day   = as.numeric(substr(colname,7,8)))%>% 
        mutate(hour  = 13) %>% 
        mutate(min   = 30 ) %>% 
        mutate(time  = ISOdate(year,month,day,hour,min) ) %>% 
        select(time,year,month,day,everything())
      
    }  
  }   
  
  #應更名為twdate.ym2ac.time
  date2time = function(datasets){
    datasets %>% 
      change_date() %>% 
      month_firstday()
    
  }  
  
  
  date2zoo = function(datasets){
    datasets %>% 
      mutate(zoo = as.yearmon(as.character(paste(Year,Month,sep="-")))) %>%
      select(zoo,everything())
  }
  
  today <- function () {
    assign("today",Sys.Date() , envir = .GlobalEnv)
    assign("today.quarter",quarter(Sys.time()) , envir = .GlobalEnv)
    assign("today.month",month(Sys.time()) , envir = .GlobalEnv)
    assign("today.year",year(Sys.time()) , envir = .GlobalEnv)
  }
  
  #與date2ac重覆
  ym2date = function(datasets){
    datasets %>% 
      mutate(Date = 100*(as.numeric(Year)-1911)+as.numeric(Month)) %>%
      select(Date,everything())
  }
  
  
  
  filter.year <- function(datasets){
    if("Time" %in% colnames(datasets)){
      datasets %>% 
        filter(Time >= as.Date(floor_date(today,"year")),Time<=today)
      
      
    }else if ("time" %in% colnames(datasets)){
      datasets %>% 
        filter(time >= as.Date(floor_date(today,"year")),time<=today)
    }
  }
  
  
  filter.quarter <- function(datasets){
    if("Time" %in% colnames(datasets)){
      datasets %>% 
        filter(Time >= as.Date(floor_date(today,"quarter")),Time<=today)
      
      
    }else if ("time" %in% colnames(datasets)){
      datasets %>% 
        filter(time >= as.Date(floor_date(today,"quarter")),time<=today)
    }
  }
  
  filter.month <- function(datasets){
    if("Time" %in% colnames(datasets)){
      datasets %>% 
        filter(Time >= as.Date(floor_date(today,"month")),Time<=today)
      
      
    }else if ("time" %in% colnames(datasets)){
      datasets %>% 
        filter(time >= as.Date(floor_date(today,"month")),time<=today)
    }
  }
  
  filter.week <- function(datasets){
    if("Time" %in% colnames(datasets)){
      datasets %>% 
        filter(Time >= as.Date(floor_date(today,"week")),Time<=today)
      
      
    }else if ("time" %in% colnames(datasets)){
      datasets %>% 
        filter(time >= as.Date(floor_date(today,"week")),time<=today)
    }
  }
 
  
  #create timeline point for loop
  loop.timeline<- function(year_start,year_end){
    #今日時間
    today = Sys.time()
    
    today <- today %>% 
      str_match("^(\\w{4})-(\\w{2})-(\\w{2})")
    
    ## leading zero
    today.year  = today[1,2]
    today.month = today[1,3]
    today.day   = today[1,4]
    
    today = paste(today.year , 
                  today.month , 
                  today.day ,
                  sep="")
    
    timeline = vector(mode="character", length=0)
    for (year in (year_start:year_end)){
      for (month in 1:12){
        for (day in 1:31){
          date = paste(year,month,day,sep="/")
          date = gsub("-","",as.character(as.Date(date,format="%Y/%m/%d")))
          timeline = c(timeline,date)    
        }
      }
    }
    
    timeline=timeline[!is.na(timeline)]
    timeline=timeline[timeline<=today]
  }
######### WD ############################
  invest.splitsource.hedgecost <-function(Datasets){
    temp_0 = data.frame()
    for (loop_i in 1:12){
      temp_i <- Datasets  %>%  
        mutate(資金運用收益率_分母 = ( (可運用資金總計+lag(可運用資金總計,n=loop_i)-投資業務損益)/2 )) %>% 
        mutate(資金運用收益率_利息收入 = 12 / loop_i * 利息收入/資金運用收益率_分母) %>% 
        mutate(資金運用收益率_AC = 12 /loop_i * AC/資金運用收益率_分母) %>%
        mutate(資金運用收益率_FVPL = 12 /loop_i * (FVPL-衍生性商品避險損益)/資金運用收益率_分母) %>%
        mutate(資金運用收益率_FVOCI = 12 /loop_i * FVOCI/資金運用收益率_分母) %>%        
        mutate(資金運用收益率_Hedge = 12 / loop_i * (兌換損益+衍生性商品避險損益+外匯價格變動準備金淨變動-避險成本)/資金運用收益率_分母) %>%
        mutate(資金運用收益率_避險成本 = 12 / loop_i * (避險成本/資金運用收益率_分母)) %>%
        mutate(資金運用收益率_覆蓋法 = 12 / loop_i * (採用覆蓋法重分類之損益)/資金運用收益率_分母) %>%        
        mutate(資金運用收益率_Others = 12 /loop_i * (投資業務損益-(AC+FVPL+FVOCI+利息收入+兌換損益+外匯價格變動準備金淨變動+採用覆蓋法重分類之損益))/資金運用收益率_分母) %>% 
        mutate(資金運用收益率 = 12 / loop_i * 投資業務損益 / ( (可運用資金總計+lag(可運用資金總計,n=loop_i)-投資業務損益)/2 ) ) %>%
        filter(Month==loop_i) %>% 
        select(-資金運用收益率_分母)
      temp_0 = rbind(temp_0,temp_i)
    }
    Datasets = temp_0
  }  
  
  
  cc <- function(datasets){
    datasets %>% 
      colnames()
    
  }
  
  
  
  df = function(input){
    output = data.frame(input)
    
  }
  
  #Unit
  #dollar.bank = dollar_format()
  
  dollar.million = function(input){
    output =  input/(10^6) 
    
  }
  
  dollar.hundredmillion = function(input){
    output =  input/(10^8) 
  }
  
  dollar.trillion = function(input){
    output =  input/(10^12) 
  }
  
  
  
  
  

  
  
  endmonth = function(Datasets){
    #for USDTWD datasets
    Datasets.xts <- xts(Datasets[,-1],order.by=Datasets[,1]) 
    endmonth_points <- Datasets.xts %>%
      endpoints(on="months",k=1)
    Datasets.xts[endmonth_points,]
  } 
  
  
  
  export.doc = function(Datasets){
    doc <- docx()
    doc <- addFlexTable( doc, vanilla.table(Datasets))
    writeDoc(doc, file = "D:\\R資料輸出.docx")
  }
  
  
  
  financial_ratio = function(Datasets){
    Datasets  %<>% 
      mutate(負債比例              = 負債/資產) %<>% 
      mutate(業主權益比例          = 業主權益/資產) %<>%   
      mutate(業主權益比例_不含分離 = 業主權益/(資產-分離帳戶)) %<>% 
      #mutate(業主權益報酬率        = 12 / 年化因子 *稅後損益/( (業主權益+lag(業主權益,n=年化因子))/2) ) %<>%  
      mutate(業主權益變動率        = ( 業主權益-lag(業主權益,n=12) ) / abs(lag(業主權益,n=12)) )    %<>%  
      mutate(純益率                = 稅後損益/營業收入)     %<>%  
      mutate(淨利變動率            = (稅後損益 - lag(稅後損益,n=12))/abs(lag(稅後損益,n=12)) )     %<>% 
      mutate(不動產投資與抵押對資產比率  = (投資用不動產+不動產抵押放款)/  ( (資產+lag(資產,n=12) )/2)  )   %<>% 
      mutate(逾放比                = 逾放金額/放款總額)   %<>% 
      mutate(各種責準金淨變動對保費收入比率 = ifelse(
                                      is.infinite((保險負債淨變動+金融商品性質之準備淨變動)/保費收入),
                                      0,
                                      (保險負債淨變動+金融商品性質之準備淨變動)/保費收入 )) %<>% 
      mutate(現金流量              = (營業收入+營業外收入)/(營業成本+營業外支出+預計所得稅)) %>% 
      mutate(各種準備金淨增額對保費收入比率 = (保險負債淨變動+金融商品性質之準備淨變動)/保費收入)
    
    
    
    
    Datasets %<>% 
      mutate(銀行存款p = 銀行存款/資金運用總計) %<>% 
      mutate(有價證券p = 有價證券/資金運用總計) %<>% 
      mutate(國外投資p = 國外投資/資金運用總計) %<>% 
      mutate(不動產p   = 不動產/資金運用總計) %<>% 
      mutate(放款p     = 放款/資金運用總計) %<>% 
      mutate(專案運用與公共投資p = 專案運用與公共投資/資金運用總計) %<>% 
      mutate(投資保險相關事業p = 投資保險相關事業/資金運用總計) %<>% 
      mutate(從事衍生性商品交易p = 從事衍生性商品交易/資金運用總計) %<>% 
      mutate(其他資金運用p = 其他資金運用/資金運用總計) %<>% 
      mutate(資金運用組合變動率    = (  abs(銀行存款p-lag(銀行存款p,n=12)) +
                               abs(有價證券p-lag(有價證券p,n=12)) +
                               abs(國外投資p-lag(國外投資p,n=12)) +
                               abs(不動產p  -lag(不動產p,n=12)) +
                               abs(放款p-lag(放款p,n=12)) +
                               abs(專案運用與公共投資p-lag(專案運用與公共投資p,n=12))+ 
                               abs(投資保險相關事業p -lag(投資保險相關事業p ,n=12))+ 
                               abs(從事衍生性商品交易p-lag(從事衍生性商品交易p,n=12))+ 
                               abs(其他資金運用p-lag(其他資金運用p,n=12))   )/資金運用項目種數)
    
    
    temp_0 = data.frame()
    for (loop_i in 1:12){
      Datasets  %>% 
        mutate(資金運用收益率old = 12 / loop_i * 淨投資損益 / ( (資金運用總計+lag(資金運用總計,n=loop_i)-淨投資損益)/2 ) ) %>% 
        mutate(資金運用收益率_Son = 12 / loop_i * 淨投資損益) %>% 
        mutate(資金運用收益率_Mom = ( (資金來源總計+lag(資金來源總計,n=loop_i)-淨投資損益)/2 )) %>% 
        mutate(資金運用收益率 = 12 / loop_i * 淨投資損益 / ( (資金來源總計+lag(資金來源總計,n=loop_i)-淨投資損益)/2 ) ) %>%
        mutate(業主權益報酬率 = 12 / loop_i * 稅後損益   / ( (業主權益+lag(業主權益,n=loop_i))/2) ) %>% 
        mutate(資產報酬率 = 12 / loop_i * 稅後損益   / ( (資產+lag(資產,n=loop_i))/2) ) %>%
        filter(Month==loop_i) ->temp_i
      temp_0 = rbind(temp_0,temp_i)
    }
    Datasets = temp_0
    
  }
  
  
  
  
  filtercurrent = function(dataset){
    result <- dataset %>% 
      filter(Date==本期)
    
    
  }
  
  filterstart = function(dataset,date){
    result <- dataset %>% 
      filter(Date>= date)
    
    
  }
  
  
  filtername = function(dataset,target){
    name.english2chinese <- Hash.Company %>% 
      mutate(English_Name = tolower(English_Name) ) %>%
      filter(grepl(tolower(target),English_Name)) %>% 
      .$Name
      
    
    result<-dataset %>% 
      filter(grepl(target,Name)|Name==name.english2chinese)
    
  }

  ## getLocation 
  
  ## 這個會受查詢次數所限，所以必須用Sleep來防止問題的發生
  getLocation = function(addr){
    
    library(magrittr)
    library(jsonlite)
    library(data.table)
    addr %<>% as.character
    url = 
      paste0("https://maps.googleapis.com/maps/api/geocode/json?address=",
             addr) 
    res = fromJSON(url, flatten = T) 
    lat = res$results$geometry.location.lat
    lng = res$results$geometry.location.lng
    result = data.table("addr" = addr,
                        "lat" = lat,
                        "lng" = lng)
    return(result)  
  }  
  
  
  #Kit tool to filter Data
  get_data = function(Data,Item,Time){
    Item = as.name(Item)
    filter(Data,Date %in% Time)   %>% group_by(Date)  %>% summarise(TempName = sum(Item))
  }
  
  geom_bar_dodge <- function(){
    
    geom_bar(stat="identity",position = "dodge")
    
  }
  
  
  #find keyword in which R-file
  keyword.in.which.Rfile<-function(keyword){
    dir("D:\\R_notebook\\Watch Dog\\")
    filenames = list.files(pattern="*.R")
    for( f in filenames ){
      
      x <- readLines(f)
      y <- grep(keyword, x )
      if (length(y)!=0){
        print(paste0(f))
      }
      
      
    }
    
  }
  
  
  
  hh <-function(datasets){
    datasets %>% 
      head()
    
  }
  
  
  vvh <- function(datasets){
    datasets %>% 
      head() %>% 
      View()
    
  }
  
  
  
  vvt <- function(datasets){
    datasets %>% 
      tail() %>% 
      View()
    
  }
  
  kkh <- function(datasets){
    
    datasets %>% 
      head() %>% 
      kable()
    
  }
  
  
  kk <- function(datasets){
    
    datasets %>% 
      kable()
    
  }
  
  
  kkt <- function(datasets){
    
    datasets %>%
      tail() %>% 
      kable()
    
  }
  
  ss <-function(datasets){
    datasets %>% 
      str()
  }
  
  gg <-function(datasets){
    datasets %>% 
      glimpse()
  }
  
  
  lookup.stockname <- function(datasets,stockcode){
    output = datasets$stockname[datasets$stockcode==stockcode]
  }
  
  month_lastday = function(datasets){
    datasets %>% 
      mutate(Time = (as.Date(as.character(paste(as.numeric(Year)+ifelse(Month==12,1,0),ifelse(Month==12,1,Month+1),1,sep="-")))-1)) %>%
      select(Time,everything())
  }
  
  
  month_firstday = function(datasets){
    datasets %>% 
      mutate(Time = (as.Date(
        as.character(
          paste(as.numeric(Year),as.numeric(Month),1,sep="-"))))) %>%
      select(Time,everything())
  }
  
  name_by_row <- function(x,row){
    if(missing(row)){
      row=1
    }
    
    #remove current colnames
    colnames(x) = ""
    
    #add new colnames from specified row
    colnames(x) = x[row,]
    
    #delete
    x = x[-row,]
  } 
  
  
  #Normal_From
  nf = function(Datasets,Hash){
    Datasets %>%
      merge(unique(select(Hash,Name,Size,Order)),by="Name") %>% 
      arrange((Order))
  } 
 
  ##now
  
  now <-function(dataset){
    dataset %>% 
      filter(Date==本期)
    
  }
  
  
   
 #change data's fomrat from number to character
  num2char = function(datasets){
    if("DataDate" %in% colnames(datasets)){
      datasets %<>% 
        mutate(DataDate=toString(DataDate))
    }  
    
    if("Date" %in% colnames(datasets)){
      datasets %<>% 
        mutate(Date=as.character(Date))
    }
    
    if("Year" %in% colnames(datasets)){
      datasets %<>% 
        mutate(Year=as.character(Year))
    }    
    
    if("Month" %in% colnames(datasets)){
      datasets %<>% 
        mutate(Year=as.character(Month))
    }    
    
    if("Order" %in% colnames(datasets)){
      datasets %<>% 
        mutate(Order=as.character(Order))
    }    
  }


  
  #low level parsing link by regular expression
  parse.link.base <- function(url,pattern){
    html <- paste(readLines(url), collapse="\n")
    matched <- str_match_all(html, pattern)
    
  }

  
  #simle method for parsing link by regular expression
  parse.link.fast <- function(url){
    page <- read_html(url)
    temp.link = (html_attr(html_nodes(page, "a"), "href"))
  }

  
  
  

  
  


  
  #pecentage 
  percent = function(x,digit){
    if(missing(digit)){
      digit=2  
    }
    
    sprintf(paste0("%1.",digit,"f%%"), 100*x)
  }

  

  
  plot.heatmap <- function(datasets,x,y,z){
    
    datasets %>% 
      ggplot(aes_string((x), (y), fill=(z))) + geom_tile() +
      geom_text(aes_string(label=(z)),colour="white")+ 
      theme(axis.text.x=element_text(size = 12),axis.text.y=element_text(size = 12)) +
      theme_cht_JhengHei()
  }
    ##### -- 下面寫法值得學習
    # # 寫法1，重點在轉化
    # heatmap <- function(datasets,x_col,y_col,z_col){
    #   plot_data <- datasets[,c(x_col, y_col,z_col)]
    #   colnames(plot_data) <- c('x', 'y','z')
    #   
    #   plot_data %>% 
    #     ggplot(aes(x, y, fill=z)) + geom_tile() +
    #     geom_text(aes(label=z),colour="white")+ 
    #     theme(axis.text.x=element_text(size = 15),axis.text.y=element_text(size = 15)) +
    #     theme_cht_JhengHei()
    #   
    # }
    # 
    # # 寫法2，重點在get
    # heatmap <- function(datasets,x,y,z){
    #   
    #   datasets %>% 
    #     ggplot(aes(get(x), get(y), fill=get(z))) + geom_tile() +
    #     geom_text(aes(label=get(z)),colour="white")+ 
    #     theme(axis.text.x=element_text(size = 15),axis.text.y=element_text(size = 15)) +
    #     theme_cht_JhengHei()
    #   
    # }
    # 
  
  
  r2html <- function(filepath){
    #為的是將script convert to html
    spin(filepath)  # default markdown
    o = spin(s, knit = FALSE)  # convert to Rmd only
    knit2html(o)  # compile to HTML
    
    #  filepath = "D:\\R_notebook\\Watch Dog\\WD_Analysis_CountryRisk.R"
  }
  
  scale_x_date_Y <-function(){
    scale_x_date(date_labels=("%Y"),date_breaks = "1 year")   
  }  
  
  scale_x_date_Q <-function(){
      scale_x_yearqtr(format = "%q")
  }  
  
  
  scale_x_date_m <-function(){
    scale_x_date(date_labels=("%m"),date_breaks = "1 month")   
  }
    

  scale_x_date_my <-function(){
    scale_x_date(date_labels=("%m/%y"),date_breaks = "1 month")   
  }
  
  scale_x_date_Ymd <-function(){
    scale_x_date(date_labels = "%Y/%m/%d")
  }
  
  
  scale_y_percentage <- function(){
    scale_y_continuous(labels = scales::percent)
    
  }
  
rbc.filter.life.alive <- function(datasets){
  result<-datasets %>% 
    filter(companyID>200,companyID<300,companyID!=20401,companyID!=20402,companyID!=26401,companyID!=26402) %>% 
    filter(!companyID %in% c(207,209,210,212,215,254,258,262,265,269,271,272))
    
  
}


rbc.filter.life.all <- function(datasets){
  result<-datasets %>% 
    filter(companyID>200,companyID<300|companyID==20401|companyID==20402|companyID==26401|companyID==26402|companyID==20601|companyID==20602) 
  
  
}

rbc.filter.life.global <- function(datasets){
  #含全球區隔與非區隔
  result<-datasets %>% 
    filter(companyID>200,companyID<300|companyID==26401|companyID==26402) 
  
  
}


rbc.add.name<-function(datasets){
  result <- datasets %>% 
  merge(select(Hash.Company.RBC,companyID,Name),by="companyID")  
  
}
  
 split.moneyuse = function(Datasets){
    temp_0 = data.frame()
    for (loop_i in 1:12){
      Datasets  %>% 
        mutate(資金運用收益率   = 12 / loop_i * 淨投資損益 / ( (資金運用總計+lag(資金運用總計,n=loop_i)-淨投資損益)/2 ) ) %>%
        mutate(淨投資對資產     = 淨投資損益  / (資產-分離帳戶) ) %>%
        mutate(資產對資金運用   = 12 / loop_i * (資產-分離帳戶) / ( (資金運用總計+lag(資金運用總計,n=loop_i)-淨投資損益)/2 ) ) %>%
        filter(Month==loop_i) ->temp_i
      temp_0 = rbind(temp_0,temp_i)
    }
    Datasets = temp_0
    
 }
 
 split.netincome = function(Datasets){
   temp_0 = data.frame()
   for (loop_i in 1:12){
     Datasets  %>% 
       transmute(Date,Name,Size,Order,純益率 = 稅後損益/營業收入,ROA= 稅後損益/資產,桿槓 = 資產/營業收入) 

   }

   
 }
 
 
 split.moneyperform = function(Datasets){
   temp_0 = data.frame()
   for (loop_i in 1:12){
     Datasets  %>% 
       mutate( 資金運用收益率          = 12 / loop_i * 投資業務損益 / ( (資金運用總計+lag(資金運用總計,n=loop_i)-投資業務損益)/2 )) %>% 
       mutate( 資金運用收益率_Son      = 12 / loop_i * 投資業務損益 ) %>% 
       mutate( 資金運用收益率_Mom      = ( (資金運用總計+lag(資金運用總計,n=loop_i)-投資業務損益)/2 ) ) %>% 
       mutate( 資金運用收益率_利息收入 = 12 / loop_i * 利息收入 / 資金運用收益率_Mom ) %>% 
       mutate( 資金運用收益率_避險     = 12 / loop_i * (兌換損益+外匯價格變動準備金淨變動+profit) / 資金運用收益率_Mom ) %>% 
       mutate( 資金運用收益率_其他     = 12 / loop_i * (投資業務損益-(利息收入+兌換損益+外匯價格變動準備金淨變動+profit)) / 資金運用收益率_Mom) %>% 
       mutate( 資金運用收益率_資本利得 = 資金運用收益率_避險+資金運用收益率_其他 ) %>% 
       mutate( 資金運用收益率_leverage = -資金運用收益率_其他/資金運用收益率_避險) %>% 
       filter(Month==loop_i) ->temp_i
     temp_0 = rbind(temp_0,temp_i)
   }
   Datasets = temp_0
 }
   
 split.moneyperform2 = function(Datasets){
   temp_0 = data.frame()
   for (loop_i in 1:12){
     Datasets  %>% 
       mutate( 資金運用收益率          = 12 / loop_i * 投資業務損益 / ( (資金運用總計+lag(資金運用總計,n=loop_i)-投資業務損益)/2 )) %>% 
       mutate( 資金運用收益率_Son      = 12 / loop_i * 投資業務損益 ) %>% 
       mutate( 資金運用收益率_Mom      = ( (資金運用總計+lag(資金運用總計,n=loop_i)-投資業務損益)/2 ) ) %>% 
       mutate( 資金運用收益率_利息收入 = 12 / loop_i * 利息收入 / 資金運用收益率_Mom ) %>% 
       mutate( 資金運用收益率_避險     = 12 / loop_i * (兌換損益+外匯價格變動準備金淨變動+profit) / 資金運用收益率_Mom ) %>% 
       mutate( 資金運用收益率_其他_國外= 12 / loop_i * 國外投資損益 / 資金運用收益率_Mom )%>% 
       mutate( 資金運用收益率_其他_國內= 12 / loop_i * (投資業務損益-(利息收入+兌換損益+外匯價格變動準備金淨變動+profit+國外投資損益)) / 資金運用收益率_Mom) %>% 
       mutate( 資金運用收益率_資本利得 = 資金運用收益率_避險+資金運用收益率_其他 ) %>% 
       mutate( 資金運用收益率_leverage = -資金運用收益率_其他/資金運用收益率_避險) %>% 
       filter(Month==loop_i) ->temp_i
     temp_0 = rbind(temp_0,temp_i)
   }
   Datasets = temp_0
 } 
 
 
 
 # display data by Single + Size + Agg    
 ssa = function(Data,Select_Date,Item){
   Data.Ratio = Data
   Data.Ratio %<>%
     financial_ratio %>%
     select(Time:Size,`負債比例`:`資產報酬率`)
   
   Data.Ratio %<>% 
     filter(Date==Select_Date) %>% 
     select(Date,Name,Order,get(Item))
   
   Data.Agg.Size %<>% 
     filter(Date==Select_Date) %>% 
     select(Date,Size,get(Item)) 
   
   Data.Agg.Total %<>% 
     filter(Date==Select_Date) %>% 
     select(Date,get(Item)) 
   
   ssa<- Data.Ratio %>% 
     merge(select(Hash.Company,Name,Size),by="Name") %>% 
     merge(Data.Agg.Size,by=c("Date","Size")) %>% 
     merge(Data.Agg.Total,by="Date") %>% 
     setnames(ncol(.),"整體") %>% 
     setnames(ncol(.)-1,"規模") %>%
     setnames(ncol(.)-2,"個別") %>% 
     arrange(Order)
   
 }
 
 

 
 theme_clear <- function () { 
   theme_bw()+
   theme(legend.position = "bottom",
          axis.title.x = element_blank(),
          legend.title = element_blank())   
 }
 
 
 
 theme_cht <-function(){
   theme(text = element_text(family = '黑體-繁 中黑'))
 }
   
 
 theme_cht_JhengHei <-function(){
   windowsFonts(BL = windowsFont("微軟正黑體"))
   theme(text = element_text(family = 'BL'))
 }
 
 theme_font_size <-function(size){
   theme(text = element_text(size=size))
 }
 

 
 
 theme_x_blank <-function(){
   
   theme(axis.title.x = element_blank())
   
 }
 
 theme_y_blank <-function(){
   
   theme(axis.title.y = element_blank())
   
 } 
 
 theme_xy_blank <-function(){
   
   theme(axis.title.x = element_blank(),
         axis.title.y = element_blank())
   
 }  
 
 
 theme_legend_title_blank <- function(){
   theme(legend.title=element_blank())
   
 }
 
 theme_legend_blank <- function(){
   theme(legend.position='none')
   
 }
 
 
 theme_legend_bottom <- function(){
   theme(legend.position='bottom')
   
 }
 
 theme_title_bold <- function(fontsize){
   if(missing(fontsize)){
     fontsize=14  
   }
   theme(plot.title = element_text(color=default_blue, size=fontsize, face="bold",hjust=0,family="BL"))
   
 } 

 
 
 theme_tii <- function(){
   theme_fivethirtyeight()+
     theme_cht_JhengHei()
 } 
 
 theme_tii_white <- function(){
   theme_tii()+
   theme_backgroud_white()
 } 
 
 
 
 theme_backgroud_white <-function(){
   theme(panel.background = element_rect(fill = "white", colour = "white"))+
     theme(plot.background = element_rect(fill = "white"))

 }
 

 
 theme_title_center <- function(){
   
     theme(plot.title = element_text(hjust = 0.5))
   
 }

 

 
 tool.iconChart <- function(values, bar_width=round(.10 * max(values)), icon, categories=NULL) {
   
   max_val <- max(values)
   padding <- .15 * bar_width
   
   # Setup blank plot.
   xlim <- c(0, (bar_width+padding)*length(values))
   ylim <- c(0, ceiling(max_val / bar_width)+1)
   plot(0, 0, type="n", xlab="", ylab="", bty="n", xlim=xlim, ylim=ylim, asp=1, axes=FALSE)
   
   # Get coordinates for each element.
   for (i in 1:length(values)) {
     xoffset <- (i-1)*(bar_width+padding)
     num_rows <- ceiling(values[i] / bar_width)
     xleft <- rep(xoffset:(xoffset+bar_width-1), num_rows)[1:values[i]]
     ybottom <- rep(1:num_rows, each=bar_width)[1:values[i]]
     xright <- xleft + 1
     ytop <- ybottom + 1
     
     # Plot grid for current value.
     if (i <= length(icon)) {
       rasterImage(icon[[i]], xleft, ybottom, xright, ytop)
     } else {
       rasterImage(icon[[1]], xleft, ybottom, xright, ytop)
     }
     
     
     if (i <= length(categories)) {
       mtext(categories[i], 1, line = -2, at = (xoffset+bar_width/2))
     }
   }  
 }
 
 
 trim <- function (x){
   gsub("^\\s+|\\s+$", "", x)
 }
 
 trim_u00a0 <-function(x){
   x %<>%
     mutate_each(funs(str_replace_all(.,pattern = "\U00A0",replacement = "")))
   
 }
 
 
 #tt
 tt = function(datasets){
   datasets %>%
     t %>% 
     data.frame() %>% 
     View()
 }
 #vv
 vv = function(datasets){
   datasets %>%
     View()
 }
 
 
 ee = function(datasets){
   datasets %>%
     edit()
 }
 
 warn.close <- function(){
   options(warn=-1) #close warning message
 }
 
 warn.open  <- function(){
   options(warn=0)  #open warning message
 }


 
 ###################### color palette #############
 default_red    = "#F8766D"
 default_green  = "#00BFC4"
 default_blue   = "#0044BB"
 default_yellow = "#ffd306" 
 
 
 
 ##################### Check #####################
 
   check.ratio.moneyuse <- function(datasets,com,lag_period){
     datasets  %>% 
       transmute(Time,Name,資金運用總計,資金運用總計lag  = lag(資金運用總計,n=lag_period),淨投資損益)  %>% 
       filter(Name==com) %>% 
       dollar.hundredmillion()
   }
 
    check.ratio.mortgage <-function(datasets){
      datasets %>% 
        transmute(Date,投資用不動產,不動產抵押放款,資產,去年同期資產=lag(資產,n=12))
      
    }

###################### Stock #############
    tidy.stock.price <- function(datasets){
      datasets %>% 
        select(-time,-year,-month,-day) %>% 
        date2actime() %>% 
        mutate(time = as.Date(time)) 
      
    }
    