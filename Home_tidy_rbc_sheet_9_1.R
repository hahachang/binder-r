library(purrr)
library(dplyr)
library(magrittr)
loop_year=2019


if (isMac()) {
  path = "~/R_notebook/Watch Dog/Data_Insurance/"
} else{
  path = "D:\\R_notebook\\Watch Dog\\Data_Insurance\\"
}

df.big5_to_utf8 <-function(dataset){
  dataset=as.data.frame(lapply(dataset, function(x) iconv(x, "BIG-5", "UTF-8")))
}


get.rbc.sheet09_1<-function(loop_year,sheet.select,com.select){
  print(loop_year)
  rbc.file.name = paste0("RBC",loop_year,".Rdata")
  
  load(paste0(path,rbc.file.name))
  
  temp.data = eval(parse(text=paste0("RBC.",loop_year))) %>% 
    df.big5_to_utf8()
  
  if (is.factor(temp.data$companyID)){
    temp.data %<>%
      mutate(companyID = as.numeric.factor(companyID))
    
  }
  
  
  if (loop_year>=2019){
    temp.data %<>%
      filter(sheet==sheet.select) %>%
      filter(companyID %in% com.select) %>%
      filter(row>5) %>%
      select_if(~all(!is.na(.))) %>%
      mutate(year = loop_year) %>%
      select(year,companyID,isin = c2,isin.name = c3,fx = c19,interest = c21,bookvalue = c24)
    
  }else{
    temp.data %<>%
      filter(sheet==sheet.select) %>%
      filter(companyID %in% com.select) %>%
      filter(row>5) %>%
      select_if(~all(!is.na(.))) %>%
      mutate(year = loop_year) %>%
      select(year,companyID,isin = c2,isin.name = c3,fx = c18,interest = c20,bookvalue = c23)   
    
  }
  
}


get.rbc.sheet11_1<-function(loop_year,sheet.select,com.select){
  print(loop_year)
  rbc.file.name = paste0("RBC",loop_year,".Rdata")
  
  load(paste0(path,rbc.file.name))
  
  temp.data = eval(parse(text=paste0("RBC.",loop_year))) %>% 
    df.big5_to_utf8()
  
  if (is.factor(temp.data$companyID)){
    temp.data %<>%
      mutate(companyID = as.numeric.factor(companyID))
    
  }
  
  if (loop_year==2018){
    temp.data %<>%
      filter(sheet==sheet.select) %>%
      filter(companyID %in% com.select) %>%
      filter(row>5) %>%
      select_if(~all(!is.na(.))) %>%
      mutate(year = loop_year) %>%
      select(year,companyID,isin = c2,isin.name = c3,fx = c19,interest = c21,bookvalue = c25)
    
  }else{
    temp.data %<>%
      filter(sheet==sheet.select) %>%
      filter(companyID %in% com.select) %>%
      filter(row>5) %>%
      select_if(~all(!is.na(.))) %>%
      mutate(year = loop_year) %>%
      select(year,companyID,isin = c2,isin.name = c3,fx = c20,interest = c22,bookvalue = c26)   
    
  }
  
}





df = get.rbc.sheet(2018,"表09-1",loop_com)


  
  
loop_year = c(2019:2012)
loop_com   = c(26401,26402)
com.select = c(26401,26402,207,264)
sheet.select = "表09-1"

#金融債
df.rbc.sheet09_1 <- map_df(loop_year,~get.rbc.sheet09_1(.x,"表09-1",com.select))

df.rbc.sheet09_1.tidy<-df.rbc.sheet09_1 %>%
  mutate(interest = as.numeric(interest),
         bookvalue = as.numeric(bookvalue),
         fx = as.character(fx)) %>%
  mutate(range = case_when(interest>0.04                    ~ "High",
                           interest>0.02 & interest<=0.04   ~ "Medium",
                           interest>0    & interest<=0.02   ~ "Low",
                           interest==0   | is.na(interest)  ~"Other"))%>%
  mutate(fxx = ifelse(fx=="TWD","TWD","no-TWD"))

p.sheet09_1.bv <- df.rbc.sheet09_1.tidy %>%
  group_by(year,fxx,companyID,range) %>%
  summarise(value = sum(bookvalue)/10^8) %>%
  data.frame() %>%
  mutate(companyID = as.character(companyID)) %>%
  ggplot(aes(year,value,color=companyID))+
  geom_line()+
  geom_point()+
  facet_wrap(fxx~range,nrow=2)+
  ggtitle("帳面價值(億)")+
  theme_cht()


p.sheet09_1.n <- df.rbc.sheet09_1.tidy %>%
  group_by(year,fxx,companyID,range) %>%
  summarise(value = n()) %>%
  data.frame() %>%
  mutate(companyID = as.character(companyID)) %>%
  ggplot(aes(year,value,color=companyID))+
  geom_line()+
  geom_point()+
  facet_wrap(fxx~range,nrow=2)+
  ggtitle("檔案")+
  theme_cht()





#公司債
df.rbc.sheet11_1 <- map_df(loop_year,~get.rbc.sheet11_1(.x,"表11-1",com.select))


df.rbc.sheet11_1.tidy<-df.rbc.sheet11_1 %>%
  mutate(interest = as.numeric(interest),
         bookvalue = as.numeric(bookvalue),
         fx = as.character(fx)) %>%
  mutate(range = case_when(interest>0.04                    ~ "High",
                           interest>0.02 & interest<=0.04   ~ "Medium",
                           interest>0    & interest<=0.02   ~ "Low",
                           interest==0   | is.na(interest)  ~"Other")) %>%
  mutate(fxx = ifelse(fx=="TWD","TWD","no-TWD"))

df.rbc.sheet11_1.tidy %>%
  group_by(year,fxx,companyID,range) %>%
  summarise(value = sum(bookvalue)/10^8) %>%
  data.frame() %>%
  mutate(companyID = as.character(companyID)) %>%
  ggplot(aes(year,value,color=companyID))+
  geom_line()+
  geom_point()+
  facet_wrap(fxx~range)


Hash.Company.RBC %>% vv

df.rbc.sheet11_1.tidy %>%
  group_by(year,fxx,companyID,range) %>%
  summarise(value = n()) %>%
  data.frame() %>%
  mutate(companyID = as.character(companyID)) %>%
  ggplot(aes(year,value,color=companyID))+
  geom_line()+
  geom_point()+
  facet_wrap(fxx~range)+
  ggtitle("檔數")+
  theme_cht()

################################
get.rbc.sheet<-function(loop_year, sheet.select, com.select){
  print(loop_year)
  rbc.file.name = paste0("RBC",loop_year,".Rdata")
  
  load(paste0("D:\\R_notebook\\Watch Dog\\Data_Insurance\\",rbc.file.name))
  
  temp.data = eval(parse(text=paste0("RBC.",loop_year)))
  
  temp.data %<>%
    filter(sheet==sheet.select) %>%
    filter(companyID %in% com.select ) %>%
    select_if(~all(!is.na(.))) %>%
    mutate(year = loop_year)
  
}

#########

make.nodiff.rbc.sheet.09_1 <-function(dataset){
  if (loop_year>=2019){
    dataset %<>%
      select(year,isin = c2,isin.name = c3,fx = c19,interest = c21,bookvalue = c24)
    
  }else{
    dataset %<>%
      select(year,isin = c2,isin.name = c3,fx = c18,interest = c20,bookvalue = c23)   
  }
}