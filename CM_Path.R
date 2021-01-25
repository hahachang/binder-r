#Set default env
if (isMac()) {
  setwd("~/R_notebook/Watch Dog")
  slash = "/"
  dir.r        =  "/Users/HaHaChang/R_notebook/"
  dir.desktop  =  "/Users/HaHaChang/Desktop/"

} else{ 
  setwd("D:\\R_notebook\\Watch Dog")
  slash = "\\"
  dir.r        = "D:\\R_notebook\\"
  dir.desktop  = "D:\\400201\\Desktop\\"

}

dir.data     =  paste0(dir.r,"data",slash) #資料倉庫
dir.out.haha =  paste0(dir.r,"out" ,slash) #
dir.out.wd   =  paste0(dir,r,"wd"  ,slash) #預警預設輸出

###########################################################

if (isMac()) {
  setwd("~/R_notebook/Watch Dog")
  desktop.save_path = "/Users/HaHaChang/Desktop/"
} else{
  setwd("D:\\R_notebook\\Watch Dog")
  desktop.save_path = "D:\\400201\\Desktop\\"
}
# Basic packages and tools
  
wd.save_path = "D:\\R_notebook\\Watch Dog\\Data_Insurance\\"
quarter.save_path = "D:\\壽險季報_底稿\\共通\\"
ir.save_path = "D:\\專案\\準備金利率\\共通\\"
quarter.temp.save_path = "D:\\壽險季報_底稿\\共通\\temp\\"
prem40.save_path  = "D:\\壽險月報\\"
claim.save_path = "D:\\壽險月報\\"
monthly.save_path = "D:\\壽險月報\\"
message.save_path = "D:\\壽險月報\\簡訊\\"
oversea.save_path = "D:\\壽險月報\\國際保險業務\\"
ib.save_path = "D:\\壽險月報\\IB\\"
ceo.save_path = "D:\\壽險月報\\CEO\\"
path_currency = "D:\\R_notebook\\Stock\\currency\\"


save_path.gif = "D:\\R_notebook\\export\\"
