##################################
####     設定期間比較參數     ####
##################################
本期      = 100*(Year_Selected-1911)+Month_Selected
前期      = 100*(ifelse(Month_Selected==1,Year_Selected-1,Year_Selected)-1911)+ifelse(Month_Selected<2,12-Month_Selected+1,Month_Selected-1)
前前期    = 100*(ifelse(Month_Selected<=2,Year_Selected-1,Year_Selected)-1911)+ifelse(Month_Selected<=2,12-Month_Selected+2,Month_Selected-2)
去年同期  = 100*(Year_Selected-1911-1)+Month_Selected
去年前期  = 100*(ifelse(Month_Selected==1,Year_Selected-1,Year_Selected)-1911-1)+ifelse(Month_Selected<=2,12-Month_Selected+1,Month_Selected-1)
去年年底  = 100*(Year_Selected-1911-1)+12
年化因子  = 12

#英文參數設定
now                     = 本期
now.pre                 = 前期
now.prepre              = 前前期
now.lastyear.same       = 去年同期 
now.lastyear.same.pre   = 去年前期
now.lastyear.end        = 去年年底


print(list(paste0("本期:",本期),
           paste0("前期:",前期),
           paste0("前前期:",前前期),
           paste0("去年同期:",去年同期),
           paste0("去年前期:",去年前期),
           paste0("去年年底:",去年年底)))
