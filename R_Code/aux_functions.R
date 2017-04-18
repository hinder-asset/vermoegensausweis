###############################################################################
#                                                                             #
#                   Function File                                             #
###############################################################################

# Version Inforamtion: 20.05.2016
# Author: Robert Leitner

Source_Table_Client_info<-"I:/Company/Excel Sheets/Account Info.xlsm"
Source_Table_Modelport<-"O://Produkte/Portfolios/Modelportfolios.xlsm"
Source_Table_Transactions<-"I:/Company/Transaktionen/Transaktionsjournal.xlsm"
Source_Table_MasterList<-"O:/Produkte/ETF_Masterliste/ETF_MasterListe.xlsm"

excel_sheet_trans<-"Transaktionen 2017"

library(HAM)
library(Rblpapi)

source_clients<-function(){
  
  input_data = openxlsx::read.xlsx(Source_Table_Client_info,sheet="Account Info")
  input_data<-as.matrix(input_data)
  input_data<-input_data[input_data[,3]=="Ja" & !is.na(input_data[,2]) ,c(2,4,6,10,11,12,13,14,15,16,19)]
 
  return(input_data) 
}


remove_first_col<-function(input_data){
  if(is.element("SW",input_data[,3])|is.element("LN",input_data[,3])|is.element("GY",input_data[,3])|is.element("US",input_data[,3])){
    return(input_data[,2:ncol(input_data)])
  }else{
    return(input_data)
  } 
}


source_models<-function(kunde){
      
  sheets<-getSheetNames(Source_Table_Modelport)
  for(sname in sheets){
    input_data = openxlsx::read.xlsx(Source_Table_Modelport,sheet=sname, startRow = 7,skipEmptyRows=T, colNames=F, rowNames=F)
    if(is.element(kunde, as.character(input_data[1,]))){
#       input_tickers = openxlsx::read.xlsx(Source_Table_Modelport,sheet=sname, startRow = 7,cols=3,skipEmptyRows=T, colNames=F, rowNames=F)
#       if(any(is.na(input_tickers))){
#         for(i in 1:nrow(input_data)){
#           a<-openxlsx::read.xlsx(Source_Table_Modelport,sheet=sname, rows=c((5+i):(6+i)),cols=3, colNames=T, rowNames=F, skipEmptyRows=T)
#           if(!is.null(a)){
#             if(any(is.na(a))){
#               print(input_data[(i+which(is.na(a))-1),3])
#               input_data[(i+which(is.na(a))-1),3]<-"na"
#             }
#           } 
#         }
#       }
      input_data<-remove_first_col(input_data)
      pos<-grep(kunde, as.character(input_data[1,]))
      return(list(pos, sname, input_data[!is.na(input_data[,pos+1])& input_data[,pos+1]!="0",c(1:4,pos:(pos+2))], input_data[grepl("KK Cash", substr(input_data[,3], 1,7)) ,c(1:4,pos:(pos+2))]))
    }else if(sname == last(sheets)){
      return(NA)
    }
  }
}

# source_transactions<-functions(){}
# 
# c("NAME", "COUNTRY", "REGION_OR_COUNTRY", "CRNCY", "HISTORY_START_DT")
#"CURRENCY_HEDGED_INDICATOR"
# FUND_TOTAL_ASSET_CRNCY
source_data<-function(ticker,field){
  
  a<-try({
    if(!exists("conn")){
      assign("conn",Rblpapi::blpConnect(), envir= .GlobalEnv)
    }else{
      1
    }
  }, silent=T)
  if(class(a)=="try-error"){
    return(rep("BL N/A", length(ticker)))
  }else{
  df<-bdp(ticker, field, con=conn)
  #bb_download_ts_lc(conn, ticker, "PX_LAST", date, date, freq="DAILY")[,3]
  df[is.na(df)]<-""
  return(as.vector(as.matrix(df)))
  }
  
}

source_data_2<-function(ticker,field){
  
  a<-try({
    if(!exists("conn")){
      assign("conn",Rblpapi::blpConnect(), envir= .GlobalEnv)
    }else{
      1
    }
  }, silent=T)
  if(class(a)=="try-error"){
    return(rep("BL N/A", length(ticker)))
  }else{
    df<-c()
    for(i in 1:length(ticker)){
      a<-try(as.character(bdp(ticker[i], field, con=conn)), silent=T)
      if(class(a)=="try-error"){
        df<-c(df, NA)
      }else{
        df<-c(df, a[1])
      }
    }
    names(df)<-ticker
    return(df)
  }
  
}

source_data_cc<-function(ticker,field, date){
  
  a<-try({
    if(!exists("conn")){
      assign("conn",Rblpapi::blpConnect(), envir= .GlobalEnv)
    }
  }, silent=T)
  if(class(a)=="try-error"){
    return(rep("BL N/A", length(ticker)))
  }else{
    df<-c()
    for(i in 1:length(ticker)){
      a<-try(as.vector(Rblpapi::bdh(con=conn, ticker[i], field, start.date=date, end.date=date,include.non.trading.days=F, options=c(currency="CHF", nonTradingDayFillMethod="PREVIOUS_VALUE",nonTradingDayFillOption="ALL_CALENDAR_DAYS", periodicitySelection="DAILY"))[,2])[1], silent=T)
          if(class(a)!="try-error" & length(a)>0 & !is.na(a)){
            df<-c(df, a)
          }else{
            a<-try(as.vector(Rblpapi::bdp(ticker[i], "FUND_NET_ASSET_VAL",con=conn, overrides= c("EQY_FUND_CRNCY"= "CF"))), silent=T)
            if(class(a)!="try-error" & length(a)>0 & !is.na(a)){
              print(a)
            df<-c(df, a)
            }else{
              df<-c(df, NA)
            }
          }
    }

    names(df)<-ticker
    #bb_download_ts_lc(conn, ticker, "PX_LAST", date, date, freq="DAILY")[,3]
    df[is.na(df)]<-""
    return(df)
  }
}

find_einstand<-function(transactions, ticker, col){
  
  dates<-rep("", length(ticker))
  if(is.null(transactions)|| length(transactions)==0){
    return(dates)
  }else{
  
  for(i in 1:nrow(transactions)){
    if(is.element(toupper(transactions[i,1]), toupper(ticker))){
      dates[which(toupper(transactions[i,1])==toupper(ticker))]<-as.character(transactions[i,col])
    }
  }
  return(dates)
  }
}

get_asset_class<-function(ticker){
 
  if(!exists("master_list_static"))
  assign("master_list_static", openxlsx::read.xlsx(Source_Table_MasterList,sheet="Statics", startRow = 7, colNames=F, rowNames=F), envir = .GlobalEnv)
  
  vec<-rep("", length(ticker))
  ticks<-toupper(master_list_static[,1])
  for(i in 1:length(ticker)){
    try(vec[i]<-as.character(master_list_static[grep(toupper(ticker[i]), ticks), 31]), silent=T)
  }
  return(vec)
}


get_exchange_rates<-function(date,ticker=c("EURCHF CURNCY", "USDCHF CURNCY", "GBPCHF CURNCY")){
  
  a<-try({
    if(!exists("conn")){
      assign("conn",Rblpapi::blpConnect(), envir= .GlobalEnv)
    }else{
      
    }
  }, silent=T)
  if(class(a)=="try-error"){
    return(rep("BL N/A", length(ticker)))
  }else{
    # vec<-bb_download_ts_lc(conn, ticker, "PX_LAST", date, date, freq="DAILY")[,3]
    # 
    dat<-Rblpapi::bdh(con=conn, ticker, "PX_LAST", start.date=date, end.date=date,include.non.trading.days=F, options=c(nonTradingDayFillMethod="PREVIOUS_VALUE",nonTradingDayFillOption="ALL_CALENDAR_DAYS"))
    vec<-unlist(dat)[seq(2,length(ticker)*2,by=2)]
    names(vec)<-ticker
    return(round(vec, 4))
  }
  
}

convert_prices<-function(prices_lc, wc, date){


  if(!any(grepl("-", date))){
  date<-as.character(date)
  date<-as.Date(date, "%m/%d/%Y")
  }else if(class(date)[1]!="Date"){
    date<-as.Date(date)
  }
  a<-rep(NA, length(prices_lc))
  for(i in 1:length(a)){
    try(a[i]<-as.numeric(prices_lc[i])*ifelse(toupper(wc[i])=="CHF", 1, as.numeric(get_exchange_rates(date[i], ticker=paste(toupper(wc[i]), "CHF CURNCY", sep="")))) ,silent=F)
  }
  return(round(a, digits=3))
}


get_transactions<-function(kunde, stichtag=NULL){ 

  input_data = openxlsx::read.xlsx(Source_Table_Transactions, sheet=excel_sheet_trans)
  input_data<-input_data[!is.na(input_data[,5]),]
  colnames(input_data)<-input_data[1,]
  input_data<-input_data[grep(kunde, input_data[,6]),]

  if(dim(input_data)[1]>0){
  ticker<-paste(as.character(input_data[,2]), as.character(input_data[,3]),"Equity", sep=" ")
  input_data[,1]<-ticker
  input_data<-input_data[,c(1,4,12,15,16,17,19,23,25,29)]
  input_data<-input_data[!is.na(input_data[,4])& input_data[,4]!=0,]
  input_data[,2]<-na.locf(convertToDate(input_data[,2]),fromLast=T, na.rm = F)
  input_data[,9]<-na.locf(convertToDate(input_data[,9]),fromLast=T, na.rm = F)
  input_data[,4]<-round(as.numeric(input_data[,4]), digits=4)
  termingeschaefte<-input_data[(grepl("NA NA",input_data[,1])),]
  input_data<-input_data[!(grepl("NA NA",input_data[,1])),]   # excludes Cash Transactions!
  }else{
    termingeschaefte<-matrix(rep("", 10), nrow=1)
    colnames(termingeschaefte)<-colnames(input_data[,c(1,4,12,15,16,17,19,23,25,29)])
    input_data<-matrix(rep("", 10), nrow=1)
    colnames(input_data)<-colnames(termingeschaefte)
  }
  colnames(input_data)[1]<-"Ticker"
  colnames(termingeschaefte)[1]<-"Ticker"

  if(is.null(stichtag) || stichtag<=as.Date(paste(year(Sys.time()),"-01-01", sep=""))){ 
    load("O:/11_Personal_Folders/07_Robert_Leitner/16_Reporting/Transaktions_Historie.RData")
    transaction_history[,2]<-as.Date(transaction_history[,2])
    transaction_history[,9]<-as.Date(transaction_history[,9])
    hist<-transaction_history[grep(kunde, transaction_history[,3]),-3]
    names(hist)<-colnames(input_data[,-7])
    input_data[,2]<-as.character(input_data[,2])
    input_data<-rbind(hist[,1:9],input_data[input_data[,2]>=paste(year(Sys.time()),"-01-01", sep=""),-7])
    return(list(input_data, termingeschaefte))
  }else{
    return(list(input_data[input_data[,2]>=stichtag,], termingeschaefte[termingeschaefte[,2]>=stichtag,]))
}
}

get_einstand_chf<-function(preis_lc, currency, date, ticker){

  preis_chf<-rep("", length(preis_lc))
  first_time<-T
  
  for(i in 1:length(preis_lc)){
    if(currency[i]=="CHF"){
     preis_chf[i]<-preis_lc[i] 
    }else if(!is.na(as.numeric(preis_lc[i]))){
      if(nchar(currency[i])==3 & date[i]!=""){
        
        if(first_time)
          a<-try({
            if(!exists("conn")){
              assign("conn",Rblpapi::blpConnect(), envir= .GlobalEnv)
            }else{
              1
            }
          }, silent=T)
        first_time<-F
            if(class(a)!="try-error"){
        
             try(preis_chf[i]<-round(as.numeric(preis_lc[i])*as.numeric(Rblpapi::bdh(con=conn, paste(currency[i], "CHF CURNCY", sep=""), "PX_LAST", start.date=as.Date(date[i]), end.date=as.Date(date[i]),include.non.trading.days=F, options=c(nonTradingDayFillMethod="PREVIOUS_VALUE",nonTradingDayFillOption="ALL_CALENDAR_DAYS"))[,2]), digits=3), silent=T)
              }
      }
    }
    
  }
  return(preis_chf)
  
}

backtrack_positions<-function(positions, transactions){
  positions<-as.matrix(positions)
  
  if(is.vector(transactions)){
    transactions<-t(as.matrix(transactions))
  }
  
  if(dim(transactions)[1]>0){
    
    bol<-grepl("EQUITY", toupper(transactions[,1]))
    transactions<-transactions[bol,]
    
    verkaufe<-transactions[toupper(transactions[,3])=="V", ]
    if(is.vector(verkaufe))
      verkaufe<-t(as.matrix(verkaufe))
    
    if(dim(verkaufe)[1]>0){
      for(i in 1:(dim(verkaufe)[1])){
        if(any(grepl(toupper(verkaufe[i,1]), toupper(positions[,1])))){
          positions[grep(toupper(verkaufe[i,1]), toupper(positions[,1])), 7]<-as.numeric(positions[grep(toupper(verkaufe[i,1]), toupper(positions[,1])), 7])+as.numeric(verkaufe[i,4])
        }else{
          positions<-rbind(positions, c(verkaufe[i,1], verkaufe[i,6], NA, NA, verkaufe[i,5], "FALSE",verkaufe[i,4], verkaufe[i,7],"", "", verkaufe[i,8], "", "", "", "", "V" ))
        }
      }
    }
    
    kaufe<-transactions[toupper(transactions[,3])=="K", ]
    if(is.vector(kaufe))
      kaufe<-t(as.matrix(kaufe))
    
    if(dim(kaufe)[1]>0){
      for(i in 1:(dim(kaufe)[1])){
        if(any(grepl(toupper(kaufe[i,1]), toupper(positions[,1])))){ 
          positions[grep(toupper(kaufe[i,1]), toupper(positions[,1])), 7]<-as.numeric(positions[grep(toupper(kaufe[i,1]), toupper(positions[,1])), 7])-as.numeric(kaufe[i,4])  
        }
      }  
      
      bol<-as.numeric(positions[,7])>0
      bol[is.na(bol)]<-F
      positions<-positions [bol,] 
      
      return(positions)
      
      
    }else{
      return(positions) 
    }
  }

}



source_data_perf<-function(ticker, start_date, end_date, kaufpreis=NULL){
  if(length(start_date)==1){
    start_date<-rep(start_date, length(ticker))
  }
  a<-try({
    if(!exists("conn")){
      assign("conn",Rblpapi::blpConnect(), envir= .GlobalEnv)
    }
  }, silent=T)
  if(class(a)[1]=="try-error"){
    return(rep("BL N/A", length(ticker)))
  }else{
    df<-c()
    af<-c()
    for(i in 1:length(ticker)){
      a<-try(na.omit(as.vector( Rblpapi::bdh(con=conn, ticker[i], "TOT_RETURN_INDEX_NET_DVDS", start.date=as.Date(start_date[i]), end.date=end_date,include.non.trading.days=F, options=c(nonTradingDayFillMethod="PREVIOUS_VALUE",currency="CHF", nonTradingDayFillOption="ALL_CALENDAR_DAYS"))[,2])), silent=T)
      if(class(a)[1]!="try-error" & length(a)>0 & !all(is.na(a))){
        df<-c(df, last(a)/first(a)-1)
        af<-c(af, first(a))
      }else{
        a<-try(na.omit(as.vector(Rblpapi::bdh(con=conn, ticker[i], "PX_LAST", start.date=as.Date(start_date[i]), end.date=end_date,include.non.trading.days=F, options=c(nonTradingDayFillMethod="PREVIOUS_VALUE",currency="CHF", nonTradingDayFillOption="ALL_CALENDAR_DAYS"))[,2])), silent=T)
        if(class(a)!="try-error" & length(a)>0){
          df<-c(df, last(a)/first(a)-1)
          af<-c(af, first(a))
        }else{
          df<-c(df, NA)
          af<-c(af, NA)
        }
      }
    }
    if(!is.null(kaufpreis)){

      for(i in 1:length(kaufpreis)){
        if(!is.na(kaufpreis[i])){
          df[i]<-(df[i]+1)/(kaufpreis[i]/af[i])-1
        }
      }
    }
    names(df)<-ticker
    return(as.numeric(df))
  }
}











#+++++++++++++++++++++++++++++++++++ FUNCTION TO CREATE HISTORY+++++++++++++++++++++++++++++
#
# get_transactions_hist<-function(kunde, stichtag=NULL){ 
#  
# #2015
#   input_data2 = openxlsx::read.xlsx("I://Company/Transaktionen/Alte Versionen/Transaktionsjournal_2015_ohne Links.xlsm")
#   input_data2<-input_data2[!is.na(input_data2[,5]),]
#   colnames(input_data2)<-input_data2[1,]
#   input_data2<-input_data2[-1,]
#   
#   ticker<-paste(as.character(input_data2[,2]), as.character(input_data2[,3]),"Equity", sep=" ")
#   input_data2[,1]<-ticker
#   input_data2<- input_data2[,c(1,4,5,11,14,15,16,22,24,28)]
#   input_data2[,2]<-na.locf(as.character(convertToDate(input_data2[,2])), fromLast=T)
#   input_data2[,9]<-na.locf(as.character(convertToDate(input_data2[,9])), fromLast=T)
#   input_data2[,5]<-round(as.numeric(input_data2[,5]), digits=4)
#   
# #2014 
#   input_data14 = openxlsx::read.xlsx("I://Company/Transaktionen/Alte Versionen/Transaktionsjournal_2013_2014.xlsm", sheet="Transaktionen 2014")
#   input_data14<-input_data14[!is.na(input_data2[,5]),]
#   colnames(input_data14)<-input_data14[1,]
#   input_data14<-input_data14[-(1:95),]
#   
#   ticker<-paste(as.character(input_data14[,2]), as.character(input_data14[,3]),"Equity", sep=" ")
#   input_data14[,1]<-ticker
#   input_data14<- input_data14[,c(1,4,5,11,14,15,16,22,24,28)]
#   input_data14[,2]<-na.locf(as.character(convertToDate(input_data14[,2])), fromLast=T)
#   input_data14[,9]<-na.locf(as.character(convertToDate(input_data14[,9])), fromLast=T)
#   input_data14[,5]<-round(as.numeric(input_data14[,5]), digits=4)
#   
# #2013-02
#   input_data13 = openxlsx::read.xlsx("I://Company/Transaktionen/Alte Versionen/Transaktionsjournal_2013_2014.xlsm", sheet="Transaktionen 2013")
#   input_data13<-input_data13[!is.na(input_data13[,5]),]
#   colnames(input_data13)<-input_data13[1,]
#   input_data13<-input_data13[-(1:86),]
#   
#   ticker<-paste(as.character(input_data13[,2]), as.character(input_data13[,3]),"Equity", sep=" ")
#   input_data13[,1]<-ticker
#   input_data13<- input_data13[,c(1,4,5,11,14,15,16,22,24,28)]
#   input_data13[,2]<-na.locf(as.character(convertToDate(input_data13[,2])), fromLast=T)
#   input_data13[,9]<-na.locf(as.character(convertToDate(input_data13[,9])), fromLast=T)
#   input_data13[,5]<-round(as.numeric(input_data13[,5]), digits=4)  
#   
# #2013-01 
#   input_data1301 = openxlsx::read.xlsx("I://Company/Transaktionen/Alte Versionen/2013-10-16 Transaktionsjournal.xlsm", sheet="Transaktionen 2013")
#   input_data1301<-input_data1301[!is.na(input_data1301[,5]),]
#   colnames(input_data1301)<-input_data1301[1,]
# 
#   ticker<-paste(as.character(input_data1301[,2]), as.character(input_data1301[,3]),"Equity", sep=" ")
#   input_data1301[,1]<-ticker
#   input_data1301<- input_data1301[,c(1,4,5,11,14,15,16,17,4,27)]
#   input_data1301[,2]<-na.locf(as.character(convertToDate(input_data1301[,2])), fromLast=T)
#   input_data1301[,9]<-na.locf(as.character(convertToDate(input_data1301[,9])), fromLast=T)
#   input_data1301[,5]<-round(as.numeric(input_data1301[,5]), digits=4)  
#   input_data1301<-input_data1301[1:476,]
#   
# #2012
#   input_data12 = openxlsx::read.xlsx("I://Company/Transaktionen/Alte Versionen/2013-10-16 Transaktionsjournal.xlsm", sheet="Transaktionen 2012")
#   input_data12<-input_data12[!is.na(input_data12[,5]),]
#   colnames(input_data12)<-input_data12[1,]
#   
#   ticker<-paste(as.character(input_data12[,2]), as.character(input_data12[,3]),"Equity", sep=" ")
#   input_data12[,1]<-ticker
#   input_data12<- input_data12[,c(1,4,5,11,14,15,16,21,4,27)]
#   input_data12[,2]<-na.locf(as.character(convertToDate(input_data12[,2])), fromLast=T)
#   input_data12[,9]<-na.locf(as.character(convertToDate(input_data12[,9])), fromLast=T)
#   input_data12[,5]<-round(as.numeric(input_data12[,5]), digits=4)  
# 
#   
#   colnames(input_data12)<-colnames(input_data2)
#   colnames(input_data1301)<-colnames(input_data2)
#   colnames(input_data13)<-colnames(input_data2)
#   colnames(input_data14)<-colnames(input_data2)
#   
#   trans_data<-rbind(input_data12,input_data1301, input_data13, input_data14, input_data2)
#   trans_data<-trans_data[trans_data[,5]!="0",]
#   trans_data<-trans_data[!is.na(trans_data[,5]),]
# 
#   transaction_history<-trans_data
#   
#   input_data = openxlsx::read.xlsx(Source_Table_Client_info,sheet="Account Info")
#   input_data<-as.matrix(input_data)
#   input_data<-input_data[input_data[,3]=="Ja" & !is.na(input_data[,2]) ,c(2,4,6,7,8,10,11,12,13,14,18)]
#   
#   for(i in 1:nrow(transaction_history)){
#     
#     flash<-input_data[grep(transaction_history[i,3], input_data[,2]),1]
#     if(length(flash)==0){
#       transaction_history[i,3]<-NA
#     }else if(length(flash)==2){
#       flash<-input_data[grep(transaction_history[i,3], input_data[,2], fixed=T),1]
#       transaction_history[i,3]<-flash[1]
#     }else{
#       transaction_history[i,3]<-flash[1]
#     }
#   }
#   
#   colnames(transaction_history)[1]<-"Ticker"
#   transaction_history<-transaction_history[!is.na(transaction_history[,3]),]
#   transaction_history[,8]<-round(as.numeric(transaction_history[,8]), digits=4)
#   
#   for(i in 1:nrow(transaction_history)){
#     if(grepl("Swap",transaction_history[i,7])){
#       transaction_history[i,1]<-"Swap"
#     }else if(grepl("Forward",transaction_history[i,7])|| grepl("Termin",transaction_history[i,7])|| grepl("termin",transaction_history[i,7])){
#       transaction_history[i,1]<-"Forward"
#     }else if(grepl("/CHF",transaction_history[i,7])|| grepl("CHF/",transaction_history[i,7])|| grepl("USD/",transaction_history[i,7])|| grepl("EUR/",transaction_history[i,7])){
#       transaction_history[i,1]<-"Currency Change"
#     }
#     
#   }
#   transaction_history[,7]<-findreplace_strings(transaction_history[,7], "&amp;", "&")
#   transaction_history[,1]<-findreplace_strings(transaction_history[,1], "NA NA Equity", "")
#   transaction_history[,1]<-findreplace_strings(transaction_history[,1], " NA ", " ")
#   
#   save(transaction_history, file = "O:/11_Personal_Folders/07_Robert_Leitner/16_Reporting/Transaktions_Historie.RData")
# 
# }


#### Archive Transactions from 2016:
# old_trans_to_add<-"I:/Company/Transaktionen/Transaktionsjournal.xlsm"
# input_data = openxlsx::read.xlsx(old_trans_to_add, sheet="Transaktionen 2016")
# input_data<-input_data[!is.na(input_data[,5]),]
# colnames(input_data)<-input_data[1,]
# input_data<-input_data[-1,]
# input_data[,2]<-paste(input_data[,2], input_data[,3], "Equity", sep=" ")
# input_data<-input_data[!is.na(input_data[,4])& input_data[,4]!=0,]
# input_data<-input_data[,c(2,4,6,12,15,16,17,19,23,25,29)]
# input_data<-input_data[!is.na(input_data[,4])& input_data[,4]!=0,]
# input_data[,2]<-as.character(convertToDate(input_data[,2]))
# input_data[,10]<-as.character(convertToDate(input_data[,10]))
# 
# input_data<-input_data[,c(1:7,9:11,8)]
# load("O:/11_Personal_Folders/07_Robert_Leitner/16_Reporting/Transaktions_Historie.RData")
# transaction_history$ISIN<-NA
# colnames(input_data)<-colnames(transaction_history)
# transaction_history<-rbind(transaction_history,input_data[449:nrow(input_data),])
# save(transaction_history, file="O:/11_Personal_Folders/07_Robert_Leitner/16_Reporting/Transaktions_Historie.RData")



# +++++++++++++++  FUNCTION TO RELOAD CLASSIFICATION TABLE +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# source_classifications<-function(){
# 
# input_data = openxlsx::read.xlsx("O:/11_Personal_Folders/07_Robert_Leitner/16_Reporting/Assetklassifizierung.xlsx",sheet="Tabelle1")
# classification_table<-as.matrix(input_data)
# 
# input_data = openxlsx::read.xlsx("O:/11_Personal_Folders/07_Robert_Leitner/16_Reporting/Assetklassifizierung.xlsx",sheet="Währungen")
# classification_currency<-as.matrix(input_data)
# 
# save(classification_table,classification_currency, file = "O:/11_Personal_Folders/07_Robert_Leitner/16_Reporting/Classification.RData")
#  }






