# Profiling App:
# As of 2015-11-15 Robert Leitner

##################################################################################################################
library(reshape2)
library(shiny)
library(shinyBS)
library(lubridate)
library(rhandsontable)
library(HAM)
library(lubridate)
library(xts)
library(openxlsx)
library(Rbbg)

rm(list=ls(all=TRUE))
options("scipen"=100, "digits"=10)    # Number presentation

source("O:/11_Personal_Folders/07_Robert_Leitner/16_Reporting/R_Code/aux_functions.R")
client_list<-source_clients()
client_list<-cbind(client_list,paste(client_list[,2], "(",client_list[,1], ")", sep=""))
client_list[,8]<-as.character(convertToDate(client_list[,8]))
client_list<-client_list[order(client_list[,12]),]

client_choices<-as.vector(client_list[,1])
names(client_choices)<-as.vector(client_list[,12])

current_page_global <- 0
n_pages <- 4
first_time_loaded<-T

##############################################################################################################
# color definitions

hblue <- rgb(0, 72, 144, maxColorValue = 255)
hblue1 <- rgb(180, 226, 244, maxColorValue = 255)
horange <- rgb(243, 112, 33, maxColorValue = 255)
hred <- rgb(178, 34, 34, maxColorValue = 255)
hgrey <- rgb(112, 128, 144, maxColorValue = 255)
hgrey1 <- rgb(220, 230, 242, maxColorValue = 255)


colors <- c(hred, hgrey, hgrey, hblue, hblue1)

##################################################################################################################
##################################################################################################################
#                                                                                                                #
#                                        Main Body                                                               #
#                                                                                                                #
##################################################################################################################

shinyServer(function(input, output,session) {

#################################################################################################################
  #   Main Panel Content Generator:
  
refresh_main_panel<-function(current_page=NULL){
    
    output$main_question_panel<-renderUI({
    
    if(current_page==0){  
      # the following command seems to be redundant, but necessary if the user presses the "Home"-button  
      current_page_global <<- 0

    list(
      br(),
      br(),
      p(h4("Erstellen eines Vermoegensausweises.")),
      br(),
      p("Stellen Sie sicher, dass sie mit Ihrem Rechner Zugang zu Bloomberg haben, sowie die Zugriffsrechte auf die Kundendaten. Marktdaten und Preise werden von Bloomberg heruntergeladen"),
      br(),
      div(align="center",
      selectInput("kunde", "Kundenauswahl",c(" "="", client_choices), selected=ifelse(is.null(input$kunde), "", input$kunde))),
      uiOutput("kunden_tabelle"),
      br(),
      br(),
      p("Bitte waehlen Sie den Stichtag, zu welchem der Vermoegensausweis erstellt werden soll, aus."),
      div(
      dateInput("stichtag", "Stichtag", value = as.Date(ifelse(exists("stichtag"),as.character(stichtag),as.character(as.Date(Sys.time())-1))), min = as.Date("2010-01-01"), max = as.Date(Sys.time())-1, format = "dd.mm.yyyy", startview = "month", weekstart = 0, language = "de", width = NULL), align="center"),
      br(),
      br(),
      p("Moechten Sie einen anonymen Vermoegensausweis erstellen?"),
      div(
        radioButtons("hide_name"," ", c("Nein", "Ja")),selected="Nein", align="center"),
      p("Bitte geben Sie den Kundenbetreuer ein:"),
      div(
        radioButtons("kundenbetreuer",NULL, c(as.character(Sys.info()["user"]), setdiff(c("ah", "aho", "cs", "rst", "wb", "mp"),as.character(Sys.info()["user"]))), selected=NULL), align="center")
      
    )
    
    }else if(current_page==1){  
      
      wechselkurs<<-get_exchange_rates(stichtag)
      isolate({
      list(
        p(h4("Eingabe Cash-Positionen.")),
        br(),
        p("Bitte ueberpruefen Sie sorgfaeltig die Cash-Positionen. Durch Einlagen und Entnahmen koennen grosse Abweichungen zu den tatsaechlichen Werten entstehen. Investitionen in Money Market Funds gelten als Anlageinstrumente und sollten auf der naechsten Seite gelistet werden."),
        br(),
        numericInput("chf", "Schweizer Franken", ifelse(is.na(portfolio[[4]][1,7]),0,portfolio[[4]][1,7]), min = -100000, max = 100000000000, step = 0.01, width = "300px"),
        br(),
        numericInput("eur", "Euro", ifelse(is.na(portfolio[[4]][2,7]),0,portfolio[[4]][2,7]), min = -100000, max = 100000000000, step = 0.01, width = "300px"),
        uiOutput("eur_wert"),
        br(),
        numericInput("usd", "US Dollar", ifelse(is.na(portfolio[[4]][3,7]),0,portfolio[[4]][3,7]), min = -100000, max = 100000000000, step = 0.01, width = "300px"),
        uiOutput("usd_wert"),
        br(),
        numericInput("gbp", "Pfund", 0, min = -100000, max = 100000000000, step = 0.01, width = "300px"),
        uiOutput("gbp_wert")
      )
    })
    }else if(current_page==2){
      
      cash_ges<<-input$chf+input$eur*wechselkurs[1]+input$usd*wechselkurs[2]+input$gbp*wechselkurs[3]
      flash<-get_transactions(input$kunde, as.Date("2013-01-01")) # no cash transactions!
      transactions<<-flash[[1]]
      termingeschaefte<<-flash[[2]]
      list(
        p(h4("Eingabe Termingeschaefte")),
        rHandsontableOutput("termingeschaefte_table") 
      )
      
    }else if(current_page==3){  

      fl<-list(
        br(),
        br(),
        p("Bitte ueberpruefen Sie sorgfaeltig alle Positionen. "),
        br(),
        div(align="right", actionButton("update", "Aktualisiere Tabelle", style="link", size="small")),
        rHandsontableOutput("positions"),
        uiOutput("summe")
      )
      
      if(as.numeric(substr(input$kunde, 3, 6))>900){
        list(fl,numericInput("scale_money", "Scaling in Mio", 1, min = 0.5, max = 100, width = "300px"),
        actionButton("scale_button", "Skaliere Portfolio", style="link", size="small"))
      }else{
        fl
      }
        
      
      }else if(current_page==4){  
        isolate({
          updateButton(session, "start_button", label="Report erstellen")
          
          list(
            br(),
            br(),
            div(
              dateInput("start_trans", "Transaktionen seit", value = as.Date(paste(year(Sys.Date()-30)-1,12,31,sep="-")), min = as.Date("2015-01-01"), max = as.Date(Sys.time())-1, format = "dd.mm.yyyy", startview = "month", weekstart = 0, language = "de", width = NULL), align="right")
            
            ,
            p("Bitte ueberpruefen Sie sorgfaeltig alle Transaktionen. "),
            br(),
            rHandsontableOutput("transactions")
          )
          
          
          
        })
      }
      })
    
}

output$eur_wert<-renderUI({
  list(
  p(paste("Wechselkurse per ", stichtag, ":    ", round(wechselkurs[1], digits=4), sep="")),
  p(paste("Wert CHF per ", stichtag,":       EUR", round(wechselkurs[1]*input$eur, 2), sep="" )))
})

observeEvent(input$eur,{
  output$eur_wert<-renderUI({
    list(
      p(paste("Wechselkurse per ", stichtag, ":   ", round(wechselkurs[1], digits=4), sep="")),
      p(paste("Wert CHF per ", stichtag,":      EUR", round(wechselkurs[1]*input$eur, 2), sep="" )))
  })
  
})

output$usd_wert<-renderUI({
  list(
    p(paste("Wechselkurse per ", stichtag, ":    ", round(wechselkurs[2], digits=4), sep="")),
    p(paste("Wert CHF per ", stichtag,":       USD", round(wechselkurs[2]*input$usd, 2), sep="" )))
})

observeEvent(input$usd,{
  output$usd_wert<-renderUI({
    list(
      p(paste("Wechselkurse per ", stichtag, ":   ", round(wechselkurs[2], digits=4), sep="")),
      p(paste("Wert CHF per ", stichtag,":      USD", round(wechselkurs[2]*input$usd, 2), sep="" )))
  })
  
})

output$gbp_wert<-renderUI({
  list(
    p(paste("Wechselkurse per ", stichtag, ":    ", round(wechselkurs[3], digits=4), sep="")),
    p(paste("Wert CHF per ", stichtag,":       GBP", round(wechselkurs[3]*input$gbp, 2), sep="" )))
})

observeEvent(input$gbp,{
  output$gbp_wert<-renderUI({
    list(
      p(paste("Wechselkurse per ", stichtag, ":   ", round(wechselkurs[3], digits=4), sep="")),
      p(paste("Wert CHF per ", stichtag,":      GBP", round(wechselkurs[3]*input$gbp, 2), sep="" )))
  })
  
})

output$termingeschaefte_table<- renderRHandsontable({
  
  cnames<<-names(termingeschaefte)
  try(termingeschaefte<-termingeschaefte[termingeschaefte[,2]>=stichtag,], silent=T)

  if(dim(termingeschaefte)[1]==0){
    name<-names(termingeschaefte)
    termingeschaefte<-matrix(rep("", 10), nrow=1)
    colnames(termingeschaefte)<-name
  }else{
    po<-termingeschaefte[toupper(termingeschaefte[,3])=="V",]
    if(dim(po)[1]>0){
      ind<-c()
      for(i in 1:nrow(po)){
        
        flash<-po[-i,4]
        if(any(grepl(po[i,4], flash))){
          
        }else{
          ind<-c(ind,i)
        }
      }
      termingeschaefte<-po[ind,]
      if(dim(termingeschaefte)[1]==0){
        name<-names(termingeschaefte)
        termingeschaefte<-matrix(rep("", 10), nrow=1)
        colnames(termingeschaefte)<-name
      }
    }else{
      name<-names(po)
      termingeschaefte<-matrix(rep("", 10), nrow=1)
      colnames(termingeschaefte)<-name
    }
  
  }
  termingeschaefte<<-termingeschaefte
  rhandsontable(data.frame(termingeschaefte),useTypes=F, colHeaders=cnames, rowHeaders=F)
})

output$transactions <- renderRHandsontable({

  
  
  if(dim(transactions)[1]==0){
    transaction_output<<-NULL
    rhandsontable(matrix(rep("", 8), ncol=8), colHeaders=NULL)
  }else{
  date<-input$start_trans
  print(date)
  if(is.null(date)){
    transaction_output<<-data.frame(transactions)
    transaction_output[,7]<<-round(as.numeric(convert_prices(transaction_output[,7], transaction_output[,5], transaction_output[,2])),digits=2)
    rhandsontable(data.frame(transaction_output[as.Date(transaction_output[,2])<=stichtag,]),useTypes=F, colHeaders=names(transaction_output), rowHeaders=if(nrow(transaction_output)>0) 1:nrow(transaction_output) else NULL)
  }else{
    transaction_output<<-transactions[transactions[,2]>=date,]
    
    if(dim(transaction_output)[1]==0){
      transaction_output<<-NULL
      rhandsontable(matrix(rep("", 8), ncol=8), colHeaders=NULL)
      }else{
        
      transaction_output[,7]<<-round(as.numeric(convert_prices(transaction_output[,7], transaction_output[,5], transaction_output[,2])),digits=2)
      rhandsontable(data.frame(transaction_output[as.Date(transaction_output[,2])>=date & as.Date(transaction_output[,2])<=stichtag,]),useTypes=F, colHeaders=names(transaction_output), rowHeaders=if(nrow(transaction_output)>0) 1:nrow(transaction_output) else NULL)
    }
  }
}

})


observeEvent(input$scale_button,{
  
if(!is.na(total_chf)){

    factor<-input$scale_money*1000000/total_chf
    
    portfolio_data[,7]<<-round(as.numeric(portfolio_data[,7])*factor, digits=0)
    portfolio_data[,9]<<-round(as.numeric(portfolio_data[,8])*as.numeric(portfolio_data[,7]), digits=2)
    output$positions <- renderRHandsontable({
      rhandsontable(data.frame(portfolio_data),useTypes=F, colHeaders=colnames(portfolio_data))
    })
    
    
    cash_positions<<-cash_positions*factor
    cash_positions_chf<<-cash_positions_chf*factor

    total_chf_net<<-sum(as.numeric(portfolio_data[,9]))+sum(cash_positions_chf)
    cash_positions["CHF"]<<-cash_positions["CHF"]+input$scale_money*1000000-total_chf_net
    cash_positions_chf["CHF"]<<-cash_positions_chf["CHF"]+input$scale_money*1000000-total_chf_net
    
    updateNumericInput(session, "chf", value=ifelse(is.na(cash_positions["CHF"]), 0, cash_positions["CHF"]))
    updateNumericInput(session, "eur", value=ifelse(is.na(cash_positions["EUR"]), 0, cash_positions["EUR"]))
    updateNumericInput(session, "usd", value=ifelse(is.na(cash_positions["USD"]), 0, cash_positions["USD"]))
    updateNumericInput(session, "gbp", value=ifelse(is.na(cash_positions["GBP"]), 0, cash_positions["GBP"]))
    
    cash_ges<<-sum(cash_positions_chf)
    
    output$summe<-renderUI({
      total_chf<<-round(sum(as.numeric(portfolio_data[,9]))+cash_ges,digits=2)
      list(
        p(paste("Gesamt-Wert Portfolio inklusive Cash: CHF", total_chf)),
        p(paste("Anteil Cash: ", round(cash_ges*100/(round(sum(as.numeric(portfolio_data[,9]))+cash_ges)),digits=2), "%", sep="")))
    })
  
}
  
})


output$positions <- renderRHandsontable({
  
  ticker<-as.character(paste(portfolio[[3]][,1],portfolio[[3]][,2], "Equity"))
  anteile<-as.character(round(as.numeric(portfolio[[3]][,6]), digits=3))
  preise<-rep("", nrow(portfolio[[3]]))
  total<-rep("", nrow(portfolio[[3]]))
  region<-rep("", nrow(portfolio[[3]]))
  currency<-rep("", nrow(portfolio[[3]]))
  einstands_preis_chf<-rep("", nrow(portfolio[[3]]))
  date_last_order<-rep("", nrow(portfolio[[3]]))
  portfolio_data<<-data.frame(list("Ticker"=ticker,
                       "Namen"=portfolio[[3]][,4], 
                       "Kategorie"=portfolio[[3]][,3],
                       "Region"=region,
                       "Waehrung"=currency,
                       "W-Exposure"=rep(NA, nrow(portfolio[[3]])),
                       "Holdings"=anteile,
                       "Preis CHF"=preise,
                      "Ges.-Wert"=total,
                      "Anteil"=rep(NA, nrow(portfolio[[3]])),
                      "Einstandspreis"=einstands_preis_chf,
                      "Datum letzte Order"=date_last_order,
                      "Ordner Typ"=rep("", nrow(portfolio[[3]])),
                      "Performance seit letzer Order"=rep("", nrow(portfolio[[3]])),
                      "Performance seit Jahresbeginn CHF"=rep("", length(preise)),#as.numeric(as.matrix(source_data_cc(ticker, "CURRENT_TRR_YTD", stichtag)))
                      "Bemerkung"=rep("", length(preise)))
  )
  cnames<-names(portfolio_data)

  if(dim(transactions[transactions[,2]>=input$stichtag,])[1]>0){
  portfolio_data<<-backtrack_positions(portfolio_data, transactions[transactions[,2]>=input$stichtag,])
  }
  portfolio_data<<-as.matrix(portfolio_data)
  
  ind<-unique(c(which(is.na(portfolio_data[,1])),which(portfolio_data[,1]=="")))
  if(length(ind)>0)
    portfolio_data<<-portfolio_data[-ind,]
  
  portfolio_data[, 1]<<-toupper(portfolio_data[,1])
  
  if(any(is.na(portfolio_data[,2]))|any(portfolio_data[,2]=="")){
    ind<-unique(c(which(is.na(portfolio_data[,2])),which(portfolio_data[,2]=="")))
    portfolio_data[ind, 2]<<-as.character(source_data(as.character(portfolio_data[ind, 1]), "NAME"))
  }
  portfolio_data[, 2]<<-findreplace_strings(portfolio_data[,2], "&amp;", "&")
  
  if(any(is.na(portfolio_data[,3]))|any(portfolio_data[,3]=="")|any(portfolio_data[,3]=="Nein")){
    ind<-unique(c(which(is.na(portfolio_data[,3])),which(portfolio_data[,3]==""), which(portfolio_data[,3]=="Nein")))
    print(ind)
    portfolio_data[ind, 3]<<-as.character(get_asset_class(toupper(as.character(portfolio_data[ind, 1]))))
  }

  
  if(any(is.na(portfolio_data[,4]))|any(portfolio_data[,4]=="")){
    ind<-unique(c(which(is.na(portfolio_data[,4])),which(portfolio_data[,4]=="")))
    portfolio_data[ind, 4]<<-source_data_2(as.character(portfolio_data[ind, 1]), "FUND_GEO_FOCUS")
  }
  
  if(any(is.na(portfolio_data[,5]))|any(portfolio_data[,5]=="")){
    ind<-unique(c(which(is.na(portfolio_data[,5])),which(portfolio_data[,5]=="")))
    portfolio_data[ind, 5]<<-source_data_2(as.character(portfolio_data[ind, 1]), "CRNCY")
  }
  
  if(any(is.na(portfolio_data[,6]))|any(portfolio_data[,6]=="")){
    ind<-unique(c(which(is.na(portfolio_data[,6])),which(portfolio_data[,6]=="")))
    portfolio_data[ind, 6]<<-source_data_2(as.character(portfolio_data[ind, 1]), "FUND_TOTAL_ASSETS_CRNCY")
  }
  try(portfolio_data[is.na(portfolio_data[, 6]), 6]<<-portfolio_data[is.na(portfolio_data[, 6]), 5], silent=T)
  try(portfolio_data[portfolio_data[, 6]=="", 6]<<-portfolio_data[portfolio_data[, 6]=="", 5], silent=T)
  portfolio_data<<-portfolio_data
  
  if(any(is.na(portfolio_data[,8]))|any(portfolio_data[,8]=="")){
    ind<-unique(c(which(is.na(portfolio_data[,8])),which(portfolio_data[,8]=="")))
    portfolio_data[ind, 8]<<-as.numeric(as.matrix(source_data_cc(as.character(portfolio_data[ind, 1]), "PX_LAST", stichtag)))
  }
  
  if(any(is.na(portfolio_data[,12]))|any(portfolio_data[,12]=="")){
    ind<-unique(c(which(is.na(portfolio_data[,12])),which(portfolio_data[,12]=="")))
    portfolio_data[ind,12]<<-as.character(find_einstand(transactions, as.character(portfolio_data[ind, 1]),2))
  }
  
  # conv_to_date<-try(portfolio_data[,12]<-as.Date(portfolio_data[,12]), silent=T)
  # if(class(conv_to_date)[1]=="try-error"){
  #   portfolio_data[,12]<<-as.character(convertToDate(as.numeric(portfolio_data[,12]), origin = "1970-01-01"))
  # }
  
  if(any(is.na(portfolio_data[,11]))|any(portfolio_data[,11]=="")){
    ind<-unique(c(which(is.na(portfolio_data[,11])),which(portfolio_data[,11]=="")))
    a<-try(portfolio_data[ind,11]<<-convert_prices(find_einstand(transactions, as.character(portfolio_data[ind, 1]),7), portfolio_data[ind,5], as.Date(portfolio_data[ind,12])), silent=T)
    if(class(a)=="try-error"){
      portfolio_data[ind,11]<<-NA
    }  
  }
  if(any(is.na(portfolio_data[,13]))|any(portfolio_data[,13]=="")){
    ind<-unique(c(which(is.na(portfolio_data[,13])),which(portfolio_data[,13]=="")))
    portfolio_data[ind,13]<<-find_einstand(transactions, as.character(portfolio_data[ind, 1]),3)
  }

  portfolio_data[,9]<<-as.character(round(as.numeric(portfolio_data[,7])*as.numeric(portfolio_data[,8]), digits=2))
  portfolio_data[,10]<<-paste(formatC(round(as.numeric(portfolio_data[,9])/(sum(as.numeric(portfolio_data[,9]))+cash_ges)*100, digits=2),format = 'f', digits=2), "%", sep="")
  portfolio_data[,14]<<-paste(formatC(round(100*source_data_perf(as.character(portfolio_data[, 1]) , as.character(portfolio_data[,12]), stichtag , kaufpreis=as.numeric(portfolio_data[,11])), digits=2),format = 'f', digits=2), "%", sep="")
  portfolio_data[,15]<<-paste(formatC(round(100*source_data_perf(as.character(portfolio_data[, 1]) , as.Date(paste(year(stichtag-60)-1,12,31,sep="-")), stichtag), digits=2),format = 'f', digits=2), "%", sep="")

   portfolio_data[is.na(portfolio_data)]<<-""
   ticker<-portfolio_data[,1]
   anteile<-as.character(round(as.numeric(portfolio_data[,6]), digits=3))
   preise<-as.numeric(portfolio_data[, 8])
   total<-as.numeric(portfolio_data[,9])
   total<<-total
   anteil_cash<<-as.numeric(cash_ges)/(sum(total, na.rm=F)+as.numeric(cash_ges))
   # "Performance seit letzter Order"<-paste(round(100*as.numeric(einstands_preis_chf)/as.numeric(preise)-100, digits=2), "%", sep="")
   # "Anteil"=as.character((as.numeric(anteile)*as.numeric(preise))/sum(as.numeric(anteile)*as.numeric(preise), na.rm=F))
   rownames(portfolio_data)<<-1:nrow(portfolio_data)
   rhandsontable(data.frame(portfolio_data),useTypes=F, colHeaders=cnames)
})

output$summe<-renderUI({
  total_chf<<-round(sum(total)+cash_ges,digits=2)
  list(
  p(paste("Gesamt-Wert Portfolio inklusive Cash: CHF", total_chf)),
  p(paste("Anteil Cash: ", round(anteil_cash*100,digits=2), "%", sep="")))
})

observeEvent(input$update,{

  if(!is.null(input$positions)){
    withProgress(message="Bitte warten, aktualisiere Daten...",{
  aaa<<-input$positions
  
  data<-matrix(NA, nrow=length(aaa$data), ncol=length(aaa$data[[1]]))
  for(i in 1:length(aaa$data)){
    for(j in 1:length(aaa$data[[1]])){
      data[i,j]<-ifelse(is.null(aaa$data[[i]][[j]]), NA, aaa$data[[i]][[j]])
    }
  }

  ind<-unique(c(which(is.na(data[,1])),which(data[,1]=="")))
  if(length(ind)>0)
  data<-data[-ind,]

  if(any(is.na(data[,2]))|any(data[,2]=="")){
    ind<-unique(c(which(is.na(data[,2])),which(data[,2]=="")))
    try(data[ind, 2]<-as.character(source_data(as.character(data[ind, 1]), "NAME")), silent=T)
  }
  if(any(is.na(data[,3]))|any(data[,3]=="")){
    ind<-unique(c(which(is.na(data[,3])),which(data[,3]=="")))
    try(data[ind, 3]<-as.character(get_asset_class(toupper(as.character(data[ind, 1])))), silent=T)
  }

  if(any(is.na(data[,4]))|any(data[,4]=="")){
    ind<-unique(c(which(is.na(data[,4])),which(data[,4]=="")))
    try(data[ind, 4]<-as.character(source_data(as.character(data[ind, 1]), "FUND_GEO_FOCUS")), silent=T)
  }

  if(any(is.na(data[,5]))|any(data[,5]=="")){
    ind<-unique(c(which(is.na(data[,5])),which(data[,5]=="")))
    try(data[ind, 5]<-as.character(source_data(as.character(data[ind, 1]), "CRNCY")), silent=T)
  }
  if(any(is.na(data[,6]))|any(data[,6]=="")){
    ind<-unique(c(which(is.na(data[,6])),which(data[,6]=="")))
    try(data[ind, 6]<-as.character(source_data(as.character(data[ind, 1]), "FUND_TOTAL_ASSETS_CRNCY")), silent=T)
  }
  try(data[is.na(data[, 6]), 6]<-data[is.na(data[, 6]), 5], silent=T)
  try(data[data[, 6]=="", 6]<-data[data[, 6]=="", 5], silent=T)

  if(any(is.na(data[,8]))|any(data[,8]=="")){
    ind<-unique(c(which(is.na(data[,8])),which(data[,8]=="")))
    try(data[ind, 8]<-as.numeric(as.matrix(source_data_cc(as.character(data[ind, 1]), "PX_LAST", stichtag))), silent=T)
  }
  if(any(is.na(data[,12]))|any(data[,12]=="")){
    ind<-unique(c(which(is.na(data[,12])),which(data[,12]=="")))
    try(data[ind,12]<-find_einstand(transactions, as.character(data[ind, 1]),2), silent=T)
  }

  if(any(is.na(data[,11]))|any(data[,11]=="")){
    ind<-unique(c(which(is.na(data[,11])),which(data[,11]=="")))
    try(data[ind,11]<-convert_prices(as.numeric(find_einstand(transactions, as.character(data[ind, 1]),7)), data[ind,5],data[ind,12] ), silent=T)
  }

  if(any(is.na(data[,13]))|any(data[,13]=="")){
    ind<-unique(c(which(is.na(data[,13])),which(data[,13]=="")))
    try(data[ind,13]<-find_einstand(transactions, as.character(data[ind, 1]),3), silent=T)
  }
  data[is.na(data)]<-""

  try(data[,9]<-round(as.numeric(data[,7])*as.numeric(data[,8]), digits=2), silent=T)
  try(data[,10]<-paste(round(as.numeric(data[,9])/(sum(as.numeric(data[,9]))+cash_ges)*100, digits=2), "%", sep=""), silent=T)
  try(data[,14]<-paste(formatC(round(100*source_data_perf(as.character(data[, 1]) , as.character(data[,12]), stichtag ,kaufpreis=as.numeric(data[,11])), digits=2),format = 'f', digits=2), "%", sep=""), silent=T)
  try(data[,15]<-paste(formatC(round(100*source_data_perf(as.character(data[, 1]) , as.Date(paste(year(stichtag-60)-1,12,31,sep="-")), stichtag), digits=2),format = 'f', digits=2), "%", sep=""), silent=T)

  try(colnames(data)<-c("Ticker", "Name", "Kategorie", "Region", "Waehrung", "W-Exposure", 
                    "Holdings","Preis CHF","Ges.-Wert","Anteil",
                    "Einstandspreis","Datum letzte Order","Ordner Typ",
                    "Performance seit Order", "Performance seit Jahresbeginn per Stichtag -60 Tage.",
                    "Bemerkung"
                    ),silent=T)
  rownames(data)<-1:nrow(data)
  
  portfolio_data<<-data
  output$positions <- renderRHandsontable({
  rhandsontable(data.frame(portfolio_data),useTypes=F, colHeaders=colnames(data))
  })
  output$summe<-renderUI({
    total_chf<<-round(sum(as.numeric(data[,9]))+cash_ges,digits=2)
    list(
      p(paste("Gesamt-Wert Portfolio inklusive Cash: CHF", total_chf)),
      p(paste("Anteil Cash: ", round(cash_ges*100/(round(sum(as.numeric(data[,9]))+cash_ges)),digits=2), "%", sep="")))
  })
  })
  }
})



############################################################################################################

report_on_page_function<-function(current_page){
  print(current_page)
  
  withProgress(message="Bitte warten, lade Daten...",{
  
  everything_ok <- T
  text_message <-""
  
  if(current_page==0){
    if(input$kunde==""){
      everything_ok <- F
      text_message<-"Bitte waehlen Sie einen Kunden"
    }else{
      if(is.null(input$kundeninfo)){
        kundeninfo<<-kunden_tab
      }else{
        kundeninfo<<-input$kundeninfo
        kundeninfo<<-t(matrix(as.matrix(unlist(kundeninfo$data)), nrow=2))
      }
      stichtag<<-input$stichtag

      message<-try({portfolio<<-source_models(input$kunde)}, silent=T)
      if(class(message)=="try-error"){
        everything_ok <- F
        text_message<-"Daten konnten nicht geladen werden."
      }else{
      }
      
    }
    
  }
  
  if(current_page==2){
    cash_positions<<-c(input$chf , input$eur , input$usd , input$gbp)
    cash_positions_chf<<-c(input$chf , wechselkurs[1]*input$eur , wechselkurs[2]*input$usd , wechselkurs[3]*input$gbp)
    names(cash_positions)<<-c("CHF", "EUR", "USD", "GBP")
    names(cash_positions_chf)<<-c("CHF", "EUR", "USD", "GBP")
    cash_positions<<-cash_positions[!is.na(cash_positions) & cash_positions!=0]
    cash_positions_chf<<-cash_positions_chf[!is.na(cash_positions_chf) & cash_positions_chf!=0]
    save_termin()
  }
  if(current_page==3){
    
    print(total_chf)
    print(class(total_chf))
    
    if(is.na(total_chf)){
       everything_ok <- F
       text_message<-"Einige Positionen besitzen keinen Preis und koennen somit nicht gedruckt werden. Bitte ueberpruefen sie diese und gegebenenfalls loeschen Sie sie."
      
         }
  }
  
  if(current_page==4){
        save_transactions()
  }

  })
    
  return(list(everything_ok=everything_ok, text_message=text_message))

}


############################################################################################################
  # observer part:
############################################################################################################


# Initiate start page: (current_page=0)
refresh_main_panel(0)


session$onSessionEnded(function() {
  stopApp()
})


#listen to Start/Weiter/Senden-Button:
observeEvent(input$start_button,
             {
               report<<- report_on_page_function(current_page_global)
               if(report$everything_ok){
                if(current_page_global==n_pages){
                  
                  withProgress(message="Bitte warten, erstelle Ausweis...",{
                  
                  hide_name<<-input$hide_name
                  kundenbetreuer<<-input$kundenbetreuer
                  transactions<<-transaction_output
                   save(cash_positions,cash_positions_chf,hide_name,wechselkurs,kundenbetreuer,stichtag,termingeschaefte,cash_ges,transactions,portfolio_data,total_chf,kundeninfo ,file="O:/11_Personal_Folders/07_Robert_Leitner/16_Reporting/Kundendata.RData")
                   library(rmarkdown)
                   Sys.setenv(RSTUDIO_PANDOC="O:/11_Personal_Folders/07_Robert_Leitner/16_Reporting/RMarkdown/pandoc")
                   rmarkdown::render("O://11_Personal_Folders/07_Robert_Leitner/16_Reporting/RMarkdown/Bericht.Rmd",
                                     output_file="O://11_Personal_Folders/07_Robert_Leitner/16_Reporting/RMarkdown/Bericht.pdf",
                                     run_pandoc=T, envir=new.env())
                   shell.exec("O://11_Personal_Folders/07_Robert_Leitner/16_Reporting/RMarkdown/Bericht.pdf")
                   
                  })
                   
                   }
               current_page_global <<- min(current_page_global+1, n_pages)
               refresh_main_panel(current_page_global)
               output$comment_panel1<-renderUI({ NULL})
               }else{
                 showModal(modalDialog(
                   title = "Warning!",
                   report$text_message,
                   easyClose = TRUE
                 ))
               }
              
               }
             )

# listen to "Go back" button
observeEvent(input$backwards_button,{
              
               current_page_global <<- max(current_page_global-1,0)
               refresh_main_panel(current_page_global)
               
            })  



observeEvent(input$kunde,{
  
  if(input$kunde!=""){
  
    output$kundeninfo<-renderUI({
      client_list[grep(input$kunde, client_list[,1]), ]
    })
    
    if(first_time_loaded){
      first_time_loaded<<-FALSE
     output$kunden_tabelle<-renderUI({
       rHandsontableOutput("kundeninfo")
    })
    }
    output$kundeninfo <-renderRHandsontable({
     kunden_tab<<-data.frame(list(ID=c("Name", "Account", "Ref-Waehrung", "Mandat", "Depot-Bank", "Kunde seit", "Mgt-Fee in %", "Anmerkung"),
                                  ID2= c(as.character(client_list[grep(input$kunde, client_list[,1]), c(2,3,4,5,7,8,10)]), "")))
    rhandsontable(kunden_tab,rowHeaders=NULL, colHeaders=NULL, useTypes=F, selectCallback=TRUE)
    })

  }  
  
})


save_termin<-function(){
  
  if(!is.null(input$termingeschaefte_table)){
    aaa<<-input$termingeschaefte_table
    
    if(length(aaa$data)>0){
      data<-matrix(NA, nrow=length(aaa$data), ncol=length(aaa$data[[1]]))
      for(i in 1:length(aaa$data)){
        for(j in 1:length(aaa$data[[1]])){
          data[i,j]<-ifelse(is.null(aaa$data[[i]][[j]]), NA, aaa$data[[i]][[j]])
        }
      }
    if(all(data=="")){
      termingeschaefte<<-NULL
    }else{termingeschaefte<<-data}
    
    }else{
    termingeschaefte<<-NULL
    }
    
  }else{
    
  }
  
}

save_transactions<-function(){

  if(!is.null(input$transactions)){
    aaa<<-input$transactions
    if(length(aaa$data)>0){
      data<-matrix(NA, nrow=length(aaa$data), ncol=length(aaa$data[[1]]))
      for(i in 1:length(aaa$data)){
        for(j in 1:length(aaa$data[[1]])){
          data[i,j]<-ifelse(is.null(aaa$data[[i]][[j]]), NA, aaa$data[[i]][[j]])
        }
      }
      if(all(data=="")){
        transaction_output<<-NULL
      }else{
        transaction_output<<-data
        }
      
    }else{
      transaction_output<<-NULL
    }
    
  }else{
    
  }
  
}







})