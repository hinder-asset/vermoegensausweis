load("O:/11_Personal_Folders/07_Robert_Leitner/16_Reporting/Kundendata.RData")
load("O:/11_Personal_Folders/07_Robert_Leitner/16_Reporting/Classification.RData")
library(tikzDevice)
library(HAM)
library(xts)
library(lubridate)

options(digits=2)

positions<-cbind()
for(i in 1:ncol(portfolio_data)){
  positions<-cbind(positions, as.character(portfolio_data[,i]))
}
colnames(positions)<-colnames(portfolio_data)
positions[grep("NA%",positions[,14]),14]<-""
positions[,3]<-sub("Schwellenlaender", "Schwellenländer", positions[,3])

if(!is.null(transactions)){
  
  if(is.vector(transactions)){
        transactions<-t(as.matrix(transactions))
        if(is.na(transactions[,2])){
          transactions[,2]<-Sys.Date()
        }
  }
      trans<-cbind()
      for(i in 1:ncol(transactions)){
        trans<-cbind(trans, as.character(transactions[,i]))
      }
      transactions<-trans
      transactions[,2]<-na.locf(na.locf(transactions[,2], fromLast=T))
      transactions[is.na(transactions[,8]),8]<-transactions[is.na(transactions[,8]),2]
      transactions[,4]<-as.character(round(as.numeric(transactions[,4]),2))
      transactions[,6]<-findreplace_strings(transactions[,6], c("&amp;","amp;"), c("&",""))
      for(i in 1:nrow(trans)){
        try(transactions[i,6]<-sanitizeTexString(substr(transactions[i,6],0,25)), silent=T)
      }
      
      # Get date format:
      if(length(na.omit(transactions[,2]))>0){
        check_date<-na.omit(transactions[,2])[1]
        if(grepl("-", check_date)){
          
        }else if(grepl("/", check_date, fixed=T)){
          transactions[,2]<-as.character(as.Date(transactions[,2], format="%m/%d/%Y"))
          transactions[,8]<-as.character(as.Date(transactions[,8], format="%m/%d/%Y"))
        }else if(!is.na(as.numeric(check_date))){
          transactions[,2]<-as.character(as.Date(as.numeric(transactions[,2]), origin="1970-01-01"))
          transactions[,8]<-as.character(as.Date(as.numeric(transactions[,8]), origin="1970-01-01"))
        }
      }
    
}

library(plotly)

plot_pie<-function(values, labels, direc, sort=T){
    
  # Sys.setenv("plotly_username"="HAM2007")
  # Sys.setenv("plotly_api_key"="qubuepdm9h")
  Sys.setenv("plotly_username"="Hinderam")
  Sys.setenv("plotly_api_key"="gecnqs9335")

  # Sys.setenv("plotly_username"="RobertLeitner")
  # Sys.setenv("plotly_api_key"="6e4juevzpg")
  
  hgrey <- rgb(112, 128, 144, maxColorValue = 255)
  hgrey1 <- rgb(187, 187, 31, maxColorValue = 255)
  bg<-rgb(217, 217, 217, maxColorValue = 255)
  
  f1<-c(0, 43, 130)
  f3<-c(180, 226, 244)
  f2<-c(220, 230, 242)
  
  n<-length(values)
  cols<-c()
  for(k in 1:length(values)){
    
    k1<-(f3[1]-f1[1])*(k-1)/(length(values))+f1[1]
    k2<-(f3[2]-f1[2])*(k-1)/(length(values))+f1[2]
    k3<-(f3[3]-f1[3])*(k-1)/(length(values))+f1[3]
    
    cola<-rgb(k1,k2,k3, maxColorValue=255)
    cols<-c(cols, cola)
    
  }
  cols<-rev(cols)
  
  ds <- data.frame(labels = labels,values = values)
  
  text<-paste(formatC(100*(values/sum(values, na.rm=T)), digits=1, format="f"), "%", sep="")
  
  p  <- plot_ly(ds, 
                labels = ~labels, 
                values = ~values, 
                type = "pie",
                text=text,
                textposition="outside",
                direction="clockwise",
                sort=sort,
                textinfo="text",
                pull=0.04,
                marker=list(colors=cols, 
                            line=list(color=hgrey, width=0.5)), 
                domain=list(x=c(0,0.7), y=c(0,1))) %>%
        layout(       paper_bgcolor=bg,
                      plot_bgcolor=bg,
                      scene=list(x=c(0.01,0.99), y=c(0.01,0.99)),
                      xaxis=list(showgrid=FALSE, zeroline=FALSE, showticklabels=F),
                      yaxis=list(showgrid=FALSE, zeroline=FALSE, showticklabels=F),
                      legend=list(font=list(family="Arial", size=14),
                                  borderwidth=0,
                                  yanchor="right",
                                  xanchor="middle"                                  
                      ))
    plotly_IMAGE(p, width=600, height=370, format="png", scale=4, out_file=direc)
}


direc<-"O:/11_Personal_Folders/07_Robert_Leitner/16_Reporting/Talloc.png"
assets<-as.numeric(positions[,9])
names(assets)<-positions[,3]
assets<-na.omit(assets)


get_assets_by_classf1<-function(assets, classification_table, col=2){
    
  bol<-rep("Sonstige", length(assets))
    for(i in 1:length(bol)){
    if(any(grepl(toupper(names(assets[i])), as.character(toupper(classification_table[,1]))), fixed=T)){
      a<-try(bol[i]<-classification_table[grep(toupper(names(assets[i])), as.character(toupper(classification_table[,1])), fixed=T),col, drop=T], silent=T)  
      if(bol[i]=="Sonstige"){
        }
      }
    }  
  return(bol)
}
names(assets)<-get_assets_by_classf1(assets, classification_table)

assets1<-c(cash_ges, assets)
names(assets1)[1]<-"Geldmarkt"

#aggregate: 
a<-c()
for(k in 1:length(unique(names(assets1)))){
  a<-c(a, sum(as.numeric(assets1[grep(unique(names(assets1))[k], names(assets1))]), na.rm=T))
}
names(a)<-unique(names(assets1))
a<-sort(a)
a_abs<-a
a<-round(a/sum(a), digits=4)

direc_alloc<-"O:/11_Personal_Folders/07_Robert_Leitner/16_Reporting/Allocation.png"
try({
plot_pie(a, names(a) , direc_alloc, sort=T)
},silent=T)

get_assets_by_class<-function(positions, class){
  
  if(class=="Sonstige"){
    bol<-rep(TRUE, nrow(positions))
    for(i in 1:length(bol)){
      if(any(toupper(positions[i,"Kategorie"])==toupper(classification_table[,1]))){
        bol[i]<-F
      }
    }
  }else{
    bol<-rep(FALSE, nrow(positions))
    
    for(i in 1:length(bol)){
      if(any(grepl(positions[i,"Kategorie"], classification_table[,1]))){
        if(classification_table[which(toupper(positions[i,"Kategorie"])==toupper(classification_table[,1])),2]==class){
          bol[i]<-T
        }
      }
    }
  }
  
  return(positions[bol,])
}

direc_a<-"O:/11_Personal_Folders/07_Robert_Leitner/16_Reporting/AAllocation.png"
t1<-try({

b<-get_assets_by_class(positions, "Aktien")
assets<-as.numeric(b[,9])
names(assets)<-b[,3]
assets<-na.omit(assets)
names(assets)<-get_assets_by_classf1(assets, classification_table, col=3)

b<-c()
for(k in 1:length(unique(names(assets)))){
  b<-c(b, sum(as.numeric(assets[grep(unique(names(assets))[k], names(assets), fixed=T)]), na.rm=T))
}
names(b)<-unique(names(assets))
b<-sort(b)
b_abs<-b
b<-round(b/sum(b), digits=4)

plot_pie(b, names(b) , direc_a, sort=T)

}, silent=T)
# if(class(t1)=="try-error"){
#   
#   plot_pie(100, "Keine Aktien vorhanden." , direc_a, sort=T)
# }

#######################
#Obligationen Chart
direc_region<-"O:/11_Personal_Folders/07_Robert_Leitner/16_Reporting/RegionAllocation.png"
t1<-try({
        c<-get_assets_by_class(positions, "Obligationen")
        obli<-as.numeric(c[,9])
        names(obli)<-c[,3]
        obli<-na.omit(obli)
        names(obli)<-get_assets_by_classf1(obli, classification_table, col=4)
        
        c<-c()
        for(k in 1:length(unique(names(obli)))){
          c<-c(c, sum(as.numeric(obli[grep(unique(names(obli))[k], names(obli))]), na.rm=T))
        }
        names(c)<-unique(names(obli))
        c<-sort(c)
        c_abs<-c
        c<-round(c/sum(c), digits=4)
        
        plot_pie(c, names(c) , direc_region, sort=T)
}, silent=T)

# if(class(t1)=="try-error"){
#   
#   plot_pie(100, "Keine Obligationen vorhanden." , direc_region, sort=T)
# }



#######################
#Währungen Chart

w<-as.numeric(positions[,9])
names(w)<-toupper(positions[,5])
# for(i in 1:length(w)){
#   if(toupper(positions[i,6])!="FALSE"& toupper(positions[i,6])!="NA(FALSE)" & toupper(positions[i,6])!="NO" & toupper(positions[i,6])!="" & !is.na(toupper(positions[i,6]))){
#     names(w)[i]<-toupper(positions[i,6])
#   }
# }

t1<-try({
w<-c(w, cash_positions_chf)
names(w)<-toupper(names(w))
w<-na.omit(w)
w<-w[w>0]

wc<-c()
for(k in 1:length(unique(names(w)))){
  wc<-c(wc, sum(as.numeric(w[grep(unique(names(w))[k], names(w))]), na.rm=T))
}
names(wc)<-unique(names(w))
wc<-sort(wc)
wc_abs<-wc
wc<-round(wc/sum(wc), digits=4)

n_wc<-names(wc)

n_new<-n_wc
for(i in 1:length(n_new)){
  try(n_new[i]<-classification_currency[grep(n_wc[i], classification_currency[,1], fixed=T),2], silent=T)
}

names(wc)<-n_new
names(wc_abs)<-n_new


direc_region<-"O:/11_Personal_Folders/07_Robert_Leitner/16_Reporting/WAllocation.png"
plot_pie(wc, names(wc) , direc_region, sort=T)
},silent=T)


save.image("O:/11_Personal_Folders/07_Robert_Leitner/16_Reporting/Kundendata_Rev.RData")


