library(ecb)
library(plyr)
library(tidyr)
library(pdfetch)

#########utility functions#########
sector <- function(input) {
  if (input=="mfi") return ("Banks")
  else if (input == "ofi" || input == "iv" || input == "icpf") return ("Non-banks")
  else if (input == "govt" || input == "hh" || input == "nfc") return ("Real economy")
  else return ("row")
}

toFrame <- function(input){
  #process the name
  str <- deparse(substitute(input))  
  strArr <- unlist(strsplit(str, "[_]"))
  #process the data
  input<-data.frame(input)
  input<-data.frame("source"=strArr[1],"target"=strArr[2],"type"=strArr[3],"time"=input$obstime,"value"=input$obsvalue)
  return (input)
}

toFrame2 <- function(input){
  #process the name
  str <- deparse(substitute(input))  
  strArr <- unlist(strsplit(str, "[_]"))
  #process the data
  input<-data.frame(input)
  input<-data.frame("Node"=strArr[1],"Sector"=sector(strArr[1]),"time"=input$obstime,"value"=input$obsvalue)
  return (input)
}

formatDate <- function(input) {
  yearQuarter<-unlist(strsplit(toString(input), "[-]"))
  quarter<-NULL
  if (yearQuarter[2]=="Q1") {
    yearQuarter[2]<-"03"
  }
  else if (yearQuarter[2]=="Q2") {
    yearQuarter[2]<-"06"
  }
  else if (yearQuarter[2]=="Q3") {
    yearQuarter[2]<-"09"
  }
  else if (yearQuarter[2]=="Q4") {
    yearQuarter[2]<-"12"
  }
  date<-paste(yearQuarter[1],yearQuarter[2],sep="")
  return (date)
}
#########data#########
#time horizon
startPeriod<-"2013-Q4"
endPeriod<-"2017-Q4"


###non mmf investment funds
##bonds
#st
iv_nfc_debtSt<-get_data("QSA.Q.N.LU.W2.S124.S11.N.A.LE.F3.S._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
iv_iv_debtSt<-get_data("QSA.Q.N.LU.W2.S124.S124.N.A.LE.F3.S._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
iv_mfi_debtSt<-get_data("QSA.Q.N.LU.W2.S124.S12K.N.A.LE.F3.S._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
iv_ofi_debtSt<-get_data("QSA.Q.N.LU.W2.S124.S12O.N.A.LE.F3.S._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
iv_icpf_debtSt<-get_data("QSA.Q.N.LU.W2.S124.S12Q.N.A.LE.F3.S._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
iv_govt_debtSt<-get_data("QSA.Q.N.LU.W2.S124.S13.N.A.LE.F3.S._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
iv_hh_debtSt<-get_data("QSA.Q.N.LU.W2.S124.S1M.N.A.LE.F3.S._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))

#lt
iv_nfc_debtLt<-get_data("QSA.Q.N.LU.W2.S124.S11.N.A.LE.F3.L._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
iv_iv_debtLt<-get_data("QSA.Q.N.LU.W2.S124.S124.N.A.LE.F3.L._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
iv_mfi_debtLt<-get_data("QSA.Q.N.LU.W2.S124.S12K.N.A.LE.F3.L._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
iv_ofi_debtLt<-get_data("QSA.Q.N.LU.W2.S124.S12O.N.A.LE.F3.L._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
iv_icpf_debtLt<-get_data("QSA.Q.N.LU.W2.S124.S12Q.N.A.LE.F3.L._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
iv_govt_debtLt<-get_data("QSA.Q.N.LU.W2.S124.S13.N.A.LE.F3.L._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
iv_hh_debtLt<-get_data("QSA.Q.N.LU.W2.S124.S1M.N.A.LE.F3.L._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))

##loans
#st
iv_nfc_loanSt<-get_data("QSA.Q.N.LU.W2.S124.S11.N.A.LE.F4.S._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
iv_iv_loanSt<-get_data("QSA.Q.N.LU.W2.S124.S124.N.A.LE.F4.S._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
iv_ofi_loanSt<-get_data("QSA.Q.N.LU.W2.S124.S12O.N.A.LE.F4.S._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
iv_icpf_loanSt<-get_data("QSA.Q.N.LU.W2.S124.S12Q.N.A.LE.F4.S._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
iv_govt_loanSt<-get_data("QSA.Q.N.LU.W2.S124.S13.N.A.LE.F4.S._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
iv_hh_loanSt<-get_data("QSA.Q.N.LU.W2.S124.S1M.N.A.LE.F4.S._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))

#lt
iv_nfc_loanLt<-get_data("QSA.Q.N.LU.W2.S124.S11.N.A.LE.F4.L._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
iv_iv_loanLt<-get_data("QSA.Q.N.LU.W2.S124.S124.N.A.LE.F4.L._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
iv_ofi_loanLt<-get_data("QSA.Q.N.LU.W2.S124.S12O.N.A.LE.F4.L._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
iv_icpf_loanLt<-get_data("QSA.Q.N.LU.W2.S124.S12Q.N.A.LE.F4.L._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
iv_govt_loanLt<-get_data("QSA.Q.N.LU.W2.S124.S13.N.A.LE.F4.L._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
iv_hh_loanLt<-get_data("QSA.Q.N.LU.W2.S124.S1M.N.A.LE.F4.L._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))

##shares
iv_nfc_shares<-get_data("QSA.Q.N.LU.W2.S124.S11.N.A.LE.F511._Z._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
iv_iv_shares<-get_data("QSA.Q.N.LU.W2.S124.S124.N.A.LE.F511._Z._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
iv_mfi_shares<-get_data("QSA.Q.N.LU.W2.S124.S12K.N.A.LE.F511._Z._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
iv_ofi_shares<-get_data("QSA.Q.N.LU.W2.S124.S12O.N.A.LE.F511._Z._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
iv_icpf_shares<-get_data("QSA.Q.N.LU.W2.S124.S12Q.N.A.LE.F511._Z._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
iv_govt_shares<-get_data("QSA.Q.N.LU.W2.S124.S13.N.A.LE.F511._Z._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))

##iv_shares
iv_iv_ivShares<-get_data("QSA.Q.N.LU.W2.S124.S124.N.A.LE.F52._Z._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
iv_mfi_ivShares<-get_data("QSA.Q.N.LU.W2.S124.S12K.N.A.LE.F52._Z._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))

###ofi
##bonds
#st
ofi_nfc_debtSt<-get_data("QSA.Q.N.LU.W2.S12O.S11.N.A.LE.F3.S._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
ofi_iv_debtSt<-get_data("QSA.Q.N.LU.W2.S12O.S124.N.A.LE.F3.S._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
ofi_mfi_debtSt<-get_data("QSA.Q.N.LU.W2.S12O.S12K.N.A.LE.F3.S._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
ofi_ofi_debtSt<-get_data("QSA.Q.N.LU.W2.S12O.S12O.N.A.LE.F3.S._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
ofi_icpf_debtSt<-get_data("QSA.Q.N.LU.W2.S12O.S12Q.N.A.LE.F3.S._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
ofi_govt_debtSt<-get_data("QSA.Q.N.LU.W2.S12O.S13.N.A.LE.F3.S._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
ofi_hh_debtSt<-get_data("QSA.Q.N.LU.W2.S12O.S1M.N.A.LE.F3.S._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))

#lt
ofi_nfc_debtLt<-get_data("QSA.Q.N.LU.W2.S12O.S11.N.A.LE.F3.L._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
ofi_iv_debtLt<-get_data("QSA.Q.N.LU.W2.S12O.S124.N.A.LE.F3.L._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
ofi_mfi_debtLt<-get_data("QSA.Q.N.LU.W2.S12O.S12K.N.A.LE.F3.L._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
ofi_ofi_debtLt<-get_data("QSA.Q.N.LU.W2.S12O.S12O.N.A.LE.F3.L._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
ofi_icpf_debtLt<-get_data("QSA.Q.N.LU.W2.S12O.S12Q.N.A.LE.F3.L._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
ofi_govt_debtLt<-get_data("QSA.Q.N.LU.W2.S12O.S13.N.A.LE.F3.L._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
ofi_hh_debtLt<-get_data("QSA.Q.N.LU.W2.S12O.S1M.N.A.LE.F3.L._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))

##loans
#st
ofi_nfc_loanSt<-get_data("QSA.Q.N.LU.W2.S12O.S11.N.A.LE.F4.S._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
ofi_iv_loanSt<-get_data("QSA.Q.N.LU.W2.S12O.S124.N.A.LE.F4.S._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
ofi_ofi_loanSt<-get_data("QSA.Q.N.LU.W2.S12O.S12O.N.A.LE.F4.S._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
ofi_icpf_loanSt<-get_data("QSA.Q.N.LU.W2.S12O.S12Q.N.A.LE.F4.S._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
ofi_govt_loanSt<-get_data("QSA.Q.N.LU.W2.S12O.S13.N.A.LE.F4.S._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
ofi_hh_loanSt<-get_data("QSA.Q.N.LU.W2.S12O.S1M.N.A.LE.F4.S._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))

#lt
ofi_nfc_loanLt<-get_data("QSA.Q.N.LU.W2.S12O.S11.N.A.LE.F4.L._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
ofi_iv_loanLt<-get_data("QSA.Q.N.LU.W2.S12O.S124.N.A.LE.F4.L._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
ofi_ofi_loanLt<-get_data("QSA.Q.N.LU.W2.S12O.S12O.N.A.LE.F4.L._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
ofi_icpf_loanLt<-get_data("QSA.Q.N.LU.W2.S12O.S12Q.N.A.LE.F4.L._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
ofi_govt_loanLt<-get_data("QSA.Q.N.LU.W2.S12O.S13.N.A.LE.F4.L._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
ofi_hh_loanLt<-get_data("QSA.Q.N.LU.W2.S12O.S1M.N.A.LE.F4.L._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))

##shares
ofi_nfc_shares<-get_data("QSA.Q.N.LU.W2.S12O.S11.N.A.LE.F511._Z._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
ofi_iv_shares<-get_data("QSA.Q.N.LU.W2.S12O.S124.N.A.LE.F511._Z._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
ofi_mfi_shares<-get_data("QSA.Q.N.LU.W2.S12O.S12K.N.A.LE.F511._Z._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
ofi_ofi_shares<-get_data("QSA.Q.N.LU.W2.S12O.S12O.N.A.LE.F511._Z._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
ofi_icpf_shares<-get_data("QSA.Q.N.LU.W2.S12O.S12Q.N.A.LE.F511._Z._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
ofi_govt_shares<-get_data("QSA.Q.N.LU.W2.S12O.S13.N.A.LE.F511._Z._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))

##iv_shares
ofi_iv_ivShares<-get_data("QSA.Q.N.LU.W2.S12O.S124.N.A.LE.F52._Z._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
ofi_mfi_ivShares<-get_data("QSA.Q.N.LU.W2.S12O.S12K.N.A.LE.F52._Z._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))

###nfc
##bonds
#st
nfc_nfc_debtSt<-get_data("QSA.Q.N.LU.W2.S11.S11.N.A.LE.F3.S._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
nfc_iv_debtSt<-get_data("QSA.Q.N.LU.W2.S11.S124.N.A.LE.F3.S._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
nfc_mfi_debtSt<-get_data("QSA.Q.N.LU.W2.S11.S12K.N.A.LE.F3.S._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
nfc_ofi_debtSt<-get_data("QSA.Q.N.LU.W2.S11.S12O.N.A.LE.F3.S._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
nfc_icpf_debtSt<-get_data("QSA.Q.N.LU.W2.S11.S12Q.N.A.LE.F3.S._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
nfc_govt_debtSt<-get_data("QSA.Q.N.LU.W2.S11.S13.N.A.LE.F3.S._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
nfc_hh_debtSt<-get_data("QSA.Q.N.LU.W2.S11.S1M.N.A.LE.F3.S._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))

#lt
nfc_nfc_debtLt<-get_data("QSA.Q.N.LU.W2.S11.S11.N.A.LE.F3.L._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
nfc_iv_debtLt<-get_data("QSA.Q.N.LU.W2.S11.S124.N.A.LE.F3.L._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
nfc_mfi_debtLt<-get_data("QSA.Q.N.LU.W2.S11.S12K.N.A.LE.F3.L._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
nfc_ofi_debtLt<-get_data("QSA.Q.N.LU.W2.S11.S12O.N.A.LE.F3.L._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
nfc_icpf_debtLt<-get_data("QSA.Q.N.LU.W2.S11.S12Q.N.A.LE.F3.L._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
nfc_govt_debtLt<-get_data("QSA.Q.N.LU.W2.S11.S13.N.A.LE.F3.L._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
nfc_hh_debtLt<-get_data("QSA.Q.N.LU.W2.S11.S1M.N.A.LE.F3.L._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))

##loans
#st
nfc_nfc_loanSt<-get_data("QSA.Q.N.LU.W2.S11.S11.N.A.LE.F4.S._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
nfc_iv_loanSt<-get_data("QSA.Q.N.LU.W2.S11.S124.N.A.LE.F4.S._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
nfc_ofi_loanSt<-get_data("QSA.Q.N.LU.W2.S11.S12O.N.A.LE.F4.S._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
nfc_icpf_loanSt<-get_data("QSA.Q.N.LU.W2.S11.S12Q.N.A.LE.F4.S._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
nfc_govt_loanSt<-get_data("QSA.Q.N.LU.W2.S11.S13.N.A.LE.F4.S._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
nfc_hh_loanSt<-get_data("QSA.Q.N.LU.W2.S11.S1M.N.A.LE.F4.S._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))

#lt
nfc_nfc_loanLt<-get_data("QSA.Q.N.LU.W2.S11.S11.N.A.LE.F4.L._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
nfc_iv_loanLt<-get_data("QSA.Q.N.LU.W2.S11.S124.N.A.LE.F4.L._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
nfc_ofi_loanLt<-get_data("QSA.Q.N.LU.W2.S11.S12O.N.A.LE.F4.L._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
nfc_icpf_loanLt<-get_data("QSA.Q.N.LU.W2.S11.S12Q.N.A.LE.F4.L._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
nfc_govt_loanLt<-get_data("QSA.Q.N.LU.W2.S11.S13.N.A.LE.F4.L._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
nfc_hh_loanLt<-get_data("QSA.Q.N.LU.W2.S11.S1M.N.A.LE.F4.L._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))

##shares
nfc_nfc_shares<-get_data("QSA.Q.N.LU.W2.S11.S11.N.A.LE.F511._Z._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
nfc_iv_shares<-get_data("QSA.Q.N.LU.W2.S11.S124.N.A.LE.F511._Z._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
nfc_mfi_shares<-get_data("QSA.Q.N.LU.W2.S11.S12K.N.A.LE.F511._Z._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
nfc_ofi_shares<-get_data("QSA.Q.N.LU.W2.S11.S12O.N.A.LE.F511._Z._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
nfc_icpf_shares<-get_data("QSA.Q.N.LU.W2.S11.S12Q.N.A.LE.F511._Z._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
nfc_govt_shares<-get_data("QSA.Q.N.LU.W2.S11.S13.N.A.LE.F511._Z._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))

##iv_shares
nfc_iv_ivShares<-get_data("QSA.Q.N.LU.W2.S11.S124.N.A.LE.F52._Z._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
nfc_mfi_ivShares<-get_data("QSA.Q.N.LU.W2.S11.S12K.N.A.LE.F52._Z._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))

###mfi
##bonds
#st
mfi_nfc_debtSt<-get_data("QSA.Q.N.LU.W2.S12K.S11.N.A.LE.F3.S._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
mfi_iv_debtSt<-get_data("QSA.Q.N.LU.W2.S12K.S124.N.A.LE.F3.S._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
mfi_mfi_debtSt<-get_data("QSA.Q.N.LU.W2.S12K.S12K.N.A.LE.F3.S._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
mfi_ofi_debtSt<-get_data("QSA.Q.N.LU.W2.S12K.S12O.N.A.LE.F3.S._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
mfi_icpf_debtSt<-get_data("QSA.Q.N.LU.W2.S12K.S12Q.N.A.LE.F3.S._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
mfi_govt_debtSt<-get_data("QSA.Q.N.LU.W2.S12K.S13.N.A.LE.F3.S._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
mfi_hh_debtSt<-get_data("QSA.Q.N.LU.W2.S12K.S1M.N.A.LE.F3.S._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))

#lt
mfi_nfc_debtLt<-get_data("QSA.Q.N.LU.W2.S12K.S11.N.A.LE.F3.L._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
mfi_iv_debtLt<-get_data("QSA.Q.N.LU.W2.S12K.S124.N.A.LE.F3.L._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
mfi_mfi_debtLt<-get_data("QSA.Q.N.LU.W2.S12K.S12K.N.A.LE.F3.L._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
mfi_ofi_debtLt<-get_data("QSA.Q.N.LU.W2.S12K.S12O.N.A.LE.F3.L._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
mfi_icpf_debtLt<-get_data("QSA.Q.N.LU.W2.S12K.S12Q.N.A.LE.F3.L._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
mfi_govt_debtLt<-get_data("QSA.Q.N.LU.W2.S12K.S13.N.A.LE.F3.L._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
mfi_hh_debtLt<-get_data("QSA.Q.N.LU.W2.S12K.S1M.N.A.LE.F3.L._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))

##loans
#st
mfi_nfc_loanSt<-get_data("QSA.Q.N.LU.W2.S12K.S11.N.A.LE.F4.S._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
mfi_iv_loanSt<-get_data("QSA.Q.N.LU.W2.S12K.S124.N.A.LE.F4.S._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
mfi_ofi_loanSt<-get_data("QSA.Q.N.LU.W2.S12K.S12O.N.A.LE.F4.S._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
mfi_icpf_loanSt<-get_data("QSA.Q.N.LU.W2.S12K.S12Q.N.A.LE.F4.S._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
mfi_govt_loanSt<-get_data("QSA.Q.N.LU.W2.S12K.S13.N.A.LE.F4.S._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
mfi_hh_loanSt<-get_data("QSA.Q.N.LU.W2.S12K.S1M.N.A.LE.F4.S._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))

#lt
mfi_nfc_loanLt<-get_data("QSA.Q.N.LU.W2.S12K.S11.N.A.LE.F4.L._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
mfi_iv_loanLt<-get_data("QSA.Q.N.LU.W2.S12K.S124.N.A.LE.F4.L._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
mfi_ofi_loanLt<-get_data("QSA.Q.N.LU.W2.S12K.S12O.N.A.LE.F4.L._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
mfi_icpf_loanLt<-get_data("QSA.Q.N.LU.W2.S12K.S12Q.N.A.LE.F4.L._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
mfi_govt_loanLt<-get_data("QSA.Q.N.LU.W2.S12K.S13.N.A.LE.F4.L._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
mfi_hh_loanLt<-get_data("QSA.Q.N.LU.W2.S12K.S1M.N.A.LE.F4.L._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))

##shares
mfi_nfc_shares<-get_data("QSA.Q.N.LU.W2.S12K.S11.N.A.LE.F511._Z._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
mfi_iv_shares<-get_data("QSA.Q.N.LU.W2.S12K.S124.N.A.LE.F511._Z._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
mfi_mfi_shares<-get_data("QSA.Q.N.LU.W2.S12K.S12K.N.A.LE.F511._Z._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
mfi_ofi_shares<-get_data("QSA.Q.N.LU.W2.S12K.S12O.N.A.LE.F511._Z._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
mfi_icpf_shares<-get_data("QSA.Q.N.LU.W2.S12K.S12Q.N.A.LE.F511._Z._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
mfi_govt_shares<-get_data("QSA.Q.N.LU.W2.S12K.S13.N.A.LE.F511._Z._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))

##iv_shares
mfi_iv_ivShares<-get_data("QSA.Q.N.LU.W2.S12K.S124.N.A.LE.F52._Z._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
mfi_mfi_ivShares<-get_data("QSA.Q.N.LU.W2.S12K.S12K.N.A.LE.F52._Z._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))

###icpf
##bonds
#st
icpf_nfc_debtSt<-get_data("QSA.Q.N.LU.W2.S12Q.S11.N.A.LE.F3.S._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
icpf_iv_debtSt<-get_data("QSA.Q.N.LU.W2.S12Q.S124.N.A.LE.F3.S._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
icpf_mfi_debtSt<-get_data("QSA.Q.N.LU.W2.S12Q.S12K.N.A.LE.F3.S._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
icpf_ofi_debtSt<-get_data("QSA.Q.N.LU.W2.S12Q.S12O.N.A.LE.F3.S._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
icpf_icpf_debtSt<-get_data("QSA.Q.N.LU.W2.S12Q.S12Q.N.A.LE.F3.S._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
icpf_govt_debtSt<-get_data("QSA.Q.N.LU.W2.S12Q.S13.N.A.LE.F3.S._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
icpf_hh_debtSt<-get_data("QSA.Q.N.LU.W2.S12Q.S1M.N.A.LE.F3.S._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))

#lt
icpf_nfc_debtLt<-get_data("QSA.Q.N.LU.W2.S12Q.S11.N.A.LE.F3.L._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
icpf_iv_debtLt<-get_data("QSA.Q.N.LU.W2.S12Q.S124.N.A.LE.F3.L._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
icpf_mfi_debtLt<-get_data("QSA.Q.N.LU.W2.S12Q.S12K.N.A.LE.F3.L._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
icpf_ofi_debtLt<-get_data("QSA.Q.N.LU.W2.S12Q.S12O.N.A.LE.F3.L._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
icpf_icpf_debtLt<-get_data("QSA.Q.N.LU.W2.S12Q.S12Q.N.A.LE.F3.L._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
icpf_govt_debtLt<-get_data("QSA.Q.N.LU.W2.S12Q.S13.N.A.LE.F3.L._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
icpf_hh_debtLt<-get_data("QSA.Q.N.LU.W2.S12Q.S1M.N.A.LE.F3.L._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))

##loans
#st
icpf_nfc_loanSt<-get_data("QSA.Q.N.LU.W2.S12Q.S11.N.A.LE.F4.S._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
icpf_iv_loanSt<-get_data("QSA.Q.N.LU.W2.S12Q.S124.N.A.LE.F4.S._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
icpf_ofi_loanSt<-get_data("QSA.Q.N.LU.W2.S12Q.S12O.N.A.LE.F4.S._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
icpf_icpf_loanSt<-get_data("QSA.Q.N.LU.W2.S12Q.S12Q.N.A.LE.F4.S._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
icpf_govt_loanSt<-get_data("QSA.Q.N.LU.W2.S12Q.S13.N.A.LE.F4.S._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
icpf_hh_loanSt<-get_data("QSA.Q.N.LU.W2.S12Q.S1M.N.A.LE.F4.S._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))

#lt
icpf_nfc_loanLt<-get_data("QSA.Q.N.LU.W2.S12Q.S11.N.A.LE.F4.L._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
icpf_iv_loanLt<-get_data("QSA.Q.N.LU.W2.S12Q.S124.N.A.LE.F4.L._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
icpf_ofi_loanLt<-get_data("QSA.Q.N.LU.W2.S12Q.S12O.N.A.LE.F4.L._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
icpf_icpf_loanLt<-get_data("QSA.Q.N.LU.W2.S12Q.S12Q.N.A.LE.F4.L._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
icpf_govt_loanLt<-get_data("QSA.Q.N.LU.W2.S12Q.S13.N.A.LE.F4.L._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
icpf_hh_loanLt<-get_data("QSA.Q.N.LU.W2.S12Q.S1M.N.A.LE.F4.L._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))

##shares
icpf_nfc_shares<-get_data("QSA.Q.N.LU.W2.S12Q.S11.N.A.LE.F511._Z._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
icpf_iv_shares<-get_data("QSA.Q.N.LU.W2.S12Q.S124.N.A.LE.F511._Z._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
icpf_mfi_shares<-get_data("QSA.Q.N.LU.W2.S12Q.S12K.N.A.LE.F511._Z._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
icpf_ofi_shares<-get_data("QSA.Q.N.LU.W2.S12Q.S12O.N.A.LE.F511._Z._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
icpf_icpf_shares<-get_data("QSA.Q.N.LU.W2.S12Q.S12Q.N.A.LE.F511._Z._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
icpf_govt_shares<-get_data("QSA.Q.N.LU.W2.S12Q.S13.N.A.LE.F511._Z._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))

##iv_shares
icpf_iv_ivShares<-get_data("QSA.Q.N.LU.W2.S12Q.S124.N.A.LE.F52._Z._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
icpf_mfi_ivShares<-get_data("QSA.Q.N.LU.W2.S12Q.S12K.N.A.LE.F52._Z._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))

###govt
##bonds
#st
govt_nfc_debtSt<-get_data("QSA.Q.N.LU.W2.S13.S11.N.A.LE.F3.S._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
govt_iv_debtSt<-get_data("QSA.Q.N.LU.W2.S13.S124.N.A.LE.F3.S._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
govt_mfi_debtSt<-get_data("QSA.Q.N.LU.W2.S13.S12K.N.A.LE.F3.S._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
govt_ofi_debtSt<-get_data("QSA.Q.N.LU.W2.S13.S12O.N.A.LE.F3.S._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
govt_icpf_debtSt<-get_data("QSA.Q.N.LU.W2.S13.S12Q.N.A.LE.F3.S._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
govt_govt_debtSt<-get_data("QSA.Q.N.LU.W2.S13.S13.N.A.LE.F3.S._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
govt_hh_debtSt<-get_data("QSA.Q.N.LU.W2.S13.S1M.N.A.LE.F3.S._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))

#lt
govt_nfc_debtLt<-get_data("QSA.Q.N.LU.W2.S13.S11.N.A.LE.F3.L._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
govt_iv_debtLt<-get_data("QSA.Q.N.LU.W2.S13.S124.N.A.LE.F3.L._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
govt_mfi_debtLt<-get_data("QSA.Q.N.LU.W2.S13.S12K.N.A.LE.F3.L._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
govt_ofi_debtLt<-get_data("QSA.Q.N.LU.W2.S13.S12O.N.A.LE.F3.L._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
govt_icpf_debtLt<-get_data("QSA.Q.N.LU.W2.S13.S12Q.N.A.LE.F3.L._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
govt_govt_debtLt<-get_data("QSA.Q.N.LU.W2.S13.S13.N.A.LE.F3.L._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
govt_hh_debtLt<-get_data("QSA.Q.N.LU.W2.S13.S1M.N.A.LE.F3.L._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))

##loans
#st
govt_nfc_loanSt<-get_data("QSA.Q.N.LU.W2.S13.S11.N.A.LE.F4.S._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
govt_iv_loanSt<-get_data("QSA.Q.N.LU.W2.S13.S124.N.A.LE.F4.S._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
govt_ofi_loanSt<-get_data("QSA.Q.N.LU.W2.S13.S12O.N.A.LE.F4.S._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
govt_icpf_loanSt<-get_data("QSA.Q.N.LU.W2.S13.S12Q.N.A.LE.F4.S._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
govt_govt_loanSt<-get_data("QSA.Q.N.LU.W2.S13.S13.N.A.LE.F4.S._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
govt_hh_loanSt<-get_data("QSA.Q.N.LU.W2.S13.S1M.N.A.LE.F4.S._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))

#lt
govt_nfc_loanLt<-get_data("QSA.Q.N.LU.W2.S13.S11.N.A.LE.F4.L._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
govt_iv_loanLt<-get_data("QSA.Q.N.LU.W2.S13.S124.N.A.LE.F4.L._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
govt_ofi_loanLt<-get_data("QSA.Q.N.LU.W2.S13.S12O.N.A.LE.F4.L._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
govt_icpf_loanLt<-get_data("QSA.Q.N.LU.W2.S13.S12Q.N.A.LE.F4.L._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
govt_govt_loanLt<-get_data("QSA.Q.N.LU.W2.S13.S13.N.A.LE.F4.L._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
govt_hh_loanLt<-get_data("QSA.Q.N.LU.W2.S13.S1M.N.A.LE.F4.L._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))

##shares
govt_nfc_shares<-get_data("QSA.Q.N.LU.W2.S13.S11.N.A.LE.F511._Z._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
govt_iv_shares<-get_data("QSA.Q.N.LU.W2.S13.S124.N.A.LE.F511._Z._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
govt_mfi_shares<-get_data("QSA.Q.N.LU.W2.S13.S12K.N.A.LE.F511._Z._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
govt_ofi_shares<-get_data("QSA.Q.N.LU.W2.S13.S12O.N.A.LE.F511._Z._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
govt_icpf_shares<-get_data("QSA.Q.N.LU.W2.S13.S12Q.N.A.LE.F511._Z._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
govt_govt_shares<-get_data("QSA.Q.N.LU.W2.S13.S13.N.A.LE.F511._Z._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))

##iv_shares
govt_iv_ivShares<-get_data("QSA.Q.N.LU.W2.S13.S124.N.A.LE.F52._Z._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
govt_mfi_ivShares<-get_data("QSA.Q.N.LU.W2.S13.S12K.N.A.LE.F52._Z._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))

###hh
##bonds
#st
hh_nfc_debtSt<-get_data("QSA.Q.N.LU.W2.S1M.S11.N.A.LE.F3.S._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
hh_iv_debtSt<-get_data("QSA.Q.N.LU.W2.S1M.S124.N.A.LE.F3.S._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
hh_mfi_debtSt<-get_data("QSA.Q.N.LU.W2.S1M.S12K.N.A.LE.F3.S._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
hh_ofi_debtSt<-get_data("QSA.Q.N.LU.W2.S1M.S12O.N.A.LE.F3.S._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
hh_icpf_debtSt<-get_data("QSA.Q.N.LU.W2.S1M.S12Q.N.A.LE.F3.S._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
hh_govt_debtSt<-get_data("QSA.Q.N.LU.W2.S1M.S13.N.A.LE.F3.S._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
hh_hh_debtSt<-get_data("QSA.Q.N.LU.W2.S1M.S1M.N.A.LE.F3.S._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))

#lt
hh_nfc_debtLt<-get_data("QSA.Q.N.LU.W2.S1M.S11.N.A.LE.F3.L._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
hh_iv_debtLt<-get_data("QSA.Q.N.LU.W2.S1M.S124.N.A.LE.F3.L._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
hh_mfi_debtLt<-get_data("QSA.Q.N.LU.W2.S1M.S12K.N.A.LE.F3.L._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
hh_ofi_debtLt<-get_data("QSA.Q.N.LU.W2.S1M.S12O.N.A.LE.F3.L._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
hh_icpf_debtLt<-get_data("QSA.Q.N.LU.W2.S1M.S12Q.N.A.LE.F3.L._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
hh_govt_debtLt<-get_data("QSA.Q.N.LU.W2.S1M.S13.N.A.LE.F3.L._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
hh_hh_debtLt<-get_data("QSA.Q.N.LU.W2.S1M.S1M.N.A.LE.F3.L._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))

##loans
#st
hh_nfc_loanSt<-get_data("QSA.Q.N.LU.W2.S1M.S11.N.A.LE.F4.S._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
hh_iv_loanSt<-get_data("QSA.Q.N.LU.W2.S1M.S124.N.A.LE.F4.S._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
hh_ofi_loanSt<-get_data("QSA.Q.N.LU.W2.S1M.S12O.N.A.LE.F4.S._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
hh_icpf_loanSt<-get_data("QSA.Q.N.LU.W2.S1M.S12Q.N.A.LE.F4.S._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
hh_govt_loanSt<-get_data("QSA.Q.N.LU.W2.S1M.S13.N.A.LE.F4.S._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
hh_hh_loanSt<-get_data("QSA.Q.N.LU.W2.S1M.S1M.N.A.LE.F4.S._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))

#lt
hh_nfc_loanLt<-get_data("QSA.Q.N.LU.W2.S1M.S11.N.A.LE.F4.L._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
hh_iv_loanLt<-get_data("QSA.Q.N.LU.W2.S1M.S124.N.A.LE.F4.L._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
hh_ofi_loanLt<-get_data("QSA.Q.N.LU.W2.S1M.S12O.N.A.LE.F4.L._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
hh_icpf_loanLt<-get_data("QSA.Q.N.LU.W2.S1M.S12Q.N.A.LE.F4.L._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
hh_govt_loanLt<-get_data("QSA.Q.N.LU.W2.S1M.S13.N.A.LE.F4.L._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
hh_hh_loanLt<-get_data("QSA.Q.N.LU.W2.S1M.S1M.N.A.LE.F4.L._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))

##shares
hh_nfc_shares<-get_data("QSA.Q.N.LU.W2.S1M.S11.N.A.LE.F511._Z._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
hh_iv_shares<-get_data("QSA.Q.N.LU.W2.S1M.S124.N.A.LE.F511._Z._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
hh_mfi_shares<-get_data("QSA.Q.N.LU.W2.S1M.S12K.N.A.LE.F511._Z._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
hh_ofi_shares<-get_data("QSA.Q.N.LU.W2.S1M.S12O.N.A.LE.F511._Z._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
hh_icpf_shares<-get_data("QSA.Q.N.LU.W2.S1M.S12Q.N.A.LE.F511._Z._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
hh_govt_shares<-get_data("QSA.Q.N.LU.W2.S1M.S13.N.A.LE.F511._Z._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))

##iv_shares
hh_iv_ivShares<-get_data("QSA.Q.N.LU.W2.S1M.S124.N.A.LE.F52._Z._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
hh_mfi_ivShares<-get_data("QSA.Q.N.LU.W2.S1M.S12K.N.A.LE.F52._Z._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))

#rest of the world
nfc_row_debtSt<-get_data("QSA.Q.N.LU.W0.S11.S1.N.A.LE.F3.S._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
row_nfc_debtSt<-get_data("QSA.Q.N.LU.W0.S11.S1.N.L.LE.F3.S._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
iv_row_debtSt<-get_data("QSA.Q.N.LU.W0.S124.S1.N.A.LE.F3.S._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
row_iv_debtSt<-get_data("QSA.Q.N.LU.W0.S124.S1.N.L.LE.F3.S._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
mfi_row_debtSt<-get_data("QSA.Q.N.LU.W0.S12K.S1.N.A.LE.F3.S._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
row_mfi_debtSt<-get_data("QSA.Q.N.LU.W0.S12K.S1.N.L.LE.F3.S._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
ofi_row_debtSt<-get_data("QSA.Q.N.LU.W0.S12O.S1.N.A.LE.F3.S._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
row_ofi_debtSt<-get_data("QSA.Q.N.LU.W0.S12O.S1.N.L.LE.F3.S._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
icpf_row_debtSt<-get_data("QSA.Q.N.LU.W0.S12Q.S1.N.A.LE.F3.S._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
row_icpf_debtSt<-get_data("QSA.Q.N.LU.W0.S12Q.S1.N.L.LE.F3.S._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
govt_row_debtSt<-get_data("QSA.Q.N.LU.W0.S13.S1.N.A.LE.F3.S._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
row_govt_debtSt<-get_data("QSA.Q.N.LU.W0.S13.S1.N.L.LE.F3.S._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
hh_row_debtSt<-get_data("QSA.Q.N.LU.W0.S1M.S1.N.A.LE.F3.S._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))

nfc_row_debtLt<-get_data("QSA.Q.N.LU.W0.S11.S1.N.A.LE.F3.L._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
row_nfc_debtLt<-get_data("QSA.Q.N.LU.W0.S11.S1.N.L.LE.F3.L._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
iv_row_debtLt<-get_data("QSA.Q.N.LU.W0.S124.S1.N.A.LE.F3.L._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
row_iv_debtLt<-get_data("QSA.Q.N.LU.W0.S124.S1.N.L.LE.F3.L._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
mfi_row_debtLt<-get_data("QSA.Q.N.LU.W0.S12K.S1.N.A.LE.F3.L._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
row_mfi_debtLt<-get_data("QSA.Q.N.LU.W0.S12K.S1.N.L.LE.F3.L._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
ofi_row_debtLt<-get_data("QSA.Q.N.LU.W0.S12O.S1.N.A.LE.F3.L._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
row_ofi_debtLt<-get_data("QSA.Q.N.LU.W0.S12O.S1.N.L.LE.F3.L._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
icpf_row_debtLt<-get_data("QSA.Q.N.LU.W0.S12Q.S1.N.A.LE.F3.L._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
row_icpf_debtLt<-get_data("QSA.Q.N.LU.W0.S12Q.S1.N.L.LE.F3.L._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
govt_row_debtLt<-get_data("QSA.Q.N.LU.W0.S13.S1.N.A.LE.F3.L._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
row_govt_debtLt<-get_data("QSA.Q.N.LU.W0.S13.S1.N.L.LE.F3.L._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
hh_row_debtLt<-get_data("QSA.Q.N.LU.W0.S1M.S1.N.A.LE.F3.L._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))

nfc_row_loanSt<-get_data("QSA.Q.N.LU.W0.S11.S1.N.A.LE.F4.S._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
row_nfc_loanSt<-get_data("QSA.Q.N.LU.W0.S11.S1.N.L.LE.F4.S._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
iv_row_loanSt<-get_data("QSA.Q.N.LU.W0.S124.S1.N.A.LE.F4.S._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
row_iv_loanSt<-get_data("QSA.Q.N.LU.W0.S124.S1.N.L.LE.F4.S._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
mfi_row_loanSt<-get_data("QSA.Q.N.LU.W0.S12K.S1.N.A.LE.F4.S._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
row_mfi_loanSt<-get_data("QSA.Q.N.LU.W0.S12K.S1.N.L.LE.F4.S._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
ofi_row_loanSt<-get_data("QSA.Q.N.LU.W0.S12O.S1.N.A.LE.F4.S._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
row_ofi_loanSt<-get_data("QSA.Q.N.LU.W0.S12O.S1.N.L.LE.F4.S._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
icpf_row_loanSt<-get_data("QSA.Q.N.LU.W0.S12Q.S1.N.A.LE.F4.S._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
row_icpf_loanSt<-get_data("QSA.Q.N.LU.W0.S12Q.S1.N.L.LE.F4.S._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
govt_row_loanSt<-get_data("QSA.Q.N.LU.W0.S13.S1.N.A.LE.F4.S._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
row_govt_loanSt<-get_data("QSA.Q.N.LU.W0.S13.S1.N.L.LE.F4.S._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
hh_row_loanSt<-get_data("QSA.Q.N.LU.W0.S1M.S1.N.A.LE.F4.S._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))

nfc_row_loanLt<-get_data("QSA.Q.N.LU.W0.S11.S1.N.A.LE.F4.L._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
row_nfc_loanLt<-get_data("QSA.Q.N.LU.W0.S11.S1.N.L.LE.F4.L._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
iv_row_loanLt<-get_data("QSA.Q.N.LU.W0.S124.S1.N.A.LE.F4.L._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
row_iv_loanLt<-get_data("QSA.Q.N.LU.W0.S124.S1.N.L.LE.F4.L._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
mfi_row_loanLt<-get_data("QSA.Q.N.LU.W0.S12K.S1.N.A.LE.F4.L._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
row_mfi_loanLt<-get_data("QSA.Q.N.LU.W0.S12K.S1.N.L.LE.F4.L._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
ofi_row_loanLt<-get_data("QSA.Q.N.LU.W0.S12O.S1.N.A.LE.F4.L._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
row_ofi_loanLt<-get_data("QSA.Q.N.LU.W0.S12O.S1.N.L.LE.F4.L._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
icpf_row_loanLt<-get_data("QSA.Q.N.LU.W0.S12Q.S1.N.A.LE.F4.L._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
row_icpf_loanLt<-get_data("QSA.Q.N.LU.W0.S12Q.S1.N.L.LE.F4.L._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
govt_row_loanLt<-get_data("QSA.Q.N.LU.W0.S13.S1.N.A.LE.F4.L._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
row_govt_loanLt<-get_data("QSA.Q.N.LU.W0.S13.S1.N.L.LE.F4.L._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
hh_row_loanLt<-get_data("QSA.Q.N.LU.W0.S1M.S1.N.A.LE.F4.L._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))

nfc_row_shares<-get_data("QSA.Q.N.LU.W0.S11.S1.N.A.LE.F511._Z._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
row_nfc_shares<-get_data("QSA.Q.N.LU.W0.S11.S1.N.L.LE.F511._Z._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
iv_row_shares<-get_data("QSA.Q.N.LU.W0.S124.S1.N.A.LE.F511._Z._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
row_iv_shares<-get_data("QSA.Q.N.LU.W0.S124.S1.N.L.LE.F511._Z._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
mfi_row_shares<-get_data("QSA.Q.N.LU.W0.S12K.S1.N.A.LE.F511._Z._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
row_mfi_shares<-get_data("QSA.Q.N.LU.W0.S12K.S1.N.L.LE.F511._Z._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
ofi_row_shares<-get_data("QSA.Q.N.LU.W0.S12O.S1.N.A.LE.F511._Z._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
row_ofi_shares<-get_data("QSA.Q.N.LU.W0.S12O.S1.N.L.LE.F511._Z._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
icpf_row_shares<-get_data("QSA.Q.N.LU.W0.S12Q.S1.N.A.LE.F511._Z._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
row_icpf_shares<-get_data("QSA.Q.N.LU.W0.S12Q.S1.N.L.LE.F511._Z._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
govt_row_shares<-get_data("QSA.Q.N.LU.W0.S13.S1.N.A.LE.F511._Z._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
row_govt_shares<-get_data("QSA.Q.N.LU.W0.S13.S1.N.L.LE.F511._Z._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
hh_row_shares<-get_data("QSA.Q.N.LU.W0.S1M.S1.N.A.LE.F511._Z._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))

nfc_row_ivShares<-get_data("QSA.Q.N.LU.W0.S11.S1.N.A.LE.F52._Z._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
iv_row_ivShares<-get_data("QSA.Q.N.LU.W0.S124.S1.N.A.LE.F52._Z._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
row_iv_ivShares<-get_data("QSA.Q.N.LU.W0.S124.S1.N.L.LE.F52._Z._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
mfi_row_ivShares<-get_data("QSA.Q.N.LU.W0.S12K.S1.N.A.LE.F52._Z._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
row_mfi_ivShares<-get_data("QSA.Q.N.LU.W0.S12K.S1.N.L.LE.F52._Z._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
ofi_row_ivShares<-get_data("QSA.Q.N.LU.W0.S12O.S1.N.A.LE.F52._Z._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
icpf_row_ivShares<-get_data("QSA.Q.N.LU.W0.S12Q.S1.N.A.LE.F52._Z._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
govt_row_ivShares<-get_data("QSA.Q.N.LU.W0.S13.S1.N.A.LE.F52._Z._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
hh_row_ivShares<-get_data("QSA.Q.N.LU.W0.S1M.S1.N.A.LE.F52._Z._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))

#stDebt
nfc_row_debtSt$obsvalue<-nfc_row_debtSt$obsvalue-nfc_govt_debtSt$obsvalue-nfc_hh_debtSt$obsvalue-nfc_iv_debtSt$obsvalue-nfc_ofi_debtSt$obsvalue-nfc_icpf_debtSt$obsvalue-nfc_mfi_debtSt$obsvalue-nfc_nfc_debtSt$obsvalue
row_nfc_debtSt$obsvalue<-row_nfc_debtSt$obsvalue-govt_nfc_debtSt$obsvalue-hh_govt_debtSt$obsvalue-iv_nfc_debtSt$obsvalue-ofi_nfc_debtSt$obsvalue-icpf_nfc_debtSt$obsvalue-mfi_nfc_debtSt$obsvalue-nfc_nfc_debtSt$obsvalue

iv_row_debtSt$obsvalue<-iv_row_debtSt$obsvalue-iv_govt_debtSt$obsvalue-iv_hh_debtSt$obsvalue-iv_iv_debtSt$obsvalue-iv_ofi_debtSt$obsvalue-iv_icpf_debtSt$obsvalue-iv_mfi_debtSt$obsvalue-iv_nfc_debtSt$obsvalue
row_iv_debtSt$obsvalue<-row_iv_debtSt$obsvalue-govt_iv_debtSt$obsvalue-hh_govt_debtSt$obsvalue-iv_iv_debtSt$obsvalue-ofi_iv_debtSt$obsvalue-icpf_iv_debtSt$obsvalue-mfi_iv_debtSt$obsvalue-nfc_iv_debtSt$obsvalue

ofi_row_debtSt$obsvalue<-ofi_row_debtSt$obsvalue-ofi_govt_debtSt$obsvalue-ofi_hh_debtSt$obsvalue-ofi_iv_debtSt$obsvalue-ofi_ofi_debtSt$obsvalue-ofi_icpf_debtSt$obsvalue-ofi_mfi_debtSt$obsvalue-ofi_nfc_debtSt$obsvalue
row_ofi_debtSt$obsvalue<-row_ofi_debtSt$obsvalue-govt_ofi_debtSt$obsvalue-hh_ofi_debtSt$obsvalue-iv_ofi_debtSt$obsvalue-ofi_ofi_debtSt$obsvalue-icpf_ofi_debtSt$obsvalue-mfi_ofi_debtSt$obsvalue-nfc_ofi_debtSt$obsvalue

icpf_row_debtSt$obsvalue<-icpf_row_debtSt$obsvalue-icpf_govt_debtSt$obsvalue-icpf_hh_debtSt$obsvalue-icpf_iv_debtSt$obsvalue-icpf_icpf_debtSt$obsvalue-icpf_icpf_debtSt$obsvalue-icpf_mfi_debtSt$obsvalue-icpf_nfc_debtSt$obsvalue
row_icpf_debtSt$obsvalue<-row_icpf_debtSt$obsvalue-govt_icpf_debtSt$obsvalue-hh_icpf_debtSt$obsvalue-iv_icpf_debtSt$obsvalue-icpf_icpf_debtSt$obsvalue-icpf_icpf_debtSt$obsvalue-mfi_icpf_debtSt$obsvalue-nfc_icpf_debtSt$obsvalue

mfi_row_debtSt$obsvalue<-mfi_row_debtSt$obsvalue-mfi_govt_debtSt$obsvalue-mfi_hh_debtSt$obsvalue-mfi_iv_debtSt$obsvalue-mfi_mfi_debtSt$obsvalue-mfi_icpf_debtSt$obsvalue-mfi_ofi_debtSt$obsvalue-mfi_nfc_debtSt$obsvalue
row_mfi_debtSt$obsvalue<-row_mfi_debtSt$obsvalue-govt_mfi_debtSt$obsvalue-hh_mfi_debtSt$obsvalue-iv_mfi_debtSt$obsvalue-mfi_mfi_debtSt$obsvalue-icpf_mfi_debtSt$obsvalue-ofi_mfi_debtSt$obsvalue-nfc_mfi_debtSt$obsvalue

govt_row_debtSt$obsvalue<-govt_row_debtSt$obsvalue-govt_govt_debtSt$obsvalue-govt_hh_debtSt$obsvalue-govt_iv_debtSt$obsvalue-govt_ofi_debtSt$obsvalue-govt_icpf_debtSt$obsvalue-govt_mfi_debtSt$obsvalue-govt_nfc_debtSt$obsvalue
row_govt_debtSt$obsvalue<-row_govt_debtSt$obsvalue-govt_govt_debtSt$obsvalue-hh_govt_debtSt$obsvalue-iv_govt_debtSt$obsvalue-ofi_govt_debtSt$obsvalue-icpf_govt_debtSt$obsvalue-mfi_govt_debtSt$obsvalue-nfc_govt_debtSt$obsvalue

hh_row_debtSt$obsvalue<-hh_row_debtSt$obsvalue-hh_hh_debtSt$obsvalue-hh_govt_debtSt$obsvalue-hh_iv_debtSt$obsvalue-hh_ofi_debtSt$obsvalue-hh_icpf_debtSt$obsvalue-hh_mfi_debtSt$obsvalue-hh_nfc_debtSt$obsvalue

#ltDebt
nfc_row_debtLt$obsvalue<-nfc_row_debtLt$obsvalue-nfc_govt_debtLt$obsvalue-nfc_hh_debtLt$obsvalue-nfc_iv_debtLt$obsvalue-nfc_ofi_debtLt$obsvalue-nfc_icpf_debtLt$obsvalue-nfc_mfi_debtLt$obsvalue-nfc_nfc_debtLt$obsvalue
row_nfc_debtLt$obsvalue<-row_nfc_debtLt$obsvalue-govt_nfc_debtLt$obsvalue-hh_nfc_debtLt$obsvalue-iv_nfc_debtLt$obsvalue-ofi_nfc_debtLt$obsvalue-icpf_nfc_debtLt$obsvalue-mfi_nfc_debtLt$obsvalue-nfc_nfc_debtLt$obsvalue

iv_row_debtLt$obsvalue<-iv_row_debtLt$obsvalue-iv_govt_debtLt$obsvalue-iv_hh_debtLt$obsvalue-iv_iv_debtLt$obsvalue-iv_ofi_debtLt$obsvalue-iv_icpf_debtLt$obsvalue-iv_mfi_debtLt$obsvalue-iv_nfc_debtLt$obsvalue
row_iv_debtLt$obsvalue<-row_iv_debtLt$obsvalue-govt_iv_debtLt$obsvalue-hh_iv_debtLt$obsvalue-iv_iv_debtLt$obsvalue-ofi_iv_debtLt$obsvalue-icpf_iv_debtLt$obsvalue-mfi_iv_debtLt$obsvalue-nfc_iv_debtLt$obsvalue

ofi_row_debtLt$obsvalue<-ofi_row_debtLt$obsvalue-ofi_govt_debtLt$obsvalue-ofi_hh_debtLt$obsvalue-ofi_iv_debtLt$obsvalue-ofi_ofi_debtLt$obsvalue-ofi_icpf_debtLt$obsvalue-ofi_mfi_debtLt$obsvalue-ofi_nfc_debtLt$obsvalue
row_ofi_debtLt$obsvalue<-row_ofi_debtLt$obsvalue-govt_ofi_debtLt$obsvalue-hh_ofi_debtLt$obsvalue-iv_ofi_debtLt$obsvalue-ofi_ofi_debtLt$obsvalue-icpf_ofi_debtLt$obsvalue-mfi_ofi_debtLt$obsvalue-nfc_ofi_debtLt$obsvalue

icpf_row_debtLt$obsvalue<-icpf_row_debtLt$obsvalue-icpf_govt_debtLt$obsvalue-icpf_hh_debtLt$obsvalue-icpf_iv_debtLt$obsvalue-icpf_icpf_debtLt$obsvalue-icpf_icpf_debtLt$obsvalue-icpf_mfi_debtLt$obsvalue-icpf_nfc_debtLt$obsvalue
row_icpf_debtLt$obsvalue<-row_icpf_debtLt$obsvalue-govt_icpf_debtLt$obsvalue-hh_icpf_debtLt$obsvalue-iv_icpf_debtLt$obsvalue-ofi_icpf_debtLt$obsvalue-icpf_icpf_debtLt$obsvalue-mfi_icpf_debtLt$obsvalue-nfc_icpf_debtLt$obsvalue

mfi_row_debtLt$obsvalue<-mfi_row_debtLt$obsvalue-mfi_govt_debtLt$obsvalue-mfi_hh_debtLt$obsvalue-mfi_iv_debtLt$obsvalue-mfi_mfi_debtLt$obsvalue-mfi_icpf_debtLt$obsvalue-mfi_ofi_debtLt$obsvalue-mfi_nfc_debtLt$obsvalue
row_mfi_debtLt$obsvalue<-row_mfi_debtLt$obsvalue-govt_mfi_debtLt$obsvalue-hh_mfi_debtLt$obsvalue-iv_mfi_debtLt$obsvalue-mfi_mfi_debtLt$obsvalue-icpf_mfi_debtLt$obsvalue-ofi_mfi_debtLt$obsvalue-nfc_mfi_debtLt$obsvalue

govt_row_debtLt$obsvalue<-govt_row_debtLt$obsvalue-govt_govt_debtLt$obsvalue-govt_hh_debtLt$obsvalue-govt_iv_debtLt$obsvalue-govt_ofi_debtLt$obsvalue-govt_icpf_debtLt$obsvalue-govt_mfi_debtLt$obsvalue-govt_nfc_debtLt$obsvalue
row_govt_debtLt$obsvalue<-row_govt_debtLt$obsvalue-govt_govt_debtLt$obsvalue-hh_govt_debtLt$obsvalue-iv_govt_debtLt$obsvalue-ofi_govt_debtLt$obsvalue-icpf_govt_debtLt$obsvalue-mfi_govt_debtLt$obsvalue-nfc_govt_debtLt$obsvalue

hh_row_debtLt$obsvalue<-hh_row_debtLt$obsvalue-hh_hh_debtLt$obsvalue-hh_govt_debtLt$obsvalue-hh_iv_debtLt$obsvalue-hh_ofi_debtLt$obsvalue-hh_icpf_debtLt$obsvalue-hh_mfi_debtLt$obsvalue-hh_nfc_debtLt$obsvalue

#loanSt
nfc_row_loanSt$obsvalue<-nfc_row_loanSt$obsvalue-nfc_govt_loanSt$obsvalue-nfc_hh_loanSt$obsvalue-nfc_iv_loanSt$obsvalue-nfc_ofi_loanSt$obsvalue-nfc_icpf_loanSt$obsvalue-nfc_nfc_loanSt$obsvalue
row_nfc_loanSt$obsvalue<-row_nfc_loanSt$obsvalue-govt_nfc_loanSt$obsvalue-hh_nfc_loanSt$obsvalue-iv_nfc_loanSt$obsvalue-ofi_nfc_loanSt$obsvalue-icpf_nfc_loanSt$obsvalue-mfi_nfc_loanSt$obsvalue-nfc_nfc_loanSt$obsvalue

iv_row_loanSt$obsvalue<-iv_row_loanSt$obsvalue-iv_govt_loanSt$obsvalue-iv_hh_loanSt$obsvalue-iv_iv_loanSt$obsvalue-iv_ofi_loanSt$obsvalue-iv_icpf_loanSt$obsvalue-iv_nfc_loanSt$obsvalue
row_iv_loanSt$obsvalue<-row_iv_loanSt$obsvalue-govt_iv_loanSt$obsvalue-hh_iv_loanSt$obsvalue-iv_iv_loanSt$obsvalue-ofi_iv_loanSt$obsvalue-icpf_iv_loanSt$obsvalue-mfi_iv_loanSt$obsvalue-nfc_iv_loanSt$obsvalue

ofi_row_loanSt$obsvalue<-ofi_row_loanSt$obsvalue-ofi_govt_loanSt$obsvalue-ofi_hh_loanSt$obsvalue-ofi_iv_loanSt$obsvalue-ofi_ofi_loanSt$obsvalue-ofi_icpf_loanSt$obsvalue-ofi_nfc_loanSt$obsvalue
row_ofi_loanSt$obsvalue<-row_ofi_loanSt$obsvalue-govt_ofi_loanSt$obsvalue-hh_ofi_loanSt$obsvalue-iv_ofi_loanSt$obsvalue-ofi_ofi_loanSt$obsvalue-icpf_ofi_loanSt$obsvalue-mfi_ofi_loanSt$obsvalue-nfc_ofi_loanSt$obsvalue

icpf_row_loanSt$obsvalue<-icpf_row_loanSt$obsvalue-icpf_govt_loanSt$obsvalue-icpf_hh_loanSt$obsvalue-icpf_iv_loanSt$obsvalue-icpf_icpf_loanSt$obsvalue-icpf_icpf_loanSt$obsvalue-icpf_nfc_loanSt$obsvalue
row_icpf_loanSt$obsvalue<-row_icpf_loanSt$obsvalue-govt_icpf_loanSt$obsvalue-hh_icpf_loanSt$obsvalue-iv_icpf_loanSt$obsvalue-icpf_icpf_loanSt$obsvalue-ofi_icpf_loanSt$obsvalue-mfi_icpf_loanSt$obsvalue-nfc_icpf_loanSt$obsvalue

mfi_row_loanSt$obsvalue<-mfi_row_loanSt$obsvalue-mfi_govt_loanSt$obsvalue-mfi_hh_loanSt$obsvalue-mfi_iv_loanSt$obsvalue-mfi_icpf_loanSt$obsvalue-mfi_ofi_loanSt$obsvalue-mfi_nfc_loanSt$obsvalue
row_mfi_loanSt$obsvalue<-row_mfi_loanSt$obsvalue-hh_mfi_loanSt$obsvalue-iv_mfi_loanSt$obsvalue-mfi_mfi_loanSt$obsvalue-icpf_mfi_loanSt$obsvalue-ofi_mfi_loanSt$obsvalue-nfc_mfi_loanSt$obsvalue

govt_row_loanSt$obsvalue<-govt_row_loanSt$obsvalue-govt_govt_loanSt$obsvalue-govt_hh_loanSt$obsvalue-govt_iv_loanSt$obsvalue-govt_ofi_loanSt$obsvalue-govt_icpf_loanSt$obsvalue-govt_nfc_loanSt$obsvalue
row_govt_loanSt$obsvalue<-row_govt_loanSt$obsvalue-govt_govt_loanSt$obsvalue-hh_govt_loanSt$obsvalue-iv_govt_loanSt$obsvalue-ofi_govt_loanSt$obsvalue-icpf_govt_loanSt$obsvalue-mfi_govt_loanSt$obsvalue-nfc_govt_loanSt$obsvalue

hh_row_loanSt$obsvalue<-hh_row_loanSt$obsvalue-hh_hh_loanSt$obsvalue-hh_govt_loanSt$obsvalue-hh_iv_loanSt$obsvalue-hh_ofi_loanSt$obsvalue-hh_icpf_loanSt$obsvalue-hh_nfc_loanSt$obsvalue

#loanLt
nfc_row_loanLt$obsvalue<-nfc_row_loanLt$obsvalue-nfc_govt_loanLt$obsvalue-nfc_hh_loanLt$obsvalue-nfc_iv_loanLt$obsvalue-nfc_ofi_loanLt$obsvalue-nfc_icpf_loanLt$obsvalue-nfc_nfc_loanLt$obsvalue
row_nfc_loanLt$obsvalue<-row_nfc_loanLt$obsvalue-govt_nfc_loanLt$obsvalue-hh_nfc_loanLt$obsvalue-iv_nfc_loanLt$obsvalue-ofi_nfc_loanLt$obsvalue-icpf_nfc_loanLt$obsvalue-mfi_nfc_loanLt$obsvalue-nfc_nfc_loanLt$obsvalue

iv_row_loanLt$obsvalue<-iv_row_loanLt$obsvalue-iv_govt_loanLt$obsvalue-iv_hh_loanLt$obsvalue-iv_iv_loanLt$obsvalue-iv_ofi_loanLt$obsvalue-iv_icpf_loanLt$obsvalue-iv_nfc_loanLt$obsvalue
row_iv_loanLt$obsvalue<-row_iv_loanLt$obsvalue-govt_iv_loanLt$obsvalue-hh_iv_loanLt$obsvalue-iv_iv_loanLt$obsvalue-ofi_iv_loanLt$obsvalue-icpf_iv_loanLt$obsvalue-mfi_iv_loanLt$obsvalue-nfc_iv_loanLt$obsvalue

ofi_row_loanLt$obsvalue<-ofi_row_loanLt$obsvalue-ofi_govt_loanLt$obsvalue-ofi_hh_loanLt$obsvalue-ofi_iv_loanLt$obsvalue-ofi_ofi_loanLt$obsvalue-ofi_icpf_loanLt$obsvalue-ofi_nfc_loanLt$obsvalue
row_ofi_loanLt$obsvalue<-row_ofi_loanLt$obsvalue-govt_ofi_loanLt$obsvalue-hh_ofi_loanLt$obsvalue-iv_ofi_loanLt$obsvalue-ofi_ofi_loanLt$obsvalue-icpf_ofi_loanLt$obsvalue-mfi_ofi_loanLt$obsvalue-nfc_ofi_loanLt$obsvalue

icpf_row_loanLt$obsvalue<-icpf_row_loanLt$obsvalue-icpf_govt_loanLt$obsvalue-icpf_hh_loanLt$obsvalue-icpf_iv_loanLt$obsvalue-icpf_icpf_loanLt$obsvalue-icpf_icpf_loanLt$obsvalue-icpf_nfc_loanLt$obsvalue
row_icpf_loanLt$obsvalue<-row_icpf_loanLt$obsvalue-govt_icpf_loanLt$obsvalue-hh_icpf_loanLt$obsvalue-iv_icpf_loanLt$obsvalue-ofi_icpf_loanLt$obsvalue-icpf_icpf_loanLt$obsvalue-mfi_icpf_loanLt$obsvalue-nfc_icpf_loanLt$obsvalue

mfi_row_loanLt$obsvalue<-mfi_row_loanLt$obsvalue-mfi_govt_loanLt$obsvalue-mfi_hh_loanLt$obsvalue-mfi_iv_loanLt$obsvalue-mfi_icpf_loanLt$obsvalue-mfi_ofi_loanLt$obsvalue-mfi_nfc_loanLt$obsvalue
row_mfi_loanLt$obsvalue<-row_mfi_loanLt$obsvalue-govt_mfi_loanLt$obsvalue-hh_mfi_loanLt$obsvalue-iv_mfi_loanLt$obsvalue-mfi_mfi_loanLt$obsvalue-icpf_mfi_loanLt$obsvalue-ofi_mfi_loanLt$obsvalue-nfc_mfi_loanLt$obsvalue

govt_row_loanLt$obsvalue<-govt_row_loanLt$obsvalue-govt_govt_loanLt$obsvalue-govt_hh_loanLt$obsvalue-govt_iv_loanLt$obsvalue-govt_ofi_loanLt$obsvalue-govt_icpf_loanLt$obsvalue-govt_nfc_loanLt$obsvalue
row_govt_loanLt$obsvalue<-row_govt_loanLt$obsvalue-govt_govt_loanLt$obsvalue-hh_govt_loanLt$obsvalue-iv_govt_loanLt$obsvalue-ofi_govt_loanLt$obsvalue-icpf_govt_loanLt$obsvalue-mfi_govt_loanLt$obsvalue-nfc_govt_loanLt$obsvalue

hh_row_loanLt$obsvalue<-hh_row_loanLt$obsvalue-hh_hh_loanLt$obsvalue-hh_govt_loanLt$obsvalue-hh_iv_loanLt$obsvalue-hh_ofi_loanLt$obsvalue-hh_icpf_loanLt$obsvalue-hh_nfc_loanLt$obsvalue

#shares
nfc_row_shares$obsvalue<-nfc_row_shares$obsvalue-nfc_govt_shares$obsvalue-nfc_iv_shares$obsvalue-nfc_ofi_shares$obsvalue-nfc_icpf_shares$obsvalue-nfc_mfi_shares$obsvalue-nfc_nfc_shares$obsvalue
row_nfc_shares$obsvalue<-row_nfc_shares$obsvalue-govt_nfc_shares$obsvalue-hh_nfc_shares$obsvalue-iv_nfc_shares$obsvalue-ofi_nfc_shares$obsvalue-icpf_nfc_shares$obsvalue-mfi_nfc_shares$obsvalue-nfc_nfc_shares$obsvalue

iv_row_shares$obsvalue<-iv_row_shares$obsvalue-iv_govt_shares$obsvalue-iv_iv_shares$obsvalue-iv_ofi_shares$obsvalue-iv_icpf_shares$obsvalue-iv_mfi_shares$obsvalue-iv_nfc_shares$obsvalue
row_iv_shares$obsvalue<-row_iv_shares$obsvalue-govt_iv_shares$obsvalue-hh_iv_shares$obsvalue-iv_iv_shares$obsvalue-ofi_iv_shares$obsvalue-icpf_iv_shares$obsvalue-mfi_iv_shares$obsvalue-nfc_iv_shares$obsvalue

ofi_row_shares$obsvalue<-ofi_row_shares$obsvalue-ofi_govt_shares$obsvalue-ofi_iv_shares$obsvalue-ofi_ofi_shares$obsvalue-ofi_icpf_shares$obsvalue-ofi_mfi_shares$obsvalue-ofi_nfc_shares$obsvalue
row_ofi_shares$obsvalue<-row_ofi_shares$obsvalue-govt_ofi_shares$obsvalue-hh_ofi_shares$obsvalue-iv_ofi_shares$obsvalue-ofi_ofi_shares$obsvalue-icpf_ofi_shares$obsvalue-mfi_ofi_shares$obsvalue-nfc_ofi_shares$obsvalue

icpf_row_shares$obsvalue<-icpf_row_shares$obsvalue-icpf_govt_shares$obsvalue-icpf_iv_shares$obsvalue-icpf_icpf_shares$obsvalue-icpf_icpf_shares$obsvalue-icpf_mfi_shares$obsvalue-icpf_nfc_shares$obsvalue
row_icpf_shares$obsvalue<-row_icpf_shares$obsvalue-govt_icpf_shares$obsvalue-hh_icpf_shares$obsvalue-iv_icpf_shares$obsvalue-ofi_icpf_shares$obsvalue-icpf_icpf_shares$obsvalue-mfi_icpf_shares$obsvalue-nfc_icpf_shares$obsvalue

mfi_row_shares$obsvalue<-mfi_row_shares$obsvalue-mfi_govt_shares$obsvalue-mfi_iv_shares$obsvalue-mfi_mfi_shares$obsvalue-mfi_icpf_shares$obsvalue-mfi_ofi_shares$obsvalue-mfi_nfc_shares$obsvalue
row_mfi_shares$obsvalue<-row_mfi_shares$obsvalue-govt_mfi_shares$obsvalue-hh_mfi_shares$obsvalue-iv_mfi_shares$obsvalue-mfi_mfi_shares$obsvalue-icpf_mfi_shares$obsvalue-ofi_mfi_shares$obsvalue-nfc_mfi_shares$obsvalue

govt_row_shares$obsvalue<-govt_row_shares$obsvalue-govt_govt_shares$obsvalue-govt_iv_shares$obsvalue-govt_ofi_shares$obsvalue-govt_icpf_shares$obsvalue-govt_mfi_shares$obsvalue-govt_nfc_shares$obsvalue
row_govt_shares$obsvalue<-row_govt_shares$obsvalue-govt_govt_shares$obsvalue-hh_govt_shares$obsvalue-iv_govt_shares$obsvalue-ofi_govt_shares$obsvalue-icpf_govt_shares$obsvalue-mfi_govt_shares$obsvalue-nfc_govt_shares$obsvalue

hh_row_shares$obsvalue<-hh_row_shares$obsvalue-hh_govt_shares$obsvalue-hh_iv_shares$obsvalue-hh_ofi_shares$obsvalue-hh_icpf_shares$obsvalue-hh_mfi_shares$obsvalue-hh_nfc_shares$obsvalue

#ivShares
nfc_row_ivShares$obsvalue<-nfc_row_ivShares$obsvalue-nfc_iv_ivShares$obsvalue-nfc_mfi_ivShares$obsvalue

iv_row_ivShares$obsvalue<-iv_row_ivShares$obsvalue-iv_iv_ivShares$obsvalue-iv_mfi_ivShares$obsvalue
row_iv_ivShares$obsvalue<-row_iv_ivShares$obsvalue-govt_iv_ivShares$obsvalue-iv_iv_ivShares$obsvalue-ofi_iv_ivShares$obsvalue-icpf_iv_ivShares$obsvalue-mfi_iv_ivShares$obsvalue-nfc_iv_ivShares$obsvalue-hh_iv_ivShares$obsvalue

ofi_row_ivShares$obsvalue<-ofi_row_ivShares$obsvalue-ofi_iv_ivShares$obsvalue-ofi_mfi_ivShares$obsvalue

icpf_row_ivShares$obsvalue<-icpf_row_ivShares$obsvalue-icpf_iv_ivShares$obsvalue-icpf_mfi_ivShares$obsvalue

mfi_row_ivShares$obsvalue<-mfi_row_ivShares$obsvalue-mfi_iv_ivShares$obsvalue-mfi_mfi_ivShares$obsvalue
row_mfi_ivShares$obsvalue<-row_mfi_ivShares$obsvalue-govt_mfi_ivShares$obsvalue-iv_mfi_ivShares$obsvalue-mfi_mfi_ivShares$obsvalue-icpf_mfi_ivShares$obsvalue-ofi_mfi_ivShares$obsvalue-nfc_mfi_ivShares$obsvalue-hh_mfi_ivShares$obsvalue

govt_row_ivShares$obsvalue<-govt_row_ivShares$obsvalue-govt_iv_ivShares$obsvalue-govt_mfi_ivShares$obsvalue

hh_row_ivShares$obsvalue<-hh_row_ivShares$obsvalue-hh_iv_ivShares$obsvalue-hh_mfi_ivShares$obsvalue

#0 values
row_hh_debtSt<-hh_row_debtSt
row_hh_debtSt$obsvalue<-0
row_hh_debtLt<-hh_row_debtSt
row_hh_debtLt$obsvalue<-0
row_hh_loanSt<-hh_row_debtSt
row_hh_loanSt$obsvalue<-0
row_hh_loanLt<-hh_row_debtSt
row_hh_loanLt$obsvalue<-0
row_hh_shares<-hh_row_debtSt
row_hh_shares$obsvalue<-0
row_hh_ivShares<-hh_row_debtSt
row_hh_ivShares$obsvalue<-0
row_row_debtSt<-hh_row_debtSt
row_row_debtSt$obsvalue<-0
row_row_debtLt<-hh_row_debtSt
row_row_debtLt$obsvalue<-0
row_row_loanSt<-hh_row_debtSt
row_row_loanSt$obsvalue<-0
row_row_loanLt<-hh_row_debtSt
row_row_loanLt$obsvalue<-0
row_row_shares<-hh_row_debtSt
row_row_shares$obsvalue<-0
row_row_ivShares<-hh_row_debtSt
row_row_ivShares$obsvalue<-0

##balance sheets
iv_assets<-get_data("QSA.Q.N.LU.W0.S124.S1.N.A.LE.F._Z._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
iv_liabilities<-get_data("QSA.Q.N.LU.W0.S124.S1.N.L.LE.F._Z._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
nfc_assets<-get_data("QSA.Q.N.LU.W0.S11.S1.N.A.LE.F._Z._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
nfc_liabilities<-get_data("QSA.Q.N.LU.W0.S11.S1.N.L.LE.F._Z._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
mfi_assets<-get_data("QSA.Q.N.LU.W0.S12K.S1.N.A.LE.F._Z._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
mfi_liabilities<-get_data("QSA.Q.N.LU.W0.S12K.S1.N.L.LE.F._Z._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
ofi_assets<-get_data("QSA.Q.N.LU.W0.S12O.S1.N.A.LE.F._Z._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
ofi_liabilities<-get_data("QSA.Q.N.LU.W0.S12O.S1.N.L.LE.F._Z._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
icpf_assets<-get_data("QSA.Q.N.LU.W0.S12Q.S1.N.A.LE.F._Z._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
icpf_liabilities<-get_data("QSA.Q.N.LU.W0.S12Q.S1.N.L.LE.F._Z._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
hh_assets<-get_data("QSA.Q.N.LU.W0.S1M.S1.N.A.LE.F._Z._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
hh_liabilities<-get_data("QSA.Q.N.LU.W0.S1M.S1.N.L.LE.F._Z._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
govt_assets<-get_data("QSA.Q.N.LU.W0.S13.S1.N.A.LE.F._Z._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
govt_liabilities<-get_data("QSA.Q.N.LU.W0.S13.S1.N.L.LE.F._Z._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
row_assets<-govt_assets
row_assets$obsvalue<-5000000
row_liabilities<-govt_liabilities
row_liabilities$obsvalue<-5000000

matrixData<-rbind(toFrame(iv_nfc_debtSt),toFrame(iv_iv_debtSt),toFrame(iv_mfi_debtSt),toFrame(iv_ofi_debtSt),toFrame(iv_icpf_debtSt),toFrame(iv_govt_debtSt),toFrame(iv_hh_debtSt),
                  toFrame(iv_nfc_debtLt),toFrame(iv_iv_debtLt),toFrame(iv_mfi_debtLt),toFrame(iv_ofi_debtLt),toFrame(iv_icpf_debtLt),toFrame(iv_govt_debtLt),toFrame(iv_hh_debtLt),
                  toFrame(iv_nfc_loanSt),toFrame(iv_iv_loanSt),toFrame(iv_ofi_loanSt),toFrame(iv_icpf_loanSt),toFrame(iv_govt_loanSt),toFrame(iv_hh_loanSt),
                  toFrame(iv_nfc_loanLt),toFrame(iv_iv_loanLt),toFrame(iv_ofi_loanLt),toFrame(iv_icpf_loanLt),toFrame(iv_govt_loanLt),toFrame(iv_hh_loanLt),
                  toFrame(iv_nfc_shares),toFrame(iv_iv_shares),toFrame(iv_mfi_shares),toFrame(iv_ofi_shares),toFrame(iv_icpf_shares),toFrame(iv_govt_shares),
                  toFrame(iv_iv_ivShares),toFrame(iv_mfi_ivShares),
                  toFrame(ofi_nfc_debtSt),toFrame(ofi_iv_debtSt),toFrame(ofi_mfi_debtSt),toFrame(ofi_ofi_debtSt),toFrame(ofi_icpf_debtSt),toFrame(ofi_govt_debtSt),toFrame(ofi_hh_debtSt),
                  toFrame(ofi_nfc_debtLt),toFrame(ofi_iv_debtLt),toFrame(ofi_mfi_debtLt),toFrame(ofi_ofi_debtLt),toFrame(ofi_icpf_debtLt),toFrame(ofi_govt_debtLt),toFrame(ofi_hh_debtLt),
                  toFrame(ofi_nfc_loanSt),toFrame(ofi_iv_loanSt),toFrame(ofi_ofi_loanSt),toFrame(ofi_icpf_loanSt),toFrame(ofi_govt_loanSt),toFrame(ofi_hh_loanSt),
                  toFrame(ofi_nfc_loanLt),toFrame(ofi_iv_loanLt),toFrame(ofi_ofi_loanLt),toFrame(ofi_icpf_loanLt),toFrame(ofi_govt_loanLt),toFrame(ofi_hh_loanLt),
                  toFrame(ofi_nfc_shares),toFrame(ofi_iv_shares),toFrame(ofi_mfi_shares),toFrame(ofi_ofi_shares),toFrame(ofi_icpf_shares),toFrame(ofi_govt_shares),
                  toFrame(ofi_iv_ivShares),toFrame(ofi_mfi_ivShares),
                  toFrame(nfc_nfc_debtSt),toFrame(nfc_iv_debtSt),toFrame(nfc_mfi_debtSt),toFrame(nfc_ofi_debtSt),toFrame(nfc_icpf_debtSt),toFrame(nfc_govt_debtSt),toFrame(nfc_hh_debtSt),
                  toFrame(nfc_nfc_debtLt),toFrame(nfc_iv_debtLt),toFrame(nfc_mfi_debtLt),toFrame(nfc_ofi_debtLt),toFrame(nfc_icpf_debtLt),toFrame(nfc_govt_debtLt),toFrame(nfc_hh_debtLt),
                  toFrame(nfc_nfc_loanSt),toFrame(nfc_iv_loanSt),toFrame(nfc_ofi_loanSt),toFrame(nfc_icpf_loanSt),toFrame(nfc_govt_loanSt),toFrame(nfc_hh_loanSt),
                  toFrame(nfc_nfc_loanLt),toFrame(nfc_iv_loanLt),toFrame(nfc_ofi_loanLt),toFrame(nfc_icpf_loanLt),toFrame(nfc_govt_loanLt),toFrame(nfc_hh_loanLt),
                  toFrame(nfc_nfc_shares),toFrame(nfc_iv_shares),toFrame(nfc_mfi_shares),toFrame(nfc_ofi_shares),toFrame(nfc_icpf_shares),toFrame(nfc_govt_shares),
                  toFrame(nfc_iv_ivShares),toFrame(nfc_mfi_ivShares),
                  toFrame(mfi_nfc_debtSt),toFrame(mfi_iv_debtSt),toFrame(mfi_mfi_debtSt),toFrame(mfi_ofi_debtSt),toFrame(mfi_icpf_debtSt),toFrame(mfi_govt_debtSt),toFrame(mfi_hh_debtSt),
                  toFrame(mfi_nfc_debtLt),toFrame(mfi_iv_debtLt),toFrame(mfi_mfi_debtLt),toFrame(mfi_ofi_debtLt),toFrame(mfi_icpf_debtLt),toFrame(mfi_govt_debtLt),toFrame(mfi_hh_debtLt),
                  toFrame(mfi_nfc_loanSt),toFrame(mfi_iv_loanSt),toFrame(mfi_ofi_loanSt),toFrame(mfi_icpf_loanSt),toFrame(mfi_govt_loanSt),toFrame(mfi_hh_loanSt),
                  toFrame(mfi_nfc_loanLt),toFrame(mfi_iv_loanLt),toFrame(mfi_ofi_loanLt),toFrame(mfi_icpf_loanLt),toFrame(mfi_govt_loanLt),toFrame(mfi_hh_loanLt),
                  toFrame(mfi_nfc_shares),toFrame(mfi_iv_shares),toFrame(mfi_mfi_shares),toFrame(mfi_ofi_shares),toFrame(mfi_icpf_shares),toFrame(mfi_govt_shares),
                  toFrame(mfi_iv_ivShares),toFrame(mfi_mfi_ivShares),
                  toFrame(icpf_nfc_debtSt),toFrame(icpf_iv_debtSt),toFrame(icpf_mfi_debtSt),toFrame(icpf_ofi_debtSt),toFrame(icpf_icpf_debtSt),toFrame(icpf_govt_debtSt),toFrame(icpf_hh_debtSt),
                  toFrame(icpf_nfc_debtLt),toFrame(icpf_iv_debtLt),toFrame(icpf_mfi_debtLt),toFrame(icpf_ofi_debtLt),toFrame(icpf_icpf_debtLt),toFrame(icpf_govt_debtLt),toFrame(icpf_hh_debtLt),
                  toFrame(icpf_nfc_loanSt),toFrame(icpf_iv_loanSt),toFrame(icpf_ofi_loanSt),toFrame(icpf_icpf_loanSt),toFrame(icpf_govt_loanSt),toFrame(icpf_hh_loanSt),
                  toFrame(icpf_nfc_loanLt),toFrame(icpf_iv_loanLt),toFrame(icpf_ofi_loanLt),toFrame(icpf_icpf_loanLt),toFrame(icpf_govt_loanLt),toFrame(icpf_hh_loanLt),
                  toFrame(icpf_nfc_shares),toFrame(icpf_iv_shares),toFrame(icpf_mfi_shares),toFrame(icpf_ofi_shares),toFrame(icpf_icpf_shares),toFrame(icpf_govt_shares),
                  toFrame(icpf_iv_ivShares),toFrame(icpf_mfi_ivShares),
                  toFrame(govt_nfc_debtSt),toFrame(govt_iv_debtSt),toFrame(govt_mfi_debtSt),toFrame(govt_ofi_debtSt),toFrame(govt_icpf_debtSt),toFrame(govt_govt_debtSt),toFrame(govt_hh_debtSt),
                  toFrame(govt_nfc_debtLt),toFrame(govt_iv_debtLt),toFrame(govt_mfi_debtLt),toFrame(govt_ofi_debtLt),toFrame(govt_icpf_debtLt),toFrame(govt_govt_debtLt),toFrame(govt_hh_debtLt),
                  toFrame(govt_nfc_loanSt),toFrame(govt_iv_loanSt),toFrame(govt_ofi_loanSt),toFrame(govt_icpf_loanSt),toFrame(govt_govt_loanSt),toFrame(govt_hh_loanSt),
                  toFrame(govt_nfc_loanLt),toFrame(govt_iv_loanLt),toFrame(govt_ofi_loanLt),toFrame(govt_icpf_loanLt),toFrame(govt_govt_loanLt),toFrame(govt_hh_loanLt),
                  toFrame(govt_nfc_shares),toFrame(govt_iv_shares),toFrame(govt_mfi_shares),toFrame(govt_ofi_shares),toFrame(govt_icpf_shares),toFrame(govt_govt_shares),
                  toFrame(govt_iv_ivShares),toFrame(govt_mfi_ivShares),
                  toFrame(hh_nfc_debtSt),toFrame(hh_iv_debtSt),toFrame(hh_mfi_debtSt),toFrame(hh_ofi_debtSt),toFrame(hh_icpf_debtSt),toFrame(hh_govt_debtSt),toFrame(hh_hh_debtSt),
                  toFrame(hh_nfc_debtLt),toFrame(hh_iv_debtLt),toFrame(hh_mfi_debtLt),toFrame(hh_ofi_debtLt),toFrame(hh_icpf_debtLt),toFrame(hh_govt_debtLt),toFrame(hh_hh_debtLt),
                  toFrame(hh_nfc_loanSt),toFrame(hh_iv_loanSt),toFrame(hh_ofi_loanSt),toFrame(hh_icpf_loanSt),toFrame(hh_govt_loanSt),toFrame(hh_hh_loanSt),
                  toFrame(hh_nfc_loanLt),toFrame(hh_iv_loanLt),toFrame(hh_ofi_loanLt),toFrame(hh_icpf_loanLt),toFrame(hh_govt_loanLt),toFrame(hh_hh_loanLt),
                  toFrame(hh_nfc_shares),toFrame(hh_iv_shares),toFrame(hh_mfi_shares),toFrame(hh_ofi_shares),toFrame(hh_icpf_shares),toFrame(hh_govt_shares),
                  toFrame(hh_iv_ivShares),toFrame(hh_mfi_ivShares),
                  toFrame(nfc_row_debtSt),toFrame(nfc_row_debtLt),toFrame(nfc_row_ivShares),toFrame(nfc_row_loanLt),toFrame(nfc_row_loanSt),toFrame(nfc_row_shares),
                  toFrame(row_nfc_debtSt),toFrame(row_nfc_debtLt),toFrame(row_nfc_loanLt),toFrame(row_nfc_loanSt),toFrame(row_nfc_shares),
                  toFrame(ofi_row_debtSt),toFrame(ofi_row_debtLt),toFrame(ofi_row_ivShares),toFrame(ofi_row_loanLt),toFrame(ofi_row_loanSt),toFrame(ofi_row_shares),
                  toFrame(row_ofi_debtSt),toFrame(row_ofi_debtLt),toFrame(row_ofi_loanLt),toFrame(row_ofi_loanSt),toFrame(row_ofi_shares),
                  toFrame(mfi_row_debtSt),toFrame(mfi_row_debtLt),toFrame(mfi_row_ivShares),toFrame(mfi_row_loanLt),toFrame(mfi_row_loanSt),toFrame(mfi_row_shares),
                  toFrame(row_mfi_debtSt),toFrame(row_mfi_debtLt),toFrame(row_mfi_loanLt),toFrame(row_mfi_loanSt),toFrame(row_mfi_shares),toFrame(row_mfi_ivShares),
                  toFrame(icpf_row_debtSt),toFrame(icpf_row_debtLt),toFrame(icpf_row_ivShares),toFrame(icpf_row_loanLt),toFrame(icpf_row_loanSt),toFrame(icpf_row_shares),
                  toFrame(row_icpf_debtSt),toFrame(row_icpf_debtLt),toFrame(row_icpf_loanLt),toFrame(row_icpf_loanSt),toFrame(row_icpf_shares),
                  toFrame(hh_row_debtSt),toFrame(hh_row_debtLt),toFrame(hh_row_ivShares),toFrame(hh_row_loanLt),toFrame(hh_row_loanSt),toFrame(hh_row_shares),
                  toFrame(govt_row_debtSt),toFrame(govt_row_debtLt),toFrame(govt_row_ivShares),toFrame(govt_row_loanLt),toFrame(govt_row_loanSt),toFrame(govt_row_shares),
                  toFrame(row_govt_debtSt),toFrame(row_govt_debtLt),toFrame(row_govt_loanLt),toFrame(row_govt_loanSt),toFrame(row_govt_shares),
                  toFrame(iv_row_debtSt),toFrame(iv_row_debtLt),toFrame(iv_row_ivShares),toFrame(iv_row_loanLt),toFrame(iv_row_loanSt),toFrame(iv_row_shares),
                  toFrame(row_iv_debtSt),toFrame(row_iv_debtLt),toFrame(row_iv_loanLt),toFrame(row_iv_loanSt),toFrame(row_iv_shares),toFrame(row_iv_ivShares),
                  toFrame(row_hh_debtSt),toFrame(row_hh_debtLt),toFrame(row_hh_loanSt),toFrame(row_hh_loanLt),toFrame(row_hh_ivShares),toFrame(row_hh_shares),
                  toFrame(row_row_debtSt),toFrame(row_row_debtLt),toFrame(row_row_loanSt),toFrame(row_row_loanLt),toFrame(row_row_ivShares),toFrame(row_row_shares)
)

banksData<-rbind(toFrame2(iv_assets),toFrame2(nfc_assets),toFrame2(mfi_assets),toFrame2(ofi_assets),toFrame2(icpf_assets),toFrame2(hh_assets),toFrame2(govt_assets),toFrame2(row_assets))
liab<-rbind(toFrame2(iv_liabilities),toFrame2(nfc_liabilities),toFrame2(mfi_liabilities),toFrame2(ofi_liabilities),
            toFrame2(icpf_liabilities),toFrame2(hh_liabilities),toFrame2(govt_liabilities),toFrame2(row_liabilities))
banksData<-cbind(banksData,round(liab$value/1000,2))
banksData$value<-round(banksData$value/1000,2)
names(banksData)[4]<-"Assets"
names(banksData)[5]<-"Liabilities"

total<-ddply(matrixData,.(source,target,time),summarize,value=sum(value))
total$type="total"
matrixData<-rbind(matrixData,total)
matrixData$value<-ifelse(!is.na(round(matrixData$value/1000,2)),(round(matrixData$value/1000,2)),0)

dimensions<-unique(matrixData[,3])
time<-unique(matrixData[,4])

#create directories
#ONLY IF NOT THERE YET
#setwd("./Data/Luxembourg")
#for(i in 1:length(dimensions)) {
#  str <- toString(dimensions[i])
#  dir.create(str)  
#}

#populate directory by asset
#CHECK WORKING DIRECTORY IS DATA!!!
for (i in 1:length(dimensions)){
  setwd(toString(dimensions[i]))
  byAssetData <- matrixData[which(matrixData$type==dimensions[i]),]
  #for every date in newdata create a matrix
  for (i in 1:length(time)){
    byTimeByAssetData <- byAssetData[which(byAssetData$time==time[i]),]
    byTimeByAssetData$time<-formatDate(byTimeByAssetData$time[1])
    matrix<-byTimeByAssetData %>% spread(target,value)
    matrix<-subset(matrix,select=-type)
    matrix<-subset(matrix,select=-time)
    for (j in 2:length(names(matrix))) {
      names(matrix)[j]<-toString(paste("_",names(matrix[j]),sep=""))
    }
    
    file<-paste("matrix ",byTimeByAssetData$time[1],".csv",sep="")
    write.csv(matrix,file,row.names=FALSE, quote=FALSE, na="0")
    
    #node files
    byTimeBanksData <- banksData[which(banksData$time==time[i]),]
    
    file<-paste("banks ",byTimeByAssetData$time[1],".csv",sep="")
    write.csv(byTimeBanksData,file,row.names=FALSE, quote=FALSE,na="0")
  }
  setwd("..")
}

