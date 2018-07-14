library(ecb)
library(plyr)
library(tidyr)
library(pdfetch)

#########utility functions#########
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
  input<-data.frame("Node"=strArr[1],"Bankengruppe"=strArr[1],"time"=input$obstime,"value"=input$obsvalue)
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
iv_nfc_debtSt<-get_data("QSA.Q.N.DE.W2.S124.S11.N.A.LE.F3.S._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
iv_iv_debtSt<-get_data("QSA.Q.N.DE.W2.S124.S124.N.A.LE.F3.S._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
iv_mfi_debtSt<-get_data("QSA.Q.N.DE.W2.S124.S12K.N.A.LE.F3.S._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
iv_ofi_debtSt<-get_data("QSA.Q.N.DE.W2.S124.S12O.N.A.LE.F3.S._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
iv_icpf_debtSt<-get_data("QSA.Q.N.DE.W2.S124.S12Q.N.A.LE.F3.S._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
iv_govt_debtSt<-get_data("QSA.Q.N.DE.W2.S124.S13.N.A.LE.F3.S._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
iv_hh_debtSt<-get_data("QSA.Q.N.DE.W2.S124.S1M.N.A.LE.F3.S._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))

#lt
iv_nfc_debtLt<-get_data("QSA.Q.N.DE.W2.S124.S11.N.A.LE.F3.L._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
iv_iv_debtLt<-get_data("QSA.Q.N.DE.W2.S124.S124.N.A.LE.F3.L._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
iv_mfi_debtLt<-get_data("QSA.Q.N.DE.W2.S124.S12K.N.A.LE.F3.L._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
iv_ofi_debtLt<-get_data("QSA.Q.N.DE.W2.S124.S12O.N.A.LE.F3.L._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
iv_icpf_debtLt<-get_data("QSA.Q.N.DE.W2.S124.S12Q.N.A.LE.F3.L._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
iv_govt_debtLt<-get_data("QSA.Q.N.DE.W2.S124.S13.N.A.LE.F3.L._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
iv_hh_debtLt<-get_data("QSA.Q.N.DE.W2.S124.S1M.N.A.LE.F3.L._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))

##loans
#st
iv_nfc_loanSt<-get_data("QSA.Q.N.DE.W2.S124.S11.N.A.LE.F4.S._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
iv_iv_loanSt<-get_data("QSA.Q.N.DE.W2.S124.S124.N.A.LE.F4.S._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
iv_ofi_loanSt<-get_data("QSA.Q.N.DE.W2.S124.S12O.N.A.LE.F4.S._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
iv_icpf_loanSt<-get_data("QSA.Q.N.DE.W2.S124.S12Q.N.A.LE.F4.S._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
iv_govt_loanSt<-get_data("QSA.Q.N.DE.W2.S124.S13.N.A.LE.F4.S._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
iv_hh_loanSt<-get_data("QSA.Q.N.DE.W2.S124.S1M.N.A.LE.F4.S._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))

#lt
iv_nfc_loanLt<-get_data("QSA.Q.N.DE.W2.S124.S11.N.A.LE.F4.L._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
iv_iv_loanLt<-get_data("QSA.Q.N.DE.W2.S124.S124.N.A.LE.F4.L._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
iv_ofi_loanLt<-get_data("QSA.Q.N.DE.W2.S124.S12O.N.A.LE.F4.L._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
iv_icpf_loanLt<-get_data("QSA.Q.N.DE.W2.S124.S12Q.N.A.LE.F4.L._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
iv_govt_loanLt<-get_data("QSA.Q.N.DE.W2.S124.S13.N.A.LE.F4.L._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
iv_hh_loanLt<-get_data("QSA.Q.N.DE.W2.S124.S1M.N.A.LE.F4.L._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))

##shares
iv_nfc_shares<-get_data("QSA.Q.N.DE.W2.S124.S11.N.A.LE.F511._Z._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
iv_iv_shares<-get_data("QSA.Q.N.DE.W2.S124.S124.N.A.LE.F511._Z._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
iv_mfi_shares<-get_data("QSA.Q.N.DE.W2.S124.S12K.N.A.LE.F511._Z._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
iv_ofi_shares<-get_data("QSA.Q.N.DE.W2.S124.S12O.N.A.LE.F511._Z._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
iv_icpf_shares<-get_data("QSA.Q.N.DE.W2.S124.S12Q.N.A.LE.F511._Z._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
iv_govt_shares<-get_data("QSA.Q.N.DE.W2.S124.S13.N.A.LE.F511._Z._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))

##iv_shares
iv_iv_ivShares<-get_data("QSA.Q.N.DE.W2.S124.S124.N.A.LE.F52._Z._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
iv_mfi_ivShares<-get_data("QSA.Q.N.DE.W2.S124.S12K.N.A.LE.F52._Z._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))

###ofi
##bonds
#st
ofi_nfc_debtSt<-get_data("QSA.Q.N.DE.W2.S12O.S11.N.A.LE.F3.S._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
ofi_iv_debtSt<-get_data("QSA.Q.N.DE.W2.S12O.S124.N.A.LE.F3.S._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
ofi_mfi_debtSt<-get_data("QSA.Q.N.DE.W2.S12O.S12K.N.A.LE.F3.S._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
ofi_ofi_debtSt<-get_data("QSA.Q.N.DE.W2.S12O.S12O.N.A.LE.F3.S._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
ofi_icpf_debtSt<-get_data("QSA.Q.N.DE.W2.S12O.S12Q.N.A.LE.F3.S._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
ofi_govt_debtSt<-get_data("QSA.Q.N.DE.W2.S12O.S13.N.A.LE.F3.S._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
ofi_hh_debtSt<-get_data("QSA.Q.N.DE.W2.S12O.S1M.N.A.LE.F3.S._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))

#lt
ofi_nfc_debtLt<-get_data("QSA.Q.N.DE.W2.S12O.S11.N.A.LE.F3.L._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
ofi_iv_debtLt<-get_data("QSA.Q.N.DE.W2.S12O.S124.N.A.LE.F3.L._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
ofi_mfi_debtLt<-get_data("QSA.Q.N.DE.W2.S12O.S12K.N.A.LE.F3.L._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
ofi_ofi_debtLt<-get_data("QSA.Q.N.DE.W2.S12O.S12O.N.A.LE.F3.L._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
ofi_icpf_debtLt<-get_data("QSA.Q.N.DE.W2.S12O.S12Q.N.A.LE.F3.L._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
ofi_govt_debtLt<-get_data("QSA.Q.N.DE.W2.S12O.S13.N.A.LE.F3.L._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
ofi_hh_debtLt<-get_data("QSA.Q.N.DE.W2.S12O.S1M.N.A.LE.F3.L._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))

##loans
#st
ofi_nfc_loanSt<-get_data("QSA.Q.N.DE.W2.S12O.S11.N.A.LE.F4.S._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
ofi_iv_loanSt<-get_data("QSA.Q.N.DE.W2.S12O.S124.N.A.LE.F4.S._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
ofi_ofi_loanSt<-get_data("QSA.Q.N.DE.W2.S12O.S12O.N.A.LE.F4.S._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
ofi_icpf_loanSt<-get_data("QSA.Q.N.DE.W2.S12O.S12Q.N.A.LE.F4.S._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
ofi_govt_loanSt<-get_data("QSA.Q.N.DE.W2.S12O.S13.N.A.LE.F4.S._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
ofi_hh_loanSt<-get_data("QSA.Q.N.DE.W2.S12O.S1M.N.A.LE.F4.S._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))

#lt
ofi_nfc_loanLt<-get_data("QSA.Q.N.DE.W2.S12O.S11.N.A.LE.F4.L._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
ofi_iv_loanLt<-get_data("QSA.Q.N.DE.W2.S12O.S124.N.A.LE.F4.L._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
ofi_ofi_loanLt<-get_data("QSA.Q.N.DE.W2.S12O.S12O.N.A.LE.F4.L._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
ofi_icpf_loanLt<-get_data("QSA.Q.N.DE.W2.S12O.S12Q.N.A.LE.F4.L._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
ofi_govt_loanLt<-get_data("QSA.Q.N.DE.W2.S12O.S13.N.A.LE.F4.L._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
ofi_hh_loanLt<-get_data("QSA.Q.N.DE.W2.S12O.S1M.N.A.LE.F4.L._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))

##shares
ofi_nfc_shares<-get_data("QSA.Q.N.DE.W2.S12O.S11.N.A.LE.F511._Z._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
ofi_iv_shares<-get_data("QSA.Q.N.DE.W2.S12O.S124.N.A.LE.F511._Z._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
ofi_mfi_shares<-get_data("QSA.Q.N.DE.W2.S12O.S12K.N.A.LE.F511._Z._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
ofi_ofi_shares<-get_data("QSA.Q.N.DE.W2.S12O.S12O.N.A.LE.F511._Z._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
ofi_icpf_shares<-get_data("QSA.Q.N.DE.W2.S12O.S12Q.N.A.LE.F511._Z._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
ofi_govt_shares<-get_data("QSA.Q.N.DE.W2.S12O.S13.N.A.LE.F511._Z._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))

##iv_shares
ofi_iv_ivShares<-get_data("QSA.Q.N.DE.W2.S12O.S124.N.A.LE.F52._Z._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
ofi_mfi_ivShares<-get_data("QSA.Q.N.DE.W2.S12O.S12K.N.A.LE.F52._Z._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))

###nfc
##bonds
#st
nfc_nfc_debtSt<-get_data("QSA.Q.N.DE.W2.S11.S11.N.A.LE.F3.S._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
nfc_iv_debtSt<-get_data("QSA.Q.N.DE.W2.S11.S124.N.A.LE.F3.S._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
nfc_mfi_debtSt<-get_data("QSA.Q.N.DE.W2.S11.S12K.N.A.LE.F3.S._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
nfc_ofi_debtSt<-get_data("QSA.Q.N.DE.W2.S11.S12O.N.A.LE.F3.S._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
nfc_icpf_debtSt<-get_data("QSA.Q.N.DE.W2.S11.S12Q.N.A.LE.F3.S._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
nfc_govt_debtSt<-get_data("QSA.Q.N.DE.W2.S11.S13.N.A.LE.F3.S._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
nfc_hh_debtSt<-get_data("QSA.Q.N.DE.W2.S11.S1M.N.A.LE.F3.S._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))

#lt
nfc_nfc_debtLt<-get_data("QSA.Q.N.DE.W2.S11.S11.N.A.LE.F3.L._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
nfc_iv_debtLt<-get_data("QSA.Q.N.DE.W2.S11.S124.N.A.LE.F3.L._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
nfc_mfi_debtLt<-get_data("QSA.Q.N.DE.W2.S11.S12K.N.A.LE.F3.L._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
nfc_ofi_debtLt<-get_data("QSA.Q.N.DE.W2.S11.S12O.N.A.LE.F3.L._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
nfc_icpf_debtLt<-get_data("QSA.Q.N.DE.W2.S11.S12Q.N.A.LE.F3.L._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
nfc_govt_debtLt<-get_data("QSA.Q.N.DE.W2.S11.S13.N.A.LE.F3.L._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
nfc_hh_debtLt<-get_data("QSA.Q.N.DE.W2.S11.S1M.N.A.LE.F3.L._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))

##loans
#st
nfc_nfc_loanSt<-get_data("QSA.Q.N.DE.W2.S11.S11.N.A.LE.F4.S._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
nfc_iv_loanSt<-get_data("QSA.Q.N.DE.W2.S11.S124.N.A.LE.F4.S._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
nfc_ofi_loanSt<-get_data("QSA.Q.N.DE.W2.S11.S12O.N.A.LE.F4.S._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
nfc_icpf_loanSt<-get_data("QSA.Q.N.DE.W2.S11.S12Q.N.A.LE.F4.S._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
nfc_govt_loanSt<-get_data("QSA.Q.N.DE.W2.S11.S13.N.A.LE.F4.S._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
nfc_hh_loanSt<-get_data("QSA.Q.N.DE.W2.S11.S1M.N.A.LE.F4.S._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))

#lt
nfc_nfc_loanLt<-get_data("QSA.Q.N.DE.W2.S11.S11.N.A.LE.F4.L._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
nfc_iv_loanLt<-get_data("QSA.Q.N.DE.W2.S11.S124.N.A.LE.F4.L._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
nfc_ofi_loanLt<-get_data("QSA.Q.N.DE.W2.S11.S12O.N.A.LE.F4.L._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
nfc_icpf_loanLt<-get_data("QSA.Q.N.DE.W2.S11.S12Q.N.A.LE.F4.L._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
nfc_govt_loanLt<-get_data("QSA.Q.N.DE.W2.S11.S13.N.A.LE.F4.L._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
nfc_hh_loanLt<-get_data("QSA.Q.N.DE.W2.S11.S1M.N.A.LE.F4.L._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))

##shares
nfc_nfc_shares<-get_data("QSA.Q.N.DE.W2.S11.S11.N.A.LE.F511._Z._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
nfc_iv_shares<-get_data("QSA.Q.N.DE.W2.S11.S124.N.A.LE.F511._Z._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
nfc_mfi_shares<-get_data("QSA.Q.N.DE.W2.S11.S12K.N.A.LE.F511._Z._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
nfc_ofi_shares<-get_data("QSA.Q.N.DE.W2.S11.S12O.N.A.LE.F511._Z._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
nfc_icpf_shares<-get_data("QSA.Q.N.DE.W2.S11.S12Q.N.A.LE.F511._Z._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
nfc_govt_shares<-get_data("QSA.Q.N.DE.W2.S11.S13.N.A.LE.F511._Z._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))

##iv_shares
nfc_iv_ivShares<-get_data("QSA.Q.N.DE.W2.S11.S124.N.A.LE.F52._Z._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
nfc_mfi_ivShares<-get_data("QSA.Q.N.DE.W2.S11.S12K.N.A.LE.F52._Z._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))

###mfi
##bonds
#st
mfi_nfc_debtSt<-get_data("QSA.Q.N.DE.W2.S12K.S11.N.A.LE.F3.S._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
mfi_iv_debtSt<-get_data("QSA.Q.N.DE.W2.S12K.S124.N.A.LE.F3.S._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
mfi_mfi_debtSt<-get_data("QSA.Q.N.DE.W2.S12K.S12K.N.A.LE.F3.S._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
mfi_ofi_debtSt<-get_data("QSA.Q.N.DE.W2.S12K.S12O.N.A.LE.F3.S._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
mfi_icpf_debtSt<-get_data("QSA.Q.N.DE.W2.S12K.S12Q.N.A.LE.F3.S._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
mfi_govt_debtSt<-get_data("QSA.Q.N.DE.W2.S12K.S13.N.A.LE.F3.S._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
mfi_hh_debtSt<-get_data("QSA.Q.N.DE.W2.S12K.S1M.N.A.LE.F3.S._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))

#lt
mfi_nfc_debtLt<-get_data("QSA.Q.N.DE.W2.S12K.S11.N.A.LE.F3.L._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
mfi_iv_debtLt<-get_data("QSA.Q.N.DE.W2.S12K.S124.N.A.LE.F3.L._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
mfi_mfi_debtLt<-get_data("QSA.Q.N.DE.W2.S12K.S12K.N.A.LE.F3.L._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
mfi_ofi_debtLt<-get_data("QSA.Q.N.DE.W2.S12K.S12O.N.A.LE.F3.L._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
mfi_icpf_debtLt<-get_data("QSA.Q.N.DE.W2.S12K.S12Q.N.A.LE.F3.L._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
mfi_govt_debtLt<-get_data("QSA.Q.N.DE.W2.S12K.S13.N.A.LE.F3.L._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
mfi_hh_debtLt<-get_data("QSA.Q.N.DE.W2.S12K.S1M.N.A.LE.F3.L._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))

##loans
#st
mfi_nfc_loanSt<-get_data("QSA.Q.N.DE.W2.S12K.S11.N.A.LE.F4.S._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
mfi_iv_loanSt<-get_data("QSA.Q.N.DE.W2.S12K.S124.N.A.LE.F4.S._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
mfi_ofi_loanSt<-get_data("QSA.Q.N.DE.W2.S12K.S12O.N.A.LE.F4.S._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
mfi_icpf_loanSt<-get_data("QSA.Q.N.DE.W2.S12K.S12Q.N.A.LE.F4.S._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
mfi_govt_loanSt<-get_data("QSA.Q.N.DE.W2.S12K.S13.N.A.LE.F4.S._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
mfi_hh_loanSt<-get_data("QSA.Q.N.DE.W2.S12K.S1M.N.A.LE.F4.S._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))

#lt
mfi_nfc_loanLt<-get_data("QSA.Q.N.DE.W2.S12K.S11.N.A.LE.F4.L._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
mfi_iv_loanLt<-get_data("QSA.Q.N.DE.W2.S12K.S124.N.A.LE.F4.L._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
mfi_ofi_loanLt<-get_data("QSA.Q.N.DE.W2.S12K.S12O.N.A.LE.F4.L._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
mfi_icpf_loanLt<-get_data("QSA.Q.N.DE.W2.S12K.S12Q.N.A.LE.F4.L._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
mfi_govt_loanLt<-get_data("QSA.Q.N.DE.W2.S12K.S13.N.A.LE.F4.L._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
mfi_hh_loanLt<-get_data("QSA.Q.N.DE.W2.S12K.S1M.N.A.LE.F4.L._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))

##shares
mfi_nfc_shares<-get_data("QSA.Q.N.DE.W2.S12K.S11.N.A.LE.F511._Z._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
mfi_iv_shares<-get_data("QSA.Q.N.DE.W2.S12K.S124.N.A.LE.F511._Z._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
mfi_mfi_shares<-get_data("QSA.Q.N.DE.W2.S12K.S12K.N.A.LE.F511._Z._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
mfi_ofi_shares<-get_data("QSA.Q.N.DE.W2.S12K.S12O.N.A.LE.F511._Z._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
mfi_icpf_shares<-get_data("QSA.Q.N.DE.W2.S12K.S12Q.N.A.LE.F511._Z._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
mfi_govt_shares<-get_data("QSA.Q.N.DE.W2.S12K.S13.N.A.LE.F511._Z._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))

##iv_shares
mfi_iv_ivShares<-get_data("QSA.Q.N.DE.W2.S12K.S124.N.A.LE.F52._Z._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
mfi_mfi_ivShares<-get_data("QSA.Q.N.DE.W2.S12K.S12K.N.A.LE.F52._Z._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))

###icpf
##bonds
#st
icpf_nfc_debtSt<-get_data("QSA.Q.N.DE.W2.S12Q.S11.N.A.LE.F3.S._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
icpf_iv_debtSt<-get_data("QSA.Q.N.DE.W2.S12Q.S124.N.A.LE.F3.S._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
icpf_mfi_debtSt<-get_data("QSA.Q.N.DE.W2.S12Q.S12K.N.A.LE.F3.S._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
icpf_ofi_debtSt<-get_data("QSA.Q.N.DE.W2.S12Q.S12O.N.A.LE.F3.S._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
icpf_icpf_debtSt<-get_data("QSA.Q.N.DE.W2.S12Q.S12Q.N.A.LE.F3.S._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
icpf_govt_debtSt<-get_data("QSA.Q.N.DE.W2.S12Q.S13.N.A.LE.F3.S._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
icpf_hh_debtSt<-get_data("QSA.Q.N.DE.W2.S12Q.S1M.N.A.LE.F3.S._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))

#lt
icpf_nfc_debtLt<-get_data("QSA.Q.N.DE.W2.S12Q.S11.N.A.LE.F3.L._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
icpf_iv_debtLt<-get_data("QSA.Q.N.DE.W2.S12Q.S124.N.A.LE.F3.L._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
icpf_mfi_debtLt<-get_data("QSA.Q.N.DE.W2.S12Q.S12K.N.A.LE.F3.L._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
icpf_ofi_debtLt<-get_data("QSA.Q.N.DE.W2.S12Q.S12O.N.A.LE.F3.L._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
icpf_icpf_debtLt<-get_data("QSA.Q.N.DE.W2.S12Q.S12Q.N.A.LE.F3.L._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
icpf_govt_debtLt<-get_data("QSA.Q.N.DE.W2.S12Q.S13.N.A.LE.F3.L._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
icpf_hh_debtLt<-get_data("QSA.Q.N.DE.W2.S12Q.S1M.N.A.LE.F3.L._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))

##loans
#st
icpf_nfc_loanSt<-get_data("QSA.Q.N.DE.W2.S12Q.S11.N.A.LE.F4.S._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
icpf_iv_loanSt<-get_data("QSA.Q.N.DE.W2.S12Q.S124.N.A.LE.F4.S._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
icpf_ofi_loanSt<-get_data("QSA.Q.N.DE.W2.S12Q.S12O.N.A.LE.F4.S._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
icpf_icpf_loanSt<-get_data("QSA.Q.N.DE.W2.S12Q.S12Q.N.A.LE.F4.S._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
icpf_govt_loanSt<-get_data("QSA.Q.N.DE.W2.S12Q.S13.N.A.LE.F4.S._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
icpf_hh_loanSt<-get_data("QSA.Q.N.DE.W2.S12Q.S1M.N.A.LE.F4.S._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))

#lt
icpf_nfc_loanLt<-get_data("QSA.Q.N.DE.W2.S12Q.S11.N.A.LE.F4.L._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
icpf_iv_loanLt<-get_data("QSA.Q.N.DE.W2.S12Q.S124.N.A.LE.F4.L._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
icpf_ofi_loanLt<-get_data("QSA.Q.N.DE.W2.S12Q.S12O.N.A.LE.F4.L._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
icpf_icpf_loanLt<-get_data("QSA.Q.N.DE.W2.S12Q.S12Q.N.A.LE.F4.L._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
icpf_govt_loanLt<-get_data("QSA.Q.N.DE.W2.S12Q.S13.N.A.LE.F4.L._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
icpf_hh_loanLt<-get_data("QSA.Q.N.DE.W2.S12Q.S1M.N.A.LE.F4.L._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))

##shares
icpf_nfc_shares<-get_data("QSA.Q.N.DE.W2.S12Q.S11.N.A.LE.F511._Z._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
icpf_iv_shares<-get_data("QSA.Q.N.DE.W2.S12Q.S124.N.A.LE.F511._Z._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
icpf_mfi_shares<-get_data("QSA.Q.N.DE.W2.S12Q.S12K.N.A.LE.F511._Z._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
icpf_ofi_shares<-get_data("QSA.Q.N.DE.W2.S12Q.S12O.N.A.LE.F511._Z._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
icpf_icpf_shares<-get_data("QSA.Q.N.DE.W2.S12Q.S12Q.N.A.LE.F511._Z._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
icpf_govt_shares<-get_data("QSA.Q.N.DE.W2.S12Q.S13.N.A.LE.F511._Z._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))

##iv_shares
icpf_iv_ivShares<-get_data("QSA.Q.N.DE.W2.S12Q.S124.N.A.LE.F52._Z._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
icpf_mfi_ivShares<-get_data("QSA.Q.N.DE.W2.S12Q.S12K.N.A.LE.F52._Z._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))

###govt
##bonds
#st
govt_nfc_debtSt<-get_data("QSA.Q.N.DE.W2.S13.S11.N.A.LE.F3.S._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
govt_iv_debtSt<-get_data("QSA.Q.N.DE.W2.S13.S124.N.A.LE.F3.S._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
govt_mfi_debtSt<-get_data("QSA.Q.N.DE.W2.S13.S12K.N.A.LE.F3.S._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
govt_ofi_debtSt<-get_data("QSA.Q.N.DE.W2.S13.S12O.N.A.LE.F3.S._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
govt_icpf_debtSt<-get_data("QSA.Q.N.DE.W2.S13.S12Q.N.A.LE.F3.S._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
govt_govt_debtSt<-get_data("QSA.Q.N.DE.W2.S13.S13.N.A.LE.F3.S._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
govt_hh_debtSt<-get_data("QSA.Q.N.DE.W2.S13.S1M.N.A.LE.F3.S._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))

#lt
govt_nfc_debtLt<-get_data("QSA.Q.N.DE.W2.S13.S11.N.A.LE.F3.L._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
govt_iv_debtLt<-get_data("QSA.Q.N.DE.W2.S13.S124.N.A.LE.F3.L._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
govt_mfi_debtLt<-get_data("QSA.Q.N.DE.W2.S13.S12K.N.A.LE.F3.L._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
govt_ofi_debtLt<-get_data("QSA.Q.N.DE.W2.S13.S12O.N.A.LE.F3.L._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
govt_icpf_debtLt<-get_data("QSA.Q.N.DE.W2.S13.S12Q.N.A.LE.F3.L._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
govt_govt_debtLt<-get_data("QSA.Q.N.DE.W2.S13.S13.N.A.LE.F3.L._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
govt_hh_debtLt<-get_data("QSA.Q.N.DE.W2.S13.S1M.N.A.LE.F3.L._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))

##loans
#st
govt_nfc_loanSt<-get_data("QSA.Q.N.DE.W2.S13.S11.N.A.LE.F4.S._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
govt_iv_loanSt<-get_data("QSA.Q.N.DE.W2.S13.S124.N.A.LE.F4.S._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
govt_ofi_loanSt<-get_data("QSA.Q.N.DE.W2.S13.S12O.N.A.LE.F4.S._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
govt_icpf_loanSt<-get_data("QSA.Q.N.DE.W2.S13.S12Q.N.A.LE.F4.S._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
govt_govt_loanSt<-get_data("QSA.Q.N.DE.W2.S13.S13.N.A.LE.F4.S._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
govt_hh_loanSt<-get_data("QSA.Q.N.DE.W2.S13.S1M.N.A.LE.F4.S._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))

#lt
govt_nfc_loanLt<-get_data("QSA.Q.N.DE.W2.S13.S11.N.A.LE.F4.L._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
govt_iv_loanLt<-get_data("QSA.Q.N.DE.W2.S13.S124.N.A.LE.F4.L._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
govt_ofi_loanLt<-get_data("QSA.Q.N.DE.W2.S13.S12O.N.A.LE.F4.L._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
govt_icpf_loanLt<-get_data("QSA.Q.N.DE.W2.S13.S12Q.N.A.LE.F4.L._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
govt_govt_loanLt<-get_data("QSA.Q.N.DE.W2.S13.S13.N.A.LE.F4.L._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
govt_hh_loanLt<-get_data("QSA.Q.N.DE.W2.S13.S1M.N.A.LE.F4.L._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))

##shares
govt_nfc_shares<-get_data("QSA.Q.N.DE.W2.S13.S11.N.A.LE.F511._Z._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
govt_iv_shares<-get_data("QSA.Q.N.DE.W2.S13.S124.N.A.LE.F511._Z._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
govt_mfi_shares<-get_data("QSA.Q.N.DE.W2.S13.S12K.N.A.LE.F511._Z._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
govt_ofi_shares<-get_data("QSA.Q.N.DE.W2.S13.S12O.N.A.LE.F511._Z._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
govt_icpf_shares<-get_data("QSA.Q.N.DE.W2.S13.S12Q.N.A.LE.F511._Z._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
govt_govt_shares<-get_data("QSA.Q.N.DE.W2.S13.S13.N.A.LE.F511._Z._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))

##iv_shares
govt_iv_ivShares<-get_data("QSA.Q.N.DE.W2.S13.S124.N.A.LE.F52._Z._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
govt_mfi_ivShares<-get_data("QSA.Q.N.DE.W2.S13.S12K.N.A.LE.F52._Z._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))

###hh
##bonds
#st
hh_nfc_debtSt<-get_data("QSA.Q.N.DE.W2.S1M.S11.N.A.LE.F3.S._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
hh_iv_debtSt<-get_data("QSA.Q.N.DE.W2.S1M.S124.N.A.LE.F3.S._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
hh_mfi_debtSt<-get_data("QSA.Q.N.DE.W2.S1M.S12K.N.A.LE.F3.S._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
hh_ofi_debtSt<-get_data("QSA.Q.N.DE.W2.S1M.S12O.N.A.LE.F3.S._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
hh_icpf_debtSt<-get_data("QSA.Q.N.DE.W2.S1M.S12Q.N.A.LE.F3.S._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
hh_govt_debtSt<-get_data("QSA.Q.N.DE.W2.S1M.S13.N.A.LE.F3.S._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
hh_hh_debtSt<-get_data("QSA.Q.N.DE.W2.S1M.S1M.N.A.LE.F3.S._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))

#lt
hh_nfc_debtLt<-get_data("QSA.Q.N.DE.W2.S1M.S11.N.A.LE.F3.L._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
hh_iv_debtLt<-get_data("QSA.Q.N.DE.W2.S1M.S124.N.A.LE.F3.L._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
hh_mfi_debtLt<-get_data("QSA.Q.N.DE.W2.S1M.S12K.N.A.LE.F3.L._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
hh_ofi_debtLt<-get_data("QSA.Q.N.DE.W2.S1M.S12O.N.A.LE.F3.L._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
hh_icpf_debtLt<-get_data("QSA.Q.N.DE.W2.S1M.S12Q.N.A.LE.F3.L._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
hh_govt_debtLt<-get_data("QSA.Q.N.DE.W2.S1M.S13.N.A.LE.F3.L._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
hh_hh_debtLt<-get_data("QSA.Q.N.DE.W2.S1M.S1M.N.A.LE.F3.L._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))

##loans
#st
hh_nfc_loanSt<-get_data("QSA.Q.N.DE.W2.S1M.S11.N.A.LE.F4.S._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
hh_iv_loanSt<-get_data("QSA.Q.N.DE.W2.S1M.S124.N.A.LE.F4.S._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
hh_ofi_loanSt<-get_data("QSA.Q.N.DE.W2.S1M.S12O.N.A.LE.F4.S._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
hh_icpf_loanSt<-get_data("QSA.Q.N.DE.W2.S1M.S12Q.N.A.LE.F4.S._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
hh_govt_loanSt<-get_data("QSA.Q.N.DE.W2.S1M.S13.N.A.LE.F4.S._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
hh_hh_loanSt<-get_data("QSA.Q.N.DE.W2.S1M.S1M.N.A.LE.F4.S._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))

#lt
hh_nfc_loanLt<-get_data("QSA.Q.N.DE.W2.S1M.S11.N.A.LE.F4.L._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
hh_iv_loanLt<-get_data("QSA.Q.N.DE.W2.S1M.S124.N.A.LE.F4.L._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
hh_ofi_loanLt<-get_data("QSA.Q.N.DE.W2.S1M.S12O.N.A.LE.F4.L._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
hh_icpf_loanLt<-get_data("QSA.Q.N.DE.W2.S1M.S12Q.N.A.LE.F4.L._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
hh_govt_loanLt<-get_data("QSA.Q.N.DE.W2.S1M.S13.N.A.LE.F4.L._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
hh_hh_loanLt<-get_data("QSA.Q.N.DE.W2.S1M.S1M.N.A.LE.F4.L._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))

##shares
hh_nfc_shares<-get_data("QSA.Q.N.DE.W2.S1M.S11.N.A.LE.F511._Z._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
hh_iv_shares<-get_data("QSA.Q.N.DE.W2.S1M.S124.N.A.LE.F511._Z._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
hh_mfi_shares<-get_data("QSA.Q.N.DE.W2.S1M.S12K.N.A.LE.F511._Z._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
hh_ofi_shares<-get_data("QSA.Q.N.DE.W2.S1M.S12O.N.A.LE.F511._Z._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
hh_icpf_shares<-get_data("QSA.Q.N.DE.W2.S1M.S12Q.N.A.LE.F511._Z._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
hh_govt_shares<-get_data("QSA.Q.N.DE.W2.S1M.S13.N.A.LE.F511._Z._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))

##iv_shares
hh_iv_ivShares<-get_data("QSA.Q.N.DE.W2.S1M.S124.N.A.LE.F52._Z._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
hh_mfi_ivShares<-get_data("QSA.Q.N.DE.W2.S1M.S12K.N.A.LE.F52._Z._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))

##balance sheets
iv_assets<-get_data("QSA.Q.N.DE.W0.S124.S1.N.A.LE.F._Z._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
iv_liabilities<-get_data("QSA.Q.N.DE.W0.S124.S1.N.L.LE.F._Z._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
nfc_assets<-get_data("QSA.Q.N.DE.W0.S11.S1.N.A.LE.F._Z._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
nfc_liabilities<-get_data("QSA.Q.N.DE.W0.S11.S1.N.L.LE.F._Z._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
mfi_assets<-get_data("QSA.Q.N.DE.W0.S12K.S1.N.A.LE.F._Z._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
mfi_liabilities<-get_data("QSA.Q.N.DE.W0.S12K.S1.N.L.LE.F._Z._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
ofi_assets<-get_data("QSA.Q.N.DE.W0.S12O.S1.N.A.LE.F._Z._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
ofi_liabilities<-get_data("QSA.Q.N.DE.W0.S12O.S1.N.L.LE.F._Z._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
icpf_assets<-get_data("QSA.Q.N.DE.W0.S12Q.S1.N.A.LE.F._Z._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
icpf_liabilities<-get_data("QSA.Q.N.DE.W0.S12Q.S1.N.L.LE.F._Z._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
hh_assets<-get_data("QSA.Q.N.DE.W0.S1M.S1.N.A.LE.F._Z._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
hh_liabilities<-get_data("QSA.Q.N.DE.W0.S1M.S1.N.L.LE.F._Z._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
govt_assets<-get_data("QSA.Q.N.DE.W0.S13.S1.N.A.LE.F._Z._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))
govt_liabilities<-get_data("QSA.Q.N.DE.W0.S13.S1.N.L.LE.F._Z._Z.XDC._T.S.V.N._T",filter=list(startPeriod=startPeriod,endPeriod=endPeriod))


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
                  toFrame(hh_iv_ivShares),toFrame(hh_mfi_ivShares)
)

banksData<-rbind(toFrame2(iv_assets),toFrame2(nfc_assets),toFrame2(mfi_assets),toFrame2(ofi_assets),toFrame2(icpf_assets),toFrame2(hh_assets),toFrame2(govt_assets))
liab<-rbind(toFrame2(iv_liabilities),toFrame2(nfc_liabilities),toFrame2(mfi_liabilities),toFrame2(ofi_liabilities),toFrame2(icpf_liabilities),toFrame2(hh_liabilities),toFrame2(govt_liabilities))
banksData<-cbind(banksData,liab$value)
banksData$value<-round(banksData$value/1000,2)
names(banksData)[4]<-"Assets"
names(banksData)[5]<-"Liabilities"

total<-ddply(matrixData,.(source,target,time),summarize,value=sum(value))
total$type="total"
matrixData<-rbind(matrixData,total)
matrixData$value<-round(matrixData$value/1000,2)

dimensions<-unique(matrixData[,3])
time<-unique(matrixData[,4])

#create directories
#ONLY IF NOT THERE YET
setwd("./Data/Germany")
for(i in 1:length(dimensions)) {
  str <- toString(dimensions[i])
  dir.create(str)  
}

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
    for (i in 2:length(names(matrix))) {
      names(matrix)[i]<-toString(paste("_",names(matrix[i]),sep=""))
    }
    
    file<-paste("matrix ",byTimeByAssetData$time[1],".csv",sep="")
    write.csv(matrix,file,row.names=FALSE, quote=FALSE)
    
    #node files
    byTimeBanksData <- banksData[which(banksData$time==time[i]),]
    
    file<-paste("banks ",byTimeByAssetData$time[1],".csv",sep="")
    write.csv(byTimeBanksData,file,row.names=FALSE, quote=FALSE)
  }
  setwd("..")
}




