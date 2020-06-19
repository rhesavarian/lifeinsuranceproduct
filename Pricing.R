library(lifecontingencies)

# Life Table ----
TMIIVLt <- new("lifetable", x=lx_table$Usia, lx=lx_table$lx,name="TMI IV lifetable") #convert ke lifetable di R

# Actuarial Table dengan i yg telah ditentukan ----
TMIIVAct <- new("actuarialtable",x=TMIIVLt@x, lx=TMIIVLt@lx, interest=0.06, name="TMI IV actuarialtable")

KomisiBerkala <- c(KomisiBerkala_table$Komisi)
KomisiLanjutan <- c(KomisiLanjutan_table$KomisiLanjutan)

# Data dan Asumsi ----
usia1 <- (20:55) #Usia Tertanggung
masa1 <- c(5, 10, 15) #Masa Pembayaran Premi untuk Pembayaran p=remi berkala
akus <- 0.02 #Biaya akusisi, bergantung UP
admin <- 0.003 #Biaya administrasi, bergantung UP
komisi <- 0.05 #Komisi premi sekaligus, bergantung Premi
pena <- 0.05 #Biaya penagihan premi, bergantung Premi
usiaakhir <- 75 #Usia tertanggung saat endowment berakhir
tabelpremisekaligus <- rep(0, times=length(usia1)) #Vector kosong, untuk diisi berikutnya
tabelpremiberkala<-data.frame(matrix(NA,nrow=length(usia1),ncol=length(masa1))) #dataframe kosong, untuk diisi berikutnya

# Perhitungan premi sekaligus ----
for(j in usia1){
  tabelpremisekaligus[j-min(usia1)+1] <- (AExn(TMIIVAct,x=j,n=usiaakhir-j)+akus+admin*axn(TMIIVAct, x=j, n=usiaakhir-j,payment="due"))/(1-komisi)
}

tabelpremisekaligus <- matrix(1000*tabelpremisekaligus, nrow = length(usia1))
colnames(tabelpremisekaligus) <- "tarif"
rownames(tabelpremisekaligus) <- c(usia1)

# Perhitungan premi berkala ----
for (i in masa1){
  for(j in usia1){
    tabelpremiberkala[match(j,usia1),match(i,masa1)]<- 1000*(AExn(TMIIVAct,x=j,n=usiaakhir-j)+akus+admin*axn(TMIIVAct, x=j, n=usiaakhir-j,payment="due"))/
        (axn(TMIIVAct, x=j, n=i,payment="due")*(1-pena)-KomisiBerkala[i]*(1+KomisiLanjutan[2]*Exn(TMIIVAct,x=j,n=1)+KomisiLanjutan[3]*Exn(TMIIVAct,x=j,n=2)))
  }
}


# Tampilkan Tabel ----
colnames(tabelpremiberkala) <- c(masa1)
rownames(tabelpremiberkala) <- c(usia1)

tabelpremisekaligus
tabelpremiberkala