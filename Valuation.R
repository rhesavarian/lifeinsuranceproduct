start_time<-Sys.time()
# Asumsi ----
valdate <- as.Date("2020-07-01")
qx_fact<-1
exp_fact<-1
ival <- 0.06
infl <- 0
akus <- 0.02 #Biaya akusisi, bergantung UP
admin <- 0.003 #Biaya administrasi, bergantung UP
komisi <- 0.05 #Komisi premi sekaligus, bergantung Premi
pena <- 0.05 #Biaya penagihan premi, bergantung Premi
usiaakhir <- 75 #Usia tertanggung saat endowment berakhir

# Olah Asumsi ----
library(lifecontingencies)
ivalM <- (1+ival)^(1/12)-1
vvalM <- 1/(1+ivalM)
dvalM <- ivalM/(1+ivalM)
inflM <- (1+infl)^(1/12)-1
KomisiBerkala <- c(KomisiBerkala_table$Komisi)
KomisiLanjutan <- c(KomisiLanjutan_table$KomisiLanjutan)
Cad <- rep(c(0),times=length(DaftarPolis$No.Polis))
ivalM2 <- ((1+ival)/(1+infl))^(1/12)-1
vvalM2 <- 1/(1+ivalM2)

# Membuat qx bulanan ----
qxbulanan<-qx_fact*head(rep(1-(1-lx_table$qx)^(1/12), each=12),-11)

# Membuat lx dan dx bulanan----
dxbulanan<-rep(0,length(qxbulanan))
lxbulanan<-append(c(100000),rep(0,length(qxbulanan)-1))
for(i in 2:length(dxbulanan)){
  dxbulanan[i-1]=lxbulanan[i-1]*qxbulanan[i-1]
  lxbulanan[i]=lxbulanan[i-1]-dxbulanan[i-1]
} 
dxbulanan<-append(dxbulanan,lxbulanan[length(lxbulanan)])

# Membuat tabel aktuaria1 ----
act<-data.frame(0:(length(lxbulanan)-1))
colnames(act)[1]<-"x"

act$lx<-lxbulanan

act$Dx<-vvalM^(act$x)*act$lx

act$Nx[1]<-sum(act$Dx)
for(i in 2:length(act$x)){
  act$Nx[i]<-act$Nx[i-1]-act$Dx[i-1]
}

for(i in 1:(length(act$x)-1)){
  act$Cx[i]<-vvalM^(act$x[i]+1)*(act$lx[i]-act$lx[i+1])
}
act$Cx[length(act$x)]<-vvalM^(act$x[length(act$x)]+1)*(act$lx[length(act$x)])

act$Mx[1]<-sum(act$Cx)
for(i in 2:length(act$x)){
  act$Mx[i]<-act$Mx[i-1]-act$Cx[i-1]
}

act$Rx[1]<-sum(act$Mx)
for(i in 2:length(act$x)){
  act$Rx[i]<-act$Rx[i-1]-act$Mx[i-1]
}

# Membuat tabel aktuaria2 ----
act2<-data.frame(0:(length(lxbulanan)-1))
colnames(act2)[1]<-"x"

act2$lx<-lxbulanan

act2$Dx<-vvalM2^(act2$x)*act2$lx

act2$Nx[1]<-sum(act2$Dx)
for(i in 2:length(act2$x)){
  act2$Nx[i]<-act2$Nx[i-1]-act2$Dx[i-1]
}

for(i in 1:(length(act2$x)-1)){
  act2$Cx[i]<-vvalM2^(act2$x[i]+1)*(act2$lx[i]-act2$lx[i+1])
}
act2$Cx[length(act2$x)]<-vvalM2^(act2$x[length(act2$x)]+1)*(act2$lx[length(act2$x)])

act2$Mx[1]<-sum(act2$Cx)
for(i in 2:length(act2$x)){
  act2$Mx[i]<-act2$Mx[i-1]-act2$Cx[i-1]
}

act2$Rx[1]<-sum(act2$Mx)
for(i in 2:length(act2$x)){
  act2$Rx[i]<-act2$Rx[i-1]-act2$Mx[i-1]
}

# Membuat fungsi ----
TERM<-function(func,x,n){
  (func$Mx[x+1]-func$Mx[x+n+1])/func$Dx[x+1]
}
ENDP<-function(func,x,n){
  func$Dx[x+n+1]/func$Dx[x+1]
}
ENDO<-function(func,x,n){
  func$Dx[x+n+1]/func$Dx[x+1]+(func$Mx[x+1]-func$Mx[x+n+1])/func$Dx[x+1]
}
ANND<-function(func,x,n){
  (func$Nx[x+1]-func$Nx[x+n+1])/func$Dx[x+1]
}

# Membuat vector bunga valuasi1 ----
ivalbulanan<-rep(0,length(qxbulanan))
for(i in 1:length(qxbulanan)){
  ivalbulanan[i]<-(1+ivalM)^i
}

# Membuat vector bunga valuasi2 ----
ivalbulanan2<-rep(0,length(qxbulanan))
for(i in 1:length(qxbulanan)){
  ivalbulanan2[i]<-(1+ivalM2)^i
}

# perhitungan GPV----
for(i in 1:length(DaftarPolis$No.Polis)){
  usiaT<-DaftarPolis$Usia.Tertanggung[i]
  maT<-DaftarPolis$Masa.Asuransi[i]
  mppT<-DaftarPolis$Masa.Pembayaran.Premi[i]
  UPT<- DaftarPolis$UP[i]
  PremiT<- DaftarPolis$Premi[i]
  Jenis.Pembayaran<- DaftarPolis$Jenis.Pembayaran[i]
  sdateT<-as.Date(DaftarPolis$Mulai.Asuransi[i])
  valT<- length(seq(from=sdateT, to=valdate, by='month')) - 1
  timeidT <- seq(from=1, to=maT*12-valT, by=1)
  
  # lx ----
  lxbulananTotal<-lxbulanan[-1:-(usiaT*12)]/lxbulanan[usiaT*12+1]
  lxbulananTotal<-lxbulananTotal[1:(maT*12)]
  lxbulanan1<-lxbulananTotal[-1:-valT]
  lxbulanan2<-lxbulanan1/lxbulanan1[1]
  
  # Manfaat Expirasi ----
  pvexpirasiT<-ENDP(act,x=usiaT*12+valT, n=maT*12-valT)*UPT
   
  # Cashflow Premi ----
  if(Jenis.Pembayaran==1){
    cfpremi1 <- append (c(PremiT),rep(0,times=maT*12-1))
  } else if(Jenis.Pembayaran==2){
    cfpremi1 <- append(rep(c(PremiT,0,0,0,0,0,0,0,0,0,0,0), times=mppT),rep(c(0),times=((maT-mppT)*12)))
  } else if(Jenis.Pembayaran==3){
    cfpremi1 <- append (rep(c(PremiT,0,0,0,0,0), times=mppT*2),rep(c(0),times=((maT-mppT)*12)))
  } else if(Jenis.Pembayaran==4){
    cfpremi1 <- append (rep(c(PremiT,0,0), times=mppT*4),rep(c(0),times=((maT-mppT)*12)))
  } else {
    cfpremi1 <- append (rep(c(PremiT), times=mppT*12),rep(c(0),times=((maT-mppT)*12)))
  } 
  cfpremi<-cfpremi1[-1:-valT]
  pvpremi<-presentValue(cashFlows = cfpremi, timeIds = timeidT-1, interestRates = ivalM, probabilities = lxbulanan2)
  
  # Cashflow Manfaat Kematian----
  pvkematian<-TERM(act,x=usiaT*12+valT, n=maT*12-valT)*UPT
  
  # Cashflow Administrasi ----
  pvadmin<-ANND(act2,x=usiaT*12+valT, n=maT*12-valT)*UPT*admin/12
  
  # Cashflow Penagihan ----
  pvpenagihan<-pvpremi*pena
  
  # Cashflow Komisi ----
  if(DaftarPolis$Jenis.Pembayaran[i]==0){
    cfkomisi1 <- nnappend(c(komisi), rep(c(0),times=12*maT-1))
    cfkomisi <- cfkomisi1[-1:-valT]
    cfkomisi <- cfkomisi*cfpremi
  } else{
    cfkomisi1 <- append(rep(KomisiBerkala[mppT]*KomisiLanjutan,each=12),rep(c(0),times=12*maT-length(KomisiLanjutan)*12))
    cfkomisi2 <- cfkomisi1[-1:-valT]
    cfkomisi <- cfkomisi2*cfpremi
  }
  pvkomisi<-presentValue(cashFlows = cfkomisi, timeIds = timeidT-1, interestRates = ivalM, probabilities = lxbulanan2)
  
  # GPV ----
  cadT<-pvkematian+pvexpirasiT+pvadmin+pvkomisi+pvpenagihan-pvpremi
  Cad[i]<-cadT

}
# Tampilkan GPV ----
DaftarPolis$GPV<-Cad
Cad
end_time<-Sys.time()
end_time - start_time