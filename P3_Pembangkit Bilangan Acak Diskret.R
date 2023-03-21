#PERTEMUAN 3
#PEMBANGKIT BILANGAN ACAK DISKRET

#VARIABEL RANDOM
#Pembangkitan dari Bilangan acak Bernoulli

binomial_sim<-function(n,p) {
  i<-1000
  n<-n
  p<-p
  Binom<-NULL
  for (z in 1:i){
    m<-0
    for (k in 1:n){
      y<-(runif(1)<=p)+0
      m<-m+y
    }
    Binom[z]<-m
  }
  (tabel<-table(Binom)/length(Binom))
  print (Binom)
}

binomial_sim(150,0.50)

#Pembakitan dari bilangan acak uniform
(tabel<-table(Binom)/length(Binom))
View(Binom)

#Pembangkitan dengan menggunakan fungsi di R
#fungsi R
x<-rbinom(16,4,0.5)
x

#VARIABEL RANDOM GEOMETRI
#Fungsi Inverse transformation method

#VR geometri
#transformation
i<-1000
p<-0.5
R<-runif(i)
X<-log(1-R)/log(1-p)
hist(X)

#input nilai p (peluang sukses)
i<-100
sebaran_geom<-function(p){
  R<-runif(i)
  X<-log(1-R)/log(1-p)
  print(X)
}
sebaran_geom(0.5)

#melalui sebaran bernouli
K<-1
p<-0.5
while(runif(1)>p)
  K=K+1;
K

#fungsi di R
#x~geomatrik(0.4) sebanyak 16 bilangan acak
x<-rgeom(16,0.4)
x

#VARAIABEL RANDOM BINOMIAL NEGATIF

#SEBARAN GEOMETRIK
#VR Binomikal negatif
#sebaran gemoetri
K<-1
p<-0.5
r<-3
R<-runif(i)
s<-0
while(s<r){
  if (runif(1)>p)
  {K=K+1;
  print=0
  }
  else
  {s=s+1;
  print=1}
}
K+r-1

#SEBARAN UNIFORM
n<-1000
u<-runif(i)
m<-5
p<-0.5
F<-pnbinom(1:20,size = m,p)
negative.binom<-NULL
for (i in 1:n) {
  negative.binom[i]<-min(which(u[i]<F))-1
}
table(negative.binom)

#fungsi di R
#x~negative binom(4,0.5)sebanyak 16 bil. acak
x<-rnbinom(16,4,0.5)
x

#VARIABEL RANDOM POISSON
#sebaran uniform
i<-100
lambda<-1
K<-NULL
for (z in 1:i) {
  k<-0
  sk<-1
  while(sk>=exp(-lambda)){
    u<-runif(1)
    sk<-sk*u
    k<-k+1
  }
  K[z]<-k
}
K
(tabel1<-table(K)/length(K))

#melalui sebaran exponensial
i<-100
lambda<-1
K<-NULL
for (z in 1:i) {
  sk<-0
  k<-0
  while (sk<=1){
    u<-runif(1)
    y<--log(u)/lambda
    sk<-y+sk
    k<-k+1
  }
  K[z]<-k-1
}
K
(tabel2<-table(K)/length(K))

#fungsi di R
#x~poisson(4) sebanyak 16 bilangan acak
x<-rpois(16,4)
x

