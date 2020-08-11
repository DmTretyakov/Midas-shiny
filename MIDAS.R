library(dplyr)
library(openxlsx)
library(ggplot2)
library(ggpubr)
library(lubridate)
library(seasonal)
library(readxl)
setwd("C:/Users/Дима/Desktop/MIDAS_очищенный_от_сезонности/с 2010")


#Загрузка

#инфляция

df_inflation <- read.xlsx("Инфляция.xlsx", colNames = T, sheet = 2)
df_inflation[,1] <- seq(as.Date("01/01/99",format = "%m/%d/%y"), by = "month",
                        length = nrow(df_inflation) )
df_inflation[,2] <- NULL
colnames(df_inflation) <- c("date", "inflation")
# Чистим от сезонности
ts = ts(df_inflation$inflation,start = c(1999,1), end = c(2019,11), frequency = 12) 
final = final(seas(ts))


df_inflation$inflation <- as.data.frame(final)

#df_inflation <- df_inflation[which(df_inflation$date == '2009-12-01'):nrow(df_inflation),]
dat <- df_inflation$date[2:nrow(df_inflation)]
x <- log(df_inflation$inflation)
y <- diff(x$x)

df_inflation <- data.frame('date' = dat, 'inflation' = y)



rm(dat)
rm(final)
rm(ts)
rm(y)
rm(x)



#ИЦП
icp <- read.xlsx("sophist_ИЦП.xlsx", colNames = F)
icp[,1] <- seq(as.Date("01/01/99",format = "%m/%d/%y"), by = "month",
               length = nrow(icp) )
colnames(icp) <- c("date", "icp")


ts = ts(icp$icp,start = c(1999,1), end = c(2019,11), frequency = 12) 
final = final(seas(ts))


icp$icp <- as.data.frame(final)

icp <- icp[which(icp$date == '2009-12-01'):nrow(icp),]
dat <- icp$date[2:nrow(icp)]
x <- log(icp$icp)
y <- diff(x$x)

icp <- data.frame('date' = dat, 'icp' = y)



rm(dat)
rm(final)
rm(ts)
rm(y)
rm(x)



#Обменный курс курс
ner <- read.xlsx("RC_F01_01_1999_T31_12_2019.xlsx", colNames = F )

colnames(ner) <- c('date', 'usd')

ner$date <- as.Date(ner$date, origin = '1899-12-30')


# miacr
mia <- read.xlsx("miacr.xlsx", colNames = F )

colnames(mia) <- c('date', 'mia')

mia$date <- as.Date(mia$date, origin = '1899-12-30')
mia$date <- rev(mia$date)
mia$mia <- rev(mia$mia)

#brent
brent <- read_xls("DCOILBRENTEU.xls",col_names = F)
names(brent) <- c('date', 'brent')
brent <- na.omit(brent)
brent <- as.data.frame(brent)



# Функция для перевода дневных данных в недельные
f<- function(df){
  s <- 2010
  ss <- c('01', '02', '03', '04', '05', '06', '07', '08', '09', '10', '11', '12')
  v <- numeric(10*12*4)
  for (i in 1:11){
    for(j in 1:12){
      a <- df[,2][format(df$date,format="%Y") == as.character(s) 
                  & format(df$date,format="%m") == ss[j]]
      v[(length(v[v!=0]) + 1):(length(v[v!=0]) + 4)] <- 
        c( mean(a[1:5],na.rm = T),
           mean(a[6:10],na.rm = T),
           mean(a[11:15],na.rm = T),
           mean(a[length(a)],na.rm = T) )
    }
    s <- s +1
    print(s)
  }
  return(v)
}

pr <- data.frame()


a <- f(ner)
pr[1:nrow(as.data.frame(na.omit(a))),1] <- as.data.frame(na.omit(a))
names(pr) <- 'ner'

a <- f(mia)
pr$mia <- as.data.frame(na.omit(a))

a <- f(brent)
pr$brent <- as.data.frame(na.omit(a))

colnames(pr) <- c('ner','mia','brent')

rm(f)
rm(a)
rm(ner)
rm(mia)
rm(brent)




X_ = pr
rm(pr)
library(midasr)



gs <- data.frame() #### с 2010 и минус 1 день для дифф - - -  до июня 2019(не вкл)
# цифры из wk для прошлой недели для диффа


#Заполняем предыдущими значениями для того чтобы вычыслить приросты
#и записываем всё в генеральную совокупность признаков gs
gs[1,1] <- 29.91473
gs[2:(nrow(X_)+1),1] <- X_$ner

gs[1,2] <- 3.8
gs[2:(nrow(X_)+1),2] <- X_$mia

gs[1,3] <- 80
gs[2:(nrow(X_)+1),3] <- X_$brent


names(gs) <- c("usd", "mia",'brent')

rm(X_)

a <- gs
a <- log(a)
a <- sapply(a, diff)
a <- as.data.frame(a)
############################################################


a$mia <- diff(gs$mia)
########################CLEARRRRRRRRRRRRRRRRRRRRRRRRRRRR
rm(gs)

X_ <- a
rm(a)


#Выделяем таргет инфляции и её лаг
gs_inf <- df_inflation[which(df_inflation$date == '2010-01-01'):nrow(df_inflation),]
lag_inf <- df_inflation[(which(df_inflation$date == '2010-01-01') - 1):(nrow(df_inflation)),]
row.names(gs_inf) <- 1:nrow(gs_inf)
row.names(lag_inf) <- 1:nrow(lag_inf)








usd_train <- X_$usd
mia_train <- X_$mia
brent_train <- X_$brent
ylag_train <- lag_inf$inf



ex <- X_[385:nrow(X_),]
us_3_tes <- ex$usd
mi_3_tes <- ex$mia
br_3_tes <- ex$brent





m <- 384
mm <- 96
hor = nrow(gs_inf) - 96
nn <- nrow(gs_inf)


t <- 1
nowcast <- numeric(hor)
lo <- numeric(hor)
hi <- numeric(hor)
forecast1 <- numeric(hor)
forecast2 <- numeric(hor)


#Строим прогнозы и вычисляем ошибку
for (i in 1:hor){
  ylag <- ylag_train[1:mm]
  ipc <- icp_train[1:mm]
  usd <- usd_train[1:m]
  mia <- mia_train[1:m]
  brent <- brent_train[1:m]
  y <- gs_inf[1:mm,]$inf
  
  um <- midas_r(y ~ mls(ylag, 0:0, m = 1) +
                  mls(usd, 0:10 , m =4,lcauchyp) 
                + mls(mia,0:5, m =4, nbeta)+
                  mls(brent, 0:3, m = 4, almonp)
                ,start = list(usd = rep(1,3),
                              mia = rep(1,3),
                              brent = c(1,1) ) )
  um_fore <- forecast(um,newdata = list(usd = us_3_tes[t:(t+3)],
                                        mia = mi_3_tes[t:(t+3)],
                                        brent = br_3_tes[t:(t+3)],
                                        ylag = lag_inf[(mm+1),]$inf,
                                        ipc =icp_train[(mm+1)])
                      ,se = TRUE, level = c(68))
  nowcast[i] <- um_fore[[4]][1]
  nowcast[i] <- um_fore[[4]]
  lo[i] <- um_fore$lower
  hi[i] <- um_fore$upper
  
  um_fore <- forecast(um, newdata = list(usd = c(us_3_tes[t:(t+3)],rep(0,4)),
                                         mia = c(mi_3_tes[t:(t+3)], rep(0,4)),
                                         brent = c(br_3_tes[t:(t+3)], rep(0,4)),
                                         ylag = c(lag_inf[(mm+1),]$inf,nowcast[i])
                                         ))
  
  forecast1[i] <- um_fore[[4]][2]
  um_fore <- forecast(um, newdata = list(usd = c(us_3_tes[t:(t+3)],rep(0,8)),
                                         mia = c(mi_3_tes[t:(t+3)], rep(0,8)),
                                         brent = c(br_3_tes[t:(t+3)], rep(0,8)),
                                         ylag = c(lag_inf[(mm+1),]$inf,nowcast[i],forecast1[i])
  ))
  
  forecast2[i] <- um_fore[[4]][3]
  m <- m+4
  mm <- mm+1
  t <- t+4
}



#Для графиков

f <- data.frame()
f[1:(nn-85+1),1] <- as.character(gs_inf$date[85:nn])
f$V1 <- as.Date(f$V1)
f$inf <- gs_inf$inf[85:nn]
f$lo <- c(rep(NA,nrow(f) - hor),lo)
f$nowcast <- c(rep(NA,nrow(f) - hor),nowcast)
f$hi <- c(rep(NA,nrow(f) - hor),hi)

ggplot(f,aes(V1,inf))+
  geom_ribbon(aes(ymin = lo, ymax = hi), fill = "red",alpha = 0.1)+
  geom_line(color = "blue",size = 1.7)+
  geom_point(color = "blue",size = 3.5)+
  geom_line(aes(V1,nowcast),color = "red",size = 1.7)+
  geom_point(aes(V1,nowcast),color = "red",size = 3.5)+
  theme_classic()+
  geom_line(aes(V1,lo),color = "red")+
  geom_line(aes(V1,hi),color = "red")+
  geom_point(aes(V1,lo),color = "red")+
  geom_point(aes(V1,hi),color = "red")+
  ggtitle("90% доверительный интервал")+
  xlab("")+
  ylab("инфляция")+
  theme(legend.title = element_blank(), 
        legend.position = "top", 
        legend.background = element_rect(fill = "black"),
        text = element_text(color = "black"), 
        panel.grid.major.y = element_line(color = "gray50",
                                          linetype = "longdash", 
                                          size = 0.3), 
        axis.text.x = element_text(face = "bold", size = 30), 
        axis.text.y = element_text(face = "bold", size = 30), 
        legend.text = element_text(size = 30), 
        title = element_text(face = "bold", size = 30))



summary(um)
#RMSE
(sum((nowcast-gs_inf$inf[97:nn])^2)/(nn-97+1))^(1/2)
(sum((forecast1[-length(forecast1)]-gs_inf$inf[98:nn])^2)/(nn-97))^(1/2)
(sum((forecast2[-c(length(forecast2),length(forecast2)-1)]-gs_inf$inf[99:nn])^2)/(nn-98))^(1/2)
BIC(um)



#Для графиков
f <- data.frame()
f[1:37,1] <- as.character(c(gs_inf$date[85:119],as.Date(gs_inf$date[119]+30),
                            (gs_inf$date[119]+61) ) )
f$V1 <- as.Date(f$V1)
f$inf <- c(gs_inf$inf[85:119],NA,NA)
f$nowcast <- c(rep(NA,12),nowcast,NA,NA)
f$forec1 <- c(rep(NA,13),forecast1,NA)
f$forec2 <- c(rep(NA,14),forecast2)

v <- numeric(23*3)
t = 1
for( i in 13:35){
  v[t:(t+2)] <- c(f$nowcast[i],f$forec1[i+1],f$forec2[i+2])
  t <- t+3
}

d <- numeric(23*3)
t = 1
for ( i in 13:35){
  d[t:(t+2)] <- c(as.character(f$V1[i]),as.character(f$V1[i+1]),as.character(f$V1[i+2]))
  t <- t+3
}

fac1 <- numeric(23*3)
t = 1
for(i in 1:25){
  fac1[t:(t+2)] <- rep(i,3)
  t <- t+3
}

fac2 <- numeric(23*3)
t = 1
fac2r <- c(0,1,2)
for(i in 1:25){
  fac2[t:(t+2)] <- fac2r
  t <- t+3
}

pl <- data.frame()
pl[1:length(v),1] <- numeric(length(v))
pl$date <- as.Date(d)
pl$val <- v
pl$f1 <- fac1[1:69]
pl$f2 <- fac2[1:69]



#pl <- na.omit(pl)


#pl[50:61,] <- f[1:12,-2]
#pl <- pl[-49.1,]

aa <- ggplot(pl,aes(date,val,group = as.factor(f1) ) )+
  geom_line()

ggplot(f,aes(V1,inf))+
  geom_line(color = "blue",size = 3,alpha = 0.2)+
  geom_point(color = "blue",size = 3.5)+
  geom_line(pl,mapping = aes(date,val,group = as.factor(f1) ),color = "red")+
  geom_point(aes(V1,nowcast),size = 4.5,color = "red")+
  geom_point(aes(V1,forec1), shape = 15,size = 4.5,color = "red")+
  geom_point(aes(V1,forec2), shape= 8,size = 4.5,color = "red")+
  ggtitle("прогноз на 1 и 2 месяца")+
  xlab('')+
  ylab("Инфляция")+
  theme_classic()+
  theme(legend.title = element_blank(), 
        legend.position = "top", 
        legend.background = element_rect(fill = "black"),
        text = element_text(color = "black"), 
        panel.grid.major.y = element_line(color = "gray50",
                                          linetype = "longdash", 
                                          size = 0.3), 
        axis.text.x = element_text(face = "bold", size = 20), 
        axis.text.y = element_text(face = "bold", size = 20), 
        legend.text = element_text(size = 20), 
        title = element_text(face = "bold", size = 20))





#перевод



df_inflation <- read.xlsx("Инфляция.xlsx", colNames = T, sheet = 2)
df_inflation[,1] <- seq(as.Date("01/01/99",format = "%m/%d/%y"), by = "month",
                        length = nrow(df_inflation) )
df_inflation[,2] <- NULL
colnames(df_inflation) <- c("date", "inflation")

ts = ts(df_inflation$inflation,start = c(1999,1), end = c(2019,11), frequency = 12) 
final = final(seas(ts))


df_inflation$inflation <- as.data.frame(final)

#df_inflation <- df_inflation[which(df_inflation$date == '2009-12-01'):nrow(df_inflation),]
dat <- df_inflation$date[2:nrow(df_inflation)]
df_inflation$inflation <- log(df_inflation$inflation)






rr <- df_inflation$inflation[1:250,1]
n <- length(gs_inf$inflation)

dd <- rr[(length(rr)-118):length(rr)]
xx <- dd + gs_inf$inflation

per <- exp(xx)


plot(per)

ff <- numeric(107)

for(i in 1:107){
  ff[i] <- per[12+i]/per[i] - 1
}

pred <- nowcast + dd[(length(dd)-22):length(dd)]
pred <- exp(pred)


nowcast_12_lag <- numeric(23)
for(i in 1:23){
  nowcast_12_lag[i] <- pred[i]/per[(length(per) - 21 - 14 + i)] - 1
}

pred <- forecast1[-length(forecast1)] + dd[(length(dd)-21):length(dd)]
pred <- exp(pred)

forecast1_12_lag <- numeric(22)
for(i in 1:22){
  forecast1_12_lag[i] <- pred[i]/per[(length(per) - 21 - 14 + i)] - 1
}

pred <- forecast2[-c(length(forecast1),(length(forecast1)-1))] + dd[(length(dd)-20):length(dd)]
pred <- exp(pred)

forecast2_12_lag <- numeric(21)
for(i in 1:21){
  forecast2_12_lag[i] <- pred[i]/per[(length(per) - 21 - 14 + i)] - 1
}



plot(ff,type = 'l')
lines( c(rep(NA,84),nowcast_12_lag), col = 'red' )


plot(ff[80:107],type = 'l')
lines( c(rep(NA,5),nowcast_12_lag), col = 'red' )
lines( c( rep(NA,6), forecast1_12_lag), col = 'blue')
lines(c( rep(NA,7), forecast2_12_lag), col = 'green')


(sum((nowcast_12_lag - ff[85:107])^2)/(nn-97+1))^(1/2)
(sum((forecast1_12_lag - ff[86:107])^2)/(nn-97))^(1/2)
(sum((forecast2_12_lag - ff[87:107])^2)/(nn-98))^(1/2)
