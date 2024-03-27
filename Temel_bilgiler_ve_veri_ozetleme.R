#Temel Fonksiyonlar

getwd()

küp <- function(x) {
  return(x^3)
}
print(küp(5)) #küp alma fonksiyonu

#------------------------------------

geri_sayım <- function(a) {
  print(a)
  while(a != 0){
    Sys.sleep(1)
    a <- a-1
    
    print(a)
  }
}
geri_sayım(10) #sayaç fonksiyonu

#------------------------------------

veri= read.csv("https://johnmuschelli.com/intro_to_r/data/Youth_Tobacco_Survey_YTS_Data.csv")
View(veri)
head(veri)
dim(veri) #dimentions of data
nrow(veri)
help("dim")
install.packages("dplyr")
library(dplyr)
veri_rename= rename(veri, year=YEAR) #rename of year 

names(veri)
names(veri_rename)

install.packages("readr")
library(readr)

veri_rename_2=rename(veri_rename, YEAR=year)
write_csv(veri_rename_2, path= "/Users/aysinko/Desktop/olcme_ve_veri_analitigi_2/bil_524_veri_madenciligi/veri/veri_rename2.csv")#veri pcde olduğunda değiştirmeye yarar
getwd()

install.packages("tidyverse")
library(tidyverse)
veri2= data(mtcars)
View(df)
veri2=data.frame(mtcars)
dim(mtcars)
head(mtcars)
View(veri2)

veri2_tibble=as_tibble(veri2)
head(veri2_tibble)
veri2_rename = dplyr::rename(veri2_tibble, MPG=mpg)
veri2_rename2 = dplyr::rename_all(veri2_rename, MPG=mpg, toupper) #toupper harfleri büyük harf yapar
names(veri2)
veri2_gear <- veri2$gear
dim(veri2_gear)
x=c(1, 2, 3) #veri türü değiştirme
?'matrix-class'

#filtreleme fonksiyonları

veri2_mpg <- select(veri2, mpg)
dim(veri2_mpg)
veri2_mpg2 <- pull(select(veri2, mpg))
dim(veri2_mpg2)

veri2_mpg_fil = filter(veri2, mpg > 19 | mpg < 17 ) # "| veya"
veri2_gear = select(filter(veri2, mpg > 19 | mpg < 17), gear)
veri2_gear = select(veri2_mpg_fil, gear)

veri2_piped = veri2 %>% filter (mpg > 22 & cyl==4) %>% select(disp, qsec, vs) # "& ve"
veri2$new_column = veri2$wt*3 #yeni sütun eklemek
View(veri2)
#verinin orijinalini çağırmak
veri2=mtcars
View(veri2)

veri2_mut = mutate(veri2, newcol= wt*3)
View(veri2_mut)

veri2_mut2 = mutate(veri2, 
                    disp_kateg = ifelse(
                       disp <= 200,
                       "low",
                       ifelse(disp <=400,
                              "medium",
                              "high")))

#arrange

arrange(veri2, desc(disp)) #disp değerinin yüksekten düşüğe sıralaması

#transmute help(transmute)

transmute(veri2, newcol2= disp*5, mpg, gear)
