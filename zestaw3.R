library(tidyverse)
library(DescTools) #funkcja Mode(v), przewodnik po pakiecie R / code academy


koszt_duze=c(9.2, 9.8, 8.2, 11.2, 9.1, 10.8, 8.6, 13.5, 11.9, 11.0, 6.1, 8.7, 10.5, 6.5, 11.4, 7.9, 10.4, 11.9, 10.8, 11.4, 11.2, 9.7, 9.7, 10.2, 11.2, 5.9, 9.5, 8.8, 13.6, 8.1, 11.7, 6.2, 10.6, 10.7, 6.3, 9.3, 9.7, 7.9, 11.5, 10.5, 9.7, 11.4, 9.6, 8.4, 11.5, 9.6, 9.8, 10.2, 14.6, 16.8, 10.2, 11.5, 13.1, 9.1, 12.3, 10.2, 11.5, 8.5, 8.6, 11.3)
koszt_male=c(7.3, 5.3, 5.6, 7.7, 8.7, 6.3, 9.5, 7.0, 8.9, 8.0, 3.8, 7.6, 6.1, 6.2, 7.6, 8.9, 4.5, 3.2, 7.5, 5.6, 3.8, 6.1, 7.3, 8.4, 3.3, 7.6, 7.5, 7.0, 7.3, 6.1, 8.1, 12.0, 8.6, 5.7, 5.5, 6.1, 7.3, 3.9, 7.2, 8.7, 7.1, 5.8, 8.5, 9.9, 4.4, 3.7, 7.1, 5.7, 8.5, 5.6, 11.0, 8.9, 10.0, 9.7, 9.8, 4.7, 6.8, 8.6, 9.1, 12.3, 6.2, 3.8, 5.6, 3.6, 7.7, 3.4, 3.8, 8.4, 5.2, 7.5, 6.7, 5.8, 6.7, 6.6, 7.4)
koszt_wszystkie=c(koszt_duze,koszt_male)

a=c(1,2,4,4,5,5,5)

#A - duze
#średnia
śr_duze=mean(koszt_duze)
#10.15
war_duze_beze=var(koszt_duze)*(length(koszt_duze)-2)/length(koszt_duze)
#4.01
median(koszt_duze)
#10.2
Mode(koszt_duze)
#9.7, 10.2, 11.5 (4 razy)

#A - male
śr_male=mean(koszt_male)
war_male_beze=var(koszt_male)*(length(koszt_male)-2)/length(koszt_male)
median(koszt_male)
Mode(koszt_male)

#A - wszystkie
śr_wszystkie=mean(koszt_wszystkie)
#8.351852
#war_duze=sum((koszt_wszystkie-śr_wszystkie))^2/length(koszt_wszystkie)#BŁĄD!!
war_wszystkie_beze=var(koszt_wszystkie)*(length(koszt_wszystkie)-2)/length(koszt_wszystkie)
#6.634245
median(koszt_wszystkie)
#8.5
Mode(koszt_wszystkie)
#5

#B
#DUŻE!!!
n=length(koszt_duze)
n
round(sqrt(n)) 
#wyszło 8, weźmy np. 8
k=8
a=0.01 #to alfa
mi=min(koszt_duze)
mi
ma=max(koszt_duze)
ma
R=ma-mi
R
#dlugość przedziału
b=ceiling((R+a/2)/(k*a))*a
b
mi-a/2+k*b #oh, nie wiem co to
#wektor krańca przedziałów
kr=mi-a/2+b*c(0:k)
kr

#HISTOGRAM
h=hist(koszt_duze, breaks=kr, ylim=c(0,20), ylab="Liczebność", xlab="Koszt", main="Histogram kosztu 1m^2 w dużym mieście", col="#69b3a2")

#SZEREG ROZDZIELCZY
przedział=1:k
lewy_kr=kr[1:k]
prawy_kr=kr[2:(k+1)]
lewy_kr
prawy_kr

#liczebność
liczebność=h$counts

Szereg_rozdzielczy_duze=as.data.frame(cbind(przedział,lewy_kr,prawy_kr,liczebność))
Szereg_rozdzielczy_duze

write.csv(Szereg_rozdzielczy_duze, "szereg_duze.csv", row.names = FALSE, fileEncoding = "UTF-8")

#MAŁE!!!!
n=length(koszt_male)
n
round(sqrt(n)) 
#wyszło 9, weźmy np. 10
k=10
a=0.01 #to alfa
mi=min(koszt_male)
mi
ma=max(koszt_male)
ma
R=ma-mi
R
#dlugość przedziału
b=ceiling((R+a/2)/(k*a))*a
b
mi-a/2+k*b #oh, nie wiem co to
#wektor krańca przedziałów
kr=mi-a/2+b*c(0:k)
kr

#HISTOGRAM
h=hist(koszt_male, breaks=kr, ylim=c(0,20), ylab="Liczebność", xlab="Koszt", main="Histogram kosztu 1m^2 w małym mieście", col="#69b3a2")

#SZEREG ROZDZIELCZY
przedział=1:k
lewy_kr=kr[1:k]
prawy_kr=kr[2:(k+1)]
lewy_kr
prawy_kr

#liczebność
liczebność=h$counts

Szereg_rozdzielczy_male=as.data.frame(cbind(przedział,lewy_kr,prawy_kr,liczebność))
Szereg_rozdzielczy_male

write.csv(Szereg_rozdzielczy_male, "szereg_male.csv", row.names = FALSE, fileEncoding = "UTF-8")

#ggplot histogram
library(hrbrthemes)
x<-as.data.frame(koszt_male)
ggh=ggplot(x, aes(x=koszt_male)) + 
  geom_histogram(breaks=kr, fill="#69b3a2", color="black")+
  ylim(0,20)+
  scale_x_continuous(breaks=seq(4,12, by=2))+
  labs(
    title="Histogram kosztu 1m^2 w małym mieście",
    x="Koszt", y="Liczebność"
  )+
  theme_ipsum() +
  theme(
    plot.title = element_text(size=15)
  )
ggh

#WSZSYTKIE!!!
n=length(koszt_wszystkie)
n
round(sqrt(n)) 
#wyszło 12, weźmy np. 11
k=11
a=0.01 #to alfa
mi=min(koszt_wszystkie)
mi
ma=max(koszt_wszystkie)
ma
R=ma-mi
R
#dlugość przedziału
b=ceiling((R+a/2)/(k*a))*a
b
mi-a/2+k*b #oh, nie wiem co to
#wektor krańca przedziałów
kr=mi-a/2+b*c(0:k)
kr

#HISTOGRAM
h=hist(koszt_wszystkie, breaks=kr, ylim=c(0,25), ylab="Liczebność", xlab="Koszt", main="Histogram kosztu 1m^2 we wszystkich\n badanych miastach", col="#69b3a2")

#SZEREG ROZDZIELCZY
przedział=1:k
lewy_kr=kr[1:k]
prawy_kr=kr[2:(k+1)]
lewy_kr
prawy_kr

#liczebność
liczebność=h$counts

Szereg_rozdzielczy_wszystkie=as.data.frame(cbind(przedział,lewy_kr,prawy_kr,liczebność))
Szereg_rozdzielczy_wszystkie

write.csv(Szereg_rozdzielczy_wszystkie, "szereg_wszystkie.csv", row.names = FALSE, fileEncoding = "UTF-8")
#C

#WSZYSTKIE
środki_przedziałów=(lewy_kr+prawy_kr)/2

śr_szer=1/n*sum(liczebność*środki_przedziałów)
#8.398407
var_szer=1/n*sum(liczebność*(środki_przedziałów-śr_szer)^2)
#6.657787
odch_szer=sqrt(var_szer)
#2.580269


#DOMINANTA
nM=ceiling((n+1)/2)

#l to dominanta
l=min(which(nM<=cumsum(liczebność)))
l
which(nM<=cumsum(liczebność))

#MEDIANA
Med_szer=lewy_kr+b/liczebność[l]*(n/2-cumsum(liczebność)[l-1])
Med_szer
#3.437609  4.677609  5.917609  7.157609  8.397609  9.637609 10.877609 12.117609 13.357609 14.597609 15.837609


N <- sum(liczebność)

# Znajdź przedział medianowy
N2 <- N / 2
F <- cumsum(liczebność) # Suma skumulowana liczebności
przedział_medianowy <- which(F >= N2)[1]

# Dane dla mediany
L <- lewy_kr[przedział_medianowy] # Dolna granica przedziału medianowego
F_below <- ifelse(przedział_medianowy == 1, 0, F[przedział_medianowy - 1]) # Suma skumulowana przed medianowym
f <- liczebność[przedział_medianowy] # Liczebność w przedziale medianowym
w <- prawy_kr[przedział_medianowy] - lewy_kr[przedział_medianowy] # Szerokość przedziału

# Obliczenie mediany
mediana <- L + ((N2 - F_below) / f) * w
mediana

#DOMINANTA
dominantowy_przedział <- which.max(liczebność)
L <- lewy_kr[dominantowy_przedział] # Dolna granica przedziału dominantowego
f_d <- liczebność[dominantowy_przedział] # Liczebność przedziału dominantowego

# Liczebności sąsiednich przedziałów
f_d_minus1 <- ifelse(dominantowy_przedział == 1, 0, liczebność[dominantowy_przedział - 1])
f_d_plus1 <- ifelse(dominantowy_przedział == k, 0, liczebność[dominantowy_przedział + 1])

# Szerokość przedziału
w <- prawy_kr[dominantowy_przedział] - lewy_kr[dominantowy_przedział]

# Obliczenie dominanty
dominanta <- L + ((f_d - f_d_minus1) / ((f_d - f_d_minus1) + (f_d - f_d_plus1))) * w
dominanta

