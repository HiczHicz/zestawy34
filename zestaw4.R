library(tidyverse)

wynagrodzenia=c(10.94, 4.85, 8.07, 6.98, 7.01, 6.54, 6.12, 6.05, 8.65, 6.49, 4.00, 4.00, 4.09, 6.26, 4.00, 5.79, 4.13, 4.91, 4.00, 4.29, 4.35)

#A

#wartości ekstremalne
xmin=min(wynagrodzenia) #4
xmax=max(wynagrodzenia) #10.94
R=xmax-xmin #6.94

#b

#kwartyle
quantile(wynagrodzenia)
#pierwszy: 4.13
#drugi (mediana): 5.79
#trzeci: 6.54

#c
#wykres pudełkowy

#d WYCIĄGNĄĆ WNIOSKI O ROZKŁADZIE:
#wszystkie dane w przedziale [4.00, 10.94]
#ok. 50% danych mieści się w przedziale [4.13, 6.54]
#ok 50% danych ma wartości poniżej 5.79
#ok 25% danych ma wartości poniżej 4.13 oraz ok 25% danych ma wartości powyżej 6.54

#d
#WYKRES PUDEŁKOWY
boxplot(wynagrodzenia, outline=FALSE, col="#69b3a2", main="Wykres pudełkowy wynagrodzenia", ylim=c(4,10), ylab="Wynagrodzenie (tys. zł)", xlab="Pracownicy")

#e
# rozstęp międzykwartylowy
q1=quantile(wynagrodzenia, prob=0.25)
q2=median(wynagrodzenia)
q3=quantile(wynagrodzenia, prob=0.75)
IRQ=q3-q1 #2.41

#odchylenie ćwiartkowe
Q=(q3-q1)/2 #1.205 czyli ok. 1.23

#kwartyl średni
Q_śr=(q1+q2)/2 #4.96

#pozycyjny współczynnik asymetrii:
Vasp=(q1+q3-2*q2)/(2*Q) #-0.3775934 czyli ok. -0.38

#kwartylny współczynnik asymetrii
VzQ=Q/q2*100 #20.81%


#ZADANIE 2
#KOBIETY
wynagrodzenie_k=c(5.88, 7.15, 5.72, 6.66, 5.76, 6.25, 7.55, 7.94, 4.18, 6.17, 4.81, 4.77, 5.50, 5.91,
                  6.02, 5.58, 3.88, 4.15, 4.92, 7.07, 5.88, 4.77, 6.15, 5.40, 6.98, 5.90, 4.52, 4.57,
                  6.11, 5.58, 6.94, 8.11, 8.06, 6.75, 8.23, 10.68, 6.86, 3.04, 4.92, 6.49, 3.66, 7.46,
                  9.65, 5.60, 6.63, 9.51, 7.15, 9.49, 4.78, 7.10, 5.86, 10.17, 5.47, 9.12, 8.51, 7.08,
                  7.02, 5.40, 8.18, 5.21, 5.57, 3.87, 7.19, 8.21, 10.13, 9.81, 5.32, 6.78, 7.66, 6.99,
                  10.46, 11.36, 9.51, 8.06, 3.27)
xmink=min(wynagrodzenie_k) #3.04
xmaxk=max(wynagrodzenie_k) #11.36
q1k=quantile(wynagrodzenie_k, prob=0.25) #5.44
q2k=median(wynagrodzenie_k) #6.49
q3k=quantile(wynagrodzenie_k, prob=0.75) #7.8

Qk=(q3k-q1k)/2
Vaspk=(q1k+q3k-2*q2k)/(2*Qk) #0.11
VzQk=Qk/q2k*100 #18.22%





wynagrodzenie_m=c(5.23, 7.38, 9.14, 3.77, 11.29, 7.41, 3.83, 6.53, 5.40, 5.38, 6.11, 8.58, 4.00, 9.08,
                  8.71, 12.79, 5.52, 9.01, 4.72, 9.59, 5.02, 9.60, 5.61, 8.31, 10.95, 7.95, 6.13, 5.14,
                  7.66, 9.85, 8.40, 13.20, 8.03, 8.67, 6.53, 5.04, 11.27, 7.33, 8.28, 12.49, 6.44, 8.35,
                  8.05, 2.50, 5.57, 8.93, 7.84, 9.71, 6.61, 10.87, 9.47, 9.29, 7.67, 6.59, 5.04, 6.22,
                  3.83, 10.59, 9.25, 9.04, 7.59, 5.35, 10.25, 5.03, 10.99, 10.33, 7.16, 2.66, 8.20, 7.32)
xminm=min(wynagrodzenie_m) #2.5
xmaxm=max(wynagrodzenie_m) #13.2
q1m=quantile(wynagrodzenie_m, prob=0.25) #5.58
q2m=median(wynagrodzenie_m) #7.76
q3m=quantile(wynagrodzenie_m, prob=0.75) #9.22

Qm=(q3m-q1m)/2
Vaspm=(q1m+q3m-2*q2m)/(2*Qm) #-0.19
VzQm=Qm/q2m*100 #23.48%


#B WYKRES WSPÓLNY
dane <- data.frame(
  wynagrodzenie_w = c(wynagrodzenie_m, wynagrodzenie_k),
  płeć = rep(c("Mężczyźni", "Kobiety"), c(length(wynagrodzenie_m), length(wynagrodzenie_k)))
)

boxplot(
  wynagrodzenie_w ~ płeć, data = dane, outline = FALSE,
  col = c("#4F81BD", "#C0504D"), 
  xlab = "Płeć",
  ylab = "Wynagrodzenie (tys. zł)",
  ylim=c(2,14),
  main = "Porównanie wynagrodzeń kobiet i mężczyzn")
