library(tidyverse)
library(DescTools) #funkcja Mode(v), przewodnik po pakiecie R / code academy


koszt_duze=c(9.2, 9.8, 8.2, 11.2, 9.1, 10.8, 8.6, 13.5, 11.9, 11.0, 6.1, 8.7, 10.5, 6.5, 11.4, 7.9, 10.4, 11.9, 10.8, 11.4, 11.2, 9.7, 9.7, 10.2, 11.2, 5.9, 9.5, 8.8, 13.6, 8.1, 11.7, 6.2, 10.6, 10.7, 6.3, 9.3, 9.7, 7.9, 11.5, 10.5, 9.7, 11.4, 9.6, 8.4, 11.5, 9.6, 9.8, 10.2, 14.6, 16.8, 10.2, 11.5, 13.1, 9.1, 12.3, 10.2, 11.5, 8.5, 8.6, 11.3)
koszt_male=c(7.3, 5.3, 5.6, 7.7, 8.7, 6.3, 9.5, 7.0, 8.9, 8.0, 3.8, 7.6, 6.1, 6.2, 7.6, 8.9, 4.5, 3.2, 7.5, 5.6, 3.8, 6.1, 7.3, 8.4, 3.3, 7.6, 7.5, 7.0, 7.3, 6.1, 8.1, 12.0, 8.6, 5.7, 5.5, 6.1, 7.3, 3.9, 7.2, 8.7, 7.1, 5.8, 8.5, 9.9, 4.4, 3.7, 7.1, 5.7, 8.5, 5.6, 11.0, 8.9, 10.0, 9.7, 9.8, 4.7, 6.8, 8.6, 9.1, 12.3, 6.2, 3.8, 5.6, 3.6, 7.7, 3.4, 3.8, 8.4, 5.2, 7.5, 6.7, 5.8, 6.7, 6.6, 7.4)
koszt_wszystkie=c(koszt_duze,koszt_male)

a=c(1,2,4,4,5,5,5)

#A - duze
#średnia
śr_duze=mean(koszt_duze)
war_duze=sum((koszt_duze-śr_duze))^2/length(koszt_duze)
median(koszt_duze)
Mode(koszt_duze)

#A - male
śr_male=mean(koszt_male)
war_duze=sum((koszt_male-śr_duze))^2/length(koszt_male)
median(śr_male)
Mode(koszt_male)

#A - wszystkie
śr_wszystkie=mean(koszt_wszystkie)
war_duze=sum((koszt_wszystkie-śr_wszystkie))^2/length(koszt_wszystkie)
median(koszt_wszystkie)
Mode(koszt_wszystkie)


#B

#C