#Na samym początku wczytuję dane
read.csv("dane.csv") -> dane


library(dplyr)
library(tidyverse)
library(ggplot2)
library(caret)
library(purrr)
library(gam)

#Najpierw uporządkuję dane: zastąpie wartości NA oraz uzupełnię brakujące komórki
#Oglądając dane zauważam, że osoby zahibernowane mają wartości 0 przy każdej z usług
#Sprawdzam, czy to prawda

dane%>%
  filter(CryoSleep == "True")%>%
  select(Name, CryoSleep, VIP, RoomService, FoodCourt, ShoppingMall, Spa, VRDeck)%>%
  head(10)

#Widać, że dla 10 pierwszych takich osób wartość kolumny z usługami wynosi 0. 
#Podobnie jest dla całej reszty, co teraz pokażę bez konieczności oglądania tabeli.
#Najpierw zastąpie wszystkie wartości NA dla interesujących kolumn tabeli na 0.

for (i in 8:12) {
  dane[ ,i][is.na(dane[ ,i])] <- 0
}

#Operacja ta wydaje się być sensowna, ponieważ śpiące osoby raczej nie korzystają z usług
#Teraz nastąpi krok, w którym pokażę, że wszędzie są 0 w kolumach z usługami.

dane%>%
  filter(CryoSleep == "True") -> zahibernowani

mean(zahibernowani$RoomService == 0)
mean(zahibernowani$FoodCourt == 0)
mean(zahibernowani$ShoppingMall == 0)
mean(zahibernowani$Spa == 0)
mean(zahibernowani$VRDeck == 0)

#A więc istotnie nie korzystają z usług, bo śpią.
#Teraz uporządkuję resztę danych. Najpierw zastąpie brakujące wartości w Age, średnią wieku osoby przed i osoby po.

for (i in 1:length(dane$Age)){
  if (is.na(dane$Age[i]) == TRUE){
    dane$Age[i] <- (dane$Age[i + 1] + dane$Age[i - 1])/2
  }
}

#Tym samym usunąłem wszystkie NA z kolumny Age. Można też np. zastąpić NA min{dane$Age[i - 1], dane$Age[i + 1]}.
#Imion zastąpić się nie da, ale one nie będą potrzebne do modelu przewidywania. 
#Zobaczmy ile jest poszczególnych osób z Marsa, Ziemii i Europy.

length(dane$HomePlanet[dane$HomePlanet == "Mars"])
length(dane$HomePlanet[dane$HomePlanet == "Earth"])
length(dane$HomePlanet[dane$HomePlanet == "Europa"])

#Widzimi zatem, że ilość osób rozkłada się mniejwięcej w stosunku 5 : 13 : 6
#Prawdopodobieństwo wylosowania osoby z Ziemii jest wyższe niż z pozostałych miejsc.
#Uzupełnię pozostałe puste komórki losowymi wartościami z c(Earth, Mars, Europe), z tym
# że prawdopodobieństwo wylosowania się danej lokacji jest odpowiednie do stosunku.

miejsca <- c(rep("Mars", 5), rep("Earth", 13), rep("Europa", 6))

dane$HomePlanet[dane$HomePlanet == ''] <- sample(miejsca, sum(dane$HomePlanet == ''), replace = TRUE)

#Teraz rozdzielę kolumnę Cabin na 3 części: deck, number, side

rozdziel_stringi <- function(data, separator) {
  podzielone_stringi <- strsplit(data, split = separator)
  
  Deck <- sapply(podzielone_stringi, function(x) x[1])
  Number <- sapply(podzielone_stringi, function(x) x[2])
  Side <- sapply(podzielone_stringi, function(x) x[3])
  
  dane%>%
    mutate(Deck = Deck, Number = Number, Side = Side)%>%
    select(-Cabin)-> dane

}

rozdziel_stringi(dane$Cabin, "/") -> dane

#Udało się. Teraz pora na zastanowienie się, co uzupełnić w brakujące komórki

length(dane$Side[dane$Side == "P"])
length(dane$Side[dane$Side == "S"])

#Jest praktycznie po tyle samo jednego i drugiego - uzupełniam losowo

sides <- c("P", "S")
dane$Side[is.na(dane$Side)] <- sample(sides, sum(is.na(dane$Side)), replace = TRUE)

#Jedno z 3 uzupełnione. Pora na Number - tutaj zrobie trik z minimum o którym wcześniej wspominałem.

for (i in 1:length(dane$Number)){
  if (is.na(dane$Number[i]) == TRUE){
    dane$Number[i] <- min(dane$Number[i - 1], dane$Number[i + 1])
  }
}

#Pora na literki. Zobaczmy jak przedstawia się ich proporcja i na tej podstawie przypiszmy do pozostałych komórek.

for (i in c("A", "B", "C", "D", "E", "F", "G")){
  print(length(dane$Deck[dane$Deck == i ]))
}

#Widzimy mniej więcej proporcje 10 : 21 : 21 : 15 : 24 : 66 : 60

literki <- c(rep("A", 10), rep("B", 21), rep("C", 21), rep("D", 15), rep("E", 24), rep("F", 66), rep("G", 60))
dane$Deck[is.na(dane$Deck)] <- sample(literki, sum(is.na(dane$Deck)), replace = TRUE)

#Jeszcze zostały nam do uzupełnienia puste komórki w CryoSleep. Najpierw wpisze True tam, gdzie za wszystkie usługi wydano 0.
#False uzupełniłem tam, gdzie jakakolwiek komórka była niezerowa.

for (i in 1:length(dane$CryoSleep)){
  if (dane$CryoSleep[i] == ''){
    if (dane$RoomService[i] == 0 & dane$FoodCourt[i] == 0 & dane$ShoppingMall[i] == 0 & dane$Spa[i] == 0 & dane$VRDeck[i] == 0){
      dane$CryoSleep[i] <- "True"
    }else{
      dane$CryoSleep[i] <- "False"
    }
  }
}

#Teraz zajme się pustymi komórkami w kolumnie VIP. Zobacze co sie tam dzieje

dane%>%
  filter(VIP == '') -> puste_vipy

#Zobaczmy stosunek vipów do nievipów i postąpmy analogicznie jak wcześniej

length(dane$VIP[dane$VIP == "True"])
length(dane$VIP[dane$VIP == "False"])

#Stosunek: 3 : 125

vipy <- c(rep("True", 3), rep("False", 125))
dane$VIP[dane$VIP == ""] <- sample(vipy, sum(dane$VIP == ""), replace = TRUE)

#Podobnie postącpie z destynacjami

destynacje <- unique(dane$Destination)

length(dane$Destination[dane$Destination == "TRAPPIST-1e"])
length(dane$Destination[dane$Destination == "PSO J318.5-22"])
length(dane$Destination[dane$Destination == "55 Cancri e"])

destynacje <- c(rep("TRAPPIST-1e", 5915), rep("PSO J318.5-22", 796), rep("55 Cancri e", 1800))
dane$Destination[dane$Destination == ''] <- sample(destynacje, sum(dane$Destination == ''), replace = TRUE)

dane%>%
  filter(Transported == "True")%>%
  group_by(Age)%>%
  summarise(Number = n())%>%
  arrange(desc(Age))%>%
  top_n(10) -> dane_1

wykres_1 <- ggplot(dane_1, aes(x = "", y = Number, fill = Age)) +
  geom_bar(stat = "identity", width = 1) +
  geom_text(aes(label = Number), position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y") +
  theme_void()

dane%>%
  filter(Transported == "True")%>%
  group_by(VIP)%>%
  summarise(NumberTransported = n()) -> dane_2a
dane%>%
  filter(Transported == "False")%>%
  group_by(VIP)%>%
  summarise(NumberNotTransported = n()) -> dane_2b
dane_2a%>%
  left_join(dane_2b) -> dane_2

dane%>%
  filter(Transported == "True")%>%
  group_by(HomePlanet, Destination)%>%
  summarise(NumberTransported = n()) -> dane_3a
dane%>%
  filter(Transported == "False")%>%
  group_by(HomePlanet, Destination)%>%
  summarise(NumberNotTransported = n()) -> dane_3b
dane_3a%>%
  left_join(dane_3b)%>%
  mutate(Ratio =NumberTransported/NumberNotTransported)-> dane_3

wykres_3 <- ggplot(dane_3, aes(x = HomePlanet, y = Ratio, fill = Destination)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Stosunek przewiezionych do nieprzewiezionych pasażerów",
       x = "Planeta pochodzenia",
       y = "Stosunek") +
  scale_fill_brewer(palette = "Set1") +  
  theme_minimal()

dane%>%
  group_by(VIP)%>%
  summarise(RoomServiceMean = mean(RoomService), FoodCourtMean = mean(FoodCourt),
            ShoppingMallMean = mean(ShoppingMall), SpaMean = mean(Spa)) -> dane_4a
dane%>%
  group_by(VIP)%>%
  summarise(RoomServiceMax = max(RoomService), FoodCourtMax = max(FoodCourt),
            ShoppingMallMax = max(ShoppingMall), SpaMax = max(Spa)) -> dane_4b

dane%>%
  filter(Transported == "True")%>%
  group_by(CryoSleep)%>%
  summarise(NumberTransported = n()) -> dane_5a
dane%>%
  filter(Transported == "False")%>%
  group_by(CryoSleep)%>%
  summarise(NumberNotTransported = n()) -> dane_5b
dane_5a%>%
  left_join(dane_5b)%>%
  mutate(Ratio =NumberTransported/NumberNotTransported) -> dane_5

dane%>%
  filter(Transported == "True")%>%
  group_by(Side, Deck)%>%
  summarise(NumberTransported = n()) -> dane_6a
dane%>%
  filter(Transported == "False")%>%
  group_by(Side, Deck)%>%
  summarise(NumberNotTransported = n()) -> dane_6b
dane_6a%>%
  left_join(dane_6b)%>%
  mutate(Ratio = NumberTransported/NumberNotTransported)-> dane_6

wykres_6 <- ggplot(dane_6, aes(x = Side, y = Ratio, fill = Deck)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Stosunek przewiezionych do nieprzewiezionych pasażerów",
       x = "Strona",
       y = "Stosunek") +
  scale_fill_brewer(palette = "Set1") +  
  theme_minimal()
