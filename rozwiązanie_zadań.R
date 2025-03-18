# cwiczenia 1
#a)
54392+32841

#b)
68321^(1/13)

#c)
log10(100484)

#d)
75849%%3
75849%%5

#e)
sd(c(1:100))

#f)
rnorm(120)

#g)
cor(c(1:50), c(-50:-1))


x <- lm(hp ~ cyl, data = mtcars)

summary(x)

y <- lm(hp ~ cyl + wt, data = mtcars)
summary(y)

#ćwiczenia 2
znizka <- 0.17
actual_price <- 132458*(1-znizka)
actual_price

#ćwiczenia 3
class(actual_price)
typeof(actual_price)

a <- "Ala"
b <- 10L
c <- 10

class(b)
class(c)

wektor <- c(1:10000)
unique(wektor)

#ćwiczenia 4

class(mtcars)
str(mtcars)

dane <- c("Ala", 4538, "kot", 0.542)
class(dane)
str(dane)

dane_lista <- list("Ala", 4538, "kot", 0.542)
class(dane_lista)
str(dane_lista)

ramka <- data.frame(
  kraj = character(),
  powierzchnia = numeric(),
  UE = logical(),
  stolica = character()
)

#ćwiczenie 5

read.csv()

read.table()

load()

########


#ćwiczenie 6
#a)
library(tidyverse)

#b)
iris %>% 
  dplyr::group_by(Species) %>% 
  dplyr::summarise_all(mean)

#c)
rok_2025 <- seq.Date(as.Date("2025-01-01"), as.Date("2025-12-31"), "day")

#d)
rok_2025 <- data.frame(
  day = rok_2025
)

#e)
rok_2025 <- rok_2025 %>% 
  dplyr::mutate(
    week = lubridate::floor_date(day, "week"),
    month = lubridate::floor_date(day, "month"),
  )

#ćwiczenie 7

i = 1
while(i <= 11) {
  print(i)
  i = i + 1
}

for(i in 1:length(letters)) {
  print(paste(letters[i], "jest ", i, "literą alfabetu"))
}

x <- 245

if(x %% 2 == 0) {
  print(paste(x, " jest liczbą parzystą"))
} else {
  print(paste(x, " jest liczbą nieparzystą"))
}

#ćwiczenie 8

ramka <- data.frame(
  liczba_egzemplarzy = sample(c(1:10000), 200)
)

ramka2 = ramka
ramka2$kategoria = NA

for(i in 1:nrow(ramka2)) {
  if(ramka2$liczba_egzemplarzy[i] < 2000) {
    ramka2$kategoria[i] = "niska sprzedaż"
  } else if(ramka2$liczba_egzemplarzy[i] >= 2000 & ramka2$liczba_egzemplarzy[i] < 5000) {
    ramka2$kategoria[i] = "średnia sprzedaż"
  } else if(ramka2$liczba_egzemplarzy[i] >= 5000 & ramka2$liczba_egzemplarzy[i] < 9000) {
    ramka2$kategoria[i] = "dobra sprzedaż"
  } else {
    "bestseller"
  }
}

ramka1 <- ramka %>% 
  dplyr::mutate(
    kategoria = case_when(
      liczba_egzemplarzy < 2000 ~ "niska sprzedaż",
      liczba_egzemplarzy >=2000 & liczba_egzemplarzy < 5000 ~ "średnia sprzedaż",
      liczba_egzemplarzy >=5000 & liczba_egzemplarzy < 9000 ~ "dobra sprzedaż",
      liczba_egzemplarzy >= 9000 ~ "bestseller"
    )
  )


# wykresy
# 1. wykres punktowy

x = sample(c(1:100), 10)
y = sample(c(1:100), 10)

plot(x, y, col = "red", pch = 2, main = "tytul", xlab = "tytul osi x", ylab = "tytul osi y")

# Wykres liniowy

t = c(1:100)
z = sin(t/10)
s
plot(t, z, type = "l", col = "blue", lwd = 2)
legend("bottomleft", legend = "sin(t/10)")

## Wykres pudełkowy

boxplot(count ~ spray, data = InsectSprays, col = "pink")

## Histogram
rozklad = rnorm(1000)

hist(rozklad, breaks = 10, col = "pink", border = "black", probability = T)
lines(density(rozklad))
