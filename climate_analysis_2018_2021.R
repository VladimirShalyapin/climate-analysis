# Температура воздуха (Цельсий) Краснодарского края с октября по июль (сельскохозяйственный год для озимой пшеницы)
air_temperature_2018_2019 <- c(14.4, 4.1, 2.6, 2.9, 3.1, 6.4, 11.9, 19.1, 25.3, 23)
air_temperature_2019_2020 <- c(13.5, 6.5, 4, 2.3, 3.8, 9.3, 10.4, 16.5, 22.9, 25.4)
air_temperature_2020_2021 <- c(16.2, 5.7, 1.8, 1.3, 0.5, 4.5, 11.1, 18, 21.7, 26.2)
air_temperature_average_annual <- c(11.3, 5.3, 1.4, -1.1, 0, 4.5, 11.8, 16.7, 20.6, 23.4)

# Сумма осадков (мм) Краснодарского края с октября по июль (сельскохозяйственный год для озимой пшеницы)
precipitation_amount_2018_2019 <- c(60, 64, 68, 89, 29, 59, 44, 53, 35, 132)
precipitation_amount_2019_2020 <- c(34, 18, 40, 64, 55, 18, 4, 89, 37, 105)
precipitation_amount_2020_2021 <- c(17, 38, 21, 102, 108, 53, 86, 64, 108, 27)
precipitation_amount_average_annual <- c(62, 65, 70, 57, 38, 43, 71, 70, 70, 51)

# Запись для подписи на графике boxplot (ящик с усами)
# rep(c(''), кол-во повторений)
year_2018_2019 <- rep(c('2018-2019'), 10)
year_2019_2020 <- rep(c('2019-2020'), 10)
year_2020_2021 <- rep(c('2020-2021'), 10)
average_annual <- rep(c('Сред.многолетние значения'), 10)

# Запись для подписи на графике boxplot (ящик с усами)
year_2018_2019_at <- rep(c('I.I'), 10)
year_2018_2019_pa <- rep(c('I.II'), 10)
year_2019_2020_at <- rep(c('II.I'), 10)
year_2019_2020_pa <- rep(c('II.II'), 10)
year_2020_2021_at <- rep(c('III.I'), 10)
year_2020_2021_pa <- rep(c('III.II'), 10)
average_annual_at <- rep(c('IV.I'), 10)
average_annual_pa <- rep(c('IV.II'), 10)

# Создание фрейма данных для температуры воздуха
data <- data.frame(list(air_temperature=c(air_temperature_2018_2019, air_temperature_2019_2020, air_temperature_2020_2021, air_temperature_average_annual), year=c(year_2018_2019, year_2019_2020,year_2020_2021, average_annual)))

# Создание графика (ящика с усами) по данным температуры воздуха в фрейме данных 
boxplot(data$air_temperature ~ data$year, main = "Температура воздуха за период вегетации озимой пшеницы", ylab = "Температура воздуха, С", xlab = "Сельскохозяйственный год", varwidht = T, data = InsectSprays)

# Расчет квантилей и основных расчетных показателей для температуры воздуха
quantile(data$air_temperature)
quantile(air_temperature_2018_2019)
quantile(air_temperature_2019_2020)
quantile(air_temperature_2020_2021)
quantile(air_temperature_average_annual)
summary(data$air_temperature)
summary(air_temperature_2018_2019)
summary(air_temperature_2019_2020)
summary(air_temperature_2020_2021)
summary(air_temperature_average_annual)

# Создание фрейма данных для суммы осадков
data <- data.frame(list(precipitation_amount=c(precipitation_amount_2018_2019, precipitation_amount_2019_2020, precipitation_amount_2020_2021, precipitation_amount_average_annual), year=c(year_2018_2019, year_2019_2020, year_2020_2021, average_annual)))

# Создание графика (ящика с усами) по данным суммы осадков в фрейме данных
boxplot(data$precipitation_amount ~ data$year, main = "Сумма осадков за период вегетации озимой пшеницы", ylab = "Сумма осадков, мм", xlab = "Сельскохозяйственный год", varwidht = T , data = InsectSprays)

# Расчет квантилей и основных расчетных показателей для суммы осадков
quantile(data$precipitation_amount)
quantile(precipitation_amount_2018_2019)
quantile(precipitation_amount_2019_2020)
quantile(precipitation_amount_2020_2021)
quantile(precipitation_amount_average_annual)
summary(data$precipitation_amount)
summary(precipitation_amount_2018_2019)
summary(precipitation_amount_2019_2020)
summary(precipitation_amount_2019_2020)
summary(precipitation_amount_average_annual)

# Считаем линейную регрессию 2018-2019
data <- data.frame(list(air_temperature_2018_2019, precipitation_amount_2018_2019))
plot(air_temperature_2018_2019 ~ precipitation_amount_2018_2019, data=data)
abline(lm(air_temperature_2018_2019 ~ precipitation_amount_2018_2019, data=data), col='red')

# Считаем линейную регрессию 2019-2020
data <- data.frame(list(air_temperature_2019_2020, precipitation_amount_2019_2020))
plot(air_temperature_2019_2020 ~ precipitation_amount_2019_2020, data=data)
abline(lm(air_temperature_2019_2020 ~ precipitation_amount_2019_2020, data=data), col='red')

# Считаем линейную регрессию 2020-2021
data <- data.frame(list(air_temperature_2020_2021, precipitation_amount_2020_2021))
plot(air_temperature_2020_2021 ~ precipitation_amount_2020_2021, data=data)
abline(lm(air_temperature_2020_2021 ~ precipitation_amount_2020_2021, data=data), col='red')

# Считаем линейную регрессию средних многолетних данных
data <- data.frame(list(air_temperature_average_annual, precipitation_amount_average_annual))
plot(air_temperature_average_annual ~ precipitation_amount_average_annual, data=data)
abline(lm(air_temperature_average_annual ~ precipitation_amount_average_annual, data=data), col='red')

# Создание фрейма данных для суммы осадков и температуры воздуха
data <- data.frame(list(info_data = c(air_temperature_2018_2019, precipitation_amount_2018_2019, air_temperature_2019_2020, precipitation_amount_2019_2020, air_temperature_2020_2021, precipitation_amount_2020_2021,air_temperature_average_annual, precipitation_amount_average_annual), year=c(year_2018_2019_at, year_2018_2019_pa, year_2019_2020_at, year_2019_2020_pa, year_2020_2021_at, year_2020_2021_pa, average_annual_at, average_annual_pa)))
# Создание графика (ящика с усами) по данным суммы осадков и температуры воздуха в фрейме данных
boxplot(data$info_data ~ data$year, main='Диаграмма климатических условий за три года исследования (2018-2021)', ylab = 'Градусы Цельсия | мм', xlab = '', varwidht = T)
# Легенда для графика (ящика с усами)
legend('topright', inset=.01, title = 'Легенда', c('I.I|I.II - температура воздуха|сумма осадков (2018-2019)', 'II.I|II.II - температура воздуха|сумма осадков (2019-2020)', 'III.I|III.II - температура воздуха|сумма осадков (2020-2021)', 'IV.I|IV.II - температура воздуха|сумма осадков (ср.мн.зн.)'))

