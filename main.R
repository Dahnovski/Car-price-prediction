car <- read.csv("/Users/nazardahnovski/datasets/car_price_dataset/Car details v3.csv")
head(car)

# извлекаем бренд машины из полного названия
car$name <- word(car$name,1)
head(car)

# строим гистограмму для брендов машин
ggplot(data = car, aes(x = name)) +  # Убираем fill для цвета
  geom_bar() +
  labs(x = "Car Brand", title = "Bar Graph of Car Brand") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 19),  # Увеличиваем размер шрифта оси X на 5 пунктов
    axis.text.y = element_text(size = 12),  # Увеличенный размер подписей оси Y
    axis.title.x = element_text(size = 16, face = "bold"),  # Увеличенный размер заголовка оси X
    axis.title.y = element_text(size = 16, face = "bold"),  # Увеличенный размер заголовка оси Y
    plot.title = element_text(size = 18, face = "bold")  # Увеличенный размер заголовка графика
  )



# заменяем названия с имен на числовые значения
car$name <- str_replace(car$name, 'Maruti', '0')
car$name <- str_replace(car$name, 'Skoda', '1')
car$name <- str_replace(car$name, 'Honda', '2')
car$name <- str_replace(car$name, 'Hyundai', '3')
car$name <- str_replace(car$name, 'Toyota', '4')
car$name <- str_replace(car$name, 'Ford', '5')
car$name <- str_replace(car$name, 'Renault', '6')
car$name <- str_replace(car$name, 'Mahindra', '7')
car$name <- str_replace(car$name, 'Tata', '8')
car$name <- str_replace(car$name, 'Chevrolet', '9')
car$name <- str_replace(car$name, 'Fiat', '10')
car$name <- str_replace(car$name, 'Datsun', '11')
car$name <- str_replace(car$name, 'Jeep', '12')
car$name <- str_replace(car$name, 'Mercedes-Benz', '13')
car$name <- str_replace(car$name, 'Mitsubishi', '14')
car$name <- str_replace(car$name, 'Audi', '15')
car$name <- str_replace(car$name, 'Volkswagen', '16')
car$name <- str_replace(car$name, 'BMW', '17')
car$name <- str_replace(car$name, 'Nissan', '18')
car$name <- str_replace(car$name, 'Lexus', '19')
car$name <- str_replace(car$name, 'Jaguar', '20')
car$name <- str_replace(car$name, 'Land', '21')
car$name <- str_replace(car$name, 'MG', '22')
car$name <- str_replace(car$name, 'Volvo', '23')
car$name <- str_replace(car$name, 'Daewoo', '24')
car$name <- str_replace(car$name, 'Kia', '25')
car$name <- str_replace(car$name, 'Force', '26')
car$name <- str_replace(car$name, 'Ambassador', '27')
car$name <- str_replace(car$name, 'Ashok', '28')
car$name <- str_replace(car$name, 'Isuzu', '29')
car$name <- str_replace(car$name, 'Opel', '30')
car$name <- str_replace(car$name, 'Peugeot', '31')

car$name <- as.numeric(car$name)
table(car$name)

# удалим колонку с крутящим моментом
car <- subset (car, select = -torque)

head(car)

# почитстим колонку mileage от (km/kg, km/kg) сконвертируем числовой формат и заменим пропущенные значения на среднее
car$mileage <- str_replace(car$mileage, 'kmpl', '')
car$mileage <- str_replace(car$mileage, 'km/kg', '')
car$mileage <- as.numeric(car$mileage)
car$mileage[is.na(car$mileage)]<-mean(car$mileage,na.rm=TRUE)

head(car)

# обработаем так же столбец с объемом двигателя (уберем CC) и оставим числовой формат
car$engine <- str_replace(car$engine, 'CC', '')
car$engine <- as.numeric(car$engine)
car$engine[is.na(car$engine)]<-mean(car$engine,na.rm=TRUE)

head(car)

# в колонке max_power удалим bhp значение чтоб сделать колонку числовой
car$max_power <- str_replace(car$max_power, 'bhp', '')
car$max_power <- as.numeric(car$max_power)
car$max_power[is.na(car$max_power)]<-mean(car$max_power,na.rm=TRUE)

head(car)

# преобразуем колонку seats в число и заменим пропуски
car$seats <- as.numeric(car$seats)
car$seats[is.na(car$seats)]<-median(car$seats,na.rm=TRUE)

head(car)

# заменим пустые строки ("") в колонках mileage, engine и max_power на NA (пропущенные значения)
car$mileage[car$mileage == ""] <- NA
car$engine[car$engine == ""] <- NA
car$max_power[car$max_power == ""] <- NA

head(car)

# проверим пропуски
sapply(car, function(x) sum(is.na(x)))

# построим Missingness Map
library(visdat)

vis_miss(car) +
  ggtitle("Missingness Map (100% Observed Data)") +
  theme(
    plot.title = element_text(size = 20, face = "bold"),  # Увеличенный заголовок
    axis.title = element_text(size = 18, face = "bold"),  # Увеличенные подписи осей
    axis.text = element_text(size = 18),                  # Увеличенные значения на осях
    axis.title.x = element_text(size = 20, face = "bold"), # Увеличенный размер подписи оси X
    axis.title.y = element_text(size = 20, face = "bold"), # Увеличенный размер подписи оси Y
    legend.text = element_text(size = 18),                 # Увеличенный размер текста легенды
    legend.title = element_text(size = 18, face = "bold")  # Увеличенный размер заголовка легенды
  )


# построим столбчатую диаграмму (bar chart), показывающую распределение автомобилей по типу топлива (fuel).
ggplot(data = car, aes(x = reorder(fuel, fuel, function(x) -length(x)))) +
  geom_bar() +
  labs(x = 'Fuel', title = "Bar Graph of Fuel") +
  theme(
    axis.title = element_text(size = 20),  # Размер шрифта для подписей осей
    axis.text = element_text(size = 25),   # Размер шрифта для текста на осях
    plot.title = element_text(size = 30),  # Размер шрифта для заголовка
    legend.position = "none"               # Убираем легенду, так как она не нужна
  )


# график распределения по брэндам
# ggplot(data = car, aes(x = factor(name), fill = factor(name))) +
#   geom_bar() +
#   labs(x = 'Car Brand') +
#   labs(title = "Bar Graph of Car Brand") +
#   theme(axis.text.x = element_text(angle = 90, hjust = 1))

# создадим столбчатую диаграмму, которая отображает распределение по количеству владельцев автомобилей.
ggplot(data = car, aes(x = reorder(owner, owner, function(x) -length(x)))) +
  geom_bar() +
  labs(x = 'Owner', title = "Bar Graph of Owner") +
  theme(
    axis.title = element_text(size = 20),      # Размер шрифта для подписей осей
    axis.text = element_text(size = 25),       # Размер шрифта для текста на осях
    plot.title = element_text(size = 25),      # Размер шрифта для заголовка
    axis.text.x = element_text(angle = 45, hjust = 1), # Наклон подписей на оси X
    legend.position = "none"                   # Убираем легенду
  )

# а теперь создадим box plot
# Пример с использованием числовых данных
ggplot(data = car, aes(x = reorder(owner, owner, function(x) -length(x)), y = mileage)) +
  geom_boxplot(outlier.colour = "red", outlier.size = 2) +  # Ящик с выбросами
  labs(x = 'Owner', y = 'Mileage', title = "Box Plot of Owner") +
  theme(
    axis.title = element_text(size = 20),      # Размер шрифта для подписей осей
    axis.text = element_text(size = 25),       # Размер шрифта для текста на осях
    plot.title = element_text(size = 25),      # Размер шрифта для заголовка
    axis.text.x = element_text(angle = 45, hjust = 1), # Наклон подписей на оси X
    legend.position = "none"                   # Убираем легенду
  )



# распределение по количеству мест
ggplot(data = car, aes(x = reorder(seats, seats, function(x) -length(x)))) +
  geom_bar() +
  labs(x = 'Seats', title = "Bar Graph of Seats") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 19),  # Увеличиваем шрифт оси X на 5 пунктов
    axis.text.y = element_text(size = 12),  # Увеличенный размер подписей оси Y
    axis.title.x = element_text(size = 20, face = "bold"),  # Увеличенный размер заголовка оси X
    axis.title.y = element_text(size = 20, face = "bold"),  # Увеличенный размер заголовка оси Y
    plot.title = element_text(size = 18, face = "bold")  # Увеличенный размер заголовка графика
  )

# График с увеличением размера точек для более популярных типов топлива
# Подсчет частоты для каждого сочетания года и типа топлива
car_summary <- car %>%
  group_by(year, fuel) %>%
  summarise(count = n())

# Построение графика
# Подсчет частоты и среднее значение пробега для каждого сочетания года и типа топлива
car_summary <- car %>%
  group_by(year, fuel) %>%
  summarise(
    km_driven_avg = mean(km_driven, na.rm = TRUE),  # Среднее значение пробега
    count = n()  # Подсчет количества
  )

# Построение графика
ggplot(car_summary, aes(x = year, y = km_driven_avg, color = fuel, size = count)) +
  geom_point(alpha = 0.6) +  # Точки с размером, основанным на подсчитанных частотах
  labs(x = "Year of Manufacture", y = "Average Kilometers Driven", title = "Car Mileage vs Year of Manufacture") +
  theme(
    plot.title = element_text(size = 20, face = "bold"),  # Увеличенный заголовок
    axis.title = element_text(size = 18, face = "bold"),  # Увеличенные подписи осей
    axis.text = element_text(size = 18),                  # Увеличенные значения на осях
    axis.title.x = element_text(size = 25, face = "bold"), # Увеличенный размер подписи оси X
    axis.title.y = element_text(size = 25, face = "bold"), # Увеличенный размер подписи оси Y
    legend.text = element_text(size = 16),                 # Увеличенный размер текста легенды
    legend.title = element_text(size = 18, face = "bold")  # Увеличенный размер заголовка легенды
  ) +
  scale_color_manual(values = c("petrol" = "blue", "diesel" = "green", "CNG" = "orange", "LPG" = "red", "electric" = "purple")) +  # Цвета для топлива
  theme(legend.position = "bottom")  # Размещение легенды внизу



# преобразуем категориальные значения "Manual" и "Automatic" в бинарные значения (0 и 1)
car$transmission <- str_replace(car$transmission, 'Manual', "0")
car$transmission <- str_replace(car$transmission, 'Automatic', "1")
car$transmission <- as.numeric(car$transmission)
table(car$transmission) # посмотрим на результат нашей замены

# теперь количество владельцев тоже засунем в числовые значения
car$owner <- str_replace(car$owner, 'First Owner', "0")
car$owner <- str_replace(car$owner, 'Second Owner', "1")
car$owner <- str_replace(car$owner, 'Third Owner', "2")
car$owner <- str_replace(car$owner, 'Fourth & Above Owner', "3")
car$owner <- str_replace(car$owner, 'Test Drive Car', "4")
car$owner <- as.numeric(car$owner)
table(car$owner) # посмотрим на результат нашей замены

# преобразуем категориальные данные о типах продавцов автомобилей, в числовые значения
car$seller_type <- str_replace(car$seller_type, "Trustmark Dealer", "0")
car$seller_type <- str_replace(car$seller_type, "Dealer", "1")
car$seller_type <- str_replace(car$seller_type, "Individual", "2")
car$seller_type <- as.numeric(car$seller_type)
table(car$seller_type) # посмотрим на результат нашей замены

# преобразуем данные о типе топлива в числовые значения
car$fuel <- str_replace(car$fuel, 'Diesel', "0")
car$fuel <- str_replace(car$fuel, 'Petrol', "1")
car$fuel <- str_replace(car$fuel, 'CNG', "2")
car$fuel <- str_replace(car$fuel, 'LPG', "3")
car$fuel <- as.numeric(car$fuel)
table(car$fuel) # посмотрим на то, что у нас вышло

# Построим гистограмму для визуализации распределения цен продажи автомобилей + добавим плотность
# Построим гистограмму для визуализации распределения цен продажи автомобилей + добавим плотность
# Подключим библиотеку для работы с форматами
library(scales)

# Построим гистограмму для визуализации распределения цен продажи автомобилей
ggplot(car, aes(x = selling_price)) +
  geom_histogram(aes(y = ..density..), colour = "black", fill = "gray", bins = 30) +  # Гистограмма с цветом в один тон (серый)
  labs(x = 'Selling Price', title = "Histogram Graph of Selling Price") +  # Названия осей и заголовок
  scale_x_continuous(trans = 'log10', labels = scales::label_comma(big.mark = ",")) +  # Логарифмическая шкала для оси X и форматирование с запятыми
  theme(
    plot.title = element_text(size = 20, face = "bold"),  # Увеличенный размер заголовка
    axis.title = element_text(size = 18, face = "bold"),  # Увеличенные подписи осей
    axis.text = element_text(size = 16),  # Увеличенные значения на осях
    axis.text.x = element_text(angle = 45, hjust = 1, size = 14),  # Наклоненные подписи оси X и увеличенный шрифт
    axis.title.x = element_text(size = 18, face = "bold"),  # Увеличенный размер подписи оси X
    axis.title.y = element_text(size = 18, face = "bold")   # Увеличенный размер подписи оси Y
  )
# логарифмическая шкала помогает смягчить влияние экстремальной цены (график более понятный и сбалансированный)

# гистограмма для пробега
# Построим гистограмму для визуализации распределения пробега
ggplot(car, aes(x = year, y = km_driven)) +
  geom_point(color = "black") +  # Темные точки на графике
  labs(x = 'Year', y = 'Km Driven', title = "Scatter Plot of Km Driven vs Year") +  # Названия осей и заголовок
  theme(
    plot.title = element_text(size = 20, face = "bold"),  # Увеличенный размер заголовка
    axis.title = element_text(size = 18, face = "bold"),  # Увеличенные подписи осей
    axis.text = element_text(size = 16),  # Увеличенные значения на осях
    axis.text.x = element_text(size = 18)  # Увеличенные подписи на оси X
  )


# матрица корреляции
library(corrplot)
library(RColorBrewer)
library(grid)

car_cor <- cor(car)  # Сначала рассчитываем корреляционную матрицу

# Построение корреляционной матрицы
corrplot(
  car_cor,
  type = "full",
  method = "color",
  mar = c(0, 0, 1, 0),  # Убираем отступы сверху
  tl.cex = 1.5,  # Увеличиваем размер шрифта
  tl.srt = 45,   # Наклоняем подписи на 45 градусов
  outline = TRUE,
  tl.col = "black",  # Подписи черного цвета
  col = brewer.pal(9, "Blues")  # Используем палитру Blues из RColorBrewer
)

# заполним корреляционную матрицу числовыми обозначениями
ggcorr(car, label = T)

# округлим значения матрицы до двух десятичных знаков
round(cor(car),2)

# Мы видим, что цена продажи тесно связана с максимальной мощностью, а затем с трансмиссией и названием.

# Теперь разделим набор данных на обучающий и тестовый и используем два алгоритма на обучающих наборах, а затем - к тестовому (70/30)
set.seed(5)
trainIndex <- createDataPartition(car$selling_price, p = .7,
                                  list = FALSE,
                                  times = 1)
Train <- car[ trainIndex,]
Test <- car[-trainIndex,]

# линейная регрессия (включает все переменные из обучаещего набора для предсказания цены продажи)
model1_lr <- lm(selling_price ~ ., data = Train)
summary(model1_lr) # выводы

# Модель линейной регрессии предсказывает цену продажи автомобиля с достаточно хорошей точностью (R-squared ≈ 0.70).
# Но стоит отметить, что некоторые переменные, такие как fuel, owner, и seats, не оказывают значительного влияния на цену,
# и могут быть удалены из модели для улучшения её точности.

# Мы можем удалить fuel, owner, seats

# Вычислим относительную важность(узнаем какие переменные оказывают наибольшее влияние на прогноз)
relImportance <- calc.relimp(model1_lr, type = "lmg", rela = F)

# сортируем
cat('Relative Importances: \n')
importance_lr <- as.data.frame(sort(round(relImportance$lmg, 3), decreasing=TRUE))

importance_lr

# теперь мы конструируем модель используя важные переменные
library(ggplot2)
install.packages("ggfortify")

library(ggfortify)

# Создание модели линейной регрессии
lr <- lm(selling_price ~ name + year + km_driven + seller_type + mileage + transmission + max_power, data = Train)
options(scipen=999)
# Основные графики диагностики модели линейной регрессии
autoplot(lr, which = 1:4) +
  theme_minimal(base_size = 16) +  # Убираем лишний фон и увеличиваем текст
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 16),
    panel.grid.major = element_line(color = "grey80"), # Более мягкая сетка
    panel.grid.minor = element_blank(), # Убираем мелкую сетку
    plot.background = element_rect(fill = "white", color = NA) # Белый фон без границ
  )


library(ggplot2)
library(broom)

# Вычисляем диагностические значения
diag_data <- augment(lr)

# Строим график Residuals vs Leverage
ggplot(diag_data, aes(x = .hat, y = .std.resid)) +
  geom_point(color = "black") +  # Темные точки
  geom_smooth(method = "loess", color = "black", se = FALSE) +  # Гладкая кривая тренда
  labs(
    title = "Residuals vs Leverage",
    x = "Leverage",
    y = "Standardized Residuals"
  ) +
  theme_minimal(base_size = 16)  # Увеличенный шрифт для читаемости

# теперь считаем корень из среднеквадратичной ошибки
pred_lr <- predict(lr, newdata = Test)
error_lr <- Test$selling_price - pred_lr
RMSE_lr <- round(sqrt(mean(error_lr^2)),2)
RMSE_lr

# построим диаграмму и сравним фактические и предсказанные цены продажи автомобилей
options(scipen=999)  # Отключаем научную нотацию

dev.new()  # Открыть новое окно графика
plot(Test$selling_price, pred_lr,
     main = "Scatterplot: Actual vs Predicted Selling Price using LR",
     col = "black",  # Черные точки
     xlab = "Actual Selling Price",
     ylab = "Predicted Selling Price",
     pch = 16,       # Плотные точки
     cex.lab = 1.5,  # Увеличенный размер подписей осей
     cex.axis = 1.5, # Увеличенный размер шкалы
     cex.main = 1.8  # Увеличенный заголовок
)

abline(a = 0, b = 1, col = "black", lwd = 2)  # Черная линия y = x


# Elastic Net
train_cont <- trainControl(method = "repeatedcv",
                           number = 10,
                           repeats = 5,
                           search = "random",
                           verboseIter = TRUE)

elastic_reg <- train(selling_price ~.,
                     data = Train,
                     method = "glmnet",
                     preProcess = c("center", "scale"),
                     tuneLength = 10,
                     trControl = train_cont)


# вычислим ошибку модели Elastic Net и выводит значение RMSE (Root Mean Squared Error) для предсказаний
pred_er <- predict(elastic_reg, Test)
error_er <- Test$selling_price - pred_er
RMSE_er <- sqrt(mean(error_er^2))
RMSE_er <- round(RMSE_er,2)

options(scipen=999)  # Отключаем научную нотацию

# Строим scatter plot (распределение фактических и предсказанных значений)
plot(Test$selling_price, pred_er,
     main = "Actual vs Predicted (Elastic Regression)",
     col = "black",    # Черные точки
     xlab = "Actual Selling Price",
     ylab = "Predicted Selling Price",
     pch = 16,         # Плотные точки
     cex.lab = 1.5,    # Увеличенный размер подписей осей
     cex.axis = 1.5,   # Увеличенный размер шкалы
     cex.main = 1.8    # Увеличенный заголовок
)

# Добавляем линию, где фактические значения равны предсказанным
abline(a = 0, b = 1, col = "black", lwd = 2)  # Черная линия y = x


# Модели
Model <- c('Linear Regression', 'Elastic Net')

# Значения RMSE для каждой модели
RMSE <- c(RMSE_lr, RMSE_er)

# Создаём датафрейм с результатами
res <- data.frame(Model, RMSE)

# Сортируем по значению RMSE по убыванию
res %>% arrange(desc(RMSE))
