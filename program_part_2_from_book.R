n <- floor(rnorm(10000, 500, 100))
t <- table(n)
barplot(t)
x <- c(1, 3, 2) # функция конкатенации, объяденяет и сохраняет в вектор
x
y = c(1, 6, 2)

length(x) # длинна
length(y)
x + y
ls() # список элеметнов
rm(x, y) # удаляем ненужные переменные rm(list = ls()) <- удалит все переменные за раз
ls()
?matrix
x <- matrix(data = c(1, 2, 3, 4), nrow = 2, ncol = 2) # создает матрицу 2х2, заполняет последовательно
x
x <- matrix(c(1,2,3,4,5,6),3,2) # тоже самая матрица что и ранее без лишних переменных  + nrow(строка), ncol(столбцы)
x
a <- matrix(c(1,2,3,4),2,2,byrow = TRUE) # заполняет по строкам
a
sqrt(x) # квадратный корень матрицы
x^2 # возведение в степень

x1 <- rnorm(50) # генерирует вектор случ. норм. распред. значений (n = размер выборки)
y1 <- x1 + rnorm(50, mean = 38, sd = 5) # создает случайные переменные стд.норм.расп со средним 0 и стд. откл 1
# mean = среднее значение, sd = стд отклонение
cor(x1, y1) # расчет корреляции между ними
set.seed(1303) # статическая генерация по числу
rnorm(50)
#mean(), var() - среднее значение и дисперсия некоторого набора данных
#применение sqrt() к результату работы var() даст стд. отклонение,или можно применить sd()
set.seed(3)
y3 = rnorm(100)
mean(y) #среднее значение
var(y3) # дисперсия
sqrt(var(y3)) # std
sd(y3) #std
# графики
x4 <- matrix(rnorm(100, mean = 38, sd = 10),nrow = 5, ncol=  5, byrow = TRUE )
y4 <- matrix(rnorm(100, mean = 78, sd = 10),nrow = 5,ncol= 5, byrow = TRUE)
x4
plot(x4, y4) # создает график
#сохранение графика в типы данных pfd(), jpeg()
pdf("Figure.pdf")
plot(x4, y4, col = "green") # точки будут зелеными
dev.off()# сообщает Р что мы создали график
dia <- seq(1 , 10)# последовательность чисел,генерация
dia
dia1 = 30:10 # сокращенный вариант seq
x6 = seq(-pi, pi, length = 50)
x6
#contour() 3-х мерный график по типу топографической карты
y6 = 0
y6 <- x6
y6
f <- outer(x6, y6, function(x6, y6) cos(y6)/(1+x6^2))#  функция кос(у)/ (1+х^2), создаем , внешний продукт массивов
contour(x6, y6, f) # отказывается строить график из одного массива(вектора), строит по х = seq(-pi, pi, length = 50)
contour(x6, y6, f, nlevels = 45, add = T) # не работает
fa = (f - t(f))/2 #функция (f -t(f))^2  t - транспонирование
contour(x6, y6, fa, nlevels = 20) # nlevels колличевство желаемых контуров на графике, std = 10
#image() работает так же как и contour(), тепловая карта с цветов, где цвет зависит от Z
#для 3-х мерных можно использовать persp(). Аргументы theta, phi - контролируют углы обзора графика
image(x6, y6, fa) # цветной
persp(x6, y6, fa) # трехмерный график
persp(x6, y6, fa, theta = 60) #крутим вертим
persp(x6, y6, fa, theta = 60, phi = 20)
persp(x6, y6, fa, theta = 60, phi = 70)
persp(x6, y6, fa, theta = 60, phi = 40)
A = matrix(1:16, 4, 4)
A[2, 3] #вторая строка, третий столбец
A[c(1,3), c(2,4)] # диапазон для матрицы
b <- c(1:45)
b
x_vec <- c(rnorm(1000, mean = 400, sd = 100),nrow = 5, ncol=  5, byrow = TRUE )
x_vec
A[1:3, 2:4]
A[1:2,] # 1:2 и все остальное
A[, 1:2]
A[1, ] # вектор
A[-c(1,3),]# ""-"" нужно оставить все строки и столбцы за исключением указанных в индексе
A[-c(1,3), -c(1, 3, 4)]
dim(A) # размерность
#read.table() - импорт данных, write.table() экспорт данных
Auto = read.table("Auto.data") # header = T первая строка = имена переменных, na.strings = при ? читает как пропущенный символ
setwd('/home/iakov/Documents/program_with_r/stat-learning/data') # выбираем директорию, полный путь к ней
getwd() # проверяем выбранную директорию
fix(Auto)# выводим на экран  в окне таблицу
Auto = read.csv("Auto.csv", header = T, na.strings = "?")
fix(Auto)
dim(Auto) # сообщает колличевство наблюдений, и колличевство переменных (строк, столбцом)
Auto[1:4,]
Auto = na.omit(Auto) # удаляем пустые строки
dim(Auto)
names(Auto)
plot(Auto$cylinders, Auto$mpg) # $ объеденяем знаком набор переменных, потому что Р не умеет

detach(Auto)# делает тоже, что и$, обратная функция  detch()
attach(Auto)
plot(cylinders, mpg)
cylinders = as.factor(cylinders) # конвертирует колличевственные перемернные в качевственные
plot(cylinders, mpg)
plot(cylinders, mpg, col = "red")
plot(cylinders, mpg, col = "red", varwidth = T)
plot(cylinders, mpg, col = "red", varwidth = T, horizontal=T)
plot(cylinders, mpg, col = "green", varwidth = T, horizontal = T, xlab = "cylinders", ylab = "MPG")
hist(mpg) # гистограма
hist(mpg, col = 2)
hist(mpg, col = 2, breaks = 15)
pairs(Auto) # диаграмма рассеяния , показывает соотношения между переменными, рассеявание
pairs(~mpg + displacement + horsepower + weight + acceleration, Auto)

# identify(horsepower, mpg, name) #подсвечивает точки на графике для их идентификацции(не работает почему то )
plot(horsepower, mpg)
summary(Auto)# рассчитывает колличевственные сводки по каждой переменной
summary(mpg)
college = read.csv('College.csv', header = T, na.strings = "?") #загружаем
fix(college) # просматриваем данные
rownames(college) = college[, 1]  # игнорируем первый столбец
fix(college)
college = college[, -1] # удаляем названия университетов
fix(college)
summary(college) # выводим статистические данные для набора
A = pairs(college[, 1:10]) # обращаемся к первым 10 столбцам набора данных

Elite <- rep("No", nrow(college)) # генерирует функцию нет равную колличевству строк коледжа
B = rep(1:4, 2) # реп генерирует функцию
B
# test func as.factor() and substring
substr("abcdrf", 2, 4)

substring("abcdef", 1:6, 1:6)
substr(rep("abcdef", 10), 1:4, 4:5) # генерирует строки в колл 10 штук, по символьно от 1:4 и 4:5, отсчет в Р начинается с 1
#создает вектор с названиями.
x <- c("asfef", "qwerty", "yuiop[", "b", "stuff.blah.yech")
x
#генерирует заданный диапазон
substr(x, 2, 5)
#походу тоже самое
substring(x , 2, 5)
# нужно заметить что использовать лучше полное название функции, т.к не всегда работает скороченное
substring(x , 2) <- c("..", "+++")
x
#лвв = значения в диапазоне
(ff <- factor(substring("statistic", 1:10, 1:10), levels = letters)) #
# вывод значений соотвествующий позиции букв в лвл летерс
as.integer(ff)
# сбрасывает лвл не используемый в функции
(f. <- factor(ff))
ff[, drop= TRUE] #тоже самое что и сверху, более "прозрачно"
factor(letters[1:20], labels = "letter") #создает ?массив  из 20 имен с названием буква

class(ordered(4:1)) #  ордер упорядоченный, наследуеммый от фактор

z <- factor(LETTERS[3:1], ordered  = TRUE)
z

o <- stopifnot(sort(z)[c(1,3)] == range(z), min(z) < max(z))
#будет нуль, так как нет значений
(x <- factor(c(1, 2, NA), exclude = NULL))
# здесь мы присваеваем NA 2 элементу, заменяем с помощью "да"
is.na(x)[2] <- TRUE
x

# выводит фалсе так как на является не точно определенным
is.na(x)



Elite[college$Top10prec > 50] = "Yes" # помещаем в перменную из колледжа тех, у кого коэф больше 50 по топ10прек
Elite = as.factor(Elite)
college = data.frame(college, Elite)
summary(college$Elite)
summary(college)
'''
private публичное или частное уч. завед
apps колличевство заявлений на поступление
accept колличевство принятых абитуриентов
enroll новых зарег студентов
top10 10% лучших учеников класса
top25 25 луч.уч.кл
F.undegrad кол. стд. уч фулл день
P.und кол. стд. уч не фулл день
Outstate олпата для стд из других штатов
Room. board  плата за проживание и питание
Книги - книги
Personal затраты на личные нужды
Phd процент преподавателей с уч. степенью
Terminal проц.препд. с самой высокой уч. стп
S.F.Ratio соотношение студентов и преподавателей
perc.alumni процет бывших учеников делающих пожертвование
Expend затраты обучения уч. зав на одного студента
Grad.Rate процет студентов полностью закончивших цикл обучения
'''
plot(college$Elite, college$Outstate)
par(mfrow = c(2, 2))
hist(college$Apps)
hist(college$Books)
hist(college$perc.alumni, col=2)
hist(college$S.F.Ratio, col=3, breaks=10)
hist(college$Expend, breaks=100)
dim(Auto)
fix(Auto)
summary(Auto)
# (а)
# количественные: миль на галлон, цилиндры, рабочий объем, лошадиные силы, вес,
# ускорение, год
# качественный: название, происхождение(origin)
# сапли это Р функция иттерации, НАМНОГО БЫСТРЕЕ !!! фор и подобных (в ~100 раз)
sapply(Auto[, 1:7], range)
#-----------------saply() func --------------------
#case 1 matrix and input argument
m1 <- matrix(1:9, nrow = 3)
m1
result <- apply(m1,1, mean) # func(X, MARGIN, FUN, ..) маржин =1 строки, маржин =2  столбы
result
class(result)#выводит класс объекта
#сумма по столбцам
result <- apply(m1, 2, sum)
result
class(result)
result <- apply(m1 ,1 , cumsum)# совокупная сумма элементов для каждой строки
result
class(result)
matrix(apply(m1, 1, cumsum), nrow = 3, byrow =T)

# функции в Р
check <-function(x){
  return(x[x>5])
}
result <- apply(m1, 1, check)
result
class(result)
# case 2 data frame as an input
#учимся создавать матрицы значений и переменных !!! ВАЖНО!!! + ренжу усвоить попозже на этой функции
ratings <- c(4.2, 4.4, 3.4, 3.9, 5, 4.1, 3.2 ,3.9, 4.6, 4.8, 5, 4, 4.5, 3.9, 4.7, 3.6)
#ratings add line б
employee.mat <- matrix(ratings, byrow = TRUE, nrow = 4, dimnames = list(c("Quarter1", "Quarter2", "Quarter3",  "Quarter4" ), c("Hari", "Shri", "John", "Albert")))
employee <- as.data.frame(employee.mat)
employee
result <- apply(employee, 2, sum)
result
class(result)
result <- apply(employee, 1, cumsum)
result
cumsum(1:10) # сумма диапазона тоже что и итерационная сумма чисел
cumprod(1:10) # факториал
check<-function(x){# создаем функциюю проверки
  return(x[x>4.2])
}
result <- apply(employee, 2, check)
result
#-----------------laply() func------------------
#case 1. vector as an input argument
ratings
# выводит лист
result <- lapply(ratings, mean)
result
#case 2
list1 <- list(maths=c(64, 45, 89, 67), english=c(79, 84, 62, 80), physics=c(68, 72, 69, 80), chemistry = c(99, 91,84, 89))
list1
result <- lapply(list1, mean)
result
class(result)
check<-function(x){
  return(x[x>75])
}
result <- lapply(list1, check)
result
class(result)
#case 3 dataframe as an input argument
result <- lapply(employee, sum)
result
result <- lapply(employee, cumsum) #накопителья сумма, а, а+б, а+б+с
result
check <- function(x){
  return(x[x>4.2])
}
result <- lapply(employee, check )
result
#----------------tapply() func --------------
salary <- c(21000, 29000, 32000, 34000, 45000)
desig <- c("Programmer", "Senior Programmer", "Senior Programmer", "Senior Programmer", "Manager")
gender <- c("M", "F", "F", "M", "M")
result <- tapply(salary, desig, mean)
result
class(result)
result <- tapply(salary, list(desig, gender), mean)
result
class(result)
#-------------- by() func --------------
result <- by(salary, desig, mean)
result
class(result)
result[2]
as.list(result)#Функции для создания, приведения и проверки обоих типов списков R
result <- by(salary, list(desig, gender), mean)
result
class(result)
#-------------maply() func -------------- m значит многомерный
result <- mapply(rep, 1:4, 4:1, 6:9)
result
class(result)
result <- mapply(rep, 1:4, 4:4)
result
class(result)
fix(college)
library(MASS)
'''

'''
Boston
fix(Boston)
plot(Boston$chas,Boston$age)
hist(Boston$chas)
hist(Boston$age)
hist(Boston$black)
hist(Boston$indus)
hist(Boston$crim)
hist(Boston$tax, 20)
# (d)
par(mfrow=c(1,3))
hist(Boston$crim[Boston$crim>1], breaks=25)
plot(Boston, Boston$crim)
plot(Boston$ptratio, Boston$tax)
plot(Boston, Boston$chas)
# (e) #subset возвращает вектор
dim(subset(Boston, chas ==1))
#f
median(Boston$ptratio)
hist(Boston$medv)
#самое низкое медиальное значение занятых домой
t(subset(Boston, medv == min(Boston$medv)))
hist(Boston$rm, 20)
dim(subset(Boston, rm>7))
dim(subset(Boston, rm>8))
