library("MASS")
library("ISLR")
'''
crim - скорость роста на душу населения, по городам
zn доля земель под жилую застройку зонированных по учатскам свыше 25 000 кв. фунт
indus - промышленная  коммерческая доля акров, не предназначенных для продаж
chas - граница с рекой Чаз, если =1 то участок граничит
nox - косентрация оксидов азота nox ( частей на 10 миллионов)
rm - среднее колличевство комнат в доме
age - дом заменимаемый владельцами, построенных  до 1945 (единиц )
dis - средневзвешанное растоение от центра ( до 5 рабочих единиц )
rad - индекс доступности радиальных автомобильных дорог
tax - ставка налога на имущество  в размере полной стоимости /$10,000
ptratio - соотношение учеников и учителей по городам
black - 1000(Bk - 0.63)^2 где BK это процент черных домов
lstat - более низкий статус населения (проценты)
medv - медиальная стоимость занятых домов владельцами в /$1000s
'''
fix(Boston)
#lm() ленийная регрессия, medv отклик, a lstat предиктор. lm( y ~ x, data), y это зависимая переменная, х - предиктор, data - набор данных.
lm.fit = lm(medv ~ lstat)
lm.fit = lm(medv ~ lstat, data = Boston)
attach(Boston) # тоже что и $
#detach(Boston) if two or big "attach" , back
lm.fit = lm(medv ~ lstat)
#основная информация по модели down
lm.fit
#более детальная информация down
summary(lm.fit)
# по коэф П, можно сделать вывод что средняя стоимость жилья напрямую связанна с колличеством бедного населения
#узнаем что еще хранится в переменной благодаря names()
names(lm.fit)
#можем обратиться по lm.fit$coefficients, но надежнее будет coef()
coef(lm.fit)
#для вычесления доверительных интервалов оценок коэ мы можем использовать confint()
confint(lm.fit)
#predict() можно использовать для рассчета доверетильных интервалов и интервалов предсказаний при прогнозировании medv по за заданому значению lstat
# таблица соотвествует 5, 10, 15 где 10 является, medv(fit) = 25.05 при lstat = 10, централизованная точка
predict(lm.fit, data.frame(lstat = (c(5, 10, 15))), interval = "confidence")
predict(lm.fit, data.frame(lstat = (c(5, 10, 15))), interval = "prediction")
#95% доверительный интервал связан с значением lstat = 10, (24.47, 25. 63), а 95% предксказния (12.828, 37.28)
#теперь изобразим lstat и medv вместе с регриссионой прямой используя функцию plot(), abline()
plot(lstat, medv) + abline(lm.fit)
plot(lstat, medv) + abline(lm.fit, lwd = 3)
plot(lstat, medv) + abline(lm.fit, lwd = 3, col = "blue")
plot(lm(medv ~ lstat, data = Boston))
plot(lm(medv ~ lstat, data = Boston), col = "red",pch = 20 )
plot(lstat, medv, col = "red") + abline(lm.fit, lwd = 3, col = "blue")
plot(lstat, medv, col = "red", pch = 20) + abline(lm.fit, lwd = 3, col = "blue")
plot(lstat, medv, col = "red", pch = "+") + abline(lm.fit, lwd = 3, col = "blue")
plot(1:20, 1:20, pch = 1:20)
par(mfrow = c(2, 2))
plot(lm.fit)
#в качевстве альтернативы мы можем расчитать остатки л. регрессии residuals(), rstudent() - выдаст стьюдитизированные остатки
plot(predict(lm.fit), residuals(lm.fit)) + abline(lm.fit, lwd = 3, col = "blue")
plot(predict(lm.fit), rstudent(lm.fit)) + abline(lm.fit, lwd = 3, col = "blue")
residuals(lm.fit)
rstudent(lm.fit)
#показатель разбалансировки
plot(hatvalues(lm.fit))
#максимум
which.max(hatvalues(lm.fit))
#множественная линейная регрессия, просто + переменная для мн.рег.
lm.fit = lm(medv ~ lstat + age, data = Boston)
summary(lm.fit)
#~. включить все переменные
lm.fit = lm(medv ~ ., data = Boston)
summary(lm.fit)
#доступ к отдельным значениям
summary(lm.fit)$r.sq
summary(lm.fit)$sigma
#library('car') - vif() даст значения фактора инфляции дисперсии, нужно install.packages()
# -age, удаляем переменные из списка для регрессионого анализа
lm.fit1 = lm(medv~., -age, data = Boston)
summary(lm.fit1)
# альтернатива
lm.fit1 = update(lm.fit, ~. -age)
#lstat:black включить синегрию между лстат и блек, lstate*age синергия + адаптивные переменные(lstate + age + lstate:age)
summary(lm(medv~lstat*age, data = Boston))
#неленейное преобразование предиктов I(X^2), создает неленейный предиктор, I() позволяте изолировать перменную и не ломать всю регрессию
lm.fit2 = lm(medv ~ lstat + I(lstat^2))
summary(lm.fit2)
#anove() для колличевственного описания степени приемущества квадратичной модели над линейной
lm.fit = lm(medv ~ lstat)
anova(lm.fit, lm.fit2)
#Но = две модели описывают одинаково, На =  полная модель описывает лучше, F = 135; Pr<0.0000...7;
#Это является четким доказательтсва превосходства полной модели над линейной*
plot(lm.fit2, col = "blue")
#для создания полинома высшей формы, poly(lstat, 5)
lm.fit5 = lm(medv ~ poly(lstat, 5))
summary(lm.fit5)
#попробуем лог для полинома log(rm)
summary(lm(medv~log(rm), data = Boston))
fix(Carseats)
'''
Carseats
‘Sales’ Unit sales (in thousands) at each location
‘CompPrice’ Price charged by competitor at each location
‘Income’ Community income level (in thousands of dollars)
‘Advertising’ Local advertising budget for company at each
location (in thousands of dollars)
‘Population’ Population size in region (in thousands)
‘Price’ Price company charges for car seats at each site
‘ShelveLoc’ индикатор качевства расположения стеллажа, место где выставленное сидение ‘Bad’, ‘Good’ and ‘Medium’
indicating the quality of the shelving location for the car
seats at each site
‘Age’ Average age of the local population
‘Education’ Education level at each location
‘Urban’ A factor with levels ‘No’ and ‘Yes’ to indicate whether
the store is in an urban or rural location
‘US’ A factor with levels ‘No’ and ‘Yes’ to indicate whether the
store is in the US or not
'''
names(Carseats)
#добавляем некоторые чавственные переменные и синергию между
lm.fit = lm(Sales ~. + Income:Advertising + Price:Age, data = Carseats)
summary(lm.fit)
#contrasts() - возвращает матрицу, коддирования R индикаторных переменных
attach(Carseats)
contrasts(ShelveLoc)
#учимся создавать свои функции
LoadLibraries()
LoadLibraries = function(){
  library(ISLR)
  library(MASS)
  print("The libraries have been loaded")
}
LoadLibraries()
setwd('/home/iakov/Documents/program_with_r/stat-learning/data')
getwd()
Auto = read.csv('Auto.csv', header = T, na.strings="?" )
fix(Auto)
Auto = na.omit(Auto)
summary(Auto)
attach(Auto)
lm.fit = lm(mpg ~ horsepower)
summary(lm.fit)
'''
### i.
да есть, об этом говорит коэф п,близко к нулю Hо отклоняем,
и F статистика намного больше чем 1.
### ii.
Для вычисления остаточной ошибки относительно отклика мы используем среднее значение
ответа и RSE. Среднее значение миль на галлон - это `` r mean (mpg, na.rm = T) ''.
RSE lm.fit было `` r 4,906 '', что указывает на погрешность в процентах
`` r 4,906 / среднее (миль на галлон, na.rm = T) * 100,0 ''%. $ R ^ 2 $ lm.fit был
о `` r summary (lm.fit) $ r.sq``, что означает `` r summary (lm.fit) $ r.sq * 100.0``%
Разница в расходах на галлон объясняется мощностью в лошадиных силах.
#iii.
Отношение между расходом топлива на галлон и мощностью отрицательное. Чем больше лошадиных сил
у автомобиля, в  линейнейной регрессии, указывает на меньшую топливную эффективность, которая будет милю на галлон
у автомобиля.
'''
predict(lm.fit, data.frame(horsepower=c(98)), interval = "confidence")
predict(lm.fit, data.frame(horsepower=c(95)), interval = "prediction")
'''
интервал находится в среднем вокруг 24.0
'''
plot(horsepower, mpg) + abline(lm.fit,col = "red")
plot(Auto,mpg)
#диаграмма рассяния по данным auto
pairs(Auto)
#матрица рассеяния кроме name
cor(subset(Auto, select=-name))
lm.fit1 = lm(mpg ~ . -name, data=Auto)
summary(lm.fit1)
'''
1) имеется ли связь между предиктом и откликом.?Да сущестует взаимосвязь, так как Ф коэф высокий(далеко от 0, значит нулевая гипотеза не применима), коэф п маленький.
2)Какие предикторы организуют статически связь с откликом . Displacement, weight, year, origin, discpalament
3)Коэффициент регрессии для года «r коэффициенты (lm.fit1) [« год »]« предполагает, что каждый год миль на галлон увеличивается на коэффициент. Другими словами, автомобили становятся более экономичными с каждым годом почти на 1 милю на галлон в год.
'''
#строим диагностический график модели л.рег.
plot(lm.fit1)
'''
Подгонка не кажется точной, потому что на графиках остатков имеется отчетливый образец кривой.
Судя по графику кредитного плеча, точка 14, по-видимому, имеет высокий левередж,
 хотя и невысокую остаточную величину.
'''
#график остатков
plot(predict(lm.fit1), rstudent(lm.fit1))
#Как видно на графике стьюдентизированных остатков,
#возможны выбросы, потому что есть данные со значением больше 3.
lm.fit2 = lm(mpg ~ year*weight + year*origin)
summary(lm.fit2)
#я выбрал переменные с высокой корреляцией с предиктом(по коэф п), после добавил силекцию и получил высокие коэф корреляции
#данная модель объясняет 0.85\1 данных. Однако коэф немного упал относительно переменной. я удалил dicplament<потерял объяснимость но получил точную корреляцию предикта
#очень высокая F статистика, не знаю что это значит но в я шоке
#применив log(mpg) мы достигли коэф Р^2 ~0.90, стд 2, и p < 2.2e-16, best model ever
lm.fit3 = lm(log(mpg)~log(weight)+sqrt(origin)+year+I(year^2)+sqrt(horsepower))
summary(lm.fit3)
#работа из rmd
lm.fit4 = lm(log(mpg)~log(weight)+sqrt(horsepower)+acceleration+I(acceleration^2))
summary(lm.fit4)
plot(lm.fit3)
plot(lm.fit4)
plot(predict(lm.fit4), rstudent(lm.fit4))
plot(predict(lm.fit3), rstudent(lm.fit3))


'''
========================================part 2=====================================
'''
library(ISLR)
summary(Carseats)
'''
IF (""(A + B)""):
  Intuitif.
IF ("+A" or "-A"):
  Statistic.
Фрейм данных с 400 наблюдениями по следующим 11 переменным.

Смоделированный набор данных,
содержащий данные о продажах детских автокресел в 400 различных магазинах.

     ‘Sales’ Штучные продажи (в тысячах) в каждом месте

     ‘CompPrice’ Цена, взимаемая конкурентом в каждом месте
"(CompPrice:Population + CompPrice:Price + CompPrice:US)"
     ‘Income’ Уровень дохода сообщества (в тысячах долларов)
""(Income:Population + Income:Age)""
     ‘Advertising’ Бюджет местной рекламы для компании в каждом месте (в тысячах долларов)
""(Advertising:Price + Advertising:CompPrice + Advertising:ShelveLoc)""
    "-" ‘Population’ Численность населения в регионе (в тысячах)
     ‘Price’ Цены компании на автокресла на каждом сайте

  кач   ‘ShelveLoc’ Фактор с уровнями «Плохо», «Хорошо» и «Средний».
          с указанием качества расположения стеллажей для автокресел на каждой площадке
"+" Price:ShelveLoc
     ‘Age’ Средний возраст местного населения
""(Age:Education + Age:Urban )""
     "-"‘Education’ Уровень образования в каждом месте

    кач  "-"‘Urban’ Фактор с уровнями «Нет» и «Да»,
     чтобы указать, находится ли магазин в городе или в сельской местности.

    кач  "-"‘US’ Фактор с уровнями «Нет» и «Да», чтобы указать, находится ли магазин в США или нет.
'''
#best model ever #  + sqrt(Advertising):log(Price) + I(Age^2):I(Education^-2)  + Age:ShelveLoc + I(Age^2)
lm.fitK_b = lm(Sales ~. -Population -Education -Urban -US -CompPrice + Sales:Price + Price:ShelveLoc +Price:Income+ Price:Age + Price:Advertising + I(CompPrice^2):sqrt(Price)  , data = Carseats)
lm.fitK_b1 = lm(Sales ~. -Population -Education -Urban -US -CompPrice+ Price:ShelveLoc +Price:Income+ Price:Age + Price:Advertising + I(CompPrice^2):sqrt(Price)  , data = Carseats)

lm.fitK_b2 = lm(Sales ~. -Population -Education -Urban -US  + Sales:Price + Price:ShelveLoc + sqrt(Advertising):log(Price) + I(Age^2):I(Education^-2)  + Age:ShelveLoc + I(Age^2) +Price:Income+ Price:Age + Price:Advertising + I(CompPrice^2):sqrt(Price)  , data = Carseats)
confint(lm.fitK_b1)
confint(lm.fitK_b)
summary(lm.fitK_b)
summary(lm.fitK_b1)

plot(lm.fitK_b)#модель с 97%
plot(lm.fitK_b1)
artem = lm(Sales ~., data = Carseats)
summary(artem)
plot(artem)
lm.fit_b1 = lm(Sales ~ -Age -Population -Education -Urban -US -CompPrice - Income -ShelveLoc -Price  + Advertising:Price + I(Age^2) + Price:ShelveLoc + ShelveLoc:Age + Price:Age + I(CompPrice^2):sqrt(Price) + sqrt(Advertising):log(Price) + I(Age^2):I(Education^-2), data = Carseats  )
summary(lm.fit_b1)


plot(lm.fitK_b)

lm.fit = lm(Sales ~ Price+Urban+US, data = Carseats)
summary(lm.fit)


#синергичная модель по моему мнению(bad)
lm.fitSin = lm(Sales ~  log(CompPrice):log(Price) + Advertising:Price + Advertising:CompPrice   + Age:Education , data = Carseats)
summary(lm.fitSin)

plot(lm.fitK)

lm.fitK1 = lm(Sales ~ Income:Population + Income:Price + Price:CompPrice + Income:Age + Income:Education + poly(Sales, 2) , data = Carseats)
summary(lm.fitK1)


#---------------------------------------------------------
lm.fit_s = lm(Sales ~ Price  + Urban + US , data = Carseats)
summary(lm.fit_s)
plot(lm.fit_s)
#дов интервал
confint(lm.fit_s)
#выбросы
plot(predict(lm.fit_s), rstudent(lm.fit_s))
attach(Carseats)
plot(Carseats)
#-----------------------11------------------------------------------
set.seed(1)
x = rnorm(100)
y = 2*x + rnorm(100)
lm.fit.x = lm(y~x+0)
summary(lm.fit.x)
#п значение низко к нулю, нулевая гипотеза отменяется
# формула Т статистики (sqrt(length(x)-1) * sum(x*y)) / (sqrt(sum(x*x) * sum(y*y) - (sum(x*y))^2))
lm.fit_1 =lm(x~y)
lm.fit_2 =lm(y~x)
summary(lm.fit_1)
summary(lm.fit_2)
#есть отклонение в стандартной ошибке
set.seed(1)
x = rnorm(100)
y = 2*x
lm.fit = lm(y~x+0)
lm.fit2 = lm(x~y+0)
summary(lm.fit)
summary(lm.fit2)
set.seed(1)
x <- rnorm(100)
y <- -sample(x, 100)
sum(x^2)
sum(y^2)
lm.fit <- lm(y~x+0)
lm.fit2 <- lm(x~y+0)
summary(lm.fit)
summary(lm.fit2)
a <- rnorm(100)
eps <- rnorm(100, 0, sqrt(0.25))
y = -1+0.5*x + eps
plot(x,y)
Я наблюдаю линейную зависимость между x и y с положительным наклоном и ожидаемой дисперсией.
lm.fit = lm(y~x)
summary(lm.fit)
plot(x,y)  + abline(lm.fit)
plot(x, y) + abline(lm.fit, lwd=3, col=2) + abline(-1, 0.5, lwd=3, col=3)+ legend(-1, legend = c("model fit", "pop. regression"), col=2:3, lwd=3)
lm.fit_sq = lm(y~x+I(x^2))
summary(lm.fit_sq)
set.seed(1)
eps1 = rnorm(100, 0, 0.125)
x1 = rnorm(100)
y1 = -1 + 0.5*x1 + eps1
plot(x1, y1)+ abline(lm.fit1, lwd=3, col=2) + legend(-1, legend = c("model fit", "pop. regression"), col=2:3, lwd=3) + abline(-1, 0.5, lwd=3, col=3)
lm.fit1 = lm(y1~x1)
summary(lm.fit1)
set.seed(1)
eps2 = rnorm(100, 0, 0.5)
x2 = rnorm(100)
y2 = -1 + 0.5*x2 + eps2
plot(x2, y2) + abline(lm.fit2, lwd=3, col=2) + abline(-1, 0.5, lwd=3, col=3) + legend(-1, legend = c("model fit", "pop. regression"), col=2:3, lwd=3)
lm.fit2 = lm(y2~x2)
summary(lm.fit2)
confint(lm.fit)
confint(lm.fit1)
confint(lm.fit2)
set.seed(1)
x1 = runif(100)
x2 = 0.5 * x1 + rnorm(100)/10
y = 2 + 2*x1 + 0.3*x2 + rnorm(100)
x1
cor(x1, x2)
plot(x1, x2)
lm.fit = lm(y~x1+x2)
summary(lm.fit)
lm.fit = lm(y~x1)
summary(lm.fit)
lm.fit = lm(y~x2)
summary(lm.fit)
x1 = c(x1, 0.1)
x2 = c(x2, 0.8)
y = c(y, 6)
lm.fit1 = lm(y~x1+x2)
summary(lm.fit1)
lm.fit2 = lm(y~x1)
summary(lm.fit2)
lm.fit3 = lm(y~x2)
summary(lm.fit3)
par(mfrow=c(2,2))
plot(lm.fit1)
a <- qnorm(seq(0.01,0.99,0.01))
plot(a)
b <- quantile(rnorm(200),probs = seq(0.01,0.99,0.01))
plot(b)
y <- qunif(ppoints(length(randu$x)))
qqplot(randu$x,y)
y <- qunif(ppoints(length(randu$x)))
plot(qqplot(randu$x, y))
plot(qqplot(qnorm(ppoints(30)), qchisq(ppoints(30),df=3)))
plot(qqplot(qnorm(ppoints(30)), qcauchy(ppoints(30))))
data1 = Boston
fix(data1)
fix(Boston)
'''
Этот фрейм данных содержит следующие столбцы:

     ‘crim’ уровень преступности на душу населения по городам.

     ‘zn’ доля земли под жилую застройку зонирована под участки площадью более 25 000 кв. футов.

     ‘indus’ доля акров, не относящихся к розничной торговле, на город.

     ‘chas’Фиктивная переменная Charles River (= 1, если участок ограничивает реку; 0 в противном случае).
река не влияет на преступность, или слишком слабо что бы вносить ее без доп переменных в модель
     ‘nox’концентрация оксидов азота (частей на 10 млн).
+"1"
     ‘rm’ среднее количество комнат в доме.
+"2**"
     ‘age’ доля занятых владельцами единиц, построенных до 1940 года.
+"2"
     ‘dis’средневзвешенное расстояние до пяти бостонских центров занятости.
+
     ‘rad’ индекс доступности радиальных автомобильных дорог.

     ‘tax’ полная ставка налога на имущество за каждые 10 000 долларов.
+"0"
     ‘ptratio’ соотношение учеников и учителей по городам.
+"1"
     ‘black’ 1000 (Bk - 0,63) ^ 2, где Bk - доля черных по городу.
+=
     ‘lstat’более низкий статус населения (в процентах).
+=
     ‘medv’ медианная стоимость домов, занимаемых владельцами, составляет 1000 долларов.

'''
attach(Boston)
lm.boston1 = lm(crim ~., data = data1)
plot(lm.boston1)
summary(lm.boston1)
lm.bs1 = lm(crim ~. -zn - indus - dis - rad - black - lstat - medv, data = data1)
plot(lm.bs1)
summary(lm.bs1)#tax
lm.bs2 = lm(crim ~. -tax -zn - indus - dis - rad - black - lstat - medv, data = Boston )
plot(lm.bs2)
summary(lm.bs2) #nox +  ptratio
lm.bs3 = lm(crim ~. -nox -ptratio -tax -zn - indus - dis - rad - black - lstat - medv, data = Boston )
summary(lm.bs3)
plot(lm.bs3)
lm.bs3 = lm(crim ~. -tax -zn -nox -rm -ptratio - indus - dis - rad - black - lstat - medv, data = Boston )
summary(lm.bs3)#chas + rm ^2 ???
lm.bs4 = lm(crim~chas)
summary(lm.bs4)
lm.beB = lm(crim ~. +nox:ptratio +age:rm + , data = Boston)
summary(lm.beB)




#===================================================================
library(MASS)
summary(Boston)
Boston$chas <- factor(Boston$chas, labels = c("N","Y"))
summary(Boston)
attach(Boston)
lm.zn = lm(crim~zn)
summary(lm.zn) # yes
lm.indus = lm(crim~indus)
summary(lm.indus) # yes
lm.chas = lm(crim~chas)
summary(lm.chas) # no
lm.nox = lm(crim~nox)
summary(lm.nox) # yes
lm.rm = lm(crim~rm)
summary(lm.rm) # yes
lm.age = lm(crim~age)
summary(lm.age) # yes
lm.dis = lm(crim~dis)
summary(lm.dis) # yes
lm.rad = lm(crim~rad)
summary(lm.rad) # yes
lm.tax = lm(crim~tax)
summary(lm.tax) # yes
lm.ptratio = lm(crim~ptratio)
summary(lm.ptratio) # yes
lm.black = lm(crim~black)
summary(lm.black) # yes
lm.lstat = lm(crim~lstat)
summary(lm.lstat) # yes
lm.medv = lm(crim~medv)
summary(lm.medv) # yes

#Все, кроме час. Постройте каждую линейную регрессию, используя «plot (lm)», чтобы увидеть остатки.

lm.all = lm(crim~., data=Boston)
summary(lm.all)

x = c(coefficients(lm.zn)[2],
      coefficients(lm.indus)[2],
      coefficients(lm.chas)[2],
      coefficients(lm.nox)[2],
      coefficients(lm.rm)[2],
      coefficients(lm.age)[2],
      coefficients(lm.dis)[2],
      coefficients(lm.rad)[2],
      coefficients(lm.tax)[2],
      coefficients(lm.ptratio)[2],
      coefficients(lm.black)[2],
      coefficients(lm.lstat)[2],
      coefficients(lm.medv)[2])
y = coefficients(lm.all)[2:14]
plot(x, y)
'
Коэффициент для nox составляет примерно -10 в одномерной модели и 31 в модели множественной регрессии.
'

lm.zn = lm(crim~poly(zn,3))
summary(lm.zn) # 1, 2
lm.indus = lm(crim~poly(indus,3))
summary(lm.indus) # 1, 2, 3
# lm.chas = lm(crim~poly(chas,3)) : qualitative predictor
lm.nox = lm(crim~poly(nox,3))
summary(lm.nox) # 1, 2, 3
lm.rm = lm(crim~poly(rm,3))
summary(lm.rm) # 1, 2
lm.age = lm(crim~poly(age,3))
summary(lm.age) # 1, 2, 3
lm.dis = lm(crim~poly(dis,3))
summary(lm.dis) # 1, 2, 3
lm.rad = lm(crim~poly(rad,3))
summary(lm.rad) # 1, 2
lm.tax = lm(crim~poly(tax,3))
summary(lm.tax) # 1, 2
lm.ptratio = lm(crim~poly(ptratio,3))
summary(lm.ptratio) # 1, 2, 3
lm.black = lm(crim~poly(black,3))
summary(lm.black) # 1
lm.lstat = lm(crim~poly(lstat,3))
summary(lm.lstat) # 1, 2
lm.medv = lm(crim~poly(medv,3))
summary(lm.medv) # 1, 2, 3
'
См. Встроенные комментарии выше, для большинства ответ положительный, кроме black и chas.
'
