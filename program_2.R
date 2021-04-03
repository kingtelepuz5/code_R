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
Фрейм данных с 400 наблюдениями по следующим 11 переменным.

Смоделированный набор данных,
содержащий данные о продажах детских автокресел в 400 различных магазинах.

     ‘Sales’ Штучные продажи (в тысячах) в каждом месте

     ‘CompPrice’ Цена, взимаемая конкурентом в каждом месте
+ Population + Price 
     ‘Income’ Уровень дохода сообщества (в тысячах долларов)
+ Population + Age
     ‘Advertising’ Бюджет местной рекламы для компании в каждом месте (в тысячах долларов)
+ Price
     ‘Population’ Численность населения в регионе (в тысячах)

     ‘Price’ Цены компании на автокресла на каждом сайте

     ‘ShelveLoc’ Фактор с уровнями «Плохо», «Хорошо» и «Средний».
          с указанием качества расположения стеллажей для автокресел на каждой площадке

     ‘Age’ Средний возраст местного населения

     ‘Education’ Уровень образования в каждом месте
     ‘Urban’ Фактор с уровнями «Нет» и «Да»,
     чтобы указать, находится ли магазин в городе или в сельской местности.

      ‘US’ Фактор с уровнями «Нет» и «Да», чтобы указать, находится ли магазин в США или нет.
'''
lm.fitK = lm(Sales ~., data = Carseats)
plot(lm.fitK)
summary(lm.fitK)
lm.fitK1 = lm(Sales ~ Income:Population + Income:Price + Price:CompPrice + Income:Age + Income:Education, data = Carseats)
summary(lm.fitK1)
