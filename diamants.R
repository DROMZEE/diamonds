library(dplyr)
library(ggplot2)
library(lattice)
diamonds <- ggplot2::diamonds %>% mutate_if(is.factor, ~ factor(., ordered = FALSE))

ls(pattern = '^geom_', env = as.environment('package:ggplot2'))

diamonds
str(diamonds)
help(diamonds)
dim(diamonds)
head(diamonds)
#data(diamonds)
summary(diamonds)


ggplot(diamonds, aes(x = cut, fill = cut)) + geom_bar()
# http://fermin.perso.math.cnrs.fr/Files/ggplot_visu.html


mean(diamonds$price)
median(diamonds$price)
summary(diamonds$price)
sum(diamonds$price < 500)
diamonds %>% filter(price <500 ) %>% count()
diamonds %>% filter(price <250 ) %>% count()
diamonds %>% filter(price > 15000 ) %>% count()
# histogramme de la variable continue price
ggplot(data=diamonds, aes(x=price)) + geom_histogram(binwidth=500) 
ggplot(data=diamonds, aes(x=price)) + geom_histogram(binwidth=500,boundary = 0) 
ggplot(data=diamonds, aes(x=price)) + geom_histogram(aes(y = ..density..), binwidth=100) + xlim(0,2500)
ggplot(data=diamonds, aes(x=price)) + geom_density()
ggplot(data=diamonds, aes(x=price)) + geom_density(adjust = 0.1)
ggplot(diamonds,aes(x = price, color = cut)) +geom_density()
ggplot(diamonds,aes(x = price, color = cut)) +geom_density() +facet_wrap(~ cut)
ggplot(diamonds,aes(x=price)) +geom_histogram(aes(fill = clarity))

diamonds %>% 
  count(color, cut) %>%  
  ggplot(mapping = aes(x = color, y = cut)) +
  geom_tile(mapping = aes(fill = n))


ggplot(data = diamonds) +
  geom_point(mapping = aes(x = carat, y = price))

ggplot(diamonds, aes(x=clarity, fill=cut)) + 
  geom_bar(position="dodge") + 
  theme(legend.position="NULL")

ggplot(diamonds, aes(x="", fill=clarity)) + 
  geom_bar() + 
  coord_polar(theta = "y") 

ggplot(data = diamonds, mapping = aes(x = cut, y = price)) +
  geom_violin() +
  coord_flip()

####
#! a adapter a diamonds
p1 <- ggplot(ames, aes(x = Sale_Price, color = Overall_Qual)) +
  geom_freqpoly() +
  scale_x_log10(breaks = c(50, 150, 400, 750) * 1000, labels = scales::dollar)

p2 <- ggplot(ames, aes(x = Sale_Price, color = Overall_Qual, fill = Overall_Qual)) +
  geom_density(alpha = .15) +
  scale_x_log10(breaks = c(50, 150, 400, 750) * 1000, labels = scales::dollar)

gridExtra::grid.arrange(p1, p2, nrow = 2)



#

ggplot(data=diamonds,aes(x=cut,y=price,color=cut)) + 
  geom_point(position = "jitter")


ggplot(diamonds, aes(x=carat, y=price)) + 
  geom_point(aes(color= cut))


####

# On crée un échantillon de test et un échantillon d'entraînement : 
coeff = 0.8
train <- sample(seq_len(nrow(diamonds)), size = coeff * nrow(diamonds))
diam_train <- diamonds[train, ]
diam_test <- diamonds[-train, ]
install.packages("caret", dependencies = c("Depends", "Suggests"))


model_train <- lm(data = diam_train, 
                     price ~ carat + cut + color + clarity + depth + table + x + y +  z)

diam_test$price_pred <- predict(model_train, newdata = diam_test)

diam_test %>% mutate(error_squared = abs(price_pred - price)) %>% 
  summarise_at(vars(error_squared), 
               list(moy = mean, min = min, med = median, max = max, sd = sd), na.rm = T)


#### validation croisée


train.control <- caret::trainControl(method = "cv", number = 10)

# On entraîne le modèle
model_cv <- caret::train(price ~ carat + cut + color + clarity + depth + table + x + y +  z, 
                         data = diamonds, 
                         na.action  = na.pass,
                         method = "lm",
                         trControl = train.control)
