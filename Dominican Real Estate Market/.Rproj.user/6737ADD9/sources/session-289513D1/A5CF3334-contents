library(tibble)
library(readr)
library(dplyr)

words <- c("Asim", "Asghar", 14000, 13191397438)
words <- c("Asim")


numeric_conversion <- function(word){
  word <- as.character(word)
  
  characters <- c(0:9, LETTERS, c("Ñ", "Á", "É", "Í", "Ó", "Ú", "!", " ", ".",
                                  ",", "#", "$", "%", "&", "/", "(", ")", "-"))
  n <- length(characters)
  
  alphavalues <- tibble(characters = characters, value = 1:n)
  
  radix <- 0
  large_prime <- 731
  split_word <- unlist(sapply(word, strsplit, ""))
  m <- o <- length(split_word) - 1
  
  for (i in 0:m){
    letter <- toupper(split_word[i + 1])
    
    value <- unname(unlist(alphavalues[alphavalues$characters == letter, "value"]))
    
    radix <- radix + ((36 ^ (o)) * value) %% large_prime
    o <- o - 1
  }
  #names(radix) <- "radix"
  
  return(radix = radix)
}

num_parser <- function(obs) {
  return(sapply(obs, numeric_conversion))
}
num_parser(neighborhood$description)
neighborhood <- read_csv("./0_Data Collection/1_data/0_raw/neighborhoods.csv")
neighborhood <- neighborhood %>%
  mutate(province2 = num_parser(province),
         id2 = num_parser(id),
         description2 = num_parser(description))

neighborhood <- neighborhood %>%
  filter(between(description2, 0, 11000))
neighborhood.scaled <- scale(neighborhood["description2"])

fviz_nbclust(neighborhood.scaled, kmeans, method = "wss") +
  geom_vline(xintercept = 3, linetype = 2)

kmeans(neighborhood["description2"])

train <- train %>%
  mutate(parking2 = num_parser(parking * 10),
         bathrooms2 = num_parser(bathrooms * 10),
         bedrooms2 = num_parser(bedrooms * 10),
         location2 = num_parser(location),
         status2 = num_parser(status),
         price2 = num_parser(price),
         area2 = num_parser(area))

train.scaled <- train %>%
  mutate(area = round(area, -1)) %>%
  select(bedrooms, location2, price, area) %>%
  scale

fviz_nbclust(train.scaled, kmeans, method = "wss") +
  geom_vline(xintercept = 3, linetype = 2)


train$cluster <- kmeans(as.matrix(train.scaled), 500, nstart = 50)$cluster

train_final <- train %>%
  group_by(cluster) %>%
  summarise(parking = median(parking),
            bathrooms = median(bathrooms),
            bedrooms = median(bedrooms),
            price = mean(price),
            area = mean(area))

shapiro.test(log(train_final$price))
shapiro.test(log(train_final$area))

ggplot(train_final, aes(log(price))) +
  geom_histogram()
ggplot(train_final, aes(log(area))) +
  geom_histogram()
fit.2 <- lm(log(price) ~ log(area), data = train_final)
summary(fit.2)
shapiro.test(residuals(fit.2))
