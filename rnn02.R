#ライブラリ
library(prophet)
library(tidyverse)
library(ggplot2)

#データ読込
df <-read.csv("international-airline-passengers.csv", stringsAsFactors = FALSE)

#データ前処理
names(df)[1] <- "ds"
names(df)[2] <- "y"

df$ds <- paste(df$ds, "-01", sep = "")
df$ds <- as.Date(df$ds)
df <- na.omit(df)

df$y <- log(df$y)

#plot(df$y)

#テストデータと検証データの分割
train_range <- (1:103)
df_train <- df[train_range, ]
df_val <- df[-train_range, ]

#予測
m <- prophet(df_train, 
             yearly.seasonality = TRUE,
             weekly.seasonality = FALSE,
             daily.seasonality = FALSE)

future <- make_future_dataframe(m, periods = 41, freq = 'month')
forecast <- predict(m, future)

#可視化
plot(m, forecast)

#精度検証
res <- forecast %>% select(ds, yhat)

#可視化用データフレーム
df_viz <- res %>% mutate(y = yhat, lab = "yhat") %>% select(ds, y, lab)
df$lab <- "y"
df_viz <- rbind(df_viz, df)

#精度
res <- res[-train_range, ]
mean((res$yhat - df_val$y)^2)
mean(abs(exp(res$yhat) - exp(df_val$y)))

#可視化
g <- ggplot(df_viz, aes(x = ds, y = exp(y), group = lab, colour = lab))
(g <- g + geom_line())


