

# 簡単な分析事例 ---------------------------------------------------------------

# 1行目：データの読み込み
# 2行目：回帰分析の実施
# 3行目：分析結果の確認

# 線形回帰モデル
beer_data <- read.csv("011-beer.csv")
mod_beer <- lm(beer ~ temperature, data = beer_data)
summary(mod_beer)


# ロジスティック回帰モデル
study_data <- read.csv("012-study-hours.csv")
mod_logistic <- glm(result ~ hours, data = study_data, family = "binomial")
summary(mod_logistic)



# 分析の準備 -------------------------------------------------------------------

# ライブラリの読み込み(毎回の分析の前に1回だけ実行)
library(ggplot2)
library(dplyr)


# データの読み込みと集計 -----------------------------------------------------------------

# データの読み込み
beer_data <- read.csv("011-beer.csv")

# データの先頭行
head(beer_data)

# サンプルサイズ
nrow(beer_data)

# データの確認
View(beer_data)

# 散布図
plot(
  beer ~ temperature, 
  data = beer_data
)

# きれいな散布図
ggplot(data = beer_data,
       aes(x = temperature, y = beer)) +
  geom_point()

# 回帰分析の実施
mod_beer <- lm(
  beer ~ temperature, # formula
  data = beer_data    # データ
)

# 分析結果の要約
summary(mod_beer)

# ggplot2ライブラリを利用して回帰直線を描く
ggplot(data = beer_data,
       aes(x = temperature, y = beer)) +
  geom_point() + 
  geom_smooth(method = "lm") # geom_smoothで回帰直線を描く

# 予測
# 気温20度の時のビールの売り上げ
predict(mod_beer, newdata = data.frame(temperature = 20))

# 回帰係数を使った予測
coef(mod_beer)
coef(mod_beer)[1] + coef(mod_beer)[2] * 20


# ロジスティック回帰分析 -------------------------------------------------------------

# データの読み込み
study_data <- read.csv("012-study-hours.csv")

# データの確認
nrow(study_data)     # 行数
head(study_data)     # 最初の6行
summary(study_data)  # データの要約統計量

View(study_data)     # データの中身の確認

# 特定のデータの取り出し
study_data$hours   # 勉強時間
study_data$result  # テストの合格フラグ（1は合格）

# グラフ
plot(
  result ~ jitter(hours), 
  data = study_data
)

# データの集約
sum_study <- 
  # 集計対象となるデータ
  study_data |>
  # hoursごとに集計処理を行う
  group_by(hours) |> 
  # 勉強時間ごと(班ごと)の人数及び、合格者・不合格者人数を計算
  summarise(
    num = n(),                # 班の人数(同じhoursである人数)
    success = sum(result),    # 合格者数
    failure = num - success   # 不合格者数
  )

# 集約結果
sum_study

# グラフ
plot(success ~ hours, 
     data = sum_study)

# ロジスティック回帰の実行
mod_logistic <- glm(
  result ~ hours,        # formula
  data = study_data,     # データ
  family = "binomial"    # 確率分布
)

# 集約結果に対するロジスティック回帰の実行
mod_logistic_2 <- glm(
  cbind(success, failure) ~ hours, # formula
  data = sum_study,                # データ
  family = "binomial"              # 確率分布
)

# 係数の推定結果は微小な数値誤差を除いて同じ
# どちらの方法を使っても問題ない
coef(mod_logistic)
coef(mod_logistic_2)

# 最大化対数尤度の値は異なるが、
# 当てはまりの良さが変わったわけではない
# モデルの比較をする際には、「同じデータの与え方」をする必要がある
logLik(mod_logistic)
logLik(mod_logistic_2)

# ggplot2ライブラリを利用して回帰曲線を描く
ggplot(data = study_data) +
  geom_point(aes(x = jitter(hours, 0.2), 
                 y = jitter(result, 0.1))) + 
  geom_smooth(aes(x = hours, y = result),
              method = "glm", 
              method.args = list(family = "binomial"))

# 予測(うまく行かない)
pred_data <- data.frame(hours = 4)
pred_dame <- predict(
  mod_logistic, 
  newdata = pred_data
)
pred_dame

# この方法は、線形回帰モデルと同じく「切片＋傾き×説明変数」を出力する
coef(mod_logistic)
coef(mod_logistic)[1] + coef(mod_logistic)[2] * 4

# これが正しい合格率
pred_tadasii <- predict(
  mod_logistic, 
  newdata = pred_data, 
  type = "response"   # ここが大事！
)
pred_tadasii

# 「切片＋傾き×説明変数」をロジスティック変換した結果が、正しい予測値
1 / (1 + exp(-pred_dame))

# 係数の解釈
coef(mod_logistic)

# 勉強時間が0の時の合格率
p0 <- predict(
  mod_logistic, 
  newdata = data.frame(hours = 0), 
  type = "response"
)
p0

# 勉強時間が0の時の対数オッズ
log(p0 / (1 - p0))

# 勉強時間が1の時の合格率
p1 <- predict(
  mod_logistic, 
  newdata = data.frame(hours = 1), 
  type = "response"
)
p1

# 勉強時間が0時間、1時間の時のオッズ
odds_0 <- p0 / (1 - p0)
odds_1 <- p1 / (1 - p1)

odds_0
odds_1

# 勉強時間を1単位増やした時の対数オッズ比
log(odds_1 / odds_0)


