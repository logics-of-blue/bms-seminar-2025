

# ライブラリの読み込み --------------------------------------------------------------

library(ggplot2)
library(dplyr)
library(KFAS)
library(forecast)


# 時変係数モデル --------------------------------------------------------------

# データの読み込み
sales_df <- read.csv("021-ssm.csv")
head(sales_df)

# ts型に変換
sales_ts <- ts(
  sales_df, 
  start = c(2020, 1), 
  frequency = 365.25
)
sales_ts

# プロット
autoplot(
  sales_ts[, c("sales")], 
  ylab = "sales"
)
autoplot(
  sales_ts[, c("sales", "promotion")], 
  facets = TRUE
)


# 時変係数モデルの推定
# Step1：モデルの構造を記述
build_ssm <- SSModel(
  H = NA,
  sales ~                                 # 売り上げをモデル化
    SSMtrend(degree = 1,                  # ローカルレベルモデル
             Q = c(list(NA))) +
    SSMregression(~ promotion, Q = NA),   # 宣伝費用に対する時変係数モデル
  data = sales_ts
)

# Step2：パラメタ推定
# 試行錯誤的に尤度が高くなるパラメータを探索する
fit_ssm <- fitSSM(
  build_ssm, 
  inits = c(1, 1, 1) # パラメータの初期値
)

# Step3、4：フィルタリング・平滑化
# 目に見えない状態の値を推定する
result_ssm <- KFS(
  fit_ssm$model, 
  filtering = c("state", "mean"),
  smoothing = c("state", "mean")
)

# 平滑化をした結果
result_ssm$alphahat

# 広告を除いた、売り上げの水準と
# 元の売り上げデータを結合させる
level_ts <- ts.union(
  sales_ts[, "sales"], 
  result_ssm$alphahat[, "level"]
)
colnames(level_ts) <- c("data", "level")

# 元の売り上げと、水準成分の推定値の比較
autoplot(level_ts, size = 1)


# 係数の推定値
result_ssm$alphahat[, "promotion"]

# 係数のグラフ
autoplot(
  result_ssm$alphahat[, "promotion"], 
  size = 1, main = "広告の係数")


# 水準成分と、広告の影響を両方考慮した結果
alpha <- 
  result_ssm$alphahat[, "level"] +     # 水準
  result_ssm$alphahat[, "promotion"] * # 係数
  sales_ts[, "promotion"]              # 広告フラグ

# 元の売り上げデータと結合
alpha_ts <- ts.union(
  sales_ts[, "sales"], 
  alpha
)
colnames(alpha_ts) <- c("data", "state")

# 元の売り上げと、水準成分の推定値の比較
autoplot(alpha_ts, size = 1)

