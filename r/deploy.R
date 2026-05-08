#!/usr/bin/env Rscript
# shinyapps.io へデプロイするスクリプト
#
# 事前準備（いずれか一方）:
#   A) R コンソールで一度だけ:
#        rsconnect::setAccountInfo(name = "アカウント名", token = "...", secret = "...")
#   B) 本スクリプト実行時に環境変数を渡す:
#        SHINYAPPS_ACCOUNT=... SHINYAPPS_TOKEN=... SHINYAPPS_SECRET=... Rscript deploy.R
#
# 任意: アプリ名を変える場合
#        SHINYAPPS_APPNAME=my-app-name Rscript deploy.R

ca <- commandArgs(trailingOnly = FALSE)
ff <- sub("^--file=", "", ca[startsWith(ca, "--file=")])
app_dir <- if (length(ff) && nzchar(ff[[1]])) {
  dirname(normalizePath(ff[[1]]))
} else {
  normalizePath(getwd())
}

if (!requireNamespace("rsconnect", quietly = TRUE)) {
  install.packages("rsconnect", repos = "https://cran.rstudio.com")
}
library(rsconnect)

if (nrow(rsconnect::accounts()) == 0L) {
  acc <- Sys.getenv("SHINYAPPS_ACCOUNT", unset = "")
  tok <- Sys.getenv("SHINYAPPS_TOKEN", unset = "")
  sec <- Sys.getenv("SHINYAPPS_SECRET", unset = "")
  if (acc == "" || tok == "" || sec == "") {
    stop(
      "rsconnect に shinyapps.io アカウントがありません。\n",
      "  R で rsconnect::setAccountInfo(name, token, secret) を実行するか、\n",
      "  SHINYAPPS_ACCOUNT / SHINYAPPS_TOKEN / SHINYAPPS_SECRET を設定してから再実行してください。\n",
      "  トークンは https://www.shinyapps.io/admin/#/tokens から取得できます。"
    )
  }
  rsconnect::setAccountInfo(name = acc, token = tok, secret = sec)
}

app_name <- Sys.getenv("SHINYAPPS_APPNAME", unset = "ito-museum-app3")
deployApp(
  appDir = app_dir,
  appName = app_name,
  launch.browser = interactive()
)
