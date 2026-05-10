FROM rocker/shiny:4.4.2

# 1. 地理空間・システムライブラリのインストール
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libgdal-dev \
    libgeos-dev \
    libproj-dev \
    libudunits2-dev \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

# 2. Rパッケージのインストール（バイナリ使用で高速化）
RUN R -e "options(repos = c(RSPM = 'https://packagemanager.posit.co/cran/__linux__/jammy/latest')); \
    install.packages(c('leaflet', 'dplyr', 'readr', 'htmltools', 'RColorBrewer'))"

# 3. アプリケーションファイルのコピー
# 作業ディレクトリを /home/shiny に設定するのが Hugging Face での安定パターンです
WORKDIR /home/shiny
COPY r/ .
COPY python/ito_sites_clean_fixed.csv .
COPY python/ito_sites_clean.csv .

# 4. 権限の設定
RUN chown -R shiny:shiny /home/shiny

# 5. ポート設定と実行
# shiny::runApp('.') とすることで、WORKDIR内の app.R を自動的に認識させます
EXPOSE 7860
CMD ["R", "-e", "shiny::runApp('app_advanced.R', host='0.0.0.0', port=7860)"]