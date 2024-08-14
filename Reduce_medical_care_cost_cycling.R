# 自転車利用による医療費抑制効果
# 20240813 kizen sasaki

library(PRISMAstatement)
library(webshot2)
library(htmlwidgets)

generate_consistent_prisma <- function(data, filename) {
  prisma_chart <- do.call(prisma, c(data, list(
    font_size = 12,
    width = 800,
    height = 1000
  )))
  
  tmp <- tempfile(fileext = ".html")
  saveWidget(prisma_chart, tmp, selfcontained = TRUE)
  
  html_content <- readLines(tmp)
  html_content <- gsub("background-color: #FFFFFF;", "background-color: rgba(0,0,0,0);", html_content)
  html_content <- gsub("<body", "<body style='background-color:rgba(0,0,0,0);'", html_content)
  writeLines(html_content, tmp)
  
  webshot(tmp, filename,
          selector = ".mermaid",
          zoom = 2,
          vwidth = 1000,
          vheight = 1250,
          cliprect = "viewport")
  
  print(paste("PRISMA フローチャートが", filename, "として保存されました。"))
}

# 自転車利用による健康効果のデータ
health_effect_data <- list(
  found = 23,  # システマティックレビューで採択されていた文献数
  found_other = 1,  # システマティックレビュー以降に報告された文献数
  no_dupes = 24,  # 重複排除後の総文献数
  screened = 24,  # スクリーニングされた文献数
  screen_exclusions = 17,  # 除外された文献数 (24 - 7)
  full_text = 7,  # 全文評価の対象となった文献数
  full_text_exclusions = 0,  # 全文評価で除外された文献数
  qualitative = 7,  # 最終的に採択された文献数
  quantitative = NULL  # 量的統合（メタ分析）の情報がない場合は NULL
)

# 自転車利用による医療費抑制効果のデータ
cost_effect_data <- list(
  found = 67,  # PubMedから特定された記録数
  found_other = 1,  # 他のソースから特定された記録数（シミュレーションモデル研究）
  no_dupes = 68,  # 重複排除後の総文献数
  screened = 68,  # スクリーニングされた文献数
  screen_exclusions = 65,  # タイトルと要約のスクリーニングで除外された記録数 (68 - 3)
  full_text = 3,  # 全文評価の対象となった文献数
  full_text_exclusions = 2,  # 全文評価で除外された文献数 (3 - 1)
  qualitative = 1,  # 最終的に採択された文献数（シミュレーションモデル研究）
  quantitative = NULL  # 量的統合（メタ分析）の情報がない場合は NULL
)

# フローチャートの生成
generate_consistent_prisma(health_effect_data, "Figure/PRISMA_flowchart_bicycle_health_effect.png")
generate_consistent_prisma(cost_effect_data, "Figure/PRISMA_flowchart_bicycle_cost_effect.png")

# 検索式の表示（医療費抑制効果の研究）
cat("医療費抑制効果の研究の検索式:\n")
cat("(bicycle OR cycling) AND (\"cost of medical care\" OR \"doctor bill\" OR \"fee for medical treatment\" OR \"health expenditure\" OR \"healthcare cost\" OR \"healthcare expenditure\" OR \"healthcare spending\" OR \"medical and dental treatment costs\" OR \"medical bill\" OR \"medical bills\" OR \"medical care cost\" OR \"medical care expenditure\" OR \"medical charges\" OR \"medical expense\" OR \"medical payment\" OR \"medical spending\" OR \"medical-care expenses\" OR \"medical cost\")\n")