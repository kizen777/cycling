# 自転車運動トレーニングによる血糖値への影響の文献検索
# 20240813 kizen sasaki

# 1. 必要なパッケージのインストールとロード
if (!requireNamespace("rentrez", quietly = TRUE)) {
  install.packages("rentrez")
}
library(rentrez)
library(XML)

# 2. 検索式の設定
query <- '("blood glucose") AND 
          ("cycling exercise" OR 
           cycling OR 
           "cycle ergometer") AND 
          ("intervention")'

# 3. PubMed IDの取得
pmids <- entrez_search(db = "pubmed", term = query, retmax = 1000)

# 4. 検索結果の確認
print(paste0("検索結果：", pmids$count, "件"))

# 5. 検索結果の取得
records <- entrez_fetch(db = "pubmed", id = pmids$ids, rettype = "xml", retmode = "text")

# 6. 検索結果の解析
xml_data <- xmlParse(records)
articles <- getNodeSet(xml_data, "//PubmedArticle")

extract_info <- function(article) {
  pmid <- xpathSApply(article, ".//PMID", xmlValue)
  title <- xpathSApply(article, ".//ArticleTitle", xmlValue)
  year <- xpathSApply(article, ".//PubDate/Year", xmlValue)
  journal_title <- xpathSApply(article, ".//Journal/Title", xmlValue)
  abstract <- xpathSApply(article, ".//Abstract/AbstractText", xmlValue)
  
  # 著者情報の取得
  authors <- xpathSApply(article, ".//Author", function(x) {
    paste(
      xpathSApply(x, ".//LastName", xmlValue),
      xpathSApply(x, ".//ForeName", xmlValue),
      collapse = " "
    )
  })
  authors <- paste(authors, collapse = "; ")
  
  data.frame(pmid = pmid, 
             authors = authors,
             title = title, 
             abstract = paste(abstract, collapse = " "),
             year = year, 
             journal_title = journal_title, 
             stringsAsFactors = FALSE)
}

abstract_df <- do.call(rbind, lapply(articles, extract_info))

# 7. 結果の表示
# print(head(abstract_df[, c("pmid", "authors", "title", "year")]))

# 8. 結果をCSVファイルに保存
write.csv(abstract_df[, c("pmid", "authors", "title", "abstract", "year")], 
          file = "Data/pubmed_search.csv", 
          row.names = FALSE, 
          fileEncoding = "UTF-8")

print("検索結果がpubmed_search.csvファイルに保存されました。")

# 9. PubMed検索結果の第1次スクリーニング #######################
# 10. 必要なパッケージのロード
library(dplyr)

# 11. CSVファイルの読み込み
data <- read.csv("data/pubmed_search.csv", 
                 stringsAsFactors = FALSE, encoding = "UTF-8")

# 12. 除外基準の関数を定義
exclude_criteria <- function(title, abstract) {
  lower_title <- tolower(title)
  lower_abstract <- tolower(abstract)
  
  if (grepl("acute|一過性|single.bout", lower_title) || grepl("acute|一過性|single.bout", lower_abstract)) {
    return("① 一過性の自転車運動効果")
  }
  if (grepl("athlete|競技者|エリート", lower_title) || grepl("athlete|競技者|エリート", lower_abstract)) {
    return("② トップアスリートを対象")
  }
  if (grepl("upper.limb|上肢", lower_title) || grepl("upper.limb|上肢", lower_abstract)) {
    return("③ 上肢のエルゴメーター使用")
  }
  if (grepl("spinal.cord.injury|脊椎損傷|術後", lower_title) || grepl("spinal.cord.injury|脊椎損傷|術後", lower_abstract)) {
    return("④ 重度の疾患者を対象")
  }
  if (grepl("walking|筋力トレーニング|diet|食事", lower_title) || grepl("walking|筋力トレーニング|diet|食事", lower_abstract)) {
    return("⑤ 介入方法が複数")
  }
  if (grepl("observational|観察研究", lower_title) || grepl("observational|観察研究", lower_abstract)) {
    return("⑥ 観察研究")
  }
  return(NA)
}

# 13. 除外基準の適用
data$exclusion_reason <- mapply(exclude_criteria, data$title, data$abstract)

# 14. 結果の表示
excluded <- data %>% filter(!is.na(exclusion_reason))
included <- data %>% filter(is.na(exclusion_reason))

# 4. 検索結果の確認　再掲
print(paste0("検索結果：", pmids$count, "件"))

print(paste("除外された文献数:", nrow(excluded)))
print(paste("採用された文献数:", nrow(included)))

# 15. 除外理由の集計
exclusion_summary <- table(excluded$exclusion_reason)
print("除外理由の内訳:")
print(exclusion_summary)

# 16. 結果の保存
write.csv(excluded, "data/excluded_studies.csv", row.names = FALSE, fileEncoding = "UTF-8")
write.csv(included, "data/included_studies.csv", row.names = FALSE, fileEncoding = "UTF-8")

print("スクリーニング結果が保存されました。") ########################

# 16. PRISMA 2020フローチャートの作成
# 必要なパッケージのインストールとロード
if (!requireNamespace("PRISMAstatement", quietly = TRUE)) {
  install.packages("PRISMAstatement")
}
library(PRISMAstatement)

# 16.1. 既存のデータフレーム
prisma_data <- data.frame(
  Phase = c("Identification", "Identification", "Screening", "Screening", "Included"),
  Categories = c(
    "Records identified from PubMed",
    "Records screened",
    "Reports sought for retrieval",
    "Reports assessed for eligibility",
    "Reports of included studies"
  ),
  Values = c(54, 54, 27, 27, 16)
)

excluded_data <- data.frame(
  Phase = c("Screening", "Screening", "Included"),
  Categories = c(
    "Records excluded (title/abstract)",
    "Reports not retrieved",
    "Reports excluded"
  ),
  Values = c(27, 0, 11)
)

# 16.2データフレームから値を抽出 フローチャーム用変数をコードより確定
found <- prisma_data$Values[prisma_data$Categories == "Records identified from PubMed"]
screened <- prisma_data$Values[prisma_data$Categories == "Records screened"]
full_text <- prisma_data$Values[prisma_data$Categories == "Reports assessed for eligibility"]
included <- prisma_data$Values[prisma_data$Categories == "Reports of included studies"]

screen_exclusions <- excluded_data$Values[excluded_data$Categories == "Records excluded (title/abstract)"]
full_text_exclusions <- excluded_data$Values[excluded_data$Categories == "Reports excluded"]

# 16.3. PRISMA フローチャートの作成

prisma_chart <- prisma(
  found = found,           # PubMedから特定された記録
  found_other = 1,      # 他のソース（血圧に関するレビュー）から特定された記録
  no_dupes = 55,        # 重複排除後の記録数（この場合、重複はないと仮定）
  screened = 55,        # スクリーニングされた記録数
  screen_exclusions = 28, # タイトルと要約のスクリーニングで除外された記録数 (55 - 27)
  full_text = 27,       # 全文評価の対象となった記録数
  full_text_exclusions = 12, # 全文評価で除外された記録数 (27 - 15)
  qualitative = 16,     # 質的統合に含まれた研究数（最終的な採択論文数）
  quantitative = NULL   # 量的統合（メタ分析）の情報がない場合は NULL
)

library(PRISMAstatement)
library(webshot2)
library(htmlwidgets)

# PRISMAフローチャートの生成
prisma_chart <- prisma(
  found = 54,
  found_other = 1,
  no_dupes = 55,
  screened = 55,
  screen_exclusions = 28,
  full_text = 27,
  full_text_exclusions = 12,
  qualitative = 16,
  quantitative = NULL
)

# フローチャートをHTMLファイルとして保存
tmp <- tempfile(fileext = ".html")
saveWidget(prisma_chart, tmp, selfcontained = TRUE, background = "transparent")

# ウェブページの内容を取得し、背景を透明に設定
html_content <- readLines(tmp)
html_content <- gsub("background-color: #FFFFFF;", 
                     "background-color: transparent;", html_content)
writeLines(html_content, tmp)

# webshotを使用して最適化されたPNG画像を生成
webshot(tmp, "Figure/PRISMA_flowchart_optimized.png",
        selector = ".mermaid",  # mermaidグラフのみを選択
        zoom = 2,  # 高解像度
        expand = 5,  # 周囲に5ピクセルの余白を追加
        vwidth = 900,  # ビューポート幅（調整可能）
        vheight = 600,  # ビューポート高さ（調整可能）
        cliprect = "viewport")

print("最適化されたPRISMA フローチャートが 'Figure/PRISMA_flowchart_optimized.png' 
      として保存されました。")

# フローチャートの保存
# ggsave("Figure/PRISMA_2020_flowchart.png", prisma_plot, width = 10, height = 12, dpi = 300)
# 
# print("Figure/PRISMA 2020フローチャートが 'PRISMA_2020_flowchart.png' として保存されました。")

# 17. 変数と数値の一覧表作成（Rマークダウン形式）
# 17. Create a summary table of variables and values (R Markdown format)

# Create a dataframe with variables and values
variables_summary <- data.frame(
  Number = 1:9,
  Description = c(
    "Records identified from PubMed",
    "Records identified from other sources",
    "Records after duplicates removed",
    "Records screened",
    "Records excluded (title/abstract)",
    "Reports sought for retrieval",
    "Reports not retrieved",
    "Reports assessed for eligibility",
    "Reports excluded"
  ),
  Variable = c("found", "found_other", "no_dupes", "screened", "screen_exclusions", 
               "full_text", "full_text_exclusions", "qualitative", "quantitative"),
  Value = c(found, 1, 55, screened, screen_exclusions, 
            full_text, full_text_exclusions, included, "NULL")
)

# Create table in R Markdown format
cat("## Summary of PRISMA 2020 Flow Diagram Variables\n\n")
cat("| No. | Description | Variable | Value |\n")
cat("|-----|-------------|----------|------|\n")
for(i in 1:nrow(variables_summary)) {
  cat(sprintf("| %d | %s | %s | %s |\n", 
              variables_summary$Number[i],
              variables_summary$Description[i],
              variables_summary$Variable[i], 
              variables_summary$Value[i]))
}

# Create 'results' directory if it doesn't exist
if (!dir.exists("results")) {
  dir.create("results")
}

# Save results to file
writeLines(capture.output({
  cat("## Summary of PRISMA 2020 Flow Diagram Variables\n\n")
  cat("| No. | Description | Variable | Value |\n")
  cat("|-----|-------------|----------|------|\n")
  for(i in 1:nrow(variables_summary)) {
    cat(sprintf("| %d | %s | %s | %s |\n", 
                variables_summary$Number[i],
                variables_summary$Description[i],
                variables_summary$Variable[i], 
                variables_summary$Value[i]))
  }
}), "results/variables_summary.md")

print("The summary table of variables and values has been saved as 'results/variables_summary.md'.")