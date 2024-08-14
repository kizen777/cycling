

library(PRISMAstatement)
library(webshot2)
library(htmlwidgets)

# 血圧への影響に関するPRISMAフローチャートの生成
prisma_chart <- prisma(
  found = 98,           # PubMedから特定された記録
  found_other = 2,      # 血糖値のレビューから特定された追加の記録
  no_dupes = 100,       # 重複排除後の記録数（この場合、重複はないと仮定）
  screened = 100,       # スクリーニングされた記録数
  screen_exclusions = 72, # タイトルと要約のスクリーニングで除外された記録数 (100 - 28)
  full_text = 28,       # 全文評価の対象となった記録数
  full_text_exclusions = 9, # 全文評価で除外された記録数 (28 - 19)
  qualitative = 21,     # 質的統合に含まれた研究数（最終的な採択論文数）
  quantitative = NULL   # 量的統合（メタ分析）の情報がない場合は NULL
)

# フローチャートをHTMLファイルとして保存
tmp <- tempfile(fileext = ".html")
saveWidget(prisma_chart, tmp, selfcontained = TRUE)

# ウェブページの内容を取得し、背景を透明に設定（Mac互換）
html_content <- readLines(tmp)
html_content <- gsub("background-color: #FFFFFF;", "background-color: rgba(0,0,0,0);", html_content)
html_content <- gsub("<body", "<body style='background-color:rgba(0,0,0,0);'", html_content)
writeLines(html_content, tmp)

# webshotを使用して最適化されたPNG画像を生成
webshot(tmp, "Figure/PRISMA_flowchart_blood_pressure.png",
        selector = ".mermaid",  # mermaidグラフのみを選択
        zoom = 2,  # 高解像度
        expand = 5,  # 周囲に5ピクセルの余白を追加
        vwidth = 900,  # ビューポート幅（調整可能）
        vheight = 600,  # ビューポート高さ（調整可能）
        cliprect = "viewport")

print("血圧への影響に関するPRISMA フローチャートが 'Figure/PRISMA_flowchart_blood_pressure.png' として保存されました。")

# 検索式の表示
cat("検索式: (\"blood pressure\") AND (\"cycling exercise\" OR cycling OR \"cycle ergometer\") AND (\"intervention\")\n")

# 文献採択基準の表示
cat("文献採択基準:\n")
cat("以下の項目に該当する文献は不採用:\n")
cat("①血糖値に関する文献採択基準①～⑥に該当する研究\n")
cat("②介入前後でのアウトカム（収縮期血圧や平均血圧）の値が不明確な研究\n")
cat("③血圧低下を抑制するために運動を行っていた研究（透析療法中に実施している自転車運動）\n")