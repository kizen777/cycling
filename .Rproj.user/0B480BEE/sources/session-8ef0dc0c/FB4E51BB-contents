### 2018 自転車運動トレーニングおよび自転車利用と健康効果：文献レビューより
### easyPubMed
### 20240813 Kizen Sasaki

# 自転車運動トレーニングによる血糖値への影響に関する介入研究についての文献検索

# 1. easyPubMedパッケージのロード
library(easyPubMed)

# 2. 検索式の設定
# 2.1. 血糖値への影響に関する検索式 p52参照
query_glucose <- '("blood glucose") AND ("cycling exercise" OR cycling OR "cycle ergometer") AND ("intervention")'
# 2.2. 血圧への影響に関する検索式
query_bp <- '("blood pressure") AND ("cycling exercise" OR cycling OR "cycle ergometer") AND ("intervention")'

# 3. PubMed IDの取得
# 3.1. 血糖値への影響に関する検索
glucose_ids <- get_pubmed_ids(query_glucose)
# 3.2. 血圧への影響に関する検索
bp_ids <- get_pubmed_ids(query_bp)

# 4. 検索結果の確認
# 4.1. 血糖値への影響に関する検索結果の件数確認
print(paste0("血糖値への影響に関する検索結果：", glucose_ids$Count, "件"))
# 4.2. 血圧への影響に関する検索結果の件数確認
print(paste0("血圧への影響に関する検索結果：", bp_ids$Count, "件"))

# 5. PubMed IDを使って文献データの取得
# 5.1. 血糖値への影響に関する文献データの取得
glucose_data <- fetch_pubmed_data(glucose_ids)
# 5.2. 血圧への影響に関する文献データの取得
bp_data <- fetch_pubmed_data(bp_ids)

# 6. 文献データをリスト化
# 6.1. 血糖値への影響に関する文献リストの作成
glucose_list <- articles_to_list(glucose_data)
# 6.2. 血圧への影響に関する文献リストの作成
bp_list <- articles_to_list(bp_data)

# 7. 文献リストから必要な情報の抽出
# 7.1. 血糖値への影響に関する文献の解析
glucose_df <- lapply(glucose_list, article_to_df, max_chars = -1, getAuthors = TRUE)
glucose_result <- do.call("rbind", glucose_df)
# 7.2. 血圧への影響に関する文献の解析
bp_df <- lapply(bp_list, article_to_df, max_chars = -1, getAuthors = TRUE)
bp_result <- do.call("rbind", bp_df)