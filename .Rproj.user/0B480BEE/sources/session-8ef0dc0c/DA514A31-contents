# 自転車運動トレーニングによる血糖値への影響の文献検索

# 1. easyPubMedパッケージのロード
library(easyPubMed)

# 2. 検索式の設定
query <- '("blood glucose") AND 
          ("cycling exercise" OR 
           cycling OR 
           "cycle ergometer") AND 
          ("intervention")'

# 3. PubMed IDの取得
pmids <- get_pubmed_ids(query)

# 4. 検索結果の確認
print(paste0("検索結果：", pmids$Count, "件"))

# 5. 検索結果の取得
records <- fetch_pubmed_data(pmids)

# 6. 検索結果の解析
articles <- articles_to_list(records)
abstracts <- lapply(articles, article_to_df, max_chars = -1, getAuthors = FALSE)
abstract_df <- do.call("rbind", abstracts)

# 7. 結果の表示
print(head(abstract_df[, c("pmid", "title", "year", "journal_title")]))