# import data
platform <- read_csv("Dummy_Data.csv")
sns <- read_csv("Most_Used_Social_Media_Platforms.csv")
website <- read_csv("online_shoppers_intention.csv")
keyword <- read_csv("ecommerce_search_relevance.csv")

# keyword cleaned
keyword_new <- keyword %>%
  select(query, relevance, `relevance:variance`) %>%
  rename(category = query, relevance_variance = `relevance:variance`) %>%
  mutate(
    relevance = replace_na(relevance, 0),
    relevance_variance = replace_na(relevance_variance, 0)
  ) %>%
  group_by(category) %>%
  summarise(
    relevance = mean(relevance),
    relevance_variance = mean(relevance_variance)
  ) %>%
  arrange(desc(relevance))

# keyword (top 5)
keyword_top <- keyword_new %>%
  top_n(5, relevance)

# keyword (last 5)
keyword_bot <- keyword_new %>%
  top_n(-5, relevance)


write.csv(keyword_top,"C:/Users/김수빈/Documents/SKKU/3학년1학기(2021-1)/대용량자료관리및시각화/final/keyword_top.csv", row.names = FALSE)
write.csv(keyword_bot,"C:/Users/김수빈/Documents/SKKU/3학년1학기(2021-1)/대용량자료관리및시각화/final/keyword_bot.csv", row.names = FALSE)
