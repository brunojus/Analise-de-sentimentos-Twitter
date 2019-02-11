library(tidyverse) 
library(ggExtra)
library(magrittr) 
library(lubridate)
library(stringr) 
library(tidytext) 
library(lexiconPT)

data("oplexicon_v3.0")
data("sentiLex_lem_PT02")

op30 <- oplexicon_v3.0
sent <- sentiLex_lem_PT02

df_comments <- read.delim("~/Documents/artigo/R/Aecio1_oplexiconLimpo.txt", stringsAsFactors=FALSE)

df_comments %<>% mutate(comment_id = row_number())

df_comments_unnested <- df_comments %>% unnest_tokens(term, text)

df_comments_unnested %>%
  select(comment_id, term) %>%
  head(20)

df_comments_unnested %>% 
  left_join(op30, by = "term") %>% 
  left_join(sent %>% select(term, lex_polarity = polarity), by = "term") %>% 
  select(comment_id, term, polarity, lex_polarity) %>% 
  head(10)

df_comments_unnested <- df_comments_unnested %>% 
  inner_join(op30, by = "term") %>% 
  inner_join(sent %>% select(term, lex_polarity = polarity), by = "term") %>% 
  group_by(comment_id) %>% 
  summarise(
    comment_sentiment_op = sum(polarity),
    comment_sentiment_lex = sum(lex_polarity),
    n_words = n()
  ) %>% 
  ungroup() %>% 
  rowwise() %>% 
  mutate(
    most_neg = min(comment_sentiment_lex, comment_sentiment_op),
    most_pos = max(comment_sentiment_lex, comment_sentiment_op)
  )

head(df_comments_unnested)


df_comments_wide <- df_comments %>% 
  # filtrar fora palavras neutras
  filter(sentiment.y == 0) %>% 
  # converter numerico para categorico
  mutate(sentiment.y = ifelse(sentiment.y == 0, "neutro", "neutral")) %>% 
  # agrupar os dados
  count(date, text, permalink, sentiment.y) %>% 
  # converter para formato wide
  spread(sentiment.y, n, fill = 0) %>% 
  mutate(sentimento = positivo - negativo) %>% 
  ungroup() %>% 
  arrange(date)


df_comments %>% 
  mutate(
    temer = str_detect(str_to_lower(text), "temer"),
    lula = str_detect(str_to_lower(text), "lula"),
    pmdb = str_detect(str_to_lower(text), "pmdb"),
    psdb = str_detect(str_to_lower(text), "psdb"),
    pt = str_detect(str_to_lower(text), "pt"),
    dilma = str_detect(str_to_lower(text), "dilma"),
    doria = str_detect(str_to_lower(text), "doria"),
    governo = str_detect(str_to_lower(text), "governo"),
    bolsonaro = str_detect(str_to_lower(text), "bolsonaro")
  ) %>% 
  gather(termo, eh_presente, temer:bolsonaro) %>% 
  filter(eh_presente) %>% 
  group_by(termo) %>% 
  summarise(sentiment.y = mean(sentiment.y)) %>% 
  ggplot(aes(x = termo, y = sentiment.y)) + 
  geom_col(fill = "#C10534")
