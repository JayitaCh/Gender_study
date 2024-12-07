require(tidyverse)
install.packages("reshape2")
require(reshape2)


df <- read.csv("data/data_v1.csv")
df$id <- as.numeric(rownames(df))
df <- df %>%
  relocate(id, .before = Gender)

df_long <- melt(df, id.vars=-c(14:16), measure.vars=c("Choice.Card._1", "Choice.Card_2", "Choice.Card_3" ), 
                variable.name="Cards", value.name="Choice")
df_long <- df_long %>%
  arrange(id)
