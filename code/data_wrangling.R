require(tidyverse)
install.packages("reshape2")
require(reshape2)
require(dplyr)

### Load Design data ###
design_df <- read.csv("data/design/design_v3.csv")

design_bus <- design_df[seq(1, nrow(design_df), by = 2), ]
design_bus$Block <- rep(1:ceiling(nrow(design_bus)/3), each = 3)[1:nrow(design_bus)]
design_bus$chSet <- ave(design_bus$Block,design_bus$Block,FUN=seq_along)

design_bus <- design_bus %>%
  select(-c("X","m.at","m.wt","m.tc"))

colnames(design_bus) <- ifelse(startsWith(colnames(design_bus), "b"), 
                               colnames(design_bus), 
                               paste0("b.", colnames(design_bus)))
rownames(design_bus)<-NULL


design_metro <- design_df[seq(2, nrow(design_df), by = 2), ]
design_metro <- design_metro %>%
  select(-c("X","b.at","b.wt","b.tc"))
colnames(design_metro) <- ifelse(startsWith(colnames(design_metro), "m"), 
                               colnames(design_metro), 
                               paste0("m.", colnames(design_metro)))
rownames(design_metro)<-NULL

combined_design <- cbind(design_bus,design_metro)

combined_design <- combined_design %>%
  relocate(b.Block,b.chSet,.before = b.at)

### Load Survey Data ###
df <- read.csv("data/data_v1.csv")
df$id <- as.numeric(rownames(df))
df <- df %>%
  relocate(id, .before = Gender)

df_long <- melt(df, id.vars=-c(14:16), measure.vars=c("Choice.Card._1", "Choice.Card_2", "Choice.Card_3" ), 
                variable.name="Cards", value.name="Choice")
df_long <- df_long %>%
  arrange(id)

df_long$Cards <- as.character(df_long$Cards)

df_long$Cards[df_long$Cards=="Choice.Card._1"] <- 1
df_long$Cards[df_long$Cards=="Choice.Card_2"] <- 2
df_long$Cards[df_long$Cards=="Choice.Card_3"] <- 3
df_long$Cards <- as.numeric(df_long$Cards )


### Insert choice attributes based on choice set and block information in the design data
df_long$at_bus <- 0; df_long$at_metro <- 0
df_long$wt_bus <- 0; df_long$wt_metro <- 0
df_long$tc_bus <- 0; df_long$tc_metro <- 0
df_long$tt_bus <- 0; df_long$tt_metro <- 0
df_long$saccstop_bus <- 0; df_long$saccstop_metro <- 0
df_long$swaitenv_bus <-0; df_long$swaitenv_metro <-0
df_long$sboal_bus <-0; df_long$sboal_metro <-0
df_long$safety_bus <-0; df_long$safety_metro <-0

for(i in 1:nrow(combined_design)){
  r <- df_long$Block ==combined_design$b.Block[i] &  df_long$Cards == combined_design$b.chSet[i]
  df_long$at_bus[r] <- combined_design$b.at[i]
  df_long$wt_bus[r] <- combined_design$b.wt[i]
  df_long$tc_bus[r] <- combined_design$b.tc[i]
  df_long$tt_bus[r] <- combined_design$b.tt[i]
  df_long$saccstop_bus[r] <- combined_design$b.s_acc_stop[i]
  df_long$swaitenv_bus[r] <- combined_design$b.s_wait_env[i]
  df_long$sboal_bus[r] <- combined_design$b.s_bo_al[i]
  df_long$safety_bus[r] <- combined_design$b.safety[i]
  
  df_long$at_metro[r] <- combined_design$m.at[i]
  df_long$wt_metro[r] <- combined_design$m.wt[i]
  df_long$tc_metro[r] <- combined_design$m.tc[i]
  df_long$tt_metro[r] <- combined_design$m.tt[i]
  df_long$saccstop_metro[r] <- combined_design$m.s_acc_stop[i]
  df_long$swaitenv_metro[r] <- combined_design$m.s_wait_env[i]
  df_long$sboal_metro[r] <- combined_design$m.s_bo_al[i]
  df_long$safety_metro[r] <- combined_design$m.safety[i]
}