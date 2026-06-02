library(readxl)
library(dplyr)
library(tidyr)
library(purrr)

fl <- "~/Projects/GLA/E-comm/Youth tracker 2026 full data set - Copy (1).xlsx"

sec_e <- read_excel(fl, "Tables")



ww <- which(sec_e[[1]] == "COL")[1]
nms <- sec_e[ww,]
nms[2] <- "Question"
names(sec_e) <- nms



sec_e$Q <- sec_e$base_desc <- "" 

sec_e$Q[which(sec_e$COL == "VT")] <- sec_e$Question[which(sec_e$COL == "VT")]
sec_e$Q[which(!sec_e$COL == "VT")] <- NA

sec_e$base_desc[which(sec_e$COL == "BT")] <- sec_e$Question[which(sec_e$COL == "BT")]
sec_e$base_desc[which(!sec_e$COL == "BT")] <- NA
# sec_e$unweighted[which(sec_e$COL == "UNR")] <- sec_e$Total[which(sec_e$COL == "UNR")]
# sec_e$unweighted[which(!sec_e$COL == "UNR")] <- NA
# 
# sec_e$weighted[which(sec_e$COL == "PTR")] <- sec_e$Total[which(sec_e$COL == "PTR")]
# sec_e$weighted[which(!sec_e$COL == "PTR")] <- NA

sec_ea <- sec_e %>%
    select(COL, base_desc,Q, Question, Total, London, England, 41:43) %>% 
    fill(Q) %>% 
    fill(base_desc) %>% 
    fill(Question) %>% 
    filter(COL %in% c("PRC", "RTV0", "UNR", "PTR")) %>% 
    pivot_longer(-c(COL:Question), names_to = "x") 


sec_eb <- map_df(unique(sec_ea$Q), function(q) {
  print(q)
  tryCatch({
    df <- sec_ea %>% filter(Q == q)
    bases <- df %>% filter(COL %in% c("UNR", "PTR")) %>% 
      select(Question, x, value) %>% 
      pivot_wider( names_from = Question, values_from = value)
    df <- df %>% filter(!COL %in% c("UNR", "PTR")) %>% 
      left_join(bases, by = "x") %>% 
      pivot_wider(names_from = "COL", values_from = value) %>% 
      rename(value = PRC, prc_in_file = RTV0) %>% 
      mutate(prc_calc = as.numeric(value) / as.numeric(`Weighted Bases`))
    df
    
  }, error = function(e) {
    print("ERROR HERE")
    NULL
  })
})

saveRDS(sec_eb, file = "~/Projects/GLA/E-comm/all_data.RDS")

library(plotly)

df <- sec_eb %>% filter(Q %in% c("Q01.1. How interested would you say that you are in the following? - Politics in the UK"),  
                                 # "Q01.2. How interested would you say that you are in the following? - Politics in my local area" ),
                        Question %in% c("NET: Interested", "NET: Not Interested" ),
                        x %in% c("Total", "London")
                        )

plot_df <- df %>% select(Question, x, prc_calc) %>% 
  pivot_wider(names_from = x, values_from = prc_calc)

plot_ly(plot_df, type = "bar", x = ~Question, y =~ Total, name = "UK" ) %>% 
  add_trace(y = ~London, name = "London")


df <- sec_eb %>% filter(Q %in% c("Q01.2. How interested would you say that you are in the following? - Politics in my local area" ),
                        Question %in% c("NET: Interested", "NET: Not Interested" ),
                        x %in% c("Total", "London")
                        )

plot_df <- df %>% select(Question, x, prc_calc) %>% 
  pivot_wider(names_from = x, values_from = prc_calc)

plot_ly(plot_df, type = "bar", x = ~Question, y =~ Total, name = "UK" ) %>% 
  add_trace(y = ~London, name = "London")

