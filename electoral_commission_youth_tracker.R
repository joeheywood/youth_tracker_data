library(readxl)
library(dplyr)
library(tidyr)
library(purrr)

## Spreadsheet provided by CSP
fl <- "~/Projects/GLA/E-comm/Youth tracker 2026 full data set - Copy (1).xlsx"

## Data doesn't have the correct columns at the top. Every row with "COL" in 
## column A has them. So I gathered them from the first one. I've added one to COL B,
## which is empty
raw_data <- read_excel(fl, "Tables")
ww <- which(raw_data[[1]] == "COL")[1]
nms <- as.character(raw_data[ww,])
nms[2] <- "option_text"
names(raw_data) <- nms


## In column A, the code is VT for Question text and BT for a description of the base
## So this section adds them to new columns 
raw_data$Q <- raw_data$base_desc <- "" 
raw_data$Q[which(raw_data$COL == "VT")] <- raw_data$option_text[which(raw_data$COL == "VT")]
raw_data$Q[which(!raw_data$COL == "VT")] <- NA

raw_data$base_desc[which(raw_data$COL == "BT")] <- raw_data$option_text[which(raw_data$COL == "BT")]
raw_data$base_desc[which(!raw_data$COL == "BT")] <- NA


## Selecting only the columns we need
proc_data <- raw_data %>%
  select(COL, base_desc,Q, option_text, Total, London, England, 41:43) %>% 
  fill(Q) %>%  ## fills down Question
  fill(base_desc) %>% ## Fills down base description
  fill(option_text) %>% 
  filter(COL %in% c("PRC", "RTV0", "UNR", "PTR")) %>% 
  pivot_longer(-c(COL:option_text), names_to = "x") 


## final version

## loops over each question and moves the percentage and N values to 
## different columns. Also re-calculates the % values to check.
clean_data <- map_df(unique(proc_data$Q), function(q) {
  tryCatch({
    df <- proc_data %>% filter(Q == q)
    bases <- df %>% filter(COL %in% c("UNR", "PTR")) %>% 
      select(option_text, x, value) %>% 
      pivot_wider( names_from = option_text, values_from = value)
    df <- df %>% filter(!COL %in% c("UNR", "PTR")) %>% 
      left_join(bases, by = "x") %>% 
      pivot_wider(names_from = "COL", values_from = value) %>% 
      rename(value = PRC, prc_in_file = RTV0) %>% 
      mutate(prc_calc = as.numeric(value) / as.numeric(`Weighted Bases`))
    df
    
  }, error = function(e) {
    print(glue("ERROR with {q}")) ## Ignore if they above doesn't work (mostly summary tables we dont' need)
    NULL
  })
})