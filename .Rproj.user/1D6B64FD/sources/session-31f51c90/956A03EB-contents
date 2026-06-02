library(plotly)
library(dplyr)
library(tidyr)
library(stringr)


sec_eb <- readRDS("all_data.RDS")

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

q2df <- sec_eb %>% 
  filter(Q %in% "Q02. How much do you think politics in this country has an impact or makes a difference to your everyday life?",
         str_detect(Question, "NET: "),
         x %in% c("Total", "London")) %>% 
  mutate(x = ifelse(x == "Total", "UK", x)) %>% 
  mutate(Question = str_replace(Question, "NET: ", "")) %>% 
  select(Q,Question, x, prc_calc)

unique(q2df$Question)

# create fixed color mapping
colormap <- setNames(object = c("#9e0059", "#007acc"),
                     nm = c("UK", "London"))

# assign color to categories variable and provide colormap to colors
plot_ly(data = q2df,
        x = ~Question,
        y = ~prc_calc,
        type = "bar",
        color = ~x,
        colors = colormap) %>% 
  layout(
    yaxis = list(tickformat = ".0%", title = ""),
    xaxis = list(title = ""),
    legend = list(orientation = "h", y = 1.04)
  )

create_grouped_bar <- function(qns) {
  q2df <- sec_eb %>% 
    filter(Q %in% qns,
           str_detect(Question, "NET: "),
           x %in% c("Total", "London")) %>% 
    mutate(x = ifelse(x == "Total", "UK", x)) %>% 
    mutate(Question = str_replace(Question, "NET: ", "")) %>% 
    select(Q,Question, x, prc_calc)
  
  unique(q2df$Question)
  
  # create fixed color mapping
  colormap <- setNames(object = c("#9e0059", "#007acc"),
                       nm = c("UK", "London"))
  
  # assign color to categories variable and provide colormap to colors
  plot_ly(data = q2df,
          x = ~Question,
          y = ~prc_calc,
          type = "bar",
          color = ~x,
          colors = colormap) %>% 
    layout(
      yaxis = list(tickformat = ".0%", title = ""),
      xaxis = list(title = ""),
      legend = list(orientation = "h", y = 1.04)
    )
  
}