list_of_dfs %>% 
    count(state) %>% 
    arrange(desc(n))

library(lme4)
library(tidyverse)
df <- list_of_dfs %>% 
    unnest(raw_reviews)

m1 <- lmer(raw_reviews ~ 1 + (1|state/name), data = df)
summary(m1)
sjstats::icc(m1)

str(ranef(m1))

ranef_df <- ranef(m1)$`name:state`

ranef_df_s <- ranef(m1)$`state`

ranef_df <- ranef_df %>% rownames_to_column()

names(ranef_df) <- c("name", "estimate")

ranef_df %>% 
    arrange(desc(estimate))
