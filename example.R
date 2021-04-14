library(nycflights13)
library(dplyr)
rnorm(1e7)
fli_small <- flights %>% 
  sample_n(size = 500) 
fli_small
