#Usando o pipe

TRUE %>% 
  mean(c(1:101, NA), na.rm = .)

1:101 %>% 
  c(NA) %>% 
  mean(na.rm = TRUE)

