library(tidyverse)
library(ppcor)

data <- read.table("clipboard", header = T)

### 设置组合
combn(5, 2) %>%
  t() %>% 
  as.data.frame() -> index

data %>% 
  pivot_wider(values_from = coverage, names_from = species) %>% 
  mutate_at(vars(En:Kh), ~ifelse(is.na(.) == T, 0, .)) -> data_new

data_ana <- data_new[, 8:12]

sp_names <- names(data_ana)


### 建立空数据框用来存放循环数据
res_hh <- array(data = NA, dim = c(10, 11)) %>% as.data.frame()


### 循环计算并输出数据

for(i in 1:10){
  
  index1 <- c(index[i, 1], index[i, 2])
  
  index2 <- setdiff(1:5,index1)
  
  nnn <- sp_names[c(index1, index2)] %>% t() %>% as.data.frame()
  
  res <- ppcor::pcor.test(data_ana[,index[i, 1]],data_ana[,index[i, 2]], data_ana[,index2])
  
  res_hh[i, 1:5] <- nnn
  
  res_hh[i, 6:11] <- res[1, 1:6]
  
  names(res_hh)[6:11] <- names(res)
  
}

### 最终结果
res_hh















