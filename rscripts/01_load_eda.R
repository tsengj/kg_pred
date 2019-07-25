library('tidyverse')
sample_df <- read_csv('./data/sample_submission.csv')
test_id <- read_csv('./data/test_identity.csv')
test_tx <- read_csv('./data/test_transaction.csv')
train_id <- read_csv('./data/train_identity.csv')
train_tx <- read_csv('./data/train_transaction.csv')


# ---- Calculate how empty are the variables -------
sparse_var <- function (x) {
  na_count <- sapply(x, function(y) sum(length(which(is.na(y)))))
  cnt <- pull(count(x))
  na_count <- bind_cols(var = names(na_count),
                        as.tibble(na_count), 
                        na_pct = as.tibble(na_count)/ cnt) %>%
    rename(na_pct = value1)
  return(na_count)
}

#Visualize how empty they are
sparse_var(train_tx) %>% 
  # filter(str_detect(var,"workspace")) %>% #name match
  ggplot(aes(x=reorder(var, -na_pct),y=na_pct)) +
  geom_bar(stat='identity') + 
  geom_text(aes(label=round(na_pct,2)), position=position_dodge(width=0.9)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  ggtitle('how empty are these variables') +
  xlab('') +
  coord_flip()

var_ord <- train_tx %>% select_if(is.ordered) %>% names()
var_num <- train_tx %>% select_if(is.double) %>% names()
var_int <- train_tx %>% select_if(is.integer) %>% names()
var_chr <- train_tx %>% select_if(is.character) %>% names()
var_lgl <- train_tx %>% select_if(is.logical) %>% names()

train_tx %>%
  select(var_num,var_int) %>%
  gather() %>%
  ggplot(aes(value)) + 
  geom_histogram(bins = 50) + 
  facet_wrap(~key, scales = 'free_x')

train_tx %>%
  select(var_ord,var_chr) %>%
  gather() %>%
  ggplot(aes(value),group=key) + 
  geom_bar() + 
  facet_wrap(~key, scales = 'free_x') +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) 

#Explore Quantile Range
train_tx %>%
  select(c(var_num,var_int)) %>%
  summarise_all(funs(list(quantile(., probs = c(0,0.01,0.05,0.5, 0.95, 0.99,1), na.rm=T)))) %>%
  unnest %>%
  transpose %>%
  setNames(., c('0%','1%','05%','50%','95%','99%','100%')) %>%
  map_df(unlist) %>%
  bind_cols(data.frame(vars = c(var_num,var_int)), .)

#create index based on the anomaly conditions
#function to flag extreme values
extrm_idx <- function(df,
                      ext_col,
                      q_min = 0,
                      q_max = 0.9999) {
  df %>%
    select(ext_col) %>%
    apply(., 2, function(x) {
      which(x <= quantile(x, q_min, na.rm = TRUE) |
              x >= quantile(x, q_max, na.rm = TRUE))
    }) %>%
    unlist %>%
    unique()
}
ext_col <- var_num #c('x','y','z')
fil <- extrm_idx(diamonds,ext_col)

#View actual anomalies
diamonds[fil,]

#construct anom flag
plot_df <- 
  diamonds %>%
  select('x','y','z','price') %>%
  mutate(anom = as.factor(ifelse(row_number() %in% fil,1,0)))