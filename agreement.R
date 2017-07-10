# kod för samstämmighetstabeller

samtliga <- matrix(nrow = 46, ncol = 10)

for (i in 1:ncol(samtliga)) {
  for (j in 4:49) {
    samtliga[(j - 3), i] <-
      joined_dataset %>%
      #filter(vald == 1) %>%
      filter(Parti == unique(.[3])[i, 1]) %>%
      .[, j] %>% table() %>% as.data.frame %>% .$Freq %>% Hb2
  }
}
colnames(samtliga) <-
  unique(joined_dataset[3])[, 1] %>% as.character()
samtliga <- 1 - samtliga
samtliga <- round(samtliga, digits = 3)
rownames(samtliga) <- colnames(joined_dataset)[4:49]
samtliga %>% as.data.frame %>% rownames_to_column() %>% as_tibble -> samtliga
samtliga <- samtliga %>% transmute(
  "Fråga" = rowname,
  Centerpartiet,
  `Feministiskt initiativ`,
  Folkpartiet,
  Kristdemokraterna,
  Miljöpartiet,
  Moderaterna,
  Piratpartiet,
  Socialdemokraterna, 
  Sverigedemokraterna,
  Vänsterpartiet
)

riksdag <- matrix(nrow = 46, ncol = 8)
for (i in 1:ncol(riksdag)) {
  for (j in 4:49) {
    riksdag[(j - 3), i] <-
      joined_dataset %>%
      filter(vald == 1) %>%
      filter(Parti == unique(.[3])[i, 1]) %>%
      .[, j] %>% table() %>% as.data.frame %>% .$Freq %>% Hb2
  }
}

colnames(riksdag) <-
  unique(joined_dataset %>%  filter(vald == 1) %>% .[3])[, 1] %>% as.character()
riksdag <- 1 - riksdag
riksdag <- round(riksdag, digits = 3)
rownames(riksdag) <- colnames(joined_dataset)[4:49]
riksdag %>% as.data.frame %>% rownames_to_column() %>% as_tibble -> riksdag
riksdag <- riksdag %>% transmute(
  "Fråga" = rowname,
  Centerpartiet,
  Folkpartiet,
  Kristdemokraterna,
  Miljöpartiet,
  Moderaterna,
  Socialdemokraterna, 
  Sverigedemokraterna,
  Vänsterpartiet
)
