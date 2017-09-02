library(gsheet)
library(tidyverse)
library(basicspace)
library(plyr)
load(file="valkompass2014/.RData")

data_orig <- gsheet2tbl("https://docs.google.com/spreadsheets/d/1dkmpZb-2YcUxI5DtBs_V32sKotO5jkewCDMdRUgOHBs/edit#gid=2062535509")
valkompass <- data_orig %>% 
  mutate(Efternamn = paste(`Förnamn`, `Efternamn`)) %>% 
  select(`Id`, 
         `Efternamn`,
         `Parti`, 
         `Var någonstans skulle du placera dig själv på en politisk vänster-högerskala?`, 
         `1. Kvinnor ska kunna kvoteras in i bolagsstyrelser`,
         `2. Pensionsåldern ska höjas`,
         `3. Veckoarbetstiden ska sänkas`,
         `4. Det ska bli lättare för företag att bestämma vilka som ska sägas upp vid arbetsbrist`,
         `5. A-kassan ska höjas`,
         `6. Det ska vara lägre ingångslön för ungdomar på första jobbet`,
         `7. Eleverna ska få betyg från årskurs 4`,
         `8. Staten ska ta över ansvaret för skolan från kommunerna`,
         `9. Alla gymnasieprogram ska leda till högskolebehörighet`,
         `10. Antalet friskolor ska begränsas`,
         `11. Betyg i ordning och uppförande ska införas i skolan`,
         `12. Staten ska ta över ansvaret för sjukvården från landstingen`,
         `13. Vårdnadsbidraget för småbarnsföräldrar ska avskaffas`,
         `14. Kommunernas hemtjänst ska kosta lika mycket oavsett var man bor`,
         `15. Vinster ska inte tillåtas inom skattefinansierad vård och omsorg`,
         `16. Papperslösa flyktingar ska få fri vård`,
         `17. En förmögenhetsskatt ska återinföras`,
         `18. RUT-avdraget ska behållas som det är`,
         `19. Statligt ägda bolag ska inte säljas`,
         `20. Föräldraförsäkringen ska enligt lag delas lika`,
         `21. Det ekonomiska stödet till glesbygden ska öka`,
         `22. Dagens kärnreaktorer ska få ersättas med nya`,
         `23. Bensinskatten ska höjas`,
         `24. Stödet till vindkraften ska tas bort`,
         `25. Det ska skjutas fler vargar i Sverige`,
         `26. Det ska vara möjligt att söka asyl till Sverige på svenska ambassader utomlands`,
         `27. Färre flyktingar ska få stanna i Sverige`,
         `28. Det ska lagstiftas om att alla kommuner ska ta emot flyktingar`,
         `29. Polisen ska aktivt söka upp och avvisa asylsökande som fått avslag`,
         `30. Sverige ska lämna EU`,
         `31. Sverige ska satsa mer på försvaret`,
         `32. Sverige ska bli medlem i NATO`,
         `33. Det ska vara fri entré till statliga museer`,
         `34. Upphovsrättsskyddat material från internet ska fritt få laddas ner för eget bruk`,
         `35. Polisen ska kunna begära ut uppgifter om privatpersoners internet- och telefontrafik`,
         `36. Gårdsförsäljning av vin ska tillåtas`,
         `37. Satsa på ett Sverige med små inkomstskillnader`,
         `38. Satsa på ett Sverige med mer privat företagsamhet och marknadsekonomi`,
         `39. Satsa på ett Sverige där makt omfördelas från män till kvinnor`,
         `40. Satsa på ett Sverige där brottslingar straffas hårdare jämfört med idag`,
         `41. Satsa på ett mångkulturellt Sverige`,
         `42. Satsa på ett Sverige där kristna värden får större inflytande över politiken`,
         `43. Satsa på ett Sverige där hög utbildning ger bättre lön jämfört med idag`,
         `44. Satsa på ett Sverige där homo-, bisexuellas och transpersoners rättigheter utökas`,
         `45. Att bekämpa klimatförändringarna ska överordnas alla andra politiska frågor`)
names(valkompass)[2] <- "Namn"

valkompass[4] <- valkompass[4] %>% as.matrix() %>% factor(levels=c("Klart till vänster", 
                                                                   "Något till vänster", 
                                                                   "Varken till höger eller vänster", 
                                                                   "Något till höger",
                                                                   "Klart till höger"),ordered=T)
for(i in 5:49){
  valkompass[i] <- valkompass[i] %>% as.matrix() %>% factor(levels=c("Mycket dåligt förslag",
                                                                     "Ganska dåligt förslag",
                                                                     "Ingen åsikt",
                                                                     "Ganska bra förslag",
                                                                     "Mycket bra förslag"), ordered=T)
}

survey <- valkompass[5:49] %>% data.matrix()
survey[survey==3] <- NA
survey[survey==4] <- 3
survey[survey==5] <- 4
survey <- survey %>% as.data.frame()
#survey %>% View

#blackbox(survey, minscale=5, dims=2)$individuals[[2]]
candidates <- c(valkompass, as_tibble(blackbox(survey, minscale=5, dims=2)$individuals[[2]])) %>% as_tibble
names(candidates)[50:51] <- c("F1","F2")
#candidates %>% View

elected <- gsheet2tbl("https://docs.google.com/spreadsheets/d/1V0Y5LMTEASRtoxvM8kNV4DEq2pLmDPN8vRs0fOZ0Xnk/edit#gid=835472634") %>% select(Id, vald)
#import elected
#elected$vald[elected$vald=="Avgått"] <- NA

joined_dataset <- full_join(candidates, elected)


joined_dataset[is.na(joined_dataset$vald), ]$vald <- 0


joined_dataset[joined_dataset[, 50:51] %>% duplicated() %>% which(), ] <-
  joined_dataset[joined_dataset[, 50:51] %>% duplicated() %>% which(), ] %>% 
  mutate(F1 = jitter(F1, factor = 0.1, amount = 0.05), 
         F2 = jitter(F2, factor = 0.1, amount = 0.05))
rm(candidates)
rm(data_orig)
rm(elected)
rm(survey)
rm(valkompass)
joined_dataset[joined_dataset$vald=="Avgått",]$vald <- 0

joined_dataset[joined_dataset$Parti=="Folkpartiet",]$Parti <- "Liberalerna"

joined_dataset[joined_dataset$Namn=="Patrick Reslow",]$Parti <- "Sverigedemokraterna"
#joined_dataset %>% View

save.image(file="valkompass2014/.RData")
