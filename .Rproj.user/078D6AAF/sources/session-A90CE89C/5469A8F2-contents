prot_names <- read_excel("./data/202207_PRO_EXTERNAL_TXB_Complie_4Ws_2022_r7_v1_220823.xlsx", 
                         sheet = "a.4Ws - Data entry sheet", 
                         skip = 1) %>%
  head(1) %>% 
  transpose_df() %>% 
  select(rowname) %>% 
  mutate(rowname = gsub("\\p{Arabic}", "", rowname, perl = TRUE), 
         rowname = str_trim(str_replace_all(rowname, "\r\n|/|\\\\|\\*", ""))) %>% 
  pull(rowname)

prot <- read_excel("./data/202207_PRO_EXTERNAL_TXB_Complie_4Ws_2022_r7_v1_220823.xlsx", 
                   sheet = "a.4Ws - Data entry sheet", 
                   skip = 1) %>%
  setNames(prot_names) %>% 
  clean_names() %>% 
  rename(delivery_modality = delivery_modaity, 
         admin1pcode = code_governorate, 
         admin2pcode = code_district, 
         admin3pcode = code_sub_district, 
         admin4pcode = code_commune_village_town) 

nut_names <- read_excel("./data/202207_NC-Full Database - NW-23082022.xlsx", 
                        sheet = "4Ws") %>% 
  head(1) %>% 
  transpose_df() %>% 
  select(rowname) %>% 
  mutate(rowname = gsub("\\p{Arabic}", "", rowname, perl = TRUE), 
         rowname = str_trim(str_replace_all(rowname, "\r\n|/|\\\\", ""))) %>% 
  pull(rowname)