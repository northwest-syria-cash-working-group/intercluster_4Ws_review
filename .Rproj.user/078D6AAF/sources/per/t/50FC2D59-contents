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

com %>% 
  filter(!is.na(admin4pcode) & 
           activity != "ISIMM" & 
           project_status %in% c("Completed", "Ongoing")) %>%
  group_by(admin4pcode) %>% 
  slice(which.max(beneficiaries)) %>% 
  right_join(pop %>% select(admin4pcode, total_pop),
             by = "admin4pcode") %>% 
  mutate(pc = beneficiaries / total_pop) %>% 
  ggplot(aes(x = pc, y = total_pop)) + 
  geom_point(aes(size = beneficiaries), 
             alpha = .5) + 
  scale_x_log10(labels = percent) + 
  scale_y_log10(labels = comma, 
                breaks = c(0, 10, 30, 100, 300, 1000, 3000, 10000, 30000, 100000, 300000)) +
  scale_size_continuous(labels = comma, 
                        breaks = c(100, 1000, 10000, 30000, 100000, 200000)) + 
  geom_smooth(method = "lm", colour = "blue") + 
  labs(title = "") + 
  theme(axis.text.x = element_text(angle = 30, hjust = .5, vjust = .5))

weird <- scales::trans_new("signed_log",
                           transform=function(x) sign(x)*log1p(abs(x)),
                           inverse=function(x) sign(x)*expm1(abs(x)))


com %>% 
  filter(!is.na(admin4pcode) & 
           activity != "ISIMM" & 
           project_status %in% c("Completed", "Ongoing")) %>%
  group_by(admin4pcode, admin3pcode, admin1pcode) %>% 
  slice(which.max(beneficiaries)) %>% 
  ungroup() %>% 
  right_join(pop %>% select(admin4pcode, total_pop), 
             by = "admin4pcode") %>% 
  group_by(admin1pcode, admin3pcode) %>% 
  summarise(total_pop = sum(total_pop), 
            beneficiaries = sum(beneficiaries, na.rm = TRUE), 
            .groups = "drop") %>% 
  mutate(pc = beneficiaries / total_pop, 
         gap = total_pop - beneficiaries) %>%
  filter(admin1pcode %in% c("SY02", "SY07")) %>% 
  ggplot(aes(x = gap, y = reorder_within(admin3pcode, gap, admin1pcode))) + 
  geom_col(aes(fill = pc)) + 
  geom_text(aes(label = gap), hjust = "inward") + 
  scale_y_reordered() + 
  facet_wrap(~ admin1pcode, scales = "free_y") + 
  labs(x = "Percent of population covered", 
       title = "Percentage covered in communities reached by response")

* The number of cluster per admin4 (useful for determining inter-cluster gaps i.e. are there any areas  where we are doing Education without Shelter? Nutrition without Food Security?) 

* The number of activities per admin4 (similar to above, but more detailed. Additionally, this, together with the number of beneficiary frequencies are useful in establishing a shorthand for the overall level of investment the humanitarian community has per community). 

* The number of beneficiary frequencies at admin4. This, together with the number of activities, will be used to determine how industry-wide resources have been allocated across Northwest Syria and if those allocations have been fair i.e. are there communities of similar pre-existing vulnerability and earthquake damage that have received drastically different levels of support? The number of activities and the number of beneficiary frequencies are what are most visible to communities anyway and will be interpreted by affected persons as a proxy of the level of humanitarian interest. 

As a by-product of calculating these data, the following information has also been calculated: 

  
WVI
  
reference_table %>% 
  left_join(com %>% 
              filter(cluster == "Cash" & project_status %in% c("Completed", "Ongoing")) %>% 
              group_by(admin4pcode) %>% 
              summarise(cash_ben = sum(beneficiaries, na.rm = TRUE)), 
            by = "admin4pcode") %>% 
  mutate(total_pop = ifelse(total_pop == 0, beneficiaries, total_pop)) %>% 
  mutate(pc_ben_freq = beneficiary_frequencies / total_pop, 
         pc_ben = beneficiaries / total_pop) %>% 
  filter(admin2pcode %in% c("SY0203", "SY0703", "SY0704")) %>% 
  # ggplot(aes(x = pc_ben_freq)) + 
  # geom_histogram() + 
  # scale_x_log10()
  filter(pc_ben_freq <= .6) %>% 
  # write_csv("./data/admin4_wvi.csv")
  summarise(beneficiaries = sum(beneficiaries, na.rm = TRUE), 
            beneficiary_frequencies = sum(beneficiary_frequencies, na.rm = TRUE), 
            total_pop = sum(total_pop, na.rm = TRUE))

locations %>% 
  filter(admin2name_en %in% c("Harim", "Afrin", "Jisr-Ash-Shugur")) %>% 
  distinct(admin2pcode)

locations %>% filter(str_detect(admin2name_en, "Jisr"))

reference_table %>% 
  left_join(com %>% 
              filter(cluster == "Cash" & project_status %in% c("Completed", "Ongoing")) %>% 
              group_by(admin4pcode) %>% 
              summarise(cash_ben = sum(beneficiaries, na.rm = TRUE)), 
            by = "admin4pcode") %>% 
  write_csv("./data/admin4_reference.csv")


### Activity pairs -- most correlated


com %>%
  filter(activity != "ISIMM") %>% 
  filter(!is.na(admin4pcode) & !is.na(activity)) %>% 
  pairwise_count(activity, admin4pcode, sort = TRUE, upper = FALSE) %>% 
  left_join(com %>%
              filter(activity != "ISIMM") %>%
              filter(!is.na(admin4pcode) & !is.na(activity)) %>%
              pairwise_cor(activity, admin4pcode, sort = TRUE, upper = FALSE), 
            by = c("item1", "item2")) %>% 
  left_join(com %>%
              distinct(cluster1 = cluster, activity),
            by = c("item1" = "activity")) %>% 
  left_join(com %>% 
              distinct(cluster2 = cluster, activity),
            by = c("item2" = "activity")) %>% 
  filter(cluster1 != cluster2) %>% 
  rename(activity1 = item1, 
         activity2 = item2) %>% 
  mutate_at(vars(activity1, activity2), ~ str_sub(., start = 0L, end = 60L)) %>% 
  mutate(correlation = round(correlation, digits = 3)) %>% 
  select(activity1, activity2, corr = correlation, `com.` = n, cluster1, cluster2) %>% 
  arrange(desc(corr)) %>% 
  filter(`com.` > 20)  %>% 
  # head(10) %>% 
  flextable() %>% 
  theme_zebra() %>% 
  set_caption("Most correlated activity pairs") %>% 
  set_table_properties(layout = "autofit", width = .99) %>% 
  fontsize(size = 9, j = 1:2)

  
### Activity pairs -- most common
  

com %>%
  filter(activity != "ISIMM") %>% 
  filter(!is.na(admin4pcode) & !is.na(activity)) %>% 
  pairwise_count(activity, admin4pcode, sort = TRUE, upper = FALSE) %>% 
  left_join(com %>%
              filter(activity != "ISIMM") %>%
              filter(!is.na(admin4pcode) & !is.na(activity)) %>%
              pairwise_cor(activity, admin4pcode, sort = TRUE, upper = FALSE), 
            by = c("item1", "item2")) %>% 
  left_join(com %>%
              distinct(cluster1 = cluster, activity),
            by = c("item1" = "activity")) %>% 
  left_join(com %>% 
              distinct(cluster2 = cluster, activity),
            by = c("item2" = "activity")) %>% 
  filter(cluster1 != cluster2) %>% 
  rename(activity1 = item1, 
         activity2 = item2) %>% 
  mutate_at(vars(activity1, activity2), ~ str_sub(., start = 0L, end = 60L)) %>% 
  mutate(correlation = round(correlation, digits = 3)) %>% 
  select(activity1, activity2, corr = correlation, `com.` = n, cluster1, cluster2) %>% 
  arrange(desc(`com.`)) %>% 
  filter(`com.` > 20)  %>% 
  head(10) %>% 
  flextable() %>% 
  theme_zebra() %>% 
  set_caption("Most common activity pairs") %>% 
  set_table_properties(layout = "autofit", width = .99)


  
  ### Cluster pairs

com %>% 
  filter(activity != "ISIMM") %>% 
  group_by(cluster, admin4pcode) %>% 
  summarise(count = n()) %>% 
  pairwise_cor(cluster, admin4pcode, value = count, 
               method = c("spearman"), upper = FALSE) %>% 
  arrange(desc(correlation)) %>% 
  left_join(
    com %>% 
      filter(activity != "ISIMM") %>% 
      pairwise_count(cluster, admin4pcode), 
    by = c("item1", "item2")
  ) %>% 
  rename(cluster1 = item1, 
         cluster2 = item2, 
         communities = n) %>% 
  mutate(correlation = round(correlation, digits = 3)) %>% 
  # filter(cluster1 == "CWG" | cluster2 == "CWG") %>% 
  head(10)
flextable() %>% 
  theme_zebra() %>% 
  set_caption("Most correlated cluster pairs") %>% 
  set_table_properties(layout = "autofit", width = .7)


st_read("C:/Users/seanywng/Downloads/shakemap/pga.shp") %>% 
  st_as_sf() %>% 
  ggplot() + 
  geom_sf(aes(colour = PGAPOL_)) + 
  scale_colour_viridis(option = "turbo")





