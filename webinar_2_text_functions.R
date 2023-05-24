library(tidyverse)
library(rdracor)

install.packages("remotes")
remotes::install_github("dracor-org/rdracor")
#library(xml2)
#install.packages("udpipe")
#library(udpipe)

dream_chr_spoken <- rdracor::get_text_chr_spoken("a-midsummer-night-s-dream",
                                                 "shake")
dream_chr_spoken_together <- rdracor::get_text_chr_spoken("a-midsummer-night-s-dream",
                                                          "shake",
                                                          split_text = FALSE)

dream_chr_spoken_together

dream_chr_spoken_female <- rdracor::get_text_chr_spoken("a-midsummer-night-s-dream",
                                                          "shake",
                                                          gender = "FEMALE")

dream_chr_spoken_female
dream_chr_spoken_female_together <- rdracor::get_text_chr_spoken("a-midsummer-night-s-dream",
                                                        "shake",
                                                        gender = "FEMALE",
                                                        split_text = FALSE)

dream_by_ch <- rdracor::get_text_chr_spoken_bych("a-midsummer-night-s-dream",
                                  "shake")
dream_by_ch
dream_by_ch$FAIRIES.TITANIA.1_MND

dream_by_ch_together <- rdracor::get_text_chr_spoken_bych("a-midsummer-night-s-dream",
                                                 "shake", split_text = FALSE)
dream_by_ch_together
nchar(dream_by_ch_together)
names(dream_by_ch_together)[str_detect(dream_by_ch_together, "[lL]ord")]

dream_by_ch_df <- rdracor::get_text_chr_spoken_bych("a-midsummer-night-s-dream",
                                  "shake",
                                  split_text = FALSE,
                                  as_data_frame = TRUE)

dream_by_ch_df_together <- rdracor::get_text_chr_spoken_bych("a-midsummer-night-s-dream",
                                                    "shake",
                                                    split_text = TRUE,
                                                    as_data_frame = TRUE)
dream_by_ch_df_together
# dream_by_ch_df_together %>%
#   unnest(text)

dream_by_ch_df_together %>%
  mutate(n_lines = map_int(text, length))

dream_cast <- rdracor::get_play_cast("a-midsummer-night-s-dream", "shake")

dream_by_ch_df_together %>%
  mutate(n_lines = map_int(text, length)) %>%
  left_join(dream_cast) %>% View()

dream_stage <- rdracor::get_text_chr_stage("a-midsummer-night-s-dream", "shake")
dream_stage
dream_stage_sp <- get_text_chr_stage_with_sp("a-midsummer-night-s-dream", "shake")
dream_stage_sp

dream_tidy_df <- rdracor::get_text_df("a-midsummer-night-s-dream", "shake")
dream_tidy_df

dream_tidy_df %>%
  filter(type == "stage") %>%
  count(who, sort = TRUE)

dream_tidy_df %>%
  filter(subdiv_id == 312) %>% View()

dream_tidy_df %>%
  filter(str_detect(text, "[dD]ream")) %>%
  count(who)

dream_tidy_df %>%
  filter(type %in% c("l", "p")) %>%
  count(scene_id, scene, who) %>%
  group_by(who) %>%
  mutate(cum_n = cumsum(n)) %>%
  ggplot(aes(x = scene_id, y = cum_n, group = who)) +
  geom_line() +
  geom_point()

dream_tei <- get_text_tei("a-midsummer-night-s-dream", "shake")
dream_tei_as_text <- get_text_tei("a-midsummer-night-s-dream", "shake", parse = FALSE)
dream_tei_as_text

dream_tei %>%
  xml2::xml_ns_strip()

dream_tei_list <- dream_tei %>%
  xml2::as_list()

dream_tei_list$TEI$text$front$castList
dream_tei_list$TEI$text$body$div[[2]]

dream_tidy_df %>%
  filter(type == "stage") %>%
  filter(str_detect(text, "Helena"))

dream_tidy_df %>%
  filter(type == "stage") %>%
  filter(str_detect(text, "[Hh]er") |
           str_detect(text, "[Ss]he") |
           str_detect(text, "Helena"))

install.packages("udpipe")
library(udpipe)
udmodel <- udpipe_download_model(language = "english-ewt")

dream_tidy_df <- dream_tidy_df %>%
  mutate(doc_id = line_id)

udpiped <- udpipe(dream_tidy_df, udmodel)

udpiped %>%
  mutate(doc_id = as.integer(doc_id)) %>%
  left_join(dream_tidy_df) %>%
  left_join(dream_cast, by = c("who" = "id")) %>% View()

amph_char_net_df %>%
  mutate(across(where(is.numeric), rank, ties.method = "min"))

amph_char_net_df %>%
  mutate(across(where(is.numeric), function(x) rank(-x, ties.method = "min")))
