install.packages("rdracor")
library(rdracor)
library(tidyverse)

dracor_api_info()
meta <- get_dracor_meta()
meta
class(meta)
summary(meta)
plot(meta)


span_not_full <- get_dracor("span", full_metadata = FALSE)
span <- get_dracor("span")
span

span %>%
  transmute(yearNormalized, playName, female_ratio = numOfSpeakersFemale/numOfSpeakers) %>%
  ggplot(aes(x = yearNormalized, y = female_ratio)) +
  geom_smooth(colour = "#AA1101") +
  geom_point(size = 3, alpha = .7, colour = "#AA1101") +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
  labs(title = "female ratio in spanish drama corpus increases over time") +
  theme_minimal()

all <- get_dracor()

1:10 %>% sin()
sin(1:10)

all %>%
  group_by(corpus) %>%
  summarise(min_year = min(yearNormalized, na.rm = TRUE),
    mean_year = mean(yearNormalized, na.rm = TRUE),
    max_year = max(yearNormalized, na.rm = TRUE),
    n = n()) %>%
  arrange(mean_year)

summary(all)
summary(span)

wiki_id <- "Q131412"
maria <- get_character_plays(wiki_id)
all_marias <- maria %>%
  left_join(all, by = "id") %>%
  select(playName, corpus) %>%
  mutate(cast = map2(playName, corpus, get_play_cast))

all_marias_unpacked <- all_marias %>%
  unnest_wider(cast) %>%
  unnest_longer(col = c(id:last_col()))

godunov_meta <- get_play_metadata("pushkin-boris-godunov", "rus")
godunov_meta_small <- get_play_metadata("pushkin-boris-godunov", "rus", full_metadata = FALSE)

rdracor::get_play_cast("pushkin-boris-godunov", "rus")
rdracor::get_play_rdf("pushkin-boris-godunov", "rus")


tat <- get_dracor("tat")
tat_with_meta <- tat %>%
  mutate(metadata_list = map2(playName, corpus, get_play_metadata))

tat_with_meta %>%
  select(playName, corpus, metadata_list) %>%
  pull(metadata_list)

godunov_coocur <- rdracor::get_net_cooccur_igraph("pushkin-boris-godunov", "rus")
class(godunov_coocur)
summary(godunov_coocur)
plot(godunov_coocur)
?get_net_cooccur_igraph
plot(godunov_coocur,
     vertex.label = label_cooccur_igraph(godunov_coocur,
                                         top = 10),
     vertex.frame.color = "green")

library(igraph)
V(godunov_coocur)$cluster <- membership(cluster_optimal(godunov_coocur))
V(godunov_coocur)[[]]

plot(godunov_coocur,
     vertex.label = label_cooccur_igraph(godunov_coocur,
                                         top = 10),
     vertex.frame.color = V(godunov_coocur)$cluster,
     vertex.frame.width = 4)

class(godunov_coocur)

install.packages("tidygraph")
tidygraph::as_tbl_graph(godunov_coocur)

get_net_cooccur_graphml("pushkin-boris-godunov", "rus")
get_net_cooccur_gexf("pushkin-boris-godunov", "rus")
get_net_cooccur_edges("pushkin-boris-godunov", "rus")
get_net_cooccur_metrics("pushkin-boris-godunov", "rus")


