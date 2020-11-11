## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, 
                      warning = TRUE,
                      comment = "#>")

## ----setupr, message=FALSE----------------------------------------------------
library("BaseSet")
library("dplyr")

## ----prepare_GO, message=FALSE------------------------------------------------
# We load some libraries
library("org.Hs.eg.db")
library("GO.db")
library("ggplot2")
# Prepare the data 
h2GO_TS <- tidySet(org.Hs.egGO)
h2GO <- as.data.frame(org.Hs.egGO)

## ----evidence_ontology--------------------------------------------------------
library("forcats")
h2GO %>% 
  group_by(Evidence, Ontology) %>% 
  count(name = "Freq") %>% 
  ungroup() %>% 
  mutate(Evidence = fct_reorder2(Evidence, Ontology, -Freq),
         Ontology = case_when(Ontology == "CC" ~ "Cellular Component",
                              Ontology == "MF" ~ "Molecular Function",
                              Ontology == "BP" ~ "Biological Process",
                              TRUE ~ NA_character_)) %>% 
  ggplot() +
  geom_col(aes(Evidence, Freq)) +
  facet_grid(~Ontology) + 
  theme_minimal() +
  coord_flip() +
  labs(x = element_blank(), y = element_blank(),
       title = "Evidence codes for each ontology")

## ----nEvidence_plot-----------------------------------------------------------
h2GO_TS %>% 
  relations() %>% 
  group_by(elements, sets) %>% 
  count(sort = TRUE, name = "Annotations") %>% 
  ungroup() %>% 
  count(Annotations, sort = TRUE) %>% 
  ggplot() +
  geom_col(aes(Annotations, n)) +
  theme_minimal() +
  labs(x = "Evidence codes", y = "Annotations", 
       title = "Evidence codes for each annotation",
       subtitle = "in human") +
  scale_x_continuous(breaks = 1:7)

## ----numbers------------------------------------------------------------------
# Add all the genes and GO terms
library("GO.db")
h2GO_TS <- add_elements(h2GO_TS, keys(org.Hs.eg.db)) %>% 
  add_sets(grep("^GO:", keys(GO.db), value = TRUE))

sizes_element <- element_size(h2GO_TS) %>% 
    arrange(desc(size))
sum(sizes_element$size == 0)
sum(sizes_element$size != 0)

sizes_set <- set_size(h2GO_TS) %>% 
    arrange(desc(size))
sum(sizes_set$size == 0)
sum(sizes_set$size != 0)

## ----plots_GO-----------------------------------------------------------------
sizes_element %>% 
    filter(size != 0) %>% 
    ggplot() +
    geom_histogram(aes(size), binwidth = 1) +
    theme_minimal() +
    labs(x = "# sets per element", y = "Count")

sizes_set %>% 
    filter(size != 0) %>% 
    ggplot() +
    geom_histogram(aes(size), binwidth = 1) +
    theme_minimal() +
    labs(x = "# elements per set", y = "Count")

## ----distr_sizes--------------------------------------------------------------
head(sizes_set, 10)

## ----fuzzy_setup--------------------------------------------------------------
nr <- h2GO_TS %>% 
  relations() %>% 
  dplyr::select(sets, Evidence) %>% 
  distinct() %>% 
  mutate(fuzzy = case_when(
    Evidence == "EXP" ~ 0.9,
    Evidence == "IDA" ~ 0.8,
    Evidence == "IPI" ~ 0.8,
    Evidence == "IMP" ~ 0.75,
    Evidence == "IGI" ~ 0.7,
    Evidence == "IEP" ~ 0.65,
    Evidence == "HEP" ~ 0.6,
    Evidence == "HDA" ~ 0.6,
    Evidence == "HMP" ~ 0.5,
    Evidence == "IBA" ~ 0.45,
    Evidence == "ISS" ~ 0.4,
    Evidence == "ISO" ~ 0.32,
    Evidence == "ISA" ~ 0.32,
    Evidence == "ISM" ~ 0.3,
    Evidence == "RCA" ~ 0.2,
    Evidence == "TAS" ~ 0.15,
    Evidence == "NAS" ~ 0.1,
    Evidence == "IC" ~ 0.02,
    Evidence == "ND" ~ 0.02,
    Evidence == "IEA" ~ 0.01,
    TRUE ~ 0.01)) %>% 
  dplyr::select(sets = "sets", elements = "Evidence", fuzzy = fuzzy)

## ----fuzzy_setup2-------------------------------------------------------------
ts <- h2GO_TS %>% 
  relations() %>% 
  dplyr::select(-Evidence) %>% 
  rbind(nr) %>% 
  tidySet() %>% 
  mutate_element(Type = ifelse(grepl("^[0-9]+$", elements), "gene", "evidence"))

## ----cardinality--------------------------------------------------------------
ts %>% 
  dplyr::filter(Type != "Gene") %>% 
  cardinality() %>% 
  arrange(desc(cardinality)) %>% 
  head()

## ----size_go------------------------------------------------------------------
ts %>% 
  filter(sets %in% c("GO:0008152", "GO:0003674", "GO:0005575"),
         Type != "gene") %>% 
  set_size()

## ----evidence_go--------------------------------------------------------------
ts %>% 
  filter(sets %in% c("GO:0008152", "GO:0003674", "GO:0005575")) %>% 
  filter(Type != "gene") 

## ----prepare_reactome---------------------------------------------------------
# We load some libraries
library("reactome.db")

# Prepare the data (is easier, there isn't any ontoogy or evidence column)
h2p <- as.data.frame(reactomeEXTID2PATHID)
colnames(h2p) <- c("sets", "elements")
# Filter only for human pathways
h2p <- h2p[grepl("^R-HSA-", h2p$sets), ]

# There are duplicate relations with different evidence codes!!: 
summary(duplicated(h2p[, c("elements", "sets")]))
h2p <- unique(h2p)
# Create a tidySet and 
h2p_TS <- tidySet(h2p) %>% 
    # Add all the genes 
    add_elements(keys(org.Hs.eg.db))

## ----numbers_pathways---------------------------------------------------------
sizes_element <- element_size(h2p_TS) %>% 
    arrange(desc(size))
sum(sizes_element$size == 0)
sum(sizes_element$size != 0)

sizes_set <- set_size(h2p_TS) %>% 
    arrange(desc(size))

## ----pathways_plots-----------------------------------------------------------
sizes_element %>% 
    filter(size != 0) %>% 
    ggplot() +
    geom_histogram(aes(size), binwidth = 1) +
    scale_y_log10() +
    theme_minimal() +
    labs(x = "# sets per element", y = "Count")

sizes_set %>% 
    ggplot() +
    geom_histogram(aes(size), binwidth = 1) +
    scale_y_log10() +
    theme_minimal() +
    labs(x = "# elements per set", y = "Count")

## ----distr_sizes_pathways-----------------------------------------------------
head(sizes_set, 10)

## ----sessionInfo, echo=FALSE--------------------------------------------------
sessionInfo()

