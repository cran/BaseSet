## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, 
                      warning = TRUE,
                      comment = "#>")
run_vignette <- requireNamespace("GSEABase", quietly = TRUE) &&
    requireNamespace("GO.db", quietly = TRUE) &&  
    requireNamespace("reactome.db", quietly = TRUE) && 
    requireNamespace("org.Hs.eg.db", quietly = TRUE) &&  
    requireNamespace("ggplot2", quietly = TRUE) &&
    requireNamespace("forcats", quietly = TRUE)

## ----setupr, message=FALSE----------------------------------------------------
library("BaseSet", quietly = TRUE)
library("dplyr", quietly = TRUE)

## ----prepare_GO, message=FALSE, eval=run_vignette-----------------------------
#  # We load some libraries
#  library("org.Hs.eg.db", quietly = TRUE)
#  library("GO.db", quietly = TRUE)
#  library("ggplot2", quietly = TRUE)
#  # Prepare the data
#  h2GO_TS <- tidySet(org.Hs.egGO)
#  h2GO <- as.data.frame(org.Hs.egGO)

## ----evidence_ontology, eval=run_vignette-------------------------------------
#  library("forcats", include.only = "fct_reorder2", quietly = TRUE)
#  h2GO %>%
#      group_by(Evidence, Ontology) %>%
#      count(name = "Freq") %>%
#      ungroup() %>%
#      mutate(Evidence = fct_reorder2(Evidence, Ontology, -Freq),
#             Ontology = case_match(Ontology,
#                                   "CC" ~ "Cellular Component",
#                                   "MF" ~ "Molecular Function",
#                                   "BP" ~ "Biological Process",
#                                   .default = NA)) %>%
#      ggplot() +
#      geom_col(aes(Evidence, Freq)) +
#      facet_grid(~Ontology) +
#      theme_minimal() +
#      coord_flip() +
#      labs(x = element_blank(), y = element_blank(),
#           title = "Evidence codes for each ontology")

## ----nEvidence_plot, eval=run_vignette----------------------------------------
#  h2GO_TS %>%
#      relations() %>%
#      group_by(elements, sets) %>%
#      count(sort = TRUE, name = "Annotations") %>%
#      ungroup() %>%
#      count(Annotations, sort = TRUE) %>%
#      ggplot() +
#      geom_col(aes(Annotations, n)) +
#      theme_minimal() +
#      labs(x = "Evidence codes", y = "Annotations",
#           title = "Evidence codes for each annotation",
#           subtitle = "in human") +
#      scale_x_continuous(breaks = 1:7)

## ----numbers, eval=run_vignette-----------------------------------------------
#  # Add all the genes and GO terms
#  h2GO_TS <- add_elements(h2GO_TS, keys(org.Hs.eg.db)) %>%
#      add_sets(grep("^GO:", keys(GO.db), value = TRUE))
#  
#  sizes_element <- element_size(h2GO_TS) %>%
#      arrange(desc(size))
#  sum(sizes_element$size == 0)
#  sum(sizes_element$size != 0)
#  
#  sizes_set <- set_size(h2GO_TS) %>%
#      arrange(desc(size))
#  sum(sizes_set$size == 0)
#  sum(sizes_set$size != 0)

## ----plots_GO, eval=run_vignette----------------------------------------------
#  sizes_element %>%
#      filter(size != 0) %>%
#      ggplot() +
#      geom_histogram(aes(size), binwidth = 1) +
#      theme_minimal() +
#      labs(x = "# sets per element", y = "Count")
#  
#  sizes_set %>%
#      filter(size != 0) %>%
#      ggplot() +
#      geom_histogram(aes(size), binwidth = 1) +
#      theme_minimal() +
#      labs(x = "# elements per set", y = "Count")

## ----distr_sizes, eval=run_vignette-------------------------------------------
#  head(sizes_set, 10)

## ----fuzzy_setup, eval=run_vignette-------------------------------------------
#  nr <- h2GO_TS %>%
#      relations() %>%
#      dplyr::select(sets, Evidence) %>%
#      distinct() %>%
#      mutate(fuzzy = case_match(Evidence,
#                                "EXP" ~ 0.9,
#                                "IDA" ~ 0.8,
#                                "IPI" ~ 0.8,
#                                "IMP" ~ 0.75,
#                                "IGI" ~ 0.7,
#                                "IEP" ~ 0.65,
#                                "HEP" ~ 0.6,
#                                "HDA" ~ 0.6,
#                                "HMP" ~ 0.5,
#                                "IBA" ~ 0.45,
#                                "ISS" ~ 0.4,
#                                "ISO" ~ 0.32,
#                                "ISA" ~ 0.32,
#                                "ISM" ~ 0.3,
#                                "RCA" ~ 0.2,
#                                "TAS" ~ 0.15,
#                                "NAS" ~ 0.1,
#                                "IC" ~ 0.02,
#                                "ND" ~ 0.02,
#                                "IEA" ~ 0.01,
#                                .default = 0.01)) %>%
#      dplyr::select(sets = "sets", elements = "Evidence", fuzzy = fuzzy)

## ----fuzzy_setup2, eval=run_vignette------------------------------------------
#  ts <- h2GO_TS %>%
#      relations() %>%
#      dplyr::select(-Evidence) %>%
#      rbind(nr) %>%
#      tidySet() %>%
#      mutate_element(Type = ifelse(grepl("^[0-9]+$", elements), "gene", "evidence"))

## ----cardinality, eval=run_vignette-------------------------------------------
#  ts %>%
#      dplyr::filter(Type != "Gene") %>%
#      cardinality() %>%
#      arrange(desc(cardinality)) %>%
#      head()

## ----size_go, eval=run_vignette-----------------------------------------------
#  ts %>%
#      filter(sets %in% c("GO:0008152", "GO:0003674", "GO:0005575"),
#             Type != "gene") %>%
#      set_size()

## ----evidence_go, eval=run_vignette-------------------------------------------
#  go_terms <- c("GO:0008152", "GO:0003674", "GO:0005575")
#  ts %>%
#      filter(sets %in% go_terms & Type != "gene")

## ----prepare_reactome, eval=run_vignette--------------------------------------
#  # We load some libraries
#  library("reactome.db")
#  
#  # Prepare the data (is easier, there isn't any ontoogy or evidence column)
#  h2p <- as.data.frame(reactomeEXTID2PATHID)
#  colnames(h2p) <- c("sets", "elements")
#  # Filter only for human pathways
#  h2p <- h2p[grepl("^R-HSA-", h2p$sets), ]
#  
#  # There are duplicate relations with different evidence codes!!:
#  summary(duplicated(h2p[, c("elements", "sets")]))
#  h2p <- unique(h2p)
#  # Create a TidySet and
#  h2p_TS <- tidySet(h2p) %>%
#      # Add all the genes
#      add_elements(keys(org.Hs.eg.db))

## ----numbers_pathways, eval=run_vignette--------------------------------------
#  sizes_element <- element_size(h2p_TS) %>%
#      arrange(desc(size))
#  sum(sizes_element$size == 0)
#  sum(sizes_element$size != 0)
#  
#  sizes_set <- set_size(h2p_TS) %>%
#      arrange(desc(size))

## ----pathways_plots, eval=run_vignette----------------------------------------
#  sizes_element %>%
#      filter(size != 0) %>%
#      ggplot() +
#      geom_histogram(aes(size), binwidth = 1) +
#      scale_y_log10() +
#      theme_minimal() +
#      labs(x = "# sets per element", y = "Count")
#  
#  sizes_set %>%
#      ggplot() +
#      geom_histogram(aes(size), binwidth = 1) +
#      scale_y_log10() +
#      theme_minimal() +
#      labs(x = "# elements per set", y = "Count")

## ----distr_sizes_pathways, eval=run_vignette----------------------------------
#  head(sizes_set, 10)

## ----sessionInfo, echo=FALSE--------------------------------------------------
sessionInfo()

