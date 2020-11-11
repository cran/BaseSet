## ----setup, message=FALSE, warning=FALSE, include=FALSE-----------------------
knitr::opts_knit$set(root.dir = ".")
knitr::opts_chunk$set(collapse = TRUE, 
                      warning = TRUE,
                      comment = "#>")

## ----from_list, message=FALSE-------------------------------------------------
library("BaseSet")
gene_lists <- list(
    geneset1 = c("A", "B"),
    geneset2 = c("B", "C", "D")
)
tidy_set <- tidySet(gene_lists)
tidy_set

## ----metadata, message=FALSE--------------------------------------------------
gene_data <- data.frame(
    stat1     = c( 1,   2,   3,   4 ),
    info1     = c("a", "b", "c", "d")
)

tidy_set <- add_column(tidy_set, "elements", gene_data)
set_data <- data.frame(
    Group     = c(      100,        200 ),
    Colum     = c(     "abc",      "def")
)
tidy_set <- add_column(tidy_set, "sets", set_data)
tidy_set

## ----getters------------------------------------------------------------------
relations(tidy_set)
elements(tidy_set)
sets(tidy_set)

## ----tidySet_matrix-----------------------------------------------------------
m <- matrix(c(0, 0, 1, 1, 1, 1, 0, 1, 0), ncol = 3, nrow =3,  
               dimnames = list(letters[1:3], LETTERS[1:3]))
m
tidy_set <- tidySet(m)

## ----as.list------------------------------------------------------------------
as.list(tidy_set)

## ----incidence----------------------------------------------------------------
incidence(tidy_set)

## ----union--------------------------------------------------------------------
BaseSet::union(tidy_set, sets = c("C", "B"), name = "D")

## ----intersection-------------------------------------------------------------
intersection(tidy_set, sets = c("A", "B"), name = "D", keep = TRUE)

## ----intersection2------------------------------------------------------------
intersection(tidy_set, sets = c("A", "B"), name = "D", keep = FALSE)

## ----complement---------------------------------------------------------------
complement_set(tidy_set, sets = c("A", "B"))

## ----complement2--------------------------------------------------------------
complement_set(tidy_set, sets = c("A", "B"), name = "F")

## ----subtract-----------------------------------------------------------------
out <- subtract(tidy_set, set_in = "A", not_in = "B", name = "A-B")
out
name_sets(out)
subtract(tidy_set, set_in = "B", not_in = "A", keep = FALSE)

## ----n------------------------------------------------------------------------
nElements(tidy_set)
nSets(tidy_set)
nRelations(tidy_set)

## ----set_size-----------------------------------------------------------------
set_size(tidy_set, "A")

## ----element_size-------------------------------------------------------------
element_size(tidy_set)

## ----name---------------------------------------------------------------------
name_elements(tidy_set)
name_elements(tidy_set) <- paste0("Gene", seq_len(nElements(tidy_set)))
name_elements(tidy_set)
name_sets(tidy_set)
name_sets(tidy_set) <- paste0("Geneset", seq_len(nSets(tidy_set)))
name_sets(tidy_set)

## ----tidyverse----------------------------------------------------------------
library("dplyr")
m_TS <- tidy_set %>% 
  activate("relations") %>% 
  mutate(Important = runif(nRelations(tidy_set)))
m_TS

## ----deactivate---------------------------------------------------------------
set_modified <- m_TS %>% 
  activate("elements") %>% 
  mutate(Pathway = if_else(elements %in% c("Gene1", "Gene2"), 
                           "pathway1", 
                           "pathway2"))
set_modified
set_modified %>% 
  deactivate() %>% # To apply a filter independently of where it is
  filter(Pathway == "pathway1")

## ----group--------------------------------------------------------------------
# A new group of those elements in pathway1 and with Important == 1
set_modified %>% 
  deactivate() %>% 
  group(name = "new", Pathway == "pathway1")

## ----group2-------------------------------------------------------------------
set_modified %>% 
  group("pathway1", elements %in% c("Gene1", "Gene2"))

## ----moving-------------------------------------------------------------------
elements(set_modified)
out <- move_to(set_modified, "elements", "relations", "Pathway")
relations(out)

## ----sessionInfo, echo=FALSE--------------------------------------------------
sessionInfo()

