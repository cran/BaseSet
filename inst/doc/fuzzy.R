## ----setup, message=FALSE, warning=FALSE, include=FALSE-----------------------
knitr::opts_knit$set(root.dir = ".")
knitr::opts_chunk$set(collapse = TRUE, 
                      warning = TRUE,
                      comment = "#>")
library("BaseSet")
library("dplyr")

## ----fuzzy--------------------------------------------------------------------
set.seed(4567) # To be able to have exact replicates
relations <- data.frame(sets = c(rep("A", 5), "B", "C"),
                          elements = c(letters[seq_len(6)], letters[6]),
                          fuzzy = runif(7))
fuzzy_set <- tidySet(relations)

## ----union--------------------------------------------------------------------
BaseSet::union(fuzzy_set, sets = c("A", "B"))
BaseSet::union(fuzzy_set, sets = c("A", "B"), name = "D")

## ----union_logic--------------------------------------------------------------

BaseSet::union(fuzzy_set, sets = c("A", "B"), FUN = function(x){sqrt(sum(x))})

## ----intersection-------------------------------------------------------------
intersection(fuzzy_set, sets = c("B", "C"), keep = FALSE)
intersection(fuzzy_set, sets = c("B", "C"), keep = FALSE, FUN = "mean")

## ----complement---------------------------------------------------------------
complement_set(fuzzy_set, sets = "A", keep = FALSE)

## ----complement_previous------------------------------------------------------
filter(fuzzy_set, sets == "A")
complement_set(fuzzy_set, sets = "A", keep = FALSE, FUN = function(x){1-x^2})

## ----subtract-----------------------------------------------------------------
subtract(fuzzy_set, set_in = "A", not_in = "B", keep = FALSE, name = "A-B")
# Or the opposite B-A, but using the default name:
subtract(fuzzy_set, set_in = "B", not_in = "A", keep = FALSE)

## ----set_size-----------------------------------------------------------------
set_size(fuzzy_set)

## ----element_size-------------------------------------------------------------
element_size(fuzzy_set)

## ----cells_0------------------------------------------------------------------
sc_classification <- data.frame(
  elements = c("D2ex_1", "D2ex_10", "D2ex_11", "D2ex_12", "D2ex_13", "D2ex_14", 
               "D2ex_15", "D2ex_16", "D2ex_17", "D2ex_18", "D2ex_1", "D2ex_10", 
               "D2ex_11", "D2ex_12", "D2ex_13", "D2ex_14", "D2ex_15", "D2ex_16",
               "D2ex_17", "D2ex_18", "D2ex_1", "D2ex_10", "D2ex_11", "D2ex_12", 
               "D2ex_13", "D2ex_14", "D2ex_15", "D2ex_16", "D2ex_17", "D2ex_18", 
               "D2ex_1", "D2ex_10", "D2ex_11", "D2ex_12", "D2ex_13", "D2ex_14", 
               "D2ex_15", "D2ex_16", "D2ex_17", "D2ex_18"), 
  sets = c("alpha", "alpha", "alpha", "alpha", "alpha", "alpha", "alpha", 
           "alpha", "alpha", "alpha", "endothel", "endothel", "endothel", 
           "endothel", "endothel", "endothel", "endothel", "endothel", 
           "endothel", "endothel", "delta", "delta", "delta", "delta", "delta", 
           "delta", "delta", "delta", "delta", "delta", "beta", "beta", "beta", 
           "beta", "beta", "beta", "beta", "beta", "beta", "beta"), 
  fuzzy = c(0.18, 0.169, 0.149, 0.192, 0.154, 0.161, 0.169, 0.197, 0.162, 0.201, 
            0.215, 0.202, 0.17, 0.227, 0.196, 0.215, 0.161, 0.195, 0.178, 
            0.23, 0.184, 0.172, 0.153, 0.191, 0.156, 0.167, 0.165, 0.184, 
            0.162, 0.194, 0.197, 0.183, 0.151, 0.208, 0.16, 0.169, 0.169, 
            0.2, 0.154, 0.208), stringsAsFactors = FALSE)
head(sc_classification)

## ----cells_classification-----------------------------------------------------
sc_classification %>% 
  group_by(elements) %>% 
  filter(fuzzy == max(fuzzy)) %>% 
  group_by(sets) %>% 
  count()

## ----cells_subset-------------------------------------------------------------
scTS <- tidySet(sc_classification) # Conversion of format
sample_cells <- scTS %>% 
  element_size() %>% 
  group_by(elements) %>% 
  filter(probability == max(probability))
sample_cells

## ----celltypes----------------------------------------------------------------
scTS %>% 
  set_size() %>% 
  group_by(sets) %>% 
  filter(probability == max(probability))

## ----sessionInfo, echo=FALSE--------------------------------------------------
sessionInfo()

