% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AllGenerics.R, R/intersection.R
\name{intersection}
\alias{intersection}
\alias{intersect}
\alias{intersection,TidySet,character-method}
\title{Intersection of two or more sets}
\usage{
intersection(object, sets, ...)

\S4method{intersection}{TidySet,character}(
  object,
  sets,
  name = NULL,
  FUN = "min",
  keep = FALSE,
  keep_relations = keep,
  keep_elements = keep,
  keep_sets = keep,
  ...
)
}
\arguments{
\item{object}{A TidySet object.}

\item{sets}{The character of sets to be intersect.}

\item{...}{Other named arguments passed to \code{FUN}.}

\item{name}{The name of the new set. By defaults joins the sets with an
\ifelse{latex}{\out{$\cup$}}{\ifelse{html}{\out{&cup;}}{}}.}

\item{FUN}{A function to be applied when performing the union.
The standard intersection is the "min" function, but you can provide any
other function that given a numeric vector returns a single number.}

\item{keep}{A logical value if you want to keep originals sets.}

\item{keep_relations}{A logical value if you wan to keep old relations.}

\item{keep_elements}{A logical value if you wan to keep old elements.}

\item{keep_sets}{A logical value if you wan to keep old sets.}
}
\value{
A \code{TidySet} object.
}
\description{
Given a TidySet creates a new set with the elements on the both of them
following the logic defined on FUN.
}
\details{
#' The default uses the \code{min} function following the \href{https://en.wikipedia.org/wiki/Fuzzy_set_operations}{standard fuzzy definition}, but it can
be changed.
}
\section{Methods (by class)}{
\itemize{
\item \code{intersection(object = TidySet, sets = character)}: Applies the standard intersection

}}
\examples{
rel <- data.frame(
    sets = c(rep("A", 5), "B"),
    elements = c("a", "b", "c", "d", "f", "f")
)
TS <- tidySet(rel)
intersection(TS, c("A", "B")) # Default Name
intersection(TS, c("A", "B"), "C") # Set the name
# Fuzzy set
rel <- data.frame(
    sets = c(rep("A", 5), "B"),
    elements = c("a", "b", "c", "d", "f", "f"),
    fuzzy = runif(6)
)
TS2 <- tidySet(rel)
intersection(TS2, c("A", "B"), "C")
intersection(TS2, c("A", "B"), "C", FUN = function(x){max(sqrt(x))})
}
\seealso{
Other methods that create new sets: 
\code{\link{complement_element}()},
\code{\link{complement_set}()},
\code{\link{subtract}()},
\code{\link{union}()}

Other methods: 
\code{\link{TidySet-class}},
\code{\link{activate}()},
\code{\link{add_column}()},
\code{\link{add_relation}()},
\code{\link{arrange.TidySet}()},
\code{\link{cartesian}()},
\code{\link{complement_element}()},
\code{\link{complement_set}()},
\code{\link{complement}()},
\code{\link{element_size}()},
\code{\link{elements}()},
\code{\link{filter.TidySet}()},
\code{\link{group_by.TidySet}()},
\code{\link{group}()},
\code{\link{incidence}()},
\code{\link{is.fuzzy}()},
\code{\link{is_nested}()},
\code{\link{move_to}()},
\code{\link{mutate.TidySet}()},
\code{\link{nElements}()},
\code{\link{nRelations}()},
\code{\link{nSets}()},
\code{\link{name_elements<-}()},
\code{\link{name_sets<-}()},
\code{\link{name_sets}()},
\code{\link{power_set}()},
\code{\link{pull.TidySet}()},
\code{\link{relations}()},
\code{\link{remove_column}()},
\code{\link{remove_element}()},
\code{\link{remove_relation}()},
\code{\link{remove_set}()},
\code{\link{rename_elements}()},
\code{\link{rename_set}()},
\code{\link{select.TidySet}()},
\code{\link{set_size}()},
\code{\link{sets}()},
\code{\link{subtract}()},
\code{\link{union}()}
}
\concept{methods}
\concept{methods that create new sets}
