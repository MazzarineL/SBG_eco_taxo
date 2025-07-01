library(rotl)
library(plyr)
library(stats)
library(dplyr)

get_lineage_taxo <- function(data_taxo) {
  taxonomy_lineage <-
    rotl::tax_lineage(taxonomy_taxon_info(data_taxo$uid, include_lineage = TRUE))
  # saveRDS(taxonomy_family_lineage, file="data/tmp/taxonomy_family_lineage.RData")
  # taxonomy_family_lineage <- readRDS(file="data/tmp/taxonomy_family_lineage.RData")

  taxonomy_lineage_matt <-
    plyr::ldply(taxonomy_lineage, rbind)

  taxonomy_lineage_wide <-
    stats::reshape(
      taxonomy_lineage_matt,
      idvar = ".id",
      timevar = "rank",
      direction = "wide"
    )

  taxonomy_lineage_wide$.id <-
    as.integer(taxonomy_lineage_wide$.id)

  taxonomy_full <-
    dplyr::left_join(data_taxo,
      taxonomy_lineage_wide,
      by = c("uid" = ".id")
    )

  return(taxonomy_full)
}
