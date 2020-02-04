#' @title Cleans GCS by replacing with NA where concurrently sedated
#'
#' @description
#' Cleans GCS score
#'
#' @import data.table
#' @param dt data.table containing physiology data
#' @param gcs_ GCS
#' @param sedatives_ List of field names for sedatives; if null then uses the ITEM_REF dictionary
#'

#' @export
clean_gcs <- function(data, gcs_="gcs", sedatives_=NULL, verbose=FALSE) {
  # For debugging
  # data <- copy(wdt)
  # gcs_ <- "gcs"

  # Clean up GCS
  # Identify all sedative and analgesic infusions
  # Either use field names provided or pull names from ITEM_REF
  if (is.null(sedatives_)) {
    assert_that("ccdata" %in% installed.packages()[,1])
    dict.item_ref <- ccdata:::ITEM_REF
    (sedatives <- which_NHIC("Classification3", "Sedatives_continuous_infusion", dict_=dict.item_ref))
  } else {
    sedatives <- sedatives_
  }
  # Now filter out sedatives that are not in the data
  sedatives <- Filter(function(y) y %in% names(data),sedatives)
  assert_that(length(sedatives) > 0)
  # Create a sedation indicator
  data[,sedated := FALSE]
  for (i in 1:length(sedatives)) {
    data[!is.na(get(sedatives[i])), ("sedated") := TRUE, with=FALSE]
  }
  table(data$sedated)
  data[,gcs.clean:=NULL]
  data[, ("gcs.clean") := ifelse(get("sedated")==TRUE,NA,get(gcs_)),with=FALSE]
  # data[,.N,by=gcs][order(gcs)]
  if (verbose) {
    data[,.N,by=.(gcs,gcs.clean)][order(gcs.clean,gcs)]
  }
  # don't need to return b/c data.table works with reference therefore
  # changes are made to original object
  return(data)
}

