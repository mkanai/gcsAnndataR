#' R6 Class for GCSAnnData
#'
#' An R6 class that wraps the Python GCSAnnData object for accessing h5ad files
#' stored on Google Cloud Storage.
#'
#' @export
GCSAnnData <- R6::R6Class(
  "GCSAnnData",

  public = list(
    #' @field py_object The underlying Python GCSAnnData object
    py_object = NULL,

    #' @description
    #' Create a new GCSAnnData object
    #'
    #' @param gcs_path Path to the h5ad file in GCS (gs://bucket-name/path/to/file.h5ad)
    #' @return A new GCSAnnData object
    initialize = function(gcs_path) {
      gcs_anndata <- import_gcs_anndata()
      self$py_object <- gcs_anndata$GCSAnnData(gcs_path)
    },

    #' @description
    #' Get shape of the AnnData object
    #'
    #' @return A vector of (n_obs, n_vars)
    shape = function() {
      shape <- self$py_object$shape
      return(c(shape[[1]], shape[[2]]))
    },

    #' @description
    #' Get column names (var_names)
    #'
    #' @return A character vector of variable names
    var_names = function() {
      return(as.character(self$py_object$var_names))
    },

    #' @description
    #' Get row names (obs_names)
    #'
    #' @return A character vector of observation names
    obs_names = function() {
      return(as.character(self$py_object$obs_names))
    },

    #' @description
    #' Get rows from the X matrix
    #'
    #' @param indices Vector of row indices (1-based) or row names
    #' @param as_df Logical indicating whether to return a data.frame (default: FALSE)
    #' @param add_names Logical indicating whether to add row and column names (default: TRUE)
    #' @param zero_based Logical indicating whether indices are 0-based (default: FALSE)
    #' @return A sparse Matrix object or data.frame
    get_rows = function(indices,
                        as_df = FALSE,
                        add_names = FALSE,
                        zero_based = FALSE) {
      if (is.numeric(indices)) {
        if (!zero_based) {
          indices <- indices - 1
        }
        indices <- as.integer(indices)
      }

      # Get rows from Python object
      mat <- self$py_object$get_rows(indices, as_df = as_df)
      if (as_df) {
        return(mat)
      }

      # Add row and column names if available
      if (add_names) {
        if (is.numeric(indices)) {
          rownames(mat) <- self$obs_names()[indices + 1]
        } else {
          rownames(mat) <- indices
        }
        colnames(mat) <- self$var_names()
      }

      return(mat)
    },


    #' @description
    #' Get columns from the X matrix
    #'
    #' @param indices Vector of column indices (1-based) or column names
    #' @param as_df Logical indicating whether to return a data.frame (default: FALSE)
    #' @param add_names Logical indicating whether to add row and column names (default: TRUE)
    #' @param zero_based Logical indicating whether indices are 0-based (default: FALSE)
    #' @return A sparse Matrix object or data.frame
    get_columns = function(indices,
                           as_df = FALSE,
                           add_names = FALSE,
                           zero_based = FALSE) {
      if (is.numeric(indices)) {
        if (!zero_based) {
          indices <- indices - 1
        }
        indices <- as.integer(indices)
      }

      # Get columns from Python object
      mat <- self$py_object$get_columns(indices, as_df = as_df)
      if (as_df) {
        return(mat)
      }

      # Add row and column names if available
      if (add_names) {
        rownames(mat) <- self$obs_names()
        if (is.numeric(indices)) {
          colnames(mat) <- self$var_names()[indices + 1]
        } else {
          colnames(mat) <- indices
        }
      }

      return(mat)
    },

    #' @description
    #' Print information about the GCSAnnData object
    print = function() {
      shape <- self$shape()
      cat("GCSAnnData object with n_obs × n_vars =",
          shape[1],
          "×",
          shape[2],
          "\n")

      # Try to get obs and var keys
      tryCatch({
        obs_keys <- names(self$get_obs())
        if (length(obs_keys) > 0) {
          cat("obs: '", paste(obs_keys, collapse = "', '"), "'\n", sep = "")
        }
      }, error = function(e) {

      })

      tryCatch({
        var_keys <- names(self$get_var())
        if (length(var_keys) > 0) {
          cat("var: '", paste(var_keys, collapse = "', '"), "'\n", sep = "")
        }
      }, error = function(e) {

      })

      invisible(self)
    }
  )
)
