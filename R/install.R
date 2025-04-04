#' Install the gcs_anndata Python package
#'
#' This function installs the gcs_anndata Python package using reticulate.
#'
#' @param method Installation method to use
#' @param conda_env Conda environment to use
#' @export
install_gcs_anndata <- function(method = "auto", conda_env = "r-reticulate") {
  reticulate::py_install("gcs_anndata", method = method, conda = conda_env)
}

#' Import the gcs_anndata Python module
#'
#' @return The imported Python module
#' @keywords internal
import_gcs_anndata <- function() {
  reticulate::import("gcs_anndata")
}
