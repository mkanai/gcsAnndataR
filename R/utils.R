#' Read an h5ad file from Google Cloud Storage
#'
#' @param gcs_path Path to the h5ad file in GCS (gs://bucket-name/path/to/file.h5ad)
#' @return A GCSAnnData object
#' @export
read_gcs_h5ad <- function(gcs_path) {
  GCSAnnData$new(gcs_path)
}
