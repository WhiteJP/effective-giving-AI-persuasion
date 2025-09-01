download_osf_dir <- function(osf_node,
                             osf_dir_name,
                             dest_dir,
                             overwrite = FALSE) {
  
  # Ensure destination exists
  fs::dir_create(dest_dir)
  
  message("Resolving OSF node: ", osf_node)
  node <- tryCatch(
    osfr::osf_retrieve_node(osf_node),
    error = function(e) stop("Could not retrieve OSF node '", osf_node, "'. ", e$message, call. = FALSE)
  )
  
  # List top-level files/folders on the node
  top_level <- tryCatch(
    osfr::osf_ls_files(node, n_max = Inf),
    error = function(e) stop("Failed to list files for node '", osf_node, "'. ", e$message, call. = FALSE)
  )
  
  # Find the target folder by name
  data_dir <- top_level[top_level$name == osf_dir_name, , drop = FALSE]
  
  if (nrow(data_dir) == 0L) {
    stop("No folder named '", osf_dir_name,
         "' was found at the top level of OSF node '", osf_node, "'.", call. = FALSE)
  }
  
  all_files <- tryCatch(
    osfr::osf_ls_files(data_dir, n_max = Inf),
    error = function(e) stop("Failed to list files in folder '", osf_dir_name, "'. ", e$message, call. = FALSE)
  )
  
  if (nrow(all_files) == 0L) {
    stop("No files found in OSF folder '", osf_dir_name, "'.", call. = FALSE)
  }
  
  # Check for existing files if overwrite = FALSE
  existing <- fs::path(dest_dir, all_files$name)
  exists_mask <- fs::file_exists(existing)
  if (any(exists_mask) && !overwrite) {
    warning("File(s) with the same name already exist in ", dest_dir, ": ",
         paste(all_files$name[exists_mask], collapse = ", "),
         "\nWe skipped downloading these files. To overwrite them, set overwrite = TRUE.",
         call. = FALSE)
  }
  
  message("Downloading ", nrow(all_files), " files to: ", dest_dir)
  downloaded <- tryCatch(
    osfr::osf_download(all_files,
                 path = dest_dir,
                 conflicts = if (overwrite) "overwrite" else "skip",
                 progress = TRUE),
    error = function(e) stop("Download failed. ", e$message, call. = FALSE)
  )
  
  message("Done! Files saved in: ", dest_dir)
  invisible(downloaded)
}

download_osf_data_dir <- function(
    osf_node,
    dest_dir = here::here("data"),
    overwrite = FALSE
  ) {
  download_osf_dir(osf_node, "data", dest_dir, overwrite = overwrite) 
}