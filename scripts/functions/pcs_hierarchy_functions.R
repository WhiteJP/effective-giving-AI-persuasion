# =============================================================================
# PCS (PHILANTHROPIC CLASSIFICATION SYSTEM) AND HIERARCHY FUNCTIONS
# =============================================================================
# Functions for working with PCS data and hierarchical classifications
# =============================================================================

# =============================================================================
# PCS FUNCTIONS
# =============================================================================

#' Find missing values with fuzzy matching
#' 
#' Uses Levenshtein distance to find closest matches for missing values
#' 
#' @param vec Character vector. Values to check
#' @param df Data frame. Reference data frame
#' @return Data frame with original values, closest matches, and distances
find_missing_with_match <- function(vec, df) {
  # flatten df to unique character values
  df_vals <- unique(na.omit(as.character(unlist(df))))
  
  # which in vec are truly missing
  missing <- setdiff(vec, df_vals)
  if (length(missing) == 0) {
    return(data.frame(original = character(0),
                      closest  = character(0),
                      distance = integer(0),
                      stringsAsFactors = FALSE))
  }
  
  # for each missing element, find the df_val with minimal adist
  res <- lapply(missing, function(m) {
    dists <- adist(m, df_vals)      # base R Levenshtein distances
    i_min <- which.min(dists)
    data.frame(
      original = m,
      closest  = df_vals[i_min],
      distance = dists[i_min],
      stringsAsFactors = FALSE
    )
  })
  
  do.call(rbind, res)
}

#' Replace terms using a named vector of replacements
#' 
#' Handles semicolon-separated values and deduplicates results
#' 
#' @param vec Character vector. Values to replace
#' @param replacements Named vector. Mapping of old to new values
#' @return Character vector with replacements applied
replace_terms_named <- function(vec, replacements) {
  sapply(vec, function(entry) {
    if (is.na(entry)) return(NA_character_)
    
    parts <- stringr::str_split(entry, ";\\s*")[[1]]
    
    # Replace if in names of the replacement vector/list
    parts_new <- sapply(parts, function(part) {
      if (part %in% names(replacements)) {
        replacements[[part]]
      } else {
        part
      }
    }, USE.NAMES = FALSE)
    
    # Deduplicate and reassemble
    paste(unique(parts_new), collapse = "; ")
  }, USE.NAMES = FALSE)
}

#' Extract unique PCS labels from a vector
#' 
#' Handles semicolon-separated values and sorts results
#' 
#' @param vec Character vector. PCS labels
#' @return Character vector of unique, sorted labels
extract_unique_pcs_labels <- function(vec) {
  vec |>
    replace_na("Unknown or not classified") |>  # Treat NA as a label
    stringr::str_split(";\\s*") |>
    unlist() |>
    unique() |>
    sort()
}

#' Convert character vector to one-hot-encoded matrix
#' 
#' Creates binary variables for each unique PCS label
#' 
#' @param vec Character vector. PCS labels
#' @param prefix Character. Prefix for variable names (default: "pcs")
#' @return Matrix with binary variables for each unique label
make_pcs_matrix <- function(vec, prefix = "pcs") {
  # 1. Extract all unique labels including "NA"
  unique_labels <- extract_unique_pcs_labels(vec)
  label_lookup <- tibble(
    label = unique_labels,
    label_clean = label |>
      str_to_lower() |>
      str_replace_all("[^a-z0-9]+", "_") |>
      str_remove("_$")
  )
  
  # 2. Expand to long format (treat NA as "NA")
  long <- tibble(row_id = seq_along(vec), raw = vec) %>%
    mutate(raw = replace_na(raw, "NA")) %>%
    separate_rows(raw, sep = ";\\s*") %>%
    rename(label = raw)
  
  # 3. Generate variable names
  long_labeled <- long %>%
    left_join(label_lookup, by = "label") %>%
    mutate(varname = paste0(prefix, "_", label_clean)) %>%
    distinct(row_id, varname) %>%
    mutate(value = 1)
  
  # 4. Pivot to wide matrix
  long_labeled %>%
    pivot_wider(names_from = varname, values_from = value, values_fill = 0) %>%
    arrange(row_id) %>%
    select(-row_id)
}

#' Set unknown category for rows with missing PCS data
#' 
#' Creates an "unknown" column and sets other columns to 0 for missing rows
#' 
#' @param data Data frame. PCS matrix data
#' @param prefix Character. Prefix for PCS columns
#' @param unknown_col Character. Name of unknown category column
#' @return Data frame with unknown category properly set
set_unknown_on_na <- function(data, prefix, unknown_col) {
  # 1. Identify all subj_ columns (after possibly adding unknown_col)
  if (!unknown_col %in% names(data)) {
    data[[unknown_col]] <- 0L
  }
  subj_cols  <- names(data)[startsWith(names(data), prefix)]
  other_cols <- setdiff(subj_cols, unknown_col)
  
  data %>%
    # 2. Flag rows where any *other* subj_ col is NA
    mutate(
      .row_na_flag = if_any(all_of(other_cols), is.na)
    ) %>%
    # 3. For flagged rows: set all other subj_ cols to 0, and unknown_col to 1
    mutate(
      across(all_of(other_cols),
             ~ if_else(.row_na_flag, 0L, .)),
      !!unknown_col := if_else(.row_na_flag, 1L, .data[[unknown_col]])
    ) %>%
    select(-.row_na_flag)
}

#' Reduce PCS matrix to columns with sufficient sample size
#' 
#' Filters out sparse categories and creates "other" category if needed
#' 
#' @param d Data frame. PCS matrix data
#' @param prefix Character. Prefix for PCS columns
#' @param min_n Numeric. Minimum sample size per condition (default: 5)
#' @return Data frame with sparse categories removed and "other" category added
reduce_pcs_matrix <- function(d, prefix, min_n = 5) {
  # 1. Count occurrences of each variable
  counts <- d |> 
    group_by(condition) |>
    summarise(across(starts_with(prefix), sum))
  
  # 2. Keep vars that have > min_n for all conditions
  het_vars <- counts |>
    select(starts_with(prefix)) |>
    select(where(~ all(.x >= min_n))) |>
    names()
  
  # 3. Separate into well-supported and sparse vars
  all_vars <- counts |> select(starts_with(prefix)) |> names()
  sparse_vars <- setdiff(all_vars, het_vars)
  
  # 4. If there are sparse vars, create "Other" var
  if (length(sparse_vars) > 0) {
    d <- d |> mutate(
      !!paste0(prefix, "_other_categorization") := rowSums(across(all_of(sparse_vars)))
    )
    het_vars <- c(het_vars, paste0(prefix, "_other_categorization"))
  }
  
  d |> select(-all_of(sparse_vars))
}

#' Format EIN numbers with dash separator
#' 
#' @param eins Character vector. EIN numbers
#' @return Character vector with formatted EIN numbers
format_ein <- function(eins) {
  sub("^(\\d{2})(\\d{7})$", "\\1-\\2", eins)
}

# =============================================================================
# HIERARCHY PARSING FUNCTIONS
# =============================================================================

#' Parse HTML hierarchy file into nested list structure
#' 
#' @param html_file_path Character. Path to HTML hierarchy file
#' @param leaves_as_vec Logical. Whether to return leaves as vectors (default: FALSE)
#' @return Nested list structure representing the hierarchy
parse_hierarchy <- function(html_file_path, leaves_as_vec = FALSE) {
  html <-  rvest::read_html(html_file_path)
  
  # Recursive helper
  rec_parse_hierarchy <- function(node) {
    lis <- rvest::html_elements(node, xpath = "./li")
    
    map(lis, function(li) {
      span <- rvest::html_element(li, "span")
      label <- rvest::html_text(span) |>
        str_trim()
      
      # Skip empty labels
      if (is.na(label) || label == "") return(NULL)
      
      children <- rvest::html_elements(li, "ul")
      
      if (length(children) > 0) {
        set_names(list(rec_parse_hierarchy(children[[1]])), label)
      } else {
        label
      }
    }) |>
      compact() |>  # Remove NULLs from skipped empty labels
      (\(x) {
        if (leaves_as_vec && all(map_lgl(x, is.character))) {
          unlist(x)
        } else {
          x
        }
      })()
  }
  
  top_ul <- rvest::html_element(html, "#browser")
  rec_parse_hierarchy(top_ul)
}

#' Convert hierarchy list to data frame with level columns
#' 
#' @param hierarchy List. Nested hierarchy structure
#' @return Data frame with level columns (lvl_1, lvl_2, etc.)
hierarchy_to_df <- function(hierarchy) {
  collect_paths <- function(node, path = character(), acc = list()) {
    if (is.character(node)) {
      # leaf labels (atomic character vectors too)
      for (lbl in node) {
        acc[[length(acc) + 1]] <- c(path, lbl)
      }
    } else if (is.list(node)) {
      nm <- names(node)
      for (i in seq_along(node)) {
        child  <- node[[i]]
        name_i <- if (!is.null(nm)) nm[i] else NA_character_
        next_path <- if (!is.na(name_i) && nzchar(name_i)) c(path, name_i) else path
        acc <- collect_paths(child, next_path, acc)
      }
    }
    acc
  }
  
  all_paths <- list()
  for (entry in hierarchy) {
    if (is.list(entry) && !is.null(names(entry))) {
      # named subtree → dive in with lvl_1 = the name
      for (branch in names(entry)) {
        all_paths <- c(all_paths,
                       collect_paths(entry[[branch]], path = branch))
      }
    } else if (is.character(entry)) {
      # top‐level atomic labels → treat as a depth‐1 path
      all_paths <- c(all_paths,
                     collect_paths(entry, path = character()))
    }
  }
  
  # now pad to uniform depth
  max_depth <- max(vapply(all_paths, length, integer(1)))
  mat <- do.call(rbind, lapply(all_paths, function(p) {
    length(p) <- max_depth
    p
  }))
  
  # to tibble with lvl_1…lvl_n
  df <- as_tibble(mat)
  colnames(df) <- paste0("lvl_", seq_len(ncol(df)))
  df
}

#' Replace labels with their level-k equivalents in hierarchy
#' 
#' @param vec Character vector. Labels to replace
#' @param hierarchy_df Data frame. Hierarchy data frame with level columns
#' @param k Numeric. Level to use for replacement (default: 1)
#' @param na_replacement Character. Replacement for NA values (default: "Unknown or not classified")
#' @return Character vector with labels replaced by level-k equivalents
replace_with_level <- function(vec, hierarchy_df, k = 1, na_replacement = "Unknown or not classified") {
  lvl_cols <- grep("^lvl_", names(hierarchy_df), value = TRUE)
  lvl_col  <- paste0("lvl_", k)
  if (!lvl_col %in% lvl_cols) {
    stop("No column ", lvl_col, " in hierarchy_df")
  }
  
  sapply(vec, USE.NAMES = FALSE, function(entry) {
    if (is.na(entry)) return(na_replacement)
    
    parts <- str_split(entry, ";\\s*")[[1]]
    new_parts <- vapply(parts, FUN.VALUE = character(1), function(lbl) {
      hits <- hierarchy_df %>%
        filter(if_any(all_of(lvl_cols), ~ .x == lbl))
      
      if (nrow(hits) == 0) {
        # not in hierarchy: leave as is
        return(lbl)
      }
      
      row <- hits[1, ]
      # try lvl_k, then lvl_{k-1}, … down to lvl_1
      for (j in seq(k, 1)) {
        candidate <- row[[paste0("lvl_", j)]]
        if (!is.na(candidate) && nzchar(candidate)) {
          return(candidate)
        }
      }
      # if somehow all are NA, fall back to the original label
      lbl
    })
    
    # collapse back, remove duplicates, clean whitespace
    result <- paste(unique(new_parts), collapse = "; ")
    str_squish(str_replace_all(result, "[\r\n]", " "))
  })
}
