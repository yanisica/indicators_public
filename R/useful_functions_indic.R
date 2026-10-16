## Functions to harmonize indicators

##### Check for duplicates-------
check_dup <- function(file_name,field){
  dup_df = file_name %>% 
    dplyr::mutate(dup1 = duplicated({{field}}, fromLast = TRUE)) %>%  
    dplyr::mutate(dup2 = duplicated({{field}}, fromLast = FALSE)) %>% 
    dplyr::mutate(dup = ifelse(dup1 == TRUE,
                               yes=dup1,
                               no=dup2)) %>% 
    dplyr::filter(dup == TRUE)

  if (nrow(dup_df) > 0){
    print("Check duplicates")
  }else{
    print("No duplicates")
  }
  return(dup_df)
}

##### Check character in other fields (e.g. authorship where there is more flexibility-------
check_odd_chr <- function(file_name = file_name,
                          field = field){
  regexp1 <- "([^a-zA-Z\u00C0-\u024F\u1E00-\u1EFF])" #using extended latin characters
  odd_chr = file_name %>%
    dplyr::mutate(field1 = gsub('\\(', '',{{field}})) %>%
    dplyr::mutate(field1 = gsub('\\)', '',field1)) %>%
    dplyr::mutate(field1 = gsub('\\[', '',field1)) %>%
    dplyr::mutate(field1 = gsub('\\]', '',field1)) %>%
    dplyr::mutate(field1 = gsub('\\.', '',field1)) %>%
    dplyr::mutate(field1 = gsub('\\;', '',field1)) %>%
    dplyr::mutate(field1 = gsub('\\:', '',field1)) %>%
    dplyr::mutate(field1 = gsub('\\,', '',field1)) %>%
    dplyr::mutate(field1 = gsub('\\&', '',field1)) %>%
    dplyr::mutate(field1 = gsub('\\–', '',field1)) %>%
    dplyr::mutate(field1 = gsub('\\/', '',field1)) %>%
    dplyr::mutate(field1 = gsub('\\%', '',field1)) %>%
    dplyr::mutate(field1 = gsub('\\<', '',field1)) %>%
    dplyr::mutate(field1 = gsub('\\>', '',field1)) %>%
    dplyr::mutate(field1 = gsub('\\+', '',field1)) %>%
    dplyr::mutate(field1 = gsub('\\‑', '',field1)) %>%
    dplyr::mutate(field1 = gsub(' ', '',field1)) %>%
    dplyr::mutate(field1 = gsub('  ', '',field1)) %>%
    dplyr::mutate(field1 = gsub('[0-9]', '',field1)) %>%
    dplyr::mutate(field1 = gsub('-', '',field1)) %>%
    dplyr::mutate(field1 = gsub("'", "",field1)) %>%
    dplyr::mutate(field1 = gsub("’", "",field1)) %>%
    dplyr::mutate(field1 = gsub("’", "",field1)) %>%
    dplyr::mutate(bad_chr = grepl(pattern = regexp1, x = field1))  %>%
    dplyr::mutate(field1 = gsub(' ', '',field1)) %>%
    #dplyr::mutate(bad_chr = grepl("[^-'`’;,.äàáâaÁÄåãëèéêÉïíîÍöóòôõøÓØÖüùúûÜÚß&ñÑçÇssSSýYa-zA-Z 0-9]",field1))  %>%
    filter(bad_chr == TRUE)
  
  if (nrow(odd_chr) > 0){
    cat('Check characters')
  }else{
    cat('No odd characters')
  }
  return(odd_chr)
}


##### Pasting unique values when grouping by-------
# Helper for collapsing: paste the unique values of a column, splitting on ";"
# and dropping NAs and empty strings.
paste_unique <- function(x) {
  x <- unique(trimws(unlist(strsplit(as.character(x), ";"))))
  x <- x[!is.na(x) & x != "" & x != "NA"]
  if (length(x) == 0) NA_character_ else paste(x, collapse = ";")
}