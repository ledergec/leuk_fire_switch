

# This function computes the alpha diversity from raw data where species can occur multiple times in different layers
compute_alpha_diversity <- function(data) {
  aggregated <- data |> 
    group_by(year, global_id) |>
    summarise(
      a1 = ifelse(sum(!is.na(a1) > 0), length(unique(`Species_unified_2004-23`[!is.na(a1) & a1>0])), NA),
      a10 = ifelse(sum(!is.na(a10) > 0), length(unique(`Species_unified_2004-23`[!is.na(a10) & a10>0])), NA),
      b1 = ifelse(sum(!is.na(b1) > 0), length(unique(`Species_unified_2004-23`[!is.na(b1) & b1>0])), NA),
      b10 = ifelse(sum(!is.na(b10) > 0), length(unique(`Species_unified_2004-23`[!is.na(b10) & b10>0])), NA),
      c1 = ifelse(sum(!is.na(c1) > 0), length(unique(`Species_unified_2004-23`[!is.na(c1) & c1>0])), NA),
      c10 = ifelse(sum(!is.na(c10) > 0), length(unique(`Species_unified_2004-23`[!is.na(c10) & c10>0])), NA),
      d1 = ifelse(sum(!is.na(d1) > 0), length(unique(`Species_unified_2004-23`[!is.na(d1) & d1>0])), NA),
      d10 = ifelse(sum(!is.na(d10) > 0), length(unique(`Species_unified_2004-23`[!is.na(d10) & d10>0])), NA),
      total = length(unique(`Species_unified_2004-23`)),
      n = n(),
      elevation = mean(elevation_m)
    )
  
  aggregated$year <- factor(aggregated$year, levels = c("goedicke", "2004", "2005", "2006", "2007", "2013", "2023", "climax"))
  aggregated <- aggregated |> 
    group_by(year, global_id) |>
    summarise(
      a1 = mean(c(a1, b1, c1, d1), na.rm = T),
      a10 = mean(c(a10, b10, c10, d10), na.rm = T),
      total = total,
      elevation = elevation
    )
  return(aggregated)
}

# This function allows improving the assignment of significance letters. After 
# improvement the letters signifying different significance levels will appear 
# in alphabetic order and are also sorted alphabetically within each group.
improve_letters <- function(letters) {
  unique_letters_in_order_of_encounter <- unique(unlist(strsplit(letters, "")))
  alphabet <- c("a", "b", "c", "d", "e", "f", "g")
  result <- lapply(strsplit(letters, ""), function(list){
    indices_of_encounter <- unlist(lapply(list, function(car){
      which(unique_letters_in_order_of_encounter == car)
    }))
    indices_of_encounter <- indices_of_encounter[order(indices_of_encounter)]
    result <- paste(alphabet[indices_of_encounter], collapse = "")
    return(result)
  }) 
  return(unlist(result))
}

# Convert p_values to significance codes
convert_to_significance_code <- function(p_values) {
  return(symnum(p_values, corr = FALSE, legend = F, na = "??", cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1), symbols = c("***", " **", "  *", "  .", "   ")))
}

# convert_to_significance_code(c(0.00001, 0.1, 1, NA, 0.00000001))

# Convert p_values to significance codes
convert_to_pm <- function(estimates) {
  res <- symnum(estimates, corr = FALSE, legend = F, na = "??", cutpoints = c(-Inf, 0, Inf), symbols = c("-", "+"))
  res[estimates == 0] <- "0"
  return(res)
}


# convert_to_pm(c(-1e-10, 0, 1e-10))
