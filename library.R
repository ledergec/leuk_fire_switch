

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