
concentration <- function(raw) {
  
  # raw <- read_xlsx("./Background/test.xlsx", na = "NA") %>%
  #   select(`PHA code`, TERMINALCODE, Province, City, `Potential-2019(MAT201904)`,
  #          大区, `MAT201904销售指标（RMB）`, FTE)
  # colnames(raw) <- c("code1", "code2", "province", "city", "potential", "region", "mat_target", "fte")
  
  cum <- raw %>% 
    distinct() %>% 
    mutate(code = ifelse(is.na(code1),
                         code2,
                         code1)) %>% 
    group_by(code, province, city, region) %>% 
    summarise(potential = sum(potential, na.rm = TRUE),
              mat_target = sum(mat_target, na.rm = TRUE),
              fte = sum(fte, na.rm = TRUE)) %>% 
    ungroup() %>% 
    arrange(-potential) %>% 
    mutate(potential_cumsum = cumsum(potential),
           potential_cumctrb = potential_cumsum / sum(potential, na.rm = TRUE) * 100,
           roi = (mat_target - fte * 300000) / 300000,
           productivity = mat_target / fte,
           productivity = ifelse(is.na(productivity) | is.nan(productivity) | is.infinite(productivity),
                                 0,
                                 productivity))
  
  return(cum)
}
