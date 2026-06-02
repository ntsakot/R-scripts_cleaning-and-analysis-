#flu dashboard scripting
###ls()
loaded_objects <- load("C:/Users/NtsakoM/AppData/Local/Microsoft/Windows/INetCache/Content.Outlook/2ACXHUDQ/SRI ILI VW Flu Detection Rates 2026_19.rdata")
loaded_objects


# Sri flu
write.csv(sri_flu_dr_dfs$df_counts_type, "sri_counts_type.csv", row.names = FALSE)
write.csv(sri_flu_dr_dfs$df_counts_prov, "sri_counts_prov.csv", row.names = FALSE)
write.csv(sri_flu_dr_dfs$df_det_rate, "sri_det_rate.csv", row.names = FALSE)

# ILI flu
write.csv(ili_flu_dr_dfs$df_counts_type, "ili_counts_type.csv", row.names = FALSE)
write.csv(ili_flu_dr_dfs$df_counts_prov, "ili_counts_prov.csv", row.names = FALSE)
write.csv(ili_flu_dr_dfs$df_det_rate, "ili_det_rate.csv", row.names = FALSE)

# Wastewater flu
write.csv(vw_flu_dr_dfs$df_counts_type, "vw_counts_type.csv", row.names = FALSE)
write.csv(vw_flu_dr_dfs$df_counts_prov, "vw_counts_prov.csv", row.names = FALSE)
write.csv(vw_flu_dr_dfs$df_det_rate, "vw_det_rate.csv", row.names = FALSE)

#create tables
library(dplyr)

# Load RData file
load("C:/Users/NtsakoM/AppData/Local/Microsoft/Windows/INetCache/Content.Outlook/2ACXHUDQ/SRI ILI VW Flu Detection Rates 2026_20.rdata")

#===================================
# Counts by type
#===================================

counts_type <- bind_rows(
  sri = sri_flu_dr_dfs$df_counts_type,
  ili = ili_flu_dr_dfs$df_counts_type,
  vw  = vw_flu_dr_dfs$df_counts_type,
  .id = "source"
) %>%
  mutate(
    week = as.numeric(as.character(week)),
    FluResults = as.character(FluResults)
  )

View(counts_type)
#===================================
# Counts by province
#===================================

counts_prov <- bind_rows(
  sri = sri_flu_dr_dfs$df_counts_prov,
  ili = ili_flu_dr_dfs$df_counts_prov,
  vw  = vw_flu_dr_dfs$df_counts_prov,
  .id = "source"
) %>%
  mutate(
    week = as.numeric(as.character(week)),
    province = as.character(province)
  )

View(counts_prov)
#===================================
# Detection rates
#===================================

det_rate <- bind_rows(
  sri = sri_flu_dr_dfs$df_det_rate,
  ili = ili_flu_dr_dfs$df_det_rate,
  vw  = vw_flu_dr_dfs$df_det_rate,
  .id = "source"
) %>%
  mutate(
    week = as.numeric(as.character(week))
  )

View(det_rate)
#===================================
# Dimension tables
#===================================

dim_week <- data.frame(
  week = sort(unique(det_rate$week))
)

dim_source <- data.frame(
  source = c("sri","ili","vw")
)
