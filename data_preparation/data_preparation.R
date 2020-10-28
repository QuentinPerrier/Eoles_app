library(tidyverse)

LCOE <- read_csv("inputs/scenarios.csv") %>% 
  rename(offshore_cost = offshore,
         onshore_cost = onshore,
         PV_cost = pv,
         battery_cost = battery,
         methanation_cost = methanation)

sensitivity_results <- read_csv("inputs/sensitivity_results.csv") %>% 
  rename(offshore_cap = offshore,
         onshore_cap = onshore,
         PV_cap = PV,
         biogas_cap = biogas)

Eoles_outputs <- full_join(LCOE, 
                           sensitivity_results, 
                           by = "scen")
saveRDS(Eoles_outputs, file = "outputs/Eoles_outputs.rds")


Eoles_cost_inputs <- read_csv2("inputs/Costs.csv")
saveRDS(Eoles_cost_inputs, file = "outputs/Eoles_cost_inputs.rds")

LCOE <- read_csv2("inputs/LCOEs.csv", na = c("#DIV/0!"))
saveRDS(LCOE, file = "outputs/LCOE.rds")

cost_shares <- read_csv2("inputs/Cost_shares.csv") %>% 
  mutate(Production = Production *100, Storage = Storage * 100)
saveRDS(cost_shares, file = "outputs/cost_shares.rds")
