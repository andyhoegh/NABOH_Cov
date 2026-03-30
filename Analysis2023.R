## Load Packages
library(tidyverse) 
library(knitr)
library(rstan)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)
library(loo)
library(scales)
library(rstanarm)
library(patchwork)
library(viridis)
set.seed(12172024)
########################################################################
## Load Data
########################################################################
load('NABOH_database.RData')
us <- map_data("state")

########################################################################
## Process 2023 Data
## Keep sites with multiple visits across distinct seasons
## Combine pooled and individual data
########################################################################
samples2023 <- samples |>
  filter(project_year == '2023') |>
  group_by(bat_id) |>
  slice(1) |>
  ungroup() |>
  count(site, date_sampled) |>
  mutate(date = ymd(date_sampled)) |>
  arrange(site, date) |>
  group_by(site) |>
  mutate(visit_number = 1:n()) |>
  ungroup()

# Criteria 3 distinct visits > 5 samples each visit
multivisit <- samples2023 |>
  filter(visit_number == 3) |>
  filter(!site %in% c("COLM",
                      'Waugh Bridge',
                      "Clifford Wash",
                      "Ritter Tank",
                      'Rose Cave'))


samples2023_repeats <- samples2023 |>
  filter(site %in% multivisit$site) |>
  mutate(visit = case_when(
    site == "22nd Street Bridge" & visit_number == 2 ~ 1,
    site == "22nd Street Bridge" & visit_number == 3 ~ 2,
    site == "22nd Street Bridge" & visit_number == 4 ~ 3,
    site == "Fort Tejon" & visit_number == 2 ~ 1,
    site == "Fort Tejon" & visit_number == 3 ~ 2,
    site == "Fort Tejon" & visit_number == 4 ~ 3,
    site == "Old_Town_Bridge" & visit_number == 4 ~ 3,
    site == "Putah Creek Bridge" & visit_number == 2 ~ 1,
    site == "Putah Creek Bridge" & visit_number == 3 ~ 2,
    site == "Putah Creek Bridge" & visit_number == 4 ~ 3,
    site == "Selman" & visit_number == 2 ~ 1,
    site == "Selman" & visit_number == 3 ~ 2,
    site == "Selman" & visit_number == 4 ~ 2,
    site == "Selman" & visit_number == 5 ~ 3,
    site == "Selman" & visit_number == 6 ~ 3,
    site == "Wimberly Bridge" & visit_number == 2 ~ 1,
    site == "Wimberly Bridge" & visit_number == 3 ~ 2,
    site == "Wimberly Bridge" & visit_number == 4 ~ 3,
    TRUE ~ visit_number
  ))

indiv2023 <- samples |> 
  filter(project_year == '2023') |>
  inner_join(indiv_tests, join_by(bat_id, sample_id)) |>
  group_by(species) |>
  summarize(n = n(),
            pos = sum(indiv_result == 'Positive')) |>
  rename(`# indiv` = n,
         `# pos indiv` = pos) |>
  left_join(samples |> 
              filter(project_year == '2023') |>
  group_by(species, site) |> 
  tally() |>
  tally(), by = join_by(species)) |>
  rename(`# of sites` = n) 

pool2023 <- samples |>
  filter(project_year == '2023') |>
  inner_join(pool_key, join_by(bat_id, sample_id)) |>
  inner_join(pool_tests, join_by(pool_id)) |>
  group_by(species) |>
  summarize(n = n()) |>
  rename(`# in pools` = n) |>
  left_join(samples |>
  filter(project_year == '2023') |>
  inner_join(pool_key, join_by(bat_id, sample_id)) |>
  inner_join(pool_tests, join_by(pool_id)) |>
  group_by(pool_id, species) |>
  slice(1) |>
  summarize(n = n(),
            pos = sum(pool_result == 'Positive'), .groups = 'drop') |>
  group_by(species) |>
  summarize(sum(n), sum(pos)), 
  join_by(species)) |>
  rename(`# pools` = `sum(n)`,
         `# pos pools` = `sum(pos)`)

samples_dates <- samples |> 
  filter(!date_sampled %in% c("N/A", "TBD", "2022")) |>
  mutate(date = ymd(date_sampled),
         month = month(date),
         year = year(date))

multi_dates <- samples_dates |>
  filter(year == 2023, species == 'TABR') |>
  # group_by(site, date_sampled) |>
  # tally() |> 
  # ungroup() |>
  count(site, date_sampled) |>
  count(site) |>
   filter(n > 2)

ten_2023 <- samples_dates |>
  filter(project_year == 2023,
         species == 'TABR') |>
  group_by(bat_id) |>
  slice(1) |>
  ungroup() |>
  count(site) |>
  filter(n > 10)

keep_sites <- inner_join(multi_dates |> select(site), 
                         ten_2023 |> select(site),
                         by = join_by(site))

samples2023 <- samples |>
  filter(project_year == '2023',
         species == 'TABR') |>
  inner_join(indiv_tests, by = join_by(sample_id, bat_id)) |>
  mutate(pos = as.numeric(indiv_result == 'Positive')) |>
  left_join(samples2023_repeats, by = join_by(site, date_sampled)) |>
  mutate(pool_size = 1,
         result_numeric = as.numeric(indiv_result == "Positive"))|>
  select(site, visit, pool_size, result_numeric, sample_type, reproductive_state, bat_mass, sample_id, sex) 

samples2023_pool <- samples |> 
  filter(project_year == '2023', species == 'TABR') |>
  inner_join(pool_key, join_by(bat_id, sample_id)) |>
  inner_join(pool_tests, join_by(pool_id)) |>
  left_join(samples2023_repeats |> select(-n), by = join_by(site, date_sampled)) |>
  filter(!sample_id %in% samples2023$sample_id) |>
 # select(site, pool_result, pool_id, visit, n, sample_type, reproductive_state, bat_mass, sex) |>
  group_by(site, pool_result, pool_id, visit, n, sample_type, reproductive_state, sex) |>
  summarize(bat_mass = mean(bat_mass, na.rm = T), .groups = 'drop') |>
  mutate(pool_size = n,
         result_numeric = as.numeric(pool_result == "Positive")) |>
  select(site, visit, pool_size, result_numeric, sample_type, reproductive_state, bat_mass, sex)
  
samples2023_stack <- samples2023 |> 
  select(-sample_id) |>
  bind_rows(samples2023_pool) |>
  filter(!is.na(visit),
         site %in% keep_sites$site) |>
  arrange(site) |>
  mutate(site_id = as.numeric(as.factor(site))) |>
  left_join(sites, by = join_by(site)) 

samples2023_stack <- samples2023_stack |> 
  filter(!is.na(bat_mass)) |>
  mutate(bat_mass_centered = (bat_mass - mean(bat_mass)) / (sd(bat_mass))) |>
  filter(reproductive_state %in% c('Lactating', 'Non reproductive', 'Post-Lactating', 'Pregnant'))


SARSCov2 <- samples |>
  filter(bat_id %in% c('BCI-23-TX-0487',
                       'BCI-23-TX-0513',
                       'BCI-23-TX-0532',
                       'BCI-23-TX-0569',
                       'BCI-23-TX-0467',
                       'BCI-23-TX-0469',
                       'BCI-23-TX-0470'
                       )) |>
  mutate(visit = 3) |> 
  group_by(bat_id) |>
  slice(1) |>
  ungroup()
    

########################################################################
## Summarize 2023 Data with Tables & Figures
########################################################################

indiv2023 |> 
  full_join(pool2023, join_by(species)) |> 
  mutate(`total samples` = `# indiv` + `# in pools`) |>
  select(species,`total samples`, `# indiv`, `# pos indiv`, `# in pools`, `# pools`, `# pos pools`, `# of sites`) |> 
  arrange(desc(`total samples`)) |>
  ungroup() |>
  slice(1:7) |>
  kable(caption = "@tbl-2023. Counts by Species for 2023 data")
  

samples_dates |>
  filter(species == 'TABR') |>
  group_by(bat_id) |>
  slice(1) |>
  ungroup() |>
  filter(year == 2023,
         site %in% keep_sites$site) |> # at least 10 TABR + 3 or more visits
  group_by(site, date) |>
  tally() |> ungroup() |>
  inner_join(samples2023_repeats |> select(-n), by = join_by(site, date)) |>
  ggplot(aes(x = date, y = n, color = factor(visit))) +
  geom_bar(stat = 'identity') + theme_bw() +
  scale_x_date(date_breaks = "1 month", date_labels =  "%b %Y") +
  theme(axis.text.x=element_text(angle=60, hjust=1),
        legend.position = 'none') +
  xlab('') + ylab('Number of bats captured') +
  ggtitle('Repeat Sampling Effort in 2023 for TABR') +
  facet_wrap(.~site, ncol = 3) +
  labs(caption = 'Data at sites with at least three visits (with TABR) and more than 10 total TABR')

num_sites <- max(samples2023_stack$site_id)
site_key <- samples2023_stack |>
  group_by(site, site_id) |>
  slice(1) |>
  ungroup() |>
  select(site, site_id) |>
  add_row(tibble(site = ' Overall Estimates', site_id = 0))

########################################################################
## Fit suite of models and compare model fit with loo
########################################################################

fit_2023 <- stan("visit.stan",
                    data = list(total_pools = nrow(samples2023_stack),
                                num_sites = num_sites,
                                y = samples2023_stack$result_numeric,
                                site_index = samples2023_stack$site_id,
                                pool_size = samples2023_stack$pool_size,
                                visit2_indicator = as.numeric(samples2023_stack$visit == 2),
                                visit3_indicator = as.numeric(samples2023_stack$visit ==3)),
                    iter = 5000, chains=4)


vg_2023 <- stan("visit_guano.stan",
                    data = list(total_pools = nrow(samples2023_stack),
                                num_sites = num_sites,
                                y = samples2023_stack$result_numeric,
                                site_index = samples2023_stack$site_id,
                                pool_size = samples2023_stack$pool_size,
                                visit2_indicator = as.numeric(samples2023_stack$visit == 2),
                                visit3_indicator = as.numeric(samples2023_stack$visit ==3),
                                guano_indicator = as.numeric(samples2023_stack$sample_type == 'Guano')),
                    iter = 5000, chains=4)

loo_vg <- loo(vg_2023)
loo_vg_out <- loo_vg$looic

vr_2023 <- stan("visit_repro.stan",
                    data = list(total_pools = nrow(samples2023_stack),
                                num_sites = num_sites,
                                y = samples2023_stack$result_numeric,
                                site_index = samples2023_stack$site_id,
                                pool_size = samples2023_stack$pool_size,
                                visit2_indicator = as.numeric(samples2023_stack$visit == 2),
                                visit3_indicator = as.numeric(samples2023_stack$visit ==3),
                                pregnant_indicator = as.numeric(samples2023_stack$reproductive_state == 'Pregnant'),
                                lactating_indicator = as.numeric(samples2023_stack$reproductive_state == 'Lactating'),
                                postlactating_indicator = as.numeric(samples2023_stack$reproductive_state == 'Post-Lactating')),
                    iter = 5000, chains=4)

loo_vr <- loo(vr_2023)
loo_vr_out <- loo_vr$looic

vbm_2023 <- stan("visit_bm.stan",
                    data = list(total_pools = nrow(samples2023_stack),
                                num_sites = num_sites,
                                y = samples2023_stack$result_numeric,
                                site_index = samples2023_stack$site_id,
                                pool_size = samples2023_stack$pool_size,
                                visit2_indicator = as.numeric(samples2023_stack$visit == 2),
                                visit3_indicator = as.numeric(samples2023_stack$visit ==3),
                                bodymass = samples2023_stack$bat_mass_centered),
                    iter = 5000, chains=4)

loo_vbm <- loo(vbm_2023)
loo_vbm_out <- loo_vbm$looic


vs_2023 <- stan("visit_sex.stan",
                    data = list(total_pools = nrow(samples2023_stack),
                                num_sites = num_sites,
                                y = samples2023_stack$result_numeric,
                                site_index = samples2023_stack$site_id,
                                pool_size = samples2023_stack$pool_size,
                                visit2_indicator = as.numeric(samples2023_stack$visit == 2),
                                visit3_indicator = as.numeric(samples2023_stack$visit ==3),
                                male_indicator = as.numeric(samples2023_stack$sex == 'Male')),
                    iter = 5000, chains=4)

loo_vs <- loo(vs_2023)
loo_vs_out <- loo_vs$looic


vrbmi_2023 <- stan("visit_bm_repro.stan",
                    data = list(total_pools = nrow(samples2023_stack),
                                num_sites = num_sites,
                                y = samples2023_stack$result_numeric,
                                site_index = samples2023_stack$site_id,
                                pool_size = samples2023_stack$pool_size,
                                visit2_indicator = as.numeric(samples2023_stack$visit == 2),
                                visit3_indicator = as.numeric(samples2023_stack$visit ==3),
                                bodymass = samples2023_stack$bat_mass_centered,
                                pregnant_indicator = as.numeric(samples2023_stack$reproductive_state == 'Pregnant'),
                                lactating_indicator = as.numeric(samples2023_stack$reproductive_state == 'Lactating'),
                                postlactating_indicator = as.numeric(samples2023_stack$reproductive_state == 'Post-Lactating')),
                    iter = 5000, chains=4)

loo_vrbmi <- loo(vrbmi_2023)
loo_vrbmi_out <- loo_vrbmi$looic

vrbmit_2023 <- stan("visit_bm_repro_type.stan",
                    data = list(total_pools = nrow(samples2023_stack),
                                num_sites = num_sites,
                                y = samples2023_stack$result_numeric,
                                site_index = samples2023_stack$site_id,
                                pool_size = samples2023_stack$pool_size,
                                visit2_indicator = as.numeric(samples2023_stack$visit == 2),
                                visit3_indicator = as.numeric(samples2023_stack$visit ==3),
                                bodymass = samples2023_stack$bat_mass_centered,
                                pregnant_indicator = as.numeric(samples2023_stack$reproductive_state == 'Pregnant'),
                                lactating_indicator = as.numeric(samples2023_stack$reproductive_state == 'Lactating'),
                                postlactating_indicator = as.numeric(samples2023_stack$reproductive_state == 'Post-Lactating'),
                                guano_indicator = as.numeric(samples2023_stack$sample_type == 'Guano')),
                    iter = 5000, chains=4)

loo_vrbmit <- loo(vrbmit_2023)
loo_vrbmit_out <- loo_vrbmit$looic

vrbmis_2023 <- stan("visit_bm_repro_type.stan",
                    data = list(total_pools = nrow(samples2023_stack),
                                num_sites = num_sites,
                                y = samples2023_stack$result_numeric,
                                site_index = samples2023_stack$site_id,
                                pool_size = samples2023_stack$pool_size,
                                visit2_indicator = as.numeric(samples2023_stack$visit == 2),
                                visit3_indicator = as.numeric(samples2023_stack$visit ==3),
                                bodymass = samples2023_stack$bat_mass_centered,
                                pregnant_indicator = as.numeric(samples2023_stack$reproductive_state == 'Pregnant'),
                                lactating_indicator = as.numeric(samples2023_stack$reproductive_state == 'Lactating'),
                                postlactating_indicator = as.numeric(samples2023_stack$reproductive_state == 'Post-Lactating'),
                                guano_indicator = as.numeric(samples2023_stack$sex == 'Male')),
                    iter = 5000, chains=4)

loo_vrbmis <- loo(vrbmis_2023)
loo_vrbmis_out <- loo_vrbmis$looic


vrbmi_interact_2023 <- stan("visit_bm_repro_interact.stan",
                    data = list(total_pools = nrow(samples2023_stack),
                                num_sites = num_sites,
                                y = samples2023_stack$result_numeric,
                                site_index = samples2023_stack$site_id,
                                pool_size = samples2023_stack$pool_size,
                                visit2_indicator = as.numeric(samples2023_stack$visit == 2),
                                visit3_indicator = as.numeric(samples2023_stack$visit ==3),
                                bodymass = samples2023_stack$bat_mass_centered,
                                pregnant_indicator = as.numeric(samples2023_stack$reproductive_state == 'Pregnant'),
                                lactating_indicator = as.numeric(samples2023_stack$reproductive_state == 'Lactating'),
                                postlactating_indicator = as.numeric(samples2023_stack$reproductive_state == 'Post-Lactating'),
                                pregnant_interact = as.numeric(samples2023_stack$reproductive_state == 'Pregnant') * samples2023_stack$bat_mass_centered,
                                lactating_interact = as.numeric(samples2023_stack$reproductive_state == 'Lactating') * samples2023_stack$bat_mass_centered,
                                postlactating_interact = as.numeric(samples2023_stack$reproductive_state == 'Post-Lactating') * samples2023_stack$bat_mass_centered),
                    iter = 5000, chains=4)

loo_vrbmi_interact <- loo(vrbmi_interact_2023)
loo_vrbmi_interact_out <- loo_vrbmi_interact$looic


tibble(model = c('visit', 'visit + sample type', 'visit + reproductive status', 'visit + bodymass', 'visit + sex', 'visit + reproductive status + bodymass', 'visit + reproductive status + bodymass + sample type', 'visit + reproductive status + bodymass + sex', 'visit + reproductive status x bodymass'),
       looic = c(loo_visit_out, loo_vg_out, loo_vr_out, loo_vbm_out, loo_vs_out, loo_vrbmi_out, loo_vrbmit_out,
                 loo_vrbmis_out, loo_vrbmi_interact_out)) |>
  arrange(looic) |>
  kable()


########################################################################
## Summarize Credible Intervals for Best Model
########################################################################

beta0s <- rstan::extract(vrbmi_2023, 'beta0')$beta0
mu <- rstan::extract(vrbmi_2023, 'mu')$mu
tau2 <- rstan::extract(vrbmi_2023, 'tau2')$tau2
tau2_matrix <- matrix(tau2, ncol = num_sites, nrow = length(tau2))
tau3 <- rstan::extract(vrbmi_2023, 'tau3')$tau3
tau3_matrix <- matrix(tau3, ncol = num_sites, nrow = length(tau3))
beta_bodymass <- rstan::extract(vrbmi_2023, 'beta_bodymass')$beta_bodymass
gamma_pregnant <- rstan::extract(vrbmi_2023, 'gamma_pregnant')$gamma_pregnant
gamma_lactating <- rstan::extract(vrbmi_2023, 'gamma_lactating')$gamma_lactating
gamma_postlactating <- rstan::extract(vrbmi_2023, 'gamma_postlactating')$gamma_postlactating

tibble(mean = c(mean(mu), mean(tau2), mean(tau3), mean(beta_bodymass),
                mean(gamma_pregnant), mean(gamma_lactating), mean(gamma_postlactating)),
       type = c('mu', 'tau_maternity', 'tau_late-maternity', 'beta_bodymass',
                'gamma_pregnant', 'gamma_lactating', 'gamma_postlactating'),
       lower = c(quantile(mu, probs =.025),
                 quantile(tau2, probs = .025),
                 quantile(tau3, probs = .025),
                 quantile(beta_bodymass, probs = .025),
                 quantile(gamma_pregnant, probs = .025),
                 quantile(gamma_lactating, probs = .025),
                 quantile(gamma_postlactating, probs = .025)),
       upper = c(quantile(mu, probs =.975),
                 quantile(tau2, probs = .975),
                 quantile(tau3, probs = .975),
                 quantile(beta_bodymass, probs = .975),
                 quantile(gamma_pregnant, probs = .975),
                 quantile(gamma_lactating, probs = .975),
                 quantile(gamma_postlactating, probs = .975))) |>
  ggplot(aes(x = mean, y = type)) +
  geom_point() +
  geom_segment(inherit.aes = F, aes(x = lower, xend = upper, y = type, yend = type)) +
  theme_bw() +
  ylab('') +
#  labs(title = '') +
  xlab('95% credible intervals for model parameters') 
  
########################################################################
## Site Level Prevalance
########################################################################

set.seed(12162024)

beta0s <- rstan::extract(vrbmi_2023, 'beta0')$beta0
mu <- rstan::extract(vrbmi_2023, 'mu')$mu
tau2 <- rstan::extract(vrbmi_2023, 'tau2')$tau2
tau2_matrix <- matrix(tau2, ncol = num_sites, nrow = length(tau2))
tau3 <- rstan::extract(vrbmi_2023, 'tau3')$tau3
tau3_matrix <- matrix(tau3, ncol = num_sites, nrow = length(tau3))
pregnant <- rstan::extract(vrbmi_2023, 'gamma_pregnant')$gamma_pregnant
pregnant_matrix <- matrix(pregnant, ncol = num_sites, nrow = length(pregnant))
lactating <- rstan::extract(vrbmi_2023, 'gamma_lactating')$gamma_lactating
lactating_matrix <- matrix(lactating, ncol = num_sites, nrow = length(lactating))
post_lactating <- rstan::extract(vrbmi_2023, 'gamma_postlactating')$gamma_postlactating
post_lactating_matrix <- matrix(post_lactating, ncol = num_sites, nrow = length(post_lactating))

site_vals <- tibble(mean = c(invlogit(colMeans(beta0s)),
                             invlogit(mean(mu)),
                             invlogit(colMeans(beta0s + tau2_matrix)),
                             invlogit(mean(mu + tau2)),
                             invlogit(colMeans(beta0s + tau3_matrix)),
                             invlogit(mean(mu + tau3)),
                             
                             invlogit(colMeans(beta0s + pregnant_matrix)),
                             invlogit(mean(mu + pregnant)),
                             invlogit(colMeans(beta0s + tau2_matrix + pregnant_matrix)),
                             invlogit(mean(mu + tau2 + pregnant)),
                             invlogit(colMeans(beta0s + tau3_matrix + pregnant_matrix)),
                             invlogit(mean(mu + tau3 + pregnant)),
                             
                             invlogit(colMeans(beta0s + lactating_matrix)),
                             invlogit(mean(mu + lactating)),
                             invlogit(colMeans(beta0s + tau2_matrix + lactating_matrix)),
                             invlogit(mean(mu + tau2 + lactating)),
                             invlogit(colMeans(beta0s + tau3_matrix + lactating_matrix)),
                             invlogit(mean(mu + tau3 + lactating)),
                             
                             invlogit(colMeans(beta0s + post_lactating_matrix)),
                             invlogit(mean(mu + post_lactating)),
                             invlogit(colMeans(beta0s + tau2_matrix + post_lactating_matrix)),
                             invlogit(mean(mu + tau2 + post_lactating)),
                             invlogit(colMeans(beta0s + tau3_matrix + post_lactating_matrix)),
                             invlogit(mean(mu + tau3 + post_lactating))),
                    
                    upper = c(invlogit(apply(beta0s, 2, quantile, probs = .975)),
                              invlogit(quantile(mu, probs = .975)),
                              invlogit(apply(beta0s + tau2_matrix, 2, quantile, probs = .975)),
                              invlogit(quantile(mu + tau2, probs = .975)),
                              invlogit(apply(beta0s + tau3_matrix, 2, quantile, probs = .975)),
                              invlogit(quantile(mu + tau3, probs = .975)),
                              
                              invlogit(apply(beta0s + pregnant_matrix, 2, quantile, probs = .975)),
                              invlogit(quantile(mu + pregnant, probs = .975)),
                              invlogit(apply(beta0s + tau2_matrix + pregnant_matrix, 2, quantile, probs = .975)),
                              invlogit(quantile(mu + tau2 + pregnant, probs = .975)),
                              invlogit(apply(beta0s + tau3_matrix + pregnant_matrix, 2, quantile, probs = .975)),
                              invlogit(quantile(mu + tau3 + pregnant, probs = .975)),
                              
                              invlogit(apply(beta0s + lactating_matrix, 2, quantile, probs = .975)),
                              invlogit(quantile(mu + lactating, probs = .975)),
                              invlogit(apply(beta0s + tau2_matrix + lactating_matrix, 2, quantile, probs = .975)),
                              invlogit(quantile(mu + tau2 + lactating, probs = .975)),
                              invlogit(apply(beta0s + tau3_matrix + lactating_matrix, 2, quantile, probs = .975)),
                              invlogit(quantile(mu + tau3 + lactating, probs = .975)),
                              
                              invlogit(apply(beta0s + post_lactating_matrix, 2, quantile, probs = .975)),
                              invlogit(quantile(mu + post_lactating, probs = .975)),
                              invlogit(apply(beta0s + tau2_matrix + post_lactating_matrix, 2, quantile, probs = .975)),
                              invlogit(quantile(mu + tau2 + post_lactating, probs = .975)),
                              invlogit(apply(beta0s + tau3_matrix + post_lactating_matrix, 2, quantile, probs = .975)),
                              invlogit(quantile(mu + tau3 + post_lactating, probs = .975))),
                    
                    lower = c(invlogit(apply(beta0s, 2, quantile, probs = .025)),
                              invlogit(quantile(mu, probs = .025)),
                              invlogit(apply(beta0s + tau2_matrix, 2, quantile, probs = .025)),
                              invlogit(quantile(mu + tau2, probs = .025)),
                              invlogit(apply(beta0s + tau3_matrix, 2, quantile, probs = .025)),
                              invlogit(quantile(mu + tau3, probs = .025)),
                              
                              invlogit(apply(beta0s + pregnant_matrix, 2, quantile, probs = .025)),
                              invlogit(quantile(mu + pregnant, probs = .025)),
                              invlogit(apply(beta0s + tau2_matrix + pregnant_matrix, 2, quantile, probs = .025)),
                              invlogit(quantile(mu + tau2 + pregnant, probs = .025)),
                              invlogit(apply(beta0s + tau3_matrix + pregnant_matrix, 2, quantile, probs = .025)),
                              invlogit(quantile(mu + tau3 + pregnant, probs = .025)),
                              
                              invlogit(apply(beta0s + lactating_matrix, 2, quantile, probs = .025)),
                              invlogit(quantile(mu + lactating, probs = .025)),
                              invlogit(apply(beta0s + tau2_matrix + lactating_matrix, 2, quantile, probs = .025)),
                              invlogit(quantile(mu + tau2 + lactating, probs = .025)),
                              invlogit(apply(beta0s + tau3_matrix + lactating_matrix, 2, quantile, probs = .025)),
                              invlogit(quantile(mu + tau3 + lactating, probs = .025)),
                              
                              invlogit(apply(beta0s + post_lactating_matrix, 2, quantile, probs = .025)),
                              invlogit(quantile(mu + post_lactating, probs = .025)),
                              invlogit(apply(beta0s + tau2_matrix + post_lactating_matrix, 2, quantile, probs = .025)),
                              invlogit(quantile(mu + tau2 + post_lactating, probs = .025)),
                              invlogit(apply(beta0s + tau3_matrix + post_lactating_matrix, 2, quantile, probs = .025)),
                              invlogit(quantile(mu + tau3 + post_lactating, probs = .025))),
                    site_id = rep(rep(c(1:num_sites, 0), 3), 4),
                    visit = rep(rep(c(1, 2, 3), each = num_sites + 1), 4),
                    reproductive_state = rep(c('Non reproductive', 'Pregnant', 'Lactating', 'Post-Lactating'),each = 3 * (num_sites + 1))) |> 
  mutate(visit_type = case_when(
         visit == 1 ~ 'Early Maternity',
         visit == 2 ~ 'Maternity',
         visit == 3 ~ 'Late Maternity')) 

data_combos <- samples2023_stack |> count(site, visit, reproductive_state)

site_vals_key <- tibble(site_id =c(0,1:17), 
                        site_char = c(' Overall','A', 'B', 'C','D', 'E', 
                                      'F', 'G', 'H', 'I', 'J',
                                      'K', 'L', 'M', 'N', 'O',
                                      'P', 'Q'))

site_vals |>
  inner_join(site_key, by = join_by(site_id)) |>
  inner_join(data_combos, by = join_by(visit, reproductive_state, site)) |>
  mutate(reproductive_state = factor(reproductive_state, 
                                     levels = c('Pregnant', 'Lactating', 'Post-Lactating', 'Non reproductive'))) |>
  left_join(site_vals_key, join_by(site_id)) |>
  ggplot(aes(y = site_char)) +
  facet_grid(~fct_relevel(reproductive_state,'Pregnant', 'Lactating', 'Post-Lactating', 'Non reproductive') ~ 
               ~fct_relevel(visit_type, 'Early Maternity', 'Maternity','Late Maternity')) +
  theme_bw() +
  theme(legend.position = 'bottom') +
  scale_x_continuous( breaks = c(0,.25, .5, .75, 1),
                      labels = c('0', '.25', '.50', '.75', '1'),
                       minor_breaks = NULL,
                      limits = c(NA, 1.1)) +
  geom_jitter(data = samples2023_stack |> 
                mutate(pool_size = factor(pool_size),
                       visit_type = case_when(
                         visit == 1 ~ 'Early Maternity',
                         visit == 2 ~ 'Maternity',
                         visit == 3 ~ 'Late Maternity')) |>
                left_join(site_vals_key, by = join_by(site_id)), 
              aes(x = result_numeric, y = site_char, shape = pool_size, color = factor(visit_type)),
              width = .1, height = .1, alpha = .4) + 
  geom_point(aes(x = mean, y = site_char), color = 'darkgrey') +
  geom_segment(aes(x = lower, xend = upper, yend = site_char), color = 'darkgrey') +
  geom_point(aes(x = mean, y = site_char), color = 'black', 
             data = site_vals |> 
               inner_join(site_key, by = join_by(site_id)) |> 
               filter(site_id == 0) |>
               filter(!(visit == 3 & reproductive_state == 'Pregnant')) |>
               filter(!(visit == 3 & reproductive_state == 'Lactating')) |>
               left_join(site_vals_key, by = join_by(site_id))) +
  geom_segment(aes(x = lower, xend = upper, yend = site_char), color = 'black',
               data = site_vals |> 
               inner_join(site_key, by = join_by(site_id)) |> 
                 filter(site_id == 0) |>
               filter(!(visit == 3 & reproductive_state == 'Pregnant')) |>
               filter(!(visit == 3 & reproductive_state == 'Lactating'))|>
               left_join(site_vals_key, by = join_by(site_id))) +
  guides(color = 'none' ) +
  xlab('Coronavirus Prevalence') + 
  labs(title = '') +
  geom_jitter(inherit.aes = F, data = SARSCov2 |>
                 mutate(visit_type = case_when(
                         visit == 1 ~ 'Early Maternity',
                         visit == 2 ~ 'Maternity',
                         visit == 3 ~ 'Late Maternity')) |>
                left_join(site_key, by = join_by(site)) |>
                left_join(site_vals_key, by = join_by(site_id)), 
              aes(x = 1, y= site_id), size = 4, shape = 'C', width = .1, height = .1, color = 'red') +
  ylab('Site')


########################################################################
## Body Mass
########################################################################

mass_resolution <- 20

mu <- rstan::extract(vrbmi_2023, 'mu')$mu
mu_matrix <- matrix(mu, ncol = mass_resolution, nrow = length(mu))

bat_mass_seq <- seq(min(samples2023_stack$bat_mass_centered), max(samples2023_stack$bat_mass_centered), length.out = mass_resolution)
bat_seq <- seq(min(samples2023_stack$bat_mass), max(samples2023_stack$bat_mass), length.out = mass_resolution)
bat_mass_matrix <- matrix(bat_mass_seq, ncol = mass_resolution, nrow = length(mu), byrow = T)

beta_bodymass <- rstan::extract(vrbmi_2023, 'beta_bodymass')$beta_bodymass
beta_bodymass_matrix <- matrix(beta_bodymass, ncol = mass_resolution, nrow= (length(beta_bodymass)))

bodymass_impacts <- mu_matrix + beta_bodymass_matrix * bat_mass_matrix

tau2 <- rstan::extract(vrbmi_2023, 'tau2')$tau2
tau2_matrix <- matrix(tau2, ncol = mass_resolution, nrow = length(tau2))
tau3 <- rstan::extract(vrbmi_2023, 'tau3')$tau3
tau3_matrix <- matrix(tau3, ncol = mass_resolution, nrow = length(tau3))

gamma_pregnant <- rstan::extract(vrbmi_2023, 'gamma_pregnant')$gamma_pregnant
pregnant_matrix <- matrix(gamma_pregnant, ncol = mass_resolution, nrow = length(gamma_pregnant))
gamma_lactating <- rstan::extract(vrbmi_2023, 'gamma_lactating')$gamma_lactating
lactating_matrix <- matrix(gamma_lactating, ncol = mass_resolution, nrow = length(gamma_lactating))
gamma_postlactating <- rstan::extract(vrbmi_2023, 'gamma_postlactating')$gamma_postlactating
postlactating_matrix <- matrix(gamma_postlactating, ncol = mass_resolution, nrow = length(gamma_postlactating))


bm_dat <- tibble(mean = c(invlogit(colMeans(bodymass_impacts)), 
                invlogit(colMeans(bodymass_impacts + tau2_matrix)),
                invlogit(colMeans(bodymass_impacts + tau3_matrix)),
                
                invlogit(colMeans(bodymass_impacts + pregnant_matrix)), 
                invlogit(colMeans(bodymass_impacts + tau2_matrix + pregnant_matrix)),
                invlogit(colMeans(bodymass_impacts + tau3_matrix + pregnant_matrix)),
                
                invlogit(colMeans(bodymass_impacts + lactating_matrix)), 
                invlogit(colMeans(bodymass_impacts + tau2_matrix + lactating_matrix)),
                invlogit(colMeans(bodymass_impacts + tau3_matrix + lactating_matrix)),
                
                invlogit(colMeans(bodymass_impacts + postlactating_matrix)), 
                invlogit(colMeans(bodymass_impacts + tau2_matrix + postlactating_matrix)),
                invlogit(colMeans(bodymass_impacts + tau3_matrix + postlactating_matrix))),
       
       lower = c(invlogit(apply(bodymass_impacts, 2, quantile, probs = .025)), 
                invlogit(apply(bodymass_impacts + tau2_matrix, 2, quantile, probs = .025)),
                invlogit(apply(bodymass_impacts + tau3_matrix, 2, quantile, probs = .025)),
                
                invlogit(apply(bodymass_impacts + pregnant_matrix, 2, quantile, probs = .025)), 
                invlogit(apply(bodymass_impacts + tau2_matrix + pregnant_matrix, 2, quantile, probs = .025)),
                invlogit(apply(bodymass_impacts + tau3_matrix + pregnant_matrix, 2, quantile, probs = .025)),
                
                invlogit(apply(bodymass_impacts + lactating_matrix, 2, quantile, probs = .025)), 
                invlogit(apply(bodymass_impacts + tau2_matrix + lactating_matrix, 2, quantile, probs = .025)),
                invlogit(apply(bodymass_impacts + tau3_matrix + lactating_matrix, 2, quantile, probs = .025)),
                
                invlogit(apply(bodymass_impacts + postlactating_matrix, 2, quantile, probs = .025)), 
                invlogit(apply(bodymass_impacts + tau2_matrix + postlactating_matrix, 2, quantile, probs = .025)),
                invlogit(apply(bodymass_impacts + tau3_matrix + postlactating_matrix, 2, quantile, probs = .025))),
       
       upper = c(invlogit(apply(bodymass_impacts, 2, quantile, probs = .975)), 
                invlogit(apply(bodymass_impacts + tau2_matrix, 2, quantile, probs = .975)),
                invlogit(apply(bodymass_impacts + tau3_matrix, 2, quantile, probs = .975)),
                
                invlogit(apply(bodymass_impacts + pregnant_matrix, 2, quantile, probs = .975)), 
                invlogit(apply(bodymass_impacts + tau2_matrix + pregnant_matrix, 2, quantile, probs = .975)),
                invlogit(apply(bodymass_impacts + tau3_matrix + pregnant_matrix, 2, quantile, probs = .975)),
                
                invlogit(apply(bodymass_impacts + lactating_matrix, 2, quantile, probs = .975)), 
                invlogit(apply(bodymass_impacts + tau2_matrix + lactating_matrix, 2, quantile, probs = .975)),
                invlogit(apply(bodymass_impacts + tau3_matrix + lactating_matrix, 2, quantile, probs = .975)),
                
                invlogit(apply(bodymass_impacts + postlactating_matrix, 2, quantile, probs = .975)), 
                invlogit(apply(bodymass_impacts + tau2_matrix + postlactating_matrix, 2, quantile, probs = .975)),
                invlogit(apply(bodymass_impacts + tau3_matrix + postlactating_matrix, 2, quantile, probs = .975))),
       seq = rep(bat_mass_seq, 12),
       weight = rep(bat_seq, 12),
       visit = rep(rep(c('1', '2', '3'), each = mass_resolution), 4),
       reproductive_state = rep(c('Non reproductive', 'Pregnant', 'Lactating', 'Post-Lactating'), each = mass_resolution * 3)) |> 
  mutate(visit_type = case_when(
         visit == 1 ~ 'Early Maternity',
         visit == 2 ~ 'Maternity',
         visit == 3 ~ 'Late Maternity')) 

bm_dat_red <- bm_dat |>
  filter(!(visit_type == 'Maternity' & reproductive_state == 'Pregnant' & 
             weight < samples2023_stack |> filter(reproductive_state == "Pregnant", visit == 2) |> summarize(min(bat_mass)) |> pull())) |>
  filter(!(visit_type == 'Maternity' & reproductive_state == 'Pregnant' & 
             weight > samples2023_stack |> filter(reproductive_state == "Pregnant", visit == 2) |> summarize(max(bat_mass)) |> pull())) |>
  filter(!(visit_type == 'Maternity' & reproductive_state == 'Lactating' & 
             weight < samples2023_stack |> filter(reproductive_state == "Lactating", visit == 2) |> summarize(min(bat_mass)) |> pull())) |>
  filter(!(visit_type == 'Maternity' & reproductive_state == 'Lactating' & 
             weight > samples2023_stack |> filter(reproductive_state == "Lactating", visit == 2) |> summarize(max(bat_mass)) |> pull())) |>
  filter(!(visit_type == 'Maternity' & reproductive_state == 'Post-Lactating' & 
             weight < samples2023_stack |> filter(reproductive_state == "Post-Lactating", visit == 2) |> summarize(min(bat_mass)) |> pull())) |>
  filter(!(visit_type == 'Maternity' & reproductive_state == 'Post-Lactating' & 
             weight > samples2023_stack |> filter(reproductive_state == "Post-Lactating", visit == 2) |> summarize(max(bat_mass)) |> pull())) |>
  filter(!(visit_type == 'Maternity' & reproductive_state == 'Non reproductive' & 
             weight < samples2023_stack |> filter(reproductive_state == "Non reproductive", visit == 2) |> summarize(min(bat_mass)) |> pull())) |>
  filter(!(visit_type == 'Maternity' & reproductive_state == 'Non reproductive' & 
             weight > samples2023_stack |> filter(reproductive_state == "Non reproductive", visit == 2) |> summarize(max(bat_mass)) |> pull())) |>
  filter(!(visit_type == 'Early Maternity' & reproductive_state == 'Pregnant' & 
             weight < samples2023_stack |> filter(reproductive_state == "Pregnant", visit == 1) |> summarize(min(bat_mass)) |> pull())) |>
  filter(!(visit_type == 'Early Maternity' & reproductive_state == 'Pregnant' & 
             weight > samples2023_stack |> filter(reproductive_state == "Pregnant", visit == 1) |> summarize(max(bat_mass)) |> pull())) |>
  filter(!(visit_type == 'Early Maternity' & reproductive_state == 'Lactating' & 
             weight < samples2023_stack |> filter(reproductive_state == "Lactating", visit == 1) |> summarize(min(bat_mass)) |> pull())) |>
  filter(!(visit_type == 'Early Maternity' & reproductive_state == 'Lactating' & 
             weight > samples2023_stack |> filter(reproductive_state == "Lactating", visit == 1) |> summarize(max(bat_mass)) |> pull())) |>
  filter(!(visit_type == 'Early Maternity' & reproductive_state == 'Post-Lactating' & 
             weight < samples2023_stack |> filter(reproductive_state == "Post-Lactating", visit == 1) |> summarize(min(bat_mass)) |> pull())) |>
  filter(!(visit_type == 'Early Maternity' & reproductive_state == 'Post-Lactating' & 
             weight > samples2023_stack |> filter(reproductive_state == "Post-Lactating", visit == 1) |> summarize(max(bat_mass)) |> pull())) |>
  filter(!(visit_type == 'Early Maternity' & reproductive_state == 'Non reproductive' & 
             weight < samples2023_stack |> filter(reproductive_state == "Non reproductive", visit == 1) |> summarize(min(bat_mass)) |> pull())) |>
  filter(!(visit_type == 'Early Maternity' & reproductive_state == 'Non reproductive' & 
             weight > samples2023_stack |> filter(reproductive_state == "Non reproductive", visit == 1) |> summarize(max(bat_mass)) |> pull())) |>
  filter(!(visit_type == 'Late Maternity' & reproductive_state == 'Post-Lactating' & 
             weight < samples2023_stack |> filter(reproductive_state == "Post-Lactating", visit == 3) |> summarize(min(bat_mass)) |> pull())) |>
  filter(!(visit_type == 'Late Maternity' & reproductive_state == 'Post-Lactating' & 
             weight > samples2023_stack |> filter(reproductive_state == "Post-Lactating", visit == 3) |> summarize(max(bat_mass)) |> pull())) |>
  filter(!(visit_type == 'Late Maternity' & reproductive_state == 'Non reproductive' & 
             weight < samples2023_stack |> filter(reproductive_state == "Non reproductive", visit == 3) |> summarize(min(bat_mass)) |> pull())) |>
  filter(!(visit_type == 'Late Maternity' & reproductive_state == 'Non reproductive' & 
             weight > samples2023_stack |> filter(reproductive_state == "Non reproductive", visit == 3) |> summarize(max(bat_mass)) |> pull()))

bm_dat_red |> 
  filter(!(reproductive_state == 'Pregnant' & visit == '3')) |>
  filter(!(reproductive_state == 'Lactating' & visit == '3')) |>
  ggplot() +
  geom_line(aes(y = lower, x = weight, color = visit_type), data = bm_dat_red|> 
    filter(!(reproductive_state == 'Pregnant' & visit == '3')) |>
    filter(!(reproductive_state == 'Lactating' & visit == '3')) , linetype = 3) +
  geom_line(aes(y = upper, x = weight, color = visit_type), data = bm_dat_red|> 
  filter(!(reproductive_state == 'Pregnant' & visit == '3')) |>
  filter(!(reproductive_state == 'Lactating' & visit == '3')) , linetype = 3) +
  geom_line(aes(y = mean, x = weight, color = visit_type)) + 
  theme_bw() + 
  ylim(-0.03,1.03) +
  facet_grid(fct_relevel(reproductive_state,'Pregnant', 'Lactating', 'Post-Lactating', 'Non reproductive')  ~ fct_relevel(visit_type, 'Early Maternity', 'Maternity', 'Late Maternity')) + 
  ylab('Coronavirus Prevalence') +
 # theme(legend.position = 'none') +
  geom_jitter(inherit.aes = F, 
             data = samples2023_stack |> 
               mutate(pool_size = factor(pool_size)) |> 
  mutate(visit_type = case_when(
         visit == 1 ~ 'Early Maternity',
         visit == 2 ~ 'Maternity',
         visit == 3 ~ 'Late Maternity')) , 
             aes(y = result_numeric, x = bat_mass, shape = pool_size),
             alpha = .4, width = 0, height = .02) +
  guides(color = 'none') +
  theme(legend.position = 'bottom') +
  xlab('Mass (grams)') +
#  labs(title = 'Estimated prevalence by visit, reproductive status, and mass') +
  geom_point(data = SARSCov2 |> 
  mutate(visit_type = case_when(
         visit == 1 ~ 'Early Maternity',
         visit == 2 ~ 'Maternity',
         visit == 3 ~ 'Late Maternity')) , inherit.aes = F,
             aes(x = bat_mass, y = 1), color ='red', shape = 'C', size = 4)