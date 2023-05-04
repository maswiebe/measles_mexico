library(fixest)
library(tidyverse)
library(tabulator)
library(ipumsr)

#-------------------------------------------------------------------------------

# set path
# User must uncomment the following line ("setwd...") and set the filepath equal to the folder containing this run.r file
# setwd('/path/to/measles_mexico/')

dict = c(
  measles_pc = '$\\text{M}_{\\text{pre}}$', inc_cpi = 'Income', loginc = 'Log income', empl = 'Employed', yrschool = 'Education', exposure = 'Exposure', marst = 'Married', urban_d = 'Urban', birthyr = 'Birthyear', bplmx = 'Birthplace', year = 'Survey year', loginc1970 = 'Log income (1970)', edu1970 = 'Education (1970)', emp1970 = 'Employment (1970)',
  note = dsb('*Note*: $\\text{M}_{pre}$ $\\times$ Exposure is the state-level pre-vaccine reported measles rate interacted with exposure to the vaccine. Standard errors (in parentheses) are clustered by state-of-birth interacted with year-of-birth. Survey weights are used. Significance levels: $\\text{* } p<0.1, \\text{** } p<0.05, \\text{*** } p<0.01$.'),
  note2 = dsb('*Note*: $\\text{M}_{pre}$ $\\times$ Exposure is the state-level pre-vaccine reported measles rate interacted with exposure to the vaccine. Standard errors (in parentheses) are clustered two-ways by state-of-birth and year-of-birth. Survey weights are used. Significance levels: $\\text{* } p<0.1, \\text{** } p<0.05, \\text{*** } p<0.01$.'),
  note3 = dsb('*Note*: $\\text{M}_{pre}$ $\\times$ Exposure is the state-level pre-vaccine reported measles rate interacted with exposure to the vaccine. Standard errors (in parentheses) are clustered by state-of-birth. Survey weights are used. Significance levels: $\\text{* } p<0.1, \\text{** } p<0.05, \\text{*** } p<0.01$.'),
  note4 = dsb('*Note*: $\\text{M}_{pre}$ is the state-level pre-vaccine reported measles rate. Robust standard errors. Significance levels: $\\text{* } p<0.1, \\text{** } p<0.05, \\text{*** } p<0.01$.'
  )
)
setFixest_dict(dict)

my_style = style.tex('aer', tpt=T, notes.tpt.intro = "\\footnotesize", signif.code=c("***"=0.01, "**"=0.05, "*"=0.10))
setFixest_etable(style.tex = my_style, page.width = 'fit', fitstat = ~ n + r2, view.cache=T, digits.stats='r2')

#-------------------------------------------------------------------------------
# disease data

# calculate 8-year average measles rate: 1965-1972
# reshape long
# replace NA with 0; raw data never has count=0, they use '—'
df_dis <- read_csv('data/mexico.csv') %>% 
  rowwise() %>% 
  mutate(avg_8yr_measles_rate = mean(c_across(starts_with('measlesrate_') & matches("_(196[5-9]|197[0-2])")))) %>% 
  ungroup() %>% # undo rowwise
  pivot_longer(cols = -c(state,sid,state_abbrev,avg_8yr_measles_rate),
               names_to = c('.value', 'year'),
               names_sep = '_'
  ) %>% 
  mutate_all(~ifelse(is.na(.), 0, .)) %>%   # ~ is for formula
  # this sets population=0 for 1965, 66
  mutate(population = ifelse(population==0, NA, population),
         year = as.numeric(year),
         median_measles = median(avg_8yr_measles_rate),
         high_measles = as.numeric(avg_8yr_measles_rate>median_measles)
  )

df_measles <- df_dis %>% 
  select(sid, avg_8yr_measles_rate, high_measles) %>% 
  mutate(measles_pc = avg_8yr_measles_rate/100000) %>% 
  group_by(sid, high_measles) %>% 
  summarize(avg_8yr_measles_rate = mean(avg_8yr_measles_rate),
            measles_pc = mean(measles_pc),
  )

# df_dis %>% group_by(state) %>% summarize(mpre = mean(avg_8yr_measles_rate)) %>% arrange(mpre) %>% View()
# compare to Table A1: matches perfectly (they round to integer values)

#-------------------------------------------------------------------------------
### measles trends by state
png(file="output/figures/measles_trend.png", res=300, width=6, height=6, units='in')
df_dis %>% ggplot(aes(year, measlesrate, group=sid)) +
  geom_line(color='forestgreen') +
  labs(x='', y='') +
  facet_wrap(~ state_abbrev, ncol=8, scales = 'fixed') +
  # scale_color_manual(values='green') +
  scale_x_continuous(limits = c(1965,1978), breaks = c(1973)) + 
  geom_vline(xintercept = 1973, linetype='dashed', color='red') +
  geom_hline(yintercept = 0) +
  theme(panel.background = element_blank(),
        strip.background = element_rect(fill='white')
  )
dev.off()  

#-------------------------------------------------------------------------------
### disease event study

png(file=paste0("output/figures/disease_es_measles.png"), res=72)
df_dis %>%
  feols(measlesrate ~ i(year, avg_8yr_measles_rate, ref=1972) | year + sid, cluster = ~sid) %>%
  iplot(xlab = '', main = '', ylab='', lab.fit='simple')
dev.off()

png(file=paste0("output/figures/disease_es_syphilis.png"), res=72)
df_dis %>%
  feols(syphilisrate ~ i(year, avg_8yr_measles_rate, ref=1972) | year + sid, cluster = ~sid) %>%
  iplot(xlab = '', main = '', ylab='', lab.fit='simple')
dev.off()

png(file=paste0("output/figures/disease_es_pertussis.png"), res=72)
df_dis %>%
  feols(pertussisrate ~ i(year, avg_8yr_measles_rate, ref=1972) | year + sid, cluster = ~sid) %>%
  iplot(xlab = '', main = '', ylab='', lab.fit='simple')
dev.off()

png(file=paste0("output/figures/disease_es_tetanus.png"), res=72)
df_dis %>%
  feols(tetanusrate ~ i(year, avg_8yr_measles_rate, ref=1972) | year + sid, cluster = ~sid) %>%
  iplot(xlab = '', main = '', ylab='', lab.fit='simple')
dev.off()

#-------------------------------------------------------------------------------
# real wages: adjust by cpi
# AP use Q42004 pesos
# they use surveys from 1994-2008; census 1995, 2000

df_cpi_raw <- read_csv('data/cpi_inegi.csv', skip=12, col_names = c('date', 'cpi')) %>% select(1:2)

years <- c(1970, 1990, 1995, 2000, 2010, 2015)
cpi_values <- map_dbl(years, ~ df_cpi_raw %>% filter(date==paste0('Dic ',.x)) %>% pull(cpi)) 
# cpi_values <- vector('numeric', length(years))
# for (i in 1:length(years)) {
#   cpi_values[i] <- df_cpi %>% filter(date==paste0('Dic ',years[i])) %>% pull(cpi)
# }
cpi_2004 <- df_cpi_raw %>% filter(date=='Dic 2004') %>% pull(cpi)
df_cpi <- tibble(year=years, cpi=cpi_values, cpi2004=cpi_2004) %>% 
  mutate(ratio = cpi2004/cpi)

#-------------------------------------------------------------------------------
### census data

# note: this code is from the IPUMS .R file
ddi <- read_ipums_ddi("data/ipumsi_00025.xml")
df <- read_ipums_micro(ddi)

gc()
# rename variables to lower case
colnames(df) <- tolower(colnames(df))
df <- df %>% 
  select(-country) %>%
  # filter males, age 18-65, and natives
  filter(
    between(age, 18,65) & sex==1 & nativity==1
  ) %>% 
  mutate(birthyr = year - age,
         # replace coded missings with NA; see IPUMS documentation
         incearn = ifelse(incearn>=99999998,NA,incearn),
         inctot = ifelse(inctot>=99999998,NA,inctot),
         yrschool = ifelse(yrschool>=91,NA,yrschool),
         empstat = ifelse(empstat==9,NA,empstat),
         marst = ifelse(marst==9,NA,marst),
         urban = ifelse(urban==9,NA,urban),
         lit = ifelse(lit==9,NA,lit),
         bplmx = ifelse(bplmx>=97,NA,bplmx),
         # outcome variables
         incearn = ifelse(year==1990,incearn/1000,incearn),
         # 1993: new peso equal to 1000 old pesos
         # https://international.ipums.org/international-action/variables/INCEARN#comparability_section
         empl = (empstat==1),
         lit_d = (lit==2),
         urban_d = (urban==2)
  ) %>% 
  left_join(df_measles, by= c('bplmx'='sid')) %>% 
  mutate(
    exposure = case_when(
      birthyr >= 1973 ~ 16,
      birthyr == 1972 ~ 15,
      birthyr == 1971 ~ 14,
      birthyr == 1970 ~ 13,
      birthyr == 1969 ~ 12,
      birthyr == 1968 ~ 11,
      birthyr == 1967 ~ 10,
      birthyr == 1966 ~ 9,
      birthyr == 1965 ~ 8,
      birthyr == 1964 ~ 7,
      birthyr == 1963 ~ 6,
      birthyr == 1962 ~ 5,
      birthyr == 1961 ~ 4,
      birthyr == 1960 ~ 3,
      birthyr == 1959 ~ 2,
      birthyr == 1958 ~ 1,
      birthyr <= 1957 ~ 0
    )
  ) %>% 
  left_join(df_cpi, by='year') %>% 
  mutate(inc_cpi = incearn*ratio,
         loginc = ifelse(inc_cpi>1, log(inc_cpi), NA), # have values <1
  )
gc()

# 1970 pretreatment control variables
df1970 <- df %>%
  filter(year==1970) %>% 
  mutate(loginc = ifelse(inctot>0,log(inctot),NA),
         sid = geo1_mx - 484000) %>% 
  group_by(sid) %>% # need state of residence here, not state of birth
  summarize(edu = mean(yrschool, na.rm=T),
            inc = mean(inctot, na.rm=T),
            loginc = mean(loginc, na.rm=T),
            emp = mean(empl, na.rm=T),
            lit = mean(lit_d, na.rm=T),
  ) %>% 
  left_join(df_measles, by='sid') %>% # don't average by 1970 state of residence
  rename(measles = avg_8yr_measles_rate)

df <- df %>% filter(year>1970) # drop 1970 obs
gc()

#-------------------------------------------------------------------------------
### raw data

# age histogram: spikes at multiples of 5
png(file=paste0("output/figures/age_hist.png"), 
    res=300, width=6, height=6, units='in')
df %>% ggplot(aes(age, weight=perwt)) + 
  geom_histogram(fill='steelblue', color='black', binwidth = 1) +
  labs(x='Age', y='')
dev.off()

# birthyear histogram
png(file=paste0("output/figures/birthyear_hist.png"), 
    res=300, width=6, height=6, units='in')
df %>% ggplot(aes(birthyr, weight=perwt)) + 
  geom_histogram(fill='steelblue', color='black', binwidth = 1) + 
  labs(x='Birth year', y='') +
  geom_vline(xintercept = 1973, color = 'red')
dev.off()

# average age by birthyear
png(file=paste0("output/figures/age_birthyear.png"), 
    res=300, width=6, height=6, units='in')
df %>% 
  group_by(birthyr) %>% 
  summarize(age = weighted.mean(age,w=perwt,na.rm=T)) %>% 
  ggplot(aes(birthyr,age)) + 
  geom_line() +
  labs(x='Birth year', y='') +
  geom_vline(xintercept = 1973, color = 'red')
dev.off()

### plot outcomes against birth year, separately by high and low measles

df_cohort <- df %>% 
  group_by(birthyr, high_measles) %>% 
  summarize(
    inc = weighted.mean(inc_cpi, w=perwt, na.rm=T),
    loginc = weighted.mean(loginc, w=perwt, na.rm=T),
    empl = weighted.mean(empl, w=perwt, na.rm=T),
    edu = weighted.mean(yrschool, w=perwt, na.rm=T)
  ) %>% 
  drop_na()

dep_vars <- c('loginc', 'empl', 'edu')
for (i in dep_vars) {
  png(file=paste0('output/figures/highlow_',i,'.png'), 
      res=300, width=6, height=6, units='in')
  
  df_cohort %>% 
    ggplot(aes(birthyr, !!sym(i), group=high_measles, color = factor(high_measles))) +
    geom_line() +
    geom_vline(xintercept = c(1957,1973), linetype='dashed', color='black') +
    labs(x='Year of birth', y='', color='') + # use color='' to omit legend title
    theme(legend.position = 'bottom') + 
    scale_color_discrete(labels = c('Low', 'High'))
  print(last_plot()) # need to print to png device
  dev.off()
  
}

#-------------------------------------------------------------------------------
### main results

gc()

rhs <- '~measles_pc:exposure | marst + urban_d + birthyr + bplmx + year'

est_loginc <- df %>% feols(as.formula(paste0('loginc',rhs)), cluster = ~bplmx^birthyr, weights = ~perwt)
est_emp <- df %>% feols(as.formula(paste0('empl',rhs)), cluster = ~bplmx^birthyr, weights = ~perwt)
est_edu <- df %>% feols(as.formula(paste0('yrschool',rhs)), cluster = ~bplmx^birthyr, weights = ~perwt)

etable(est_loginc, est_emp, est_edu,
       # view=T,
       notes = 'note',
       digits='s4',
       powerBelow=-9,
       replace=T,
       file = "output/tables/did_main.tex"
)

# similar results to AP, off by factor of 1000

# fitstat(est_loginc, type='my') # 7.91; AP 1.1
# fitstat(est_edu, type='my') # 7.68; AP 8.2
# fitstat(est_emp, type='my') # 0.81; AP 0.85

rm(list=c('est_loginc', 'est_emp', 'est_edu'))
gc()

#-------------------------------------------------------------------------------
### Event study and piecewise regression

# cleaning

dep_vars <- c('loginc', 'empl', 'yrschool')
rhs <- ' ~ i(birthyr, measles_pc, ref=1956) | marst + urban_d + birthyr + bplmx + year'
rhs_spline <- ' ~ l1 + l2 + l3 | marst + urban_d + birthyr + bplmx + year'

# 16-year windows: 1941-1957, 1957-1973, 1973-1989
df <- df %>% 
  mutate(
    kink1 = 1957,
    kink2 = 1973,
    l1 = birthyr*measles_pc,
    l2 = pmax(0, birthyr-kink1)*measles_pc, # should be max, not min; typo in Roodman
    l3 = pmax(0, birthyr-kink2)*measles_pc
  )
min_cohort <- min(df$birthyr)

kink1 <- 1957
kink2 <- 1973
# sample: 1925-1997, since min cohort=1925 is age 65 in 1990
birth_year <- c(1925:1955, 1957:1997) # omit 1956
birth_year1989 <- c(1941:1955, 1957:1989) # length 48, 1941-1989

spline_fun <- function(b0,x,splines) {
  b0 + coef(splines)[1]*(x-1960) + coef(splines)[2]*pmax(x-kink1,0) + coef(splines)[3]*pmax(x-kink2,0)
}

gc()

#----------------

# note: I'm close to hitting memory constraints on my laptop (16gb RAM)

# loop over dependent variables
for (i in dep_vars) {
  formula <- as.formula(paste(i, rhs))
  start <- proc.time()
  
  es <- df %>% feols(formula, cluster = ~bplmx^birthyr, weights = ~perwt)
  print(proc.time() - start)
  gc()
  
  png(file=paste0("output/figures/es_", i, ".png"), res=72)
  iplot(es, xlab = 'Birth year', main = '', ylab='')
  abline(v=c(1956.5,1973.5), col='red')
  dev.off()
  
  points <- coef(es) # 1925-1997, missing 1956
  rm(es)
  points1989 <- points[17:64] # restrict to 1941-1989; length 48, because missing 1956
  # start at 17 = 1941, end at 64 = 1989
  
  gc()
  
  formula_spline <- as.formula(paste(i, rhs_spline))
  spline_fit <- df %>%
    filter(between(birthyr,1941,1989)) %>% 
    feols(formula_spline, cluster = ~bplmx^birthyr, weights = ~perwt)
  
  gc()
  print(proc.time() - start)
  b0 <- mean(spline_fit$sumFE)
  y_vals <- spline_fun(b0,birth_year1989, spline_fit) 
  
  ## scaling spline fit and point estimates
  # take difference between average height of point estimates and the average height of the spline fit, and add it to the spline fit
  # average point estimates over 1941-1989
  shift <-  mean(points1989 - y_vals)
  
  # plot point estimates, overlay spline fit
  # missing omitted year
  png(file=paste0("output/figures/spline_", i, ".png"), res=72)
  plot(birth_year, points, xlab='Birth year', ylab='') # plot point estimates
  lines(birth_year1989, y_vals+shift, col='red') # overlay shifted splines
  abline(v=c(1957,1973), lty = 'dashed')
  mtext(paste0('p = ', sprintf('%.2f', round(spline_fit$coeftable[2,4],2)),', ', sprintf('%.2f', round(spline_fit$coeftable[3,4],2))), side=1, line=4, adj=0) # note p-values; two decimal places
  dev.off()
  
  rm(spline_fit)
  gc()
  
}

#-------------------------------------------------------------------------------
# Table 3: correlate measles_rate with 1970 state characteristics
# 1970 variables: empstat, inctot, yrschool, lit

est_inc <- df1970 %>% feols(inc ~ measles)
est_loginc <- df1970 %>% feols(loginc ~ measles)
est_lit <- df1970 %>% feols(lit ~ measles)
est_emp <- df1970 %>% feols(emp ~ measles)
est_edu <- df1970 %>% feols(edu ~ measles)

etable(est_inc, est_loginc, est_lit, est_emp, est_edu,
       dict = c('measles' = '$\\text{M}_{\\text{pre}}$', 'edu' = 'Education', 'inc' = 'Income', 'loginc' = 'Log income', 'emp' = 'Employment', 'lit'='Literacy' ),
       vcov = 'hetero',
       notes = 'note4',
       drop='(Intercept)',
       file = "output/tables/1970controls_corr.tex",
       replace=TRUE,
       powerBelow=-9,
       digits='s4'
)

# different values than their Table 3
# fitstat(est_inc, type='my') # 8998.9; AP= 5107
# fitstat(est_lit, type='my') # 0.78; AP = 0.72
# fitstat(est_emp, type='my') # 0.86; AP = 0.49

rm(list=c('est_inc', 'est_loginc' ,'est_edu', 'est_lit', 'est_emp'))
gc()

#-------------------------------------------------------------------------------
### controlling for state characteristics

df1970 <- df1970 %>% 
  rename(edu1970 = edu, inc1970 = inc, loginc1970 = loginc, emp1970 = emp, lit1970 = lit
  ) %>% 
  select(-measles, -measles_pc)
df <- df %>% 
  left_join(df1970, by=c('bplmx'='sid'))

rhs <- '~measles_pc:exposure + loginc1970:exposure + edu1970:exposure + emp1970:exposure | marst + urban_d + birthyr + bplmx + year'

est_loginc <- df %>% feols(as.formula(paste0('loginc',rhs)), cluster = ~bplmx^birthyr, weights = ~perwt)
est_emp <- df %>% feols(as.formula(paste0('empl',rhs)), cluster = ~bplmx^birthyr, weights = ~perwt)
est_edu <- df %>% feols(as.formula(paste0('yrschool',rhs)), cluster = ~bplmx^birthyr, weights = ~perwt)

etable(est_loginc, est_emp, est_edu,
       notes = 'note',
       digits='s4',
       powerBelow=-9,
       replace=T,
       file = "output/tables/did_controls.tex"
)

rm(list=c('est_loginc', 'est_emp', 'est_edu'))
gc()

#-------------------------------------------------------------------------------
### main results, clustering by birthplace

est_loginc <- df %>% feols(loginc ~measles_pc:exposure | marst + urban_d + birthyr + bplmx + year, cluster = ~bplmx, weights = ~perwt )
est_emp <- df %>% feols(empl ~measles_pc:exposure | marst + urban_d + birthyr + bplmx + year, cluster = ~bplmx, weights = ~perwt )
est_edu <- df %>% feols(yrschool ~measles_pc:exposure | marst + urban_d + birthyr + bplmx + year, cluster = ~bplmx, weights = ~perwt )

etable(est_loginc, est_emp, est_edu,
       notes = 'note3',
       digits='s4',
       powerBelow=-9,
       replace=T,
       file = "output/tables/bpl_cluster.tex"
)

rm(list=c('est_loginc', 'est_emp', 'est_edu'))
gc()

