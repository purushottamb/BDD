#Libraries
library(sf)
library(raster)
library(ncdf4)
library(here)
library(tidyverse)
library(splitstackshape)
library(plm)
library(pdynmc)
library(dynpanel)
library(modelsummary)
library(knitr)



# Climate Data Extraction
split_data <- function(file) {
  # Read the CSV file and skip the first three rows
  data <- read.csv(file, skip = 3, header = TRUE)
  # Convert the data frame to a matrix
  data <- as.matrix(data)
  # Split the matrix by multiple dots
  data <- do.call(rbind, strsplit(data, "\\.+", fixed = TRUE))
  # Convert the matrix back to a data frame
  data <- as.data.frame(data)
  # Convert the variable to a character vector
  var <- as.character(data$V1)
  # Split the vector by whitespace
  var <- strsplit(var, "    ")
  # Unlist the vector to get a matrix of values
  var <- do.call(rbind, var)
  # Convert the matrix to a data frame
  var <- as.data.frame(var)
  # Set the column names
  colnames(var) <- c("YEAR", "JAN", "FEB", "MAR", "APR", "MAY", "JUN", "JUL", 
                     "AUG", "SEP", "OCT", "NOV", "DEC", "MAM", "JJA", "SON", 
                     "DJF", "ANN")
  # Return the data frame
  return(var)
}

# Apply the function to the CSV files for each country
cld_alg <- split_data("cld_algeria.csv")
cld_cmr <- split_data("cld_camaroon.csv")
cld_ken <- split_data("cld_kenya.csv")
cld_rsa <- split_data("cld_south_africa.csv")
cld_nig <- split_data("cld_nigeria.csv")
pre_alg <- split_data("pre_algeria.csv")
pre_cmr <- read.csv("pre_camaroon.csv")
pre_ken <- read.csv("pre_kenya.csv")
pre_nig <- read.csv("pre_nigeria.csv")
pre_rsa <- read.csv("pre_south_africa.csv")
tmp_alg <- split_data("tmp_algeria.csv")
tmp_cmr <- split_data("tmp_camaroon.csv")
tmp_ken <- split_data("tmp_kenya.csv")
tmp_nig <- split_data("tmp_nigeria.csv")
tmp_rsa <- split_data("tmp_south_africa.csv")
vap_alg <- split_data("vap_algeria.csv")
vap_cmr <- split_data("vap_camaroon.csv")
vap_ken <- split_data("vap_kenya.csv")
vap_nig <- split_data("vap_nigeria.csv")
vap_rsa <- split_data("vap_south_africa.csv")
wet_alg <- split_data("wet_algeria.csv")
wet_cmr <- read.csv("wet_camaroon.csv")
wet_ken <- split_data("wet_kenya.csv")
wet_nig <- split_data("wet_nigeria.csv")
wet_rsa <- split_data("wet_south_africa.csv")



# Function to apply filter and add Country column to data frame
apply_filter <- function(df, country) {
  country_name <- switch(country,
                         "alg" = "Algeria",
                         "cmr" = "Cameroon",
                         "ken" = "Kenya",
                         "nig" = "Nigeria",
                         "rsa" = "South Africa")
  
  df <- df %>%
    mutate_if(is.character, as.numeric) %>%
    select(YEAR, ANN) %>%
    filter(YEAR >= 2011 & YEAR <= 2021) %>%
    mutate(Country = country_name)
  
  return(df)
}

# List of data frames
data_frames <- list(
  cld_alg, cld_cmr, cld_ken, cld_rsa, cld_nig,
  pre_alg, pre_cmr, pre_ken, pre_nig, pre_rsa,
  tmp_alg, tmp_cmr, tmp_ken, tmp_nig, tmp_rsa,
  vap_alg, vap_cmr, vap_ken, vap_nig, vap_rsa,
  wet_alg, wet_cmr, wet_ken, wet_nig, wet_rsa
)

# Assigning country codes
countries <- c("alg", "cmr", "ken", "nig", "rsa")

# Applying filter function and adding Country column to each data frame
for (i in seq_along(data_frames)) {
  country_code <- substr(names[i], nchar(names[i]) - 2, nchar(names[i]))
  data_frames[[i]] <- apply_filter(data_frames[[i]], country_code)
}

# Update the original data frames with the filtered data
names <- c(
  "cld_alg", "cld_cmr", "cld_ken", "cld_rsa", "cld_nig",
  "pre_alg", "pre_cmr", "pre_ken", "pre_nig", "pre_rsa",
  "tmp_alg", "tmp_cmr", "tmp_ken", "tmp_nig", "tmp_rsa",
  "vap_alg", "vap_cmr", "vap_ken", "vap_nig", "vap_rsa",
  "wet_alg", "wet_cmr", "wet_ken", "wet_nig", "wet_rsa"
)

for (i in seq_along(data_frames)) {
  assign(names[i], data_frames[[i]])
}


cli.pre <- rbind(pre_cmr, pre_alg, pre_ken, pre_nig, pre_rsa)
cli.cld <- rbind(cld_cmr, cld_alg, cld_ken, cld_nig, cld_rsa)
cli.wet <- rbind(wet_cmr, wet_alg, wet_ken, wet_nig, wet_rsa)
cli.tmp <- rbind(tmp_cmr, tmp_alg, tmp_ken, tmp_nig, tmp_rsa)
cli.vap <- rbind(vap_cmr, vap_alg, vap_ken, vap_nig, vap_rsa)

## Read main data and merge the climate data to the

climate <- read.csv("project_data.csv")

climate <- climate %>%
  mutate(cl.pre = cli.pre$ANN,
         cl.tmp = cli.tmp$ANN,
         cl.cld = cli.cld$ANN,
         cl.vap = cli.vap$ANN,
         cl.wet = cli.wet$ANN)
# write.csv(climate, "climate.csv")



# Extracting dependent and independent variables
dependent_vars <- c("st.bankZ", "st.npl", "st.cap_asset", "st.credit_deposit", "st.reg_capital", 
                    "st.liq_asset", "st.npl_prov")
for (var in dependent_vars) {
  climate[[var]] <- na.locf(climate[[var]], na.rm = FALSE)
  
}



independent_vars <- c("se.gdp", "se.unemployment", "se.inf_cpi", "se.agri", "se.industry", 
                      "se.service", "cl.pre", "cl.cld", "cl.tmp", "cl.vap", "cl.wet", 
                      "g.pol_stability", "g.gov_ef", "g.reg_quality")

for (var in independent_vars) {
  climate[[var]] <- na.locf(climate[[var]], na.rm = FALSE)
}



## Regression Model

## Prepare data set for panel model

#new dataset
climate_data = data.frame(climate)

summary_stat_finstab <- climate_data %>%
  select(st.bankZ, st.npl, st.cap_asset, st.credit_deposit, st.reg_capital, 
         st.liq_asset, st.npl_prov) %>%
  summary()

summary_stat_finstab
kable(summary_stat, caption = "Summary Statistics of Variables")

#Plot
ggplot(climate_data, aes(x = Year, y = st.bankZ, color = Country)) +
  geom_line() +
  geom_point() +
  labs(title = "Bank Z Score Over Time by Country",
       x = "Year",
       y = "Bank Z Score",
       color = "Country") +
  theme_minimal()


# Check if memory addresses are the same
tracemem(climate_data) == tracemem(climate)
# FALSE expected

# logarithm of select columns
climate_data$bankZ=log(climate_data$st.bankZ)
climate_data$pre=log(climate_data$cl.pre)
climate_data$cpi = log(climate_data$se.inf_cpi)
climate_data$ghg = log(climate_data$cl.ghg)


##Model BankZ
model_bankZ <- pdynmc(dat = climate_data, varname.i = "Country", varname.t = "Year",
                  use.mc.diff = TRUE, use.mc.lev = FALSE, use.mc.nonlin = FALSE, inst.stata = TRUE,
                  include.y = TRUE, varname.y = "bankZ", lagTerms.y = 1,
                  fur.con = TRUE, fur.con.diff = TRUE, fur.con.lev = FALSE,
                  varname.reg.fur = c("pre","cl.cld","cl.wet","cl.tmp", "cl.vap",
                                      "ghg", "se.gdp", "cpi","se.agri", "se.service", 
                                      "se.unemployment", "se.industry", "g.pol_stability",
                                      "g.gov_ef","g.reg_quality"),
                  lagTerms.reg.fur = c(1,1,1,1,1,1,0,0,0,0,0,0,0,0,0),
                  include.dum = TRUE, dum.diff = TRUE, dum.lev = FALSE, varname.dum = "Year",
                  w.mat.stata = TRUE, std.err = "dbl.corrected", # Hwang et al. (2021)
                  estimation = "onestep", opt.meth = "none")

summary(model_bankZ)
jtest.fct(model_bankZ)

##Model NPL
model_npl <- pdynmc(dat = climate_data, varname.i = "Country", varname.t = "Year",
                      use.mc.diff = TRUE, use.mc.lev = FALSE, use.mc.nonlin = FALSE, inst.stata = TRUE,
                      include.y = TRUE, varname.y = "st.npl", lagTerms.y = 1,
                      fur.con = TRUE, fur.con.diff = TRUE, fur.con.lev = FALSE,
                      varname.reg.fur = c("pre","cl.cld","cl.wet","cl.tmp", "cl.vap",
                                          "ghg", "se.gdp", "cpi","se.agri", "se.service", 
                                          "se.unemployment", "se.industry", "g.pol_stability",
                                          "g.gov_ef","g.reg_quality"),
                      lagTerms.reg.fur = c(1,1,1,1,1,1,0,0,0,0,0,0,0,0,0),
                      include.dum = TRUE, dum.diff = TRUE, dum.lev = FALSE, varname.dum = "Year",
                      w.mat.stata = TRUE, std.err = "dbl.corrected", # Hwang et al. (2021)
                      estimation = "onestep", opt.meth = "none")

summary(model_npl)

##Model Capital Asset Ratio

model_cap_asset <- pdynmc(dat = climate_data, varname.i = "Country", varname.t = "Year",
                    use.mc.diff = TRUE, use.mc.lev = FALSE, use.mc.nonlin = FALSE, inst.stata = TRUE,
                    include.y = TRUE, varname.y = "st.cap_asset", lagTerms.y = 1,
                    fur.con = TRUE, fur.con.diff = TRUE, fur.con.lev = FALSE,
                    varname.reg.fur = c("pre","cl.cld","cl.wet","cl.tmp", "cl.vap",
                                        "ghg", "se.gdp", "cpi","se.agri", "se.service", 
                                        "se.unemployment", "se.industry", "g.pol_stability",
                                        "g.gov_ef","g.reg_quality"),
                    lagTerms.reg.fur = c(1,1,1,1,1,1,1,0,0,0,0,0,0,0,0),
                    include.dum = TRUE, dum.diff = TRUE, dum.lev = FALSE, varname.dum = "Year",
                    w.mat.stata = TRUE, std.err = "dbl.corrected", # Hwang et al. (2021)
                    estimation = "onestep", opt.meth = "none")

summary(model_cap_asset)
 

##Model Credit Deposit Ratio
model_credit_deposit <- pdynmc(dat = climate_data, varname.i = "Country", varname.t = "Year",
                          use.mc.diff = TRUE, use.mc.lev = FALSE, use.mc.nonlin = FALSE, inst.stata = TRUE,
                          include.y = TRUE, varname.y = "st.credit_deposit", lagTerms.y = 1,
                          fur.con = TRUE, fur.con.diff = TRUE, fur.con.lev = FALSE,
                          varname.reg.fur = c("pre","cl.cld","cl.wet","cl.tmp", "cl.vap",
                                              "ghg", "se.gdp", "cpi","se.agri", "se.service", 
                                              "se.unemployment", "se.industry", "g.pol_stability",
                                              "g.gov_ef","g.reg_quality"),
                          lagTerms.reg.fur = c(1,1,1,1,1,1,1,0,0,0,0,0,0,0,0),
                          include.dum = TRUE, dum.diff = TRUE, dum.lev = FALSE, varname.dum = "Year",
                          w.mat.stata = TRUE, std.err = "dbl.corrected", # Hwang et al. (2021)
                          estimation = "onestep", opt.meth = "none")

summary(model_credit_deposit)

##Model Regulatory Capital
model_reg_capital <- pdynmc(dat = climate_data, varname.i = "Country", varname.t = "Year",
                               use.mc.diff = TRUE, use.mc.lev = FALSE, use.mc.nonlin = FALSE, inst.stata = TRUE,
                               include.y = TRUE, varname.y = "st.reg_capital", lagTerms.y = 1,
                               fur.con = TRUE, fur.con.diff = TRUE, fur.con.lev = FALSE,
                               varname.reg.fur = c("pre","cl.cld","cl.wet","cl.tmp", "cl.vap",
                                                   "ghg", "se.gdp", "cpi","se.agri", "se.service", 
                                                   "se.unemployment", "se.industry", "g.pol_stability",
                                                   "g.gov_ef","g.reg_quality"),
                               lagTerms.reg.fur = c(1,1,1,1,1,1,1,0,0,0,0,0,0,0,0),
                               include.dum = TRUE, dum.diff = TRUE, dum.lev = FALSE, varname.dum = "Year",
                               w.mat.stata = TRUE, std.err = "dbl.corrected", # Hwang et al. (2021)
                               estimation = "onestep", opt.meth = "none")

summary(model_reg_capital)

##Model Liquid Assets
model_liq_asset <- pdynmc(dat = climate_data, varname.i = "Country", varname.t = "Year",
                            use.mc.diff = TRUE, use.mc.lev = FALSE, use.mc.nonlin = FALSE, inst.stata = TRUE,
                            include.y = TRUE, varname.y = "st.liq_asset", lagTerms.y = 1,
                            fur.con = TRUE, fur.con.diff = TRUE, fur.con.lev = FALSE,
                            varname.reg.fur = c("pre","cl.cld","cl.wet","cl.tmp", "cl.vap",
                                                "ghg", "se.gdp", "cpi","se.agri", "se.service", 
                                                "se.unemployment", "se.industry", "g.pol_stability",
                                                "g.gov_ef","g.reg_quality"),
                            lagTerms.reg.fur = c(1,1,1,1,1,1,1,0,0,0,0,0,0,0,0),
                            include.dum = TRUE, dum.diff = TRUE, dum.lev = FALSE, varname.dum = "Year",
                            w.mat.stata = TRUE, std.err = "dbl.corrected", # Hwang et al. (2021)
                            estimation = "onestep", opt.meth = "none")

summary(model_liq_asset)

##Model NPL_Provision
model_npl_prov <- pdynmc(dat = climate_data, varname.i = "Country", varname.t = "Year",
                          use.mc.diff = TRUE, use.mc.lev = FALSE, use.mc.nonlin = FALSE, inst.stata = TRUE,
                          include.y = TRUE, varname.y = "st.npl_prov", lagTerms.y = 1,
                          fur.con = TRUE, fur.con.diff = TRUE, fur.con.lev = FALSE,
                          varname.reg.fur = c("pre","cl.cld","cl.wet","cl.tmp", "cl.vap",
                                              "ghg", "se.gdp", "cpi","se.agri", "se.service", 
                                              "se.unemployment", "se.industry", "g.pol_stability",
                                              "g.gov_ef","g.reg_quality"),
                          lagTerms.reg.fur = c(1,1,1,1,1,1,1,0,0,0,0,0,0,0,0),
                          include.dum = TRUE, dum.diff = TRUE, dum.lev = FALSE, varname.dum = "Year",
                          w.mat.stata = TRUE, std.err = "dbl.corrected", # Hwang et al. (2021)
                          estimation = "onestep", opt.meth = "none")

summary(model_npl_prov)
