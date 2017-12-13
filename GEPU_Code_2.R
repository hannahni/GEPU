# GEPU Code
# Created 12/17

require(xlsx)
rm(list = ls())
setwd("/Users/HMN/Desktop/Davis/Global_Masters/r/r_new")

# -----------------
# Downloading files
# -----------------

dl_country_names <- c("US", "Australia", "Brazil", "Canada", "Chile", "China", 
                   "Europe", "India", "Ireland", "Japan", "Korea", "Mexico",
                   "Netherlands", "Russia", "Sweden")

dl_fun <- function(x) {
  dl_url <- paste("http://policyuncertainty.com/media/", x, "_Policy_Uncertainty_Data.xlsx", sep = "")
  download.file(dl_url, paste(x, ".xlsx", sep = ""))
}

invisible(lapply(dl_country_names, dl_fun))

# -------------------------------
# Reading and cleaning dataframes
# -------------------------------

# Function for reading excel files
read_excel <- function(filename, rows, cols, head = 0) {
  if (head == 0) {
    read.xlsx(filename, 1, rowIndex = rows, colIndex = cols)
  }
  else {
    head(read.xlsx(filename, 1, rowIndex = rows, colIndex = cols), head)
  }
}

# Australia
australia_df <- read_excel("Australia.xlsx", NULL, NULL, -1)
colnames(australia_df) <- c("Year", "Month", "Australia")

# Brazil
brazil_df <- read_excel("Brazil.xlsx", NULL, NULL, -1)
colnames(brazil_df) <- c("Year", "Month", "Brazil")

# Canada
canada_df <- read_excel("Canada.xlsx", NULL, NULL, -1)
colnames(canada_df)[3] <- "Canada"

# Chile
chile_df <- read_excel("Chile.xlsx", c(-1), c(2), -1)
chile_df <- cbind(c(rep(c(1993:2016), each = 12), rep(2017, nrow(chile_df)%%12)),
                  c(rep(c(1:12), length(c(1993:2016))), c(1:(nrow(chile_df)%%12))),
                  chile_df)
colnames(chile_df) <- c("Year", "Month", "Chile")

# China
china_df <- read_excel("China.xlsx", NULL, NULL, -1)
colnames(china_df) <- c("Year", "Month", "China")

# Europe
europe_df <- read_excel("Europe.xlsx", NULL, c(1,2,4:8), -1)
colnames(europe_df)[3:7] <- c("Germany", "Italy", "UK", "France", "Spain")

# India
india_df <- read_excel("India.xlsx", NULL, NULL, -1)
colnames(india_df)[3] <- "India"

# Ireland
ireland_df <- read_excel("Ireland.xlsx", NULL, c(2))
ireland_df <- cbind(c(rep(c(1985:2016), each = 12), rep(2017, nrow(ireland_df)%%12)),
                  c(rep(c(1:12), length(c(1985:2016))), c(1:(nrow(ireland_df)%%12))),
                  ireland_df)
colnames(ireland_df) <- c("Year", "Month", "Ireland")

# Japan
japan_df <- read_excel("Japan.xlsx", NULL, c(1:3))
colnames(japan_df)[3] <- "Japan"

# Korea
korea_df <- read_excel("Korea.xlsx", NULL, c(1:3), -1)
colnames(korea_df) <- c("Year", "Month", "Korea")

# Mexico
mexico_df <- read_excel("Mexico.xlsx", NULL, NULL, -1)
colnames(mexico_df)[3] <- "Mexico"

# Netherlands
netherlands_df <- read_excel("Netherlands.xlsx", NULL, c(2), -5)
netherlands_df <- cbind(c(rep(2003, 10), rep(c(2004:2016), each = 12), rep(2017, (nrow(netherlands_df)+2)%%12)),
                        c(c(3:12), rep(c(1:12), length(c(2004:2016))), c(1:((nrow(netherlands_df)+2)%%12))),
                        netherlands_df)
colnames(netherlands_df) <- c("Year", "Month", "Netherlands")

# Russia
russia_df <- read_excel("Russia.xlsx", NULL, NULL, -1)
colnames(russia_df)[3] <- "Russia"

# Sweden
sweden_df <- read_excel("Sweden.xlsx", NULL, NULL, -1)
colnames(sweden_df)[3] <- "Sweden"

# US
us_df <- read_excel("US.xlsx", NULL, c(1:3), -1)
colnames(us_df)[3] <- "US"

# Merging dataframes
date_cols <- setNames(data.frame(c(rep(c(1997:2016), each = 12), rep(2017, 11)), 
                                c(rep(c(1:12), length(c(1997:2016))), c(1:11))), c('Year', 'Month'))
date_cols$Index <- c(1:nrow(date_cols))

df_list <- list(date_cols, australia_df, brazil_df, canada_df, chile_df, china_df, europe_df, india_df,
                ireland_df, japan_df, korea_df, mexico_df, netherlands_df, russia_df, sweden_df,
                us_df)

raw_vals <- Reduce(function(x, y) merge(x, y, all.x = TRUE), df_list)
raw_vals <- raw_vals[order(raw_vals$Index),]
raw_vals$Index <- NULL