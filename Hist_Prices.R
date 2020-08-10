# Loading the Libraries
suppressPackageStartupMessages({
    if (!require("IBrokers")) install.packages("IBrokers"); library(IBrokers)
    if (!require("dplyr")) install.packages("dplyr"); library(dplyr)
    if (!require("parallel")) install.packages("parallel"); library(parallel)
    if (!require("doParallel")) install.packages("doParallel"); library(doParallel)
    if (!require("readxl")) install.packages("readxl"); library(readxl)
    if (!require("tictoc")) install.packages("tictoc"); library(tictoc)
    if (!require("emayili")) install.packages("emayili"); library(emayili)
    if (!require("stringr")) install.packages("stringr"); library(stringr)
    if (!require("ggplot2")) install.packages("ggplot2"); library(ggplot2)
    if (!require("tidyquant")) install.packages("tidyquant"); library(tidyquant)
    if (!require("tidyverse")) install.packages("tidyverse"); library(tidyverse)
})

# Setting the Time
Sys.setenv(TZ = "America/New_York")

# Start the Timer
S_Time <- Sys.time()

# Local Variables
ticket    <- read_xlsx("DDBB/Lista Carlos.xlsx", sheet = "Sheet1", col_names = TRUE)
first_run <- TRUE

# Loading more Cores
no_cores <- (detectCores() - 1)
cl       <- makeCluster(no_cores)
registerDoParallel(cl) 

# Local Dataframes
DDBB_Global <- NULL

# Realizamos la conexion con IB
tws <- twsConnect(clientId = 120315, port = 2000)
isConnected(tws)

# Analysis of the strategy
for(i in 1:nrow(ticket)){ # i <- 1
    
    data  <- reqHistoricalData(tws, 
                               twsSTK(ticket[i, 1] %>% pull(1)), 
                               whatToShow  = 'MIDPOINT', 
                               useRTH      = "1", 
                               barSize     = '1 min', 
                               duration    = "1 D", 
                               endDateTime = paste0(gsub("-","", reqCurrentTime(tws)))) %>%
        as.data.frame() %>%
        select(1:4) %>%
        `colnames<-`(c("Open", "High", "Low", "Close")) %>%
        rownames_to_column(var = "Dates")
     
    Open_Price <- data %>% head(1) %>% pull(2)
    DDBB_Delta <- data %>% 
        dplyr::select(1, 5) %>%
        dplyr::mutate(Delta = round((Close/Open_Price - 1)*100, digits = 2)) %>%
        dplyr::select(1, 3) %>%
        `colnames<-`(c("Dates", ticket[i, 1] %>% pull(1)))
    
    if(first_run == TRUE){
        DDBB_Global <- bind_cols(DDBB_Global, DDBB_Delta)
        first_run   <- FALSE
    }else{
        DDBB_Global <- left_join(DDBB_Global, DDBB_Delta, by = "Dates")
    }
}

# tws Disconnect
twsDisconnect(tws)

# Closing the extra cores
stopCluster(cl)

# Setting the Time
E_Time <- Sys.time()

# Total Time
E_Time - S_Time

DDBB_Global %>%
    gather(variable, value, -Dates) %>% 
    spread(Dates, value) %>%
    write.csv(file = "Results/Portfolio_Performance.csv")
