library(tidyverse)
library(lubridate)
library(edwr)

dir_raw <- "data/raw/consults"

consults <- read_data(dir_raw, "consults", FALSE) %>%
    rename(
        millennium.id = `Encounter Identifier`,
        order.datetime = `Date and Time - Order Start`,
        order = `Mnemonic (Primary Generic) FILTER ON`,
        order.location = `Nurse Unit (Order)`
    ) %>%
    format_dates("order.datetime") %>%
    filter(
        order.datetime >= mdy("7/1/2017", tz = "US/Central"),
        order.datetime < mdy("7/1/2018", tz = "US/Central")
    ) %>%
    mutate(order.day = floor_date(order.datetime, unit = "days")) %>%
    distinct(millennium.id, order.day, .keep_all = TRUE)

monthly <- consults %>%
    mutate(order.month = floor_date(order.datetime, unit = "month")) %>%
    distinct(millennium.id, order.month) %>%
    count(order.month)
