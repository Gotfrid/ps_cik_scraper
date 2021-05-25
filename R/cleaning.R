library(data.table)

dt = readRDS("all_uik_data.RDS")

# perform checks - count lost data
dt[, .N, by = .(uik_address == "" | is.na(uik_address))]
dt[, .N, by = .(uik_lon == "" | is.na(uik_lon))]

# # remove unnecessary repeated data
# clean address
dt[, .N, by = .(grepl("^Адрес комиссии:", uik_address))]
dt[, uik_address := gsub("^Адрес комиссии:", "", uik_address)]
dt[, uik_address := trimws(uik_address)]
dt[uik_address == "", .N]

# clean phone
dt[, .N, by = .(grepl("^Телефон:", uik_phone))]
dt[, uik_phone := gsub("^Телефон:", "", uik_phone)]
dt[, uik_phone := trimws(uik_phone)]
dt[uik_phone == "", .N]

# clean email
dt[, .N, by = .(grepl("^Адрес электронной почты:", uik_email))]
dt[, uik_email := gsub("^Адрес электронной почты:", "", uik_email)]
dt[, uik_email := trimws(uik_email)]
dt[uik_email == "", uik_email := NA]

# clean duty date
dt[, .N, by = .(grepl("^Срок окончания полномочий:", uik_duty_end))]
dt[, uik_duty_end := gsub("^Срок окончания полномочий:", "", uik_duty_end)]
dt[, uik_duty_end := trimws(uik_duty_end)]
dt[uik_duty_end == "", uik_duty_end := NA]
dt[, uik_duty_end := as.Date(uik_duty_end, format = "%d.%m.%Y")]

saveRDS(dt, "uik_data_clean.RDS")
fwrite(dt, "uik_data_clean.csv")
