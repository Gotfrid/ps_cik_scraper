library(rvest)
library(data.table)
library(readxl)
library(crayon)

# === Read the list of region URL === #
url = "http://cikrf.ru"
page <- read_html(url)

region_choices <- page %>% 
    html_nodes(css = "select > option")

# combine data into single table
region_addresses <- data.table(
    region_name = html_text(region_choices),
    region_url = html_attr(region_choices, "value")
)

# data trimming and uniqueness happens only now
# to ensure correct data extraction in previous step
region_addresses <-
    region_addresses[, region_name := trimws(region_name, "both")
                     ][region_name != ""
                       ][!duplicated(region_name)]

region_addresses[, region_short := tstrsplit(region_url, "\\.", keep = 2)]
region_addresses[, election_url := gsub(pattern = ".izbirkom.ru", 
                                        replacement = ".vybory.izbirkom.ru", 
                                        x = region_url, 
                                        fixed = T)]
region_addresses[, ps_url := paste(election_url, 
                                   "region", 
                                   region_short,
                                   "?action=ik",
                                   sep = "/")]
# hotfix for HMAO
region_addresses[region_short == "hmao", 
                 ps_url := "http://www.khantu-mansy.vybory.izbirkom.ru/region/khantu-mansy/?action=ik"]


# # acquire vrn id
# # somehow dont need to change vrn or region url
# # only have to change region number
for (i in 1:nrow(region_addresses)) {
    print(region_addresses[i, region_name])
    page <- as.character(read_html(region_addresses$ps_url[i], encoding = "CP1251"))
    tik_id <- stringr::str_extract(page, "\\d{13}")
    region_num <- stringr::str_extract(page, "&region=.{7}") %>%
        strsplit("\"") %>%
        unlist() %>%
        .[3] %>% # should be 3 elements always
        as.character()

    region_addresses[i, vrn := tik_id]
    region_addresses[i, reg_num := region_num]
}

# acquire region children - TIK
vrn = region_addresses$vrn[1]
ps_url = region_addresses$ps_url[1]
datalist = list()

for (i in 1:nrow(region_addresses)) {
    reg_num = region_addresses$reg_num[i]
    reg_name = region_addresses$region_name[i]
    print(reg_name)
    
    url = glue::glue("{ps_url}Tree&region={reg_num}&vrn={vrn}")
    page <- read_html(url, encoding = "CP1251")
    region_children <- page %>% 
        html_node("body > p") %>% 
        html_text() %>% 
        jsonlite::fromJSON(flatten = F) %>% 
        .$children %>% 
        .[[1]] %>% 
        .[, c("id", "text")]
    datalist[[reg_name]] <- region_children
}

all_tik <- rbindlist(datalist, idcol = "region_name")
all_tik <- merge(all_tik,
                 region_addresses[, .(region_name, reg_num)],
                 all.x = T)
all_tik[, unique_name := paste0(region_name, "___", text)]

datalist_uik = list()
for (i in 606:nrow(all_tik)) {
    ps_url = region_addresses$ps_url[1]
    vrn = all_tik$id[i]
    reg_num = all_tik$reg_num[i]
    reg_name = all_tik$region_name[i]
    tik_name = all_tik$text[i]
    unique_name = all_tik$unique_name[i]
    
    cat(yellow(i), bgBlack(white(reg_name)), " --- ", tik_name, " --- ", vrn,"\n")
    
    url = glue::glue("{ps_url}Tree&region={reg_num}&vrn={vrn}&onlyChildren=true")
    page <- read_html(url, encoding = "CP1251")
    tik_children <- page %>% 
        html_node("body > p") %>% 
        html_text() %>% 
        jsonlite::fromJSON(flatten = F)
    if (!length(tik_children)) {
        cat(red("NO DATA"), "\n")
        datalist_uik[[unique_name]] <- data.frame(id = NA, text = NA)
        next    
    }
    
    datalist_uik[[unique_name]] <- tik_children[, c("id", "text")]
    Sys.sleep(2)
}

all_uik <- rbindlist(datalist_uik, idcol = "unique_name")
all_uik[, c("region_name", "tik") := tstrsplit(unique_name, "___")]
all_uik[, unique_name := NULL]
setnames(all_uik, "text", "uik")
all_uik <-
    merge(all_uik,
          region_addresses[, .(region_name, ps_url)],
          all.x = T,
          by = "region_name")

all_uik[, url := paste0(ps_url, "&vrn=", id)]
all_uik[, ps_url := NULL]
setcolorder(all_uik, c("region_name", "tik", "uik", "id", "url"))

# remove empty TIKs
all_uik <- all_uik[!is.na(id)]
# saveRDS(all_uik, "all_uik.RDS") # save it in case anything happens

# FINALLY, gather actual data
for (i in 1:nrow(all_uik)) {
    
    reg = all_uik[i, region_name]
    tik = all_uik[i, tik]
    uik = all_uik[i, uik]
    
    cat(yellow(i), bgBlack(white(reg)), " --- ", tik, " --- ", uik,"\n")
    
    page <- read_html(all_uik$url[i], encoding = "CP1251")
    
    # all nodes
    paragrapsh <- page %>% 
        html_nodes(xpath = "//p") %>% 
        html_text(trim = T)
    
    # address
    address <- paragrapsh[like(paragrapsh, "Адрес комиссии:")]
    
    # phone
    phone <- paragrapsh[like(paragrapsh, "Телефон:")]
    
    # email
    email <- paragrapsh[like(paragrapsh, "Адрес электронной почты:")]
    
    # duty end
    duty_end <- paragrapsh[like(paragrapsh, "Срок окончания полномочий:")]
    
    # map
    coordinates <- page %>% 
        html_node(xpath = "//span[@class='view_in_map']") %>% 
        html_attrs() %>% 
        .[c("coordlat", "coordlon")]
    
    all_uik[i, `:=`(
        uik_address = address,
        uik_phone = phone,
        uik_email = email,
        uik_duty_end = duty_end,
        uik_lon = coordinates['coordlon'],
        uik_lat = coordinates['coordlat']
    )]
    
    # time interval is an ethical MUST
    Sys.sleep(1)
}

saveRDS(all_uik, "all_uik_full.RDS")