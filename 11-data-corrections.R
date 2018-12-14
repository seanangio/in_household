### a number of small errors along the way were easier to fix at the end
### instead of re-running scripts

# country 91 is missing num_ea and num_la
c91 <- readRDS("shiny/data/country/country91.rds")
c91 <- c91 %>% 
    mutate(
        num_ea = num_ea_la + num_ea_ln,
        num_la = num_ea_la + num_en_la
    ) %>% 
    select(1:11, c("num_ea", "num_la"), everything())

saveRDS(c91, "shiny/data/country/country91.rds")

# State corrections -------------------------------------------------------

s11 <- readRDS("shiny/data/state/state11.rds")
s01 <- readRDS("shiny/data/state/state01.rds")
s91 <- readRDS("shiny/data/state/state91.rds")

# s91 missing variables num_ea, num_la
s91 <- s91 %>% 
    mutate(
        num_ea = num_ea_la + num_ea_ln,
        num_la = num_ea_la + num_en_la
    ) %>% 
    select(1:12, c("num_ea", "num_la"), everything())

# level changes; in 91: Others -> All Others; 01: All others -> All Others
s91 <- s91 %>% 
    mutate(water_source = fct_recode(water_source, 
                                     `All Others` = "Others"))

s01 <- s01 %>% 
    mutate(water_source = fct_recode(water_source, 
                                     `All Others` = "All others"))

# gave wrong code for la
s01 <- s01 %>% 
    mutate(la = num_la / total_hh)

saveRDS(s01new, "shiny/data/state/state01.rds")

# need to complete implicitly missing rows

abb_df <- readRDS("8-abb-region/abb_region.rds") %>% 
    select(-region)

# 11: 3*3*8*4*35 = 10080
s11 <- s11 %>% 
    filter(geo_section == "state") %>% 
    complete(year, state, societal_section, demo_section, 
             water_source, water_avail) %>%
    mutate(geo_section = "state") %>%
    left_join(
        abb_df %>% select(abb, state11),
        by = c("state" = "state11")
        ) %>% 
    select(-abb.x) %>%
    rename(abb = abb.y)

# 01: 3*3*6*4*35 = 7560
s01 <- s01 %>% 
    filter(geo_section == "state") %>% 
    complete(year, state, societal_section, demo_section,
             water_source, water_avail) %>%
    mutate(geo_section = "state") %>%
    left_join(
        abb_df %>% select(abb, state01),
        by = c("state" = "state01")
    ) %>% 
    select(-abb.x) %>%
    rename(abb = abb.y)

# 91: 3*3*7*3*31 = 5859 ; but entirely missing JK
s91 <- s91 %>% 
    filter(geo_section == "state") %>% 
    complete(year, state, societal_section, demo_section, 
             water_source, water_avail) %>%
    mutate(geo_section = "state") %>%
    left_join(
        abb_df %>% select(abb, state91),
        by = c("state" = "state91")
    ) %>% 
    select(-abb.x) %>%
    rename(abb = abb.y)

# now take correct number of rows and replace with JK
jk_rows <- s91 %>% 
    slice(1:189) %>% 
    mutate(
        state = "Jammu & Kashmir",
        abb = "JK"
    )
jk_rows[8:38] <- NA

# 91: 3*3*7*3*32 = 6048
s91 <- bind_rows(s91, jk_rows) %>% 
    arrange(abb)

saveRDS(s91, "shiny/data/state/state91.rds")
saveRDS(s01, "shiny/data/state/state01.rds")
saveRDS(s11, "shiny/data/state/state11.rds")


# District corrections ----------------------------------------------------

d11 <- readRDS("shiny/data/district/district11.rds")
d01 <- readRDS("shiny/data/district/district01.rds")
d91 <- readRDS("shiny/data/district/district91.rds")
d91 <- readRDS("shiny/data/district/district91_new.rds")

d11 <- d11 %>% 
    mutate(
        district = fct_recode(district, `Leh (Ladakh)` = "Leh(Ladakh)"),
        district_abb = fct_recode(district_abb, `Leh (Ladakh) (JK)` = "Leh(Ladakh) (JK)")
    )

saveRDS(d11, "shiny/data/district/district11.rds")

# d91 missing variables num_ea, num_la
d91 <- d91 %>%
    mutate(
        num_ea = num_ea_la + num_ea_ln,
        num_la = num_ea_la + num_en_la
    ) %>%
    select(1:15, c("num_ea", "num_la"), everything())

saveRDS(d91, "shiny/data/district/district91.rds")

# gave wrong code for la
d01 <- d01 %>% 
    mutate(la = num_la / total_hh)

saveRDS(d01, "shiny/data/district/district01.rds")

