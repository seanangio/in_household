build_table <- function(df, geo) {
    
    dat <- df %>% 
        select(geo_section, state, district, zone, year, societal_section, 
               demo_section, total_hh, ea, la) %>% 
        rename(
            State = state,
            District = district,
            Zone = zone,
            Year = year,
            Society = societal_section,
            Demographic = demo_section,
            Households = total_hh,
            Electricity = ea,
            Latrines = la
        )
    
    if (geo == "State") {
        dat %>% 
            select(-c(geo_section, District))
    } else {
        dat %>% 
            select(-geo_section)
    }
}

