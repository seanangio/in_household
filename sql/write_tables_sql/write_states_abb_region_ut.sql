--CREATE DATABASE in_household;

CREATE TABLE states_abb_region_ut (
    state varchar(90),                  -- state name,
    abb char(2) NOT NULL,               -- state abbreviation
    region varchar(90) NOT NULL,        -- state region
    year date CHECK 
        ((year IN ('1991-01-01',
        '2001-01-01','2011-01-01'))),    -- state year (states change over time)
    ut_status boolean NOT NULL,         -- is it a union territory?
        
    CONSTRAINT state_key PRIMARY KEY (state, year)
);

-- should add indices?
    
COPY states_abb_region_ut
FROM '/Users/seanangiolillo/Library/Mobile Documents/com~apple~CloudDocs/in_household/sql/states_abb_region_ut.csv'
WITH (FORMAT CSV, HEADER);

