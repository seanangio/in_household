-- country data

CREATE TABLE country_census (
    geo_section char(8) NOT NULL,            -- level of geography (always state),
    year date CHECK                          -- census year
        (year IN ('1991-01-01',
              '2001-01-01','2011-01-01')), 
    societal_section varchar(3) CHECK 
        (societal_section IN 
            ('ALL','SC','ST')),              -- All, SC Only, ST Only
    demo_section char(5) CHECK 
        (demo_section IN 
            ('Total','Urban','Rural')),      -- Total, Urban Only, Rural Only
    water_source varchar(90) NOT NULL,       -- options like Tap, Well, Etc
    water_avail varchar(90) NOT NULL,        -- options like Within, Away from Premises
    
    total_hh integer,               -- total number of households
    num_ea_la integer,              -- count: both electricity, latrine
    num_ea_ln integer,              -- count: electricity, no latrine
    num_en_la integer,              -- count: no electricity, but latrine
    num_en_ln integer,              -- count: neither electricity, latrine
    num_ea integer,                 -- count: having electricity
    num_la integer,                 -- count: having latrine
    
    ea decimal(13,12) 
    CHECK (ea >= 0 AND ea <= 1 OR NULL),        -- % having electricity
    la decimal(13,12) 
    CHECK (la >= 0 AND la <= 1 OR NULL),        -- % having latrine
    ea_la decimal(13,12) 
    CHECK (ea_la >= 0 AND ea_la <= 1 OR NULL),  -- % having electricity and latrine    
    ea_ln decimal(13,12) 
    CHECK (ea_ln >= 0 AND ea_ln <= 1 OR NULL),  -- % having electricity but no latrine
    en_la decimal(13,12) 
    CHECK (en_la >= 0 AND en_la <= 1 OR NULL),  -- % having no electricity but latrine
    en_ln decimal(13,12)  
    CHECK (en_ln >= 0 AND en_ln <= 1 OR NULL),  -- % having neither electricity nor latrine
    
    num_total integer,   -- count: total households (same as total_hh where present)
    num_within integer,  -- count: households with water within premises
    num_near integer,    -- count: households with water near premises
    num_away integer,    -- count: households with water away from premises
    
    within decimal(13,12) 
    CHECK (within >= 0 AND within <= 1 OR NULL), -- % having water within premises
    near decimal(13,12) 
    CHECK (near >= 0 AND near <= 1 OR NULL),     -- % having water near premises
    away decimal(13,12) 
    CHECK (away >= 0 AND away <= 1 OR NULL),     -- % having water away from premises
    
    
    num_all integer,                -- count: total households (same as total_hh where present)
    num_tap_treated integer,        -- count: households with treated tap as water source
    num_tap_untreated integer,      -- count: households with untreated tap as water source
    num_covered_well integer,       -- count: households with covered well as water source
    num_uncovered_well integer,     -- count: households with uncovered well as water source
    num_hand_pump integer,          -- count: households with hand pump as water source
    num_tube_well integer,          -- count: households with tube well as water source
    num_others integer,             -- count: households with other as water source
    
    tap_treated decimal(13,12) 
    CHECK (tap_treated >= 0 AND tap_treated <= 1 OR NULL),          -- % having treated tap as water source
    tap_untreated decimal(13,12) 
    CHECK (tap_untreated >= 0 AND tap_untreated <= 1 OR NULL),      -- % having untreated tap as water source
    covered_well decimal(13,12) 
    CHECK (covered_well >= 0 AND covered_well <= 1 OR NULL),        -- % having covered well as water source
    uncovered_well decimal(13,12) 
    CHECK (uncovered_well >= 0 AND uncovered_well <= 1 OR NULL),    -- % having uncovered well as water source
    hand_pump decimal(13,12) 
    CHECK (hand_pump >= 0 AND hand_pump <= 1 OR NULL),              -- % having hand pump as water source
    tube_well decimal(13,12) 
    CHECK (tube_well >= 0 AND tube_well <= 1 OR NULL),              -- % having tube well as water source
    others decimal(13,12)
    CHECK (others >= 0 AND others <= 1 OR NULL),                    -- % having other as water source
    
    -- misc columns present in certain years
    
    num_en integer,                 -- count: households without electricity (only reported 2001)
    num_ln integer,                 -- count: households without latrine (only reported 2001)
    num_tap integer,                -- count: households with tap as water source
    num_well integer,               -- count: households with well as water source
    
    tap decimal(13,12) 
    CHECK (tap >= 0 AND tap <= 1 OR NULL),      -- % having tap (undifferentiated) as water source
    well decimal(13,12) 
    CHECK (well >= 0 AND well <= 1 OR NULL),    -- % having well as water source
    
    num_outside integer,            -- count: households with water outside premises (only 1991)
    
    outside decimal(13,12) 
    CHECK (outside >= 0 AND outside <= 1 OR NULL), -- % having water outside premises (only 1991)
    
    num_handpump_tubewell integer,  -- count: households with handpump/tubewell as water source
    num_river_canal integer,        -- count: households with river/canal as water source
    num_tank integer,               -- count: households with tank as water source
    
    handpump_tubewell decimal(13,12) 
    CHECK (handpump_tubewell >= 0 AND handpump_tubewell <= 1 OR NULL),  -- % having handpump/tubewell as water source
    river_canal decimal(13,12) 
    CHECK (river_canal >= 0 AND river_canal <= 1 OR NULL),              -- % having river/canal as water source
    tank decimal(13,12) 
    CHECK (tank >= 0 AND tank <= 1 OR NULL),                            -- % having tank as water source
    
    CONSTRAINT country_census_key PRIMARY KEY 
        (year, societal_section, 
        demo_section, water_source, water_avail)   
);

COPY country_census
FROM '/Users/seanangiolillo/Library/Mobile Documents/com~apple~CloudDocs/in_household/sql/country_census.csv'
WITH (FORMAT CSV, HEADER, NULL 'NA');