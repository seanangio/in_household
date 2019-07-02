-- state data

CREATE TABLE state_shapes (
    state varchar(90) NOT NULL,  -- state name
    year date CHECK                          -- census year
        (year IN ('1991-01-01',
              '2001-01-01','2011-01-01')), 
    geometry geography,
    CONSTRAINT state_shapes_key PRIMARY KEY 
        (state, year)   
);

COPY state_shapes
FROM '/Users/seanangiolillo/Library/Mobile Documents/com~apple~CloudDocs/in_household/sql/state_shapes.csv'
WITH (FORMAT CSV, HEADER, NULL 'NA');