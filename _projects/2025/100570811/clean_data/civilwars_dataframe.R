## Creating civil wars dataset 

civil_wars_data <- data.frame(
  location = c(
    # East Asia, South-East Asia & Oceania (21 conflicts)
    "China", "China (Tibet)", "Vietnam", "South Vietnam", "Cambodia", 
    "Indonesia", "Indonesia", "Indonesia (East-Timor)", "Indonesia (West-Papua)", 
    "Indonesia (Aceh)", "Myanmar", "Myanmar (Kachin)", "Myanmar (Karen)", 
    "Myanmar (Shan)", "Myanmar (Arakan)", "Philippines", "Philippines", 
    "Philippines (Mindanao)", "Laos", "Malaysia", "Thailand",
    
    # South & Central Asia (17 conflicts)
    "Afghanistan", "Sri Lanka", "Pakistan (East)", "Pakistan", 
    "Pakistan (Balochistan)", "India (Kashmir)", "India (Punjab/Khalistan)", 
    "India", "India", "India (Nagaland)", "Nepal", "Tajikistan", 
    "Azerbaijan", "Iran", "Iran (Kurdistan)", "Hyderabad", "Georgia",
    
    # Sub-Saharan Africa (31 conflicts)
    "Ethiopia (Eritrea)", "Ethiopia", "Ethiopia (Ogaden)", "Ethiopia (Oromia)", 
    "Cameroon", "Cameroon", "Angola", "Angola", "Uganda", "Mozambique", 
    "Mozambique", "Sudan", "Sudan (South)", "Nigeria", "Somalia", "Chad", 
    "Congo", "Congo", "Congo-Brazzaville", "Madagascar", "Zimbabwe", 
    "Sierra Leone", "Liberia", "Rwanda", "Burundi", "Guinea-Bissau", 
    "Kenya", "South Africa", "South Africa (Namibia)",
    
    # Middle East & North Africa (15 conflicts)
    "Algeria", "Algeria", "Lebanon", "Iraq", "Iraq (Kurdistan)", 
    "Yemen (North)", "Yemen (North)", "Yemen (South)", "Yemen", 
    "Syria", "Syria", "Israel (Palestinian Territories)", 
    "Morocco (Western Sahara)", "Libya",
    
    # Europe (11 conflicts)
    "Greece", "Turkey (Kurdistan)", "Russia (Chechnya)", 
    "Russia (Caucasus Emirate)", "Bosnia (Serbia)", "Bosnia (Croatia)", 
    "Soviet Union (Ukraine)", "Soviet Union (Lithuania)", "Serbia (Croatia)", 
    "Serbia (Kosovo)", "United Kingdom (Northern Ireland)",
    
    # Americas (7 conflicts)
    "El Salvador", "Guatemala", "Nicaragua", "Colombia", "Peru", 
    "Argentina", "Paraguay", "Dominican Republic"
  ),
  
  start_year = c(
    # East Asia, South-East Asia & Oceania
    1946, 1950, 1946, 1955, 1967,
    1954, 1946, 1975, 1965, 1990,
    1948, 1961, 1949, 1959, 1949,
    1947, 1969, 1970, 1959, 1949, 1974,
    
    # South & Central Asia
    1978, 1984, 1971, 2007,
    1974, 1989, 1983, 1983, 1952,
    1948, 1956, 1996, 1993, 1979,
    1979, 1948, 1992,
    
    # Sub-Saharan Africa
    1964, 1976, 1977, 1978,
    1960, 1958, 1961, 1975, 1975,
    1977, 1964, 1971, 1964, 1967,
    1988, 1967, 1997, 1964, 1993,
    1948, 1968, 1992, 2000, 1990,
    1991, 1963, 1953, 1982, 1966,
    
    # Middle East & North Africa
    1954, 1991, 1975, 1982, 1961,
    1962, 1948, 1986, 2009,
    2011, 1979, 1948,
    1975, 2011,
    
    # Europe
    1946, 1984, 1994, 2007,
    1992, 1993, 1946, 1946,
    1991, 1998, 1971,
    
    # Americas
    1972, 1965, 1978, 1964,
    1982, 1974, 1947, 1965
  ),
  
  end_year = c(
    # East Asia, South-East Asia & Oceania
    1949, 1958, 1954, 1964, 1998,
    1961, 1946, 1998, 1978, 2005,
    1993, 1992, 2011, 2011, 1993,
    1953, 2012, 2012, 1973, 1957, 1982,
    
    # South & Central Asia
    2012, 2009, 1971, 2012,
    1977, 2012, 1993, 2012, 1990,
    1968, 2006, 1998, 1994,
    2011, 1997, 1949, 1993,
    
    # Sub-Saharan Africa
    1991, 1991, 1984, 2012,
    1961, 1959, 1974, 2002, 2011,
    1992, 1974, 2012, 1973, 1970,
    2012, 2010, 2012, 1967, 2002,
    1948, 1979, 2002, 2003, 2012,
    2008, 1974, 1957, 1989, 1988,
    
    # Middle East & North Africa
    1962, 2012, 1990, 2012, 1996,
    1970, 1948, 1986, 2012,
    2012, 1982, 2012,
    1989, 2012,
    
    # Europe
    1949, 2012, 2008, 2012,
    1995, 1994, 1950, 1948,
    1991, 1999, 1998,
    
    # Americas
    1991, 1990, 1990, 2012,
    2010, 1978, 1958, 1965
  ),
  
  deaths_thousands = c(
    # East Asia, South-East Asia & Oceania
    600.0, 10.0, 188.6, 82.5, 186.8, 17.1, 1.8, 37.7, 4.8, 2.3, 
    25.2, 18.0, 17.4, 15.0, 3.4, 19.5, 4.8, 22.1, 14.0, 5.4, 2.5,
    
    # South & Central Asia
    334.4, 62.2, 55.9, 19.2, 4.3, 19.3, 7.6, 4.8, 6.3, 2.2, 
    9.9, 6.8, 4.6, 3.7, 3.3, 3.2, 2.2,
    
    # Sub-Saharan Africa
    122.7, 48.4, 10.8, 2.8, 77.5, 5.9, 39.5, 71.3, 67.4, 62.9, 
    11.7, 57.3, 11.7, 37.5, 31.6, 22.3, 15.3, 3.6, 14.2, 4.6, 
    13.6, 10.6, 2.6, 9.2, 8.6, 7.5, 6.5, 2.2, 6.7,
    
    # Middle East & North Africa
    91.3, 18.3, 66.3, 18.4, 34.6, 15.9, 2.0, 5.7, 3.7, 15.9, 
    3.8, 9.3, 6.4, 2.3,
    
    # Europe
    77.0, 27.0, 17.6, 2.4, 11.6, 3.6, 8.9, 4.3, 3.9, 2.6, 1.5,
    
    # Americas
    28.7, 21.8, 20.3, 19.3, 11.4, 2.3, 2.0, 2.0
  ),
  
  region = c(
    # East Asia, South-East Asia & Oceania (21)
    rep("East Asia, South-East Asia & Oceania", 21),
    
    # South & Central Asia (17)
    rep("South & Central Asia", 17),
    
    # Sub-Saharan Africa (29)
    rep("Sub-Saharan Africa", 29),
    
    # Middle East & North Africa (14)
    rep("Middle East & North Africa", 14),
    
    # Europe (11)
    rep("Europe", 11),
    
    # Americas (8)
    rep("Americas", 8)
  ),
  
  stringsAsFactors = FALSE
) |> 
  arrange(desc(deaths_thousands))

write.csv(civil_wars_data,
          "civil_wars_data.csv",
          row.names = FALSE)