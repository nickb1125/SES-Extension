# SES-Extension
Mapping, cleaning, and pulling function extensions for tidycensus SES package

**NOTE: Open usage_example.html in example folder to see how pulling and functions work. In order to use functions, you will need to (1) get tidycensus key and (2) pull shape data using a few lines. These processes are both described in usage_example.html.**

Note that all pulled data is based on previous 5 year census averages (ie. 2020 = 2015-2020 average) as we have found this to be most reliable window. Alternatives that could be implimented include 10 year averages as well as individual years, which we have found to be highly unstable. Also note that data cleaning reflects the census codes that have been used previously (which are found in codes_data folder) and additonal cleaning would likely need to be implimented in the get_ACS() functions if previously unpulled variables were added.

## Pulling Functions

(1) *get_multiple_ACS(years, codes, geo)* **(Most useful)**

Takes list of years (integers), codes, and geo ("county" or "zcta") and returns wide formatted census data with cleaned and appropriate labels (not codes) for all years requested.

(2) *get_ACS_long(year, codes, geo)*

Takes year (integer), codes, and geo ("county" or "zcta") and returns long formatted census data with requested codes

## Ploting Functions

(1) *get_leaflet(dat_all, years, states, attribute, geo)*

Takes dat_all (pulled SES data using get_multiple_ACS()), years (single year, integer), states (single, as abbreviation i.e. "NC" or "All"), attribute (SES variable to be plotted), and geo ("county" or "zcta"), to get draggable leaflet plot of attributes. Note that only individual stage with zoom programmed appropriately is NC, so generally more useful to use "All" to get entire US. 

(2) *get_geom_poly(dat_all, states, years, attribute, geo)*

Takes dat_all (pulled SES data using get_multiple_ACS()), years (single year, integer), states (single, as abbreviation i.e. "NC" or "All"), attribute (SES variable to be plotted), and geo ("county" or "zcta")

(3) *see_year_progression(dat_all = dat_all, attribute, geo, years)*

Same as get_geom_poly() but shows progression of multiple years (i.e. years variable takes in list of years as opposed to single)








