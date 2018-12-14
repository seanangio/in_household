### Data Source

All of the raw data for this visualisation comes from the Government of India's
<a href="http://censusindia.gov.in/DigitalLibrary/Archive_home.aspx" target="_blank">Census Digital Library</a>. To find the original tables, navigate to the Tables tab, select a Census Year, and choose "Tables on Houses, Household Amenities and Assets". Then for each State/UT, download the file regarding "Households Classified by Source and Location of Drinking Water and Availablity of Electricity and Latrine" and the corresponding SC and ST files where present.

### Spatial Units

The spatial unit mapped here is the district level. The number of districts grows from 466 in 1991, to 593 in 2001, to 640 in 2011. The changing composition of these districts poses two challenges. 

One challenge is that a given district's population and area both vary widely. This can be problematic for a choropleth because the area of the spatial unit can be misleading (See <a href="http://socviz.co/maps.html#maps" target="_blank">Kieran Healy, Data Visualization: A Practical Introduction</a>). For example, large districts like Ladakh (JK) stand out prominently on the map, but hold relatively few households. Metros like Mumbai (MH) on the other hand, are hardly visible without increasing the map's zoom, yet hold far more people.

I have tried to address this challenge in two ways. The first is that clicking on a district provides the raw district household counts. The second is that "Household Count" appears as a separate data type. Selecting it plots, on a logarithmic scale, the distribution of household counts per district. Particularly if one is unfamiliar with India's population density, it would be beneficial to first observe the household count distribution. 

A second challenge is that districts change frequently over time, complicating time series analysis across decades. Rather than try to plot a district-level time series, clicking on a district adds the district point and the associate state trend. States do change, but much less frequently than districts. Also, All-India and State/UT averages are reported as provided in the raw data, rather than explicitly calculated by summing districts.

### Missing Data

In 1991 and 2011, these tables report on a given geographic unit's performance with simultaneous regards to Electricity and Latrine status. Accordingly, it is possible to determine all combinations of electricity and latrine status. In 2001 however, figures are only reported with respect to the presence or absence of electricity and the presence or absence of a latrine. Although the data for this year is entirely missing, I have kept those levels as an option so that it becomes easier to scan across years where it is present.

In 1991, violence in Jammu and Kashmir prevented the census from being conducted according to the 1991 <a href="http://censusindia.gov.in/DigitalLibrary/data/Census_1991/Publication/India/45969_1991_CHN.pdf" target="_blank">Census Handbook</a>. Hence, you will find those districts appear gray on the map.

Lastly, I was unable to successfully read in the 1991 SC and ST files for Bihar. There seems to be an error in them.

### R Packages

Huge thanks to the authors and maintainers of all of the R packages used to make this visualisation, especially the `tidyverse`, `shiny`, `sf`, `leaflet`, `leaflet.extras`, `shinyWidgets`, `shinythemes`, and `shinycssloaders`.

### Further Information
The R scripts wrangling the data and creating the visualisation can be found on <a href="https://github.com/seanangio/in_household/" target="_blank">Github</a>. 

An exploration into the key findings, visualisation choices, and challenges can be found in a blog post for [Social Cops]().

Other data projects by the author can be found at his <a href="https://sean.rbind.io/" target="_blank">web site.</a>

Any project feedback is greatly appreciated. The author can be contacted on <a href="https://twitter.com/seanangiolillo/" target="_blank">Twitter</a> or via the email address on his web page.
