### Objectives

This visualization is an effort to highlight the respective advantages and disadvantages of common styles of thematic mapping. It accompanies a SocialCops blog post on this topic found [here](https://blog.socialcops.com/technology/data-science/comparative-thematic-mapping/).

### Data Source

All of the raw data for this visualisation comes from the Government of India's [Census Digital Library](http://censusindia.gov.in/DigitalLibrary/Archive_home.aspx). To find the original tables, navigate to the Tables tab, select a Census Year, and choose “Tables on Houses, Household Amenities and Assets”. Then for each State/UT, download the file regarding “Households Classified by Source and Location of Drinking Water and Availablity of Electricity and Latrine” and the corresponding SC and ST files where present.

Greater discussion of the data cleaning process can be found in a previous [SocialCops blog](https://blog.socialcops.com/technology/data-science/shiny-electricity-latrine-water-india/) using the same dataset.

### R Packages
Many thanks to the authors and maintainers of all of the R packages used to make this visualisation, especially the {tidyverse}, {shiny}, {sf}, {mapdeck}, {shinyWidgets}, and {shinythemes}.

### Further Information
The R scripts wrangling the data and creating this visualisation can be found on [Github](https://github.com/seanangio/in_household). See the `dots.R` script for creating the dot density data and the `3d_choropleth.R` script for the other mapping styles.

An exploration into the key findings, visualisation choices, and challenges can be found in a blog post for Social Cops.

Other data projects by the author can be found at his [web site](https://sean.rbind.io/).

Any project feedback is greatly appreciated. The author can be contacted on [Twitter](https://twitter.com/seanangiolillo) or via the email address on his web page.
