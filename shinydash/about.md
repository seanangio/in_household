### Objectives

This visualization is the third and final installment of a series of projects concerning India's electricity and latrine census data. 

The [first project](https://shiny.socialcops.com/gisvector/) was an interactive choropleth with matching histogram and line plot. The [second](https://seanangio.shinyapps.io/thematic_mapping/) focused on comparative thematic mapping styles, such as dot density maps, proportional symbols maps, and 3D choropleths.

While the first two projects prioritized spatial representations of the data, this project includes spatial and non-spatial representations to explore a more specific question. For every state or district, what is the relationship between household access to electricity AND household access to latrines? How do these metrics vary with respect to each other? We might reasonably expect that states or districts that have high levels of access to electricity also have high levels of access to latrines. To what extent is this true? And where is that pattern not the case?

Visualizing this relationship poses a few challenges. The first is the need to plot two variables for each spatial unit. The scatterplot does this by assigning the rate of electricity and latrine access to an x and y coordinate, respectively. The dumbbell plot uses these two metrics as the end points of a line segment. The bubble map uses a bivariate color scheme to track positions with respect to both variables. Doing so allows us to define categories that encompass two dimensions at the cost of much less specificity for each.

The next challenge is to account for the vastly uneven population counts across spatial units. (Think huge states like Uttar Pradesh and tiny union territories like Lakshadweep). With this in mind, I chose visualizations that allow for the household count to be mapped to a size aesthetic. In the cases of the scatterplot, dumbbell plot and bubble map, each provides a way to distinguish population differences by mapping household counts to the area of the respective points. For example, I chose a bubble map over a choropleth for this reason.

Lastly, I wanted the flexibility to examine data at both the state and district levels, where again population differences between metro districts and rural districts are substantial. Toggling between state and district level units is simple enough. However, in most cases, the high number of districts (640 for 2011) creates too much overcrowding. In order to address this, I built a series of geographic filters, the ability to add or remove regions or individual states, that can be used to explore a particular geographic area in clearer detail. 

### Data Source

All of the raw data for this visualisation comes from the Government of India's [Census Digital Library](http://censusindia.gov.in/DigitalLibrary/Archive_home.aspx). To find the original tables, navigate to the Tables tab, select a Census Year, and choose "Tables on Houses, Household Amenities and Assets". Then for each State/UT, download the file regarding "Households Classified by Source and Location of Drinking Water and Availablity of Electricity and Latrine" and the corresponding SC and ST files where present.

Greater discussion of the data cleaning process can be found in a previous [SocialCops blog](https://blog.socialcops.com/technology/data-science/shiny-electricity-latrine-water-india/) using the same dataset.

### R Packages

Many thanks to the authors and maintainers of all of the R packages used to make this visualisation, especially the {tidyverse}, {shiny}, {shinydashboard}, {shinydashboardplus}, {sf}, and {ggiraph}.

### Further Information

The R scripts wrangling the data and creating this visualisation can be found on [Github](https://github.com/seanangio/in_household). See the `shinydash_data.R` script for generating the app's data and the `shinydash` folder for the app's code.

Other data projects by the author can be found at his [web site](https://sean.rbind.io/).

Any project feedback is greatly appreciated. The author can be contacted on [Twitter](https://twitter.com/seanangiolillo) or via the email address on his web page.
