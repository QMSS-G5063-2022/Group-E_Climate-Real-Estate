PROJECT TITLE: Rethinking Real Estate through Climate Change

NAMES: Rory Butler, Cindy Chen, Lizabeth Singh, Jeffray Tsai

GITHUB REPOSITORY:
https://github.com/QMSS-G5063-2022/Group-E\_Climate-Real-Estate

BRIEF DESCRIPTION OF THE PROPOSED VISUALIZATIONS/ ANALYSES: Frenzied
real estate investing and skyrocketing property values across the United
States have been a fixture in recent news stories, but these stories
rarely (if ever) consider the long-term impact of climate change on
whether a property is a "good deal". After all, could your
million-dollar home be prone to flooding or forest fires in five years?

Accordingly, the proposed data visualization project will explore the
question: where are the "good deals" and "bad deals" in real estate when
we incorporate climate change trends like natural disasters, flooding,
drought, and fires? In terms of "good deals", where are the
slowest-growing real-estate markets that are also the most climate
change resilient, as measured by frequency of extreme events or by a
custom metric that our team generates? Alternatively, where are the
"worst deals": the rapidly rising real estate markets that are doomed by
climate change? By incorporating county-level property value growth data
and climate change-related data, our visualizations offer the audience
value in elegantly identifying discrepancies that would not have been
easily distilled, as well as predicting the best and worst property
regions to invest in the near future.

Proposed Scope: 
-   County-level and state-level data on year-over-year
    median property values before 2019 (to exclude the pandemic) 
-   County-level data and state-level mapping climate-change incidents like
    natural disasters (tornadoes, hurricanes), flooding, wildfires, drought
-   Generate a metric 
-   Data by year for the past 10 years (potentially
    excluding the pandemic, which would mean data from 2009 - 2019) 
-   Focus on one US state or a few states (selection currently unfinalized; will
    depend on data availability)

LINKS TO DATA SOURCES/ API:

REAL ESTATE VALUE / SALE PRICE The real estate data will provide metrics
for property value in the form of median sale price and price indices.
-   Data on most recent median and average list price and price
    increases. While there is no historical data, we can use the
    county-level data to compare current county-level real estate metrics
    against themselves. https://www.realtor.com/research/data/
-   The FHFA government database has the following relevant information:
    state-level quarterly average and median home prices from 2000 to
    2010, and annual house price indices at various levels of
    granularity including county
    https://www.fhfa.gov/DataTools/Downloads/Pages/House-Price-Index-Datasets.aspx

-   American Community Survey data on median home value by county.
    https://data.census.gov/cedsci/table?q=United%20States&t=Housing%20Value%20and%20Purchase%20Price&g=0100000US%240500000&tid=ACSDT1Y2019.B25077&moe=false&tp=true

-   Monthly housing market data (by median sale price) from 2012 to now
    at the granularity of county, city, zip code, and neighborhood
    https://www.redfin.com/news/data-center/

County-level or state-level climate-change related data The climate
change data will quantify negative effects of climate change by county
and state, whether that be counting natural disaster incidents or
another measure such as annual precipitation. We will need to explore
data sets further to ensure that we have sufficient historical data by
county.

-   Weeks in Drought data by state/county:
    https://droughtmonitor.unl.edu/DmData/DataDownload/WeeksInDrought.aspx

-   FEMA API data on disaster declaration by county
    https://www.fema.gov/about/openfema/data-sets
    https://www.fema.gov/about/openfema/developer-resources

-   The US Climate Divisional Dataset in FTP format; includes historical
    precipitation Palmer Drought Severity index, which would help
    identify areas with increasing/decreasing precipitation and risk of
    flooding/drought
    https://www.ncei.noaa.gov/access/metadata/landing-page/bin/iso?id=gov.noaa.ncdc%3AC00702

COUNTY CODES Since we are using disparate data sets on property values
and climate change, we will need to join data to each county by a unique
standardized ID, and this data will allow us to do so: \*
https://www2.census.gov/geo/pdfs/maps-data/data/tiger/tiger2006se/app\_a03.pdf
\*
https://www.census.gov/library/reference/code-lists/ansi.html\#par\_statelist

OUTLINE BRIEFLY WHICH TYPES OF VISUALIZATIONS YOU PLAN TO USE: \*
Geospatial visualizations \* Choropleth maps with interactive data by
county. The choropleth's main variable could include real estate prices
or climate indicators with the interactive hovering providing county
identifier, population, mean sale price value \* Heat maps or
topographical maps that don't conform to county boundaries but still
provide a geospatial narrative of where flooding or fires may be
occurring

-   Line graphs and/or bar charts showing changes in real estate market
    or climate metrics (such as frequency of fires) over time, which are
    updated/customized based on the county selected

-   Bubble charts highlighting patterns in the county data. As opposed
    to a choropleth map which is best for highlighting how data appears
    geospatially, the bubble chart presents multiple variables
    simultaneously and allows us a more comparative view between data
    points.

-   Data tables: When a reader selects an entire state, they can easily
    and conveniently scroll through metrics by county in a single table

ARE THERE ANY SIGNIFICANT HURDLES THAT YOU HAVE DOUBTS ABOUT? WOULD NOT
SOLVE THEM RENDER THE PROJECT INCOMPLETE \* Narrowing the focus on one
state, multiple states, or the whole country as well as which types of
climate change-related natural disasters. This will depend on data
sources and how we define a manageable project scope Focusing on a
specific city or county would be more manageable for this project,
however, it would be more interesting if we could include state-wide
data

-   For visualizations, we are considering time-series line plots
    describing how counties have changed over time in terms of
    aggregated property value and climate change-related incidents, but
    this requires sufficient historical data

-   Too much data in terms of an overly-broad scope and the inclusion of
    granular data could be an issue especially if it affects website
    performance

-   Ability to read all the file types for the data sources that we've
    found (concern over reading and converting data types that we are
    unfamiliar with or may require other programs like ArcMap to open,
    etc.)
