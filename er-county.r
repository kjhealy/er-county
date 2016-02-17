library(maptools)
library(mapproj)
library(rgeos)
library(rgdal)
library(RColorBrewer)
library(ggplot2)
library(stringr)
library(scales)
library(RColorBrewer)

###--------------------------------------------------
### Set up the Maps.
### Code borrows heavily from work by Bob Rudis:
### https://github.com/hrbrmstr/rd3albers
###--------------------------------------------------

theme_set(theme_minimal())

## Make a "figures" subdirectory if one doesn't exist
ifelse(!dir.exists(file.path("figures")),
       dir.create(file.path("figures")),
       FALSE)


## for theme_map
## devtools::source_gist("33baa3a79c5cfef0f6df")

theme_map <- function(base_size=9, base_family="") {
    require(grid)
    theme_bw(base_size=base_size, base_family=base_family) %+replace%
    theme(axis.line=element_blank(),
          axis.text=element_blank(),
          axis.ticks=element_blank(),
          axis.title=element_blank(),
          panel.background=element_blank(),
          panel.border=element_blank(),
          panel.grid=element_blank(),
          panel.margin=unit(0, "lines"),
          plot.background=element_blank(),
          legend.justification = c(0,0),
          legend.position = c(0,0)
          )
}

## US Census Shapefiles
## https://www.census.gov/geo/maps-data/data/cbf/cbf_counties.html

## Converted to geojson format
## http://eric.clst.org/Stuff/USGeoJSON
## Read U.S. counties moderately-simplified GeoJSON file
us.counties <- readOGR(dsn="data/geojson/gz_2010_us_050_00_5m.json",
                       layer="OGRGeoJSON")

# Convert it to Albers equal area
us.counties.aea <- spTransform(us.counties,
                               CRS("+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs"))

us.counties.aea@data$id <- rownames(us.counties.aea@data)

# Extract, then rotate, shrink & move alaska (and reset projection)
# need to use state IDs via # https://www.census.gov/geo/reference/ansi_statetables.html
alaska <- us.counties.aea[us.counties.aea$STATE=="02",]
alaska <- elide(alaska, rotate=-50)
alaska <- elide(alaska, scale=max(apply(bbox(alaska), 1, diff)) / 2.3)
alaska <- elide(alaska, shift=c(-2100000, -2500000))
proj4string(alaska) <- proj4string(us.counties.aea)

# extract, then rotate & shift hawaii
hawaii <- us.counties.aea[us.counties.aea$STATE=="15",]
hawaii <- elide(hawaii, rotate=-35)
hawaii <- elide(hawaii, shift=c(5400000, -1400000))
proj4string(hawaii) <- proj4string(us.counties.aea)

# remove old states and put new ones back in; note the different order
# we're also removing puerto rico in this example but you can move it
# between texas and florida via similar methods to the ones we just used
us.counties.aea <- us.counties.aea[!us.counties.aea$STATE %in% c("02", "15", "72"),]
us.counties.aea <- rbind(us.counties.aea, alaska, hawaii)


###--------------------------------------------------
### Make the county-level data frame, with county names
### and state abbreviations for convenience (e.g. if
### we want to subset states for closer inspection later)
###--------------------------------------------------




## Convenience function to clean a year's worth of data
clean.pop <- function(fname,
                      counties=county.names,
                      states=state.data){

    require(Hmisc)

    popdata <- read.csv(fname, row.names = 1)
    colnames(popdata) <- c("fips", "aa.pop")
    popdata$id <- as.character(popdata$fips)

    ## Drop NAs
    ind <- complete.cases(popdata)
    popdata <- popdata[ind,]

    ## Get FIPS ready for use with maps later
    ## by restoring leading zeros to the FIPS codes
    ind <- popdata$fips<10000
    popdata$id[ind] <- paste("0", popdata$id[ind], sep="")
    popdata$id[popdata$id=="00"] <- "0"

    ind <- match(popdata$fips, counties$fips)
    popdata$name <- counties$name[ind]
    popdata$state <- counties$state[ind]

    ind <- match(states$fips, popdata$fips)
    popdata$state[ind] <- states$State.Abbr

    ind <- match(states$fips, popdata$fips)
    popdata[is.na(popdata$state),]

    levels(popdata$name) <- c(levels(popdata$name), levels(states$State))
    popdata$name[ind] <- states$State

    ## Create a categorical population measure, cutting aa.pop up into bins
    popdata$aa.pop.cut <- cut2(popdata$aa.pop,
                               cuts=c(0, 100, 1000, 10000, 50000, 100000, 200000))

    return(popdata)
}

## label for use later in the map legends
cut.points <- c("<100", "100-1,000", "1k-10k", "10k-50k", "50k-100k", "100k-200k", ">200k")


## use this to get state fips (has extraneous data we don't need)
state.data <- read.csv("data/census/state-data-statabs-2012.csv", header=TRUE)

### Table of county names and FIPS
county.names <- read.csv("data/census/fips-by-state.csv", header=TRUE)

### ER's data

### We need to clean the fips a bit so they will
### match properly --- need to restore leading zeros
### basically, and remove NA rows. That's what the function
### above does (with no error checking, watch out!)

## 1910.
pop10 <- clean.pop("data/pop/map10-edited.csv")

## 1950
pop50 <- clean.pop("data/pop/map50-edited.csv")


###--------------------------------------------------
### Merge
##--------------------------------------------------

### Merge 1910 population data with map file

## create spatial data frame
co.map10 <- fortify(us.counties.aea, region="GEO_ID")

## strip out fips prefix
co.map10$id <- str_replace(co.map10$id, "0500000US", "")

## merge in population data by fips
co.map10 <- merge(co.map10, pop10, by="id")

## reorder (because the ER data were out of order)
co.map10 <- co.map10[order(co.map10$order),]


### Merge 1950 population data with map file
co.map50 <- fortify(us.counties.aea, region="GEO_ID")

co.map50$id <- str_replace(co.map50$id, "0500000US", "")

co.map50 <- merge(co.map50, pop50, by="id")

co.map50 <- co.map50[order(co.map50$order),]


###--------------------------------------------------
### Make the maps
###--------------------------------------------------

## Draw the 1910 data alone
p <- ggplot(data=co.map10, aes(x=long, y=lat, group=fips))

## With a lot of missing data it can be nice to have a county
## boundary grid underneath. That's all this layer does.
p0 <- p + geom_polygon(data=co.map10, aes(x=long, y=lat, group=group),
             color="gray70",
             fill=NA,
             size=0.03)


p1 <- p0 + geom_map(data=co.map10,
                   map = co.map10,
                   aes(map_id=id,
                       x=long,
                       y=lat,
                       group=group,
                       fill=aa.pop.cut),
                   color="white",
                   size=0.2)


p2 <- p1 + scale_fill_brewer(palette="Oranges",
                             labels = cut.points)
p2 <- p2 + coord_equal()
p2 <- p2 + theme_map()
p2 <- p2 + theme(legend.position="right") + labs(fill="Number")
p2 <- p2 + ggtitle("African American Population in 1910")

## Note it doesn't use the full scale
pdf(file="binned-map-1910.pdf", height=10, width=15)
print(p2)
dev.off()


### Draw the 1950 data alone
p <- ggplot(data=co.map50, aes(x=long, y=lat, group=fips))

p0 <- p + geom_polygon(data=co.map50, aes(x=long, y=lat, group=group),
             color="gray70",
             fill=NA,
             size=0.03)


p1 <- p0 + geom_map(data=co.map50,
                   map = co.map50,
                   aes(map_id=id,
                       x=long,
                       y=lat,
                       group=fips,
                       fill=aa.pop.cut),
                   color="white",
                   size=0.2)

p2 <- p1 + scale_fill_brewer(palette="Oranges",
                             labels = cut.points)
p2 <- p2 + coord_equal()
p2 <- p2 + theme_map()
p2 <- p2 + theme(legend.position="right") + labs(fill="Number")
p2 <- p2 + ggtitle("African American Population in 1950")

pdf(file="binned-map-1950.pdf", height=10, width=15)
print(p2)
dev.off()


###--------------------------------------------------
### Combine into one data set and then plot
###--------------------------------------------------

library(dplyr)

## First add an identifying variable

co.map10$Year <- "1910"
co.map50$Year <- "1950"

big.data <- rbind_all(list(co.map10, co.map50))


### Now a faceted plot

p <- ggplot(data=big.data, aes(x=long, y=lat, group=group))

p0 <- p + geom_polygon(data=big.data, aes(x=long, y=lat, group=group),
             color="gray70",
             fill=NA,
             size=0.03)

p1 <- p0 + geom_map(data=big.data,
                   map = big.data,
                   aes(map_id=id,
                       x=long,
                       y=lat,
                       group=fips,
                       fill=aa.pop.cut),
                   color="white",
                   size=0.2)


p2 <- p1 + scale_fill_brewer(palette="Oranges",
                             labels = cut.points)
p2 <- p2 + coord_equal()
p2 <- p2 + theme_map()
p2 <- p2 + theme(legend.position="right") + labs(fill="Number")
p2 <- p2 + facet_wrap(~ Year) + ggtitle("African American Population")

pdf(file="binned-map-both.pdf", height=10, width=15)
print(p2)
dev.off()
