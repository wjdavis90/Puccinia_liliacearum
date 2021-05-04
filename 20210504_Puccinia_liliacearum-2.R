#Set working directory
setwd("C://Users//wjdavis//Documents//R_working_dir//Puccinia_lil")

#Load libraries
library(tidyverse)
library(janitor)
library(lubridate)
library(collapse)
library(RColorBrewer)
library(patchwork)
library(maps)
library(sf)
library(mapproj)
library(ggspatial)
library(rnaturalearth)
librar(rnaturalearthdata)

#Load data & reduce to relevant columns

Puccinia_liliacearum_tidy <- read.csv(file="Puccinia_lil_20210408-1.csv", header =TRUE) %>%
clean_names() %>%
subset(, select = c(institution_code,
basis_of_record,
catalog_number,
scientific_name,
genus,
specific_epithet,
recorded_by,
year,
month,
day,
host,
country,
state_province,
decimal_latitude,
decimal_longitude,
geodetic_datum,
coordinate_uncertainty_in_meters)) %>%

#Bin by decade

mutate(decade= case_when(year %in% c(1800:1809) ~ "1800",
year %in% c(1810:1819) ~ "1810",
year %in% c(1820:1829) ~ "1820",
year %in% c(1830:1839) ~ "1830",
year %in% c(1840:1849) ~ "1840",
year %in% c(1850:1859) ~ "1850",
year %in% c(1860:1869) ~ "1860",
year %in% c(1870:1879) ~ "1870",
year %in% c(1880:1889) ~ "1880",
year %in% c(1890:1899) ~ "1890",
year %in% c(1900:1909) ~ "1900",
year %in% c(1910:1919) ~ "1910",
year %in% c(1920:1929) ~ "1920",
year %in% c(1930:1939) ~ "1930",
year %in% c(1940:1949) ~ "1940",
year %in% c(1950:1959) ~ "1950",
year %in% c(1960:1969) ~ "1960",
year %in% c(1970:1979) ~ "1970",
year %in% c(1980:1989) ~ "1980",
year %in% c(1990:1999) ~ "1990",
year %in% c(2000:2009) ~ "2000",
year %in% c(2010:2019) ~ "2010",
year %in% c(2020:2029) ~ "2020",)) %>%

relocate(decade, .after=day) %>%
relocate(scientific_name, .after=specific_epithet)

Puccinia_liliacearum_tidy <- type_convert(Puccinia_liliacearum_tidy,
							na= c("", "NA", "NA-NA"))

write.csv(Puccinia_liliacearum_tidy, file="Puccinia_liliacearum_tidy.csv")

#Deduplication with collapse
Puccinia_liliacearum_tidy_collapse <-Puccinia_liliacearum_tidy %>% 
						fgroup_by(recorded_by,
							year,
							month,
							day,
							state_province) %>%
						collapg %>%
						relocate(recorded_by, .after=scientific_name) %>%
						relocate(year, .after=recorded_by) %>%
						relocate(month, .after=year) %>%
						relocate(day, .after=month) %>%
						relocate(state_province, .after=country) %>%
						relocate(decade, .after=day)

write.csv(Puccinia_liliacearum_tidy_collapse, file="Puccinia_liliacearum_tidy_collaspe.csv")

Puccinia_liliacearum_tidy_collapse_decades <- drop_na(Puccinia_liliacearum_tidy_collapse, decade)

Puccinia_liliacearum_tidy_collapse_decades$decade <-as.character(Puccinia_liliacearum_tidy_collapse_decades$decade)

Puccinia_liliacearum_decade_plot <-ggplot(Puccinia_liliacearum_tidy_collapse_decades,
aes(x=decade)) +
geom_bar(fill="#081d58") +
scale_y_continuous(expand=expansion(mult=c(0, .02))) +
labs(x="Decade", y="Number of Collections") +
theme(axis.text.x=element_text(size=12, color="black")) +
theme(axis.text.y=element_text(size=12, color="black")) +
theme(axis.title.x=element_text(size=10, color="black")) +
theme(axis.title.y=element_text(size=10, color="black"))

pdf("Puccinia_liliacearum_decade_plot.pdf")
print(Puccinia_liliacearum_decade_plot)
dev.off()

states_map <-map_data("state")

Puccinia_liliacearum_tidy_collapse_lat_long <-drop_na(Puccinia_liliacearum_tidy_collapse_decades, decimal_latitude)


world <-ne_countries(scale="medium", returnclass="sf")

class(world)

states <-st_as_sf(map("state", plot=FALSE, fill=TRUE))

Puccinia_liliacearum_distribution_plot_geom_point <- ggplot(data=world)+
ggtitle("Distribution of Puccinia liliacearum in the United States of America") +
geom_sf(fill="white", color="black") +
geom_sf(data=states, fill=NA, color="black") +
geom_point(data=Puccinia_liliacearum_tidy_collapse_lat_long,
		aes(x=decimal_longitude, y=decimal_latitude,
			fill=decade),
		size=4,
		shape=21) +
scale_fill_brewer(palette="PuBu") +
theme(axis.text.x=element_text(size=12, color="black")) +
theme(axis.text.y=element_text(size=12, color="black")) +
theme(legend.position="bottom") +
coord_sf(xlim=c(-91, -69), ylim=c(36, 49)) +
labs(x=NULL, y=NULL)

pdf("Puccinia_liliacearum_distribution_plot_geom_point.pdf")
print(Puccinia_liliacearum_distribution_plot_geom_point)
dev.off()
