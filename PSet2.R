### Problem Set 2
  # Due Jan 31, 2024

# 1 ----

 # link to repo: https://github.com/kelliehaddon/PSet2



# 2 ----

library(tidyverse)

# open my data
gspace = read_csv('greenspace_data_share.csv') # added quotes around the file name

# summarize average urban greenspace by region
table =
  gspace |>
  group_by(Major_Geo_Region) |> # missing pipe
  summarize(
    obs = n(), # missing comma
    avg = mean(annual_avg_2020), # missing underscore between avg_2020
    weighted_avg = mean(annual_weight_avg_2020) # missing underscore between weighted_avg
  )

# output as table
knitr::kable(table, digits = 1) # added library knitr to access kable function and changed object from "gspace" to "table"

  # See script below.


# 3 ----

count(gspace)

  # The greenspace data covers 1,038 urban areas.



# 4 ----

gs2021 = summarize(gspace,
                  mean = mean(annual_avg_2021, na.rm = T),
                  median = median(annual_avg_2021, na.rm = T),
                  minimum = min(annual_avg_2021, na.rm = T),
                  maximum = max(annual_avg_2021, na.rm = T))
knitr::kable(gs2021)

  # In 2021, both the mean and median of the annual average NDVI is 0.28, which is considered very low.
  # The annual average NDVI spans from a minimum of 0.04 (extremely low) to a maximum of 0.63 (very high).


# 5 ----

## a ----
high2015 = filter(gspace, indicator_2015 == 'High' | 
                    indicator_2015 == 'Very High' | 
                    indicator_2015 == 'Exceptionally High')
count(high2015)

  # 62 urban areas scored `High` or above for greenspace in 2015.

## b ----
exlow = filter(gspace,
               indicator_2010 == 'Exceptionally Low' |
                 indicator_2015 == 'Exceptionally Low' |
                 indicator_2020 == 'Exceptionally Low' |
                 indicator_2021 == 'Exceptionally Low')
count(exlow)

  # 240 urban areas scored `Exceptionally Low` at any point in the years covered.

## c ----
less_arid = filter(gspace, 
              Climate_region == "Arid" & 
                annual_weight_avg_2010 < annual_weight_avg_2020)
count(less_arid)

  # 225 urban areas in arid climate became greener from 2010 to 2020.

arid_check = select(less_arid, annual_weight_avg_2010, annual_weight_avg_2020)



# 6 ----

less_green = filter(gspace, annual_avg_2010 > annual_avg_2021)
count(less_green)

  # 128 urban areas became less green from 2010 to 2021.
  # These observations were not concentrated in a particular geographic or climate region.
  # The top geographic and climate regions only exceeded the second most common geographic and climate regions by about 10 urban areas.
  # Europe had the most urban areas fitting this criteria at 47, followed by Asia at 35 and Africa at 25.
  # The country with the most urban areas fitting this criteria was Russia at 13 followed by the United States and India, both at 8.
  # Temperate climates were the most popular among these areas at 45, followed by continental climates at 36.

count(less_green, Major_Geo_Region)
  # Most in Europe (47) followed by Asia (35) and Africa (25)
print(n = 120, count(less_green, Country))
  # Russia has the most (13), next at 8 being US and India
count(less_green, Climate_region)
  # Most are temperate (45), continental (36), and tropical climate (14)



# 7 ----

gspace = mutate(gspace,
                change = annual_avg_2021 - annual_avg_2010)


change_plot = ggplot(gspace) +
  geom_histogram(aes(change),
                 fill = 'darkolivegreen3',
                 color = 'darkolivegreen') +
  labs(x = 'Change in Annual Average',
       y = 'Count',
       title = 'Change in Greenspace from 2010 to 2021') +
  theme_minimal()

# See figure below.


# 8 ----

scatter = ggplot(gspace)+
  geom_point(aes(x = annual_weight_avg_2010,
                 y = annual_weight_avg_2021),
             color = 'darkolivegreen4',
             shape = 1) +
  labs(x = '2010',
       y = '2021',
       title = 'Population Weighted Annual NDVI as Mean of Seasonal NDVI Values',
       subtitle = '2010 vs 2021') +
  theme_minimal()

  # See figure below.

## Extra credit ----

gspace = mutate(gspace,
                change3 = annual_weight_avg_2021 - annual_weight_avg_2010)

gspace<-mutate(gspace,
                change4=case_when(
                  between(change3,0,1) ~ 'Added_Greenspace',
                  between(change3,-1,0) ~ 'Lost_Greenspace'))

scatterec = ggplot(gspace)+
  geom_point(aes(x = annual_weight_avg_2010,
                 y = annual_weight_avg_2021,
                 color = change4),
             shape = 1) +
  labs(x = '2010',
       y = '2021',
       title = 'Population Weighted Annual NDVI as Mean of Seasonal NDVI Values',
       subtitle = '2010 vs 2021') +
  guides(color = guide_legend(title = '')) +
  geom_abline(intercept = 0, slope = 1) +
  scale_color_manual(values = c("darkolivegreen3", "burlywood4", "grey50"),
                     breaks = c("Added_Greenspace", "Lost_Greenspace")) +
  theme_minimal()

  # See figure below.

