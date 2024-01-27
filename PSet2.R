### Problem Set 2
  # Due Jan 31, 2024

# 1 ----

 # include link to repo



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
knitr::kable(gspace, digits = 1) # added library knitr to access kable function



# 3 ----

  # 1038



# 4 ----

gs2021 = gspace[c(23:27)]
s2021 = summarize(gs2021,
                  annual_avg = mean(annual_avg_2021, na.rm = T),
                  peak_NDVI = mean(peak_NDVI_2021, na.rm = T),
                  annual_weight_avg = mean(annual_weight_avg_2021, na.rm = T),
                  peak_weight = mean(peak_weight_2021, na.rm = T))



# 5 ----

## a ----
high2015 = filter(gspace, indicator_2015 == 'High')
count(high2015)
  # 62

## b ----
exlow = filter(gspace,
               indicator_2010 == 'Exceptionally Low' |
                 indicator_2015 == 'Exceptionally Low' |
                 indicator_2020 == 'Exceptionally Low' |
                 indicator_2021 == 'Exceptionally Low')
count(exlow)
  # 240

## c ----
less_arid = filter(gspace, 
              Climate_region == "Arid" & 
                annual_weight_avg_2010 < annual_weight_avg_2020)
count(less_arid)
  # 225
arid_check = select(less_arid, annual_weight_avg_2010, annual_weight_avg_2020)



# 6 ----

less_green = filter(gspace, annual_avg_2010 > annual_avg_2021)
count(less_green)
  # 128

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
                 fill = 'chartreuse4',
                 color = 'black') +
  labs(x = 'Change in Annual Average',
       y = 'Count',
       title = 'Change in Greenspace from 2010 to 2021') +
  theme_minimal()



# 8 ----

scatter = ggplot(gspace)+
  geom_point(aes(x = annual_weight_avg_2010,
                 y = annual_weight_avg_2021),
             color = 'hotpink3',
             shape = 1) +
  labs(x = '2010',
       y = '2021',
       title = 'Population Weighted Annual NDVI as Mean of Seasonal NDVI Values',
       subtitle = '2010 vs 2021') +
  theme_minimal()


## Extra credit ----
gspace<-mutate(gspace,
                change3=case_when(
                  between(change2,0,1) ~ 'Added_Greenspace',
                  between(change2,-1,0) ~ 'Lost_Greenspace'))

scatterec = ggplot(gspace)+
  geom_point(aes(x = annual_weight_avg_2010,
                 y = annual_weight_avg_2021,
                 color = change3),
             shape = 1) +
  labs(x = '2010',
       y = '2021',
       title = 'Population Weighted Annual NDVI as Mean of Seasonal NDVI Values',
       subtitle = '2010 vs 2021') +
  guides(color = guide_legend(title = '')) +
  geom_abline(intercept = 0, slope = 1) +
  scale_color_manual(values = c("chartreuse4", "tan4", "grey50"),
                     breaks = c("Added_Greenspace", "Lost_Greenspace")) +
  theme_minimal()



