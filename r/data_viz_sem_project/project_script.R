library(tidyverse)
library(lubridate)
library(RColorBrewer)

##### EDA #####
results <- read_csv("results.csv")
#View(results)
constructors <- read_csv("constructors.csv")
#View(constructors)
drivers <- read_csv("drivers.csv")     
#View(drivers)
status.lookup <- read_csv("status.csv")
#View(status.lookup)
pit_stops <- read_csv("pit_stops.csv")
#View(pit_stops)
races <- read_csv("races.csv")
#View(races)

drivers %>% 
  select(driverId, driverRef) -> driver.lookup

constructors %>%
  select(constructorId, constructorRef, own_engine) -> constructor.lookup

last_grid <- c('hamilton', 'bottas',
                  'vettel', 'leclerc',
                  'albon', 'max_verstappen',
                  'norris', 'sainz',
                  'ricciardo', 'ocon',
                  'gasly', 'kvyat',
                  'perez', 'stroll',
                  'raikkonen', 'giovinazzi',
                  'grosjean', 'kevin_magnussen',
                  'latifi', 'russell')

last_constructor <- c('mercedes', 'red_bull',
                      'mclaren', 'racing_point',
                      'renault', 'ferrari',
                      'alphatauri', 'alfa',
                      'haas', 'williams')

results %>% 
  inner_join(.,driver.lookup) %>% 
  inner_join(.,constructor.lookup) -> driver_constructor_results

##### Engine by Constructor vs Purchased by Finishing Position #####

        # methodology - look up via wiki links provided
        # if constructor has done both, the last/current phase is recorded

driver_constructor_results %>% 
  group_by(constructorRef, driverRef, own_engine) %>% 
  mutate(fastestLapSpeed = as.numeric(fastestLapSpeed), position = as.numeric(position)) %>% 
  filter(!is.na(fastestLapSpeed)) %>% 
  filter(!is.na(position)) %>% 
ggplot() +
  geom_jitter(aes(position, fastestLapSpeed, color = own_engine))+
  ggtitle('Finishing Position vs Fastest Lap Speed by Engine Constructors vs Engine Purchasers', subtitle = 'Engine constructors tend to finish in a better position and generally earn slightly faster lap speeds than their counterparts')+
  theme_minimal()+
  xlab('Finishing Position')+
  ylab('Fastest Lap Speed')+
  scale_color_viridis_d()+
  labs(caption = 'Source: Vopani | Kaggle')

#ggsave("f1_positionvsfastestlap.pdf")

##### Engine by Constructor vs Purchased by Podium Position in 2020#####

driver_constructor_results %>% 
  mutate(own_engine = own_engine == "Y") %>% 
  group_by(constructorRef, driverRef, own_engine) %>% 
  mutate(position = as.numeric(position)) %>% 
  filter(!is.na(fastestLapSpeed)) %>% 
  filter(!is.na(position)) %>% 
  filter(position <= 3) %>% 
  filter(constructorRef %in% last_constructor) %>% 
  filter(driverRef %in% last_grid) %>% 
ggplot() + 
  geom_bar(aes(position, fill = own_engine), position = 'dodge')+
  ggtitle('Podium Positions Won in 2020 by Engine Constructors', subtitle = 'Constructors who build and use their own engines consistently finish on the podium more often than those who purchase their engines')+
  theme_minimal()+
  xlab('Finishing Position')+
  ylab('Total')+
  scale_fill_viridis_d()+
  labs(caption = 'Source: Vopani | Kaggle')

#ggsave("f1_podiumconstructors.pdf")

##### Linear model of total wins by engine manufacturers  #####

races %>% 
  select("raceId", "year") -> race_year_lookup

races %>% count(vars = year) -> race_per_year

driver_constructor_results %>%
  inner_join(.,race_year_lookup) %>%
  mutate(own_engine = own_engine == "Y") %>%
  filter(position %in% c('1', '2', '3')) %>%
  mutate(position = as.numeric(position)) %>% 
  filter(!is.na(position)) %>% 
  filter(own_engine == T) %>% 
  group_by(own_engine, year) %>% 
  summarise(totalT = sum(position)) -> wins_by_manufacture

driver_constructor_results %>%
  inner_join(.,race_year_lookup) %>%
  mutate(own_engine = own_engine == "Y") %>%
  filter(position %in% c('1', '2', '3')) %>%
  mutate(position = as.numeric(position)) %>% 
  filter(!is.na(position)) %>% 
  filter(own_engine == F) %>% 
  group_by(own_engine, year) %>% 
  summarise(totalF = sum(position)) -> wins_by_nonmanufacture
  
wins_by_manufacture %>% 
  inner_join(wins_by_nonmanufacture, by = c('year' = 'year')) %>% 
  select(year,totalT,totalF) %>% 
  mutate(total = totalT + totalF) %>% 
  mutate(proportionT = totalT/total) %>%
ggplot()+
  geom_line(aes(year, proportionT), color = rgb(0, 222, 214, maxColorValue = 255), lwd = 1)+
  ggtitle('Total Race Wins by Engine Manufacturers Since 1950', subtitle = "Constructors who build and use their own engines have regularly been competitive in F1, with a drastic increase in performance over the last 20 seasons") +
  theme_minimal()+
  ylab('Total Race Wins')+
  xlab('')+
  labs(caption = 'Source: Vopani | Kaggle')

#ggsave("f1_winenginemaker.pdf")
              
              
##### Engine by Constructor vs Purchased by Points per Constructor #####

driver_constructor_results %>% 
  left_join(.,race_year_lookup) -> driver_constructor_results_year

driver_constructor_results_year %>%
  filter(year == '2020') %>% 
  mutate(points = as.numeric(points)) %>% 
  filter(!is.na(points)) %>%
  ungroup() %>% 
  group_by(constructorRef, driverRef, own_engine) %>%
  filter(constructorRef %in% last_constructor, driverRef %in% last_grid) %>% 
  summarise(sum_points = sum(points)) %>%
  arrange(sum_points) -> stored_points_2020

stored_points_2020$driverRef[5] <- "russell_merc"
driver_order <- stored_points_2020$driverRef

driver_order

stored_points_2020 %>% 
  mutate(driverRef = factor(driverRef, levels = driver_order)) %>% 
ggplot()+
  geom_col(aes(driverRef, sum_points, fill = own_engine)) +
  scale_fill_viridis_d()+
  coord_flip() +
  ggtitle("Total Points per Driver on the 2020 Grid", subtitle = 'A majority of the points in 2020 were won by drivers who drove vehicles with the engine produced by their own constructor') +
  theme_minimal() +
  xlab("Driver")+
  ylab("Total Points Earned")+
  labs(caption = 'Source: Vopani | Kaggle')

#ggsave('f1_pointsperdriver.pdf')



##### __NOT USED__ GGANIMATE#####
# library(av)
# library(gifski)
# library(gganimate)
# 
# animate <- driver_constructor_results %>%
#   inner_join(.,race_year_lookup) %>% 
#   mutate(own_engine = own_engine == "Y") %>% 
#   group_by(constructorRef, driverRef, own_engine) %>% 
#   filter(position %in% c('1', '2', '3')) %>%
#   filter(own_engine == T) %>% 
#   mutate(position = as.numeric(position)) %>% 
#   filter(!is.na(position)) %>% 
#   ungroup() %>% 
#   group_by(own_engine, year) %>%
#   summarise(total = sum(position)) %>% 
#   ggplot()+
#   geom_line(aes(year, total), color = rgb(0, 222, 214, maxColorValue = 255), lwd = 1)+
#   ggtitle('Total Race Wins by Engine Manufacturers Since 1950', subtitle = "Constructors who build and use their own engines have regularly been competitive in F1, with a drastic increase in performance over the last 20 seasons") +
#   theme_minimal()+
#   ylab('Total Race Wins')+
#   xlab('')+
#   labs(caption = 'Source: Kaggle (Vopani)')+
#   #gganimate starts here
#   transition_reveal(year)
# 
# animate(animate, width = 852, height = 480, renderer = gifski_renderer())
# anim_save("revealline.gif",path = "~/School/Syracuse/Jr - Sem 2/IST 421/f1_data/anim")
# 
# 
# animate2 <- driver_constructor_results %>% 
#   group_by(constructorRef, driverRef, own_engine) %>% 
#   mutate(fastestLapSpeed = as.numeric(fastestLapSpeed), position = as.numeric(position)) %>% 
#   filter(!is.na(fastestLapSpeed)) %>% 
#   filter(!is.na(position)) %>%
#   ggplot() +
#   geom_jitter(aes(position, fastestLapSpeed, color = own_engine))+
#   ggtitle('Finishing Position vs Fastest Lap Speed by Engine Constructors vs Engine Purchasers', subtitle = 'Engine constructors tend to finish in a better position and generally earn slightly faster lap speeds than their counterparts')+
#   theme_minimal()+
#   xlab('Finishing Position')+
#   ylab('Fastest Lap Speed')+
#   scale_color_manual('Engine', values = c(rgb(245, 111, 114, maxColorValue = 255), rgb(0, 222, 214, maxColorValue = 255)),
#                      labels = c("Purchased",'Built'))+
#   labs(caption = 'Source: Kaggle (Vopani)')+
#   #gganimate starts here
#   transition_states(own_engine,transition_length = .5, state_length = 1)+
#   enter_fade()+
#   exit_fade()
# 
# animate(animate2, width = 852, height = 480, renderer = gifski_renderer())
# anim_save("fade_state.gif",path = "~/School/Syracuse/Jr - Sem 2/IST 421/f1_data/anim")

##### Built vs Constructed by Grid vs Position in 2020 #####
driver_constructor_results %>% 
  filter(constructorRef %in% last_constructor) %>%
  filter(driverRef %in% last_grid) %>% 
  group_by(constructorRef, driverRef, own_engine) %>% 
  mutate(grid = as.numeric(grid)) %>% 
  filter(!is.na(grid)) %>%
  mutate(position = as.numeric(position)) %>% 
  filter(!is.na(position)) -> dcr_2020
  dcr_2020 %>% 
    ungroup() %>% 
    select(grid, position, own_engine) %>% 
    filter(grid>0) %>% 
    group_by(grid,position) %>% 
    summarise(total_count=n()) -> dcr_totalgp
  dcr_2020 %>% 
    ungroup() %>% 
    select(grid, position, own_engine) %>% 
    filter(grid>0) %>% 
    group_by(grid,position,own_engine) %>% 
    summarise(count=n()) %>% 
    ungroup() %>% 
    full_join(.,dcr_totalgp) %>% 
    mutate(pct = count/total_count) %>% 
    distinct(grid,position, .keep_all = T) -> dcr_pct
    
  dcr_pct$fixedpct <- if_else(dcr_pct$own_engine=="N",1-dcr_pct$pct,dcr_pct$pct)

  dcr_pct %>% 
  ggplot()+
    geom_tile(aes(grid,position,fill=fixedpct))+
    scale_fill_viridis_c()+
    theme_minimal()+
    xlab('Starting Position')+
    ylab('Finishing Position')+
    labs(fill="% Built Engine",caption = 'Source: Vopani | Kaggle')+
    ggtitle("Starting vs Finishing Position in 2020", subtitle = "Constructors who build their own engines tended to both start and finish better than those who did not")
  
  #ggsave("heatmap_engineconstructor.pdf")
  
##### Drivers on Own Engine Teams vs Not#####
  
  driver_constructor_results_year %>% 
    #filter(driverRef %in% last_grid) %>%
    #filter(constructorRef %in% last_constructor) %>%
    #filter(year>=2020) %>%
    mutate(grid = as.numeric(grid)) %>% 
    filter(!is.na(grid)) %>%
    mutate(position = as.numeric(position)) %>% 
    filter(!is.na(position)) %>% 
    filter(position== 1) %>%
    group_by(constructorRef, driverRef, own_engine) %>%
    summarise(count = n()) %>% 
    ungroup() %>% 
    group_by(driverRef) %>%
    filter(n() > 1) -> repeat_place
    
  repeat_place %>% 
    select(driverRef,own_engine) %>% 
    filter(own_engine=="N") %>% 
    select(driverRef) -> rpn
  
  repeat_place %>% 
    select(driverRef,own_engine) %>% 
    filter(own_engine=="Y") %>% 
    select(driverRef) -> rpy
  
  rpconsolidated <- inner_join(rpn,rpy) %>% 
      distinct() %>%
      pull(driverRef)
  
  repeat_place %>% 
    filter(driverRef %in% rpconsolidated) %>%
    summarise(total = sum(count)) -> totalplace
  
  repeat_place %>% 
    filter(driverRef %in% rpconsolidated) %>% 
    ungroup() %>% 
    select(-constructorRef) %>%
    filter(own_engine=="Y") %>% 
    left_join(.,totalplace) %>% 
    group_by(driverRef,total) %>% 
    summarise(count_y=sum(count)) %>%
    mutate(count_n = total-count_y) %>% 
    mutate(check = count_y + count_n - total) %>%
    mutate(diff = count_y-count_n) %>%
    arrange(-diff) %>% 
    mutate(built = diff>0) %>% 
    filter(diff!=0) -> driver_success_desc
    
  driver_success_desc %>% 
    mutate(driverRef = factor(driverRef, levels=driver_success_desc$driverRef)) %>% 
  ggplot()+
    geom_bar(aes(driverRef,diff, fill=built),stat='identity')+
    scale_fill_viridis_d()+
    theme_minimal()+
    theme(axis.text.x = element_text(angle = 45))+
    theme(axis.text.x = element_text(hjust=1.0))+
    theme(axis.text.x = element_text(vjust=1.05))+
    xlab('Driver')+
    ylab('Differential')+
    labs(fill="Constructor-Built Engine",caption = 'Source: Vopani | Kaggle')+
    ggtitle("Number of First Places by Driver", subtitle = "Though the overall amount of constructor-built wins (303) is greater than non-constructor-built wins (276), the drivers' affect cannot be discounted")
  
  #ggsave("driver_effect.pdf")
  
  