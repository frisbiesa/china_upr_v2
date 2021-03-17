library(tidyverse)
library(gganimate)
library(magick)

"C:/Users/nico/Desktop/Harris/Winter 2021/Data Programming II/Fortunato-Frisbie-final-project"


c_f <- read.csv("chinese_finance.csv")
debt <- read.csv("debt_stock_china.csv")


debt$debt_usd <- str_remove_all(debt$debt_usd, ",")
debt$debt_usd <- as.integer(debt$debt_usd)


ranked_data <-  debt %>%
  group_by(year) %>%
  mutate(rank = min_rank(-debt_usd)*1, #for each year, rank the department by the budget (the - makes it desc)
         Value_lbl = paste0(" ", country, ": ", format(debt_usd, big.mark = ",", scientific = F))) %>%
  filter(rank <= 10) %>%
  ungroup() 

#creating the anim object (which is a combo of ggplot and gganimate)
anim <- ggplot(ranked_data, aes( #remeber that the x and y axis will be flipped
  x = rank, #the rank is what creates the order for the bars. there should be 10 per year/frame
  y = country,#the bars should be organized around department
  label = country,
  group = country,
  fill = country
) #groups determine OBJECT PERMENANCE for gganimate. 
#this is very important to ensure that the same bar does not transition in and out 
) +
  geom_tile(
    aes(
      y = debt_usd/2, #unclear as to why but the y value MUST be the height variable/2
      height = debt_usd,
      width = 0.9,
      fill = country # determine the color of the bars
    ), 
    alpha = 0.8, show.legend = TRUE)+
  geom_text(aes( 
    y = debt_usd, #the labels need to be mapped to the budget bar 
    label = Value_lbl, #the label variable 
    #color = ifelse(Budget > 600000, "#ffffff", "#000000"),
    hjust = 1 # if the budget is large, we want the text to be in the bar instead of outside the bar
  ) #end of aes
  ) + #end of geom_text
  coord_flip(clip = "off", expand = FALSE) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_reverse() +
  theme_minimal()+ # removes a lot of plot elements
  theme(
    plot.title = element_text(color = "#01807e", face = "bold", hjust = 0, size = 30),
    axis.ticks.y = element_blank(), #removes axis ticks
    axis.text.y = element_blank(), #removes axis ticks
    panel.grid.major = element_blank(), #removes grid lines
    panel.grid.minor = element_blank(), #removes grid lines
    legend.position = "bottom",
    legend.justification = "left"
  )+
  labs(title = "{closest_state}",#shows the closest state (year) for that frame
       subtitle = "Total debt to China",
       y = "",
       x = "",
       caption = "Source: ")+
  
  #this part provides the actual animation
  transition_states(Year, #what variable is going to be the frames
                    transition_length = 4, #relative length of transition frames
                    state_length = 1, #relative length of state frames
                    wrap = TRUE) + #should the gif go back to the start at the end
  ease_aes("cubic-in-out") #how do new elements transition in

#now that we have the anim object, we need to render it
animate(anim, #more frames for make it smoother but longer to render
        fps = 15,
        #how many frames are shown per second
        renderer = magick_renderer()
        )

q <- ggplot(ranked_data, aes(-rank, debt_usd, fill = country)) +
  geom_col(width = 0.8, position = "identity") +
  coord_flip() +
  geom_text(aes(-rank,y=0,label = country, hjust=0)) +       #country label
  geom_text(aes(-rank, y= debt_usd,label = Value_lbl, hjust=0)) + # value label
  theme_minimal() +
  theme(legend.position = "none",axis.title = element_blank()) +
  # animate along Year
  transition_states(Year,4,1)

animate(q, 100, fps = 25, duration = 20, width = 800, height = 600)
  
  

