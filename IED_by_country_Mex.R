# Libraries 
library("tidyverse") # Toolkit for data manipulation
library("readxl") # For the Excel formats 
library("gganimate") # For gif animation
library("gifski") # Render
library("png")

theme_set(theme_bw()) # For theme of graphics

# Data from the "Secretaría de Economía" from Mexican government
setwd("~/R/Data")
ied <- as.data.frame(read_excel("flujosporpaisdeorigen.xlsx", 
                              sheet = "Por tipo de inversión",
                              skip = 1)
                     )

# I select only the total amount by year
columns_selected <- sapply(seq(1999,2022, by = 1), as.character)
columns_selected <- paste("Total", columns_selected)
columns_selected <- c("...1",columns_selected)

# Only the total amount by country
ied <- ied %>% 
  select(columns_selected) %>% 
  rename("país" = "...1") %>% 
  filter(!(país %in% c("Nuevas inversiones",
                       "Reinversión de utilidades",
                       "Cuentas entre compañías")
           )
         )

# Delete NA, in this case, empty rows
ied <- drop_na(ied)

# Replace the letter "C" (for "Confidencial" in spanish) and remplace them by NAs values
ied[ied == "C"] <- NA

# Only need the values for countries, then we remove the total amount by year
ied_by_country <- ied %>% 
  filter(país != "Total")

# For this case, to make the gif more easy, transform the data containing the 
# variables of the plot un a table like year, country and amount in the columns

  # Names of the countries
country <- c()
for (i in ied_by_country["país"]){
  country[i] <- i
  rm(i)
}

  # gif data frame
gif_data_frame <- as.data.frame(c())

for (i in country){
  
  x <- ied_by_country %>% 
    filter(país == i)
  x <- x[,-1]
  
  c <- rep(i, times = length(x)
           )
  
  y <- c()
  for (i in x){
    y[i] <- as.numeric(i)
    rm(i)
  }
  y <- as.data.frame(cbind(seq(1999,2022,by=1
                               ),
                           c,
                           y
                           )
                     )
  
  colnames(y) <- c("year","country","amount")
  rownames(y) <- 1:nrow(y)
  
  gif_data_frame <- rbind(gif_data_frame,y)
  
  rm(c,y)
}

  # Change the format of the variables
gif_data_frame["year"] <- lapply(gif_data_frame["year"], as.numeric)
gif_data_frame["amount"] <- lapply(gif_data_frame["amount"], as.numeric)

rm(ied, ied_by_country, x, columns_selected, country)

gif_data_frame <- gif_data_frame %>% 
  group_by(year) %>% 
  mutate(rank = rank(-amount),
         Amount_rel = amount/amount[rank == 1],
         Amount_lbl = paste0(" ", round(amount))) %>% 
  group_by(country) %>% 
  filter(rank <= 10) %>% 
  ungroup()

gif_data_frame["country"][gif_data_frame["country"] == "Alemania"] <- "Germany"
gif_data_frame["country"][gif_data_frame["country"] == "Argentina"] <- "Argentina"
gif_data_frame["country"][gif_data_frame["country"] == "Australia"] <- "Australia"
gif_data_frame["country"][gif_data_frame["country"] == "Bélgica"] <- "Belgium"
gif_data_frame["country"][gif_data_frame["country"] == "Brasil"] <- "Brazil"
gif_data_frame["country"][gif_data_frame["country"] == "Canadá"] <- "Canada"
gif_data_frame["country"][gif_data_frame["country"] == "Colombia"] <- "Colombia"
gif_data_frame["country"][gif_data_frame["country"] == "Corea, República de"] <- "Republic of Korea"
gif_data_frame["country"][gif_data_frame["country"] == "Dinamarca"] <- "Denmark"
gif_data_frame["country"][gif_data_frame["country"] == "España"] <- "Spain"
gif_data_frame["country"][gif_data_frame["country"] == "Estados Unidos de América"] <- "USA"
gif_data_frame["country"][gif_data_frame["country"] == "Finlandia"] <- "Finland"
gif_data_frame["country"][gif_data_frame["country"] == "Francia"] <- "France"
gif_data_frame["country"][gif_data_frame["country"] == "Hong Kong"] <- "Hong Kong"
gif_data_frame["country"][gif_data_frame["country"] == "Irlanda"] <- "Ireland"
gif_data_frame["country"][gif_data_frame["country"] == "Israel"] <- "Israel"
gif_data_frame["country"][gif_data_frame["country"] == "Italia"] <- "Italy"
gif_data_frame["country"][gif_data_frame["country"] == "Japón"] <- "Japan"
gif_data_frame["country"][gif_data_frame["country"] == "Luxemburgo"] <- "Luxembourg"
gif_data_frame["country"][gif_data_frame["country"] == "Noruega"] <- "Norway"
gif_data_frame["country"][gif_data_frame["country"] == "Países Bajos"] <- "Netherlands"
gif_data_frame["country"][gif_data_frame["country"] == "Reino Unido de la Gran Bretaña e Irlanda del Norte"] <- "United Kingdom"
gif_data_frame["country"][gif_data_frame["country"] == "Suecia"] <- "Sweden"
gif_data_frame["country"][gif_data_frame["country"] == "Suiza"] <- "Switzerland"
gif_data_frame["country"][gif_data_frame["country"] == "Taiwan"] <- "Taiwan"
gif_data_frame["country"][gif_data_frame["country"] == "Otros países"] <- "Other countries"
str(gif_data_frame)

# Making the gif
  # Static plot
plot <-  ggplot(gif_data_frame, aes(rank, group = country, 
                                       fill = as.factor(country), color = as.factor(country))) +
  geom_tile(aes(y = amount/2,
                height = amount,
                width = 0.9), alpha = 1, color = NA) +
  geom_text(aes(y = 0, label = paste(country, " ")), vjust = 0.2, hjust = 1) +
  geom_text(aes(y= amount, label = Amount_lbl, hjust = 0)) +
  coord_flip(clip = "off", expand = FALSE) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_reverse() +
  guides(color = FALSE, fill = FALSE) +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none",
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.grid.major.x = element_line( size=.1, color="grey" ),
        panel.grid.minor.x = element_line( size=.1, color="grey" ),
        plot.title=element_text(size=25, hjust=0.5, face="bold", colour="grey", vjust=-1),
        plot.subtitle=element_text(size=18, hjust=0.5, face="italic", color="grey"),
        plot.caption =element_text(size=8, hjust=0.5, face="italic", color="grey"),
        plot.background=element_blank(),
        plot.margin = margin(2,2, 2, 4, "cm"))
plot

  # Animation
setwd("~/R/Programs/Figures/foering_investing_mex")

foering_invest_mex <- plot + transition_states(year, transition_length = 8, state_length = 1) +
  view_follow(fixed_x = TRUE)  +
  labs(title = "Foering Investment by Country or Origin, Mexico : {closest_state}",  
       subtitle  =  "Top 10 Countries",
       caption  = "Millions USD | Data Source:  Secretaría de Economía, Mexico")

  # Rendering
animate(foering_invest_mex, 200, duration = 25, fps = 25,  width = 800, height = 700, 
        renderer = gifski_renderer())

anim_save("foering_invest_mex.gif")