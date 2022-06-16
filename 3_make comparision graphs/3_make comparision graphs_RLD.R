#=== Task ======
# Compare RLD graphs from both manual annotation and model estimation 

# Set working directory
setwd("C:/Users/Kaining/BGU ZKN/R/MR data analysis with R/from_aotocam_to_autoanalysis/from_autocam_to_autoanalysis/3_make comparision graphs")

# Load libraries
library(readxl)
library(tidyr) # make wide data long
library(dplyr)
library(xlsx) # Write data into excel
library(ggplot2)
library(export)

#=== For Manual Annotation ======

# Load data
Rootfly_TRL<- read_excel("C:/Users/Kaining/BGU ZKN/R/MR data analysis with R/from_aotocam_to_autoanalysis/from_autocam_to_autoanalysis/2_compare pred TRL with manual/compare_pred_n_manul.xlsx", 
                         sheet = "Rootfly_TRL")
View(Rootfly_TRL)

# Calculate real depth by window size (2.5cm width * 1.9cm height per window)
Rootfly_TRL <- Rootfly_TRL %>% mutate(Depth =
                                      case_when(Window <= 5 ~ "-10", 
                                                Window <= 10 ~ "-20",
                                                Window <= 15 ~ "-30",
                                                Window <= 20 ~ "-40",
                                                Window <= 25 ~ "-50",
                                                Window >= 26 ~ "-60")
)

# Convert DAP to session number
Rootfly_TRL <-Rootfly_TRL %>% mutate(Session =
                                      case_when(DAP == 5 ~ "1", 
                                                DAP == 10 ~ "2", 
                                                DAP == 15 ~ "3", 
                                                DAP == 20 ~ "4", 
                                                DAP == 25 ~ "5", 
                                                DAP == 30 ~ "6", 
                                                DAP == 35 ~ "7", 
                                                DAP == 40 ~ "8", 
                                                DAP == 45 ~ "9", 
                                                DAP == 50 ~ "10", 
                                                DAP == 55 ~ "11", 
                                                DAP == 60 ~ "12", 
                                                DAP == 65~ "13", 
                                                DAP == 70 ~ "14", 
                                                DAP == 75 ~ "15", 
                                                DAP == 80 ~ "16", 
                                                DAP == 85 ~ "17", 
                                                DAP == 90 ~ "18", 
                                                DAP == 95 ~ "19", 
                                                DAP == 100 ~ "20", 
                                                DAP == 105 ~ "21", 
                                                DAP == 110 ~ "22", 
                                                DAP == 115 ~ "23",
                                                DAP == 120 ~ "24",
                                                DAP == 125 ~ "25",
                                                DAP == 130 ~ "26"
                                      )
)

# Calculate TRL in each depth range
Rootfly_TRL<- Rootfly_TRL%>%
  group_by(DAP, Depth, Session) %>%  
  summarise_at(vars("Rootfly_TRL"), sum)

# Calculate RLD in each depth range
Rootfly_TRL<- Rootfly_TRL %>% 
  mutate(
    RLD = (Rootfly_TRL/10)/(10*2.5)
) # Calculate RLD with unit of cm/cm2

View (Rootfly_TRL)

# Redefine class of certain columns in order to plot grediant graph later
Rootfly_TRL$Session<- as.numeric(Rootfly_TRL$Session) 
Rootfly_TRL$Depth<- as.numeric(Rootfly_TRL$Depth) 

# Make RLD graph

ggplot(
  Rootfly_TRL,
  aes(
    Depth,
    RLD,
    colour = Session,
    group = Session
  )
)+
  geom_line(
    size= 1
  )+
  scale_colour_gradient2(
    name = "Session \nnumber",
    low = "white",
    mid = "red",
    high = "darkred",
    midpoint = 13
  )+
  scale_x_continuous(
    breaks = seq(-60,
                 -10,
                 by = 10
    )
  )+
  theme_classic()+
  theme(
    plot.title = element_text(
      size = 28,
      face = "bold", 
      colour = "black", 
      hjust = 0.5
    ),
    legend.title = element_text(
      size= 24
    ),
    legend.text = element_text(
      size = 24
    ),
    axis.title = element_text(
      size=24
    ),
    axis.text = element_text(
      size=24, 
      color="black"
    ),
    axis.title.x = element_text(
      margin = margin(
        t = 20, 
        r = 0, 
        b = 0, 
        l = 0)
    ),
    axis.title.y = element_text(
      margin = margin(
        t = 0, 
        r = 20, 
        b = 0, 
        l = 0)
    ),
    axis.line = element_line(
      size = 1, 
      colour = "black"
    ),
    axis.ticks = element_line(
      size = 1,
    ),
    legend.position = "right",
    legend.spacing.y = unit(0.7, 'cm'),
    legend.key.size = unit(1, 'cm'),
    axis.ticks.length = unit(0.2, "cm")
  )+
  labs(title = "Manual annotation\n", 
       x = "Depth (cm)",
       y=expression("RLD (cm/cm"^2*")")
  ) +
  coord_flip()+
  ylim(0, 1.5)

graph2ppt(file="ggplot2_plot.pptx", width=6, height=8, append = TRUE)

#=== Rooting depth ===========

# Load data
Rootfly_Rdep<- read_excel("C:/Users/Kaining/BGU ZKN/R/MR data analysis with R/from_aotocam_to_autoanalysis/from_autocam_to_autoanalysis/2_compare pred TRL with manual/compare_pred_n_manul.xlsx", 
                         sheet = "Rootfly_TRL")

Rootfly_Rdep<- Rootfly_Rdep[Rootfly_Rdep$Rootfly_TRL> 0,] # First exclude windows do not contain roots

# Calculate RLD
Rootfly_Rdep<- Rootfly_Rdep %>%
  group_by(DAP) %>%  
  summarise_at(vars("Window"), max) %>%  
  mutate(Rootfly_max_Rdep = Window * 1.9)# Then choose the the deepest depth as the maximum rooting depth

# Make graph
ggplot(
  Rootfly_Rdep,
  aes(
    DAP,
    Max_rooting_depth
  )
)+
  geom_line(
    size= 1.5,
    color="darkred"
  )+
  scale_x_continuous(
    breaks = seq(0,
                 130,
                 by = 10
    )
  )+
  theme_classic()+
  theme(
    plot.title = element_text(
      size = 28,
      face = "bold", 
      colour = "black", 
      hjust = 0.5
    ),
    axis.title = element_text(
      size=24
    ),
    axis.text = element_text(
      size=24, 
      color="black",
    ),
    axis.text.x = element_text(
     angle = 60,
    ),
    axis.title.x = element_text(
      margin = margin(
        t = 20, 
        r = 0, 
        b = 0, 
        l = 0)
    ),
    axis.title.y = element_text(
      margin = margin(
        t = 0, 
        r = 20, 
        b = 0, 
        l = 0)
    ),
    axis.line = element_line(
      size = 1, 
      colour = "black"
    ),
    axis.ticks = element_line(
      size = 1,
    ),
    axis.ticks.length = unit(0.2, "cm")
  )+
  labs(title = "Manual annotation\n", 
       x = "Days after planting",
       y=expression("Maximum rooting depth (cm)")
  ) 

# Export plot
graph2ppt(file="ggplot2_plot.pptx", width=6, height=8, append = TRUE)


#=== For Manual Annotation ======

# Load data
Rootfly_TRL<- read_excel("C:/Users/Kaining/BGU ZKN/R/MR data analysis with R/from_aotocam_to_autoanalysis/from_autocam_to_autoanalysis/2_compare pred TRL with manual/compare_pred_n_manul.xlsx", 
                         sheet = "Rootfly_TRL")
View(Rootfly_TRL)

# Calculate real depth by window size (2.5cm width * 1.9cm height per window)
Rootfly_TRL <- Rootfly_TRL %>% mutate(Depth =
                                        case_when(Window <= 5 ~ "-10", 
                                                  Window <= 10 ~ "-20",
                                                  Window <= 15 ~ "-30",
                                                  Window <= 20 ~ "-40",
                                                  Window <= 25 ~ "-50",
                                                  Window >= 26 ~ "-60")
)

# Convert DAP to session number
Rootfly_TRL <-Rootfly_TRL %>% mutate(Session =
                                       case_when(DAP == 5 ~ "1", 
                                                 DAP == 10 ~ "2", 
                                                 DAP == 15 ~ "3", 
                                                 DAP == 20 ~ "4", 
                                                 DAP == 25 ~ "5", 
                                                 DAP == 30 ~ "6", 
                                                 DAP == 35 ~ "7", 
                                                 DAP == 40 ~ "8", 
                                                 DAP == 45 ~ "9", 
                                                 DAP == 50 ~ "10", 
                                                 DAP == 55 ~ "11", 
                                                 DAP == 60 ~ "12", 
                                                 DAP == 65~ "13", 
                                                 DAP == 70 ~ "14", 
                                                 DAP == 75 ~ "15", 
                                                 DAP == 80 ~ "16", 
                                                 DAP == 85 ~ "17", 
                                                 DAP == 90 ~ "18", 
                                                 DAP == 95 ~ "19", 
                                                 DAP == 100 ~ "20", 
                                                 DAP == 105 ~ "21", 
                                                 DAP == 110 ~ "22", 
                                                 DAP == 115 ~ "23",
                                                 DAP == 120 ~ "24",
                                                 DAP == 125 ~ "25",
                                                 DAP == 130 ~ "26"
                                       )
)

# Calculate TRL in each depth range
Rootfly_TRL<- Rootfly_TRL%>%
  group_by(DAP, Depth, Session) %>%  
  summarise_at(vars("Rootfly_TRL"), sum)

# Calculate RLD in each depth range
Rootfly_TRL<- Rootfly_TRL %>% 
  mutate(
    RLD = (Rootfly_TRL/10)/(10*2.5)
  ) # Calculate RLD with unit of cm/cm2

View (Rootfly_TRL)

# Redefine class of certain columns in order to plot grediant graph later
Rootfly_TRL$Session<- as.numeric(Rootfly_TRL$Session) 
Rootfly_TRL$Depth<- as.numeric(Rootfly_TRL$Depth) 

# Make RLD graph

ggplot(
  Rootfly_TRL,
  aes(
    Depth,
    RLD,
    colour = Session,
    group = Session
  )
)+
  geom_line(
    size= 1
  )+
  scale_colour_gradient2(
    name = "Session \nnumber",
    low = "white",
    mid = "red",
    high = "darkred",
    midpoint = 13
  )+
  scale_x_continuous(
    breaks = seq(-60,
                 -10,
                 by = 10
    )
  )+
  theme_classic()+
  theme(
    plot.title = element_text(
      size = 28,
      face = "bold", 
      colour = "black", 
      hjust = 0.5
    ),
    legend.title = element_text(
      size= 24
    ),
    legend.text = element_text(
      size = 24
    ),
    axis.title = element_text(
      size=24
    ),
    axis.text = element_text(
      size=24, 
      color="black"
    ),
    axis.title.x = element_text(
      margin = margin(
        t = 20, 
        r = 0, 
        b = 0, 
        l = 0)
    ),
    axis.title.y = element_text(
      margin = margin(
        t = 0, 
        r = 20, 
        b = 0, 
        l = 0)
    ),
    axis.line = element_line(
      size = 1, 
      colour = "black"
    ),
    axis.ticks = element_line(
      size = 1,
    ),
    legend.position = "right",
    legend.spacing.y = unit(0.7, 'cm'),
    legend.key.size = unit(1, 'cm'),
    axis.ticks.length = unit(0.2, "cm")
  )+
  labs(title = "Manual annotation\n", 
       x = "Depth (cm)",
       y=expression("RLD (cm/cm"^2*")")
  ) +
  coord_flip()+
  ylim(0, 1.5)

graph2ppt(file="ggplot2_plot.pptx", width=6, height=8, append = TRUE)

#=== Rooting depth ===========

# Load data
Rootfly_Rdep<- read_excel("C:/Users/Kaining/BGU ZKN/R/MR data analysis with R/from_aotocam_to_autoanalysis/from_autocam_to_autoanalysis/2_compare pred TRL with manual/compare_pred_n_manul.xlsx", 
                          sheet = "Rootfly_TRL")

Rootfly_Rdep<- Rootfly_Rdep[Rootfly_Rdep$Rootfly_TRL> 0,] # First exclude windows do not contain roots

# Calculate RLD
Rootfly_Rdep<- Rootfly_Rdep %>%
  group_by(DAP) %>%  
  summarise_at(vars("Window"), max) %>%  
  mutate(Rootfly_max_Rdep = Window * 1.9)# Then choose the the deepest depth as the maximum rooting depth

# Make graph
ggplot(
  Rootfly_Rdep,
  aes(
    DAP,
    Rootfly_max_Rdep
  )
)+
  geom_line(
    size= 1.5,
    color="darkred"
  )+
  scale_x_continuous(
    breaks = seq(0,
                 130,
                 by = 10
    )
  )+
  theme_classic()+
  theme(
    plot.title = element_text(
      size = 28,
      face = "bold", 
      colour = "black", 
      hjust = 0.5
    ),
    axis.title = element_text(
      size=24
    ),
    axis.text = element_text(
      size=24, 
      color="black",
    ),
    axis.text.x = element_text(
      angle = 60,
    ),
    axis.title.x = element_text(
      margin = margin(
        t = 20, 
        r = 0, 
        b = 0, 
        l = 0)
    ),
    axis.title.y = element_text(
      margin = margin(
        t = 0, 
        r = 20, 
        b = 0, 
        l = 0)
    ),
    axis.line = element_line(
      size = 1, 
      colour = "black"
    ),
    axis.ticks = element_line(
      size = 1,
    ),
    axis.ticks.length = unit(0.2, "cm")
  )+
  labs(title = "Manual annotation\n", 
       x = "Days after planting",
       y=expression("Maximum rooting depth (cm)")
  ) 

# Export plot
graph2ppt(file="ggplot2_plot.pptx", width=6, height=8, append = TRUE)


#=== For Model Estimation ======

# Load data
Pred_TRL<- read_excel("C:/Users/Kaining/BGU ZKN/R/MR data analysis with R/from_aotocam_to_autoanalysis/from_autocam_to_autoanalysis/2_compare pred TRL with manual/compare_pred_n_manul.xlsx", 
                      sheet = "Pred_TRL")
View(Pred_TRL)

# Calculate real depth by window size (2.5cm width * 1.9cm height per window)
Pred_TRL <- Pred_TRL %>% mutate(Depth =
                                        case_when(Window <= 5 ~ "-10", 
                                                  Window <= 10 ~ "-20",
                                                  Window <= 15 ~ "-30",
                                                  Window <= 20 ~ "-40",
                                                  Window <= 25 ~ "-50",
                                                  Window >= 26 ~ "-60")
)

# Convert DAP to session number
Pred_TRL <- Pred_TRL %>% mutate(Session =
                                       case_when(DAP == 5 ~ "1", 
                                                 DAP == 10 ~ "2", 
                                                 DAP == 15 ~ "3", 
                                                 DAP == 20 ~ "4", 
                                                 DAP == 25 ~ "5", 
                                                 DAP == 30 ~ "6", 
                                                 DAP == 35 ~ "7", 
                                                 DAP == 40 ~ "8", 
                                                 DAP == 45 ~ "9", 
                                                 DAP == 50 ~ "10", 
                                                 DAP == 55 ~ "11", 
                                                 DAP == 60 ~ "12", 
                                                 DAP == 65~ "13", 
                                                 DAP == 70 ~ "14", 
                                                 DAP == 75 ~ "15", 
                                                 DAP == 80 ~ "16", 
                                                 DAP == 85 ~ "17", 
                                                 DAP == 90 ~ "18", 
                                                 DAP == 95 ~ "19", 
                                                 DAP == 100 ~ "20", 
                                                 DAP == 105 ~ "21", 
                                                 DAP == 110 ~ "22", 
                                                 DAP == 115 ~ "23",
                                                 DAP == 120 ~ "24",
                                                 DAP == 125 ~ "25",
                                                 DAP == 130 ~ "26"
                                       )
)

# Calculate TRL in each depth range
Pred_TRL<- Pred_TRL%>%
  group_by(DAP, Depth, Session) %>%  
  summarise_at(vars("Pred_TRL"), sum)

# Calculate RLD in each depth range
Pred_TRL<- Pred_TRL %>% 
  mutate(
    RLD = (Pred_TRL/10)/(10*2.5)
  ) # Calculate RLD with unit of cm/cm2

View (Pred_TRL)

# Redefine class of certain columns in order to plot grediant graph later
Pred_TRL$Session<- as.numeric(Pred_TRL$Session) 
Pred_TRL$Depth<- as.numeric(Pred_TRL$Depth) 

# Make RLD graph

ggplot(
  Pred_TRL,
  aes(
    Depth,
    RLD,
    colour = Session,
    group = Session
  )
)+
  geom_line(
    size= 1
  )+
  scale_colour_gradient2(
    name = "Session \nnumber",
    low = "white",
    mid = "blue",
    high = "darkblue",
    midpoint = 13
  )+
  scale_x_continuous(
    breaks = seq(-60,
                 -10,
                 by = 10
    )
  )+
  theme_classic()+
  theme(
    plot.title = element_text(
      size = 28,
      face = "bold", 
      colour = "black", 
      hjust = 0.5
    ),
    legend.title = element_text(
      size= 24
    ),
    legend.text = element_text(
      size = 24
    ),
    axis.title = element_text(
      size=24
    ),
    axis.text = element_text(
      size=24, 
      color="black"
    ),
    axis.title.x = element_text(
      margin = margin(
        t = 20, 
        r = 0, 
        b = 0, 
        l = 0)
    ),
    axis.title.y = element_text(
      margin = margin(
        t = 0, 
        r = 20, 
        b = 0, 
        l = 0)
    ),
    axis.line = element_line(
      size = 1, 
      colour = "black"
    ),
    axis.ticks = element_line(
      size = 1,
    ),
    legend.position = "right",
    legend.spacing.y = unit(0.7, 'cm'),
    legend.key.size = unit(1, 'cm'),
    axis.ticks.length = unit(0.2, "cm")
  )+
  labs(title = "Model estimation\n", 
       x = "Depth (cm)",
       y=expression("RLD (cm/cm"^2*")")
  ) +
  coord_flip()+
  ylim(0, 1.5)

graph2ppt(file="ggplot2_plot.pptx", width=6, height=8, append = TRUE)

#=== Rooting depth ===========

# Load data
Pred_Rdep <- read_excel("C:/Users/Kaining/BGU ZKN/R/MR data analysis with R/from_aotocam_to_autoanalysis/from_autocam_to_autoanalysis/2_compare pred TRL with manual/compare_pred_n_manul.xlsx", 
                      sheet = "Pred_TRL")

Pred_Rdep<- Pred_Rdep[Pred_Rdep$Pred_TRL> 6,] # Set threshold as 6 mm when actual TRL=0

# Calculate RLD
Pred_Rdep<- Pred_Rdep %>%
  group_by(DAP) %>%  
  summarise_at(vars("Window"), max) %>%  
  mutate(Pred_max_Rdep = Window * 1.9)# Then choose the the deepest depth as the maximum rooting depth

# Make graph
ggplot(
  Pred_Rdep,
  aes(
    DAP,
    Pred_max_Rdep
  )
)+
  geom_line(
    size= 1.5,
    color="darkblue"
  )+
  scale_x_continuous(
    breaks = seq(0,
                 130,
                 by = 10
    )
  )+
  theme_classic()+
  theme(
    plot.title = element_text(
      size = 28,
      face = "bold", 
      colour = "black", 
      hjust = 0.5
    ),
    axis.title = element_text(
      size=24
    ),
    axis.text = element_text(
      size=24, 
      color="black",
    ),
    axis.text.x = element_text(
      angle = 60,
    ),
    axis.title.x = element_text(
      margin = margin(
        t = 20, 
        r = 0, 
        b = 0, 
        l = 0)
    ),
    axis.title.y = element_text(
      margin = margin(
        t = 0, 
        r = 20, 
        b = 0, 
        l = 0)
    ),
    axis.line = element_line(
      size = 1, 
      colour = "black"
    ),
    axis.ticks = element_line(
      size = 1,
    ),
    axis.ticks.length = unit(0.2, "cm")
  )+
  labs(title = "Model estimation\n", 
       x = "Days after planting",
       y=expression("Maximum rooting depth (cm)")
  ) 

# Export plot
graph2ppt(file="ggplot2_plot.pptx", width=6, height=8, append = TRUE)




