#=== Task ======
# Compare RLD graphs from both manual annotation and model estimation 
# Graphs:
# 1. RLD in different depth change with time
# 2. Root length density
# 3. Compare two dataset with scattered graphs

# Set working directory
setwd("C:/Users/Kaining/BGU ZKN/R/MR data analysis with R/from_aotocam_to_autoanalysis/from_autocam_to_autoanalysis/3_make comparision graphs")

# Load libraries
library(readxl)
library(tidyr) # make wide data long
library(dplyr)
library(xlsx) # Write data into excel
library(ggplot2)
library(export)

#===========================
#=== For Rootfly data ======
#===========================

# Load data

R_Rootfly<- read_excel("C:/Users/Kaining/BGU ZKN/R/MR data analysis with R/from_aotocam_to_autoanalysis/from_autocam_to_autoanalysis/2_compare pred TRL with manual/compare_pred_n_manul.xlsx", 
                         sheet = "Rootfly_TRL") # This is the data from Rootfly
View(R_Rootfly)

# Calculate real depth by window size (2.5cm width * 1.9cm height per window)
Rootfly_TRL <- R_Rootfly %>% mutate(Depth =
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
    axis.ticks.length = unit(0.2, "cm"),
    axis.ticks = element_line(
      size = 1
    ),
    legend.position = "right",
    legend.spacing.y = unit(0.7, 'cm'),
    legend.key.size = unit(1, 'cm')
  )+
  labs(title = "Manual annotation\n", 
       x = "Depth (cm)",
       y=expression("RLD (cm/cm"^2*")")
  ) +
  coord_flip()+
  ylim(0, 1.5)

graph2ppt(file="ggplot2_plot.pptx", width=6, height=8, append = TRUE)

#=== Rooting depth ===========

Rootfly_Rdep<- R_Rootfly[R_Rootfly$Rootfly_TRL> 0,] # First exclude windows do not contain roots

Rootfly_Rdep<- Rootfly_Rdep %>%
  group_by(DAP) %>%  
  summarise_at(vars("Window"), max) %>%  
  mutate(Max_rooting_depth = Window * 1.9)# Then choose the the deepest depth as the maximum rooting depth

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
      color="black"
    ),
    axis.text.x = element_text(
      size=24, 
      color="black",
      angle = 60
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
    axis.ticks.length = unit(0.2, "cm"),
    axis.ticks = element_line(
      size = 1
    )
  )+
  labs(title = "Manual annotation\n",
       x = "Days after planting",
       y="Maximum rooting depth (cm)"
  )

# Export graph
graph2ppt(file="ggplot2_plot.pptx", width=6, height=8, append = TRUE)

#====================================
#=== For model estimation data ======
#====================================

# Load data

R_Model<- read_excel("C:/Users/Kaining/BGU ZKN/R/MR data analysis with R/from_aotocam_to_autoanalysis/from_autocam_to_autoanalysis/2_compare pred TRL with manual/compare_pred_n_manul.xlsx", 
                     sheet = "Pred_TRL") # This is the data from model estimation
View(R_Model)

# Calculate real depth by window size (2.5cm width * 1.9cm height per window)
Model_TRL <- R_Model %>% mutate(Depth =
                                      case_when(Window <= 5 ~ "-10", 
                                                Window <= 10 ~ "-20",
                                                Window <= 15 ~ "-30",
                                                Window <= 20 ~ "-40",
                                                Window <= 25 ~ "-50",
                                                Window >= 26 ~ "-60")
)

# Convert DAP to session number
Model_TRL <-Model_TRL %>% mutate(Session =
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
Model_TRL<- Model_TRL%>%
  group_by(DAP, Depth, Session) %>%  
  summarise_at(vars("Pred_TRL"), sum)

# Calculate RLD in each depth range
Model_TRL<- Model_TRL %>% 
  mutate(
    RLD = (Pred_TRL/10)/(10*2.5)
  ) # Calculate RLD with unit of cm/cm2

View (Model_TRL)

# Redefine class of certain columns in order to plot grediant graph later
Model_TRL$Session<- as.numeric(Model_TRL$Session) 
Model_TRL$Depth<- as.numeric(Model_TRL$Depth) 

# Make RLD graph
ggplot(
  Model_TRL,
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
    axis.ticks.length = unit(0.2, "cm"),
    axis.ticks = element_line(
      size = 1
    ),
    legend.position = "right",
    legend.spacing.y = unit(0.7, 'cm'),
    legend.key.size = unit(1, 'cm')
  )+
  labs(title = "Model estimation \n", 
       x = "Depth (cm)",
       y=expression("RLD (cm/cm"^2*")")
  ) +
  coord_flip()+
  ylim(0, 1.5)

graph2ppt(file="ggplot2_plot.pptx", width=6, height=8, append = TRUE)

#=== Rooting depth ===========

Model_Rdep<- R_Model[R_Model$Pred_TRL > 6,] # First exclude windows do not contain roots,define the threshold as 6 mm

Model_Rdep<- Model_Rdep %>%
  group_by(DAP) %>%  
  summarise_at(vars("Window"), max) %>%  
  mutate(Max_rooting_depth = Window * 1.9)# Then choose the the deepest depth as the maximum rooting depth

# Make graph
ggplot(
  Model_Rdep,
  aes(
    DAP,
    Max_rooting_depth
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
      color="black"
    ),
    axis.text.x = element_text(
      size=24, 
      color="black",
      angle = 60
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
    axis.ticks.length = unit(0.2, "cm"),
    axis.ticks = element_line(
      size = 1
    )
  )+
  labs(title = "Model estimation\n",
       x = "Days after planting",
       y="Maximum rooting depth (cm)"
  )

# Export graph
graph2ppt(file="ggplot2_plot.pptx", width=6, height=8, append = TRUE)


#========================================================
#=== Compare both dataset in the same graph =============
#========================================================

R_bothmethod <- read_excel("C:/Users/Kaining/BGU ZKN/R/MR data analysis with R/from_aotocam_to_autoanalysis/from_autocam_to_autoanalysis/2_compare pred TRL with manual/compare_pred_n_manul.xlsx", 
                           sheet = "Compare TRL")
View(R_bothmethod)
