


Year_to_Filter_Data_by <- 2019

{ # Library   ----
  
  library(tidyverse)
  library(ggpubr)
  library(glue)
  library(lubridate)
  library(scales)
  library(pdp)
  library(randomForest)
  library(arrow)
  library(ggforce)
  library(indicspecies)
  
}

{ # Species and Trophic Levels   ----
  
  Species_Info <- readr::read_csv("Meta_Data/Species_Complete.csv")
  
  Mixed_Data_xRef_Biomass <- readr::read_csv("Meta_Data/Mixed_Data_xref_Fish_Biomass.csv")
  
  Mixed_Data_xRef_Density <- readr::read_csv("Meta_Data/Mixed_Data_xref_Fish_Density.csv")
  
  Benthic_Biomass_Species <- c(
    "Macrocystis pyrifera", "giant kelp",
    "Crassedoma giganteum", "rock scallop",
    "Haliotis rufescens", "red abalone",
    "Haliotis corrugata", "pink abalone",
    "Haliotis fulgens", "green abalone",
    "Kelletia kelletii", "Kellet's whelk",           
    "Lithopoma gibberosa", "red turban snail", 
    "Lytechinus anamesus", "white sea urchin", 
    "Megastraea undosa", "wavy turban snail",             
    "Megathura crenulata", "giant keyhole limpet", 
    "Patiria miniata", "bat star",
    "Pisaster giganteus", "giant-spined sea star",            
    "Pycnopodia helianthoides", "sunflower star",
    "Strongylocentrotus franciscanus", "red sea urchin", 
    "Strongylocentrotus purpuratus", "purple sea urchin",
    "Tethya aurantia", "orange puffball sponge", 
    "Tegula regina", "queen tegula", 
    "Macrocystis pyrifera", "giant kelp",
    "Muricea californica", "California golden gorgonian",
    "Muricea fruticosa", "brown gorgonian",
    "Lophogorgia chilensis", "red gorgonian")
  
  Potential_Biomass_Additions <- c(
    "Astrangia lajollaensis", "Corynactis californicus",
    "Phragmatopoma californica", "Serpulorbis squamiger",
    "Diaperoecia californica")
  
  Fish_Biomass_Species <- c(
    "Caulolatilus princeps", "ocean whitefish",
    "Chromis punctipinnis", "blacksmith",
    "Embiotoca jacksoni", "black surfperch",
    "Embiotoca lateralis", "striped surfperch",
    "Girella nigricans", "opaleye",
    "Halichoeres semicinctus", "rock wrasse, female", "rock wrasse, male",     
    "Hypsypops rubicundus", "garibaldi",
    "Medialuna californiensis", "halfmoon",
    "Ophiodon elongatus", "lingcod",
    "Oxyjulis californica", "senorita",
    "Paralabrax clathratus", "kelp bass",
    "Rhacochilus toxotes", "rubberlip surfperch",
    "Rhacochilus vacca", "pile perch",
    "Scorpaena guttata", "California scorpionfish",
    "Scorpaenichthys marmoratus", "cabezon",
    "Sebastes atrovirens", "kelp rockfish",
    "Sebastes chrysomelas", "black and yellow rockfish",
    "Sebastes mystinus", "blue rockfish",  
    "Sebastes serranoides", "olive rockfish",
    "Sebastes serriceps", "treefish",
    "Semicossyphus pulcher", "California sheephead, male", "California sheephead, female")
  
  VFT_Species <- c(
    "blacksmith", 
    "black_surfperch", 
    "striped_surfperch", 
    "opaleye",                     
    "garibaldi", 
    "senorita",
    "kelp_bass", 
    "pile_perch",                 
    "kelp_rockfish", 
    "blue_rockfish", 
    "olive_rockfish", 
    "California_sheephead_female",
    "California_sheephead_male", 
    "rock_wrasse_female", 
    "rock_wrasse_male")
  
  BenthicBiomassColor <- c(
    'Lithopoma gibberosa' = "deeppink", 
    'Megastraea undosa' = "grey40", 
    'Patiria miniata' = "orange2", 
    'Strongylocentrotus franciscanus' = "red2", 
    'Strongylocentrotus purpuratus' = "darkorchid2", 
    'Tegula regina' = "yellow", 
    'Pisaster giganteus' = "deepskyblue2", 
    'Crassedoma giganteum' = "gold", 
    'Haliotis rufescens' = "firebrick1",             
    'Kelletia kelletii' = "black",
    'Lytechinus anamesus' = "grey80", 
    'Megathura crenulata' = "aquamarine2", 
    'Pycnopodia helianthoides' = "aquamarine2",       
    'Tethya aurantia' = "gold1", 
    'Macrocystis pyrifera' = "forestgreen", 
    'Lophogorgia chilensis' = "red",       
    'Muricea californica' = "gold4", 
    'Muricea fruticosa' = "brown")
  
  oneM_Biomass_Species <- c(
    "Megastraea undosa",  #  All years
    "Lithopoma gibberosa",  #  All years
    "Tegula regina", # 2006 to present (when they were added as a species)
    "Patiria miniata",  #  All years               
    "Pisaster giganteus", # 1982 - 1995 then 5 m to 2013
    "Strongylocentrotus franciscanus",  #  All years
    "Strongylocentrotus purpuratus")  #  All years
  fiveM_Biomass_Species <- c("Pisaster giganteus") # 1996 - 2013 then to Band Transects
  bands_Biomass_Species <- c(
    "Tethya aurantia", #  All years 
    "Haliotis rufescens",  #  All years
    "Kelletia kelletii",  #  All years
    "Megathura crenulata",  #  All years
    "Crassedoma giganteum", #  All years
    "Pisaster giganteus",  #  2014 - present 
    "Pycnopodia helianthoides",  #  All years
    "Lytechinus anamesus")  #  All years
  
  SpeciesColor <- c(as.character(Species_Info$Color))
  names(SpeciesColor) <- c(Species_Info$CommonName)
  SpeciesColor <- SpeciesColor[!is.na(SpeciesColor)]
  
  
  
  
  Target_Colors <- c("Calculated Value" = "#440154FF", "Categorical" = "#FDE725FF",
                     "Non-targeted" = "#7ad151", "Targeted" = "#414487", "Mixed" = "#2a788e")
  
  Target_Shapes <- c("Targeted" = 10, 
                     "Non-targeted" = 5, 
                     'Mixed' = 9)
  
  Protocols <- c("Species List" = "species",
                 "1 m² Quadrats" = "1m", 
                 "5 m² Quadrats" = "5m",  
                 "Band Transects" = "bands", 
                 "Random Point Contacts" = "rpcs",
                 "Natural Habitat Size Frequencies" = "nhsf",
                 "Artificial Recruitment Modules" = "arms", 
                 "Roving Diver Fish Count" = "rdfc", 
                 "Visual Fish Transect" = "vft", 
                 "Fish Size Frequencies" = "fsf", 
                 "Video Taped Transects" = "vtt",
                 "Temperature Loggers" = "temp")
}

{ # Sites   -----
  Site_Info <- readr::read_csv("Meta_Data/Site_Info.csv")
  
  site_data <- Site_Info |>
    dplyr::mutate(Island = IslandName) |> 
    dplyr::select(SiteNumber, Island, IslandName, SiteCode, SiteName, Reference, ReserveStatus, ARMs, ReserveYear,
                  Latitude, Longitude, MeanDepth, Rock, Cobble, Sand) |>
    dplyr::rename(`Site #` = SiteNumber, `Site Code` = SiteCode, `Site` = SiteName,
                  Reference = Reference, `Reserve Status` = ReserveStatus, `Mean Depth` = MeanDepth, 
                  `Rock (%)` = Rock, `Cobble (%)` = Cobble, `Sand (%)` =  Sand) |> 
    dplyr::mutate(Island = gsub(" Island", "", Island)) 
  
  SiteLevels <- c(
    # San Miguel
    "Wyckoff Ledge", "Miracle Mile", # Out
    "Hare Rock", # In
    # Santa Rosa
    "Johnson's Lee North", "Johnson's Lee South", "Rodes Reef", "Cluster Point", # Out
    "Trancion Canyon", "Chickasaw", "South Point", # In
    # Santa Cruz
    "Fry's Harbor", "Pelican Bay", "Yellow Banks", "Devil's Peak Member", "Pedro Reef", "Little Scorpion", # Out
    "Gull Island South", "Scorpion Anchorage", "Potato Pasture", "Cavern Point",  # In
    # Anacapa
    "Admiral's Reef", "East Fish Camp", "Lighthouse",  # Out
    "Cathedral Cove" , "Landing Cove", "Black Sea Bass Reef", "Keyhole",  # In
    # Santa Barbara
    "Arch Point", "Cat Canyon", "Webster's Arch",  # Out
    "SE Sea Lion Rookery", "Graveyard Canyon", "Southeast Reef") # In
  
  SiteColor <- c(as.character(Site_Info$Color), as.character(Site_Info$Color))
  names(SiteColor) <- c(Site_Info$SiteName, Site_Info$SiteCode)
  
  SiteLine <- c(Site_Info$LineType, Site_Info$LineType)
  names(SiteLine) <- c(Site_Info$SiteName, Site_Info$SiteCode)
  
}

{ # Islands and MPAs   -----
  
  multipatt_island_list <- c(rep("AN", 3), rep("SB", 3), rep("SC", 3), rep("SR", 3))
  
  Island_Colors <- 
    c("San Miguel" = "darkmagenta", "San Miguel Island" = "darkmagenta", "SM" = "darkmagenta", 
      "Santa Rosa" = "dodgerblue4", "Santa Rosa Island" = "dodgerblue4", "SR" = "dodgerblue4", 
      "Santa Cruz" = "forestgreen", "Santa Cruz Island" = "forestgreen", "SC" = "forestgreen", 
      "Anacapa" = "darkorange", "Anacapa Island" = "darkorange", "AN" = "darkorange", 
      "Santa Barbara" = "firebrick2", "Santa Barbara Island" = "firebrick2", "SB" = "firebrick2", 
      "Inside" = "green", "Outside" = "red", "1978" = "green", "2003" = "dodgerblue2") 
  
  Island_Viridis_Colors <-
    c("San Miguel" = "#440154FF", "San Miguel Island" = "#440154FF", "SM" = "#440154FF",
      "Santa Rosa" = "#3B528BFF", "Santa Rosa Island" = "#3B528BFF", "SR" = "#3B528BFF",
      "Santa Cruz" = "#21908CFF", "Santa Cruz Island" = "#21908CFF", "SC" = "#21908CFF",
      "Anacapa" = "#5DC863FF", "Anacapa Island" = "#5DC863FF", "AN" = "#5DC863FF",
      "Santa Barbara" = "#FDE725FF", "Santa Barbara Island" = "#FDE725FF", "SB" = "#FDE725FF",
      "Inside" = "#440154FF", "Outside" = "#FDE725FF", "1978" = "#440154FF", "2003" = "#21908CFF")
  
  Island_Code_Levels <- c("SR", "SC", "AN", 'SB')
  
  Island_Levels_Short <- 
    c("San Miguel", 
      "Santa Rosa", 
      "Santa Cruz", 
      "Anacapa", 
      "Santa Barbara")
  
  Island_Levels_Long <- 
    c("San Miguel Island", 
      "Santa Rosa Island", 
      "Santa Cruz Island", 
      "Anacapa Island",  
      "Santa Barbara Island")
  
  MPA_Levels_Short <- 
    c("Santa Rosa", 
      "Santa Cruz", 
      "Anacapa",  
      "Santa Barbara")
  
  MPA_Levels_Long <- 
    c("Santa Rosa Island",
      "Santa Cruz Island", 
      "Anacapa Island",  
      "Santa Barbara Island")
  
  
}

{ # SST Indicies  ----
  SST_Index <- arrow::read_feather("Tidy_Data/SST_Anomaly_Index.feather") |> 
    dplyr::filter(Date < as.Date(paste(Year_to_Filter_Data_by, "-08-01", sep = "")))
  
  SST_Index_2005 <- SST_Index |> 
    dplyr::filter(Date > as.Date("2005-06-01"))
  
  SST_Anomaly_Index_Mean <- SST_Index |>
    dplyr::filter(SurveyYear > 2004, Date < base::as.Date(glue("{Year_to_Filter_Data_by + 1}-1-1"))) |> 
    dplyr::group_by(SurveyYear)  |> 
    dplyr::summarise(Mean_ONI_Anom = mean(ONI_ANOM), Mean_PDO_Anom = mean(PDO_ANOM)) 
  
  Temp_Anom_Table <- arrow::read_feather("Tidy_Data/KFM_Temp_Anomaly.feather") %>%
    dplyr::group_by(Year, IslandCode) %>%
    dplyr::summarise(Mean_KFM_Anom = mean(Monthly_Anom)) %>%
    dplyr::filter(Year > 2004) %>%
    dplyr::rename(SurveyYear = Year) %>%
    dplyr::left_join(
      SST_Anomaly_Index_Mean %>%
        dplyr::distinct(Mean_ONI_Anom, Mean_PDO_Anom, SurveyYear),
      by = "SurveyYear") %>%
    dplyr::ungroup() 
}

{ # Dive meta data  ----
  Dive_Meta_Data <- readr::read_csv("Meta_Data/Dive_Totals.csv")
  
  Total_Dives <- sum(Dive_Meta_Data$Dives)
  
  Divers <- mean(Dive_Meta_Data$Divers)
  
  Dive_Time <- sum(Dive_Meta_Data$Dive_Hours)
  
  Vessel_Time <- sum(Dive_Meta_Data$Vessel_Days)
  
}

{ # Plot Themes   ----
  
  map_bubble_theme <- function() {
    ggplot2::theme_void() +
      ggplot2::theme(legend.position = "right", 
                     plot.title = element_text(hjust = 0.5),
                     plot.subtitle = element_text(hjust = 0.5))
  }
  
  all_sites_theme <- function () {
    ggpubr::theme_classic2() +
      ggplot2::theme(
        legend.position = "right",
        panel.grid.major = element_line(),
        legend.justification = c(0, 0.5),
        legend.key.width = unit(.75, "cm"),
        legend.background = element_rect(size = unit(5, "cm")),
        legend.title = element_text(size = 12, color = "black"),
        legend.text = element_text(size = 10, colour = "black"),
        legend.spacing.y = unit(.01, 'cm'),
        legend.margin = ggplot2::margin(unit(0.1, "cm")),
        axis.title = element_text(hjust = .5, size = 12),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 12),
        strip.text = element_text(size = 10, colour = "black", angle = 90))
  }
  
  timeseries_top_theme <- function () {
    ggpubr::theme_classic2() +
      ggplot2::theme(
        text = element_text(color = "black"),
        legend.justification = c(0, 0.5),
        legend.key.width = unit(.75, "cm"),
        legend.title = element_text(size = 10, color = "black"),
        legend.text = element_text(size = 10, color = "black"),
        axis.title = element_text(size = 10, color="black"),
        axis.text.y = element_text(size = 10, color="black"),
        axis.text.x = element_blank(),
        panel.grid.major= element_line())
  }
  
  timeseries_bottom_theme <- function (){
    ggpubr::theme_classic2() +
      ggplot2::theme(
        text = element_text(color="black"),
        plot.caption = element_text(size = 13),
        legend.justification = c(0, 0.5),
        legend.key.width = unit(.75, "cm"),
        legend.title = element_text(size = 10, color = "black"),
        legend.text = element_text(size = 10, color = "black"),
        legend.margin = ggplot2::margin(unit(0.1, "cm")),
        axis.title = element_text(size = 10, color = "black"),
        axis.text.y = element_text(size = 10, color = "black"),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 10, color="black"),
        axis.line.x = element_blank(),
        panel.grid.major= element_line())
  }
  
  Original_16_top_theme <- function () {
    ggpubr::theme_classic2() +
      ggplot2::theme(
        text = element_text(color = "black"),
        legend.key.width = unit(.75, "cm"),
        legend.justification = c(0, 0.5),
        legend.title = element_text(size = 10, color = "black"),
        legend.text = element_text(size = 10, color = "black"),
        axis.title = element_text(hjust = .5, size = 12),
        axis.text.y = element_text(size = 10, color="black"),
        axis.text.x = element_blank(),
        panel.grid.major= element_line())
  }
  
  Original_16_bottom_theme <- function () {
    ggpubr::theme_classic2() +
      ggplot2::theme(
        text = element_text(color="black"),
        legend.position = "right",
        legend.justification = c(0,0.5),
        legend.key.width = unit(.75, "cm"),
        legend.background = element_rect(size = unit(5, "cm")),
        legend.title = element_text(size = 10, color = "black"),
        legend.text = element_text(size = 10, colour = "black"),
        panel.grid.major = element_line(),
        axis.title = element_text(hjust = .5, size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 10, color="black"),
        axis.text.y = element_text(size = 10, color="black"),
        axis.line.x = element_blank(),
        strip.text = element_text(size = 10, colour = "black", angle = 90))
  }
  
  nMDS_theme <- function () {
    ggplot2::theme_minimal() + 
      ggplot2::theme(
        legend.position = "bottom", 
        plot.title = element_text(size = 16, hjust = 0.5),
        axis.text = element_blank(), 
        axis.ticks = element_blank(), 
        axis.title = element_blank(), 
        panel.border = element_rect(fill = NA),
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank(),  
        plot.caption = element_text(size=9, hjust = 0),
        aspect.ratio=1) 
  }
  
  PDP_theme <- function() {
    theme_minimal() +
      theme(legend.position = "bottom",
            panel.grid.major = element_blank(),  
            panel.grid.minor = element_blank(),
            axis.line = element_line())
  }
  
  ISA_theme <- function () {
    ggplot2::theme_bw() + 
      ggplot2::theme(plot.title = element_text(size = 16, hjust = 0.5),
                     axis.text.x = element_text(size = 10.5, color = "black", angle = 45, hjust = 1, vjust = 1),
                     axis.text.y = element_text(size = 11, color = "black", face = "italic"),
                     axis.title = element_text(size = 14),
                     legend.position = "right",
                     legend.text = element_text(size = 10, color = "black"),
                     legend.title = element_text(size = 12),
                     panel.grid.major = element_blank(),  
                     panel.grid.minor = element_blank())
  }
  
  Ratio_theme <- function () {
    theme_classic() +
      theme(panel.grid.major = element_line(),
            legend.position = "bottom",
            strip.background = element_blank())
  }
  
  Biomass_Summary_theme <- function () {
    ggplot2::theme_classic() +
      ggplot2::theme(
        plot.title = element_text(hjust = 0.5, size = 18),
        plot.subtitle = element_text(hjust = 0.5, size = 16),
        plot.caption = element_text(hjust = 0),
        panel.border = element_rect(fill = FALSE),
        legend.position = "right",
        legend.justification = c(0.5, 0.5),
        legend.title = element_text(size = 12, color = "black"),
        legend.text = element_text(size = 9, color = "black"),
        axis.title = element_text(size = 16),
        axis.text.y = element_text(size = 12), 
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 12),
        panel.spacing.x = unit(.45, "cm"),
        strip.text = element_text(size = 10, colour = "black"))
  }
  
  Boxplot_theme <- function() {
    theme_classic() +
      theme(plot.title = element_text(size = 16, face = "italic"),
            plot.subtitle = element_text(size = 14),
            axis.title = element_text(size = 12),
            axis.text.y = element_text(size = 12),
            axis.text.x = element_text(size = 12, angle = 45, hjust = 1, vjust = 1),
            strip.text = element_text(size = 12, angle = 90),
            legend.position = "bottom",
            legend.background = element_rect(),
            legend.title = element_text(size = 12),
            legend.text = element_text(size = 10),
            plot.caption = element_text(size = 10, hjust = 0),
            axis.line.x = element_blank())
  }
  
}

{ # Plot Templates  ----
  
  { # Defaults   ---- 
    
    p_1 <- ggplot2::ggplot() +
      ggplot2::scale_x_date(date_labels = "%Y", date_breaks = "year", expand = c(0, 0)) +
      ggplot2::scale_y_continuous(limits = c(0, NA), expand = c(0, 0), oob = squish) +
      ggplot2::scale_color_viridis_d(end = .8) +
      ggplot2::labs(x = NULL, y = NULL, linetype = "Reserve Status", color = "Reserve Status", tag = "A") +
      timeseries_top_theme()
    
    p_2 <- ggplot2::ggplot() +
      ggplot2::scale_x_date(date_labels = "%Y", date_breaks = "year", expand = c(0, 0)) +
      ggplot2::scale_y_continuous(limits = c(0, NA), expand = c(0, 0), oob = squish) +
      ggplot2::scale_color_viridis_d() +
      ggplot2::labs(x = NULL, y = NULL, color = "Island", tag = "B") +
      timeseries_top_theme()
    
    p_3 <- ggplot2::ggplot() +
      ggplot2::geom_rect(data = SST_Index_2005, 
                         aes(xmin = DateStart, xmax = DateEnd, ymin = -Inf, ymax = 0, fill = ONI_ANOM)) +
      ggplot2::scale_fill_viridis_c(
        option = "plasma",
        guide = guide_colorbar(direction = "horizontal", title.position = "top",
                               order = 3, barheight = unit(.2, "cm"))) +
      ggplot2::scale_x_date(date_labels = "%Y", date_breaks = "year", expand = c(0, 0)) +
      ggplot2::scale_y_continuous(expand = expansion(mult = c(.1, 0.01)),
                                  limits = c(0, NA), oob = squish) +
      ggplot2::scale_color_viridis_d() +
      ggplot2::geom_hline(aes(yintercept = 0)) +
      ggplot2::guides(color = guide_legend(order = 1), 
                      linetype = guide_legend(order = 2, override.aes = list(col = 'black'))) +
      ggplot2::labs(x = "Survey Year", y = NULL, color = "Island", tag = "C",
                    fill = "Oceanic Ni\u00f1o Index", linetype = "Reserve Status") +
      timeseries_bottom_theme()
    
    p_1_16 <- ggplot2::ggplot() + 
      ggplot2::geom_vline(aes(xintercept = as.Date("2003-01-01")), size = 1) +
      ggplot2::geom_label(aes(x = as.Date("2003-01-01"), y = Inf, vjust = 1, 
                              hjust = 1, label = "MPAs Created (2003)"), color = "black") +
      ggplot2::scale_x_date(date_labels = "%Y", date_breaks = "year", expand = c(0, 0)) +
      ggplot2::scale_y_continuous(expand = expansion(mult = c(0, .1)),
                                  limits = c(0, NA), oob = squish) +
      ggplot2::scale_color_viridis_d(end = .8) +
      ggplot2::labs(title = NULL, subtitle = NULL, tag = "A",
                    color = "Reserve Year", linetype = "Reserve Year",
                    x = NULL, y = NULL) +
      Original_16_top_theme()
  }
  
  { # Diversity Plots   ----
    Diversity_Plot <- function(Sci_Name) {
      DF <- Diversity |> 
        dplyr::filter(Index == Sci_Name)
      
      Ind <- if(Sci_Name == "shannon_2005") {"Shannon's"} else {"Simpson's"}
      
      p1 <- ggplot2::ggplot() +
        ggplot2::geom_smooth(data = DF, aes(x = Date, y = Value, color = ReserveStatus), size = 1.25, 
                             method = 'loess', formula = 'y~x') +
        ggplot2::scale_x_date(date_labels = "%Y", date_breaks = "year", expand = c(0, 0)) +
        ggplot2::scale_y_continuous(limits = c(NA, NA), expand = c(0, 0), oob = squish) +
        ggplot2::scale_color_viridis_d(end = .8) +
        ggplot2::labs(x = NULL, y = NULL, linetype = "Reserve Status", color = "Reserve Status", tag = "A") +
        timeseries_top_theme() 
      
      p2 <- ggplot2::ggplot() +
        ggplot2::geom_smooth(data = DF, aes(x = Date, y = Value, color = IslandName),
                             size = 1.25, method = 'loess', formula = 'y~x') +
        ggplot2::scale_x_date(date_labels = "%Y", date_breaks = "year", expand = c(0, 0)) +
        ggplot2::scale_y_continuous(limits = c(NA, NA), expand = c(0, 0), oob = squish) +
        ggplot2::scale_color_viridis_d() +
        ggplot2::labs(x = NULL, y = NULL, tag = "B",
                      color = "Island") +
        timeseries_top_theme()
      
      p3 <- ggplot2::ggplot() +
        ggplot2::geom_rect(data = SST_Index_2005,
                           aes(xmin = DateStart, xmax = DateEnd, ymin = -Inf, ymax = .4, fill = ONI_ANOM)) +
        ggplot2::scale_fill_viridis_c(
          option = "plasma",
          guide = guide_colorbar(direction = "horizontal", title.position = "top",
                                 order = 3, barheight = unit(.2, "cm"))) +
        ggplot2::geom_smooth(data = DF, size = 1.25, 
                             aes(x = Date, y = Value, color = IslandName, linetype = ReserveStatus),
                             method = 'loess', formula = 'y~x', se = F) +
        ggplot2::geom_hline(aes(yintercept = 0)) +
        ggplot2::scale_x_date(date_labels = "%Y", date_breaks = "year", expand = c(0, 0)) +
        ggplot2::scale_y_continuous(expand = expansion(mult = c(.1, 0)),
                                    limits = c(.4, NA), oob = squish) +
        ggplot2::guides(color = guide_legend(order = 1), 
                        linetype = guide_legend(order = 2, override.aes = list(col = 'black'))) +
        ggplot2::scale_color_viridis_d() +
        ggplot2::labs(x = "Survey Year", y = NULL,
                      color = "Island", tag = "C",
                      fill = "Oceanic Ni\u00f1o Index",
                      linetype = "Reserve Status") +
        timeseries_bottom_theme() 
      Div_Plot <-ggpubr::ggarrange(p1, p2, p3, ncol = 1, align = "v", heights = c(.8, .8, 1))
      Div_annotated <- ggpubr::annotate_figure(
        Div_Plot,
        left = text_grob(paste(Ind, " Index (Diversity)", sep = ""), 
                         family ="Cambria", color = "black", rot = 90, size = 13))
      print(Div_annotated)
    }
  }
  
  { # Diversity Original 16 Plot  ----
    Diversity_16_Plot <- function(Sci_Name) {
      
      DF <- Diversity_Orginal_16 |> 
        dplyr::filter(Index == Sci_Name)
      
      SST <- dplyr::filter(SST_Index, Date > as.Date(paste(min(DF$SurveyYear), "-06-30", sep = "")))
      
      Ind <- if(Sci_Name == "shannon_all"){"Shannon's"}else{"Simpson's"}
      
      p1 <- ggplot2::ggplot() + 
        ggplot2::geom_smooth(data = DF, aes(x = Date, y = Value, color = ReserveYear),
                             size = 1.25, method = 'loess', formula = 'y~x') +
        ggplot2::geom_vline(aes(xintercept = as.Date("2003-01-01")), size = 1) +
        ggplot2::geom_label(aes(x = as.Date("2003-01-01"), y = Inf, vjust = 1, 
                                hjust = 1, label = "MPAs Created (2003)"), color = "black") +
        ggplot2::scale_x_date(date_labels = "%Y", date_breaks = "year", expand = c(0, 0)) +
        ggplot2::scale_y_continuous(expand = expansion(mult = c(0, .1)),
                                    limits = c(NA, NA), oob = squish) +
        ggplot2::scale_color_viridis_d(end = .8) +
        ggplot2::labs(title = NULL, subtitle = NULL, tag = "A",
                      color = "Reserve Year", linetype = "Reserve Year",
                      x = NULL, y = NULL) +
        Original_16_top_theme()
      
      p2 <- ggplot2::ggplot() +
        ggplot2::geom_rect(
          data = SST, aes(xmin = DateStart, xmax = DateEnd, ymin = -Inf, ymax = .4, fill = ONI_ANOM)) +
        ggplot2::scale_fill_viridis_c(
          option = "plasma",
          guide = guide_colorbar(direction = "horizontal", title.position = "top",
                                 order = 3, barheight = unit(.2, "cm"))) +
        ggplot2::geom_smooth(data = DF, size = 1.25, method = 'loess', formula = 'y~x',
                             aes(x = Date, y = Value, color = IslandName)) +
        ggplot2::geom_vline(aes(xintercept = as.Date("2003-01-01"), ymin = 0, ymax = Inf), size = 1) +
        ggplot2::geom_hline(aes(yintercept = 0)) +
        ggplot2::scale_x_date(date_labels = "%Y", date_breaks = "year", expand = c(0, 0)) +
        ggplot2::scale_y_continuous(expand = expansion(mult = c(.1, 0)),
                                    limits = c(.4, NA), oob = squish) +
        ggplot2::scale_color_viridis_d() +
        ggplot2::guides(color = guide_legend(order = 1), 
                        linetype = guide_legend(order = 2, override.aes = list(col = 'black'))) +
        ggplot2::labs(title = NULL, subtitle = NULL, tag = "B",
                      color = "Island",
                      x = "Survey Year", y = NULL,
                      fill = "Oceanic Ni\u00f1o Index") +
        Original_16_bottom_theme()
      
      Orig16_Diversity_Plot <- ggpubr::ggarrange(p1, p2, ncol = 1, align = "v", heights = c(.8, 1))
      Orig16_Diversity_Annotated <- ggpubr::annotate_figure(
        Orig16_Diversity_Plot,
        left = text_grob(paste(Ind, " Index (Diversity)", sep = ""), 
                         color = "black", rot = 90, size = 12))
      print(Orig16_Diversity_Annotated)
    }
  }
  
  { # Mean Density Plot   ----
    Mean_Density_Plot <- function(Sci_Name) {
      DF <- Density |> 
        dplyr::filter(ScientificName == Sci_Name)
      
      p1 <- p_1 +
        ggplot2::geom_smooth(data = DF, aes(x = Date, y = Mean_Density, color = ReserveStatus),
                             size = 1.25, method = 'loess', formula = 'y~x')
      
      p2 <- p_2 +
        ggplot2::geom_smooth(data = DF, aes(x = Date, y = Mean_Density, color = IslandName),
                            size = 1.25, method = 'loess', formula = 'y~x') 
      
      p3 <- p_3 +
        ggplot2::geom_smooth(data = DF, aes(x = Date, y = Mean_Density, color = IslandName, linetype = ReserveStatus),
                             method = 'loess', formula = 'y~x', size = 1.25, se = FALSE)
      
      Density_Plot <- ggpubr::ggarrange(p1, p2, p3, ncol = 1, align = "v", heights = c(.8, .8, 1))
      
      Density_annotated <- 
        ggpubr::annotate_figure(
          Density_Plot,
          left = text_grob(paste(Sci_Name, " density (#/\U33A1)"), color = "black", rot = 90, size = 12))
      
      print(Density_annotated)
    }
  }
  
  { # Mean Density Original 16 Plot  ----
    Mean_Density_16_Plot <- function(Sci_Name, Com_Name) {
      
      DF <- Density_Orginal_16 |> 
        dplyr::filter(ScientificName == Sci_Name,
                      CommonName %in% Com_Name) |> 
        dplyr::group_by(ScientificName, SurveyYear, SiteNumber) |> 
        dplyr::mutate(Mean_Density = sum(Count)/2000)
      
      SST <- dplyr::filter(SST_Index, Date > as.Date(paste(min(DF$SurveyYear), "-06-30", sep = "")))
      
      if(Sci_Name == "Parastichopus parvimensis"){
        p1 <- p_1_16 + 
        ggplot2::geom_smooth(data = DF, aes(x = Date, y = Mean_Density, color = ReserveYear),
                             size = 1.25, method = 'loess', formula = 'y~x') +
          ggplot2::geom_vline(aes(xintercept = as.Date("1993-01-01")), size = 1) +
          ggplot2::geom_label(aes(x = as.Date("1993-01-01"), y = Inf, vjust = 1,
                                  hjust = 1, label = "Dive Fishery Begins"), color = "black")
        
        p2 <- ggplot2::ggplot() +
          ggplot2::geom_rect(
            data = SST, aes(xmin = DateStart, xmax = DateEnd, ymin = -Inf, ymax = 0, fill = ONI_ANOM)) +
          ggplot2::scale_fill_viridis_c(
            option = "plasma",
            guide = guide_colorbar(direction = "horizontal", title.position = "top",
                                   order = 3, barheight = unit(.2, "cm"))) +
          ggplot2::geom_smooth(data = DF, size = 1.25, method = 'loess', formula = 'y~x',
                               aes(x = Date, y = Mean_Density, color = IslandName)) +
          ggplot2::geom_vline(aes(xintercept = as.Date("1993-01-01")), size = 1) +
          ggplot2::geom_vline(aes(xintercept = as.Date("2003-01-01"), ymin = 0, ymax = Inf), size = 1) +
          ggplot2::geom_hline(aes(yintercept = 0)) +
          ggplot2::scale_x_date(date_labels = "%Y", date_breaks = "year", expand = c(0, 0)) +
          ggplot2::scale_y_continuous(expand = expansion(mult = c(.1, 0)),
                                      limits = c(0, NA), oob = squish) +
          ggplot2::scale_color_viridis_d() +
          ggplot2::guides(color = guide_legend(order = 1), 
                          linetype = guide_legend(order = 2, override.aes = list(col = 'black'))) +
          ggplot2::labs(title = NULL, subtitle = NULL, tag = "B",
                        color = "Island",
                        x = "Survey Year", y = NULL,
                        fill = "Oceanic Ni\u00f1o Index") +
          Original_16_bottom_theme()
      } 
      else {
        p1 <- ggplot2::ggplot(data = DF, aes(x = Date, y = Mean_Density, color = ReserveYear)) + 
          ggplot2::geom_smooth(size = 1.25, method = 'loess', formula = 'y~x') +
          ggplot2::geom_vline(aes(xintercept = as.Date("2003-01-01")), size = 1) +
          ggplot2::geom_label(aes(x = as.Date("2003-01-01"), y = Inf, vjust = 1, 
                                  hjust = 1, label = "MPAs Created (2003)"), color = "black") +
          ggplot2::scale_x_date(date_labels = "%Y", date_breaks = "year", expand = c(0, 0)) +
          ggplot2::scale_y_continuous(expand = expansion(mult = c(0, .1)),
                                      limits = c(0, NA), oob = squish) +
          ggplot2::scale_color_viridis_d(end = .8) +
          ggplot2::labs(title = NULL, subtitle = NULL, tag = "A",
                        color = "Reserve Year", linetype = "Reserve Year",
                        x = NULL, y = NULL) +
          Original_16_top_theme()
        
        p2 <- ggplot2::ggplot() +
          ggplot2::geom_rect(
            data = SST, aes(xmin = DateStart, xmax = DateEnd, ymin = -Inf, ymax = 0, fill = ONI_ANOM)) +
          ggplot2::scale_fill_viridis_c(
            option = "plasma",
            guide = guide_colorbar(direction = "horizontal", title.position = "top",
                                   order = 3, barheight = unit(.2, "cm"))) +
          ggplot2::geom_smooth(data = DF, size = 1.25, method = 'loess', formula = 'y~x',
                               aes(x = Date, y = Mean_Density, color = IslandName)) +
          ggplot2::geom_vline(aes(xintercept = as.Date("2003-01-01"), ymin = 0, ymax = Inf), size = 1) +
          ggplot2::geom_hline(aes(yintercept = 0)) +
          ggplot2::scale_x_date(date_labels = "%Y", date_breaks = "year", expand = c(0, 0)) +
          ggplot2::scale_y_continuous(expand = expansion(mult = c(.1, 0)),
                                      limits = c(0, NA), oob = squish) +
          ggplot2::scale_color_viridis_d() +
          ggplot2::guides(color = guide_legend(order = 1), 
                          linetype = guide_legend(order = 2, override.aes = list(col = 'black'))) +
          ggplot2::labs(title = NULL, subtitle = NULL, tag = "B",
                        color = "Island",
                        x = "Survey Year", y = NULL,
                        fill = "Oceanic Ni\u00f1o Index") +
          Original_16_bottom_theme()
      }
      
      Orig16_Density_Plot <- ggpubr::ggarrange(p1, p2, ncol = 1, align = "v", heights = c(.8, 1))
      Orig16_Density_Annotated <- ggpubr::annotate_figure(
        Orig16_Density_Plot,
        left = text_grob(paste(DF$ScientificName, " density (#/\U33A1)"), 
                         color = "black", rot = 90, size = 12))
      print(Orig16_Density_Annotated)
    }
  }
  
  { # Mean Biomass Plot   ----
    Mean_Biomass_Plot <- function(Sci_Name, Com_Name) {
      DF <- Biomass |> 
        dplyr::filter(ScientificName == Sci_Name,
                      CommonName == Com_Name)
      
      p1 <- p_1 +
        ggplot2::geom_smooth(data = DF, aes(x = Date, y = Mean_Biomass, color = ReserveStatus),
                             size = 1.25, method = 'loess', formula = 'y~x')
      
      p2 <- p_2 +
        ggplot2::geom_smooth(data = DF, aes(x = Date, y = Mean_Biomass, color = IslandName),
                             size = 1.25, method = 'loess', formula = 'y~x') 
      
      p3 <- p_3 +
        ggplot2::geom_smooth(data = DF, method = 'loess', formula = 'y~x', size = 1.25,
                             aes(x = Date, y = Mean_Biomass, color = IslandName, 
                                 linetype = ReserveStatus), se = FALSE) 
      
      Biomass_Plot <- ggpubr::ggarrange(p1, p2, p3, ncol = 1, align = "v", heights = c(.8, .8, 1))
      
      Biomass_annotated <- 
        ggpubr::annotate_figure(
          Biomass_Plot,
          left = text_grob(paste(Sci_Name, " biomass (g/\U33A1)"), color = "black", rot = 90, size = 12))
      
      print(Biomass_annotated)
    }
  }
  
  { # Mean Biomass Original 16 Plot  ----
    Mean_Biomass_16_Plot <- function(Sci_Name) {
      
      DF <- dplyr::filter(Biomass_Orginal_16, ScientificName == Sci_Name)
      
      SST <- dplyr::filter(SST_Index, Date > as.Date(paste(min(DF$SurveyYear), "-06-30", sep = "")))
      
      p1 <- p_1_16 + 
        ggplot2::geom_smooth(data = DF, aes(x = Date, y = Mean_Biomass, color = ReserveYear),
                             size = 1.25, method = 'loess', formula = 'y~x')
      
      p2 <- ggplot2::ggplot() +
        ggplot2::geom_rect(data = SST,
                           aes(xmin = DateStart, xmax = DateEnd, ymin = -Inf, ymax = 0, fill = ONI_ANOM)) +
        ggplot2::scale_fill_viridis_c(
          option = "plasma",
          guide = guide_colorbar(direction = "horizontal", title.position = "top",
                                 order = 3, barheight = unit(.2, "cm"))) +
        ggplot2::geom_smooth(data = DF, size = 1.25, method = 'loess', formula = 'y~x',
                             aes(x = Date, y = Mean_Biomass, color = IslandName)) +
        ggplot2::geom_vline(aes(xintercept = as.Date("2003-01-01"), ymin = 0, ymax = Inf), size = 1) +
        ggplot2::geom_hline(aes(yintercept = 0)) +
        ggplot2::scale_x_date(date_labels = "%Y", date_breaks = "year", expand = c(0, 0)) +
        ggplot2::scale_y_continuous(expand = expansion(mult = c(.1, 0)),
                                    limits = c(0, NA), oob = squish) +
        ggplot2::scale_color_viridis_d() +
        ggplot2::guides(color = guide_legend(order = 1), 
                        linetype = guide_legend(order = 2, override.aes = list(col = 'black'))) +
        ggplot2::labs(title = NULL, subtitle = NULL, tag = "B",
                      color = "Island",
                      x = "Survey Year", y = NULL,
                      fill = "Oceanic Ni\u00f1o Index") +
        Original_16_bottom_theme()
      
      Orig16_Biomass_Plot <- ggpubr::ggarrange(p1, p2, ncol = 1, align = "v", heights = c(.8, 1))
      Orig16_Biomass_Annotated <- ggpubr::annotate_figure(
        Orig16_Biomass_Plot,
        left = text_grob(paste(DF$ScientificName, "biomass (g/\U33A1)"), 
                         color = "black", rot = 90, size = 12))
      print(Orig16_Biomass_Annotated)
    }
  }
  
  { # Percent Cover Plot  ----
    Percent_Cover_Plot <- function(Sci_Name) {
      DF <- RPC |> 
        dplyr::filter(ScientificName == Sci_Name)
      
      p1 <- p_1 +
        ggplot2::geom_smooth(data = DF, aes(x = Date, y = Percent_Cover, color = ReserveStatus),
                             size = 1.25, method = 'loess', formula = 'y~x') 
      
      p2 <- p_2 +
        ggplot2::geom_smooth(data = DF, aes(x = Date, y = Percent_Cover, color = IslandName),
                             size = 1.25, method = 'loess', formula = 'y~x')
      
      p3 <- p_3 +
        ggplot2::geom_smooth(data = DF, method = 'loess', formula = 'y~x', size = 1.25,
                             aes(x = Date, y = Percent_Cover, color = IslandName, 
                                 linetype = ReserveStatus), se = FALSE) 
      
      cover_Plot <- ggpubr::ggarrange(p1, p2, p3, ncol = 1, align = "v", heights = c(.8, .8, 1))
      
      cover_annotated <- 
        ggpubr::annotate_figure(
          cover_Plot,
          left = text_grob(paste(Sci_Name, " percent cover", sep = ""), color = "black", rot = 90, size = 12))
      
      print(cover_annotated)
    }
  }
  
  { # Percent Cover Original 16 Plot  ----
    Percent_Cover_Original_16_Plot <- function(Sci_Name) {
      DF <- RPC_Original_16 |> 
        dplyr::filter(ScientificName == Sci_Name)
      
      SST <- dplyr::filter(SST_Index, Date > as.Date(paste(min(DF$SurveyYear), "-06-30", sep = "")))
      
      p1 <-  p_1_16 + 
        ggplot2::geom_smooth(data = DF, aes(x = Date, y = Percent_Cover, color = ReserveYear),
                             size = 1.25, method = 'loess', formula = 'y~x')
      
      p2 <- ggplot2::ggplot() +
        ggplot2::geom_rect(data = SST,
                           aes(xmin = DateStart, xmax = DateEnd, ymin = -Inf, ymax = 0, fill = ONI_ANOM)) +
        ggplot2::scale_fill_viridis_c(
          option = "plasma",
          guide = guide_colorbar(direction = "horizontal", title.position = "top",
                                 order = 3, barheight = unit(.2, "cm"))) +
        ggplot2::geom_smooth(data = DF, size = 1.25, method = 'loess', formula = 'y~x', 
                             aes(x = Date, y = Percent_Cover, color = IslandName)) +
        ggplot2::geom_vline(aes(xintercept = as.Date("2003-01-01")), size = 1) +
        ggplot2::geom_hline(aes(yintercept = 0)) +
        ggplot2::scale_y_continuous(expand = expansion(mult = c(.1, 0)),
                                    limits = c(0, NA), oob = squish) +
        ggplot2::scale_x_date(date_labels = "%Y", date_breaks = "year", expand = c(0, 0)) +
        ggplot2::scale_color_viridis_d() +
        ggplot2::guides(color = guide_legend(order = 1), 
                        linetype = guide_legend(order = 2, override.aes = list(col = 'black'))) +
        ggplot2::labs(title = NULL, subtitle = NULL, tag = "B",
                      color = "Island",
                      x = "Survey Year", y = NULL,
                      fill = "Oceanic Ni\u00f1o Index") +
        Original_16_bottom_theme() 
      
      Cover_Orig16_Plot <- ggpubr::ggarrange(p1, p2, ncol = 1, align = "v", heights = c(.8, 1))
      Cover_Orig16_annotated <- ggpubr::annotate_figure(
        Cover_Orig16_Plot,
        left = text_grob(paste(Sci_Name, " percent cover", sep = ""),
                         color = "black", rot = 90, size = 12))
      print(Cover_Orig16_annotated)
    }
  }
  
}

{ # Biodiversity Data    ----
  Diversity <- arrow::read_feather("Tidy_Data/Diversity_2005.feather") |> 
    dplyr::mutate(IslandName = gsub(" Island", "", IslandName),
                  IslandName = factor(IslandName, levels = Island_Levels_Short))
  
  
  Diversity_Orginal_16 <- arrow::read_feather("Tidy_Data/Diversity_All.feather") |> 
    dplyr::mutate(IslandName = gsub(" Island", "", IslandName),
                  IslandName = factor(IslandName, levels = Island_Levels_Short))
}

{ # Community Similarity Data   ----
  nMDS <- arrow::read_feather('Tidy_Data/nMDS.feather') |> 
    dplyr::mutate(IslandName = gsub(" Island", "", IslandName),
                  IslandName = factor(IslandName, levels = Island_Levels_Short))
  
  ellipses <- arrow::read_feather("Tidy_Data/ellipses.feather") |> 
    dplyr::mutate(IslandName = gsub(" Island", "", IslandName),
                  IslandName = factor(IslandName, levels = Island_Levels_Short))
  
  anosim_table <- arrow::read_feather("Tidy_Data/ANOSIM.feather")
}

{ # Mixed Data   ----
  Mixed_2005 <- arrow::read_feather("Tidy_Data/Mixed_Data_2005.feather") |> 
    dplyr::filter(Reference == TRUE, SurveyYear > 2004) |>
    dplyr::mutate(SurveyYear = factor(SurveyYear),
                  IslandCode = factor(IslandCode, levels = Island_Code_Levels),
                  ReserveStatus = factor(ReserveStatus)) |> 
    dplyr::select(-SiteNumber, -SiteName, 
                  -IslandName, -SiteCode) 
}

{ # Random Forest Models   ----
  RF_Reserve_Model_2005 <- base::readRDS("Models/RF_Reserve_Model_2005.rds")
  
}

{ # Important Species Data  ---- 
  RF_Importance <- arrow::read_feather("Tidy_Data/RF_Importance.feather") |> 
    dplyr::arrange(desc(MeanDecreaseAccuracy))
  partial_df <- arrow::read_feather("Tidy_Data/PDP_data.feather") |> 
    dplyr::mutate(CommonName = fct_inorder(CommonName))
}

{ # Density/Biomass/RPC Data    ----
  Density <- arrow::read_feather("Tidy_Data/Density.feather") |> 
    dplyr::mutate(IslandName = gsub(" Island", "", IslandName),
                  IslandName = factor(IslandName, levels = Island_Levels_Short)) |> 
    dplyr::filter(Reference == TRUE, SurveyYear > 2004) |>
    dplyr::mutate(Date = base::as.Date(base::ISOdate(SurveyYear, 7, 1)))
  
  Density_Orginal_16 <- arrow::read_feather("Tidy_Data/Density.feather") |>  
    dplyr::mutate(IslandName = gsub(" Island", "", IslandName),
                  IslandName = factor(IslandName, levels = Island_Levels_Short)) |> 
    dplyr::filter(SiteNumber %in% 1:16, Survey_Type != "RDFC") |>
    dplyr::mutate(Date = base::as.Date(base::ISOdate(SurveyYear, 7, 1)))
  
  Biomass <- arrow::read_feather("Tidy_Data/Biomass.feather") |> 
    dplyr::mutate(IslandName = gsub(" Island", "", IslandName),
                  IslandName = factor(IslandName, levels = Island_Levels_Short)) |> 
    dplyr::filter(Reference == TRUE, SurveyYear > 2004) |>
    dplyr::mutate(Date = base::as.Date(base::ISOdate(SurveyYear, 7, 1)))
  
  Biomass_Orginal_16 <- arrow::read_feather("Tidy_Data/Biomass.feather") |> 
    dplyr::mutate(IslandName = gsub(" Island", "", IslandName),
                  IslandName = factor(IslandName, levels = Island_Levels_Short)) |> 
    dplyr::filter(SiteNumber %in% 1:16) |>
    dplyr::mutate(Date = base::as.Date(base::ISOdate(SurveyYear, 7, 1)))
  
  RPC <- arrow::read_feather("Tidy_Data/RPC_Cover.feather") |> 
    dplyr::mutate(IslandName = gsub(" Island", "", IslandName),
                  IslandName = factor(IslandName, levels = Island_Levels_Short)) |> 
    dplyr::filter(Reference == TRUE, SurveyYear > 2004) |>
    dplyr::mutate(Date = base::as.Date(base::ISOdate(SurveyYear, 7, 1)))
  
  RPC_Original_16 <- arrow::read_feather("Tidy_Data/RPC_Cover.feather") |> 
    dplyr::mutate(IslandName = gsub(" Island", "", IslandName),
                  IslandName = factor(IslandName, levels = Island_Levels_Short)) |> 
    dplyr::filter(SiteNumber %in% 1:16) |>
    dplyr::mutate(Date = base::as.Date(base::ISOdate(SurveyYear, 7, 1)),
                  ReserveStatus = case_when(
                    SurveyYear < 2003 & SiteCode == "LC" ~ "Inside",
                    SurveyYear < 2003 & SiteCode == "CC" ~ "Inside",
                    SurveyYear < 2003 ~ "Outside",
                    TRUE ~ ReserveStatus))
  
   
  
  All_Ratios <- arrow::read_feather("Tidy_Data/Ratios.feather")
}

{ # Sizes  ----
  Benthic_Sizes <- arrow::read_feather("Tidy_Data/Benthic_Sizes.feather")
  Fish_Sizes <- arrow::read_feather("Tidy_Data/Fish_Sizes.feather")
  ARM_Sizes <- arrow::read_feather("Tidy_Data/ARMs.feather")
  ARM_par_Sizes <- arrow::read_feather("Tidy_Data/ARMs_par.feather")
}

{ # Report Text   -----
  Text <- arrow::read_feather("Tidy_Data/Text.feather")
  Acronyms <- dplyr::arrange(data.table::fread("Meta_Data/Acronyms.csv", encoding = "Latin-1"))
}

{ # GLMM  ----
  GLMM_Results <- arrow::read_feather("Tidy_Data/GLMM_Results.feather")
}








