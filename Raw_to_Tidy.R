#
#
#
#
#     Use for Manipulating data frames 
#
#
#     Use ALT + O to see outline
#
#

Export_END_Year <- 2019

{ # Library   ----
 
  library(tidyverse)
  library(tidymodels)
  library(ggpubr)
  library(glue)
  library(lubridate)
  library(rmarkdown)
  library(zoo)
  library(MASS)
  library(vegan)
  library(labdsv)
  library(lme4)
  library(car)
  library(knitr)
  library(tinytex)
  library(Cairo)
  library(ggnewscale)
  library(randomForest)
  library(pdp)
  library(broom)
  library(scales)
  library(arrow)
  library(indicspecies)
  

}

{ # Metadata    ----
  
  Species_Info <- readr::read_csv("Meta_Data/Species_Complete.csv")
  
  Site_Info <- readr::read_csv("Meta_Data/Site_Info.csv")
  
  Mixed_Data_xRef_Biomass <- readr::read_csv("Meta_Data/Mixed_Data_xref_Fish_Biomass.csv")
  
  Mixed_Data_xRef_Density <- readr::read_csv("Meta_Data/Mixed_Data_xref_Fish_Density.csv")
  
  Fish_Biomass_Species <- c(
    "Caulolatilus princeps", "ocean whitefish", "ocean_whitefish",
    "Chromis punctipinnis", "blacksmith", 
    "Embiotoca jacksoni", "black surfperch", "black_surfperch",
    "Embiotoca lateralis", "striped surfperch", "striped_surfperch",
    "Girella nigricans", "opaleye", 
    "Halichoeres semicinctus", "rock wrasse, female", "rock wrasse, male",   
                              "rock_wrasse_female", "rock_wrasse_male",  
    "Hypsypops rubicundus", "garibaldi",
    "Medialuna californiensis", "halfmoon", 
    "Ophiodon elongatus", "lingcod", 
    "Oxyjulis californica", "senorita",
    "Paralabrax clathratus", "kelp bass", "kelp_bass",
    "Rhacochilus toxotes", "rubberlip surfperch", "rubberlip_surfperch",
    "Rhacochilus vacca", "pile perch", "pile_perch",
    "Scorpaena guttata", "California scorpionfish", "California_scorpionfish",
    "Scorpaenichthys marmoratus", "cabezon",
    "Sebastes atrovirens", "kelp rockfish", "kelp_rockfish",
    "Sebastes chrysomelas", "black and yellow rockfish", "black_and_yellow_rockfish",
    "Sebastes mystinus", "blue rockfish", "blue_rockfish",  
    "Sebastes serranoides", "olive rockfish", "olive_rockfish",
    "Sebastes serriceps", "treefish", 
    "Semicossyphus pulcher", "California sheephead, male", "California sheephead, female",
                             "California_sheephead_male", "California_sheephead_female")
  
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

  Island_Code_Levels <- c("SR", "SC", "AN", "SB")
}

{ # SST Anomaly Index (ONI and PDO)   ----
  
  { # Oceanic Nino Index  ----
    oni <- read.table( # Read in  ONI to be added to all data
      "https://origin.cpc.ncep.noaa.gov/products/analysis_monitoring/ensostuff/detrend.nino34.ascii.txt",
      header = T) |>
      dplyr::mutate(Date = as.Date(ISOdate(YR, MON, 1)),
                    DateStart = as.Date(ISOdate(YR, MON, 1)),
                    DateEnd = ceiling_date(DateStart, "month")) |>
      dplyr::rename(ONI_ANOM = ANOM,
                    Month = MON,
                    SurveyYear = YR) |> 
      dplyr::select(SurveyYear, Month, Date, DateStart, DateEnd, ONI_ANOM) 
    
  }
  
  { # PDO  ----
    pdo <- read.table(
      "https://www.cpc.ncep.noaa.gov/products/GODAS/PDO/pdo_h300_pac_current.txt",
      header = T) |>
      dplyr::mutate(Date = as.Date(ISOdate(Year, Month, 1)),
                    DateStart = as.Date(ISOdate(Year, Month, 1)),
                    DateEnd = ceiling_date(DateStart, "month")) |>
      dplyr::rename(PDO_ANOM = PDO,
                    SurveyYear = Year) |> 
      dplyr::select(SurveyYear, Month, Date, DateStart, DateEnd, PDO_ANOM)
    
  }
  
  { # Full Index  ----
    SST_Anomaly_Index <- dplyr::left_join(oni, pdo) |> 
      arrow::write_feather("Tidy_Data/SST_Anomaly_Index.feather")
  }
  
  { # Mean ONI and PDO  ----
    SST_Anomaly_Index_Mean <- SST_Anomaly_Index |>
      dplyr::filter(SurveyYear > 2004, Date < base::as.Date(glue("{Export_END_Year + 1}-1-1"))) |> 
      dplyr::group_by(SurveyYear)  |> 
      dplyr::mutate(Mean_ONI_Anom = mean(ONI_ANOM),
                    Mean_PDO_Anom = mean(PDO_ANOM)) |> 
      dplyr::select(SurveyYear, Mean_ONI_Anom, Mean_PDO_Anom) |> 
      dplyr::distinct(SurveyYear, .keep_all = TRUE)
    
  }
  
}

{ # Density   ----
  
  { # 1 m Density     ----
    One_M_Density <- readr::read_csv(
      glue("Raw_Data/KFM_1mQuadrat_RawData_1982-{Export_END_Year}.txt"),   
      col_types = cols(CountA = col_number(), CountB = col_number())) |>
      dplyr::filter(IslandCode != "CL",
                    ScientificName != "Lithopoma gibberosa" | SurveyYear > 2002) |>
      dplyr::left_join(Site_Info) |>
      tidyr::separate(SurveyDate, c('Date','Time'),' ') |>
      dplyr::mutate(Date = lubridate::mdy(Date)) |>
      tidyr::pivot_longer(cols = c(CountA, CountB), values_to = "Count") |> 
      dplyr::filter(!is.na(Count), !is.na(CommonName),
                    !CommonName %in% c(
                      "giant kelp stipes > 1m",
                      'giant kelp, all',
                      "California sea palm, all",
                      "oar weed, all",
                      "Southern sea palm, all"),
                    !ScientificName %in% c(
                      "Undaria pinnatifida",
                      "Dictyoneuropsis reticulata/Agarum fimbriatum",
                      "Haliotis rufescens",
                      "Crassedoma giganteum",
                      "Kelletia kelletii",
                      "Oxylebius pictus",
                      "Pycnopodia helianthoides",
                      "Lytechinus anamesus",
                      "Sargassum horneri")) |> 
      dplyr::group_by(SiteNumber, IslandCode, IslandName, SiteCode, SiteName, Species, ScientificName,
                      CommonName, SurveyYear, Date, ReserveStatus, MeanDepth, Reference) |>
      dplyr::summarise(Count_To_Reuse = Count,
                       Area_Surveyed = ifelse(SurveyYear %in% 1985:1994, n() * 2, n()),
                       Total_Count = base::sum(Count),
                       Mean_Density = base::round(Total_Count / Area_Surveyed, 4), 
                       SD = base::round(stats::sd(Count), 4),
                       SE = base::round(SD / base::sqrt(Area_Surveyed), 4),
                       Survey_Type = "1 m² quads") |> 
      dplyr::ungroup() |>  
      dplyr::distinct(SiteNumber, IslandCode, IslandName, SiteCode, SiteName, ScientificName, CommonName, SurveyYear, 
                      Total_Count, Mean_Density, SD, SE, Area_Surveyed, .keep_all = TRUE) |> 
      dplyr::filter(ScientificName != "Pisaster giganteus" | SurveyYear < 1996,
                    CommonName != "giant kelp, adult (>1m)" | SurveyYear < 1996) |>
      dplyr::arrange(SiteNumber, SurveyYear, ScientificName) |>
      dplyr::select(SiteNumber, IslandCode, IslandName, SiteCode, SiteName, SurveyYear, Date,
                    Species, ScientificName, CommonName, Mean_Density, SD, SE, 
                    Area_Surveyed, MeanDepth, Survey_Type, ReserveStatus, Reference) 
  } 
  
  { # 5 m Density    ----
    
    Macro_Combo <- 
      readr::read_csv(
        glue::glue("Raw_Data/KFM_5mQuadrat_RawData_1996-{Export_END_Year}.txt")) |>
      dplyr::filter(Species == 2002.25 | Species == 2002.75) |>
      dplyr::mutate(CommonName = "giant kelp, adult (>1m)",
                    Species = 2002) |> 
      dplyr::group_by(SiteNumber, SurveyYear, QuadratNumber) |> 
      dplyr::mutate(Count = sum(Count)) |> 
      dplyr::distinct(.keep_all = TRUE) |> 
      dplyr::ungroup()
    
    Sargassum_Combo <- 
      readr::read_csv(
        glue::glue("Raw_Data/KFM_5mQuadrat_RawData_1996-{Export_END_Year}.txt")) |>   
      dplyr::filter(Species == 2016.00 | Species == 2016.50) |>
      dplyr::mutate(CommonName = "devil weed (all)",
                    Species = 2017) |> 
      dplyr::group_by(SiteNumber, SurveyYear, QuadratNumber) |> 
      dplyr::mutate(Count = sum(Count)) |> 
      dplyr::distinct(.keep_all = TRUE) |>
      dplyr::ungroup()
    
    Five_M_Density <- 
      readr::read_csv(
        glue::glue("Raw_Data/KFM_5mQuadrat_RawData_1996-{Export_END_Year}.txt")) |>
      base::rbind(Macro_Combo, Sargassum_Combo) |> 
      tidyr::separate(SurveyDate, c('Date','Time'),' ') |>
      dplyr::filter(IslandCode != "CL") |>
      dplyr::left_join(Site_Info) |>
      dplyr::mutate(Date = lubridate::mdy(Date)) |>
      dplyr::filter(!is.na(Count), !is.na(CommonName)) |> 
      dplyr::group_by(SiteNumber, IslandCode, IslandName, SiteCode, SiteName, Species, ScientificName, CommonName,
                      SurveyYear, ReserveStatus, MeanDepth, Reference) |>
      dplyr::summarise(Date = Date, # GI in 2003 was surveyed on 2 dates, cant group by
                       Area_Surveyed = dplyr::n() * 5,
                       Total_Count = base::sum(Count),
                       Mean_Density = base::round(Total_Count / Area_Surveyed, 4), 
                       SD = base::round(stats::sd(Count), 4),
                       SE = base::round(SD / base::sqrt(Area_Surveyed), 4),
                       Survey_Type = "5 m² quads") |> 
      dplyr::ungroup() |>
      dplyr::distinct(SiteNumber, IslandCode, IslandName, SiteCode, SiteName, CommonName,
                      ScientificName, SurveyYear, Total_Count, Mean_Density, SD, SE, Area_Surveyed, .keep_all = TRUE) |> 
      dplyr::filter(ScientificName != "Pisaster giganteus" | SurveyYear < 2014,
                    ScientificName != "Pisaster ochraceus" | SurveyYear < 2014,
                    ScientificName != "Undaria pinnatifida") |>
      dplyr::arrange(SiteNumber, SurveyYear, ScientificName) |> 
      dplyr::select(SiteNumber, IslandCode, IslandName, SiteCode, SiteName, SurveyYear, Date,
                    Species, ScientificName, CommonName, Mean_Density, SD, SE, 
                    Area_Surveyed, MeanDepth, Survey_Type, ReserveStatus, Reference) 
    
  }
  
  { # Bands Density    ----
    Bands_Density <- readr::read_csv(
      glue("Raw_Data/KFM_BandTransect_RawData_1982-{Export_END_Year}.txt"),   
      col_types = cols(CountA = col_number(), CountB = col_number())) |>
      dplyr::filter(IslandCode != "CL") |>
      dplyr::left_join(Site_Info) |>
      tidyr::separate(SurveyDate, c('Date','Time'),' ') |>
      dplyr::mutate(Date = lubridate::mdy(Date)) |>
      tidyr::pivot_longer(cols = c(CountA, CountB), values_to = "Count") |> 
      dplyr::filter(!is.na(Count), 
                    !is.na(CommonName),
                    ScientificName != "Sargassum horneri") |> 
      dplyr::group_by(SiteNumber, ScientificName, CommonName, SurveyYear, TransectNumber) |>
      dplyr::mutate(Count = sum(Count, na.rm = TRUE)) |>
      dplyr::ungroup() |>
      dplyr::distinct(SiteNumber, ScientificName, CommonName, SurveyYear, TransectNumber, .keep_all = TRUE) |> 
      dplyr::group_by(SiteNumber, IslandCode, IslandName, SiteCode, SiteName, Species, ScientificName,
                      CommonName, SurveyYear, ReserveStatus, MeanDepth, Reference) |>
      dplyr::summarise(Date = Date,
                       Area_Surveyed = ifelse(SurveyYear %in% 1983:1984, n() * 40, n() * 60),
                       Total_Count = base::sum(Count),
                       Mean_Density = base::round(Total_Count / Area_Surveyed, 4), 
                       SD = base::round(stats::sd(Count), 4),
                       SE = base::round(SD / base::sqrt(Area_Surveyed), 4),
                       Survey_Type = "bands transects") |> 
      dplyr::ungroup() |> 
      dplyr::distinct(IslandCode, IslandName, SiteCode, SiteName, ScientificName, CommonName, 
                      SurveyYear,  Mean_Density, ReserveStatus, Reference, .keep_all = TRUE) |>
      dplyr::select(SiteNumber, IslandCode, IslandName, SiteCode, SiteName, SurveyYear, Date,
                    Species, ScientificName, CommonName, Mean_Density, SD, SE, 
                    Area_Surveyed, MeanDepth, Survey_Type, ReserveStatus, Reference) 
    
  }
  
  { # Benthic Density   ----
    Benthic_Density <- base::rbind(One_M_Density, Five_M_Density, Bands_Density) 
  }
  
  { # RDFC    ----
    RDFC_Density <- 
      readr::read_csv(
        glue("Raw_Data/KFM_RovingDiverFishCount_RawData_1982-{Export_END_Year}.txt"),
        locale(encoding = "ISO-8859-1"), col_names = TRUE,
        col_types = cols(Count = col_double())) |>
      tidyr::separate(SurveyDate, c('Date','Time'),' ') |>
      dplyr::mutate(Date = lubridate::mdy(Date),
                    Count = as.double(Count)) |> 
      dplyr::left_join(Site_Info) |>
      dplyr::group_by(SiteCode, SurveyYear) |> 
      dplyr::filter(IslandCode != "CL",
                    Date == base::max(Date), SurveyYear > 2003, 
                    ExperienceLevel == "E") |>
      dplyr::ungroup() |> 
      dplyr::mutate(CommonName = gsub('ñ', 'n', CommonName),
                    Abundance = gsub("c", "C", Abundance),
                    Abundance = gsub("f", "F", Abundance),
                    Abundance = gsub("s", "S", Abundance),
                    Abundance = gsub("m", "M", Abundance),
                    Abundance = gsub("^$", NA, Abundance),
                    Abundance = gsub("-", NA, Abundance),
                    Score = ifelse(is.na(Score), 0, Score),
                    Abundance = ifelse(is.na(Abundance), "N", Abundance),
                    Count = ifelse(Score == 0, 0, Count)) |> 
      dplyr::filter(!CommonName %in% c(
        "black surfperch, all", "blacksmith, all", "blue rockfish, all", "kelp bass, all", 
        "kelp rockfish, all", "olive rockfish, all",  "opaleye, all", "pile perch, all", 
        "senorita, all", "striped surfperch, all")) |> 
      dplyr::group_by(SiteNumber, IslandCode, IslandName, SiteCode, SiteName, SurveyYear, Date, 
                      ScientificName, CommonName, ReserveStatus, Reference) |>
      dplyr::summarise(Count = mean(Count, na.rm = TRUE)) |> 
      dplyr::mutate(Count = ifelse(Count > 0 & Count < 1, 1, round(Count, 0))) |> 
      dplyr::ungroup() 
    # |> 
      # arrow::write_feather("Tidy_Data/RDFC_Count.feather")
    
  }
  
  { # VFT   ----
    VFT_Density <- 
      readr::read_csv(
        glue("Raw_Data/KFM_VisualFishTransect_RawData_1985-{Export_END_Year}.txt"),
        locale(encoding = "ISO-8859-1"), col_names = TRUE, col_types = NULL) |> 
      tidyr::separate(SurveyDate, c('Date','Time'),' ') |>
      dplyr::mutate(Date = as.Date(Date, format = '%m/%d/%Y'),
                    CommonName = gsub('ñ', 'n', CommonName)) |>
      dplyr::left_join(Site_Info) |>
      dplyr::rename(Count = CountA) |>
      dplyr::group_by(SiteNumber, SurveyYear) |>
      dplyr::filter(Date == max(Date), 
                    IslandCode != "CL") |>
      dplyr::ungroup() |> 
      dplyr::group_by(SiteNumber, SurveyYear, Species) |> 
      tidyr::pivot_wider(names_from = "Transect_Number", names_prefix = "T", values_from = "Count") |>
      dplyr::mutate(T1 = ifelse(SurveyYear < 1997, T1, T1 + T2),
                    T2 = ifelse(SurveyYear < 1997, T2, T3 + T4)) |> 
      dplyr::select(-T3, -T4) |>
      tidyr::pivot_longer(cols = c(T1, T2), values_to = "Count", names_to = "Transect_Number") |>
      dplyr::ungroup() |> 
      dplyr::group_by(SiteNumber, IslandCode, IslandName, SiteCode, SiteName, SurveyYear, Date,
                      CommonName, ScientificName, ReserveStatus, Reference, ReserveYear) |>
      dplyr::summarise(Mean_Density = round(sum(Count, na.rm = TRUE) / 600, 4)) |> 
      dplyr::ungroup() |>  
      dplyr::distinct(SiteNumber, IslandCode, IslandName, SiteCode, SiteName, SurveyYear, Date,
                      CommonName, ScientificName, Mean_Density, ReserveStatus, Reference) 
    # |>
      # arrow::write_feather("Tidy_Data/VFT_Density.feather") 
  }
  
  { # All Density    ----
      
    RDFC_Density_CSV <- RDFC_Density |> 
      dplyr::select(-ScientificName) |>
      tidyr::pivot_wider(names_from = CommonName, values_from = Count, values_fill = 0) |>
      tidyr::pivot_longer(cols = 10:161, names_to = "CommonName", values_to = "Count") |>
      dplyr::distinct(.keep_all = TRUE) |> 
      dplyr::left_join(
        Species_Info |>
          dplyr::select(ScientificName, CommonName) |>
          dplyr::distinct()) |>
      dplyr::mutate(Survey_Type = "RDFC", 
                    Mean_Density = Count / 2000)
    
    VFT_Density_CSV <- VFT_Density |> 
      dplyr::mutate(Survey_Type = "VFT", 
                    Count = Mean_Density * 2000)
    
    Fish_Density_CSV <- RDFC_Density_CSV |> 
      base::rbind(VFT_Density_CSV) |> 
      dplyr::left_join(
        Species_Info |>
          dplyr::select(ScientificName, Species, CommonName, Classification) |>
          dplyr::distinct()) |>
      dplyr::filter(!is.na(Species)) |>    
      dplyr::left_join(
        Site_Info |>
          dplyr::select(SiteName, ReserveYear, MeanDepth, Latitude, Longitude)) |> 
      dplyr::mutate(
        SE = NA, SD = NA,
        ReserveStatus = case_when(
          SurveyYear < 2003 & SiteCode == "LC" ~ "Inside",
          SurveyYear < 2003 & SiteCode == "CC" ~ "Inside",
          SurveyYear < 2003 ~ "Outside",
          TRUE ~ ReserveStatus)) |>
      dplyr::select(SiteNumber, IslandCode, IslandName, SiteCode, SiteName, SurveyYear, Date,
                    Species, ScientificName, CommonName, Mean_Density, SD, SE, Count,
                    MeanDepth, Survey_Type, ReserveStatus, Reference, ReserveYear, Latitude, Longitude, Classification)
    
    Benthic_Density_CSV <- Benthic_Density  |>
      dplyr::mutate(
        ReserveStatus = case_when(
          SurveyYear < 2003 & SiteCode == "LC" ~ "Inside",
          SurveyYear < 2003 & SiteCode == "CC" ~ "Inside",
          SurveyYear < 2003 ~ "Outside",
          TRUE ~ ReserveStatus),
        Count = Mean_Density * 2000) |> 
      dplyr::select(-Area_Surveyed) |>    
      dplyr::left_join(
        Site_Info |>
          dplyr::select(SiteName, ReserveYear, Latitude, Longitude)) |>
      dplyr::left_join(
        Species_Info |>
          dplyr::select(CommonName, Classification))
    
    Density_CSV <- Benthic_Density_CSV |> 
      base::rbind(Fish_Density_CSV) |>
      arrow::write_feather("Tidy_Data/Density.feather")
  }
  
}

{ # RPC % Cover   ----
  RPC_Cover <- readr::read_csv(
    glue::glue("Raw_Data/KFM_RandomPointContact_RawData_1982-{Export_END_Year}.txt"), 
    col_types = cols(CountA = col_number(), CountB = col_number(), 
                     CountC = col_number(), CountD = col_number())) |>
    tidyr::pivot_longer(cols = c(CountA, CountB, CountC, CountD), values_to = "Count") |>
    tidyr::separate(SurveyDate, c('Date','Time'),' ') |>
    dplyr::filter(IslandCode != "CL",
                  !ScientificName %in% c(
                    "Macrocystis, Pterygophora, and Eisenia combined",
                    "Leucetta losangelensis",
                    "Hydrozoa", "Balanus",
                    "Sargassum muticum",
                    "Polymastia pachymastia",
                    "Spirobranchus spinosus")) |>
    dplyr::mutate(
      Date = lubridate::mdy(Date),
      CommonName = as.factor(CommonName),
      ScientificName = as.factor(ScientificName),
      CommonName = forcats::fct_collapse(
        CommonName, 
        "Misc Inverts" = c(
          "Miscellaneous Invertebrates excluding Ophiothrix spiculata", 
          "Miscellaneous Invertebrates including Ophiothrix spiculata",
          "spiny brittle star")), 
      ScientificName = forcats::fct_collapse(
        ScientificName, 
        "Misc Inverts" = c(
          "Miscellaneous Invertebrates excluding Ophiothrix spiculata", 
          "Miscellaneous Invertebrates including Ophiothrix spiculata",
          "Ophiothrix spiculata")),
      CommonName = as.character(CommonName),
      ScientificName = as.character(ScientificName),
      Species = ifelse(ScientificName == "Misc Inverts", 13003, Species),
      ScientificName = dplyr::case_when(
        CommonName == "encrusting coralline algae" ~ "encrusting coralline algae",
        CommonName == "articulated coralline algae" ~ "articulated coralline algae",
        TRUE ~ ScientificName)) |> 
    dplyr::left_join(Site_Info) |> 
    dplyr::group_by(SiteNumber, IslandCode, IslandName, SiteCode, SiteName, SurveyYear, 
                    Species, ScientificName, CommonName, ReserveStatus, Reference) |>
    dplyr::filter(!is.na(Count), !is.na(CommonName), Date == base::max(Date)) |> 
    dplyr::summarise(
      Date = Date,
      Area_Surveyed = 
        ifelse(SurveyYear == 1982, 500, 
               ifelse(SurveyYear == 1983, 400,
                      ifelse(SurveyYear == 1984, 500,
                             ifelse(SurveyYear > 1984 & SurveyYear <= 1995, 1000, 600)))),
      Total_Count = sum(Count),
      Percent_Cover = 
        ifelse(SurveyYear == 1982, round((Total_Count / Area_Surveyed), 4), 
               ifelse(SurveyYear == 1983, round((Total_Count / Area_Surveyed), 4),
                      ifelse(SurveyYear == 1984, round((Total_Count / Area_Surveyed), 4),
                             ifelse(SurveyYear > 1984 & SurveyYear <= 1995, round((Total_Count / Area_Surveyed), 4), 
                                    round((Total_Count / Area_Surveyed), 4))))) * 100,
      SD = round(sd(Count), 4),
      SE = round(SD / sqrt(Area_Surveyed), 4)) |> 
    dplyr::ungroup() |>  
    dplyr::distinct(SiteCode, ScientificName, CommonName, SurveyYear, Percent_Cover, .keep_all = TRUE) |>
    dplyr::left_join(
      Site_Info |>
        dplyr::select(SiteName, ReserveYear)) |> 
    arrow::write_feather("Tidy_Data/RPC_Cover.feather")
  
  # unique(RPC_Cover$CommonName)
  # unique(RPC_Cover$ScientificName)
  
  # RPC_Substrate <- RPC_Cover |>
  #   dplyr::filter(ScientificName %in% c("Rock", "Cobble", "Sand")) |>
  #   dplyr::group_by(SiteNumber, ScientificName) |>
  #   dplyr::summarise(Percent_Cover = round(mean(Percent_Cover), 2)) |>
  #   tidyr::pivot_wider(names_from = ScientificName, values_from = Percent_Cover)
  # 
  # Site_Info <- Site_Info |>
  #   left_join(RPC_Substrate) |>
  #   arrow::write_feather("Meta_Data/Site_Info.feather")
  
}

{ # Diversity   ----
  
  { # Shannon's Index   ---- 
    
    Benthic_Counts <- Benthic_Density |> 
      dplyr::filter(!CommonName %in% c(
        "giant kelp, juvenile (<1m)",
        "giant kelp, subadult (>1m and no haptera above the primary dichotomy)",
        "giant kelp, adult (>1m and haptera above the primary dichotomy)",
        "Sargassum horneri, adult (>50cm or recepticles present)",
        "Sargassum horneri, juvenile (<50cm and no recepticles)")) |> 
      dplyr::mutate(Count = Mean_Density * 2000) |> 
      dplyr::group_by(SiteNumber, IslandCode, IslandName, SiteCode, SiteName, 
                      ScientificName, SurveyYear, ReserveStatus, Reference) |> 
      dplyr::summarise(Count = sum(Count)) |> 
      dplyr::ungroup()
  
    Fish_Counts_All <- VFT_Density |>
      dplyr::group_by(SiteNumber, IslandCode, IslandName, SiteCode, SiteName, 
                      ScientificName, SurveyYear, ReserveStatus, Reference) |> 
      dplyr::summarise(Count = Mean_Density * 2000) |> 
      dplyr::ungroup() |> 
      dplyr::select(SiteNumber, IslandCode, IslandName, SiteCode, SiteName, 
                    ScientificName, SurveyYear, Count, ReserveStatus, Reference)
    
    Counts_All <- rbind(Benthic_Counts, Fish_Counts_All) |>
      dplyr::group_by(SiteNumber, SurveyYear) |> 
      dplyr::mutate(richness_all = length(Count > 0)) |> 
      dplyr::ungroup() |> 
      tidyr::pivot_wider(names_from = ScientificName, values_fn = sum,
                         values_from = Count, values_fill = 0) 
    
    ShannonIndex_All <- Counts_All |>
      dplyr::select(-SiteNumber, -IslandCode, -IslandName, -SiteCode, -SiteName, 
                    -SurveyYear, -ReserveStatus, -Reference, -richness_all) |>
      vegan::diversity()
    
    Diversity_Shannon_All <- Counts_All |> 
      dplyr::select(SiteNumber, IslandCode, IslandName, SiteCode, 
                    SiteName, SurveyYear, ReserveStatus, Reference, richness_all) |>
      base::cbind("shannon_all" = ShannonIndex_All)
    
    Fish_Counts_2005 <- RDFC_Density |>
      dplyr::select(SiteNumber, IslandCode, IslandName, SiteCode, SiteName, 
                    ScientificName, SurveyYear, Count, ReserveStatus, Reference) |> 
      dplyr::filter(!ScientificName %in% 
                      c('Alloclinus holderi', 'Coryphopterus nicholsi', 
                        'Lythrypnus dalli', 'Sebastes')) 

    Counts_2005 <- rbind(Benthic_Counts, Fish_Counts_2005) |>
      dplyr::group_by(SiteNumber, SurveyYear) |> 
      dplyr::mutate(richness_2005 = length(Count > 0)) |> 
      dplyr::ungroup() |> 
      tidyr::pivot_wider(names_from = ScientificName, values_fn = sum,
                         values_from = Count, values_fill = 0) |> 
      dplyr::filter(SurveyYear > 2004)

    ShannonIndex_2005 <- Counts_2005 |>
      dplyr::select(-SiteNumber, -IslandCode, -IslandName, -SiteCode, -SiteName, 
                    -SurveyYear, -ReserveStatus, -Reference, richness_2005) |>
      vegan::diversity()
    
    Diversity_Shannon_2005 <- Counts_2005 |> 
      dplyr::select(SiteNumber, IslandCode, IslandName, SiteCode, 
                    SiteName, SurveyYear, ReserveStatus, Reference, richness_2005) |>
      base::cbind("shannon_2005" = ShannonIndex_2005)
  }
  
  { # Simpson's Index  ----
    RPC_Cover_Wide <- RPC_Cover |>
      dplyr::select(-SE, -SD, -CommonName, -Date, -Species,
                    -Area_Surveyed, -Total_Count, -ReserveYear) |>
      dplyr::filter(!ScientificName %in% c("Rock", "Sand", "Cobble", "Bare Substrate")) |>
      tidyr::pivot_wider(names_from = ScientificName, values_fn = sum,
                         values_from = Percent_Cover, values_fill = 0)
    
    SimpsonIndex <- RPC_Cover_Wide |>
      dplyr::select(-SiteNumber, -IslandCode, -IslandName, -SiteCode, -SiteName,
                    -SurveyYear, -ReserveStatus, -Reference) |>
      vegan::diversity(index = "simpson")

    Diversity_Simpson <- RPC_Cover_Wide |> 
      dplyr::select(SiteNumber, IslandCode, IslandName, SiteCode, 
                    SiteName, SurveyYear, ReserveStatus, Reference) |>
      base::cbind("simpson" = SimpsonIndex) 
  }
  
  { # Diversity Complete   ----
    Diversity <- Diversity_Shannon_All  |> 
      dplyr::left_join(Diversity_Simpson) |> 
      dplyr::left_join(Diversity_Shannon_2005) |> 
      dplyr::left_join(
        Site_Info |> 
          dplyr::select(SiteNumber, ReserveYear, Latitude, Longitude)) |> 
      dplyr::mutate(
        ReserveStatus = case_when(
          SurveyYear < 2003 & SiteCode == "LC" ~ "Inside",
          SurveyYear < 2003 & SiteCode == "CC" ~ "Inside",
          SurveyYear < 2003 ~ "Outside",
          TRUE ~ ReserveStatus),
        ReserveColor = ifelse(ReserveStatus == "Inside", "green", "red")) |>
      dplyr::mutate(Date = base::as.Date(base::ISOdate(SurveyYear, 7, 1))) |> 
      dplyr::filter(SiteCode != "MM" | SurveyYear > 2007) |> 
      arrow::write_feather("Tidy_Data/Diversity.feather")
  }
  
}

{ # Size Frequencies & Biomass   ----
  
  { # Conversion Coefficients  ----
    Benthic_Biomass_Coversions <- 
      readr::read_csv("Meta_Data/Benthic_Biomass_Equations.csv")
    
    Fish_Biomass_Coversions <- 
      readr::read_csv("Meta_Data/Fish_Biomass_Coversions.csv")
    
    Fish_Bio <- 
      readr::read_csv("Meta_Data/Fish_Bio2.csv") |>
      dplyr::select(ScientificName, WL_a_corrected, WL_b)
  }
  
  { # Kelp Sizes  ----
    Kelp_Sizes <- 
      read_csv(
        glue::glue("Raw_Data/KFM_Macrocystis_RawData_1984-{Export_END_Year}.txt"),
        col_types = cols(PermanentObserverNumber = col_double())) |>  
      tidyr::separate(SurveyDate, c('Date','Time'),' ') |>
      dplyr::filter(IslandCode != "CL") |> 
      dplyr::mutate(Date = lubridate::mdy(Date)) |> 
      dplyr::left_join(Site_Info) |>  
      dplyr::rename(Size = Stipe_Count) |> 
      dplyr::mutate(CommonName = "giant kelp, adult (>1m)",
                    Biomass = Size * 8.477333) |> 
      dplyr::select(SiteNumber, IslandCode, IslandName, SiteCode, SiteName, SurveyYear, Date,
                    ScientificName, CommonName, Size, Biomass, ReserveStatus, Reference)
  }
  
  { # Gorgonian Sizes    ----
    Gorgonian_Sizes <- 
      readr::read_csv(
        glue::glue("Raw_Data/KFM_Gorgonians_RawData_1984-2014_ NHSF_ 2015-{Export_END_Year}.txt")) |> 
      tidyr::separate(SurveyDate, c('Date','Time'),' ') |>
      dplyr::filter(IslandCode != "CL") |>
      dplyr::mutate(Date = lubridate::mdy(Date)) |> 
      dplyr::left_join(Site_Info) |>  
      dplyr::rename(Size = Width_cm) |>
      tidyr::uncount(weights = Count) |>
    dplyr::mutate(
      Biomass = case_when(
        ScientificName ==  "Lophogorgia chilensis" ~ (.018 * 10 ^ 1.529) * Size ^ 1.529,
        ScientificName ==  "Muricea californica" ~ (.002 * 10 ^ 1.529) * Size ^ 2.001,
        ScientificName ==  "Muricea fruticosa" ~ (.002 * 10 ^ 1.529) * Size ^ 2.001)) |> 
      dplyr::select(SiteNumber, IslandCode, IslandName, SiteCode, SiteName, SurveyYear, Date,
                    ScientificName, CommonName, Size, Biomass, ReserveStatus, Reference)
  }
  
  { # Invertebrate Sizes   ----
    Invert_Sizes <- 
      readr::read_csv(
        glue::glue("Raw_Data/KFM_NaturalHabitatSizeFrequency_RawData_1985-{Export_END_Year}.txt")) |> 
      tidyr::separate(SurveyDate, c('Date','Time'),' ') |>
      dplyr::filter(IslandCode != "CL") |>
      dplyr::mutate(Date = lubridate::mdy(Date)) |>
      dplyr::left_join(Site_Info) |>
      dplyr::left_join(
        dplyr::select(
          Benthic_Biomass_Coversions, ScientificName, CommonName, Independent_variable, a, b)) |> 
      tidyr::uncount(weights = NoOfInd) |>  
      dplyr::rename(Size = Size_mm) |>
      dplyr::mutate(
        Biomass = case_when(
        ScientificName ==  "Macrocystis pyrifera" ~  Size * 8.477333,
        Independent_variable ==  "body diameter" ~  a * (Size * 2) ^ b,
        TRUE ~ a * Size ^ b)) |> 
      dplyr::select(SiteNumber, IslandCode, IslandName, SiteCode, SiteName, SurveyYear, Date,
                    ScientificName, CommonName, Size, Biomass, ReserveStatus, Reference)
   
    
  }
  
  { # Benthic Sizes  ----
    Benthic_Sizes <- base::rbind(Kelp_Sizes, Gorgonian_Sizes, Invert_Sizes) |>
      dplyr::group_by(SiteCode, SurveyYear, CommonName) |> 
      dplyr::mutate(Total_Count = length(Size), Mean_Size = mean(Size), Date = max(Date)) |> 
      dplyr::ungroup() |> 
      arrow::write_feather("Tidy_Data/Benthic_Sizes.feather") |> 
      dplyr::select(-Mean_Size, -Total_Count) 
  }
  
  { # Benthic Biomass Long  ----
    Benthic_Biomass <- Benthic_Density |> 
      dplyr::filter(CommonName %in% unique(Benthic_Sizes$CommonName)) |>
      dplyr::select(SiteNumber, IslandCode, IslandName, SiteCode, SiteName, SurveyYear, ScientificName, CommonName,        
                    Mean_Density, Survey_Type, ReserveStatus, Reference) |>
      dplyr::full_join(Benthic_Sizes) |>
      dplyr::filter(!ScientificName %in% c(
        "Haliotis assimilis",
        "Cypraea spadicea",
        # "Parastichopus parvimensis", # Only measured in early years, Not sure if should leave as count (Uncommented) 
        # or as biomass (commented). The R values look good on the regression, grouped by species and reserve status.
        "Centrostephanus coronatus",
        "Stylaster californicus")) |> 
      dplyr::group_by(SiteCode, ScientificName, SurveyYear) |> 
      dplyr::mutate(
        Biomass = case_when(
          is.na(Biomass) & Mean_Density == 0 ~ 0,
          is.na(Biomass) & is.na(Mean_Density) ~ 0,
          TRUE ~ Biomass),
        Mean_Density = case_when(
          Biomass > 0 & Mean_Density == 0 ~ n()/2000,
          Biomass > 0 & is.na(Mean_Density) ~ n()/2000,
          Biomass == 0 & is.na(Mean_Density) ~ 0,
          TRUE ~ Mean_Density),
        ReserveStatus = case_when(
          SurveyYear < 2003 & SiteCode == "LC" ~ "Inside",
          SurveyYear < 2003 & SiteCode == "CC" ~ "Inside",
          SurveyYear < 2003 ~ "Outside",
          TRUE ~ ReserveStatus)) |> 
      dplyr::ungroup() |> 
      dplyr::group_by(SiteNumber, IslandCode, IslandName, SiteCode, SiteName, SurveyYear,
                      ScientificName, CommonName, Mean_Density, Survey_Type, ReserveStatus, Reference) |> 
      dplyr::summarise(Mean_Biomass = sum((Biomass * Mean_Density)/n())) |>
      # dplyr::summarise(Mean_Biomass = mean(Biomass) * Mean_Density) |> # Does same thing
      # dplyr::distinct(SiteNumber, IslandCode, IslandName, SiteCode, SiteName, SurveyYear,
      #                 ScientificName, CommonName, Mean_Density, Survey_Type, ReserveStatus,
                      # Reference, .keep_all = TRUE) |>
      dplyr::ungroup() 
    
    Benthic_Regression_Tidy <- Benthic_Biomass |>
      group_by(CommonName, ReserveStatus) |>
      dplyr::do(broom::tidy(lm(Mean_Biomass ~ Mean_Density, ., na.action = na.exclude)))
    
    Benthic_Regression_Glance <- Benthic_Biomass %>%
      group_by(CommonName, ReserveStatus) %>%
      do(broom::glance(lm(Mean_Biomass ~ Mean_Density, ., na.action = na.exclude)))  %>% 
      dplyr::select(-statistic, -p.value) %>% 
      dplyr::full_join(Benthic_Regression_Tidy) %>% 
      readr::write_csv("Meta_Data/Benthic_Regression.csv")
    
    Benthic_Regression <- Benthic_Regression_Tidy |> 
      dplyr::select(-statistic, -p.value, -std.error) |> 
      tidyr::pivot_wider(names_from = term, values_from = estimate) |> 
      dplyr::rename(yint = "(Intercept)", b = Mean_Density) |> 
      dplyr::mutate(yint = ifelse(yint < 0, 0, yint))
    
    Benthic_partial_Biomass <- Benthic_Biomass |> 
      left_join(Benthic_Regression) |> 
      dplyr::left_join(
        Species_Info |> 
          dplyr::distinct(
            ScientificName, CommonNameSimple, Trophic_Broad, Targeted_Broad, 
            Recreational_Fishery, Commercial_Fishery)) |> 
      dplyr::mutate(
        CommonName = CommonNameSimple,
        Mean_Biomass = dplyr::case_when(
          is.na(Mean_Biomass) ~ yint + Mean_Density * b,
          TRUE ~ Mean_Biomass)) |> 
      dplyr::mutate(Date = base::as.Date(base::ISOdate(SurveyYear, 7, 1))) |>
      dplyr::select(SiteNumber, IslandCode, IslandName, SiteCode, SiteName,
                    SurveyYear, Date, ScientificName, CommonName, Mean_Density,
                    Mean_Biomass, Survey_Type, ReserveStatus, Reference, Trophic_Broad, 
                    Targeted_Broad, Recreational_Fishery, Commercial_Fishery)
    
    Benthic_total_Biomass <- Benthic_partial_Biomass |> 
      dplyr::group_by(SiteNumber, IslandCode, IslandName, SiteCode, SiteName,
                      SurveyYear, Date, ReserveStatus, Reference) |> 
      dplyr::summarise(Mean_Biomass = sum(Mean_Biomass, na.rm = TRUE)) |> 
      dplyr::mutate(ScientificName = "total benthic biomass",
                    CommonName = "total benthic biomass",
                    Mean_Density = NA, Survey_Type = 'Mixed', Trophic_Broad = 'Mixed', 
                    Targeted_Broad = 'Mixed', Recreational_Fishery = 'Mixed', Commercial_Fishery = 'Mixed') |> 
      dplyr::ungroup()
    
    Benthic_target_Biomass <- Benthic_partial_Biomass |>
      dplyr::group_by(SiteNumber, IslandCode, IslandName, SiteCode, SiteName,
                      SurveyYear, Date, ReserveStatus, Reference, Targeted_Broad) |>
      dplyr::summarise(Mean_Biomass = sum(Mean_Biomass, na.rm = TRUE)) |>
      dplyr::mutate(ScientificName = Targeted_Broad,
                    CommonName = Targeted_Broad,
                    Mean_Density = NA, Survey_Type = 'Mixed', Trophic_Broad = 'Mixed', 
                    Recreational_Fishery = 'Mixed', Commercial_Fishery = 'Mixed') |>
      dplyr::ungroup()
    
    Benthic_trophic_Biomass <- Benthic_partial_Biomass |>
      dplyr::group_by(SiteNumber, IslandCode, IslandName, SiteCode, SiteName,
                      SurveyYear, Date, ReserveStatus, Reference, Trophic_Broad) |>
      dplyr::summarise(Mean_Biomass = sum(Mean_Biomass, na.rm = TRUE)) |>
      dplyr::mutate(ScientificName = Trophic_Broad,
                    CommonName = Trophic_Broad,
                    Mean_Density = NA, Survey_Type = 'Mixed', Targeted_Broad = 'Mixed', 
                    Recreational_Fishery = 'Mixed', Commercial_Fishery = 'Mixed') |>
      dplyr::ungroup()
    
    Benthic_Mean_Biomass <- rbind(
      Benthic_partial_Biomass, Benthic_total_Biomass, 
      Benthic_target_Biomass, Benthic_trophic_Biomass)  |>
      dplyr::left_join(
        Species_Info |> 
          dplyr::distinct(ScientificName, Classification)) |> 
      dplyr::filter(Classification %in% c('Invertebrates', 'Algae'))
    
  }
  
  { # Fish Density for Biomass   ----
    RDFC_Biomass <-  
      readr::read_csv(
        glue("Raw_Data/KFM_RovingDiverFishCount_RawData_1982-{Export_END_Year}.txt"),
        locale(encoding = "ISO-8859-1"), col_names = TRUE,
        col_types = cols(Count = col_double())) |>
      tidyr::separate(SurveyDate, c('Date','Time'),' ') |>
      dplyr::mutate(Date = lubridate::mdy(Date),
                    Count = as.double(Count)) |> 
      dplyr::left_join(Site_Info) |>
      dplyr::group_by(SiteCode, SurveyYear) |> 
      dplyr::filter(
        IslandCode != "CL",
        Date == base::max(Date), SurveyYear > 2004, 
        ExperienceLevel == "E",
        ScientificName %in% Fish_Biomass_Species,
        !CommonName %in% c(
          "black surfperch, all", "blacksmith, all", 
          "blue rockfish, all", "kelp bass, all", 
          "kelp rockfish, all", "olive rockfish, all", 
          "opaleye, all", "pile perch, all", 
          "señorita, all", "striped surfperch, all")) |>
      dplyr::ungroup() |> 
      dplyr::mutate(
        CommonName = gsub("ñ", "n", CommonName),
        Abundance = gsub("c", "C", Abundance),
        Abundance = gsub("f", "F", Abundance),
        Abundance = gsub("s", "S", Abundance),
        Abundance = gsub("m", "M", Abundance),
        Abundance = gsub("^$", NA, Abundance),
        Abundance = gsub("-", NA, Abundance),
        Score = ifelse(is.na(Score), 0, Score),
        Abundance = ifelse(is.na(Abundance), "N", Abundance),
        Count = ifelse(Score == 0, 0, Count),
        CommonName = base::factor(CommonName),
        CommonName = forcats::fct_collapse(
          CommonName, 
          "California sheephead, female" = c(
            "California sheephead, female", "California sheephead, juvenile"),
          "rock wrasse" = c(
            "rock wrasse, male", "rock wrasse, female", "rock wrasse, juvenile")),
        CommonName = base::as.character(CommonName),
        CommonName = base::gsub(", adult", "", CommonName),
        CommonName = base::gsub(", subadult", "", CommonName),
        CommonName = base::gsub(", juvenile", "", CommonName)) |>
      dplyr::group_by(SiteNumber, IslandCode, IslandName, SiteCode, SiteName, SurveyYear, Date,
                      ScientificName, CommonName, ReserveStatus, Reference, PermanentObserverNumber) |>
      dplyr::summarise(Count = sum(Count, na.rm = TRUE)) |>
      dplyr::group_by(SiteNumber, IslandCode, IslandName, SiteCode, SiteName, SurveyYear, Date, 
                      ScientificName, CommonName, ReserveStatus, Reference) |>
      dplyr::summarise(Count = mean(Count, na.rm = TRUE)) |> 
      dplyr::mutate(Count = ifelse(Count > 0 & Count < 1, 1, round(Count, 0))) |> 
      dplyr::ungroup() 
  }
  
  { # Fish Sizes  ----
    Fish_Sizes <- 
      data.table::fread(glue::glue(
        "Raw_Data/KFM_FishSizeFrequency_RawData_2007-{Export_END_Year}.txt")) |>
      dplyr::left_join(Site_Info) |>
      tidyr::separate(SurveyDate, c('Date','Time'),' ') |>
      dplyr::mutate(
        CommonName = gsub('ñ', 'n', CommonName),
        CommonName = gsub(", all", "", CommonName),
        CommonName = factor(CommonName),
        CommonName = forcats::fct_collapse(
          CommonName, 
          "California sheephead, female" = c(
            "California sheephead, female", "California sheephead, juvenile"),
          "rock wrasse" = c(
            "rock wrasse, male", "rock wrasse, female", "rock wrasse, juvenile"),
          "rockfish spp., juvenile" = c("rockfish spp.", "rockfish spp., juvenile")),
        Date = lubridate::mdy(Date)) |> 
      dplyr::rename(Size = TotalLength_cm) |>
      dplyr::group_by(SiteCode, SurveyYear) |> 
      dplyr::filter(Date == base::max(Date), SurveyYear > 2004) |> 
      dplyr::ungroup() |> 
      tidyr::uncount(weights = Count, .remove = TRUE) |>
      dplyr::left_join(Fish_Bio) |>
      dplyr::mutate(
        Biomass = case_when(
          ScientificName == "Embiotoca jacksoni" ~ WL_a_corrected * (0.799 * Size - 0.407) ^ WL_b,
          ScientificName == "Girella nigricans" ~ WL_a_corrected * (0.851 * Size) ^ WL_b,
          ScientificName == "Hypsypops rubicundus" ~ WL_a_corrected * (0.79 * Size + 0.42) ^ WL_b,
          ScientificName == "Medialuna californiensis" ~ WL_a_corrected * (0.92 * Size) ^ WL_b,
          ScientificName == "Scorpaenichthys marmoratus" ~ WL_a_corrected * Size ^ WL_b * 1000,
          ScientificName == "Ophiodon elongatus" ~ WL_a_corrected * Size ^ WL_b * 1000,
          TRUE ~ WL_a_corrected * Size ^ WL_b))  |> 
      dplyr::select(SiteNumber, IslandCode, IslandName, SiteCode, SiteName, SurveyYear, Date,
                    ScientificName, CommonName, Size, Biomass, ReserveStatus, Reference)  |>
      dplyr::group_by(SiteCode, SurveyYear, CommonName) |> 
      dplyr::mutate(Total_Count = length(Size), Mean_Size = mean(Size), Date = max(Date)) |> 
      dplyr::ungroup() |> 
      arrow::write_feather("Tidy_Data/Fish_Sizes.feather")
    
  }
  
  { # Fish Biomass   ----
    
    Fish_Biomass <- Fish_Sizes |>
      dplyr::filter(ScientificName %in% Fish_Biomass_Species) |> 
      dplyr::select(-Date, -Total_Count, -Mean_Size) |>
      dplyr::full_join(RDFC_Biomass) |>
      dplyr::group_by(SiteNumber, SurveyYear, CommonName) |>
      dplyr::mutate(
        Count = case_when(
          Biomass > 0 & (is.na(Count) | Count == 0) ~ as.double(dplyr::n()),
          Biomass == 0 & is.na(Count) ~ 0,
          TRUE ~ Count)) |> 
      dplyr::ungroup() |>
      dplyr::group_by(SiteNumber, IslandCode, SiteCode,  IslandName, SiteName, SurveyYear, 
                      ScientificName, CommonName, Count, ReserveStatus, Reference) |> 
      dplyr::summarise(Mean_Biomass = sum(Biomass, na.rm = TRUE)) |>
      dplyr::ungroup() |> 
      tidyr::complete(nesting(SiteNumber, SiteCode, SiteName, IslandName,
                              IslandCode, ReserveStatus, Reference),
                      nesting(ScientificName, CommonName), SurveyYear,
                      fill = list(Mean_Biomass = NA, Count = NA)) |>
      dplyr::group_by(SiteNumber, IslandCode, SiteCode,  IslandName, SiteName, SurveyYear, 
                      ScientificName, CommonName, ReserveStatus, Reference) |> 
      dplyr::mutate(
        Mean_Biomass = case_when(
          Count > 0 & Mean_Biomass == 0 ~ -1,
          Count == 0 & is.na(Mean_Biomass) ~ 0,
          is.na(Count) & is.na(Mean_Biomass) ~ 0,
          TRUE ~ Mean_Biomass),
        Count = case_when(
          Mean_Biomass == 0 & is.na(Count) ~ 0,
          is.na(Count) & is.na(Mean_Biomass) ~ 0,
          TRUE ~ Count)) |>
      dplyr::ungroup() |> 
      dplyr::mutate(Mean_Biomass = ifelse(Mean_Biomass == -1, NA, Mean_Biomass))
    
  }
  
  { # Fish Biomass Long ----- 
    
    Fish_Regression <- Fish_Biomass |>
      dplyr::group_by(CommonName, ReserveStatus) |>
      dplyr::do(broom::tidy(lm(Mean_Biomass ~ Count, ., na.action = na.exclude))) |> 
      dplyr::select(-statistic, -p.value, -std.error) |>
      tidyr::pivot_wider(names_from = term, values_from = estimate) |>
      dplyr::rename(yint = "(Intercept)", b = Count) |>
      dplyr::mutate(yint = ifelse(yint < 0, 0, yint))
    
    Fish_partial_Biomass <- Fish_Biomass |>
      left_join(Fish_Regression) |>
      dplyr::left_join(
        Species_Info |> 
          dplyr::distinct(
            ScientificName, Trophic_Broad, Targeted_Broad, 
            Recreational_Fishery, Commercial_Fishery)) |> 
      dplyr::mutate(
        Mean_Biomass = dplyr::case_when(
          is.na(Mean_Biomass) ~ yint + Count * b,
          Count == 0 ~ 0,
          TRUE ~ Mean_Biomass)) |>
      dplyr::mutate(Date = base::as.Date(base::ISOdate(SurveyYear, 7, 1))) |>
      dplyr::select(SiteNumber, IslandCode, IslandName, SiteCode, SiteName,
                    SurveyYear, Date, ScientificName, CommonName, Count,
                    Mean_Biomass, ReserveStatus, Reference, Trophic_Broad, Targeted_Broad, 
                    Recreational_Fishery, Commercial_Fishery) 
    
    Fish_total_Biomass <- Fish_partial_Biomass |> 
      dplyr::group_by(SiteNumber, IslandCode, IslandName, SiteCode, SiteName,
                      SurveyYear, Date, ReserveStatus, Reference) |> 
      dplyr::summarise(Mean_Biomass = sum(Mean_Biomass, na.rm = TRUE)) |> 
      dplyr::mutate(ScientificName = "total fish biomass",
                    CommonName = "total fish biomass",
                    Count = NA, Trophic_Broad = 'Mixed', 
                    Targeted_Broad = 'Mixed', Recreational_Fishery = 'Mixed', Commercial_Fishery = 'Mixed') |> 
      dplyr::ungroup()
    
    
    Fish_target_Biomass <- Fish_partial_Biomass |>
      dplyr::group_by(SiteNumber, IslandCode, IslandName, SiteCode, SiteName,
                      SurveyYear, Date, ReserveStatus, Reference, Targeted_Broad) |>
      dplyr::summarise(Mean_Biomass = sum(Mean_Biomass, na.rm = TRUE)) |>
      dplyr::mutate(ScientificName = Targeted_Broad,
                    CommonName = Targeted_Broad,
                    Count = NA, Trophic_Broad = 'Mixed', 
                    Recreational_Fishery = 'Mixed', Commercial_Fishery = 'Mixed') |>
      dplyr::ungroup()
    
    Fish_trophic_Biomass <- Fish_partial_Biomass |>
      dplyr::group_by(SiteNumber, IslandCode, IslandName, SiteCode, SiteName,
                      SurveyYear, Date, ReserveStatus, Reference, Trophic_Broad) |>
      dplyr::summarise(Mean_Biomass = sum(Mean_Biomass, na.rm = TRUE)) |>
      dplyr::mutate(ScientificName = Trophic_Broad,
                    CommonName = Trophic_Broad,
                    Count = NA, Targeted_Broad = 'Mixed', 
                    Recreational_Fishery = 'Mixed', Commercial_Fishery = 'Mixed') |>
      dplyr::ungroup()
    
    Fish_Mean_Biomass <- rbind(
      Fish_partial_Biomass, Fish_target_Biomass, 
      Fish_trophic_Biomass, Fish_total_Biomass) |> 
      dplyr::mutate(Survey_Type = "RDFC") |>
      dplyr::left_join(
        Species_Info |> 
          dplyr::distinct(ScientificName, Classification)) |> 
      dplyr::filter(Classification == 'Fish')
    
  }
  
  { # Fish Biomass Wide   ----
    Fish_Biomass_Wide <- Fish_Mean_Biomass |>
      dplyr::select(SiteNumber, IslandCode, IslandName, SiteCode, SiteName,
                    SurveyYear, CommonName, Mean_Biomass, ReserveStatus, Reference) |>
      tidyr::pivot_wider(names_from = CommonName, values_from = Mean_Biomass) |>
      dplyr::rename_with(~ base::gsub(",", "", .)) |>
      dplyr::rename_with(~ base::gsub(" ", "_", .))
  } 
  
  { # Biomass All   -----
    
    Benthic_Mean_Biomass_CSV <- Benthic_Mean_Biomass |> 
      dplyr::mutate(Count = Mean_Density * 2000) |> 
      dplyr::select(-Mean_Density)
    
    total_biomass <- Fish_Mean_Biomass |> 
      base::rbind(Benthic_Mean_Biomass_CSV) |> 
      dplyr::filter(!ScientificName %in% c(
        "total benthic biomass", "total fish biomass", 
        "Targeted", "Non-targeted",
        "Detritivore", "Herbivore", "Planktivore", "Producer", "Carnivore", "Piscivore")) |> 
      dplyr::group_by(SiteNumber, IslandCode, IslandName, SiteCode, SiteName,
                      SurveyYear, Date, ReserveStatus, Reference) |> 
      dplyr::summarise(Mean_Biomass = sum(Mean_Biomass, na.rm = TRUE),
                       Count = sum(Count, na.rm = T)) |> 
      dplyr::mutate(ScientificName = "total biomass",
                    CommonName = "total biomass",
                    Survey_Type = 'Mixed', Trophic_Broad = 'Mixed', Classification = 'Mixed', 
                    Targeted_Broad = 'Mixed', Recreational_Fishery = 'Mixed', Commercial_Fishery = 'Mixed') |> 
      dplyr::ungroup() |> 
      dplyr::filter(!SiteNumber > 21 | SurveyYear > 2005)
    
    
    Mean_Biomass_CSV <- Fish_Mean_Biomass |> 
      base::rbind(total_biomass, Benthic_Mean_Biomass_CSV) |> 
      dplyr::left_join(
        Site_Info |> 
          dplyr::select(SiteName, ReserveYear, Latitude, Longitude)) |> 
      dplyr::mutate(CommonName = gsub("giant kelp", "giant kelp, adult (>1m)", CommonName)) |> 
      arrow::write_feather("Tidy_Data/Biomass.feather")
    
  }
 
}

{ # Mixed Data (% Cover, Count, Biomass) for Random Forest Model   ----
  
  { # RDFC Counts for Mixed Data ---- 
    
    RDFC_Wide <- RDFC_Density |> 
      dplyr::filter(SurveyYear > 2004,
                    !ScientificName %in% Fish_Biomass_Species,
                    !CommonName %in% c( 
                      "blackeye goby", "blue-banded goby",
                      "island kelpfish", "copper rockfish, all",
                      "goby spp.", "kelp greenling, adult",
                      "rockfish spp.", "rockfish spp., adult")) |> 
      dplyr::mutate(
        CommonName = factor(CommonName),
        CommonName = forcats::fct_collapse(
          CommonName,
          "baitfish spp." = c("baitfish unidentified", "Pacific sardine", "northern anchovy"),
          "kelp greenling" = c("kelp greenling, male", "kelp greenling, female",  "kelp greenling, juvenile"),
          "Rockfish YOY" = c(
            "black and yellow/gopher rockfish, juvenile", 
            "kelp/gopher/copper/black and yellow rockfish, juvenile",
            "olive/yellowtail rockfish, juvenile",
            "rockfish spp., juvenile",
            "rosy rockfish, juvenile",
            "squarespot rockfish, juvenile",
            "stripetail rockfish, juvenile",
            "splitnose rockfish, juvenile",
            "bocaccio, juvenile",
            "brown rockfish, juvenile", 
            "calico rockfish, juvenile", 
            "canary rockfish, juvenile",
            "halfbanded rockfish, juvenile"), 
          "sculpin spp." = c(
            "lavender sculpin",
            "sailfin sculpin",
            "snubnose sculpin",
            "sculpin spp.",
            "spotfin sculpin",
            "scalyhead sculpin"),
          "surfperch spp." = c(
            "surfperch spp., adult",
            "surfperch spp., juvenile",
            "sharpnose surfperch",
            "sharpnose/white surfperch",
            "white surfperch"),
          "c_o turbot" = c("c-o turbot")),
        CommonName = as.character(CommonName),
        CommonName = gsub(", juvenile", "", CommonName),
        CommonName = gsub(", adult", "", CommonName)) |>
      dplyr::group_by(SiteNumber, IslandCode, IslandName, SiteCode, SiteName, SurveyYear, 
                      CommonName, ReserveStatus, Reference) |>
      dplyr::summarise(Mean_Density = sum(Count)) |>
      dplyr::ungroup() |>
      dplyr::select(SiteNumber, IslandCode, IslandName, SiteCode, SiteName, SurveyYear,
                    CommonName, Mean_Density, ReserveStatus, Reference) |>
      tidyr::pivot_wider(names_from = CommonName, values_from = Mean_Density, values_fill = 0) |> 
      dplyr::rename_with(~ base::gsub(",", "", .)) |> 
      dplyr::rename_with(~ base::gsub(" ", "_", .)) |> 
      dplyr::rename_with(~ base::gsub("-", "_", .)) |> 
      dplyr::rename_with(~ base::gsub("'", "", .)) 
  }
  
  { # VFT and Benthic Counts for Mixed Data   ----
    
    VFT_Counts <- VFT_Density |> 
      dplyr::mutate(
        CommonName = gsub(
          "California sheephead, juvenile", "California sheephead, female", CommonName),
        CommonName = gsub(
          "rock wrasse, juvenile", "rock wrasse, female", CommonName),
        CommonName = gsub(", juvenile", "", CommonName),
        CommonName = gsub(", subadult", "", CommonName),
        CommonName = gsub(", adult", "", CommonName)) |> 
      dplyr::group_by(SiteNumber, IslandCode, IslandName, SiteCode, SiteName, SurveyYear, 
                      ScientificName, CommonName, ReserveStatus, Reference) |> 
      dplyr::summarise(Mean_Density = sum(Mean_Density * 2000))
    
    Counts <- Benthic_Counts |>
      dplyr::left_join(
        Species_Info |> 
          dplyr::distinct(ScientificName, CommonNameSimple)) |> 
      dplyr::mutate(CommonName = CommonNameSimple,
                    Mean_Density = Count) |>  
      dplyr::select(-CommonNameSimple, -Count) |>
      base::rbind(VFT_Counts) |> 
      dplyr::filter(!CommonName %in% Benthic_Mean_Biomass$CommonName) |> 
      dplyr::mutate(
        ReserveStatus = case_when(
          SurveyYear < 2003 & SiteCode == "LC" ~ "Inside",
          SurveyYear < 2003 & SiteCode == "CC" ~ "Inside",
          SurveyYear < 2003 ~ "Outside",
          TRUE ~ ReserveStatus))
    
  }
  
  { # Mixed Data   -----
    
    Mixed_Data_All <- RPC_Cover |> 
      dplyr::filter(
        !ScientificName %in% c(
          "Macrocystis pyrifera", "Eisenia arborea",
          "Pterygophora californica", "Laminaria farlowii",
          "Sargassum horneri", "Bare Substrate",
          "Rock", "Cobble", "Sand")) |> 
      dplyr::left_join(dplyr::distinct(Species_Info, ScientificName, CommonNameSimple)) |> 
      dplyr::mutate(CommonName = CommonNameSimple,
                    Mean_Density = Percent_Cover) |> 
      dplyr::select(SiteNumber, IslandCode, IslandName, SiteCode, SiteName, SurveyYear, 
                    ScientificName, CommonName, Mean_Density, ReserveStatus, Reference) |> 
      dplyr::mutate(
        ReserveStatus = dplyr::case_when(
          SurveyYear < 2003 & SiteCode == "LC" ~ "Inside",
          SurveyYear < 2003 & SiteCode == "CC" ~ "Inside",
          SurveyYear < 2003 ~ "Outside",
          TRUE ~ ReserveStatus)) |> 
      base::rbind(Counts, Benthic_Mean_Biomass |>
                    dplyr::select(-Date, -Mean_Density,  -Survey_Type, -Classification, -Trophic_Broad, 
                                  -Targeted_Broad, -Recreational_Fishery, -Commercial_Fishery) |>
                    dplyr::mutate(Mean_Density = Mean_Biomass) |>
                    dplyr::select(-Mean_Biomass)) |>
      base::rbind(total_biomass |>
                    dplyr::select(-Date, -Count, -Survey_Type, -Classification, -Trophic_Broad, 
                                  -Targeted_Broad, -Recreational_Fishery, -Commercial_Fishery) |>
                    dplyr::mutate(Mean_Density = Mean_Biomass) |>
                    dplyr::select(-Mean_Biomass)) |>
      dplyr::select(SiteNumber, IslandCode, IslandName, SiteCode, SiteName, SurveyYear, 
                    CommonName, Mean_Density, ReserveStatus, Reference) |> 
      tidyr::pivot_wider(names_from = CommonName, values_from = Mean_Density, values_fill = 0) |>
      dplyr::left_join(
        Diversity_Shannon_All |> 
          dplyr::select(SiteNumber, SurveyYear, richness_all, shannon_all)) |>
      dplyr::left_join(
        Diversity_Simpson |> 
          dplyr::select(SiteNumber, SurveyYear, simpson)) |> 
      dplyr::rename_with(~ base::gsub(",", "", .)) |> 
      dplyr::rename_with(~ base::gsub(" ", "_", .)) |> 
      dplyr::rename_with(~ base::gsub("-", "_", .)) |> 
      dplyr::rename_with(~ base::gsub("'", "", .)) |> 
      dplyr::rename(Non_targeted_invert_biomass = Non_targeted,
                    Targeted_invert_biomass = Targeted,
                    Carnivore_invert = Carnivore, 
                    Herbivore_invert = Herbivore, 
                    Planktivore_invert = Planktivore) |> 
      dplyr::filter(SiteCode != "MM" | SurveyYear > 2004) |>
      arrow::write_feather("Tidy_Data/Mixed_Data_All.feather")
    
    Mixed_Data_2005 <- Mixed_Data_All |> 
      dplyr::select(-all_of(VFT_Species), -richness_all, -shannon_all) |> 
      dplyr::filter(SurveyYear > 2004) |>
      dplyr::left_join(Diversity_Shannon_2005) |> 
      dplyr::left_join(RDFC_Wide) |> 
      dplyr::left_join(Fish_Biomass_Wide) |> 
      base::replace(is.na(.), 0) |>
      dplyr::rename_with(~ base::gsub("-", "_", .)) |> 
      dplyr::rename(Non_targeted_fish_biomass = Non_targeted,
                    Targeted_fish_biomass = Targeted,
                    Carnivore_fish = Carnivore, 
                    Herbivore_fish = Herbivore, 
                    Planktivore_fish = Planktivore) |> 
      arrow::write_feather("Tidy_Data/Mixed_Data_2005.feather")
  }
  
}

{ # Random Forest Important Species    ----

  { # 2005 Reserve Model   -----
    
    Mixed_2005 <- arrow::read_feather("Tidy_Data/Mixed_Data_2005.feather") |> 
      dplyr::filter(Reference == TRUE, SurveyYear > 2004) |>
      dplyr::mutate(SurveyYear = factor(SurveyYear),
                    IslandCode = factor(IslandCode, levels = Island_Code_Levels),
                    ReserveStatus = factor(ReserveStatus)) |> 
      dplyr::select(-SiteNumber, -SiteName, 
                    -IslandName, -SiteCode)  
    
    RF_Reserve_Model_2005 <- randomForest::randomForest(
      data = Mixed_2005,
      ReserveStatus  ~ ., ntree = 3000, mtry = 8,
      importance = TRUE, proximity = TRUE, keep.forest = TRUE)
    
    saveRDS(RF_Reserve_Model_2005, "Models/RF_Reserve_Model_2005.rds")
    
    RF_VI_Res_2005 <- randomForest::importance(RF_Reserve_Model_2005) |>
      as.data.frame() |>
      tibble::rownames_to_column("Common_Name")
  }
 
  { # 2005 Island Model   -----
    
    RF_Island_Model_2005 <- randomForest::randomForest(
      data = Mixed_2005,
      IslandCode ~ ., ntree = 3000, mtry = 8,
      importance = TRUE, proximity = TRUE, keep.forest = TRUE)
    
    saveRDS(RF_Island_Model_2005, "Models/RF_Island_Model_2005.rds")
    
    RF_VI_Isl_2005 <- randomForest::importance(RF_Island_Model_2005) |>
      as.data.frame() |>
      tibble::rownames_to_column("Common_Name") |> 
      dplyr::rename(MeanDecreaseAccuracy_Isl = MeanDecreaseAccuracy,
                    MeanDecreaseGini_Isl = MeanDecreaseGini)
    
    RF_VI_2005 <- RF_VI_Isl_2005 |>  
      dplyr::full_join(RF_VI_Res_2005) |>
      dplyr::left_join(Mixed_Data_xRef_Biomass) |> 
      dplyr::select(Common_Name, CommonName, ScientificName, Inside, Outside, 
                    SR, SC, AN, SB, 
                    MeanDecreaseAccuracy, MeanDecreaseGini, MeanDecreaseAccuracy_Isl, MeanDecreaseGini_Isl,             
                    Data_Type, Classification, Targeted) |> 
      dplyr::mutate(Type = 'RF_2005')
    
  }
  
  { # RF All ranked variables   ----
    RF_Importance <- RF_VI_2005 |> 
      dplyr::filter(CommonName != "Detritivore invert") |> 
      dplyr::arrange(desc(MeanDecreaseAccuracy)) |>  
      arrow::write_feather("Tidy_Data/RF_Importance.feather")
  }
  
  { # Partial Dependence Plots  ----
    Top_30_MDA <- RF_Importance |>
      dplyr::arrange(desc(MeanDecreaseAccuracy)) |>  
      head(30) |> 
      droplevels()
    
    pdp <- lapply(Top_30_MDA$Common_Name[1:30], function(i){
      partial(RF_Reserve_Model_2005, pred.var = i) |>
        data.frame() |>
        dplyr::mutate(CommonName = i) |>
        dplyr::rename(x_var = i)
    })
    pdp2 <- pdp
    # pdp2[[20]][["x_var"]] <- as.numeric(pdp2[[20]][["x_var"]])
    partial_df <- bind_rows(pdp2) |>
      dplyr::mutate(CommonName = fct_inorder(CommonName)) |>  
      arrow::write_feather("Tidy_Data/PDP_data.feather")
  }
  
}

{ # nMDS Calculations    ----

    Density_Wide <- Density_CSV |> 
      dplyr::filter(
        Survey_Type != "VFT", SurveyYear > 2004, Reference == TRUE,
        !ScientificName %in% c("Alloclinus holderi", "Coryphopterus nicholsi", "Lythrypnus dalli") | Survey_Type == "1 m² quads") |> 
      dplyr::select(SiteNumber, IslandCode, IslandName, SiteCode, SiteName, SurveyYear,
                    CommonName, Count, ReserveStatus) |>  
      tidyr::pivot_wider(names_from = CommonName, values_from = Count, values_fill = 0)
    
    anosim_table <- data.frame(SurveyYear = integer(), P_val = double(), R_statistic = double())
    
    df_ellipse <- data.frame(SurveyYear = integer(), IslandName = character())
    
    nMDS_2D_2005 <- data.frame(
      NMDS1 = double(), NMDS2 = double(), SiteCode = character(), SiteName = character(),
      IslandName = character(), ReserveStatus = character(), SurveyYear = integer())
    
    for (k in unique(Density_Wide$SurveyYear)) {
      nMDS_Table <- Density_Wide |>
        filter(SurveyYear %in% k) |>
        arrange(IslandName) |> 
        droplevels()
      
      nMDS <- nMDS_Table |>
        dplyr::select(-SiteNumber, -IslandCode, - IslandName, -SiteCode,
                      -SiteName, - SurveyYear, - ReserveStatus) |>
        metaMDS(k = 2, trymax = 100)
      
      data_scores <- as.data.frame(scores(nMDS))
      data_scores$SiteCode <- nMDS_Table$SiteCode
      data_scores$SiteName <- nMDS_Table$SiteName
      data_scores$IslandName <- nMDS_Table$IslandName
      data_scores$ReserveStatus <- nMDS_Table$ReserveStatus
      data_scores$SurveyYear <- k
      
      nMDS_2D_2005 <- rbind(data_scores, nMDS_2D_2005)
      
      plot.new() # this is here because ordiellipse pops an error that plot.new() hasn't been opened yet
      ellipses <- ordiellipse(nMDS, nMDS_Table$IslandName, 
                              display = "sites", kind = "sd", conf = 0.95, label = T)
      
      
      veganCovEllipse <- function (cov, center = c(0, 0), scale = 1, npoints = 100) {
        theta <- (0:npoints) * 2 * pi/npoints
        Circle <- cbind(cos(theta), sin(theta))
        t(center + scale * t(Circle %*% chol(cov)))
      }
      
      for(g in unique(nMDS_Table$IslandName)){
        df_ellipse <- rbind(
          df_ellipse, 
          cbind(as.data.frame(with(
            nMDS_Table[nMDS_Table$IslandName == g,],
            veganCovEllipse(ellipses[[g]]$cov,
                            ellipses[[g]]$center, 
                            ellipses[[g]]$scale))),
            IslandName=g,
            SurveyYear = k))
      }
      
      # df_ellipse$SurveyYear <- k
      
      anosim_output <- nMDS_Table |>
        dplyr::select(-IslandCode, -IslandName, -SiteCode, -SiteName, -SurveyYear, -ReserveStatus) |>
        anosim(nMDS_Table$IslandName)
      
      anosim_table <- anosim_table |>
        add_row(SurveyYear = k, P_val = anosim_output$signif, R_statistic= anosim_output$statistic)
      
    }
    
    nMDS_2D_2005 |>
      dplyr::left_join(Site_Info) |> 
      dplyr::select(SiteNumber, IslandCode, IslandName, SiteCode, SiteName, SurveyYear, 
                    NMDS1, NMDS2, ReserveStatus, Reference) |>
    arrow::write_feather("Tidy_Data/nMDS.feather")
    
    df_ellipse |>
      dplyr::left_join(Site_Info) |> 
      dplyr::select(SiteNumber, IslandCode, IslandName, SiteCode, SiteName, SurveyYear, 
                    NMDS1, NMDS2, ReserveStatus, Reference) |>
      arrow::write_feather("Tidy_Data/ellipses.feather")
    
    arrow::write_feather(anosim_table, "Tidy_Data/ANOSIM.feather")
  
}

{ # GLMMs  ----
  
  { # Diversity  ----
    Diversity_2005 <- Diversity |> 
      dplyr::filter(Reference == TRUE, SurveyYear > 2004) |> 
      dplyr::left_join(SST_Anomaly_Index_Mean)
    
    div_d_v_names <- c("shannon_2005", "richness_2005", "simpson")
    div_results <- data.frame(`Dependent Variable` = character(), `Independent Variable` = character(), 
                              `F` = double(), Df = integer(), Df.res = integer(), `Pr(>F)` = double())
    
    for (y in div_d_v_names){
      form <- formula(paste(y, "~ ReserveStatus * IslandCode + Mean_ONI_Anom + Mean_PDO_Anom  + (1 | SurveyYear)"))
      model <- car::Anova(test = "F", lme4::lmer(data = Diversity_2005, formula = form)) |> 
        as.data.frame() |> 
        rownames_to_column(var = "Independent Variable") |> 
        dplyr::mutate(`Dependent Variable` = y)
      
      div_results <- rbind(div_results, model)
    }
    div_results <- dplyr::select(div_results, `Dependent Variable`, `Independent Variable`, 
                                 `F`, Df, Df.res, `Pr(>F)`)  |> 
      dplyr::mutate(Data_Type = "Calculated Value")
  }
  
  { # Counts  ----
    Count_Wide <- Density_CSV |> 
      dplyr::filter(Survey_Type != "VFT", SurveyYear > 2004, Reference == TRUE,
                    !ScientificName %in% c(
                      "Alloclinus holderi", "Coryphopterus nicholsi", "Lythrypnus dalli") | Survey_Type == "1 m² quads",
                    !CommonName %in% c("white abalone", "wakame, adult", "wakame, juvenile",
                                       "opaleye, juvenile", "northern ronquil",
                                       "black croaker","northern anchovy",                                              
                                       "leopard shark","California halibut",                                              
                                       "wolf eel", "monkeyface prickleback",                                          
                                       "sarcastic fringehead", "plainfin midshipman",                                             
                                       "walleye surfperch", "ocean sunfish")) |> 
      dplyr::select(SiteNumber, IslandCode, IslandName, SiteCode, SiteName, SurveyYear,
                    CommonName, Count, ReserveStatus) |>  
      tidyr::pivot_wider(names_from = CommonName, values_from = Count, values_fill = 0) |> 
      dplyr::rename_with(~ base::gsub(",", "", .)) |> 
      dplyr::rename_with(~ base::gsub(" ", "_", .)) |> 
      dplyr::rename_with(~ base::gsub("-", "_", .)) |> 
      dplyr::rename_with(~ base::gsub("'", "", .)) |> 
      dplyr::rename_with(~ base::gsub("<", "", .)) |> 
      dplyr::rename_with(~ base::gsub(">", "", .)) |> 
      dplyr::rename_with(~ base::gsub("1", "", .)) |> 
      dplyr::rename_with(~ base::gsub("0", "", .)) |> 
      dplyr::rename_with(~ base::gsub("5", "", .)) |> 
      dplyr::rename_with(~ base::gsub("[()]", "", .)) |> 
      dplyr::left_join(SST_Anomaly_Index_Mean)
    
    count_d_v_names <- names(dplyr::select(Count_Wide, 8:103))
    count_results <- data.frame(`Dependent Variable` = character(), `Independent Variable` = character(), 
                                `F` = double(), Df = integer(), Df.res = integer(), `Pr(>F)` = double())
    
    for (y in count_d_v_names){
      form <- formula(paste(y, "~ ReserveStatus * IslandCode + Mean_ONI_Anom + Mean_PDO_Anom + (1 | SurveyYear)"))
      model <- car::Anova(test = "F", lme4::lmer(data = Count_Wide, formula = form)) |> 
        as.data.frame() |> 
        rownames_to_column(var = "Independent Variable") |> 
        dplyr::mutate(`Dependent Variable` = y)
      
      count_results <- rbind(count_results, model)
    }
    count_results <- dplyr::select(count_results, `Dependent Variable`, `Independent Variable`, 
                                   `F`, Df, Df.res, `Pr(>F)`) |> 
      dplyr::mutate(Data_Type = "Count")
  }
  
  { # Biomass  ----
    Biomass_Wide <- Mean_Biomass_CSV |> 
      dplyr::filter(SurveyYear > 2004, Reference == TRUE,
                    !CommonName %in% c(
                      "Detritivore", "Producer", "Non-targeted", "Targeted", "Carnivore",                   
                      "Herbivore", "Piscivore", "Planktivore"                  
                    )) |> 
      dplyr::select(SiteNumber, IslandCode, IslandName, SiteCode, SiteName, SurveyYear,
                    CommonName, Mean_Biomass, ReserveStatus) |>  
      tidyr::pivot_wider(names_from = CommonName, values_from = Mean_Biomass, values_fill = 0) |> 
      dplyr::rename_with(~ base::gsub(",", "", .)) |> 
      dplyr::rename_with(~ base::gsub(" ", "_", .)) |> 
      dplyr::rename_with(~ base::gsub("-", "_", .)) |>
      dplyr::rename_with(~ base::gsub("'", "", .)) |>
      dplyr::rename_with(~ base::gsub("<", "", .)) |>
      dplyr::rename_with(~ base::gsub(">", "", .)) |>
      dplyr::rename_with(~ base::gsub("1m", "", .)) |>
      dplyr::rename_with(~ base::gsub("[()]", "", .)) |> 
      dplyr::left_join(SST_Anomaly_Index_Mean)
    
    biomass_d_v_names <- names(dplyr::select(Biomass_Wide, 8:53))
    biomass_results <- data.frame(`Dependent Variable` = character(), `Independent Variable` = character(), 
                                `F` = double(), Df = integer(), Df.res = integer(), `Pr(>F)` = double())
    
    for (y in biomass_d_v_names){
      form <- formula(paste(y, "~ ReserveStatus * IslandCode + Mean_ONI_Anom + Mean_PDO_Anom + (1 | SurveyYear)"))
      model <- car::Anova(test = "F", lme4::lmer(data = Biomass_Wide, formula = form)) |> 
        as.data.frame() |> 
        rownames_to_column(var = "Independent Variable") |> 
        dplyr::mutate(`Dependent Variable` = y)
      
      biomass_results <- rbind(biomass_results, model)
    }
    biomass_results <- dplyr::select(biomass_results, `Dependent Variable`, `Independent Variable`, 
                                   `F`, Df, Df.res, `Pr(>F)`) |> 
      dplyr::mutate(Data_Type = "Biomass")
  }
  
  { # All Results ----
    GLMM_Results <- rbind(div_results, count_results, biomass_results) |> 
      arrow::write_feather("Tidy_Data/GLMM_Results.feather")
  }

}

{ # Indicator Species Analysis   ----
  
  multipatt_island_list <- c(rep("AN", 3), rep("SB", 3), rep("SC", 3), rep("SR", 3))
  
  Count_Wide <- arrow::read_feather("Tidy_Data/Density.feather") |> 
    dplyr::filter(Survey_Type != "VFT", SurveyYear > 2004, Reference == TRUE,
                  !ScientificName %in% c(
                    "Alloclinus holderi", "Coryphopterus nicholsi", "Lythrypnus dalli") | Survey_Type == "1 m² quads",
                  !CommonName %in% c("white abalone", "wakame, adult", "wakame, juvenile",
                                     "opaleye, juvenile", "northern ronquil",
                                     "black croaker","northern anchovy",                                              
                                     "leopard shark","California halibut",                                              
                                     "wolf eel", "monkeyface prickleback",                                          
                                     "sarcastic fringehead", "plainfin midshipman",                                             
                                     "walleye surfperch", "ocean sunfish")) |> 
    dplyr::select(SiteNumber, IslandCode, IslandName, SiteCode, SiteName, SurveyYear,
                  CommonName, Count, ReserveStatus) |>  
    tidyr::pivot_wider(names_from = CommonName, values_from = Count, values_fill = 0) |> 
    dplyr::rename_with(~ base::gsub(",", "", .)) |> 
    dplyr::rename_with(~ base::gsub(" ", "_", .)) |> 
    dplyr::rename_with(~ base::gsub("-", "_", .)) |> 
    dplyr::rename_with(~ base::gsub("'", "", .)) |> 
    dplyr::rename_with(~ base::gsub("<", "", .)) |> 
    dplyr::rename_with(~ base::gsub(">", "", .)) |> 
    dplyr::rename_with(~ base::gsub("1", "", .)) |> 
    dplyr::rename_with(~ base::gsub("0", "", .)) |> 
    dplyr::rename_with(~ base::gsub("5", "", .)) |> 
    dplyr::rename_with(~ base::gsub("[()]", "", .))
  
  ISA <- 
    lapply(
      unique(Count_Wide$ReserveStatus), function(rs){
        lapply(
          unique(Count_Wide$SurveyYear), function(yr){
            Count_Wide |>
              dplyr::filter(ReserveStatus %in% rs) |>
              dplyr::filter(SurveyYear %in% yr) |>
              dplyr::select(-SiteNumber,-IslandCode, -IslandName, -SiteCode, -SiteName, - SurveyYear, - ReserveStatus) |>
              indicspecies::multipatt(multipatt_island_list, func = "r.g", control = how(nperm = 1000)) |>  
              base::getElement('sign') |> 
              tibble::rownames_to_column("Common_Name") |>
              dplyr::mutate(Year = yr, ReserveStatus = rs)
          })
      })
  
  ISA_df <- bind_rows(ISA) |>
    dplyr::filter(p.value < 0.05) |> 
    dplyr::rename(`Santa Rosa` = s.SR, `Santa Cruz` = s.SC, Anacapa = s.AN, `Santa Barbara` = s.SB) |> 
    tidyr::pivot_longer(cols = c(`Santa Rosa`, `Santa Cruz`, Anacapa, `Santa Barbara`), names_to = "IslandName", values_to = "isl_values") |> 
    dplyr::filter(isl_values > 0)
  
  # df <- arrow::read_feather("Tidy_Data/Mixed_Data_2005.feather") |> 
  #   dplyr::filter(Reference == TRUE, SurveyYear > 2004) |>
  #   dplyr::mutate(SurveyYear = factor(SurveyYear),
  #                 IslandCode = factor(IslandCode, levels = Island_Code_Levels),
  #                 ReserveStatus = factor(ReserveStatus)) 
  
  # ISA <- 
  #   lapply(
  #     unique(df$ReserveStatus), function(rs){
  #       lapply(
  #         unique(df$SurveyYear), function(yr){
  #           df |>
  #             dplyr::filter(ReserveStatus %in% rs) |>
  #             dplyr::filter(SurveyYear %in% yr) |>
  #             dplyr::select(-SiteNumber,-IslandCode, -IslandName, -SiteCode, -SiteName, - SurveyYear, - ReserveStatus) |>
  #             indicspecies::multipatt(multipatt_island_list, func = "r.g", control = how(nperm = 1000)) |>  
  #             base::getElement('sign') |> 
  #             tibble::rownames_to_column("Common_Name") |>
  #             dplyr::mutate(Year = yr, ReserveStatus = rs)
  #         })
  #     })
  # 
  # ISA_df <- bind_rows(ISA) |>
  #   dplyr::filter(p.value < 0.05) |> 
  #   dplyr::rename(`Santa Rosa` = s.SR, `Santa Cruz` = s.SC, Anacapa = s.AN, `Santa Barbara` = s.SB) |> 
  #   tidyr::pivot_longer(cols = c(`Santa Rosa`, `Santa Cruz`, Anacapa, `Santa Barbara`), names_to = "IslandName", values_to = "isl_values") |> 
  #   dplyr::filter(isl_values > 0)

 
}









{ # RATIOS - BEWARE... THESE TAKE A LOOOOONG TIME  -----------------
  
  { # Biomass Ratios   ----
    
    { # Ratio Functions   -----
      
      biomass_boot_ratio <- function (data, indices) {
        sample = data[indices, ]
        ratio = mean(sample$Mean_Biomass[sample$ReserveStatus == "Inside"])/
          mean(sample$Mean_Biomass[sample$ReserveStatus == "Outside"])
        return(ratio) 
      }
      
      density_boot_ratio <- function (data, indices) {
        sample = data[indices, ]
        ratio = mean(sample$Mean_Density[sample$ReserveStatus == "Inside"])/
          mean(sample$Mean_Density[sample$ReserveStatus == "Outside"])
        return(ratio) 
      }
      
    }
    
    { # Data  ----
      Biomass_Data <- arrow::read_feather("Tidy_Data/Biomass.feather") |>
        dplyr::filter(
          # SiteCode != "KH",
          !ScientificName %in% c(
            "total benthic biomass", "total fish biomass", "total biomass", "Targeted", "Non-targeted",
            "Detritivore", "Herbivore", "Planktivore", "Producer", "Carnivore", "Piscivore"),
          ScientificName != 'Muricea californica' | SurveyYear > 1990,
          ScientificName != 'Lithopoma gibberosa' | SurveyYear > 2002) |> 
        dplyr::group_by(SiteNumber, CommonName, SurveyYear) |> 
        dplyr::mutate(Mean_Biomass = Mean_Biomass + runif(1, min = .9, max = 1.1),
                      CommonName = factor(CommonName),
                      Targeted_Broad = factor(Targeted_Broad),
                      Trophic_Broad = factor(Trophic_Broad)) |> 
        dplyr::ungroup()
    }
    
    { # Species Level  ----
      Biomass_Species_Ratios <- tibble(
        CommonName = character(), SurveyYear = integer(), Classification = character(),
        Mean_Ratio = double(), CI_plus = double(), CI_minus = double())
      
      for (class in unique(Biomass_Data$Classification)) {
        class_filtered <- Biomass_Data |> 
          dplyr::filter(Classification == class)
        for (yr in unique(class_filtered$SurveyYear)){
          dropped_levels <- class_filtered |> 
            dplyr::filter(SurveyYear == yr) |> 
            droplevels()
          for (sp in levels(dropped_levels$CommonName)){
            d <- dropped_levels |>
              filter(CommonName == sp) 
            output <- boot::boot(data = d, statistic = biomass_boot_ratio, R = 1000)
            ci_boot <- boot::boot.ci(boot.out = output, conf = 0.95, type = "perc")
            Biomass_Species_Ratios <- Biomass_Species_Ratios |> 
              tibble::add_row(
                CommonName = sp, SurveyYear = yr, Classification = class,
                Mean_Ratio = ci_boot$t0, CI_minus = ci_boot$percent[4], CI_plus = ci_boot$percent[5])
          }
        }
      }
    }
    
    { # Total Class Level  ----
      Biomass_Total_Ratios <- tibble(
        CommonName = character(), SurveyYear = integer(), Classification = character(),
        Mean_Ratio = double(), CI_plus = double(), CI_minus = double())
      
      for (class in unique(Biomass_Data$Classification)) {
        class_filtered <- Biomass_Data |> 
          dplyr::filter(Classification == class)
        for(yr in unique(class_filtered$SurveyYear)){
          dropped_levels <- class_filtered |> 
            dplyr::filter(SurveyYear == yr) |> 
            droplevels() 
          output <- boot::boot(data = dropped_levels, statistic = biomass_boot_ratio, R = 5000)
          ci_boot <- boot::boot.ci(boot.out = output, conf = 0.95, type = "perc")
          Biomass_Total_Ratios <- Biomass_Total_Ratios |> 
            tibble::add_row(
              CommonName = paste("Total ", class, " Biomass", sep = ""), SurveyYear = yr, Classification = class, 
              Mean_Ratio = ci_boot$t0, CI_minus = ci_boot$percent[4], CI_plus = ci_boot$percent[5])
          
        }
      }
    }
    
    { # All Total Level  ----
      Biomass_All_Total_Ratios <- tibble(
        CommonName = character(), SurveyYear = integer(), Classification = character(),
        Mean_Ratio = double(), CI_plus = double(), CI_minus = double())
      
      for(yr in unique(Biomass_Data$SurveyYear)){
        dropped_levels <- Biomass_Data |> 
          dplyr::filter(SurveyYear == yr) |> 
          droplevels() 
        output <- boot::boot(data = dropped_levels, statistic = biomass_boot_ratio, R = 5000)
        ci_boot <- boot::boot.ci(boot.out = output, conf = 0.95, type = "perc")
        Biomass_All_Total_Ratios <- Biomass_All_Total_Ratios |> 
          tibble::add_row(CommonName = "total biomass", SurveyYear = yr, Classification = "Mixed", 
                          Mean_Ratio = ci_boot$t0, CI_minus = ci_boot$percent[4], CI_plus = ci_boot$percent[5])
        
        
      }
    }
    
    { # Targeted Level  ----
      Biomass_Target_Ratios <- tibble(
        CommonName = character(), SurveyYear = integer(), Classification = character(),
        Mean_Ratio = double(), CI_plus = double(), CI_minus = double())
      
      for (class in unique(Biomass_Data$Classification)) {
        class_filtered <- Biomass_Data |> 
          dplyr::filter(Classification == class)
        for(yr in unique(class_filtered$SurveyYear)){
          dropped_levels <- class_filtered |> 
            dplyr::filter(SurveyYear == yr) |> 
            droplevels()
          for(sp in levels(dropped_levels$Targeted_Broad)){
            d <- dropped_levels |>
              filter(Targeted_Broad == sp) 
            output <- boot::boot(data = d, statistic = biomass_boot_ratio, R = 5000)
            ci_boot <- boot::boot.ci(boot.out = output, conf = 0.95, type = "perc")
            Biomass_Target_Ratios <- Biomass_Target_Ratios |> 
              tibble::add_row(CommonName = sp, SurveyYear = yr, Classification = class, 
                              Mean_Ratio = ci_boot$t0, CI_minus = ci_boot$percent[4], CI_plus = ci_boot$percent[5])
          }
        }
      }
    }
    
    { # Trophic level ----
      Biomass_Trophic_Ratios <- tibble(
        CommonName = character(), SurveyYear = integer(), Classification = character(),
        Mean_Ratio = double(), CI_plus = double(), CI_minus = double())
      
      for (class in unique(Biomass_Data$Classification)) {
        class_filtered <- Biomass_Data |> 
          dplyr::filter(Classification == class)
        for(yr in unique(class_filtered$SurveyYear)){
          dropped_levels <- class_filtered |> 
            dplyr::filter(SurveyYear == yr) |> 
            droplevels()
          for(sp in levels(dropped_levels$Trophic_Broad)){
            d <- dropped_levels |>
              filter(Trophic_Broad == sp) 
            output <- boot::boot(data = d, statistic = biomass_boot_ratio, R = 5000)
            ci_boot <- boot::boot.ci(boot.out = output, conf = 0.95, type = "perc")
            Biomass_Trophic_Ratios <- Biomass_Trophic_Ratios |> 
              tibble::add_row(CommonName = sp, SurveyYear = yr, Classification = class, 
                              Mean_Ratio = ci_boot$t0, CI_minus = ci_boot$percent[4], CI_plus = ci_boot$percent[5])
          }
        }
      }
    }
    
    { # Data Output  ----
      
      Biomass_Ratios <- base::rbind(
        Biomass_Species_Ratios, 
        Biomass_Total_Ratios,
        Biomass_All_Total_Ratios,
        Biomass_Target_Ratios,
        Biomass_Trophic_Ratios) |> 
        dplyr::mutate(
          Date = lubridate::mdy(glue::glue('7-1-{SurveyYear}')),
          Survey_Type = 'Mixed',
          Metric = 'biomass_ratio') |>
        dplyr::left_join(
          Species_Info |> 
            dplyr::distinct(
              ScientificName, CommonName, Classification, Trophic_Broad, 
              Targeted_Broad, Recreational_Fishery, Commercial_Fishery)) |> 
        dplyr::filter(CommonName != "Targeted" | Classification != "Algae",
                      CommonName != "Producer" | Classification != "Algae",
                      CommonName != "Total Algae Biomass" | Classification != "Algae")
    }
    
  }
  
  { # Density Ratios   ----
    
    { # Benthic Density Ratios    -----
      
      { # Data    ----
        Density_Boot <- Benthic_Density_CSV |>
          dplyr::filter(
            # SiteCode != "KH",
            ScientificName != 'Muricea californica' | SurveyYear > 1990,
            ScientificName != 'Cypraea spadicea' | SurveyYear > 1983,
            ScientificName != 'Undaria pinnatifida',
            ScientificName != 'Haliotis assimilis',
            ScientificName != 'Haliotis sorenseni',
            ScientificName != 'Pisaster ochraceus') |>
          dplyr::left_join(
            Species_Info |> 
              dplyr::distinct(ScientificName, Trophic_Broad, Targeted_Broad, 
                              Recreational_Fishery, Commercial_Fishery)) |>
          dplyr::group_by(SiteNumber, CommonName, SurveyYear) |> 
          dplyr::mutate(Mean_Density = Mean_Density + runif(1, min = .9, max = 1.1),
                        CommonName = factor(CommonName),
                        Targeted_Broad = factor(Targeted_Broad),
                        Trophic_Broad = factor(Trophic_Broad)) |> 
          dplyr::ungroup()
      }
      
      { # Species Level  ----
        Density_Species_Ratio <- tibble(
          CommonName = character(), SurveyYear = integer(),
          Mean_Ratio = double(), CI_plus = double(), CI_minus = double())
        
        for(y in unique(Density_Boot$SurveyYear)){
          dropped <- Density_Boot |> 
            dplyr::filter(SurveyYear == y) |> 
            droplevels()
          for(s in levels(dropped$CommonName)){
            d <- dropped |>
              filter(CommonName == s) 
            output <- boot::boot(data = d, statistic = density_boot_ratio, R = 1000)
            ci_boot <- boot::boot.ci(boot.out = output, conf = 0.95, type = "perc")
            Density_Species_Ratio <- Density_Species_Ratio |> 
              tibble::add_row(CommonName = s, SurveyYear = y, 
                              Mean_Ratio = ci_boot$t0, CI_minus = ci_boot$percent[4], CI_plus = ci_boot$percent[5])
          }
        } 
      }
      
      { # Targeted Level  ----
        Density_Target_Species_Ratio <- tibble(
          CommonName = character(), SurveyYear = integer(),
          Mean_Ratio = double(), CI_plus = double(), CI_minus = double())
        
        for(yr in unique(Density_Boot$SurveyYear)){
          drop <- Density_Boot |> 
            dplyr::filter(SurveyYear == yr,
                          Targeted_Broad != 'Mixed') |> 
            droplevels()
          for(c in levels(drop$Targeted_Broad)){
            d <- drop |>
              filter(Targeted_Broad == c) 
            output <- boot::boot(data = d, statistic = density_boot_ratio, R = 1000)
            ci_boot <- boot::boot.ci(boot.out = output, conf = 0.95, type = "perc")
            Density_Target_Species_Ratio <- Density_Target_Species_Ratio |> 
              tibble::add_row(CommonName = c, SurveyYear = yr, 
                              Mean_Ratio = ci_boot$t0, CI_minus = ci_boot$percent[4], CI_plus = ci_boot$percent[5])
          }
        }
      }
      
      { # Trophic level ----
        Density_Trophic_Level_Ratios <- tibble(
          CommonName = character(), SurveyYear = integer(),
          Mean_Ratio = double(), CI_plus = double(), CI_minus = double())
        
        for(year in unique(Density_Boot$SurveyYear)){
          dropT <- Density_Boot |> 
            dplyr::filter(SurveyYear == year,
                          Trophic_Broad != 'Mixed Trophic Levels') |> 
            droplevels()
          for(t in levels(dropT$Trophic_Broad)){
            d <- dropT |>
              filter(Trophic_Broad == t) 
            output <- boot::boot(data = d, statistic = density_boot_ratio, R = 1000)
            ci_boot <- boot::boot.ci(boot.out = output, conf = 0.95, type = "perc")
            Density_Trophic_Level_Ratios <- Density_Trophic_Level_Ratios |> 
              tibble::add_row(CommonName = t, SurveyYear = year, 
                              Mean_Ratio = ci_boot$t0, CI_minus = ci_boot$percent[4], CI_plus = ci_boot$percent[5])
          }
        }
      }
      
      { # Data Outputs  ----
        
        Density_Ratios <- base::rbind(
          Density_Species_Ratio, 
          Density_Target_Species_Ratio, 
          Density_Trophic_Level_Ratios) |>
          dplyr::mutate(Date = lubridate::mdy(glue::glue('7-1-{SurveyYear}')),
                        Survey_Type = 'Mixed',
                        Metric = 'density_ratio') |>
          dplyr::left_join(
            Species_Info |> 
              dplyr::filter(
                Classification != "Fish" |
                  CommonName %in% c('island kelpfish', 'blackeye goby', 'blue-banded goby')) |>
              dplyr::distinct(
                ScientificName, CommonName, Classification, Trophic_Broad, 
                Targeted_Broad, Recreational_Fishery, Commercial_Fishery))
        
      }
      
    }
    
    { # RDFC Density Ratios    -----
      
      { # Data    ----
        RDFC_Density_Boot <- Fish_Density_CSV |>
          dplyr::filter(
            # SiteCode != "KH",
            Survey_Type == "RDFC",
            !ScientificName %in% 
              c('Coryphopterus nicholsi', 
                'Lythrypnus dalli', 
                'Alloclinus holderi')) |>
          dplyr::left_join(
            Species_Info |> 
              dplyr::distinct(ScientificName, Trophic_Broad, Targeted_Broad, 
                              Recreational_Fishery, Commercial_Fishery)) |>
          dplyr::group_by(SiteNumber, CommonName, SurveyYear) |> 
          dplyr::mutate(
            Mean_Density = Mean_Density + runif(1, min = .9, max = 1.1),
            CommonName = factor(CommonName),
            Targeted_Broad = factor(Targeted_Broad),
            Trophic_Broad = factor(Trophic_Broad)) |> 
          dplyr::ungroup()
      }
      
      { # Species Level  ----
        RDFC_Species_Ratio <- tibble(
          CommonName = character(), SurveyYear = integer(),
          Mean_Ratio = double(), CI_plus = double(), CI_minus = double())
        
        for(y in unique(RDFC_Density_Boot$SurveyYear)){
          dropped <- RDFC_Density_Boot |> 
            dplyr::filter(SurveyYear == y) |> 
            droplevels()
          for(s in levels(dropped$CommonName)){
            d <- dropped |>
              filter(CommonName == s) 
            output <- boot::boot(data = d, statistic = density_boot_ratio, R = 1000)
            ci_boot <- boot::boot.ci(boot.out = output, conf = 0.95, type = "perc")
            RDFC_Species_Ratio <- RDFC_Species_Ratio |> 
              tibble::add_row(CommonName = s, SurveyYear = y, 
                              Mean_Ratio = ci_boot$t0, CI_minus = ci_boot$percent[4], CI_plus = ci_boot$percent[5])
          }
        } 
      }
      
      { # Targeted Level  ----
        RDFC_Target_Species_Ratio <- tibble(
          CommonName = character(), SurveyYear = integer(),
          Mean_Ratio = double(), CI_plus = double(), CI_minus = double())
        
        for(yr in unique(RDFC_Density_Boot$SurveyYear)){
          drop <- RDFC_Density_Boot |> 
            dplyr::filter(SurveyYear == yr,
                          Targeted_Broad != 'Mixed') |> 
            droplevels()
          for(c in levels(drop$Targeted_Broad)){
            d <- drop |>
              filter(Targeted_Broad == c) 
            output <- boot::boot(data = d, statistic = density_boot_ratio, R = 1000)
            ci_boot <- boot::boot.ci(boot.out = output, conf = 0.95, type = "perc")
            RDFC_Target_Species_Ratio <- RDFC_Target_Species_Ratio |> 
              tibble::add_row(CommonName = c, SurveyYear = yr, 
                              Mean_Ratio = ci_boot$t0, CI_minus = ci_boot$percent[4], CI_plus = ci_boot$percent[5])
          }
        }
      }
      
      { # Trophic level ----
        RDFC_Trophic_Level_Ratios <- tibble(
          CommonName = character(), SurveyYear = integer(),
          Mean_Ratio = double(), CI_plus = double(), CI_minus = double())
        
        for(year in unique(RDFC_Density_Boot$SurveyYear)){
          dropT <- RDFC_Density_Boot |> 
            dplyr::filter(SurveyYear == year,
                          Trophic_Broad != 'Mixed Trophic Levels') |> 
            droplevels()
          for(t in levels(dropT$Trophic_Broad)){
            d <- dropT |>
              filter(Trophic_Broad == t) 
            output <- boot::boot(data = d, statistic = density_boot_ratio, R = 1000)
            ci_boot <- boot::boot.ci(boot.out = output, conf = 0.95, type = "perc")
            RDFC_Trophic_Level_Ratios <- RDFC_Trophic_Level_Ratios |> 
              tibble::add_row(CommonName = t, SurveyYear = year, 
                              Mean_Ratio = ci_boot$t0, CI_minus = ci_boot$percent[4], CI_plus = ci_boot$percent[5])
          }
        }
      }
      
      { # Data Outputs  ----
        
        RDFC_Density_Ratios <- base::rbind(
          RDFC_Species_Ratio,
          RDFC_Target_Species_Ratio, 
          RDFC_Trophic_Level_Ratios) |>
          dplyr::mutate(
            Date = lubridate::mdy(glue::glue('7-1-{SurveyYear}')),
            Survey_Type = 'RDFC',
            Metric = 'density_ratio') |>
          dplyr::left_join(
            Species_Info |> 
              dplyr::filter(Classification == 'Fish') |> 
              dplyr::distinct(
                ScientificName, CommonName, Classification, Trophic_Broad, 
                Targeted_Broad, Recreational_Fishery, Commercial_Fishery))
        
      }
      
    }
    
    { # VFT Density Ratios    -----
      
      { # Data    ----
        
        VFT_Density_Boot <- Fish_Density_CSV |>
          dplyr::filter(
            # SiteCode != "KH",
            Survey_Type == "VFT") |>
          dplyr::left_join(
            Species_Info |> 
              dplyr::distinct(ScientificName, Trophic_Broad, Targeted_Broad, 
                              Recreational_Fishery, Commercial_Fishery)) |>
          dplyr::group_by(SiteNumber, CommonName, SurveyYear) |> 
          dplyr::mutate(Mean_Density = Mean_Density + runif(1, min = .9, max = 1.1),
                        CommonName = factor(CommonName),
                        Targeted_Broad = factor(Targeted_Broad),
                        Trophic_Broad = factor(Trophic_Broad)) |> 
          dplyr::ungroup()
      }
      
      { # Species Level  ----
        VFT_Species_Ratio <- tibble(
          CommonName = character(), SurveyYear = integer(),
          Mean_Ratio = double(), CI_plus = double(), CI_minus = double())
        
        for(y in unique(VFT_Density_Boot$SurveyYear)){
          dropped <- VFT_Density_Boot |> 
            dplyr::filter(SurveyYear == y) |> 
            droplevels()
          for(s in levels(dropped$CommonName)){
            d <- dropped |>
              filter(CommonName == s) 
            output <- boot::boot(data = d, statistic = density_boot_ratio, R = 1000)
            ci_boot <- boot::boot.ci(boot.out = output, conf = 0.95, type = "perc")
            VFT_Species_Ratio <- VFT_Species_Ratio |> 
              tibble::add_row(CommonName = s, SurveyYear = y, 
                              Mean_Ratio = ci_boot$t0, CI_minus = ci_boot$percent[4], CI_plus = ci_boot$percent[5])
          }
        } 
      }
      
      { # Targeted Level  ----
        VFT_Target_Species_Ratio <- tibble(
          CommonName = character(), SurveyYear = integer(),
          Mean_Ratio = double(), CI_plus = double(), CI_minus = double())
        
        for(yr in unique(VFT_Density_Boot$SurveyYear)){
          drop <- VFT_Density_Boot |> 
            dplyr::filter(SurveyYear == yr,
                          Targeted_Broad != 'Mixed') |> 
            droplevels()
          for(c in levels(drop$Targeted_Broad)){
            d <- drop |>
              filter(Targeted_Broad == c) 
            output <- boot::boot(data = d, statistic = density_boot_ratio, R = 1000)
            ci_boot <- boot::boot.ci(boot.out = output, conf = 0.95, type = "perc")
            VFT_Target_Species_Ratio <- VFT_Target_Species_Ratio |> 
              tibble::add_row(CommonName = c, SurveyYear = yr, 
                              Mean_Ratio = ci_boot$t0, CI_minus = ci_boot$percent[4], CI_plus = ci_boot$percent[5])
          }
        }
      }
      
      { # Trophic level ----
        VFT_Trophic_Level_Ratios <- tibble(
          CommonName = character(), SurveyYear = integer(),
          Mean_Ratio = double(), CI_plus = double(), CI_minus = double())
        
        for(year in unique(VFT_Density_Boot$SurveyYear)){
          dropT <- VFT_Density_Boot |> 
            dplyr::filter(SurveyYear == year,
                          Trophic_Broad != 'Mixed Trophic Levels') |> 
            droplevels()
          for(t in levels(dropT$Trophic_Broad)){
            d <- dropT |>
              filter(Trophic_Broad == t) 
            output <- boot::boot(data = d, statistic = density_boot_ratio, R = 1000)
            ci_boot <- boot::boot.ci(boot.out = output, conf = 0.95, type = "perc")
            VFT_Trophic_Level_Ratios <- VFT_Trophic_Level_Ratios |> 
              tibble::add_row(CommonName = t, SurveyYear = year, 
                              Mean_Ratio = ci_boot$t0, CI_minus = ci_boot$percent[4], CI_plus = ci_boot$percent[5])
          }
        }
      }
      
      { # Data Outputs  ----
        
        VFT_Density_Ratios <- base::rbind(
          VFT_Species_Ratio,
          VFT_Target_Species_Ratio, 
          VFT_Trophic_Level_Ratios) |>
          dplyr::mutate(
            Date = lubridate::mdy(glue::glue('7-1-{SurveyYear}')),
            Survey_Type = 'VFT',
            Metric = 'density_ratio') |>
          dplyr::left_join(
            Species_Info |> 
              dplyr::filter(Classification == 'Fish') |> 
              dplyr::distinct(
                ScientificName, CommonName, Classification, Trophic_Broad, 
                Targeted_Broad, Recreational_Fishery, Commercial_Fishery))
        
      }
      
    }
    
  }
  
  { # All Ratios   -----
    All_Ratios <- 
      base::rbind(
        Biomass_Ratios,  
        Density_Ratios, 
        RDFC_Density_Ratios, 
        VFT_Density_Ratios) |> 
      arrow::write_feather("Tidy_Data/Ratios.feather")
  }
  
}






