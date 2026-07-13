library(tidyverse)
library(sf)
library(tidycensus)
library(RSocrata)
library(lubridate)
library(showtext)
library(jsonlite)
library(Kendall)
library(car)
library(suncalc)
sf_use_s2(TRUE)
# -----------------------------------------------------------------------------
# 1. PARAMETERS + STYLE
# -----------------------------------------------------------------------------
START_DATE <- "2021-01-01"
END_DATE   <- "2025-12-31"
ACS_YEAR   <- 2024
NYC_CRS    <- 2263
SOURCE     <- "Data: NYPD, NYC 311, NYC DCP, NYC DOT, MTA, Census ACS 5yr 2020-2024"

census_api_key("a9e713a06a0a0f8ec8531e047c9d01e7d9f507d9")

pal <- list(paper = "transparent", ink = "#1F1B16",
            female = "#C0306B", night = "#737ac9", day = "#f2ce72",
            hot_deep = "#8E0B3A", cold_deep = "#2A3170")
tryCatch({
  font_add_google("Fraunces", "fraunces"); font_add_google("Archivo", "archivo")
  showtext_auto(); TITLE_FONT <- "fraunces"; BODY_FONT <- "archivo"
}, error = function(e) { TITLE_FONT <<- "serif"; BODY_FONT <<- "sans" })
ramp_heat <- c(pal$night, pal$day, pal$female, pal$hot_deep)
lisa_cols <- c("High-High"=pal$female, "Low-Low"=pal$night,
               "High-Low"="#E8A0BF", "Low-High"="#A9C7E8", "Not significant"="grey85")
theme_chart <- theme_minimal(base_family = BODY_FONT) +
  theme(plot.background = element_rect(fill = pal$paper, color = NA),
        panel.background = element_rect(fill = pal$paper, color = NA),
        panel.grid.major.x = element_line(linetype = "dotted", linewidth = .15, color = pal$ink),
        panel.grid.major.y = element_blank(), panel.grid.minor = element_blank(),
        plot.title = element_text(family = TITLE_FONT, size = 22, color = pal$ink),
        plot.caption = element_text(color = pal$ink, size = 10, hjust = 0),
        axis.text = element_text(color = pal$ink, size = 10),
        axis.title = element_text(color = pal$ink, size = 12, face = "bold"),
        legend.title = element_text(color = pal$ink, size = 10),
        legend.text = element_text(color = pal$ink, size = 8))
theme_map <- theme_minimal(base_family = BODY_FONT) +
  theme(plot.background = element_rect(fill = pal$paper, color = NA),
        panel.background = element_rect(fill = pal$paper, color = NA),
        panel.grid = element_blank(), axis.text = element_blank(),
        axis.title = element_blank(), axis.ticks = element_blank(),
        plot.title = element_text(family = TITLE_FONT, size = 22, color = pal$ink),
        plot.caption = element_text(color = pal$ink, size = 10, hjust = 0),
        legend.title = element_text(color = pal$ink, size = 10),
        legend.text = element_text(color = pal$ink, size = 9))
cap <- labs(caption = SOURCE)

# -----------------------------------------------------------------------------
# 2. CRIME DATA — NYPD complaints
# -----------------------------------------------------------------------------
NYPD_HISTORIC <- "qgea-i56i"
pub_codes  <- c("104","116","233",          # sexual: rape, felony sex, sex crimes
                "101","106","344","124",    # physical: murder, felony/assault-3, kidnap
                "578",                      # psychological: harassment
                "105","109","110")          # economic: robbery, grand larceny, GLA-MV
viol_codes <- c("104","116","233","101","106","344","124")
sex_codes  <- c("104","116","233")

nypd_select <- "cmplnt_fr_dt,cmplnt_fr_tm,ky_cd,ofns_desc,prem_typ_desc,vic_sex,latitude,longitude"
nypd_where  <- sprintf(
  "cmplnt_fr_dt between '%sT00:00:00' and '%sT23:59:59' AND latitude IS NOT NULL AND ky_cd in (%s)",
  START_DATE, END_DATE, paste0("'", pub_codes, "'", collapse = ","))
incidents_raw <- read.socrata(sprintf(
  "https://data.cityofnewyork.us/resource/%s.json?$select=%s&$where=%s&$limit=2000000",
  NYPD_HISTORIC, nypd_select, nypd_where))

label_lookup <- fromJSON(sprintf(
  "https://data.cityofnewyork.us/resource/%s.json?$select=ky_cd,ofns_desc&$group=ky_cd,ofns_desc&$limit=2000",
  NYPD_HISTORIC)) %>% as_tibble() %>%
  mutate(ky_cd = as.character(ky_cd)) %>%
  filter(!is.na(ofns_desc), ofns_desc != "(null)") %>%
  distinct(ky_cd, .keep_all = TRUE) %>% select(ky_cd, ofns_label = ofns_desc)

crime_sf <- incidents_raw %>%
  mutate(ky_cd = as.character(ky_cd),
         vic_sex = toupper(trimws(vic_sex)),
         prem_typ_desc = toupper(trimws(prem_typ_desc)),
         occ_dt = ymd_hms(paste(substr(cmplnt_fr_dt, 1, 10), cmplnt_fr_tm),
                          tz = "America/New_York", quiet = TRUE),
         occ_hour = hour(occ_dt),
         latitude = as.numeric(latitude), longitude = as.numeric(longitude)) %>%
  left_join(label_lookup, by = "ky_cd") %>%
  mutate(ofns_desc = coalesce(na_if(ofns_desc, "(null)"), ofns_label)) %>%
  filter(!is.na(latitude), !is.na(longitude), latitude != 0, longitude != 0) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>% st_transform(NYC_CRS)

sun <- getSunlightTimes(date = seq(as.Date(START_DATE), as.Date(END_DATE), by = "day"),
                        lat = 40.7128, lon = -74.0060,
                        keep = c("dawn", "dusk"), tz = "America/New_York") %>%
  as_tibble() %>% select(date, dawn, dusk)

crime_sf$.d <- as.Date(crime_sf$occ_dt, tz = "America/New_York")
idx <- match(crime_sf$.d, sun$date)
crime_sf$is_dark <- crime_sf$occ_dt < sun$dawn[idx] | crime_sf$occ_dt > sun$dusk[idx]
crime_sf$.d <- NULL
mean(crime_sf$is_dark, na.rm = TRUE)


# -----------------------------------------------------------------------------
# 3. PUBLIC-SPACE + VICTIM UNIVERSES
# -----------------------------------------------------------------------------
public_prem <- c("STREET", "PARK/PLAYGROUND", "HIGHWAY/PARKWAY", "BRIDGE", "TUNNEL",
                 "BUS STOP", "TRANSIT - NYC SUBWAY", "OPEN AREAS (OPEN LOTS)",
                 "PARKING LOT/GARAGE (PUBLIC)")
crime_sf <- crime_sf %>% filter(prem_typ_desc %in% public_prem)
target_h <- crime_sf %>% filter(vic_sex %in% c("F","M"))   # primary: all person victims
target_f <- target_h %>% filter(vic_sex == "F")            # female sub-analysis
n_total <- nrow(crime_sf); n_human <- nrow(target_h); n_women <- nrow(target_f)
cat(sprintf("\nPublic incidents %s to %s\n  all records: %s | F+M victims: %s | female: %s (%.1f%%)\n",
            START_DATE, END_DATE, format(n_total, big.mark=","),
            format(n_human, big.mark=","), format(n_women, big.mark=","), 100*n_women/n_human))
print(target_h %>% st_drop_geometry() %>% count(ky_cd, ofns_desc, sort = TRUE))

# -----------------------------------------------------------------------------
# 4. INFRASTRUCTURE DATA
# -----------------------------------------------------------------------------
NY_DOMAIN  <- "https://data.ny.gov"
NYC_DOMAIN <- "https://data.cityofnewyork.us"
ID_SUB_STATIONS <- "39hk-dx4f"; ID_PLUTO <- "64uk-42ks"
ID_311 <- "erm2-nwe9";          ID_PAVEMENT <- "6yyb-pb25"

to_points <- function(df) {
  lat <- names(df)[str_detect(names(df), regex("latitude", ignore_case = TRUE))][1]
  lon <- names(df)[str_detect(names(df), regex("longitude", ignore_case = TRUE))][1]
  stopifnot(!is.na(lat), !is.na(lon))
  df %>% mutate(.lat = as.numeric(.data[[lat]]), .lon = as.numeric(.data[[lon]])) %>%
    filter(!is.na(.lat), !is.na(.lon), .lat != 0, .lon != 0) %>%
    st_as_sf(coords = c(".lon", ".lat"), crs = 4326) %>% st_transform(NYC_CRS)
}

stations_sf <- read.socrata(sprintf("%s/resource/%s.json", NY_DOMAIN, ID_SUB_STATIONS)) %>% to_points()

vacant_sf <- read.socrata(sprintf(
  "%s/resource/%s.json?$select=bbl,landuse,latitude,longitude&$where=landuse='11'&$limit=200000",
  NYC_DOMAIN, ID_PLUTO)) %>% to_points()

aband_sf <- read.socrata(sprintf(paste0(
  "%s/resource/%s.json?$select=created_date,latitude,longitude",
  "&$where=created_date >= '%sT00:00:00' AND latitude IS NOT NULL",
  " AND complaint_type='General Construction/Plumbing'",
  " AND descriptor='Building - Vacant, Open And Unguarded'&$limit=500000"),
  NYC_DOMAIN, ID_311, START_DATE)) %>% to_points()
cat(sprintf("vacant/open/unguarded buildings, %s onward: %s\n",
            START_DATE, format(nrow(aband_sf), big.mark=",")))

sidewalk_desc <- c("Broken Sidewalk","Sidewalk Collapsed","Pedestrian Ramp Defective",
                   "Defective Hardware","Sidewalk Grating - Defective",
                   "Metal Protruding - Sign Stump","Sidewalk Grating - Missing")
sidewalk_sf <- read.socrata(sprintf(paste0(
  "%s/resource/%s.json?$select=created_date,latitude,longitude",
  "&$where=created_date >= '%sT00:00:00' AND latitude IS NOT NULL",
  " AND complaint_type='Sidewalk Condition' AND descriptor in(%s)&$limit=500000"),
  NYC_DOMAIN, ID_311, START_DATE,
  paste0("'", sidewalk_desc, "'", collapse = ","))) %>% to_points()
cat(sprintf("sidewalk-defect SRs, %s onward: %s\n", START_DATE,
            format(nrow(sidewalk_sf), big.mark=",")))

STREETLIGHT_TYPE <- "Street Light Condition"
SL_PULL_FROM     <- "2018-01-01"
dark_patterns <- regex(paste("light out|lights out|lamp out|lamp dim|lamp missing",
                             "light cycling|lamp cycling|light dim|defective streetlight", sep="|"),
                       ignore_case = TRUE)
sl_all <- read.socrata(sprintf(paste0(
  "%s/resource/%s.json?$select=unique_key,created_date,closed_date,",
  "descriptor,latitude,longitude&$where=created_date >= '%sT00:00:00' AND latitude IS NOT NULL ",
  "AND complaint_type='%s'&$limit=4000000"),
  NYC_DOMAIN, ID_311, SL_PULL_FROM, STREETLIGHT_TYPE)) %>%
  to_points() %>% filter(!is.na(descriptor), str_detect(descriptor, dark_patterns))
streetlight_sf <- sl_all %>%
  filter(created_date >= as.Date(START_DATE), created_date <= as.Date(END_DATE))
cat(sprintf("311 dark SRs: %s since %s | %s created in window\n",
            format(nrow(sl_all), big.mark=","), SL_PULL_FROM, format(nrow(streetlight_sf), big.mark=",")))

# full rated street network once; poor segments are a subset
pave_all <- st_read(sprintf("%s/resource/%s.geojson?$where=%s&$limit=1000000",
                            NYC_DOMAIN, ID_PAVEMENT,
                            URLencode("systemrating IS NOT NULL", reserved = TRUE)),
                    quiet = TRUE) %>% st_transform(NYC_CRS) %>%
  mutate(systemrating = as.numeric(systemrating))

# -----------------------------------------------------------------------------
# 5. ACS
# -----------------------------------------------------------------------------
NYC_COUNTIES <- c("005","047","061","081","085")
acs_vars <- c(total_pop="B01003_001", female_pop="B01001_026",
              pov_univ="B17001_001", pov_below="B17001_002",
              occ_total="B25003_001", occ_renter="B25003_003",
              hu_total="B25002_001", hu_vacant="B25002_003")
safe_div <- function(a, b) ifelse(!is.na(b) & b > 0, a/b, NA_real_)
tracts_sf <- get_acs(geography="tract", variables=acs_vars, state="NY", county=NYC_COUNTIES,
                     year=ACS_YEAR, survey="acs5", geometry=TRUE, output="wide", cache_table=TRUE) %>%
  st_transform(NYC_CRS) %>%
  mutate(area_sqkm = as.numeric(st_area(geometry)) * 0.09290304 / 1e6,
         poverty_rate = safe_div(pov_belowE, pov_univE),
         renter_share = safe_div(occ_renterE, occ_totalE),
         vacancy_rate = safe_div(hu_vacantE, hu_totalE)) %>%
  filter(total_popE > 0, female_popE >= 0) %>%
  select(GEOID, NAME, area_sqkm, total_pop=total_popE, female_pop=female_popE,
         poverty_rate, renter_share, vacancy_rate, geometry)
cat(sprintf("ACS tracts kept: %s\n", format(nrow(tracts_sf), big.mark=",")))

# -----------------------------------------------------------------------------
# 6. JOIN -> model_df
# -----------------------------------------------------------------------------
count_in_tract <- function(pts, nm) pts %>%
  st_join(dplyr::select(tracts_sf, GEOID), join = st_intersects) %>%
  st_drop_geometry() %>% filter(!is.na(GEOID)) %>% dplyr::count(GEOID, name = nm)

inc_counts     <- count_in_tract(target_h, "inc_n")
fem_counts     <- count_in_tract(target_f, "fem_n")
sexf_counts    <- count_in_tract(target_f %>% filter(ky_cd %in% sex_codes), "sexf_n")
vacant_counts  <- count_in_tract(vacant_sf, "vacant_n")
aband_counts   <- count_in_tract(aband_sf, "aband_n")
sidewalk_counts <- count_in_tract(sidewalk_sf, "sidewalk_n")
sl_counts      <- count_in_tract(streetlight_sf, "streetlight_n")
cent <- st_centroid(tracts_sf)
ni   <- st_nearest_feature(cent, stations_sf)
tracts_sf <- tracts_sf %>%
  mutate(dist_subway_km = as.numeric(st_distance(cent, stations_sf[ni,], by_element=TRUE)) * 0.0003048)

pave_tr <- pave_all %>%
  st_intersection(dplyr::select(tracts_sf, GEOID)) %>%
  mutate(seg_ft = as.numeric(st_length(geometry))) %>% st_drop_geometry()
street_len <- pave_tr %>% group_by(GEOID) %>%
  dplyr::summarise(street_ft = sum(seg_ft, na.rm=TRUE), .groups="drop")
poor_pave <- pave_tr %>% filter(systemrating <= 3) %>% group_by(GEOID) %>%
  dplyr::summarise(poor_pave_ft = sum(seg_ft, na.rm=TRUE), .groups="drop")

model_df <- tracts_sf %>%
  left_join(inc_counts, by="GEOID") %>% left_join(fem_counts, by="GEOID") %>%
  left_join(sexf_counts, by="GEOID") %>% left_join(vacant_counts, by="GEOID") %>%
  left_join(aband_counts, by="GEOID") %>% left_join(sidewalk_counts, by="GEOID") %>%
  left_join(sl_counts, by="GEOID") %>%
  left_join(street_len, by="GEOID") %>% left_join(poor_pave, by="GEOID") %>%
  mutate(across(c(inc_n, fem_n, sexf_n, vacant_n, aband_n, sidewalk_n,
                  streetlight_n, street_ft, poor_pave_ft),
                ~ tidyr::replace_na(., 0)),
         inc_pct = 100 * inc_n / pmax(total_pop, 1))
cat(sprintf("model_df: %s tracts | incidents joined: %s (%.1f%%) | zero tracts: %d\n",
            format(nrow(model_df), big.mark=","), format(sum(model_df$inc_n), big.mark=","),
            100*sum(model_df$inc_n)/nrow(target_h), sum(model_df$inc_n == 0)))

# -----------------------------------------------------------------------------
# 7. EDA
# -----------------------------------------------------------------------------
comp <- target_h %>% st_drop_geometry() %>%
  dplyr::count(ofns_desc, sort = TRUE) %>% mutate(pct = 100*n/sum(n))
print(comp)

ggplot(comp, aes(reorder(ofns_desc, n), n)) +
  geom_col(fill = pal$female) +
  geom_text(aes(label = sprintf("%.1f%%", pct)), hjust = -0.15, size = 4, color = pal$ink) +
  coord_flip() +
  scale_y_continuous(labels = scales::comma, expand = expansion(mult = c(0, .12))) +
  labs(title = "Public-space incidents by offence type, 2021-2025", x = NULL, y = NULL) + cap + theme_chart
ggsave("outputs/p1_offense_composition.png", width = 6, height = 2, dpi = 250)

heat <- target_h %>% st_drop_geometry() %>%
  mutate(dow = wday(occ_dt, label = TRUE, week_start = 1), hr = occ_hour) %>%
  filter(!is.na(dow), !is.na(hr)) %>% dplyr::count(dow, hr)
ggplot(heat, aes(hr, dow, fill = n)) +
  geom_tile(color = "white", linewidth = .4) +
  scale_fill_gradientn(colors = ramp_heat, labels = scales::comma, name = NULL) +
  scale_x_continuous(breaks = 0:23, expand = c(0,0)) + scale_y_discrete(limits = rev, expand = c(0,0)) +
  labs(title = "Public-space incidents by day of week and hour, 2021-2025", x = "Hour", y = NULL) +
  cap + theme_chart + theme(legend.text = element_text(size = 12))
ggsave("outputs/p2_heatmap_day_hour.png", width = 6, height = 2, dpi = 250)

dow_days <- tibble(d = seq(as.Date(START_DATE), as.Date(END_DATE), by = "day")) %>%
  mutate(dow = wday(d, label = TRUE, week_start = 1)) %>% dplyr::count(dow, name = "n_days")
heat_avg <- heat %>% left_join(dow_days, by = "dow") %>% mutate(avg = n / n_days)
ggplot(heat_avg, aes(hr, dow, fill = avg)) +
  geom_tile(color = "white", linewidth = .4) +
  scale_fill_gradientn(colors = ramp_heat, name = NULL) +
  scale_x_continuous(breaks = 0:23, expand = c(0,0)) + scale_y_discrete(limits = rev, expand = c(0,0)) +
  labs(title = "Average public-space incidents by day of week and hour, 2021-2025",
       x = "Hour", y = NULL) +
  cap + theme_chart + theme(legend.text = element_text(size = 12))
ggsave("outputs/p2_heatmap_day_hour_avg.png", width = 6, height = 2, dpi = 250)

monthly <- target_h %>% st_drop_geometry() %>%
  mutate(ym = floor_date(occ_dt, "month")) %>% dplyr::count(ym) %>% arrange(ym) %>% filter(!is.na(ym))
mk   <- Kendall::MannKendall(monthly$n)
mk_s <- Kendall::SeasonalMannKendall(ts(monthly$n, frequency = 12))
cat(sprintf("Mann-Kendall: raw tau = %.3f (p = %.4g) | seasonal tau = %.3f (p = %.4g)\n",
            mk$tau, mk$sl, mk_s$tau, mk_s$sl))

ggplot(monthly, aes(ym, n)) +
  geom_point(color = pal$female, alpha = .7) +
  geom_smooth(method = "lm", se = FALSE, color = pal$ink, linewidth = .5) +
  labs(title = "Monthly public-space incident counts, 2021-2025", x = NULL, y = NULL) + cap + theme_chart
ggsave("outputs/p2_monthly_trend.png", width = 6, height = 3, dpi = 250)

all_days <- tibble(d = seq(as.Date(START_DATE), as.Date(END_DATE), by = "day"))
daily <- target_h %>% st_drop_geometry() %>% filter(!is.na(occ_dt)) %>%
  mutate(d = as_date(occ_dt)) %>% filter(d >= as.Date(START_DATE), d <= as.Date(END_DATE)) %>%
  dplyr::count(d, name = "n")
daily <- all_days %>% left_join(daily, by = "d") %>%
  mutate(n = tidyr::replace_na(n, 0L), yr = year(d),
         dow = wday(d, label = TRUE, week_start = 1), week = as.integer(format(d, "%W")))
ggplot(daily, aes(week, dow, fill = n)) +
  geom_tile(color = "white", linewidth = .3) +
  scale_fill_gradientn(colors = ramp_heat, labels = scales::comma, name = NULL) +
  scale_x_continuous(breaks = seq(0, 52, 4), expand = c(0,0)) +
  scale_y_discrete(limits = rev, expand = c(0,0)) + facet_grid(yr ~ ., switch = "y") +
  labs(title = "Public-space incidents daily trend, 2021-2025", x = "Week of year", y = NULL) +
  cap + theme_chart +
  theme(panel.grid = element_blank(), panel.spacing = unit(3, "pt"),
        strip.text.y.left = element_text(angle = 0, size = 9, color = pal$ink),
        legend.key.width = unit(0.3, "cm"),
        legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 12), axis.text = element_text(face = "bold"))
ggsave("outputs/p2_calendar_heatmap.png", width = 6, height = 4, dpi = 250)


q2 <- target_h %>% st_drop_geometry() %>%
  group_by(ofns_desc) %>%
  summarise(n = n(), dark_pct = 100*mean(is_dark, na.rm=TRUE), .groups="drop") %>%
  arrange(dark_pct) %>% mutate(ofns_desc = factor(ofns_desc, levels = ofns_desc))
q2_long <- q2 %>%
  transmute(ofns_desc, Dark = dark_pct, Daylight = 100 - dark_pct) %>%
  tidyr::pivot_longer(c(Dark, Daylight), names_to = "period", values_to = "pct") %>%
  mutate(period = factor(period, levels = c("Daylight", "Dark")))
ggplot(q2_long, aes(pct, ofns_desc, fill = period)) +
  geom_col(width = .68) +
  geom_text(data = q2, aes(x = dark_pct, y = ofns_desc, label = sprintf("%.0f%%", dark_pct)),
            inherit.aes = FALSE, hjust = 1.5, fontface = "bold", size = 4,  color = pal$ink) +
  geom_text(data = q2, aes(x = 102, y = ofns_desc, label = paste0("n = ", scales::comma(n))),
            inherit.aes = FALSE, hjust = 0, size = 3.1, color = pal$ink) +
  scale_fill_manual(values = c(Dark = pal$night, Daylight = pal$day),
                    breaks = c("Dark", "Daylight"), name = NULL) +
  scale_x_continuous(limits = c(0, 126), breaks = seq(0, 100, 25),
                     labels = function(x) paste0(x, "%"), expand = c(0, 0)) +
  labs(title = "% incidents occurring in darkness (after sunset) by offence type",
       x = "% of incidents", y = NULL) +
  cap + theme_chart +
  theme(panel.grid.major.x = element_blank(), panel.grid.major.y = element_blank(),
        legend.position = "top")
ggsave("outputs/q2_dark_by_offence.png", width = 6, height = 3.4, dpi = 250)

q3 <- target_h %>% st_drop_geometry() %>%
  group_by(ofns_desc) %>%
  summarise(n = n(), fem_pct = 100*mean(vic_sex == "F"), .groups="drop") %>%
  arrange(fem_pct) %>% mutate(ofns_desc = factor(ofns_desc, levels = ofns_desc))
q3_long <- q3 %>%
  transmute(ofns_desc, Female = fem_pct, Male = 100 - fem_pct) %>%
  tidyr::pivot_longer(c(Female, Male), names_to = "victim", values_to = "pct") %>%
  mutate(victim = factor(victim, levels = c("Male", "Female")))
ggplot(q3_long, aes(pct, ofns_desc, fill = victim)) +
  geom_col(width = .68) +
  geom_vline(xintercept = 50, linetype = "dotted", color = pal$ink, linewidth = .4) +
  geom_text(data = q3, aes(x = fem_pct, y = ofns_desc, label = sprintf("%.0f%%", fem_pct)),
            inherit.aes = FALSE, hjust = 1.5, fontface = "bold", size = 4, color = pal$ink) +
  geom_text(data = q3, aes(x = 102, y = ofns_desc, label = paste0("n = ", scales::comma(n))),
            inherit.aes = FALSE, hjust = 0,  size = 3.1, color = pal$ink) +
  scale_fill_manual(values = c(Female = pal$female, Male = pal$night),
                    breaks = c("Female", "Male"), name = "victim") +
  scale_x_continuous(limits = c(0, 126), breaks = seq(0, 100, 25),
                     labels = function(x) paste0(x, "%"), expand = c(0, 0)) +
  labs(title = "% female victims by offence type",
       x = "% of incidents", y = NULL) +
  cap + theme_chart +
  theme(panel.grid.major.x = element_blank(), panel.grid.major.y = element_blank(),
        legend.position = "top", legend.title = element_text(face = "bold"))
ggsave("outputs/q3_female_share_by_offence.png", width = 6, height = 3.4, dpi = 250)

# -----------------------------------------------------------------------------
# 8. OVERDISPERSION DIAGNOSTIC
# -----------------------------------------------------------------------------
cat(sprintf("inc_n: mean = %.1f, var = %.1f, var/mean = %.1f\n",
            mean(model_df$inc_n), var(model_df$inc_n), var(model_df$inc_n)/mean(model_df$inc_n)))
ggplot(model_df, aes(inc_n)) +
  geom_histogram(bins = 50, fill = pal$female, color = "white", linewidth = .3) +
  labs(title = "Distribution of tract-level public-space incident counts", x = "Incidents Per Tract", y = NULL) +
  cap + theme_chart
ggsave("outputs/p3_count_distribution.png", width = 6, height = 3, dpi = 250)

# -----------------------------------------------------------------------------
# 9. SPATIAL WEIGHTS + GLOBAL MORAN
# -----------------------------------------------------------------------------
patch_islands <- function(sfobj, nb) {
  empt <- which(spdep::card(nb) == 0)
  if (length(empt)) {
    pts <- st_coordinates(st_point_on_surface(st_geometry(sfobj)))
    k1  <- spdep::knn2nb(spdep::knearneigh(pts, k = 1))
    for (i in empt) nb[[i]] <- k1[[i]]
    message(length(empt), " island tract(s) patched")
  }
  nb
}
nb   <- patch_islands(model_df, spdep::poly2nb(model_df, queen = TRUE))
wt   <- spdep::nb2listw(nb, style = "W", zero.policy = TRUE)
wt_s <- spdep::nb2listw(spdep::include.self(nb), style = "B", zero.policy = TRUE)

gm <- spdep::moran.test(model_df$inc_n, wt, zero.policy = TRUE)
cat(sprintf("Global Moran's I: I = %.3f, p = %.4g\n",
            gm$estimate[["Moran I statistic"]], gm$p.value))

ggplot(model_df) +
  geom_sf(aes(fill = inc_n), color = NA) +
  scale_fill_gradientn(colors = ramp_heat, name = "Incidents", trans = "sqrt", labels = scales::comma) +
  labs(title = "Public-space incidents spatial distribution") + cap + theme_map +
  theme(legend.key.height = unit(0.2, "cm"), legend.position = "top",
        legend.key.width = unit(1.2, "cm"),
        legend.title = element_text(size = 12, face = "bold"), legend.text = element_text(size = 12))
ggsave("outputs/p4_crimedistribution.png", width = 6, height = 4, dpi = 250)

# -----------------------------------------------------------------------------
# 10. LOCAL SPATIAL STATS — permutation LISA + Gi* (9999 sims)
# -----------------------------------------------------------------------------
x <- model_df$inc_n
set.seed(1)
lmp <- spdep::localmoran_perm(x, wt, nsim = 9999, zero.policy = TRUE)
lpcol <- { s <- grep("Sim", colnames(lmp)); if (length(s)) tail(s,1) else grep("^Pr", colnames(lmp))[1] }
model_df$lisa_psim <- lmp[, lpcol]
sig_levels <- c("Not significant","p <= 0.05","p <= 0.01","p <= 0.001","p <= 0.0001")
sig_cols <- c("Not significant"="grey85","p <= 0.05"="#EBC2D3","p <= 0.01"="#D87DA3",
              "p <= 0.001"="#C0306B","p <= 0.0001"="#7E1E47")
model_df$lisa_sig <- factor(cut(model_df$lisa_psim, c(-Inf,.0001,.001,.01,.05,Inf),
                                labels = rev(sig_levels)), levels = sig_levels)
ggplot(model_df) +
  geom_sf(aes(fill = lisa_sig), color = NA) +
  scale_fill_manual(values = sig_cols, breaks = sig_levels, drop = FALSE, name = NULL) +
  labs(title = "Public-space incidents LISA significance map") + cap + theme_map +
  theme(legend.key.height = unit(0.4, "cm"), legend.key.width = unit(0.4, "cm"),
        legend.title = element_text(size = 12, face = "bold"), legend.text = element_text(size = 12))
ggsave("outputs/p5_lisa_significance.png", width = 6, height = 4, dpi = 250)

xs <- as.numeric(scale(x)); wx <- spdep::lag.listw(wt, xs, zero.policy = TRUE)
model_df$lisa <- dplyr::case_when(
  model_df$lisa_psim >= 0.05 ~ "Not significant",
  xs > 0 & wx > 0 ~ "High-High", xs < 0 & wx < 0 ~ "Low-Low",
  xs > 0 & wx < 0 ~ "High-Low", TRUE ~ "Low-High")
ggplot(model_df) +
  geom_sf(aes(fill = lisa), color = NA) +
  scale_fill_manual(values = lisa_cols, name = NULL) +
  labs(title = "Public-space incidents local spatial clustering (LISA)") + cap + theme_map +
  theme(legend.key.height = unit(0.4, "cm"), legend.key.width = unit(0.4, "cm"),
        legend.title = element_text(size = 12, face = "bold"), legend.text = element_text(size = 12))
ggsave("outputs/p5_lisa.png", width = 6, height = 4, dpi = 250)

set.seed(1)
gi_perm <- spdep::localG_perm(x, wt_s, nsim = 9999, zero.policy = TRUE)
model_df$gi_z <- as.numeric(gi_perm)
gi_int  <- attr(gi_perm, "internals")
gi_pcol <- { s <- grep("Sim", colnames(gi_int)); if (length(s)) tail(s,1) else grep("^Pr", colnames(gi_int))[1] }
model_df$gi_psim <- gi_int[, gi_pcol]
gi_levels <- c("Hot 99%","Hot 95%","Not significant","Cold 95%","Cold 99%")
gi_cols <- c("Hot 99%"="#C0306B","Hot 95%"="#E8A0BF","Not significant"="grey85",
             "Cold 95%"="#737ac9","Cold 99%"="#2A3170")
model_df$gi <- factor(dplyr::case_when(
  model_df$gi_psim <= 0.01 & model_df$gi_z > 0 ~ "Hot 99%",
  model_df$gi_psim <= 0.05 & model_df$gi_z > 0 ~ "Hot 95%",
  model_df$gi_psim <= 0.01 & model_df$gi_z < 0 ~ "Cold 99%",
  model_df$gi_psim <= 0.05 & model_df$gi_z < 0 ~ "Cold 95%",
  TRUE ~ "Not significant"), levels = gi_levels)
print(table(model_df$gi))
ggplot(model_df) +
  geom_sf(aes(fill = gi), color = NA) +
  scale_fill_manual(values = gi_cols, breaks = gi_levels, drop = FALSE, name = NULL) +
  labs(title = "Public-space incident hot and cold spots (Getis-Ord Gi*)") + cap + theme_map +
  theme(legend.key.height = unit(0.4, "cm"), legend.key.width = unit(0.4, "cm"),
        legend.title = element_text(size = 12, face = "bold"), legend.text = element_text(size = 12))
ggsave("outputs/p5_gistar.png", width = 6, height = 4, dpi = 250)

# -----------------------------------------------------------------------------
# 11. PREDICTOR EDA
# -----------------------------------------------------------------------------
pred_infra <- c("dist_subway_km","streetlight_n","poor_pave_ft","sidewalk_n","vacant_n","aband_n")
pred_ctrl  <- c("poverty_rate","renter_share","vacancy_rate","log_area","street_ft")
preds      <- c(pred_infra, pred_ctrl)
to_log     <- c(pred_infra, "street_ft")
pretty_lab <- c(dist_subway_km="Metro proximity (km)", streetlight_n="Light-out (count)",
                poor_pave_ft="Poor pavement (ft)", sidewalk_n="Sidewalk defects (count)",
                vacant_n="Vacant land (count)", aband_n="Vacant/unsecured bldg (count)",
                poverty_rate="Poverty rate", renter_share="Renter percentage",
                vacancy_rate="Housing vacancy rate", log_area="log tract area",
                street_ft="Street length (ft)")
pretty_lab_mf <- pretty_lab
pretty_lab_mf[to_log] <- c("log Metro proximity","log Light-out","log Poor pavement",
                           "log Sidewalk defects","log Vacant land","log Vacant/unsecured bldg",
                           "log Street length")

pred_data <- model_df %>% st_drop_geometry() %>%
  mutate(log_area = log(area_sqkm)) %>%
  dplyr::select(inc_n, total_pop, all_of(preds)) %>% tidyr::drop_na()
pred_mf <- pred_data; pred_mf[to_log] <- lapply(pred_mf[to_log], log1p)

skewness <- function(v) { v <- v[is.finite(v)]; mean((v - mean(v))^3)/sd(v)^3 }
print(pred_data %>% summarise(across(all_of(preds), skewness)) %>%
        tidyr::pivot_longer(everything(), names_to="var", values_to="skew") %>%
        arrange(desc(abs(skew))))

pred_data %>% dplyr::select(all_of(preds)) %>%
  tidyr::pivot_longer(everything(), names_to="var", values_to="val") %>%
  ggplot(aes(val)) +
  geom_histogram(bins = 40, fill = pal$female, color = "white", linewidth = .2) +
  facet_wrap(~var, scales = "free", ncol = 3, labeller = as_labeller(pretty_lab)) +
  labs(title = "Predictor distributions (raw)", x = NULL, y = NULL) + cap + theme_chart +
  theme(panel.grid.major.x = element_blank(),
        strip.text = element_text(face = "bold", size = 9), axis.text = element_text(size = 7))
ggsave("outputs/p7a_predictor_distributions.png", width = 6, height = 5.5, dpi = 250)

cor_vars <- c("inc_n", preds)
M <- cor(pred_mf[cor_vars], use = "pairwise.complete.obs")
ramp201 <- colorRampPalette(c("#2A3170","#FFFFFF","#8E0B3A"))(201)
r_to_hex <- function(r) { r <- pmax(pmin(r,1),-1); ramp201[round((r+1)/2*200)+1] }
lab_all <- c(inc_n = "Incident count", pretty_lab_mf)
cor_long <- as.data.frame(M) %>% tibble::rownames_to_column("v1") %>%
  tidyr::pivot_longer(-v1, names_to="v2", values_to="r") %>%
  mutate(is_diag = v1 == v2, fill = ifelse(is_diag, pal$paper, r_to_hex(r)),
         v1 = factor(v1, levels = cor_vars), v2 = factor(v2, levels = rev(cor_vars)))
ggplot(cor_long, aes(v1, v2)) +
  geom_tile(aes(fill = fill), color = "white", linewidth = 0.2) +
  geom_text(data = filter(cor_long, !is_diag), aes(label = sprintf("%.2f", r)), size = 3.5, color = pal$ink) +
  scale_fill_identity() + geom_point(aes(color = r), alpha = 0, na.rm = TRUE) +
  scale_color_gradientn(colours = ramp201, limits = c(-1,1), breaks = c(-1,-.5,0,.5,1), name = "Pearson r",
                        guide = guide_colorbar(barheight = unit(5.5,"cm"), barwidth = unit(0.4,"cm"),
                                               ticks.colour = pal$ink, frame.colour = pal$ink)) +
  guides(color = guide_colorbar(override.aes = list(alpha = 1))) +
  scale_x_discrete(labels = lab_all, position = "top") + scale_y_discrete(labels = lab_all) +
  coord_fixed() + labs(title = "Predictor correlation matrix (model-form)", x = NULL, y = NULL, caption = SOURCE) +
  theme_map + theme(axis.text.x = element_text(angle = 90, hjust = 0, size = 10, face = "bold"),
                    axis.text.y = element_text(size = 10, face = "bold"),
                    legend.position = "top", legend.key.height = unit(0.2, "cm"))
ggsave("outputs/p7b_correlation_matrix.png", width = 7, height = 5, dpi = 300)

vif_tbl <- car::vif(lm(reformulate(preds, "inc_n"), data = pred_mf)) %>%
  tibble::enframe("var", "vif") %>% arrange(desc(vif))
print(vif_tbl)
ggplot(vif_tbl, aes(reorder(var, vif), vif)) +
  geom_col(fill = pal$female, width = 0.4) +
  geom_hline(yintercept = c(1,2,3), linetype = "dotted", color = pal$ink) +
  geom_text(aes(label = sprintf("%.1f", vif)), hjust = -0.2, size = 5, color = pal$ink) +
  scale_x_discrete(labels = pretty_lab_mf) + scale_y_continuous(expand = expansion(mult = c(0,.15))) +
  coord_flip() + labs(title = "Variance inflation factors (model-form)", x = NULL, y = NULL) +
  cap + theme_chart + theme(panel.grid.major.x = element_blank(), axis.text = element_text(size = 12))
ggsave("outputs/p7c_vif.png", width = 6, height = 3.2, dpi = 250)

pred_mf %>% dplyr::select(inc_n, all_of(preds)) %>%
  tidyr::pivot_longer(all_of(preds), names_to="var", values_to="val") %>%
  ggplot(aes(val, inc_n)) +
  geom_point(alpha = .2, color = pal$female, size = .1) +
  geom_smooth(method = "loess", se = FALSE, color = pal$ink, linewidth = .5) +
  facet_wrap(~var, scales = "free_x", ncol = 3, labeller = as_labeller(pretty_lab_mf)) +
  scale_y_sqrt() + labs(title = "Incident count vs each predictor (model-form)", x = NULL, y = "Incident count (sqrt)") +
  cap + theme_chart + theme(strip.text = element_text(face = "bold", size = 10), axis.text = element_text(size = 9))
ggsave("outputs/p7d_dv_predictor_scatters.png", width = 6, height = 5.5, dpi = 250)

# -----------------------------------------------------------------------------
# 12. FEATURE ENGINEERING + WEIGHTS ON MODELLING ROWS
# -----------------------------------------------------------------------------
feat_sf <- model_df %>%
  mutate(log_area = log(area_sqkm), log_dist = log1p(dist_subway_km),
         log_light = log1p(streetlight_n), log_pave = log1p(poor_pave_ft),
         log_sidewalk = log1p(sidewalk_n),
         log_vacant = log1p(vacant_n), log_aband = log1p(aband_n),
         log_street = log1p(street_ft)) %>%
  filter(total_pop > 0, female_pop > 0, !is.na(poverty_rate), !is.na(renter_share),
         !is.na(vacancy_rate), is.finite(log_area)) %>%
  mutate(across(c(log_dist, log_light, log_pave, log_sidewalk, log_vacant, log_aband,
                  log_street,
                  poverty_rate, renter_share, vacancy_rate, log_area),
                ~ as.numeric(scale(.x)), .names = "z_{.col}"))
cat(sprintf("Modelling sample: %d tracts (%d dropped)\n", nrow(feat_sf), nrow(model_df) - nrow(feat_sf)))

nb_m <- patch_islands(feat_sf, spdep::poly2nb(feat_sf, queen = TRUE))
wt_m <- spdep::nb2listw(nb_m, style = "W", zero.policy = TRUE)
dat  <- st_drop_geometry(feat_sf)

# -----------------------------------------------------------------------------
# 13. ASPATIAL NB + residual-autocorrelation gate
# -----------------------------------------------------------------------------
f_nb <- inc_n ~ z_log_dist + z_log_light + z_log_pave + z_log_sidewalk +
  z_log_vacant + z_log_aband + z_log_street + z_log_area +
  z_renter_share + z_vacancy_rate + poly(z_poverty_rate, 2) + offset(log(total_pop))
m_nb <- MASS::glm.nb(f_nb, data = dat, control = glm.control(maxit = 100))
print(summary(m_nb))

m_pois <- glm(f_nb, family = poisson, data = dat)
lr <- as.numeric(2 * (logLik(m_nb) - logLik(m_pois)))
cat(sprintf("NB vs Poisson LR: chi2 = %.1f, p = %.4g\n", lr, pchisq(lr, 1, lower.tail = FALSE)))

rm_cs <- spdep::moran.test(residuals(m_nb, type = "pearson"), wt_m, zero.policy = TRUE)
cat(sprintf("Aspatial NB residual Moran's I: I = %.4f, p = %.4g\n",
            rm_cs$estimate[["Moran I statistic"]], rm_cs$p.value))

# -----------------------------------------------------------------------------
# 13b. SPATIAL NB (mgcv MRF) — total, female, female-sexual outcomes
# -----------------------------------------------------------------------------
if (!requireNamespace("mgcv", quietly = TRUE)) stop("install.packages('mgcv')")
library(mgcv)
gid <- as.character(feat_sf$GEOID)
nb_sym <- spdep::make.sym.nb(nb_m)
nb_mrf <- lapply(nb_sym, function(v) gid[v])
names(nb_mrf) <- gid
dat$GEOID_f <- factor(dat$GEOID, levels = gid)

rhs_mrf <- paste("~ z_log_dist + z_log_light + z_log_pave + z_log_sidewalk +",
                 "z_log_vacant + z_log_aband + z_log_street + z_log_area +",
                 "z_renter_share + z_vacancy_rate + poly(z_poverty_rate, 2) +",
                 "s(GEOID_f, bs = 'mrf', xt = list(nb = nb_mrf), k = 250)")

m_mrf <- gam(as.formula(paste("inc_n", rhs_mrf, "+ offset(log(total_pop))")),
             family = nb(), data = dat, method = "REML")
print(summary(m_mrf))
m_mrf_f <- gam(as.formula(paste("fem_n", rhs_mrf, "+ offset(log(female_pop))")),
               family = nb(), data = dat, method = "REML")
m_mrf_s <- gam(as.formula(paste("sexf_n", rhs_mrf, "+ offset(log(female_pop))")),
               family = nb(), data = dat, method = "REML")

res_mrf <- residuals(m_mrf, type = "pearson")
rm_mrf <- spdep::moran.test(res_mrf, wt_m, zero.policy = TRUE)
cat(sprintf("\nSpatial (MRF) conditional residual Moran's I = %.4f, p = %.4g\n",
            rm_mrf$estimate[["Moran I statistic"]], rm_mrf$p.value))
cat(sprintf("AIC  aspatial NB = %.0f | spatial MRF = %.0f\n", AIC(m_nb), AIC(m_mrf)))

lab_map <- c(z_log_light="Streetlight outages", z_log_pave="Poor pavement",
             z_log_sidewalk="Sidewalk defects",
             z_log_dist="Subway distance", z_log_vacant="Vacant land",
             z_log_aband="Vacant/unsecured buildings",
             z_log_street="Street length",
             z_renter_share="Renter share", z_vacancy_rate="Housing vacancy (ACS)")
infra   <- c("z_log_light","z_log_pave","z_log_sidewalk","z_log_dist","z_log_vacant","z_log_aband")
context <- c("z_log_street")

pt <- as.data.frame(summary(m_mrf)$p.table); names(pt) <- c("beta","se","z","p")
importance <- pt %>% tibble::rownames_to_column("term") %>%
  filter(term %in% names(lab_map)) %>%
  transmute(predictor = lab_map[term],
            domain = dplyr::case_when(term %in% infra ~ "infrastructure",
                                      term %in% context ~ "context",
                                      TRUE ~ "socioeconomic"),
            beta_sd = round(beta,3), pct_per_sd = round((exp(beta)-1)*100,1), p = signif(p,3),
            sig = cut(p, c(-Inf,.001,.01,.05,.1,Inf), c("***","**","*",".","")),
            lo = beta-1.96*se, hi = beta+1.96*se) %>%
  arrange(desc(abs(beta_sd)))
pov_p <- pt[grep("poverty", rownames(pt)), "p"]

grab <- function(m, nm) {
  p <- as.data.frame(summary(m)$p.table)
  tibble::tibble(term = rownames(p),
                 "{nm}_b" := round(p[,1], 3), "{nm}_p" := signif(p[,4], 2))
}
cmp <- purrr::reduce(list(grab(m_mrf,"all"), grab(m_mrf_f,"fem"), grab(m_mrf_s,"sexf")),
                     dplyr::left_join, by = "term") %>%
  filter(term %in% names(lab_map)) %>%
  mutate(term = lab_map[term])
cat("\n===== OUTCOME COMPARISON: all victims | female | female sexual offenses =====\n")
print(cmp, n = 20)

# -----------------------------------------------------------------------------
# 14. STREETLIGHT PANEL — dark lamp-days per tract-month
# -----------------------------------------------------------------------------
for (p in c("fixest","sandwich","lmtest"))
  if (!requireNamespace(p, quietly = TRUE)) stop("install ", p)
library(fixest); library(sandwich); library(lmtest)

PANEL_START <- as.Date("2021-01-01")
PANEL_END   <- as.Date("2025-11-01")
N_LAGS      <- 3
STOCK_START <- floor_date(PANEL_START %m-% months(N_LAGS), "month")
geoid_order <- feat_sf$GEOID
months_seq  <- seq(STOCK_START, floor_date(PANEL_END, "month"), by = "month")

outage_d <- sl_all %>%
  st_join(dplyr::select(tracts_sf, GEOID), join = st_intersects) %>% st_drop_geometry() %>%
  filter(!is.na(GEOID), GEOID %in% geoid_order) %>%
  transmute(GEOID,
            open  = lubridate::as_date(created_date),
            close = dplyr::coalesce(lubridate::as_date(closed_date), PANEL_END)) %>%
  filter(!is.na(open), close >= open, close >= STOCK_START, open <= PANEL_END) %>%
  mutate(open = pmax(open, STOCK_START), close = pmin(close, PANEL_END))

dark_days <- outage_d %>%
  mutate(om = floor_date(open, "month"), cm = floor_date(close, "month"),
         month = purrr::map2(om, cm, ~ seq(.x, .y, by = "month"))) %>%
  tidyr::unnest(month) %>%
  mutate(dd = as.numeric(pmin(close, ceiling_date(month, "month") - days(1)) -
                           pmax(open, month)) + 1) %>%
  group_by(GEOID, month) %>% summarise(dark_days = sum(dd), .groups = "drop")

own <- tidyr::expand_grid(GEOID = geoid_order, month = months_seq) %>%
  left_join(dark_days, by = c("GEOID","month")) %>%
  mutate(light_stock = tidyr::replace_na(dark_days, 0)) %>% dplyr::select(-dark_days)

own_wide <- own %>% mutate(month = as.character(month)) %>%
  tidyr::pivot_wider(names_from = month, values_from = light_stock, values_fill = 0) %>%
  arrange(match(GEOID, geoid_order))
stopifnot(identical(own_wide$GEOID, geoid_order))
WS <- spdep::listw2mat(wt_m) %*% as.matrix(own_wide[,-1])
W_long <- as.data.frame(WS, check.names = FALSE) %>% mutate(GEOID = own_wide$GEOID) %>%
  tidyr::pivot_longer(-GEOID, names_to = "month", values_to = "W_light_stock") %>%
  mutate(month = as.Date(month))

inc <- target_h %>%
  st_join(dplyr::select(tracts_sf, GEOID), join = st_intersects) %>% st_drop_geometry() %>%
  filter(!is.na(GEOID), GEOID %in% geoid_order, !is.na(occ_dt)) %>%
  mutate(month = floor_date(as_date(occ_dt), "month")) %>%
  filter(month >= PANEL_START, month <= PANEL_END) %>%
  dplyr::count(GEOID, month, name = "inc_n")

zcols <- c("l0","l1","l2","l3","Wl0","Wl1","Wl2","Wl3")
panel <- own %>%
  left_join(W_long, by = c("GEOID","month")) %>% left_join(inc, by = c("GEOID","month")) %>%
  mutate(inc_n = tidyr::replace_na(inc_n, 0L), ls = log1p(light_stock), Wls = log1p(W_light_stock)) %>%
  arrange(GEOID, month) %>% group_by(GEOID) %>%
  mutate(l0 = ls, l1 = lag(ls,1), l2 = lag(ls,2), l3 = lag(ls,3),
         Wl0 = Wls, Wl1 = lag(Wls,1), Wl2 = lag(Wls,2), Wl3 = lag(Wls,3)) %>%
  ungroup() %>% filter(month >= PANEL_START) %>%
  mutate(year = year(month), moy = month(month))
panel[zcols] <- lapply(panel[zcols], function(v) as.numeric(scale(v)))

ctrl <- feat_sf %>% st_drop_geometry() %>%
  dplyr::select(GEOID, total_pop, z_log_dist, z_log_pave, z_log_sidewalk,
                z_log_vacant, z_log_aband, z_log_street, z_log_area,
                z_renter_share, z_vacancy_rate, z_poverty_rate)
panel <- panel %>% inner_join(ctrl, by = "GEOID")
cat(sprintf("panel: %s tract-months (%d tracts x %d months)\n",
            format(nrow(panel), big.mark=","), n_distinct(panel$GEOID), n_distinct(panel$month)))

# -----------------------------------------------------------------------------
# 15. PANEL MODELS
# -----------------------------------------------------------------------------
m_fe <- fepois(inc_n ~ l0+l1+l2+l3+Wl0+Wl1+Wl2+Wl3 | GEOID + year + moy,
               data = panel, cluster = ~GEOID)
print(summary(m_fe))

m_nb_panel <- MASS::glm.nb(
  inc_n ~ l0+l1+l2+l3+Wl0+Wl1+Wl2+Wl3 + z_log_dist+z_log_pave+z_log_sidewalk+
    z_log_vacant+z_log_aband+z_log_street+z_log_area+
    z_renter_share+z_vacancy_rate+poly(z_poverty_rate,2) +
    factor(year)+factor(moy) + offset(log(total_pop)), data = panel)
cat("\nPooled NB distributed lag (cluster-robust SE):\n")
print(lmtest::coeftest(m_nb_panel, vcov = sandwich::vcovCL, cluster = ~GEOID)[1:9,])

panel$res_fe <- panel$inc_n - predict(m_fe, newdata = panel, type = "response")
res_full <- tibble::tibble(GEOID = geoid_order) %>%
  left_join(panel %>% group_by(GEOID) %>% summarise(res = mean(res_fe, na.rm=TRUE), .groups="drop"), by = "GEOID") %>%
  mutate(res = tidyr::replace_na(res, 0)) %>% arrange(match(GEOID, geoid_order))
rm_fe <- spdep::moran.test(res_full$res, wt_m, zero.policy = TRUE)
cat(sprintf("FE residual Moran's I: I = %.4f, p = %.4g\n",
            rm_fe$estimate[["Moran I statistic"]], rm_fe$p.value))

ci <- setNames(as.data.frame(confint(m_fe))[,1:2], c("lo","hi"))
co <- ci %>% tibble::rownames_to_column("term") %>% mutate(est = coef(m_fe)[term]) %>%
  filter(term %in% zcols) %>%
  mutate(source = ifelse(grepl("^W", term), "Neighbour (W)", "Own tract"),
         lag = as.integer(gsub("\\D","",term)),
         lag_lab = factor(lag, 0:3, c("t (same month)","t-1","t-2","t-3")))
ggplot(co, aes(lag_lab, est, color = source)) +
  geom_hline(yintercept = 0, linetype = "dotted", color = pal$ink) +
  geom_pointrange(aes(ymin = lo, ymax = hi), position = position_dodge(width = .4), linewidth = .6) +
  scale_color_manual(values = c("Own tract"=pal$female, "Neighbour (W)"=pal$night), name = NULL) +
  labs(title = "Effect of dark lamp-days on incidents by lag (FE Poisson)", x = NULL, y = "coefficient (log rate)") +
  cap + theme_chart + theme(legend.position = "top")
ggsave("outputs/p8_streetlight_lag_effects.png", width = 6, height = 3.2, dpi = 250)

flow_ct <- function(pts_sf, nm) pts_sf %>%
  st_join(dplyr::select(tracts_sf, GEOID), join = st_intersects) %>% st_drop_geometry() %>%
  dplyr::filter(!is.na(GEOID), GEOID %in% geoid_order) %>%
  dplyr::mutate(month = floor_date(lubridate::as_date(created_date), "month")) %>%
  dplyr::count(GEOID, month, name = nm)
panel_r <- panel %>%
  left_join(flow_ct(sidewalk_sf, "sw_new"), by = c("GEOID","month")) %>%
  left_join(flow_ct(aband_sf,   "ab_new"), by = c("GEOID","month")) %>%
  mutate(across(c(sw_new, ab_new), ~ log1p(tidyr::replace_na(., 0L)))) %>%
  arrange(GEOID, month) %>% group_by(GEOID) %>%
  mutate(sw1 = lag(sw_new, 1), ab1 = lag(ab_new, 1)) %>% ungroup()
m_fe_r <- fepois(inc_n ~ l0+l1+l2+l3+Wl0+Wl1+Wl2+Wl3 + sw_new+sw1 + ab_new+ab1 |
                   GEOID + year + moy, data = panel_r, cluster = ~GEOID)
cat("\nFE with 311 flow controls:\n")
print(fixest::coeftable(m_fe_r)[1:8, ])

# event study: closures of sustained (>= 30 day) outages
sl_ev <- sl_all %>%
  st_join(dplyr::select(tracts_sf, GEOID), join = st_intersects) %>% st_drop_geometry() %>%
  filter(!is.na(GEOID), GEOID %in% geoid_order) %>%
  transmute(GEOID,
            closed = lubridate::as_date(closed_date),
            dur = as.numeric(lubridate::as_date(closed_date) - lubridate::as_date(created_date))) %>%
  filter(!is.na(closed), dur >= 30, dur <= 365)
events <- sl_ev %>% mutate(close_m = floor_date(closed, "month"))
cat(sprintf("sustained-outage closures: %s | median duration: %.0f days\n",
            format(nrow(events), big.mark=","), median(events$dur, na.rm=TRUE)))

ev_months <- seq(STOCK_START, floor_date(PANEL_END %m+% months(2), "month"), by = "month")
ev <- tidyr::expand_grid(GEOID = geoid_order, month = ev_months) %>%
  left_join(events %>% distinct(GEOID, close_m) %>% transmute(GEOID, month = close_m, any_close = 1L),
            by = c("GEOID","month")) %>%
  mutate(any_close = tidyr::replace_na(any_close, 0L)) %>%
  arrange(GEOID, month) %>% group_by(GEOID) %>%
  mutate(F2 = lead(any_close,2), F1 = lead(any_close,1), L0 = any_close,
         L1 = lag(any_close,1), L2 = lag(any_close,2), L3 = lag(any_close,3)) %>%
  ungroup() %>% left_join(inc, by = c("GEOID","month")) %>%
  filter(month >= PANEL_START, month <= PANEL_END) %>%
  mutate(inc_n = tidyr::replace_na(inc_n, 0L), year = year(month), moy = month(month))
cat(sprintf("closure saturation: %.1f%% of tract-months\n", 100*mean(ev$any_close)))

m_ev <- fepois(inc_n ~ F2+F1+L0+L1+L2+L3 | GEOID + year + moy, data = ev, cluster = ~GEOID)
print(summary(m_ev))

ev_terms <- c("F2","F1","L0","L1","L2","L3")
ci_ev <- setNames(as.data.frame(confint(m_ev))[,1:2], c("lo","hi"))
co_ev <- ci_ev %>% tibble::rownames_to_column("term") %>% mutate(est = coef(m_ev)[term]) %>%
  filter(term %in% ev_terms) %>%
  mutate(k = dplyr::recode(term, F2=-2,F1=-1,L0=0,L1=1,L2=2,L3=3),
         phase = ifelse(k < 0, "before closure", "at / after closure"),
         lab = factor(term, ev_terms, c("t-2","t-1","closure","t+1","t+2","t+3")))
ggplot(co_ev, aes(lab, est, color = phase)) +
  geom_hline(yintercept = 0, linetype = "dotted", color = pal$ink) +
  geom_vline(xintercept = 2.5, linetype = "dashed", color = pal$ink, linewidth = .3) +
  geom_pointrange(aes(ymin = lo, ymax = hi), linewidth = .6) +
  scale_color_manual(values = c("before closure"=pal$night, "at / after closure"=pal$female), name = NULL) +
  labs(title = "Incidents around closure of a sustained streetlight outage",
       x = "months relative to closure", y = "coefficient (log rate)") +
  cap + theme_chart + theme(legend.position = "top")
ggsave("outputs/p9_closure_event_study.png", width = 6, height = 3.4, dpi = 250)

# -----------------------------------------------------------------------------
# 16. RESULTS, ROBUSTNESS, MODEL COMPARISON
# -----------------------------------------------------------------------------
LN <- function(...) cat(sprintf(...), "\n")
LN("\n===== SCOPE =====")
LN("public incidents (F+M): %s | female: %s (%.1f%%) | tracts: %s | panel: %s tract-months",
   format(n_human, big.mark=","), format(n_women, big.mark=","), 100*n_women/n_human,
   format(nrow(model_df), big.mark=","), format(nrow(panel), big.mark=","))
LN("dark share: %.1f%% | seasonal MK tau = %.3f (p = %.4g)",
   100*mean(target_h$is_dark, na.rm=TRUE), mk_s$tau, mk_s$sl)

LN("\n===== CLUSTERING =====")
LN("Global Moran's I = %.3f (p = %.3g) | Gi* hot = %d, cold = %d",
   gm$estimate[["Moran I statistic"]], gm$p.value,
   sum(grepl("Hot", model_df$gi)), sum(grepl("Cold", model_df$gi)))
cat("Top 10 hotspot tracts:\n")
print(model_df %>% st_drop_geometry() %>% arrange(desc(gi_z)) %>%
        transmute(GEOID, inc_n, fem_n, gi_z = round(gi_z,1), streetlight_n, sidewalk_n, aband_n,
                  poverty_rate = round(poverty_rate,2)) %>% head(10))

LN("\n===== SPATIAL MRF ESTIMATES (per 1 SD) =====")
print(importance %>% dplyr::select(predictor, domain, beta_sd, pct_per_sd, p, sig))
LN("poverty (quadratic): p = %s", paste(signif(pov_p, 2), collapse = ", "))

f_lin <- inc_n ~ z_log_dist + z_log_light + z_log_pave + z_log_sidewalk +
  z_log_vacant + z_log_aband + z_log_street + z_log_area +
  z_renter_share + z_vacancy_rate + z_poverty_rate + offset(log(total_pop))
m_nb_lin <- MASS::glm.nb(f_lin, data = dat, control = glm.control(maxit = 100))

viol_n <- target_h %>% filter(ky_cd %in% viol_codes) %>%
  st_join(dplyr::select(tracts_sf, GEOID), join = st_intersects) %>% st_drop_geometry() %>%
  filter(!is.na(GEOID)) %>% dplyr::count(GEOID, name = "viol_n")
dat_r <- dat %>% left_join(viol_n, by = "GEOID") %>% mutate(viol_n = tidyr::replace_na(viol_n, 0L))
m_viol <- MASS::glm.nb(update(f_lin, viol_n ~ .), data = dat_r, control = glm.control(maxit = 100))
LN("\n===== VIOLENCE-ONLY REFIT (%.0f%% of pooled outcome) =====",
   100*sum(dat_r$viol_n)/sum(dat$inc_n))
print(round(summary(m_viol)$coefficients[
  c("z_log_light","z_log_pave","z_log_sidewalk","z_log_dist","z_log_vacant","z_log_aband"), c(1,4)], 3))

LN("\n===== PANEL RESULTS =====")
fe_tab <- as.data.frame(fixest::coeftable(m_fe)); names(fe_tab)[1:4] <- c("b","se","z","p")
print(fe_tab %>% tibble::rownames_to_column("term") %>%
        transmute(term, beta = round(b,4), p = signif(p,3), sig = cut(p, c(-Inf,.05,.1,Inf), c("*",".",""))))
bw <- panel %>% group_by(GEOID) %>% summarise(wsd = sd(ls, na.rm=TRUE), m = mean(ls, na.rm=TRUE), .groups="drop")
LN("darkness variation: within-SD = %.3f, between-SD = %.3f",
   mean(bw$wsd, na.rm=TRUE), sd(bw$m, na.rm=TRUE))
ev_tab <- as.data.frame(fixest::coeftable(m_ev)); names(ev_tab)[1:4] <- c("b","se","z","p")
print(ev_tab %>% tibble::rownames_to_column("term") %>%
        transmute(term, beta = round(b,4), p = signif(p,3), sig = cut(p, c(-Inf,.05,.1,Inf), c("*",".",""))))

dev_expl <- function(m) 1 - m$deviance/m$null.deviance
fit_cs <- purrr::imap_dfr(list(Poisson=m_pois, `NB quad`=m_nb, `NB lin`=m_nb_lin),
                          ~ tibble::tibble(model = .y, AIC = round(AIC(.x)), BIC = round(BIC(.x)), dev_expl = round(dev_expl(.x),3)))
fit_cs <- dplyr::bind_rows(fit_cs, tibble::tibble(model="MRF spatial", AIC = round(AIC(m_mrf)), BIC = NA, dev_expl = NA))
LN("\n===== FIT (cross-sectional) ====="); print(fit_cs)

dat$borough <- substr(as.character(dat$GEOID), 3, 5)
spatial_cv <- function(fml) {
  pr <- numeric(nrow(dat))
  for (b in unique(dat$borough)) {
    m <- suppressWarnings(MASS::glm.nb(fml, data = dat[dat$borough != b,], control = glm.control(maxit=100)))
    pr[dat$borough == b] <- predict(m, newdata = dat[dat$borough == b,], type = "response")
  }
  c(RMSE = sqrt(mean((dat$inc_n - pr)^2)), MAE = mean(abs(dat$inc_n - pr)))
}
f_quad <- inc_n ~ z_log_dist+z_log_light+z_log_pave+z_log_sidewalk+
  z_log_vacant+z_log_aband+z_log_street+z_log_area+
  z_renter_share+z_vacancy_rate+poly(z_poverty_rate,2)+offset(log(total_pop))
v0 <- spatial_cv(inc_n ~ 1 + offset(log(total_pop)))
v1 <- spatial_cv(f_quad)
v2 <- spatial_cv(f_lin)
cv_tbl <- tibble::tibble(model = c("NULL","NB quad","NB lin"),
                         RMSE  = c(v0["RMSE"], v1["RMSE"], v2["RMSE"]),
                         MAE   = c(v0["MAE"],  v1["MAE"],  v2["MAE"]))
LN("\n===== BOROUGH LOGO-CV =====")
print(cv_tbl %>% mutate(RMSE = round(RMSE,1), MAE = round(MAE,1)))

readr::write_csv(importance, "outputs/importance_ranking.csv")
readr::write_csv(cmp, "outputs/outcome_comparison.csv")
model_df %>% st_drop_geometry() %>%
  dplyr::select(GEOID, NAME, inc_n, fem_n, sexf_n, inc_pct, gi_z, gi, lisa, lisa_sig,
                streetlight_n, poor_pave_ft, street_ft, sidewalk_n, dist_subway_km,
                vacant_n, aband_n,
                poverty_rate, renter_share, vacancy_rate, female_pop, total_pop) %>%
  readr::write_csv("outputs/tract_level_data.csv")

ggplot(importance, aes(reorder(predictor, beta_sd), beta_sd, color = domain)) +
  geom_hline(yintercept = 0, linetype = "dotted", color = pal$ink) +
  geom_pointrange(aes(ymin = lo, ymax = hi), linewidth = .6) + coord_flip() +
  scale_color_manual(values = c("infrastructure"=pal$female, "socioeconomic"=pal$night,
                                "context"=pal$day), name = NULL) +
  labs(title = "Predictor associations with public-space incidents",
       x = NULL, y = "log IRR per 1 SD (spatial MRF)") +
  cap + theme_chart + theme(legend.position = "top", panel.grid.major.y = element_blank())
ggsave("outputs/p10_importance_ranking.png", width = 6, height = 3.8, dpi = 250)
