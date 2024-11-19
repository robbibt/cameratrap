
#################
# Load packages #
#################

library("RColorBrewer")
library("ggplot2")
library("Cairo")
library("cowplot")
library("tidyverse")


###############
# Working dir #
###############

setwd("C:/Users/Robbi/Dropbox/Other/cameratrap/")

theme_custom = theme( plot.background = element_blank(),
                      panel.background = element_blank(),
                      panel.grid.major = element_blank(),
                      panel.grid.minor = element_blank(),   
                      panel.border = element_blank(),
                      axis.line = element_line(color = 'black'),
                      panel.spacing = unit(0.2, "cm"),
                      legend.key = element_blank(),
                      legend.background = element_blank(),
                      legend.text.align = 0,
                      strip.background = element_blank(),
                      axis.title = element_text(size = 12,  vjust = 0.2),
                      strip.text.x = element_text(size = 12, face = "bold", hjust = 0),
                      strip.text.y = element_text(size = 12, face = "italic", angle = 90))


getwd()


################
# Prepare data #
################

# Import data
camera_data_raw = read_csv("data/cameratrapdata_18Nov2024.csv")
colnames(camera_data_raw)

camera_data_clean = camera_data_raw %>%
  
  # Drop empty rows and unnecessary columns, and rename date column
  filter(is.finite(`Activity (number of hits)`)) %>%
  select(-`2013 Date month`:-`Cumulative number of taxa`,-`Dingo+Fox+Cat+Dog combined`,-`Rats/mice combined`,-`Fire`, -matches("^\\.\\.\\.")) %>%
  rename(Date = `Middle Date`) %>%
  
  # Create additional summary columns for combinations of species
  mutate(
    Date = as.Date(Date, format = "%d/%m/%Y"),
    `Apex predator` = Dingo + Dog,
    `Ferals` = Cat + Fox + Dog,
    `Possums` = `Brush-tailed possum` + `Ring-tailed possum`,
    `Meso predators` = Cat + Fox + Goanna + `Collared sparrowhawk` + `Tiger quoll`,
    `Large mammals` = Echidna + Koala,
    `Small mammals` = `Short white-tailed mouse` + `Small bat` + `Long-nosed bandicoot` + 
                      Anechinus + `Hopping mouse` + `Rat with long whitish rigid tail` + `Tiger quoll`,
    `Birds` = `Rufous fantail` + Catbird + `Lewins honeyeater` + `Whites thrush` + `Brush turkey` + 
              `Noisy pitta` + `Lyre bird` + `Yellow-throated srubwren` + `Wonga pigeon` + Whipbird + 
              `Grey shrike thrush` + `Red-browed firetail` + Magpie + `Yellow robin` + `Emerald dove` + 
              `Collared sparrowhawk` + `Torreian crow?` + `Bar-shouldered dove` + Logrunner + Kookaburra,
    `Large birds` = `Brush turkey` + `Lyre bird`,
    `Small birds` = `Rufous fantail` + Catbird + `Lewins honeyeater` + `Whites thrush` + `Noisy pitta` + 
                    `Yellow-throated srubwren` + `Wonga pigeon` + Whipbird + `Grey shrike thrush` + 
                    `Red-browed firetail` + Magpie + `Yellow robin` + `Emerald dove` + `Collared sparrowhawk` + 
                    `Torreian crow?` + `Bar-shouldered dove` + Logrunner + Kookaburra,
    `Invertebrates` = Moth + Leech + `Crayfish(?)` + Spider + Cicada + Tipulid + Grasshopper,
    `Macropods` = `Swamp wallaby` + `Paddymelon` + `Potoroos??`,
    `Reptiles` = `Forest dragon` + `Land mullet` + Python + Goanna + `Frog?` + `Black snake?`
  ) %>%
  
  # Compute sum for all categories for each date
  group_by(Date) %>%
  summarise_all(sum) %>%
  
  # Convert to long format, and sort by mean count per species
  gather(animal, count, -Date) %>%
  mutate(animal = forcats::fct_reorder(
    factor(animal),
    count,
    max,
    na.rm = TRUE,
    .desc = TRUE
  )) %>% 
  
  # Add custom annotations
  mutate(animal_text = recode(animal, 
                       'Macropods' = "paste(bold('Macropods'), ' (e.g. Pademelons, Wallabies)')",
                       'Possums' = "paste(bold('Possums'), ' (e.g. Brush-tailed, Ring-tailed)')",
                       'Large birds' = "paste(bold('Large birds'), ' (e.g. Brush Turkeys, Lyrebirds)')",
                       'Small birds' = "paste(bold('Small birds'), ' (e.g. Robins, scrubwrens, honeyeaters)')",
                       'Small mammals' = "paste(bold('Small mammals'), ' (e.g. Bandicoots, Antechinus)')",
                       'Dingo' = "bold('Dingo')",
                       'Ferals' = "paste(bold('Ferals'), ' (e.g. Cats, foxes, dogs)')",
                       'Reptiles' = "paste(bold('Reptiles'), ' (e.g. Goannas, snakes, lizards)')")) 



# Create datafame of Winter seasons to add as underlay
seasons = data.frame(
  xmin = seq(as.Date('2012-06-01'), length.out = 13, by = "years"),
  xmax = seq(as.Date('2012-08-31'), length.out = 13, by = "years"),
  ymin = -Inf,
  ymax = Inf
)


# Export all species
camera_data_clean %>%
  
  # Plot data
  ggplot() + 
  geom_rect(data=seasons, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), alpha = 0.08) +
  geom_ribbon(aes(x = Date, ymin = 0, ymax = count, fill=animal), colour='#5e5e5e') +
  geom_vline(aes(xintercept= as.Date('2019-12-24', format = "%Y-%m-%d")), linetype='dashed', linewidth=0.5, color='black') +
  facet_wrap(~animal, ncol=1, scales = "free_y") +
  theme_custom + 
  guides(fill="none") +
  # scale_fill_brewer(palette="Spectral") +
  scale_x_date(expand=c(0.0, 0.0)) + 
  scale_y_continuous(expand=c(0.0, 0.0)) + 
  ylab("") 

ggsave("results/output_species_all.jpg", width = 7, height = 49)


########################################


library('rphylopic')
wallaby_img = pick_phylopic(uuid="940c14a5-1acc-4998-85a6-a4002e980fa8")
possum_img = pick_phylopic(uuid="f5592cab-cc61-4aab-b1dd-fba7cd2df7c9")
lyrebird_img = pick_phylopic(uuid="217b650d-edaa-4f21-ac5d-03908fd5eac4")
turkey_img = pick_phylopic(uuid="828b7d15-d69f-4773-99c5-9eb571d8db19")
robin_img = pick_phylopic(uuid="3c4ef873-76b5-473d-a740-8fcb4864462b")
bandi_img = pick_phylopic(uuid="d9e97d9d-cbaa-42d1-93fd-6a787c1b4afc")
antech_img = pick_phylopic(uuid="295cd9f7-eef2-441e-ba7e-40c772ca7611")
dingo_img = pick_phylopic(uuid="3c534a59-fd0c-41bb-80c7-1d18db9bae13")
fox_img = pick_phylopic(uuid="76352962-1eeb-4197-acdd-e3c7eeab839d")
cat_img = pick_phylopic(uuid="23cd6aa4-9587-4a2e-8e26-de42885004c9")
goanna_img = pick_phylopic(uuid="ce6a78bc-3ef1-4d60-ab40-113eb84c7802")
snake_img = pick_phylopic(uuid="403858a0-6fd0-4db8-a38f-6f6d93a9bf74")

# Export selected species and groups
output_selected = camera_data_clean %>%
  
  # Select data to plot
  dplyr::filter(animal %in% c('Dingo', 'Large birds', 'Small birds', 'Possums', 
                              'Macropods', 'Small mammals', 'Ferals', 'Reptiles')) %>%

  # Plot data
  ggplot() + 
  geom_rect(data=seasons, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), alpha = 0.08) +
  geom_ribbon(aes(x = Date, ymin = 0, ymax = count, fill=animal_text), colour='#5e5e5e') +
  geom_vline(aes(xintercept= as.Date('2019-12-24', format = "%Y-%m-%d")), linetype='dashed', size=0.3, color='black') +
  # geom_label(y=100, x=as.Date('2019-12-24', format = "%Y-%m-%d"), label="Black\nSummer\nfire", size=5, position='nudge') +
  facet_wrap(~animal_text, ncol=1, scales = "free_y", labeller=label_parsed) +
  theme_custom + 
  guides(fill="none") +
  scale_fill_brewer(palette="Spectral") +
  scale_x_date(expand=c(0.0, 0.0)) + 
  scale_y_continuous(expand=c(0.0, 0.0)) + 
  ylab("") +
  annotate(geom = "text", x = as.Date('2012-07-10', format = "%Y-%m-%d"), y = 0, label = "Winter", size=3, color = "grey",
           angle = 90, hjust=-1.15) 

# Add annotations
cowplot::plot_grid(output_selected) +
  add_phylopic(img=wallaby_img, x = 0.935, y = 0.975, height = 0.055) +
  add_phylopic(img=possum_img, x = 0.935, y = 0.86, height = 0.045) +
  add_phylopic(img=turkey_img, x = 0.87, y = 0.74, height = 0.03) +
  add_phylopic(img=lyrebird_img, x = 0.95, y = 0.742, height = 0.03) +
  add_phylopic(img=robin_img, x = 0.965, y = 0.62, height = 0.025) +
  add_phylopic(img=bandi_img, x = 0.89, y = 0.50, height = 0.022) +
  add_phylopic(img=antech_img, x = 0.96, y = 0.497, height = 0.015) +
  add_phylopic(img=dingo_img, x = 0.963, y = 0.383, height = 0.035) +
  add_phylopic(img=cat_img, x = 0.89, y = 0.264, height = 0.026) +
  add_phylopic(img=fox_img, x = 0.96, y = 0.264, height = 0.024) +
  add_phylopic(img=goanna_img, x = 0.87, y = 0.143, height = 0.027) +
  add_phylopic(img=snake_img, x = 0.95, y = 0.143, height = 0.019) +
  cowplot::draw_text(c('🔥 Black Summer', 'Fire (24.12.2019)'), x = 0.69, y = c(0.982, 0.971), size=7)

ggsave("results/output_species_selected.jpg", width = 7, height = 10)


#################################################


# Richness, activity etc
camera_data_clean %>%
  
  filter(count > 0,!(
    animal %in% c(
      'Apex predator',
      'Ferals',
      'Meso predators',
      'Large mammals',
      'Small mammals',
      'Birds',
      'Large birds',
      'Small birds',
      'Invertebrates',
      'Reptiles'
    )
  )) %>%
  # group_by(Date) %>%
  
  mutate(Date = lubridate::floor_date(Date, "month")) %>%
  group_by(Date) %>%
  
  summarise(species_n = n_distinct(animal),
            activity = sum(count)) %>% 
  gather(variable, value, -Date) %>% 
  
  # Plot data
  ggplot() + 
  geom_rect(data=seasons, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), alpha = 0.08) +
  geom_ribbon(aes(x =Date, ymin = 0, ymax = value, fill=variable), colour='#5e5e5e') +
  geom_vline(aes(xintercept= as.Date('2019-12-24', format = "%Y-%m-%d")), linetype='dashed', size=0.5, color='black') +
  # geom_label(y=100, x=as.Date('2019-12-24', format = "%Y-%m-%d"), label="Black\nSummer\nfire", size=5, position='nudge') +
  facet_wrap(~variable, ncol=1, scales = "free_y") +
  theme_custom + 
  guides(fill="none") +
  scale_fill_brewer(palette="Spectral") +
  scale_x_date(expand=c(0.0, 0.0)) + 
  scale_y_continuous(expand=c(0.0, 0.0)) + 
  ylab("") 

ggsave("results/output_summary.jpg", width = 7, height = 5)
