read_blubber <- function(){
  # Reads data from Excel sheet and computes derived measures
  readxl::read_excel("data/data_excel.xlsx", 
                     col_names = c("species", "cause_of_death", "month", "accnr", "sex", 
                                   "pregnant", "age", "county", "basin", "body_length", 
                                   "body_weight", "circ_neck", "blubber_neck_ventral", "blubber_neck_side", 
                                   "blubber_neck_dorsal", "circ_chest", "blubber_chest_ventral", 
                                   "blubber_chest_side", "blubber_chest_dorsal", "circ_umbili", 
                                   "blubber_umbili_ventral", "blubber_umbili_side", 
                                   "blubber_umbili_dorsal", "circ_hips", "blubber_hips_ventral", 
                                   "blubber_hips_side", "blubber_hips_dorsal"), skip = 1) %>% 
    mutate(body_length = as.numeric(body_length)) %>% 
    mutate(group = case_when(age < 5 ~ "juvenile",
                             sex == "F" ~ "female",
                             sex == "M" ~ "male"),
           group = fct_relevel(group, "juvenile"),
           pregnant = ifelse((pregnant != "P")|is.na(pregnant), FALSE, TRUE),
           quarter = as.factor(paste0("Q", ceiling(month/3))),
           across(starts_with("circ"), ~.*10/(2*pi), .names = "radi_{.col}"),
           BMI = body_weight / (body_length/100)^2,
           CI = body_weight / (body_length/100)^3,
           WI = circ_chest / body_length) %>% 
    rename_with(~str_remove(.x, "_circ"), starts_with("radi")) %>% 
    rowwise() %>% 
    mutate(mean_area = mean(c_across(starts_with("radi"))^2 * pi / 10000, na.rm = TRUE),
           mean_blubber = mean(c_across(starts_with("blubber"))),
           mean_radius = mean(c_across(starts_with("radi"))),
           area_muscle = pi*(mean_radius - mean_blubber)^2,
           area_blubber = pi*mean_radius^2 - area_muscle,
           prop_blubber = area_blubber/(area_blubber + area_muscle)
           ) %>% 
    ungroup()
}

blubber_ratio <- function(data){
  # Converts all blubber measurements from blubber thickness to proportion of radius
  mutate(data, 
         across(starts_with("blubber_neck"), ~.x/(10 * circ_neck/(2*pi))),
         across(starts_with("blubber_chest"), ~.x/(10 * circ_chest/(2*pi))),
         across(starts_with("blubber_umbili"), ~.x/(10 * circ_umbili/(2*pi))),
         across(starts_with("blubber_hips"), ~.x/(10 * circ_hips/(2*pi)))
  )
}


longer_blubber <- function(data){
  # Converts data to long format
  pivot_longer(data, starts_with("blubber"), names_to = "pos", values_to = "value") %>% 
    mutate(pos = str_remove(pos, "blubber_")) %>% 
    separate(pos, into = c("pos1", "pos2"), sep = "_") %>% 
    mutate(pos1 = fct_relevel(pos1, "neck", "chest", "umbili", "hips")) %>% 
    filter(!is.na(value))
}

longer_dorsal_ventral <- function(data){
  ventral <- data %>% select(-ends_with("dorsal"), -ends_with("side")) %>% 
    pivot_longer(starts_with("blubber"), names_to = "pos", values_to = "value") %>% 
    mutate(pos = str_remove(pos, "blubber_")) %>% 
    separate(pos, into = c("pos1", "pos2"), sep = "_") %>% 
    mutate(pos1 = fct_relevel(pos1, "neck", "chest", "umbili", "hips"))
  dorsal <- data %>% select(accnr, ends_with("dorsal")) %>% 
    pivot_longer(starts_with("blubber"), names_to = "pos", values_to = "value") %>% 
    mutate(pos = str_remove(pos, "blubber_")) %>% 
    separate(pos, into = c("pos1", "pos2"), sep = "_") %>% 
    mutate(pos1 = fct_relevel(pos1, "neck", "chest", "umbili", "hips"))
  left_join(ventral, dorsal, by = c("accnr", "pos1"), suffix = c("_ventral", "_dorsal"))
}

read_skin <- function(){
  # Reads data with skin thickness from Excel sheet
  readxl::read_excel("data/data_excel.xlsx", sheet = "skin") %>% 
    select(blubber = "Ventral blubber chest mm", starts_with("skin"), "body length") %>% 
    janitor::clean_names() %>% 
    rowwise() %>% 
    mutate(skin_mean = mean(c_across(starts_with("skin")), na.rm = TRUE),
           blubber_w_skin = blubber + skin_chest_mm) %>% 
    ungroup()
}
conversion_table <- function(data){
  name <- pull(data, term)
  factor <- exp(pull(data, estimate))
  matrix <- factor %*% t(1 / factor) %>% 
    as.data.frame()
  names(matrix) <- name
  bind_cols(data.frame(Name = name), matrix)
}
