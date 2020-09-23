# Assign individual LAFA categories to the groups they belong to
# QDR / FWI / 10 Feb. 2020

lafa <- list('dairy', 'fat', 'fruit', 'grain', 'meat', 'sugar', 'veg') %>%
  map_dfr(~ data.frame(Group = ., Food = unique(get(.)$Category)))

write_csv(lafa, file.path(fp_crosswalks, 'lafa_category_structure.csv'))
