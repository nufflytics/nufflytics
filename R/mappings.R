id_to_race <- function(id) {
  id = as.character(id)
  switch(id,
    "1"  = "Human",
    "2"  = "Dwarf",
    "3"  = "Skaven",
    "4"  = "Orc",
    "5"  = "Lizardmen",
    "7"  = "Wood Elf",
    "8"  = "Chaos",
    "9"  = "Dark Elf",
    "10" = "Undead",
    "12" = "Norse",
    "15" = "High Elf",
    "16" = "Khemri",
    "17" = "Necromantic",
    "18" = "Nurgle",
    "21" = "Chaos Dwarf",
    "24" = "Brettonian",
    "Unlisted"
  )
}
