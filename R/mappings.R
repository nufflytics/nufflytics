#' ID to Race
#'
#' @param id Racial ID
#'
#' @return Name of race
#' @export

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
         "Unknown race"
  )
}

#' Id to Playertype
#'
#' @param id Playertype ID
#'
#' @return Name of Playertype
#' @export
#'
id_to_playertype <- function(id) {
  id = as.character(id)
  switch(id,
         "1" = "Lineman",
         "2" = "Catcher",
         "3" = "Thrower",
         "4" = "Blitzer",
         "5" = "Ogre",
         "6" = "Longbeard",
         "7" = "Runner",
         "8" = "Blitzer",
         "9" = "Troll Slayer",
         "10" = "Deathroller",
         "11" = "Lineman",
         "12" = "Catcher",
         "13" = "Thrower",
         "14" = "Wardancer",
         "15" = "Treeman",
         "16" = "Lineman",
         "17" = "Thrower",
         "18" = "Gutter Runner",
         "19" = "Storm Vermin",
         "20" = "Rat Ogre",
         "21" = "Lineman",
         "22" = "Goblin",
         "23" = "Thrower",
         "24" = "Black Orc",
         "25" = "Blitzer",
         "26" = "Troll",
         "27" = "Skink",
         "28" = "Saurus",
         "29" = "Kroxigor",
         "32" = "Beastman",
         "33" = "Chaos Warrior",
         "34" = "Minotaur",
         "36" = "Star Player",
         "37" = "Star Player",
         "42" = "Star Player",
         "43" = "Star Player",
         "47" = "Lineman",
         "48" = "Runner",
         "49" = "Assassin",
         "50" = "Blitzer",
         "51" = "Witch Elf",
         "52" = "Star Player",
         "53" = "Star Player",
         "54" = "Skeleton",
         "55" = "Zombie",
         "56" = "Ghoul",
         "57" = "Wight",
         "58" = "Mummy",
         "62" = "Lineman",
         "63" = "Thrower",
         "64" = "Runner",
         "65" = "Berserker",
         "66" = "Ulfwerener",
         "67" = "Yhetee",
         "77" = "Lineman",
         "78" = "Thrower",
         "79" = "Catcher",
         "80" = "Blitzer",
         "81" = "Skeleton",
         "82" = "Thro Ra",
         "83" = "Blitz Ra",
         "84" = "Tomb Guardian",
         "86" = "Zombie",
         "87" = "Ghoul",
         "88" = "Wight",
         "89" = "Flesh Golem",
         "90" = "Werewolf",
         "91" = "Rotter",
         "92" = "Pestigor",
         "93" = "Nurgle Warrior",
         "94" = "Beast of Nurgle",
         "101" = "Star Player",
         "102" = "Star Player",
         "104" = "Star Player",
         "108" = "Hobgoblin",
         "109" = "Blocker",
         "110" = "Bull Centaur",
         "111" = "Minotaur",
         "134" = "Star Player",
         "135" = "Star Player",
         "137" = "Star Player",
         "139" = "Lineman",
         "140" = "Blitzer",
         "141" = "Blocker",
         "146" = "Star Player",
         "149" = "Star Player",
         "152" = "Star Player",
         "217" = "Star Player",
         "243" = "Star Player",
         "251" = "Star Player",
         "254" = "Star Player",
         "275" = "Star Player",
         "277" = "Star Player",
         "282" = "Star Player",
         "Unknown playertype"
  )
}

#' ID to Casualty
#'
#' @param id Casualty ID
#'
#' @return Type of casulty
#' @export
#'
id_to_casualty <- function(id) {
  id = as.character(id)
  switch(id,
         "0"  = NA,
         "1"  = "BH",
         "2"  = "MNG",
         "3"  = "MNG",
         "4"  = "MNG",
         "5"  = "MNG",
         "6"  = "MNG",
         "7"  = "MNG",
         "8"  = "MNG",
         "9"  = "MNG",
         "10" = "Niggle",
         "11" = "Niggle",
         "12" = "-MA",
         "13" = "-MA",
         "14" = "-AV",
         "15" = "-AV",
         "16" = "-AG",
         "17" = "-ST",
         "18" = "Dead",
         "Unknown casualty"
  )
}

#' Star Player Names
#'
#' Converts internal Star Player names into human readable forms
#'
#' @param name Internal representation of Star Player name
#'
#' @return Human readable Star Player name
#' @export
#'
#' @example star_player_name("PLAYER_NAMES_CHAMPION_HUMAIN") #Griff Oberwald
star_player_name <- function(name) {
  switch(name,
         "PLAYER_NAMES_CHAMPION_SKITTER_FALLBACK" = "Skitter Stab-Stab",
         "PLAYER_NAMES_CHAMPION_MIGHTYZUG" = "Mighty Zug",
         "PLAYER_NAMES_CHAMPION_WILHELM" = "Wilhelm Chaney",
         "PLAYER_NAMES_CHAMPION_HUMAIN" = "Griff Oberwald",
         "PLAYER_NAMES_CHAMPION_ELDRILSIDEWINDER" = "Eldril Sidewinder",
         "PLAYER_NAMES_CHAMPION_RAMTUTIII" = "Ramtut III",
         "PLAYER_NAMES_CHAMPION_LORDBORAKTHEDESPOILER" = "Lord Borak the Despoiler",
         "PLAYER_NAMES_CHAMPION_LEWDGRIP_FALLBACK" = "Lewdgrip Whiparm",
         "PLAYER_NAMES_CHAMPION_GLART" = "Glart Smashrip",
         "PLAYER_NAMES_CHAMPION_NEKBREKEREKH" = "Setekh",
         "PLAYER_NAMES_CHAMPION_HUBRISRAKARTH" = "Hubris Rakarth",
         "PLAYER_NAMES_CHAMPION_MORANION_FALLBACK" = "Prince Moranion",
         "PLAYER_NAMES_CHAMPION_SOAREN" = "Soaren Hightower",
         "PLAYER_NAMES_CHAMPION_BARIK" = "Barik Farblast",
         "PLAYER_NAMES_CHAMPION_HOMME_LEZARD" = "Slibli",
         "PLAYER_NAMES_CHAMPION_DARK_ELF" = "Horkon Heartripper",
         "PLAYER_NAMES_CHAMPION_SKAVEN_FALLBACK" = "Headsplitter",
         "PLAYER_NAMES_CHAMPION_ORC" = "Varag Ghoul-Chewer",
         "PLAYER_NAMES_CHAMPION_CHAOS" = "Grashnak Blackhoof",
         "PLAYER_NAMES_CHAMPION_ZZHARGMADEYE" = "Zzharg Madeye",
         "PLAYER_NAMES_CHAMPION_HEMLOCK" = "Hemlock",
         "PLAYER_NAMES_CHAMPION_LOTTABOTTOL_FALLBACK" = "Lottabottol",
         "PLAYER_NAMES_CHAMPION_MORGNTHORG" = "Morg 'n' Thorg",
         name
         )
}


##### Brought across from old package ----
#Collection of data structures containing long and boring information. Eg. translation tables for action types, or lists of metadata values to extract from the replay file

#' Return a .bbrz filepath for testing
#' @export
test_file <- function() {"/Users/pea25i/Library/Application Support/BloodBowl2/Profiles/5E7AF328CEDDB99407DF4F74178D9228/Replays//Coach-49988-b428805ea5a58d7629fd8888b7f3d1c8_2017-01-13_01_49_50.bbrz"}

#' Sets up a new test replay file on restart without long data load process
#' @export
set_up_new_test_file <- function() {
  r = readRDS("data/test_replay_file.Rds")
  r$xml = xml2::read_xml(unzip(test_file(), exdir = "/tmp"))

  r
}

metadata_to_collect <- function() {
  c(
    "HomeScore",
    "AwayScore",
    "CoachHomeName",
    "CoachAwayName",
    "TeamHomeName",
    "TeamAwayName",
    "IdCoachHome",
    "IdCoachAway",
    "Finished"
  )
}

model_type <- function(player_type_id) {
  mapping <- c(
    "27" = "Skink",
    "28" = "Saurus",
    "29" = "Kroxigor",
    "54" = "Skeleton",
    "55" = "Zombie",
    "56" = "Ghoul",
    "57" = "Wight",
    "58" = "Mummy"
  )

  if (player_type_id %in% names(mapping)) return(mapping[player_type_id])
  else return(player_type_id)
}

player_class <- function(player_type) {
  mapping <- c(
    "Skink" = "stunty",
    "Saurus" = "high_strength",
    "Kroxigor" = "big_guy",
    "Skeleton" = "lineman",
    "Zombie" = "lineman",
    "Ghoul" = "positional",
    "Wight" = "blitzer",
    "Mummy" = "big_guy"
  )

  if (player_type %in% names(mapping)) return(mapping[player_type])
  else return(player_type)
}

action_type <- function(action_type_id) {
  mapping <- c(
    "1"  = "Block",
    "2"  = "Blitz",
    "4"  = "Handoff",
    "5"  = "Foul?",
    "6"  = "Blocked Down?",
    "15" = "Negatrait",
    "42" = "Move/Activate?"
  )

  if (action_type_id %in% names(mapping)) return(mapping[action_type_id])
  else return(action_type_id)
}

roll_type <- function(roll_type_id) {
  mapping <- c(
    "1"  = "GFI",
    "2"  = "Dodge",
    "3"  = "Armour",
    "4"  = "Injury",
    "5"  = "Block",
    "7"  = "Pickup",
    "9"  = "Catch",
    "10" = "Scatter",
    "12" = "Pass",
    "16" = "Intercept?",
    "20" = "Bonehead",
    "21" = "Really Stupid",
    "22" = "Wild Animal",
    "23" = "Loner"
  )

  if (roll_type_id %in% names(mapping)) return(mapping[roll_type_id])
  else return(roll_type_id)
}
