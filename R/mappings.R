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
         "37" = "Star Player",
         "47" = "Lineman",
         "48" = "Runner",
         "49" = "Assassin",
         "50" = "Blitzer",
         "51" = "Witch Elf",
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
         "108" = "Hobgoblin",
         "109" = "Blocker",
         "110" = "Bull Centaur",
         "111" = "Minotaur",
         "135" = "Star Player",
         "139" = "Lineman",
         "140" = "Blitzer",
         "141" = "Blocker",
         "251" = "Star Player",
         "254" = "Star Player",
         "277" = "Star Player",
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
#' @example star_player_name(PLAYER_NAMES_CHAMPION_HUMAIN) #Griff Oberwald
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
         name
         )
}
