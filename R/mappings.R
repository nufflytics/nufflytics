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
         "6"  = "Goblin",
         "7"  = "Wood Elf",
         "8"  = "Chaos",
         "9"  = "Dark Elf",
         "10" = "Undead",
         "12" = "Norse",
         "11" = "Halfling",
         "13" = "Amazon",
         "14" = "Elven Union",
         "15" = "High Elf",
         "16" = "Khemri",
         "17" = "Necromantic",
         "18" = "Nurgle",
         "19" = "Ogre",
         "20" = "Vampire",
         "21" = "Chaos Dwarf",
         "22" = "Underworld Denizens",
         "24" = "Brettonian",
         "25" = "Kislev Circus",
         "32" = "Superior Being Ring",
         "33" = "Chaotic Player Pact",
         "34" = "Chaos Gods Selection",
         "35" = "Far East Association",
         "36" = "Elfic Grand Coalition",
         "37" = "Anti-Fur Society",
         "38" = "Alliance of Goodness",
         "39" = "Human League",
         "40" = "Violence Together",
         "41" = "Union of Small People",
         "42" = "Afterlife United",
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
         "30" = "Goblin",
         "31" = "Looney",
         "32" = "Beastman",
         "33" = "Chaos Warrior",
         "34" = "Minotaur",
         "36" = "Star Player",
         "37" = "Star Player",
         "41" = "Star Player",
         "42" = "Star Player",
         "43" = "Star Player",
         "44" = "Troll",
         "45" = "Pogoer",
         "46" = "Fanatic",
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
         "59" = "Star Player",
         "60" = "Halfling",
         "61" = "Treeman",
         "62" = "Lineman",
         "63" = "Thrower",
         "64" = "Runner",
         "65" = "Berserker",
         "66" = "Ulfwerener",
         "67" = "Yhetee",
         "68" = "Linewoman",
         "69" = "Thrower",
         "70" = "Catcher",
         "71" = "Blitzer",
         "72" = "Lineman",
         "73" = "Thrower",
         "74" = "Catcher",
         "75" = "Blitzer",
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
         "95" = "Gnoblar",
         "96" = "Ogre",
         "97" = "Thrall",
         "98" = "Vampire",
         "99" = "Star Player",
         "100" = "Star Player",
         "101" = "Star Player",
         "102" = "Star Player",
         "103" = "Star Player",
         "104" = "Star Player",
         "105" = "Star Player",
         "107" = "Bombardier",
         "108" = "Hobgoblin",
         "109" = "Blocker",
         "110" = "Bull Centaur",
         "111" = "Minotaur",
         "123" = "Goblin",
         "124" = "Lineman",
         "125" = "Thrower",
         "126" = "Blitzer",
         "127" = "Warpstone Troll",
         "133" = "Star Player",
         "134" = "Star Player",
         "135" = "Star Player",
         "137" = "Star Player",
         "139" = "Lineman",
         "140" = "Blitzer",
         "141" = "Blocker",
         "142" = "Lineman",
         "143" = "Catcher",
         "144" = "Blitzer",
         "145" = "Tame Bear",
         "146" = "Star Player",
         "147" = "Star Player",
         "148" = "Star Player",
         "149" = "Star Player",
         "152" = "Star Player",
         "164" = "Star Player",
         "217" = "Star Player",
         "243" = "Star Player",
         "244" = "Star Player",
         "247" = "Star Player",
         "251" = "Star Player",
         "252" = "Star Player",
         "254" = "Star Player",
         "255" = "Star Player",
         "275" = "Star Player",
         "277" = "Star Player",
         "280" = "Star Player",
         "282" = "Star Player",
         "304" = "Star Player",
         "306" = "Star Player",
         "310" = "Star Player",
         "316" = "Star Player",
         "318" = "Star Player",

         #Mixed Teams
         "323" = "High Elf Lineman",
         "324" = "High Elf Blitzer",
         "325" = "High Elf Thrower",
         "326" = "High Elf Catcher",
         "327" = "Vampire Thrall",
         "328" = "Vampire",
         "329" = "Chaos Dwarf Hobgoblin",
         "330" = "Chaos Dwarf Blocker",
         "331" = "Chaos Dwarf Bull Centaur",
         "332" = "Chaos Dwarf Minotaur",
         "333" = "Brettonian Lineman",
         "334" = "Brettonian Blocker",
         "335" = "Brettonian Blitzer",
         "336" = "Chaos Beastman",
         "337" = "Chaos Warrior",
         "338" = "Chaos Minotaur",
         "339" = "Skaven Lineman",
         "340" = "Skaven Thrower",
         "341" = "Skaven Gutter Runner",
         "342" = "Skaven Storm Vermin",
         "343" = "Skaven Rat Ogre",
         "344" = "Dark Elf Blitzer",
         "345" = "Dark Elf Lineman",
         "346" = "Dark Elf Assassin",
         "347" = "Dark Elf Runner",
         "348" = "Dark Elf Witch Elf",
         "355" = "Chaos Beastman",
         "356" = "Chaos Warrior",
         "357" = "Chaos Minotaur",
         "358" = "Nurgle Rotter",
         "359" = "Nurgle Pestigor",
         "360" = "Nurgle Warrior",
         "361" = "Beast of Nurgle",
         "362" = "Chaos Dwarf Hobgoblin",
         "363" = "Chaos Dwarf Blocker",
         "364" = "Chaos Dwarf Bull Centaur",
         "365" = "Chaos Dwarf Minotaur",
         "366" = "Orc Lineman",
         "367" = "Orc Blitzer",
         "368" = "Orc Thrower",
         "369" = "Orc Goblin",
         "370" = "Black Orc",
         "371" = "Orc Troll",
         "372" = "Goblin",
         "373" = "Goblin Looney",
         "374" = "Goblin Pogoer",
         "375" = "Goblin Bombadier",
         "376" = "Goblin Fanatic",
         "377" = "Goblin Troll",
         "378" = "Skaven Lineman",
         "379" = "Skaven Storm Vermin",
         "380" = "Skaven Thrower",
         "381" = "Skaven Gutter Runner",
         "382" = "Skaven Rat Ogre",
         "387" = "High Elf Lineman",
         "388" = "High Elf Blitzer",
         "389" = "High Elf Catcher",
         "400" = "High Elf Thrower",
         "401" = "Kislev Circus Lineman",
         "402" = "Kislev Circus Blitzer",
         "403" = "Kislev Circus Catcher",
         "404" = "Kislev Circus Tame Bear",
         "405" = "Norse Lineman",
         "406" = "Norse Thrower",
         "407" = "Norse Runner",
         "408" = "Norse Berserker",
         "409" = "Norse Ulfwerener",
         "410" = "Norse Yhetee",
         "411" = "Amazon Linewoman",
         "412" = "Amazon Thrower",
         "413" = "Amazon Catcher",
         "414" = "Amazon Blitzer",
         "415" = "Lizardmen Skink",
         "416" = "Lizardmen Saurus",
         "417" = "Lizardmen Kroxigor",
         "418" = "Brettonian Blocker",
         "419" = "Brettonian Blitzer",
         "420" = "Brettonian Lineman",
         "421" = "Human Blitzer",
         "422" = "Human Lineman",
         "423" = "Human Catcher",
         "424" = "Human Thrower",
         "425" = "Human Ogre",
         "426" = "Dwarf Longbeard",
         "427" = "Dwarf Blitzer",
         "428" = "Dwarf Troll Slayer",
         "429" = "Dwarf Runner",
         "430" = "Dwarf Deathroller",
         "431" = "Halfling",
         "432" = "Halfling Treeman",
         "438" = "Human Lineman",
         "439" = "Human Blitzer",
         "440" = "Human Catcher",
         "441" = "Human Thrower",
         "442" = "Human Ogre",
         "443" = "Brettonian Lineman",
         "444" = "Brettonian Blocker",
         "445" = "Brettonain Blitzer",
         "446" = "Kislev Circus Lineman",
         "447" = "Kislev Circus Blitzer",
         "448" = "Kislev Circus Catcher",
         "449" = "Kislev Circus Tame Bear",
         "450" = "Norse Lineman",
         "451" = "Norse Runner",
         "452" = "Norse Thrower",
         "453" = "Norse Berserker",
         "454" = "Norse Ulfwerener",
         "455" = "Norse Yhetee",
         "456" = "Amazon Linewoman",
         "457" = "Amazon Thrower",
         "458" = "Amazon Catcher",
         "459" = "Amazon Blitzer",
         "460" = "Orc Lineman",
         "461" = "Orc Blitzer",
         "462" = "Orc Thrower",
         "463" = "Orc Goblin",
         "464" = "Black Orc",
         "465" = "Orc Troll",
         "466" = "Goblin",
         "467" = "Goblin Looney",
         "468" = "Goblin Pogoer",
         "469" = "Goblin Bombadier",
         "470" = "Goblin Fanatic",
         "471" = "Goblin Troll",
         "472" = "Lizardmen Skink",
         "473" = "Lizardmen Saurus",
         "474" = "Lizardmen Kroxigor",
         "475" = "Ogre",
         "476" = "Ogre Gnoblar",
         "477" = "Ogre",
         "478" = "Ogre Gnoblar",
         "479" = "Goblin",
         "480" = "Goblin Bombadier",
         "481" = "Goblin Looney",
         "482" = "Goblin Pogoer",
         "483" = "Goblin Fanatic",
         "484" = "Goblin Troll",
         "485" = "Halfling",
         "486" = "Halfling Treeman",
         "492" = "Undead Skeleton",
         "493" = "Undead Zombie",
         "494" = "Undead Ghoul",
         "495" = "Undead Wight",
         "496" = "Undead Mummy",
         "497" = "Necromantic Werewolf",
         "498" = "Necromantic Flesh Golem",
         "499" = "Khemri Skeleton",
         "500" = "Khemri Thro-Ra",
         "501" = "Khemri Blitz-Ra",
         "502" = "Khemri Tomb Guardian",
         "503" = "Vampire Thrall",
         "504" = "Vampire",
         "732" = "Underworld Goblin",
         "733" = "Underworld Lineman",
         "734" = "Underworld Thrower",
         "735" = "Underworld Blitzer",
         "736" = "Underworld Warpstone Troll",
         "737" = "Ogre Gnoblar",
         "738" = "Ogre",
         "746" = "Elven Union Lineman",
         "747" = "Elven Union Blitzer",
         "748" = "Elven Union Thrower",
         "749" = "Elven Union Catcher",
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
  if(is.null(id)) return(NULL)
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

#' Casualty state to effect
#'
#' Casualties are stored internally as specific injuries (from CRP table). This converts them into the in game effect on the player.
#'
#' @param state string describing the casualty
#'
#' @return a string describing the effect of the casualty (MNG/Niggle/etc.)
#' @export
#'
#' @examples
state_to_casualty <- function(state) {
  if(is.null(state)) return(NULL)
  state = as.character(state)
  switch (state,
    "BadlyHurt" = "BH",
    "BrokenRibs" = "MNG",
    "GroinStrain" = "MNG",
    "GougedEyes" = "MNG",
    "BrokenJaw" = "MNG",
    "FracturedArm" = "MNG",
    "FracturedLeg" = "MNG",
    "SmashedHand" = "MNG",
    "PinchedNerve" = "MNG",
    "DamagedBack" = "Niggle",
    "SmashedKnee" = "Niggle",
    "SmashedHip" = "-MA",
    "SmashedAnkle" = "-MA",
    "SeriousConcussion" = "-AV",
    "FracturedSkull" = "-AV",
    "BrokenNeck" = "-AG",
    "SmashedCollarBone" = "-ST",
    "Dead" = "Dead",
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
         "PLAYER_NAMES_CHAMPION_UNDEAD" = "Count Luthor Von Drakenborg",
         "PLAYER_NAMES_CHAMPION_FUNGUS_FALLBACK" = "Fungus the Loon",
         "PLAYER_NAMES_CHAMPION_HACK_FALLBACK" = "Hack Enslash",
         "PLAYER_NAMES_CHAMPION_DEEPROOTSTRONGBRANCH" = "Deeproot Strongbranch",
         "PLAYER_NAMES_CHAMPION_DOLFAR" = "Dolfar Longstride",
         "PLAYER_NAMES_CHAMPION_GOBELIN" = "Ripper",
         "PLAYER_NAMES_CHAMPION_HELMUT_FALLBACK" = "Helmut Wulf",
         "PLAYER_NAMES_CHAMPION_ROXANNA_FALLBACK" = "Roxanna Darknail",
         "PLAYER_NAMES_CHAMPION_ZARATHESLAYER" = "Zara the Slayer",
         "PLAYER_NAMES_CHAMPION_NOBBLA_FALLBACK" = "Nobbla Blackwart",
         "PLAYER_NAMES_CHAMPION_MAX_FALLBACK" = "Max Spleenripper",
         "PLAYER_NAMES_CHAMPION_SCRAPPASOREHEAD" = "Scrappa Sorehead",
         "PLAYER_NAMES_CHAMPION_GROTTY" = "Grotty",
         "PLAYER_NAMES_CHAMPION_BRICK" = "Brick Far'th",
         "PLAYER_NAMES_CHAMPION_BOOMER_FALLBACK" = "Boomer Eziasson",
         "PLAYER_NAMES_CHAMPION_WILLOW_FALLBACK" = "Willow Rosebark",
         "PLAYER_NAMES_CHAMPION_BOMBERDRIBBLESNOT" = "Bomber Dribblesnot",
         "PLAYER_NAMES_CHAMPION_UGROTH_FALLBACK" = "Ugroth Bolgrot",
         "PLAYER_NAMES_CHAMPION_FEZGLITCH_FALLBACK" = "Fezglitch",
         "PLAYER_NAMES_CHAMPION_BERTHA_FALLBACK" = "Bertha Bigfist",
         "PLAYER_NAMES_CHAMPION_PUGGY_FALLBACK" = "Puggy Baconbreath",
         "PLAYER_NAMES_CHAMPION_HUMERUS_FALLBACK" = "Humerus Carpal",
         "PLAYER_NAMES_CHAMPION_IGOR_FALLBACK" = "Crazy Igor",
         "PLAYER_NAMES_CHAMPION_HAKFLEMSKUTTLESPIKE" = "Hakflem Skuttlespike",
         "PLAYER_NAMES_CHAMPION_NAIN" = "Grim Ironjaw",
         "PLAYER_NAMES_CHAMPION_FLINT_FALLBACK" = "Flint Churnblade",
         "PLAYER_NAMES_CHAMPION_ELFE_SYLVAIN" = "Jordel Freshbreeze",
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
