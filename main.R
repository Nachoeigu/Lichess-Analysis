library(bigchess)
## File with the historical performance on Lichess
historical_data <- read.pgn("chess.pgn")
View(historical_data)
library(tidyverse)
library("stringr")
library(lubridate)
library(data.table)
library(seplyr)

#Removing unnecessary information about the total movements per piece
historical_data <- deselect(historical_data, c('B_moves', 'K_moves', 'N_moves', 'O_moves', 'Q_moves', 'R_moves'))

#Creating columns about my movements -as well if there were castling- in each piece
historical_data <- mutate(historical_data, King_movements= ifelse(White=='Nachoeigu', W_K_moves, B_K_moves))
historical_data <- mutate(historical_data, Queen_movements= ifelse(White=='Nachoeigu', W_Q_moves, B_Q_moves))
historical_data <- mutate(historical_data, Rook_movements= ifelse(White=='Nachoeigu', W_R_moves, B_R_moves))
historical_data <- mutate(historical_data, Bishop_movements= ifelse(White=='Nachoeigu', W_B_moves, B_B_moves))
historical_data <- mutate(historical_data, Knight_movements= ifelse(White=='Nachoeigu', W_N_moves, B_N_moves))
historical_data <- mutate(historical_data, My_castling= ifelse(White=='Nachoeigu'& W_O_moves==1, "Yes", ifelse(White=='Nachoeigu' & W_O_moves==0,"No", ifelse(Black=='Nachoeigu' & B_O_moves==1, "Yes", "No"))))
historical_data <- mutate(historical_data, Rival_castling= ifelse(White!='Nachoeigu'& W_O_moves==1, "Yes", ifelse(White!='Nachoeigu' & W_O_moves==0,"No", ifelse(Black!='Nachoeigu' & B_O_moves==1, "Yes", "No"))))
historical_data <- historical_data %>% deselect(c('W_B_moves', 'W_K_moves', 'W_N_moves', 'W_O_moves', 'W_Q_moves', 'W_R_moves', 'B_B_moves', 'B_K_moves', 'B_N_moves', 'B_O_moves', 'B_Q_moves', 'B_R_moves'))

## All the matches -without casual games- and after cleaning URLs from the Event column
matches <- historical_data %>% deselect(c('Site', 'Round')) %>% filter(!Event %like% 'Casual') %>% filter(Movetext!='')
matches <- separate(matches,Event, c('Events', 'Delete'), 'htt') %>% deselect('Delete')

## Cleaning and formatting the date column
matches$Date <- gsub('.', '-', matches$Date, fixed = TRUE) %>% ymd()

## Cleaning and formatting the Events column
matches$Events <- as.factor(matches$Events)

## Replacing the number of the result with the result status
View(matches)
matches$Result <- as.character(matches$Result)
matches$Result[matches$Result == '1-0'] <- ifelse(matches$White[matches$Result == '1-0'] == 'Nachoeigu', 'Win', 'Lose')
matches$Result[matches$Result == '0-1'] <- ifelse(matches$White[matches$Result == '0-1'] == 'Nachoeigu', 'Lose', 'Win')
matches$Result[matches$Result == '1/2-1/2'] <- 'Draw'
View(matches)

## Replace white and black columns with Position and rival
matches <- mutate(matches,My_position='1', Rival='a')
matches$My_position <- ifelse(matches$White=='Nachoeigu', "White", 'Black')
matches$Rival <- ifelse(matches$White != 'Nachoeigu', matches$White, matches$Black)
matches <- deselect(matches, c('White', 'Black'))

#Create a function which name, in a new column, what kind of basic opening is each one
fun <- function(a) {
  if (startsWith(a, '1. e4 e5')) {
  a <- 'Open Game'
} else if (startsWith(a, '1. e4 e4')){
  a <- 'Semi Open Game'
} else if (startsWith(a,'1. e4 a5')) {
  a <- 'Semi Open Game'
} else if (startsWith(a,'1. e4 a6')) {
  a <- 'Semi Open Game'
} else if (startsWith(a,'1. e4 b5')) {
  a <- 'Semi Open Game'
} else if (startsWith(a,'1. e4 b6')) {
  a <- 'Semi Open Game'
} else if (startsWith(a,'1. e4 c5')) {
  a <- 'Semi Open Game'
} else if (startsWith(a,'1. e4 c6')) {
  a <- 'Semi Open Game'
} else if (startsWith(a,'1. e4 d5')) {
  a <- 'Semi Open Game'
} else if (startsWith(a,'1. e4 d6')) {
  a <- 'Semi Open Game'
} else if (startsWith(a,'1. e4 f5')) {
  a <- 'Semi Open Game'
} else if (startsWith(a,'1. e4 f6')) {
  a <- 'Semi Open Game'
} else if (startsWith(a,'1. e4 g5')) {
  a <- 'Semi Open Game'
} else if (startsWith(a,'1. e4 g6')) {
  a <- 'Semi Open Game'
} else if (startsWith(a,'1. e4 h5')) {
  a <- 'Semi Open Game'
} else if (startsWith(a,'1. e4 h6')) {
  a <- 'Semi Open Game'
} else if (startsWith(a,'1. d4 d5')) {
  a <- 'Closed Game'
} else  if (startsWith(a,'1. d4 a5')) {
  a <- 'Semi Closed Game'
} else  if (startsWith(a,'1. d4 a6')) {
  a <- 'Semi Closed Game'
} else  if (startsWith(a,'1. d4 b5')) {
  a <- 'Semi Closed Game'
} else  if (startsWith(a,'1. d4 b6')) {
  a <- 'Semi Closed Game'
} else  if (startsWith(a,'1. d4 c5')) {
  a <- 'Semi Closed Game'
} else  if (startsWith(a,'1. d4 c6')) {
  a <- 'Semi Closed Game'
} else  if (startsWith(a,'1. d4 e5')) {
  a <- 'Semi Closed Game'
} else  if (startsWith(a,'1. d4 e6')) {
  a <- 'Semi Closed Game'
} else  if (startsWith(a,'1. d4 f5')) {
  a <- 'Semi Closed Game'
} else  if (startsWith(a,'1. d4 f6')) {
  a <- 'Semi Closed Game'
} else  if (startsWith(a,'1. d4 g5')) {
  a <- 'Semi Closed Game'
} else  if (startsWith(a,'1. d4 g6')) {
  a <- 'Semi Closed Game'
} else  if (startsWith(a,'1. d4 h5')) {
  a <- 'Semi Closed Game'
} else  if (startsWith(a,'1. d4 h6')) {
  a <- 'Semi Closed Game'
} else if (startsWith(a,'1. a3')) {
  a <- 'Flank White Opening'
} else if (startsWith(a,'1. a4')) {
  a <- 'Flank White Opening'
} else if (startsWith(a,'1. b3')) {
  a <- 'Flank White Opening'
} else if (startsWith(a,'1. b4')) {
  a <- 'Flank White Opening'
} else if (startsWith(a,'1. c3')) {
  a <- 'Flank White Opening'
} else if (startsWith(a,'1. c4')) {
  a <- 'Flank White Opening'
} else if (startsWith(a,'1. f3')) {
  a <- 'Flank White Opening'
} else if (startsWith(a,'1. f4')) {
  a <- 'Flank White Opening'
} else if (startsWith(a,'1. g3')) {
  a <- 'Flank White Opening'
} else if (startsWith(a,'1. g4')) {
  a <- 'Flank White Opening'
} else if (startsWith(a,'1. h3')) {
  a <- 'Flank White Opening'
} else if (startsWith(a,'1. h4')) {
  a <- 'Flank White Opening'
} else if (startsWith(a,'1. Nf3')) {
  a <- 'Flank White Opening'
} else if (startsWith(a,'1. Na3')) {
  a <- 'Flank White Opening'
} else if (startsWith(a,'1. Nc3')) {
  a <- 'Flank White Opening'
} else if (startsWith(a,'1. Nh3')) {
  a <- 'Flank White Opening'
} else {
  a <- 'Unusual Opening'
}}

#Creating the Opening column after creating the custom function
matches <- mutate(matches,Opening=Movetext)
matches$Opening <- unlist(lapply(matches$Opening, fun))
fun2 <- function(a) {
  if(startsWith(a, '1. e4 e5 2. Nf3 Nc6 3. Bb5')) {
    a <- 'Ruy-Lopez Opening'
  } else if (startsWith(a, '1. e4 e5 2.Nf3 Nc6 3.Bc4')){
    a <- 'Italian Game'
  } else if (startsWith(a,'1. e4 c5 2. Nc3')) {
    a <- 'Sicilian Defense: Closed'
  } else if (startsWith(a,'1. e4 d6 2. d4 Nf6')) {
    a <-'Pirc Defense'
  } else if (startsWith(a,'1. e4 c5 2. c3')) {
    a <-'Sicilian Defense Alapin Variation'
  } else if (startsWith(a,'1. e4 Nf6')) {
    a <-'Alekhine\'s Defense'
  } else if (startsWith(a,'1. e4 e5 2. f4')){
    a <-'King\'s Gambit'
  } else if (startsWith(a,'1. e4 e5 2. Nc3')){
    a <-'Vienna Game'
  } else if (startsWith(a,'1. e4 e5 2. Nf3 Nc6 3. d4')){
    a <-'Scotch Game'
  } else if (startsWith(a,'1. d4 d5 2. c4')){
    a <-'Queen\'s Gambit'
  } else if (startsWith(a,'1. d4 d5 2. c4 c6')){
    a <-'Slav Defense'
  } else if (startsWith(a,'1. d4 Nf6 2. c4 g6')){
    a <-'King\'s Indian Defense'
  } else if (startsWith(a,'1. d4 Nf6 2. c4 e6 3. Nf3 Bb4+')){
    a <-'Bogo-Indian Defense'
  } else if (startsWith(a,'1. d4 Nf6 2. c4 e6 3. Nf3 b6')){
    a <-'Queen\'s Indian Defense'
  } else if (startsWith(a,'1. d4 Nf6 2. c4 e6 3. Nc3 Bb4')){
    a <-'Nimzo-Indian Defense'
  } else if (startsWith(a,'1. d4 Nf6 2. c4 g6 3. Nc3 d5')){
    a <-'Grunfeld Defense'
  } else if (startsWith(a,'1. d4 Nf6 2. Bg5')){
    a <-'Trompowsky Attack'
  } else if (startsWith(a,'1. d4 f5')){
    a <-'Dutch Defens'
  } else if (startsWith(a,'1. d4 Nf6 2. c4 c5 3. d5 e6 4. Nc3 exd5 5. cxd5 d6')){
    a <-'Benoni Defense: Modern Variation'
  } else if (startsWith(a,'1. d4 d5 2. Nf3 Nf6 3. Bf4')){
    a <-'London System'
  } else if (startsWith(a,'1. d4 Nf6 2. c4 c5 3. d5 b5')){
    a <-'Benko Gambit'
  } else if (startsWith(a,'1. d4 Nf6 2. c4 e6 3. g3')){
    a <-'Catalan Opening'
  } else if (startsWith(a,'1. Nf3 d5 2. g3')){
    a <-'King\'s Indian Attack'
  } else if (startsWith(a,'1. c4')){
    a <-'English Opening'
  } else if (startsWith(a,'1. g3')){
    a <-'King\'s Fianchetto Opening'
  } else if (startsWith(a,'1. Nf3')){
    a <-'R�ti Opening'
  } else if (startsWith(a,'1. f4')){
    a <-'Bird\'s Opening'
  } else if (startsWith(a,'1. b3')){
    a <-'Nimzowitsch-Larsen Attack'
  } else if (startsWith(a,'1. b4')){
    a <-'Polish Opening'
  } else if (startsWith(a,'1. g4')){
    a <-'Grob Opening'
  } else  if (startsWith(a,'1. e4 c6')) {
    a <- 'Caro Kann Defense'
  } else if (startsWith(a,'1. e4 d5')) {
    a <-  'Scandinavian Defense'
  } else if (startsWith(a,'1. e4 c5')){
    a <-'Sicilian Defense'
  } else if (startsWith(a,'1. e4 e6')){
    a <-'French Defense'
  } else if (startsWith(a,'1. a3 a5 2. b4 e5')) {
    a <-"Bugayev Attack"
  } else if (startsWith(a,'1. a3 a5 2. b4')) {
    a <-"Anderssen\'s Opening Polish Gambit"
  } else if (startsWith(a,'1. a3 e5 2. h3 d5')) {
    a <-"Anderssenn\'s Opening, Creepy Crawly Formation"
  } else if (startsWith(a,'1. a3 g6 2. g4')) {
    a <-"Anderssen\'s Opening Andersspike"
  } else if (startsWith(a,'1. a4 e5 2. h4')) {
    a <-"Ware Opening Crab Variation"
  } else if (startsWith(a,'1. a4 e5 2. a5 d5 3. e3 f5 4. a6')) {
    a <-"Ware Opening Ware Gambit"
  } else if (startsWith(a,'1. a4 b5 2. axb5 Bb7')) {
    a <-"Ware Opening Wing Gambit"
  } else if (startsWith(a,'1. Na3 e5 2. Nc4 Nc6 3. e4 f5')) {
    a <-"Sodium Attack Durkin Gambit"
  } else if (startsWith(a,'1. b4 e5 2. Bb2 c5')) {
    a <-"Sokolsky Opening, Wolferts Gambit"
  } else if (startsWith(a,'1. b4 e5 2. a3')) {
    a <-"Sokolsky Opening, Bugayev Attack"
  } else if (startsWith(a,'1. b4 d5 2. Bb2 c6 3. a4')) {
    a <-"Sokolsky Opening, Myers Variation"
  } else if (startsWith(a,'1. b4 c6 2. Bb2 a5 3. b5 cxb5')) {
    a <-"Sokolsky Opening, Schuhler Gambit"
  } else if (startsWith(a,'1. b4 c6')) {
    a <-"Sokolsky Opening, Outflank Variation"
  } else if (startsWith(a,'1. b4 c5')) {
    a <-"Sokolsky Opening, Birmingham Gambit"
  } else if (startsWith(a,'1. f3 e5 2. Kf2')) {
    a <-"Barnes Opening, Hammerschlag Variation"
  } else if (startsWith(a,'1. g3 g5')) {
    a <-"Myers Defense"
  } else if (startsWith(a,'1. g4 g5 2. f4')) {
    a <-"Coca Cola Gambit"
  } else if (startsWith(a,'1. g4 f5')) {
    a <-"Alessi Gambit"
  } else if (startsWith(a,'1. b3 e5')) {
    a <-"Modern variation"
  } else if (startsWith(a,'1. b3 d5')) {
    a <-"Classical variation"
  } else if (startsWith(a,'1. b3 Nf6')) {
    a <-"Indian variation"
  } else if (startsWith(a,'1. b3 b6')) {
    a <-"Symmetrical variation"
  } else if (startsWith(a,'1. b3 c5')) {
    a <-"English variation"
  } else if (startsWith(a,'1. b3 b5')) {
    a <-"Polish variation"
  } else if (startsWith(a,'1. b3 f5')) {
    a <-"Dutch variation"
  } else if (startsWith(a,'1. h3')) {
    a <-"Clemenz Opening"
  } else if (startsWith(a,'1. h4')) {
    a <-"Desprez Opening"
  } else if (startsWith(a,'1. Nh3')) {
    a <-"Amar Opening"
  } else if (startsWith(a,'1. b3')) {
    a <-"Larsen \'s Opening"
  } else if (startsWith(a,'1. g4')) {
    a <-"Grob\'s Attack"
  } else if (startsWith(a,'1. g3')) {
    a <-"Benko\'s Opening"
  } else if (startsWith(a,'1. f3')) {
    a <-"Barnes Opening"
  } else if (startsWith(a,'1. d3')) {
    a <-"Mieses Opening"
  } else if (startsWith(a,'1. e3')) {
    a <-"Van 't Kruijs Opening"
  } else if (startsWith(a,' 1. Nc3')) {
    a <-"Van Geet Opening"
  } else if (startsWith(a,'1. c3')) {
    a <-"Saragossa Opening"
  } else if (startsWith(a,'1. b4')) {
    a <-"Sokolsky Opening"
  } else if (startsWith(a,'1. Na3')) {
    a <-"Durkin Opening"
  } else if (startsWith(a,'1. a4')) {
    a <-"Ware Opening"
  } else if (startsWith(a,'1. a3')) {
    a <-"Anderssen\'s Opening"
  } else {
    a <- 'Other'
  }
}

#Creating a new column with the specific type of opening
matches <- mutate(matches,Specific_opening=Movetext)
matches$Specific_opening <- unlist(lapply(matches$Specific_opening, fun2))

#Transforming the values in the movements into characters
fun3 <- function(a){
a <- as.character(a)
a <- ifelse(is.na(a),'',a)
}
matches$W1 <- unlist(lapply(matches$W1,fun3))
matches$W2 <- unlist(lapply(matches$W2,fun3))
matches$W3 <- unlist(lapply(matches$W3,fun3))
matches$W4 <- unlist(lapply(matches$W4,fun3))
matches$W5 <- unlist(lapply(matches$W5,fun3))
matches$W6 <- unlist(lapply(matches$W6,fun3))
matches$W7 <- unlist(lapply(matches$W7,fun3))
matches$W8 <- unlist(lapply(matches$W8,fun3))
matches$W9 <- unlist(lapply(matches$W9,fun3))
matches$W10 <- unlist(lapply(matches$W10,fun3))
matches$B1 <- unlist(lapply(matches$B1,fun3))
matches$B2 <- unlist(lapply(matches$B2,fun3))
matches$B3 <- unlist(lapply(matches$B3,fun3))
matches$B4 <- unlist(lapply(matches$B4,fun3))
matches$B5 <- unlist(lapply(matches$B5,fun3))
matches$B6 <- unlist(lapply(matches$B6,fun3))
matches$B7 <- unlist(lapply(matches$B7,fun3))
matches$B8 <- unlist(lapply(matches$B8,fun3))
matches$B9 <- unlist(lapply(matches$B9,fun3))
matches$B10 <- unlist(lapply(matches$B10,fun3))

#Creating a new column with my opening detai -first ten moves-
matches$My_opening_detail <- ifelse(matches$My_position=='White', paste(matches$W1, " ", matches$W2, " ", matches$W3, " ", matches$W4, " ", matches$W5, " ", matches$W6, " ", matches$W7, " ", matches$W8, " ", matches$W9, " ", matches$W10), paste(matches$B1, " ", matches$B2, " ", matches$B3, " ", matches$B4, " ", matches$B5, " ", matches$B6, " ", matches$B7, " ", matches$B8, " ", matches$B9, " ", matches$B10))
matches <- matches%>% deselect(c('W1','B1','Movetext','W2','B2','W3','B3','W4','B4','W5','B5','W6','B6','W7','B7','W8','B8','W9','B9','W10','B10', 'complete.movetext'))

#Renaming some columns
matches <- matches %>% 
  rename(
    last_move = last.move,
    Total_Movements = NMoves
  )

#READY!!
library(writexl)
write_xlsx(matches,"matches.xlsx")