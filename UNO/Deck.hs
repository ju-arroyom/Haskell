module Deck (
    Deck(..),
    newDeck,
    draw
) where

import Card

newtype Deck = Deck [Card]
    deriving (Show, Eq)


{-
   This variable returns a deck with all 108 cards for Uno.

   See the image as an example: https://en.wikipedia.org/wiki/Uno_(card_game)#/media/File:UNO_cards_deck.svg

   You do not need to shuffle this deck. I will do that in my GUI code.

-}
newDeck :: Deck
newDeck =
  let block1 = [Card {color = colr, rank = num} | colr <- [Red, Green, Blue, Yellow], num <- [0..9]]
      block2 = [Card {color = colr, rank = num} | colr <- [Red, Green, Blue, Yellow], num <- [1..9]]
      block3 = [Skip {color = colr} | colr <- concat $ replicate 2 [Red, Green, Blue, Yellow]]
      block4 = [Reverse {color = colr} | colr <- concat $ replicate 2 [Red, Green, Blue, Yellow]]
      block5 = [Draw2 {color = colr} | colr <- concat $ replicate 2 [Red, Green, Blue, Yellow]]
      block6 = replicate 4 Wild ++ replicate 4 Draw4
  in Deck (block1 ++ block2 ++ block3 ++ block4 ++ block5 ++ block6)



{-
   This function takes the top most card from the deck and returns a new deck with that card removed
   and the card
 -}
draw :: Deck -> (Deck, Card)
draw (Deck lst) =
  let newdeck = tail lst
      topcard = head lst
  in (Deck newdeck, topcard)
