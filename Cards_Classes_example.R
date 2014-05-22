# Let's create a "class" that represents a pack
  # of cards form which we can draw random cards:
  
  PackOfCards <- function() {
    # A Card "class" within a PackOfCards "class"
    Card <- function(x) {
      # An "instance" of a Card is really just a list containing
      # an integer x (named rawInteger, that we don't actually use
      # in this example) as a "property" and a function 
      # that maps x to an interpretation of a card (named showCard)
      # as a "method"...
      list(rawInteger = x, showCard = function() {
        face <- c(
          "Ace", "Two", "Three", "Four", "Five", "Six", "Seven",
          "Eight", "Nine", "Ten", "Jack", "Queen", "King"
        )[x %% 13+  1]
        suit <- c(
          "Hearts", "Diamonds",
          "Clubs", "Spades"
        )[x %/% 13 %% 4 +  1]
        cat(sprintf("%s of %s\n", face, suit))
      })
    }
    
    # Let's keep track of which cards are still in the pack.
    cardInPack <- rep(1, 52)
    
    # A PackOfCards "instance" is then simply a list containing
    # several "methods".
    list(
      cardsRemaining = function() sum(cardInPack), 
      drawCard = function() {
        if (sum(cardInPack) == 0) message("No more cards in the pack!")
        else {
          c <- numeric(1)
          repeat {
            c <- sample(1:52, 1)
            if (cardInPack[c] == 1) break
          }
          cardInPack[c] <<- 0
          # Create an "instance" of a Card and call its showCard "method"
          card = Card(c)
          card$showCard()
        } 
      }, 
      showRemainingCards = function() for (i in 1:52) if (cardInPack[i]) Card(i)$showCard()
    )
  }
  
  
# pack1 <- PackOfCards()
# pack1$cardsRemaining()
#[1] 52
# 
# # Let's draw 40 cards from the deck
# for (i in 1:40) pack1$drawCard()
#pack1$showRemainingCards()
