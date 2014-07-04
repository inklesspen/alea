;;;; tarot.lisp

(in-package #:alea)

(defparameter *tarot-cards*
  #(
    "The Fool" "The Magician" "The High Priestess" "The Empress" "The Emperor" "The Hierophant" "The Lovers" "The Chariot" "Justice" "The Hermit" "Wheel of Fortune" "Strength" "The Hanged Man" "Death" "Temperance" "The Devil" "The Tower" "The Star" "The Moon" "The Sun" "Judgement" "The World"
    "Ace of Pentacles" "Two of Pentacles" "Three of Pentacles" "Four of Pentacles" "Five of Pentacles" "Six of Pentacles" "Seven of Pentacles" "Eight of Pentacles" "Nine of Pentacles" "Ten of Pentacles" "Page of Pentacles" "Knight of Pentacles" "Queen of Pentacles" "King of Pentacles"
    "Ace of Wands" "Two of Wands" "Three of Wands" "Four of Wands" "Five of Wands" "Six of Wands" "Seven of Wands" "Eight of Wands" "Nine of Wands" "Ten of Wands" "Page of Wands" "Knight of Wands" "Queen of Wands" "King of Wands"
    "Ace of Cups" "Two of Cups" "Three of Cups" "Four of Cups" "Five of Cups" "Six of Cups" "Seven of Cups" "Eight of Cups" "Nine of Cups" "Ten of Cups" "Page of Cups" "Knight of Cups" "Queen of Cups" "King of Cups"
    "Ace of Swords" "Two of Swords" "Three of Swords" "Four of Swords" "Five of Swords" "Six of Swords" "Seven of Swords" "Eight of Swords" "Nine of Swords" "Ten of Swords" "Page of Swords" "Knight of Swords" "Queen of Swords" "King of Swords"
))

(defparameter *chargen-sephirot*
  (list "Keter: the big concept" "Chokhmah: outward expression and mode of acting" "Binah: how the world acts upon this character" "Chesed: stable or predictably good things" "Geburah: something predictably bad or instability which drives to quests" "Tiphareth: unify the previous cards to sum up inner face and essential nature" "Netzach: emotions and interpersonal relationships" "Hod: intellect and communication" "Yesod: unify the previous two cards to sum up outer face" "Malkuth: wrap up, conclusion, ultimate end"))
