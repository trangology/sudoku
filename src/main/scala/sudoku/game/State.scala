package sudoku.game

sealed trait State

case object GET_STARTED extends State
case object UNSOLVED extends State
case object UNSOLVABLE extends State
case object SOLVED extends State
