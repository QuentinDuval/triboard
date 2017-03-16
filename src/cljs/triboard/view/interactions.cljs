(ns triboard.view.interactions)


(defprotocol IUserInteractions
  "A protocol for the interactions that can be triggered from the GUI"
  (on-new-game [this] "Send a new game command")
  (on-toogle-help [this] "Send a toggle help command")
  (on-restart [this] "Send a restart command")
  (on-undo [this] "Send an undo command")
  (on-player-move [this x y] "Send a command to player at [x y]"))
