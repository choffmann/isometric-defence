gegner:
    - Position {x: , y: }
    - Hitpoints: int
    - geschwindigkeit: int
    - worth (währung)
    - damage (wenn er durchkommt)
    - (resistent gegen?)

Turm:
    - damage (an gegner)
    - reichweite (radius)
    - position
    - kosten
    - angriffsgeschwindigkeit
    - 
    - (schadensart?)
    - (upgradekosten?)


type alias tower = {
    damage: Int, attackRadius: Int, position: Point,
    price: Int, attackSpeed: Float (angriffe/s)
}



state:
    - leben
    - geld
    - [gegnern]
    - path
    - [türmen]
    - Maybe Turm

Running, Paused, Lost, Won