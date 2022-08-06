/-  *minesweeper
/+  default-agent, dbug
|%
+$  versioned-state
  $%  state-0
  ==
+$  state-0  [%0 =settings =game-state =grid =leaderboard]
+$  card  card:agent:gall
-- 
%-  agent:dbug
=|  state-0
=*  state  -
^-  agent:gall
=<
|_  =bowl:gall
+*  this      .
    def   ~(. (default-agent this %.n) bowl)
    hc    ~(. +> bowl)
++  on-init
  ^-  (quip card _this)
  `this(settings [5 5 7], game-state [0 %.n %.n], grid (generate-grid:hc 5 5 10))
::
++  on-save
  ^-  vase
  !>(state)
::
++  on-load
|=  old-state=vase
^-  (quip card _this)
=/  old  !<(versioned-state old-state)
?-  -.old
%0  `this(state old)
==
::
++  on-poke   on-poke:def
++  on-watch  on-watch:def
++  on-leave  on-leave:def
++  on-peek   on-peek:def
++  on-agent  on-agent:def
++  on-arvo   on-arvo:def
++  on-fail   on-fail:def
--
::
|_  =bowl:gall
++  generate-grid
  ~&  our:bowl
  |=  [width=@ud height=@ud mines=@ud]
  =/  empty-grid  (new-grid width height mines)
  =/  mines-grid  (place-mines empty-grid mines)
  =/  final-grid  (calc-neighbors mines-grid)
  mines-grid
::
++  place-mines
  |=  [empty-grid=(list (list tile)) mines=@ud]
  empty-grid
::
++  calc-neighbors
  |=  mines-grid=(list (list tile))
  mines-grid
::
++  new-grid
  |=  [width=@ud height=@ud mines=@ud]
  =/  y  0
  |-
  ^-  (list (list tile))
  ?:  =(height y)
    ~
  :-  (new-row width mines)
  $(y +(y))
::
++  new-row
  |=  [width=@ud mines=@ud]
  =/  x  0
  |-
  ^-  (list tile)
  ?:  =(width x)
    ~
  :-  (new-tile mines)
  $(x +(x))
::
++  new-tile  
  |=  mines=@ud
  ^-  tile
  :*  %.n  %.n
    :: Just doing probability for now
    :: mines = percent chance of any tile being a mine
    =/  dice-roll  (~(rad og eny:bowl) 100)
    ~&  dice-roll
    ?:  (lte dice-roll mines)
      %.y
    %.n
  0
  ==
--