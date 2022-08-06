/-  *minesweeper
/+  default-agent, dbug
|%
+$  versioned-state
  $%  state-0
  ==
+$  state-0  [%0 =settings =game-state gameboard=grid =leaderboard]
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
  `this(settings [5 5 7], game-state [0 %.n %.n], gameboard (generate-grid:hc 5 5 7))
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
  |=  [width=@ud height=@ud mines=@ud]
  =/  empty-grid  (new-grid width height)
  =/  mines-grid  (place-mines empty-grid mines)
  ::  =/  final-grid  (calc-neighbors mines-grid)
  mines-grid
::
++  place-mines
  |=  [newgrid=grid minetotal=@ud]
  ^-  grid
  =/  i  0
  =/  minecount  0
  |-
  ?:  =(minecount minetotal)
    newgrid
  %=  $
    i  +(i)
    newgrid  (roll-for-mine newgrid i minetotal)
    minecount  ?:  =(newgrid (roll-for-mine newgrid i minetotal))
                 minecount
               +(minecount)
  ==
::
++  roll-for-mine
  |=  [newgrid=grid i=@ud minetotal=@ud]
  ^-  grid
  =/  size  (lent newgrid)
  :: Using mod throws a hood error here?
  ::=/  pos  (mod size i) 
  =/  pos   |-
            ?:  (gth i size)
              $(i (sub i size))
            i
  =/  dice-roll  (~(rad og (add eny:bowl i)) 1.000)
  ?:  (lte dice-roll 1)
    (snap newgrid pos mine-tile)
  newgrid
::
::  ++  calc-neighbors
::    |=  newgrid=grid
::    =/  size  (lent newgrid)
::    =/  i  0
::    |-
::    ?:  =(i size)
::      newgrid
::    $(newgrid (my-neighbors newgrid i), i +(i))
::  ::
::  ++  my-neighbors
::    |=  [newgrid=grid i=@ud]
::    =~  0
::        ?:  =(i (mod size width)) :: haha, but is mod going to work here? or will it error out?
::    ==
::  
::    ?~  (snag newgrid (sub i 1))
::  
::
++  new-grid
  |=  [width=@ud height=@ud]
  =/  size  (mul width height)
  =/  count  0
  |-
  ^-  grid
  ?:  =(count size)
    ~
  :-  new-tile
  $(count +(count))
::
++  new-tile  [%.n %.n %.n 0]
++  mine-tile  ^-  tile  [%.n %.n %.y 0]
--