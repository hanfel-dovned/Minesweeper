/-  *minesweeper
/+  default-agent, dbug, server, schooner
/*  minesweeper-ui  %html  /app/minesweeper-ui/html
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
  :_  this(settings [5 5 7], game-state [0 %.n %.n], gameboard (generate-grid:hc 5 5 7))
  :~
    :*  %pass  /eyre/connect  %arvo  %e 
        %connect  `/apps/minesweeper  %minesweeper
    ==  
  ==
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
++  on-poke
  |=  [=mark =vase]
  ^-  (quip card _this)
  |^
  ?>  =(src.bowl our.bowl)
  ?+    mark  (on-poke:def mark vase)
      %handle-http-request
    =^  cards  state
      (handle-http !<([@ta =inbound-request:eyre] vase))
    [cards this]
  ==
  ::
  ++  handle-http
    |=  [eyre-id=@ta =inbound-request:eyre]
    ^-  (quip card _state)
    =/  ,request-line:server
      (parse-request-line:server url.request.inbound-request)
    =+  send=(cury response:schooner eyre-id)
    ?.  authenticated.inbound-request
      :_  state
      %-  send
      [302 ~ [%login-redirect './apps/minesweeper']]
    ::
    ?+    method.request.inbound-request  
      [(send [405 ~ [%stock ~]]) state]
      ::
      ::   %'POST'
      :: ?~  body.request.inbound-request
      ::   [(send [405 ~ [%stock ~]]) state]
      :: =/  json  (de-json:html q.u.body.request.inbound-request)
      :: =/  action  (dejs-action +.json) 
      :: (handle-action action) 
      :: 
        %'GET'
      ?+  site  :_  state 
                %-  send
                :+  404
                  ~ 
                [%plain "404 - Not Found"] 
          [%apps %minesweeper ~]
        :_  state
        %-  send  
        :+  200
          ~
        [%html minesweeper-ui]  
        ::
          [%apps %minesweeper %state ~]
        :_  state
        %-  send
        :+  200   
          ~ 
        [%json (enjs-state [settings game-state gameboard])]
      ==
    ==
  ::              
  ++  enjs-state
    =,  enjs:format
    |=  $:  
            settings=[width=@ud height=@ud mines=@ud]
            game-state=[reveals=@ud win=? lose=?]
            grid=(list [revealed=? flagged=? mine=? neighbors=@ud])
        ==
    ^-  json
    :-  %a
    :~ 
      (numb width:settings)
      (numb height:settings)
      (numb mines:settings)
      (numb reveals:game-state)
      [%b win:game-state]
      [%b lose:game-state]
      :-  %a
      %+  turn
        grid
      |=  tile=[revealed=? flagged=? mine=? neighbors=@ud]
      :-  %a
      :~
          [%b revealed:tile]
          [%b flagged:tile]
          [%b mine:tile]
          (numb neighbors:tile)
      ==
    ==
  ::
  ++  handle-action
    |=  act=action
    ^-  (quip card _state)
    ?-    -.act
        %new-game
      :-  ~
      %=  state
        settings  +.act
        game-state  [0 %.n %.n]
        gameboard  (generate-grid:hc +.act)
      ==
      ::
        %guess
      =/  pos  (add (mul y:act width:settings) x:act)
      =/  tile  (snag pos gameboard)
      ?:  mine:tile
        `state(game-state [reveals:game-state %.n %.y])
      =/  newtile  ^+  tile
        [%.y flagged:tile mine:tile neighbors:tile]
      =/  victory
        ?:  .=  (add +(reveals:game-state) mines:settings) 
            (mul width:settings height:settings)
          %.y
        %.n
      :-  ~
      %=  state
        gameboard  (snap gameboard pos newtile)
        game-state  [+(reveals:game-state) victory %.n]
      ==
      ::
        %flag
      =/  pos  (add (mul y:act width:settings) x:act)
      =/  tile  (snag pos gameboard)
      =/  newtile  ^+  tile
        ?.  flagged:tile
           [revealed:tile %.y mine:tile neighbors:tile]
         [revealed:tile %.n mine:tile neighbors:tile]
      `state(gameboard (snap gameboard pos newtile))
    ==
  --
::
++  on-watch
  |=  =path
  ^-  (quip card _this)
  ?+    path  (on-watch:def path)
      [%http-response *]
    `this
  ==
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
  =/  final-grid  (calc-neighbors mines-grid width)
  final-grid
::
++  place-mines
  |=  [=grid minetotal=@ud]
  ^+  grid
  =/  i  0
  =/  minecount  0
  |-
  ?:  =(minecount minetotal)
    grid
  %=  $
    i  +(i)
    grid  (roll-for-mine grid i)
    minecount  ?:  =(grid (roll-for-mine grid i))
                 minecount
               +(minecount)
  ==
::
++  roll-for-mine
  |=  [=grid i=@ud]
  ^+  grid
  =/  size  (lent grid)
  =/  pos  (mymod i size)
  =/  dice-roll  (~(rad og (add eny:bowl i)) size)
  ?:  (lte dice-roll 1)
    (snap grid pos mine-tile)
  grid
::
++  calc-neighbors
  |=  [=grid width=@ud]
  ^+  grid
  =/  size  (lent grid)
  =/  i  0
  |-
  ?:  =(i size)
    grid
  %=  $
    i  +(i)
    grid  (tile-neighbors grid i width)
  ==
::
++  tile-neighbors
  |=  [=grid i=@ud width=@ud]
  ^+  grid
  =/  n  0
  =.  n  (add n (nbr-left grid i width))
  =.  n  (add n (nbr-right grid i width))
  =.  n  (add n (nbr-up grid i width))
  =.  n  (add n (nbr-down grid i width))
  =.  n  (add n (nbr-up-left grid i width))
  =.  n  (add n (nbr-up-right grid i width))
  =.  n  (add n (nbr-down-left grid i width))
  =.  n  (add n (nbr-down-right grid i width))
  =/  tile  (snag i grid)
  (snap grid i [revealed:tile flagged:tile mine:tile n])
::
++  nbr-left
  |=  [=grid i=@ud width=@ud]
  ?:  =(0 (mymod i width))
    0
  ?:  =(%.y mine:(snag (sub i 1) grid))
    1
  0
::
++  nbr-right
  |=  [=grid i=@ud width=@ud]
  ?:  =((sub width 1) (mymod i width))
    0
  ?:  =(%.y mine:(snag (add i 1) grid))
    1
  0
::
++  nbr-up
  |=  [=grid i=@ud width=@ud]
  ?:  (lth i width)
    0
  ?:  =(%.y mine:(snag (sub i width) grid))
    1
  0
::
++  nbr-down
  |=  [=grid i=@ud width=@ud]
  ?:  (gte i (sub (lent grid) width))
    0
  ?:  =(%.y mine:(snag (add i width) grid))
    1
  0
::
++  nbr-up-left
  |=  [=grid i=@ud width=@ud]
  ?:  (lth i width)
    0
  ?:  =(0 (mymod i width))
    0
  ?:  =(%.y mine:(snag (sub (sub i width) 1) grid))
    1
  0
::
++  nbr-down-left
  |=  [=grid i=@ud width=@ud]
  ?:  (gte i (sub (lent grid) width))
    0
  ?:  =(0 (mymod i width))
    0
  ?:  =(%.y mine:(snag (sub (add i width) 1) grid))
    1
  0
::
++  nbr-down-right
  |=  [=grid i=@ud width=@ud]
  ?:  (gte i (sub (lent grid) width))
    0
  ?:  =((sub width 1) (mymod i width))
    0
  ?:  =(%.y mine:(snag (add (add i width) 1) grid))
    1
  0
::
++  nbr-up-right
  |=  [=grid i=@ud width=@ud]
  ?:  (lth i width)
    0
  ?:  =((sub width 1) (mymod i width))
    0
  ?:  =(%.y mine:(snag (add (sub i width) 1) grid))
    1
  0
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
::  mod from stdlib is erroring
++  mymod
  |=  [x=@ud y=@ud]  
  |-
  ?:  (gte x y)
    $(x (sub x y))
  x
::
++  new-tile  [%.n %.n %.n 0]
++  mine-tile  ^-  tile  [%.n %.n %.y 0]
--