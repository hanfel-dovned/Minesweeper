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
  :_  this(settings [5 5 7], game-state [0 %.n %.n], gameboard (generate-grid:hc 5 5 7), leaderboard 0)
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
      ::
        %'POST'
      ?~  body.request.inbound-request
        [(send [405 ~ [%stock ~]]) state]
      =/  json  (de-json:html q.u.body.request.inbound-request)
      =/  act  (dejs-action +.json) 
      ?-    -.act
          %new-game
        =/  newboard  (generate-grid:hc +.act)
        :_
          %=  state
            settings  +.act
            game-state  [0 %.n %.n]
            gameboard  newboard
          ==
        %-  send
        :+  200  ~
        :-  %json 
        %-  enjs-state
        :*  
            +.act
            [0 %.n %.n]
            newboard
        ==
        ::
          %guess
        =/  pos  (add (mul y:act width:settings) x:act)
        =/  tile  (snag pos gameboard)
        ?:  revealed:tile  `state
        ?:  lose:game-state  `state
        ?:  win:game-state  `state
        =/  newboard  (reveal-neighbors gameboard pos width:settings)
        =/  loss  ?:(mine:tile %.y %.n)
        =/  victory
          ?:  loss  %.n
          ?:  .=  (add (count-reveals newboard) mines:settings) 
              (mul width:settings height:settings)
            %.y
          %.n
        =/  score  :(mul mines:settings width:settings height:settings)
        =/  newleaderboard  
          ?.  victory  leaderboard
          ?:  (gth score leaderboard)
            score
          leaderboard
        :_
          %=  state
            gameboard  newboard
            game-state  [(count-reveals newboard) victory loss]
            leaderboard  newleaderboard
          ==
        %-  send
        :+  200  ~
        :-  %json 
        %-  enjs-state
        :*  
            settings
            [(count-reveals newboard) victory loss]
            newboard
        ==
        ::
          %flag
        =/  pos  (add (mul y:act width:settings) x:act)
        =/  tile  (snag pos gameboard)
        =/  newtile  ^+  tile
          ?.  flagged:tile
            [revealed:tile %.y mine:tile neighbors:tile]
          [revealed:tile %.n mine:tile neighbors:tile]
        :_  state(gameboard (snap gameboard pos newtile))
        %-  send
        :+  200  ~
        :-  %json 
        %-  enjs-state
        :*  
            settings
            game-state
            (snap gameboard pos newtile)
        ==
      ==
    ==
  ::
  ++  dejs-action
    =,  dejs:format
    |=  jon=json
    ^-  action
    %.  jon
    %-  of
    :~  new-game+(at ~[ni ni ni])
        guess+(at ~[ni ni])
        flag+(at ~[ni ni])
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
  =/  final-grid  (fill-neighbors mines-grid width)
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
    grid  (roll-for-mine grid i minecount minetotal)
    minecount  ?:  =(grid (roll-for-mine grid i minecount minetotal))
                 minecount
               +(minecount)
  ==
::
++  roll-for-mine
  |=  [=grid i=@ud minecount=@ud minetotal=@ud]
  ^+  grid
  =/  size  (lent grid)
  =/  pos  (mymod i size)
  =/  dice-roll  (~(rad og (add eny:bowl i)) (sub size i))
  ?:  (lte dice-roll (sub minetotal minecount))
    (snap grid pos mine-tile)
  grid
::
::  Fill in the grid's neighbor values. 
++  fill-neighbors
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
::  Fill in one tile's neighbor values.
++  tile-neighbors
  |=  [thegrid=grid pos=@ud w=@ud]
  ^-  grid
  =/  n  8
  =/  nbr  neighbor  =.  nbr  nbr(grid thegrid, i pos, width w)
  =.  n  ?~  (left:nbr)  (sub n 1)  (sub n mine:u:+:(left:nbr))
  =.  n  ?~  (right:nbr)  (sub n 1)  (sub n mine:u:+:(right:nbr))
  =.  n  ?~  (up:nbr)  (sub n 1)  (sub n mine:u:+:(up:nbr))
  =.  n  ?~  (down:nbr)  (sub n 1)  (sub n mine:u:+:(down:nbr))
  =.  n  ?~  (upleft:nbr)  (sub n 1)  (sub n mine:u:+:(upleft:nbr))
  =.  n  ?~  (upright:nbr)  (sub n 1)  (sub n mine:u:+:(upright:nbr))
  =.  n  ?~  (downleft:nbr)  (sub n 1)  (sub n mine:u:+:(downleft:nbr))
  =.  n  ?~  (downright:nbr)  (sub n 1)  (sub n mine:u:+:(downright:nbr))
  =/  tile  (snag pos thegrid)
  (snap thegrid pos [revealed:tile flagged:tile mine:tile n])
::
::  Count revealed tiles.
++  count-reveals
  |=  =grid
  ^-  @ud
  =/  count  0
  =/  i  0
  |-
  ?:  =(i (lent grid))
    count
  %=  $
    i  +(i)
    count  ?:  revealed:(snag i grid)
             +(count)
           count
  ==
::  Recursively reveal neighboring tiles that aren't touching any mines.
++  reveal-neighbors
  |=  [thegrid=grid pos=@ud w=@ud]
  ^-  grid
  =/  tile  (snag pos thegrid)
  =/  newtile  ^+  tile
    [%.y flagged:tile mine:tile neighbors:tile]
  =.  thegrid  (snap thegrid pos newtile)
  ?:  (gth neighbors:(snag pos thegrid) 0)  
    thegrid
  =/  nbr  neighbor  =.  nbr  nbr(grid thegrid, i pos, width w)
  =.  thegrid  ?~  (left:nbr)  thegrid
    ?:  revealed:u:+:(left:nbr)  thegrid
    (reveal-neighbors thegrid (sub pos 1) w)
  =.  thegrid  ?~  (right:nbr)  thegrid
    ?:  revealed:u:+:(right:nbr)  thegrid
    (reveal-neighbors thegrid (add pos 1) w)
  =.  thegrid  ?~  (up:nbr)  thegrid
    ?:  revealed:u:+:(up:nbr)  thegrid
    (reveal-neighbors thegrid (sub pos w) w)
  =.  thegrid  ?~  (down:nbr)  thegrid
    ?:  revealed:u:+:(down:nbr)  thegrid
    (reveal-neighbors thegrid (add pos w) w)
  =.  thegrid  ?~  (upleft:nbr)  thegrid
    ?:  revealed:u:+:(upleft:nbr)  thegrid
    (reveal-neighbors thegrid (sub (sub pos w) 1) w)
  =.  thegrid  ?~  (upright:nbr)  thegrid
    ?:  revealed:u:+:(upright:nbr)  thegrid
    (reveal-neighbors thegrid (add (sub pos w) 1) w)
  =.  thegrid  ?~  (downleft:nbr)  thegrid
    ?:  revealed:u:+:(downleft:nbr)  thegrid
    (reveal-neighbors thegrid (sub (add pos w) 1) w)
  =.  thegrid  ?~  (downright:nbr)  thegrid
    ?:  revealed:u:+:(downright:nbr)  thegrid
    (reveal-neighbors thegrid (add (add pos w) 1) w)
  thegrid
::
++  neighbor
  |_  [=grid i=@ud width=@ud]
  ++  left
    |=  @  ^-  (unit tile)
    ?:  =(0 (mymod i width))  ~
    [~ (snag (sub i 1) grid)]
  ::
  ++  right
    |=  @  ^-  (unit tile)
    ?:  =((sub width 1) (mymod i width))  ~
    [~ (snag (add i 1) grid)]
  ::
  ++  up
    |=  @  ^-  (unit tile)
    ?:  (lth i width)  ~
    [~ (snag (sub i width) grid)]
  ::
  ++  down
    |=  @  ^-  (unit tile)
    ?:  (gte i (sub (lent grid) width))  ~
    [~ (snag (add i width) grid)]
  ::
  ++  upleft
    |=  @  ^-  (unit tile)
    ?:  (lth i width)  ~
    ?:  =(0 (mymod i width))  ~
    [~ (snag (sub (sub i width) 1) grid)]
  ::
  ++  downleft
    |=  @  ^-  (unit tile)
    ?:  (gte i (sub (lent grid) width))  ~
    ?:  =(0 (mymod i width))  ~
    [~ (snag (sub (add i width) 1) grid)]
  ::
  ++  downright
    |=  @  ^-  (unit tile)
    ?:  (gte i (sub (lent grid) width))  ~
    ?:  =((sub width 1) (mymod i width))  ~
    [~ (snag (add (add i width) 1) grid)]
  ::
  ++  upright
    |=  @  ^-  (unit tile)
    ?:  (lth i width)  ~
    ?:  =((sub width 1) (mymod i width))  ~
    [~ (snag (add (sub i width) 1) grid)]
  --
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