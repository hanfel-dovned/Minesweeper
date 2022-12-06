/-  *minesweeper, spaces-store, visas, membership
/+  default-agent, dbug, server, schooner
/*  minesweeper-ui  %html  /app/minesweeper-ui/html
|%
+$  versioned-state
  $%  state-0
  ==
+$  state-0  [%0 =settings =game-state gameboard=grid =leaderboard active-space=path]
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
  :_  %=  this
        settings  [10 10 10]
        game-state  [0 %.n %.n]
        gameboard  (generate-grid:hc [10 10 10])
        leaderboard  (malt ~[[our.bowl 0]])
        active-space  /
      ==
  :~
    :*  %pass  /eyre/connect  %arvo  %e 
        %connect  `/apps/minesweeper  %minesweeper
    ==
    :*  %pass  /spaces-updates  %agent
        [our.bowl %spaces]  %watch  /spaces
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
        ::  Send the HTML and set active space.
          [%apps %minesweeper ~]
        =/  urltape  (trip url.request.inbound-request)
        =/  query
          ^-  path
          |-
          ?~  urltape  ~
          ?:  =(-.urltape '=')
            (stab (crip +.urltape))
          $(urltape +.urltape)
        :_  state(active-space query)
        %-  send  
        [200 ~ [%html minesweeper-ui]]
        ::
        ::  The frontend will request this upon loading.
        ::  Send agent state and scores filtered by active space.
          [%apps %minesweeper %state ~]
        =/  scores
          ?:  =(active-space:state /(scot %p our.bowl)/our)
            leaderboard
          ::  =/  space-members  .^([%members members:membership] %gx /(scot %p our.bowl)/realm/spaces/(scot %p +2:active-space:state)/(scot %tas +6:active-space:state)/members/membership-view)
          ::  ~&  space-members
          ::  =/  member-map  convert space-members to a (map member ~)
          ::  (~(int by member-map) leaderboard)
          leaderboard
        :_  state
        %-  send
        [200 ~ [%json (enjs-state [settings game-state gameboard scores])]]
      ==
      ::
      ::  Note that the leaderboard in these POST
      ::  responses isn't filtered by space,
      ::  but that's okay, because the frontend
      ::  won't update its scores here.
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
            leaderboard
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
        =/  hiscore  (~(got by leaderboard) our.bowl)
        =.  leaderboard  
          ?.  victory  leaderboard
          ?:  (gth score hiscore)
            (~(put by leaderboard) our.bowl score)
          leaderboard
        =/  eyre-cards
          %-  send
          :+  200  ~
          :-  %json 
          %-  enjs-state
          :*  
              settings
              [(count-reveals newboard) victory loss]
              newboard
              leaderboard  ::  This isn't space-filtered either
          ==
        :_
          %=  state
            gameboard  newboard
            game-state  [(count-reveals newboard) victory loss]
          ==
        ?.  (gth score hiscore)
          eyre-cards
        %+  weld  
          :~  :*  %give  %fact  ~[/updates/out]  %minesweeper-update 
                  !>(`update`hiscore+score)
          ==  ==
        eyre-cards
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
            leaderboard  ::  Not space-filtered
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
            leaderboard=(map player=@p score=@ud)
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
      ::
      :-  %a
      %+  turn
        ~(tap by leaderboard)
      |=  [player=@p score=@ud]
      :-  %a
      :~
        (ship player)
        (numb score)
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
    ::
      [%updates %out ~]
    ::  Your own Minesweeper agent will lazily try
    ::  to subscribe to itself. We prevent that here.
    ?<  =(src.bowl our.bowl)
    :_  this
    :~  :*  %give  %fact  ~  %minesweeper-update
            !>(`update`hiscore+(~(got by leaderboard) our.bowl))
        ==
    ==
  ==
++  on-leave  on-leave:def
++  on-peek   on-peek:def
++  on-agent
  |=  [=wire =sign:agent:gall]
  ^-  (quip card _this)
  ?+    wire  (on-agent:def wire sign)
      [%spaces-updates ~]
    ?+    -.sign  (on-agent:def wire sign)
        %fact
      ?+    p.cage.sign  (on-agent:def wire sign)
          %spaces-reaction
        =/  reaction  !<(reaction:spaces-store q.cage.sign)
        ?+    -.reaction  (on-agent:def wire sign)
            %add
          =/  members  ~(tap by members:reaction)
          :-
            %+  snoc
              ^-  (list card)
              %+  turn  members
              |=  [k=@p v=*]
              :*
                %pass  /updates/in
                %agent  [k %minesweeper]
                %watch  /updates/out
              ==
            ^-  card
            :*
              %pass  /space/(scot %p ship:path:space:reaction)/(scot %tas space:path:space:reaction)
              %agent  [our.bowl %spaces]
              %watch  /spaces/(scot %p ship:path:space:reaction)/(scot %tas space:path:space:reaction)
            ==
          %=  this
            leaderboard
            |-
            ?~  members
              leaderboard
            %=  $
                leaderboard  %-  
                             ~(put by leaderboard)
                             [-<.members 0]
                members  +.members
            ==
          ==
        ==
      ==
    ==
    ::
      [%updates %in ~]
    ?+    -.sign  (on-agent:def wire sign)
        %fact
      ?+    p.cage.sign  (on-agent:def wire sign)
          %minesweeper-update
        =/  newupdate  !<(update q.cage.sign)
        `this(leaderboard (~(put by leaderboard) [src.bowl score:newupdate]))
      ==
    ==
    ::
      [%space @ @ ~]
    ?+    -.sign  (on-agent:def wire sign)
        %fact
      ?+    p.cage.sign  (on-agent:def wire sign)
          %visa-reaction
        =/  reaction  !<(reaction:visas q.cage.sign)
        ?+    -.reaction  `this
            %invite-sent
          :_  this(leaderboard (~(put by leaderboard) [ship:reaction 0]))
          :~  :*
            %pass  /updates/in
            %agent  [ship:reaction %minesweeper]
            %watch  /updates/out
          ==  ==
        ==
        ::
          %spaces-reaction
        =/  reaction  !<(reaction:spaces-store q.cage.sign)
        ?+    -.reaction  `this
            %remove
          :_  this
          ~[[%pass wire %agent [our.bowl %spaces] %leave ~]]
        ==
      ==
    ==
  ==
::
++  on-arvo   on-arvo:def
++  on-fail   on-fail:def
--
::
::  Why did I make this a door?
::  Couldn't it just be a core? 
::  Do I need the bowl anymore?
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