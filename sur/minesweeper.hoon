|%
+$  settings  [width=@ud height=@ud mines=@ud]
+$  game-state  [reveals=@ud win=? lose=?]
+$  tile  [revealed=? flagged=? mine=? neighbors=@ud]
+$  grid  (list tile)
+$  leaderboard  (map member=@p score=@ud)
+$  action
  $%  [%new-game width=@ud height=@ud mines=@ud]
      [%guess x=@ud y=@ud]
      [%flag x=@ud y=@ud]
  ==
+$  update
  $%  [%hiscore score=@ud]
  ==
--