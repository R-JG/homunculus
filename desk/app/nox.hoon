/-  homunculus
|%
+$  nav
  $:  des=$~(%base desk)
      pax=path
  ==
+$  history-tree
  $~  [~. ~ ~]
  $:  name=knot
      recent=(unit knot)
      branches=(list history-tree)
  ==
+$  history  (map desk history-tree)
+$  col  [prev=path rows=(list knot)]
+$  cols  [lef=col cen=col rig=col fil=path]
+$  file  (unit cord)
+$  state
  $:  =nav
      =history
  ==
::
+$  card  card:agent:gall
--
::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  
=|  state
=*  state  -
^-  agent:gall
=<
|_  bol=bowl:gall
+*  this  .
++  on-init
  ^-  (quip card _this)
  :_  this(state *^state)
  :~  (~(render tui [nav bol]) [(get-sel nav) hotkeys])
  ==
++  on-save
  ^-  vase
  !>(~)
++  on-load
  |=  old=vase
  ^-  (quip card _this)
  :_  this
  :~  (~(render tui [nav bol]) [(get-sel nav) hotkeys])
  ==
::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  
++  on-poke
  |=  [=mark =vase]
  |^  ^-  (quip card _this)
  ?>  =(our.bol src.bol)
  ?+  mark  !!
    ::
      %open
    =.  history  ~
    :_  this
    :~  (~(render tui [nav bol]) [(get-sel nav) hotkeys])
    ==
    ::
      %homunculus-event
    =/  eve  !<(event:homunculus vase)
    ?+  -.eve  !!
      ::
        %select
      =/  pav=path  (stab id.eve)
      ?~  pav  !!
      ?+  i.pav  !!
          %row  (do-row-select t.pav)
      ==
      ::
        %hotkey
      ?+  ^-(@tas id.eve)  !!
        %nav-left   do-nav-left
        %nav-right  do-nav-right
      ==
      ::
    ==
    ::
  ==
  ::
  ++  do-row-select
    |=  pav=path
    ^-  (quip card _this)
    ?~  pav  !!
    ?~  pax.nav
      =.  des.nav  i.pav
      :_  this
      :~  (~(render tui [nav bol]) [~ hotkeys])
      ==
    =/  his=(unit history-tree)  (~(get by history) des.nav)
    =:  pax.nav  pav
        history
          %+  %~  put  by  history
            des.nav
          (put-history-tree (rear pav) [des.nav (snip `path`pav)] his)
      ==
    :_  this
    :~  (~(render tui [nav bol]) [~ hotkeys])
    ==
  ::
  ++  do-nav-left
    ^-  (quip card _this)
    =.  pax.nav  (snip pax.nav)
    :_  this
    :~  (~(render tui [nav bol]) [(get-sel nav) hotkeys])
    ==
  ::
  ++  do-nav-right
    ^-  (quip card _this)
    =/  his=(unit history-tree)  (~(get by history) des.nav)
    =/  nex=(unit knot)
      ?~  his  ~
      (read-history-tree nav u.his)
    =/  scy=cols  (do-scry nav bol)
    =.  state
      ?~  nex
        ?:  =(~ pax.nav)
          ?~  rows.cen.scy
            state
          %_  state
            pax.nav  ^-(path [i.rows.cen.scy ~])
            history
              %+  %~  put  by  history
                des.nav
              (put-history-tree i.rows.cen.scy nav his)
          ==
        ?~  rows.rig.scy
          state
        %_  state
          pax.nav  (snoc pax.nav i.rows.rig.scy)
          history
            %+  %~  put  by  history
              des.nav
            (put-history-tree i.rows.rig.scy nav his)
        ==
      %_  state
        pax.nav  (snoc pax.nav u.nex)
        history
          %+  %~  put  by  history
            des.nav
          (put-history-tree u.nex nav his)
      ==
    ?^  fil.scy
      :_  this
      :~  (~(render tui [nav bol]) [[~ (spat [%file pax.nav])] hotkeys])
      ==
    :_  this
    :~  (~(render tui [nav bol]) [(get-sel nav) hotkeys])
    ==
  ::
  --
::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  
++  on-watch  |=(path ^-((quip card _this) !!))
++  on-leave  |=(path ^-((quip card _this) !!))
++  on-peek   |=(path ^-((unit (unit cage)) !!))
++  on-agent  |=([wire sign:agent:gall] ^-((quip card _this) !!))
++  on-arvo   |=([=wire sign=sign-arvo] ^-((quip card _this) !!))
++  on-fail   |=([term tang] ^-((quip card _this) !!))
--
::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  
|%
::
++  hotkeys
  ^-  hotkeys:homunculus
  :~  [[%arrow %l] 'nav-left']
      [[%arrow %r] 'nav-right']
  ==
::
++  get-sel
  |=  ^nav
  ^-  select-default:homunculus
  :-  ~
  %-  spat
  :-  %row
  ?~  pax  [des ~]
  pax
::
++  read-history-tree
  |=  [nav=path his=history-tree]
  ^-  (unit knot)
  ?~  nav  recent.his
  ?.  =(i.nav name.his)  ~
  =/  nav  t.nav
  |-  ^-  (unit knot)
  ?~  nav  recent.his
  ?~  branches.his  ~
  ?:  =(i.nav name.i.branches.his)
    $(nav t.nav, his i.branches.his)
  $(branches.his t.branches.his)
::
++  put-history-tree
  |=  [new=term nav=path his=(unit history-tree)]
  ^-  history-tree
  ?~  his
    ?~  nav
      [new ~ ~]
    [i.nav ?^(t.nav [~ i.t.nav] ~) [$(nav t.nav) ~]]
  ?~  nav  u.his
  %_  u.his
    recent  ?~(t.nav [~ new] recent.u.his)
    branches
      |-  ^-  (list history-tree)
      ?~  branches.u.his
        [^$(nav t.nav, his ~) ~]
      ?~  t.nav
        ?:  =(new name.i.branches.u.his)
          branches.u.his
        :-  i.branches.u.his
        $(branches.u.his t.branches.u.his)
      ?.  =(i.t.nav name.i.branches.u.his)
        :-  i.branches.u.his
        $(branches.u.his t.branches.u.his)
      :-  ^$(nav t.nav, his [~ i.branches.u.his])
      $(branches.u.his t.branches.u.his)
  ==
::
++  do-scry
  |=  [^nav bol=bowl:gall]
  ^-  cols
  =/  lef=col
    =/  xap=path
      ?:  |(?=(~ pax) ?=(~ t.pax))  ~
      (snip (snip ^-(path pax)))
    :-  xap
    %+  sort
      %~  tap  in
      ?:  |(?=(~ pax) ?=(~ t.pax))
        .^((set desk) %cd (en-beam [[our.bol %$ [%da now.bol]] ~]))
      %~  key  by
      =<  dir
      .^  arch  %cy
          %-  en-beam
          :-  [our.bol des [%da now.bol]]
          xap
      ==
    aor
  =/  cen=col
    =/  xap=path
      ?:  |(?=(~ pax) ?=(~ t.pax))  ~
      (snip ^-(path pax))
    :-  xap
    %+  sort
      %~  tap  in
      %~  key  by
      =<  dir
      .^  arch  %cy
          %-  en-beam
          :-  [our.bol des [%da now.bol]]
          xap
      ==
    aor
  =/  [rig=col fil=path]
    =/  xap=path
      ?~  pax
        =/  his=(unit history-tree)  (~(get by history) des)
        =/  rea=(unit knot)  ?~(his ~ (read-history-tree ~ u.his))
        ?^  rea  (snoc prev.cen u.rea)
        ?~  rows.cen  !!
        (snoc prev.cen i.rows.cen)
      pax
    =/  scy=arch
      .^  arch  %cy
          %-  en-beam
          :-  [our.bol des [%da now.bol]]
          xap
      ==
    ?^  fil.scy
      [[xap ~] xap]
    :_  ~
    :-  xap
    %+  sort
      %~  tap  in
      %~  key  by
      dir.scy
    aor
  [lef cen rig fil]
::
++  fi-scry
  |=  [fil=path ^nav bol=bowl:gall]
  ^-  file
  %-  mole
  |.
  !<  cord
  .^(vase %cr (en-beam [[our.bol des [%da now.bol]] fil]))
::
++  tui
  |_  [=^nav bol=bowl:gall]
  ::
  ++  render
    |=  [met=metadata:homunculus]
    ^-  card
    :*  %pass  /homunculus  %agent  [our.bol %ulus]  :: [our.bol %homunculus]
        %poke  %homunculus-session  !>([met root])
    ==
  ::
  ++  root
    ^-  manx
    ;box(w "100%", h "100%", px "5%", pt "1", pb "5%", cb cb-1, cf cf-1)
      ;box(w "100%", h "1", fl "row")
        ;txt: {(scow %p our.bol)}
        ;txt: {(spud [des.nav pax.nav])}
      ==
      ;+  columns
    ==
  ::
  ++  columns
    ^-  manx
    =/  =cols  (do-scry nav bol)
    =/  =file  ?~(fil.cols ~ (fi-scry fil.cols nav bol))
    ;box(w "100%", h "grow", b "arc", fl "row")
      ;+  (column-left lef.cols ?=(^ file))
      ;line-v;
      ;+  (column-center cen.cols ?=(^ file))
      ;line-v;
      ;+  (column-right rig.cols file)
    ==
  ::
  ++  column-left
    |=  [=col collapse=?]
    ^-  manx
    =/  def-cf=tape  ?~(pax.nav cf-1 cf-2)
    ;scroll(w ?:(collapse "10%" "30%"), h "100%", cf def-cf)
        =id  ?:(|(?=(~ pax.nav) ?=(~ t.pax.nav)) "desks" (spud [des.nav prev.col]))
      ;*  ?~  pax.nav  (rows-main col)
          ^-  marl
          %+  turn  rows.col
          |=  cod=cord
          ^-  manx
          =/  light=?
            ?|  &(?=(~ t.pax.nav) =(cod des.nav))
                &(?=(^ t.pax.nav) =(cod (rear (snip ^-(path pax.nav)))))
            ==
          ;box(w "100%", h "1", cb ?:(light cf-2 cb-1), cf ?:(light cb-1 def-cf))
            ;+  ;/  (trip cod)
          ==
    ==
  ::
  ++  column-center
    |=  [=col collapse=?]
    ^-  manx
    =/  his=(unit history-tree)  (~(get by history) des.nav)
    =/  nex=(unit knot)  ?~(his ~ (read-history-tree nav u.his))
    =/  def-cf=tape  ?^(pax.nav cf-1 cf-2)
    ;scroll(w ?:(collapse "10%" "40%"), h "100%", cf def-cf)
        =id  (spud [des.nav prev.col])
      ;*  ?^  pax.nav  (rows-main col)
          ^-  marl
          %+  spun  rows.col
          |=  [cod=cord i=@]
          ^-  [manx @]
          =/  light=?  ?^(nex =(cod u.nex) =(0 i))
          :_  +(i)
          ;box(w "100%", h "1", cb ?:(light cf-2 cb-1), cf ?:(light cb-1 def-cf))
            ;+  ;/  (trip cod)
          ==
    ==
  ::
  ++  column-right
    |=  [=col =file]
    ^-  manx
    =/  his=(unit history-tree)  (~(get by history) des.nav)
    =/  nex=(unit knot)  ?~(his ~ (read-history-tree nav u.his))
    =/  las=(unit knot)
      ?:(|(?=(~ his) ?=(~ nex)) ~ (read-history-tree (snoc nav u.nex) u.his))
    =/  def-cf=tape  cf-2
    ;scroll(w "grow", h "100%", cb ?^(file cb-2 cb-1), cf ?^(file cf-1 def-cf))
        =id  ?^(file (spud [%file prev.col]) (spud [des.nav prev.col]))
      ;*  ?^  file
            ^-  marl
            :~  ;box: {(trip u.file)}
            ==
          ^-  marl
          %+  spun  rows.col
          |=  [cod=cord i=@]
          ^-  [manx @]
          =/  light=?
            ?:  ?=(~ nex)  =(0 i)
            ?:  ?=(~ pax.nav)
              ?:  ?=(~ las)  =(0 i)
              =(cod u.las)
            =(cod u.nex)
          :_  +(i)
          ;box(w "100%", h "1", cb ?:(light cf-2 cb-1), cf ?:(light cb-1 def-cf))
            ;+  ;/  (trip cod)
          ==
    ==
  ::
  ++  rows-main
    |=  =col
    ^-  marl
    %+  turn  rows.col
    |=  cod=cord
    ^-  manx
    ;select(w "100%", h "1", select-cb cf-1, select-cf cb-1)
      =id  (spud [%row (snoc prev.col cod)])
      ;+  ;/  (trip cod)
    ==
  ::
  ++  cb-1  "#000000"
  ++  cb-2  "#044303"
  ++  cf-1  "#0dc40a"
  ++  cf-2  "#0a7a09"
  ::
  --
--
