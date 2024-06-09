/-  homunculus
|%
+$  nav
  $:  des=$~(%base desk)
      pax=path
  ==
+$  history-tree
  $~  [%$ ~ ~]
  $:  name=term
      recent=(unit term)
      branches=(list history-tree)
  ==
+$  history  (map desk history-tree)
+$  col  [prev=path rows=(list @t)]
+$  cols  [lef=col cen=col rig=col]
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
  ^-  (quip card _this)
  ?+  mark  !!
    ::
      %open
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
        ::
          %row
        ?~  t.pav  !!
        ?~  pax.nav
          =.  des.nav  i.t.pav
          :_  this
          :~  (~(render tui [nav bol]) [~ hotkeys])
          ==
        :: to do: add center col select case
        [~ this]
        ::
      ==
      ::
        %hotkey
      ?+  ^-(@tas id.eve)  !!
        ::
          %nav-left
        :: note: nav left does not transform history, unlike right and up/down select 
        ~&  >  %left
        [~ this]
        ::
          %nav-right
        =/  his=(unit history-tree)  (~(get by history) des.nav)
        =/  nex=(unit term)
          ?~  his  ~
          (read-history-tree pax.nav u.his)
        =/  scy=cols  (do-scry nav bol)
        =.  state
          ?~  nex
            ?:  =(~ pax.nav)
              ?~  rows.cen.scy  state
              %_  state
                pax.nav  ^-(path [i.rows.cen.scy ~])
                history
                  ?~  his  history  :: to do: if null, make put-history-tree populate a tree up to the current point.
                  %+  %~  put  by  history
                    des.nav
                  (put-history-tree i.rows.cen.scy pax.nav u.his)
              ==
            ?~  rows.rig.scy  state
              %_  state
                pax.nav  (snoc pax.nav i.rows.rig.scy)
                history
                  ?~  his  history
                  %+  %~  put  by  history
                    des.nav
                  (put-history-tree i.rows.rig.scy pax.nav u.his)
              ==
          ?~  (find [u.nex ~] ?~(pax.nav rows.cen.scy rows.rig.scy))
            :: to do:
            :: del-history --> delete the branch from his corresponding to the nex term, and set the recent to null in the parent.
            :: nest inward based on the first row default like the above cases.
            state
            ::
          %_  state
            pax.nav  (snoc pax.nav u.nex)
            history
              ?~  his  history
              %+  %~  put  by  history
                des.nav
              (put-history-tree u.nex pax.nav u.his)
          ==
        :_  this
        :~  (~(render tui [nav bol]) [(get-sel nav) hotkeys])
        ==
        ::
      ==
      ::
    ==
    ::
  ==
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
  |=  [pax=path his=history-tree]
  ^-  (unit term)
  ?~  pax  recent.his
  |-  ^-  (unit term)
  ?~  branches.his  ~
  ?:  =(i.pax name.i.branches.his)
    ^$(his i.branches.his)
  $(branches.his t.branches.his)
::
++  put-history-tree
  |=  [new=term pre=path his=history-tree]
  ^-  history-tree
  ?:  &(?=(^ pre) ?=(~ t.pre) =(i.pre name.his))
    %_  his
      recent    [~ new]
      branches  [[new ~ ~] branches.his]
    ==
  ?~  pre  his
  %_    his
      branches
    |-  ^-  (list history-tree)
    ?~  branches.his  ~
    :-  ^$(pre t.pre, his i.branches.his)
    $(branches.his t.branches.his)
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
  =/  rig=col
    ?:  &(?=(~ pax) ?=(~ rows.cen))
      [~ ~]
    =/  xap=path
      ?~  pax
        ?~  rows.cen  !!
        (snoc prev.cen i.rows.cen)
      pax
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
  [lef cen rig]
::
++  tui
  |_  [=^nav bol=bowl:gall]
  ::
  ++  render
    |=  [met=metadata:homunculus]
    ^-  card
    :*  %pass  /homunculus  %agent  [our.bol %homunculus]
        %poke  %homunculus-session  !>([met root])
    ==
  ::
  ++  root
    ^-  manx
    ;box(w "100%", h "100%", cb cb-1, cf cf-1)
      ;+  columns
    ==
  ::
  ++  columns
    ^-  manx
    =/  =cols  (do-scry nav bol)
    ;box(w "100%", h "100%", b "arc")
      ;+  (column-left lef.cols)
      ;line-v;
      ;+  (column-center cen.cols)
      ;line-v;
      ;+  (column-right rig.cols)
    ==
  ::
  ++  column-left
    |=  =col
    ^-  manx
    ;scroll(w "30%", h "100%")
      ;*  ?~  pax.nav
            (rows-select col)
          (rows-inert col)
    ==
  ::
  ++  column-center
    |=  =col
    ^-  manx
    ;scroll(w "40%", h "100%")
      ;*  ?^  pax.nav
            (rows-select col)
          (rows-inert col)
    ==
  ::
  ++  column-right
    |=  =col
    ^-  manx
    ;scroll(w "grow", h "100%")
      ;*  (rows-inert col)
    ==
  ::
  ++  rows-select
    |=  =col
    ^-  marl
    %+  turn
      rows.col
    |=  cod=cord
    ;select(w "100%", h "1", select-cb cf-1, select-cf cb-1)
      =id  (spud [%row (snoc prev.col cod)])
      ;+  ;/  (trip cod)
    ==
  ::
  ++  rows-inert
    |=  =col
    ^-  marl
    %+  turn
      rows.col
    |=  cod=cord
    ;box(w "100%", h "1")
      ;+  ;/  (trip cod)
    ==
  ::
  ++  cb-1  "#000000"
  ++  cf-1  "#0dc40a"
  ::
  --
--
