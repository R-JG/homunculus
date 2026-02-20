/-  homunculus
|%
::
+$  xeno-sources  (map desk ship)
::
+$  place  $@(~ [=desk =path])
+$  tabs   (list place)
::
+$  state-0
  $:  explorer=place
      editor-source=place
      editor-tabs=tabs
  ==
+$  state-n
  $%  [%0 state-0]
  ==
+$  card  card:agent:gall
--
::
=|  $>(%0 state-n)
=*  state  -
=>
::
|_  [bol=bowl:gall cards=(list card)]
++  cor   .
++  abet  :-  (flop cards)  state
++  emit  |=  =card  cor(cards [card cards])
++  emil  |=  caz=(list card)  cor(cards (welp (flop caz) cards))
::
++  poke
  |=  [mak=mark vaz=vase]
  ^+  cor
  ?+  mak  ~|(bad-poke/mak !!) 
  ::
      %homunculus-event
    ?>  =(src our):bol
    =/  eve  !<(event:homunculus vaz)
    ?+  -.eve  !!
    ::
        %open
      %-  emil
      :~  render-full:tui
          register:tui
      ==
    ::
        %select
      cor
    ::
        %act
      ?+  p.eve  !!
      ::
          [%explorer *]
        ?~  t.p.eve  cor
        =/  next=place  [i.t.p.eve t.t.p.eve]
        ?:  (file-exists next bol)
          =:  editor-source  next
              editor-tabs    [next editor-tabs]
              explorer       ?~(explorer ~ explorer(path (snip path.explorer)))
            ==
          %-  emil
          :~  render-full:tui
          ==
        =.  explorer  next
        %-  emil
        :~  render-explorer-panel:tui
        ==
      ::
          [%explorer-back ~]
        ?:  =(~ explorer)  !!
        =.  explorer
          ?:  ?|  ?=(~ explorer)
                  ?=(~ path.explorer)
              ==
            ~
          %=  explorer
            path  (snip `path`path.explorer)
          ==
        %-  emil
        :~  render-explorer-panel:tui
        ==
      ::
          [%tab *]
        ?~  t.p.eve  !!
        =/  next=place  [i.t.p.eve t.t.p.eve]
        ?:  =(next editor-source)  cor
        =.  editor-source  next
        %-  emil
        :~  render-full:tui
        ==
      ::
      ==
    ::
        %form
      :: ~&  >  eve
      cor
    ::
    ==
  ::
  == 
::
++  peek
  |=  poe=(pole @ta)
  ^-  (unit (unit cage))
  ~
::
++  watch
  |=  poe=(pole @ta)
  ^+  cor
  cor
::
++  leave
  |=  poe=(pole @ta)
  ^+  cor
  cor
::
++  fail
  |=  [tem=term tan=tang]
  ^+  cor
  cor
::
++  arvo
  |=  [wir=(pole @ta) sin=sign-arvo]
  ^+  cor
  cor
::
++  agent
  |=  [wir=wire sin=sign:agent:gall]
  ^+  cor
  cor
::
  ::
::
++  file-exists
  |=  [at=place bol=bowl:gall]
  ^-  ?
  ?~  at  |
  .^(? %cu (en-beam [[our.bol desk.at [%da now.bol]] path.at]))
::
++  get-explorer-list
  |=  bol=bowl:gall
  ^-  (list @t)
  %+  sort
    %~  tap  in
    ?~  explorer
      .^((set desk) %cd (en-beam [[our.bol %$ [%da now.bol]] ~]))
    %~  key  by
    =<  dir
    .^(arch %cy (en-beam [[our.bol desk.explorer [%da now.bol]] path.explorer]))
  aor
::
++  tui
  |%
  ::
  ++  render-full
    ^-  card
    %-  make-update-card
    :~  [%element root]
    ==
  ::
  ++  render-explorer-panel
    ^-  card
    %-  make-update-card
    :~  [%element explorer-panel]
        [%set-scroll-position %c 0 /explorer-list]
    ==
  ::
  ++  make-update-card
    |=  upd=update:homunculus
    ^-  card
    :*  %pass  /homunculus  %agent  [our.bol %homunculus]
        %poke  %homunculus-update  !>(upd)
    ==
  ::
  ++  register
    ^-  card
    :*  %pass  /homunculus  %agent  [our.bol %homunculus]
        %poke  %homunculus-register  !>(~)
    ==
  ::
  ++  root
    ^-  manx
    ;row(w "100%", h "100%", bg dark-gray, fg light-blue-1)
      ;+  explorer-panel
      ;+  editor-panel
    ==
  ::
  ++  explorer-panel
    ^-  manx
    ;col/"explorer-panel"(w "22%", h "100%", fx "center", bg dark-blue-2)
      ;row(w "100%", h "1", px "2", fg cyan-1)
        ;select/"explorer-back"(px "1", select-bg cyan-1, select-fg dark-gray):"◀"
        ;row(ml "2")
          ;+  ;/
            ?~  explorer  "~"
            (spud explorer)
        ==
      ==
      ;scroll/"explorer-list"(w "85%", h "grow")
        ;*  %+  turn  (get-explorer-list bol)
            |=  i=@t
            ^-  manx
            =/  id=tape
              %+  weld  "explorer"
              ?~  explorer  "/{(trip i)}"
              (spud `path`explorer(path (snoc path.explorer i)))
            ;select/"{id}"(w "100%", px "1", mt "1", bg dark-blue-1, select-bg cyan-1, select-fg dark-gray):"{(trip i)}"
      ==
    ==
  ::
  ++  editor-panel
    ^-  manx
    ;col(w "grow", h "100%", bg dark-blue-2)
      ;*  ?~  editor-source
            ;=  ;row(w "100%", h "1", fx "center", fg cyan-1):"~"
                ;col(w "100%", h "grow", pb "1", pr "2")
                  ;pattern(w "100%", h "100%", bg dark-gray, fg dark-blue-1):" □"
                ==
            ==
          =/  src  (spud editor-source)
          ;=  ;row(w "100%", h "1", px "2", fg cyan-1)
                ;*  %+  turn  editor-tabs
                    |=  i=place
                    ^-  manx
                    ?>  ?=(^ i)
                    =/  s  (spud i)
                    =/  is-active=?  =(i editor-source)
                    ;select/"tab{s}"(h "1", mx "1")
                      ;+  ?:  is-active
                            ;row(px "1", bg light-blue-1, fg blue-1):"{s}"
                          ;row(px "1", bg dark-blue-1, fg light-blue-1, select-fg white):"{s}"
                    ==
              ==
              ;col(w "100%", h "grow", pb "1", pr "2")
                ;editor/"{src}"(w "100%", h "100%", bg dark-gray, fg light-blue-2);
              ==
          ==
    ==
  ::
  ++  white         "#FFFFFF"
  ++  black         "#020202"
  ++  dark-gray     "#171C1D"
  ++  light-blue-1  "#51D6FF"
  ++  light-blue-2  "#A8E0FF"
  ++  blue-1        "#2B59C3"
  ++  dark-blue-1   "#12355B"
  ++  dark-blue-2   "#13293D"
  ++  cyan-1        "#bdfff6"
  ++  orange        "#FF570A"
  ::
  --
::
  ::
::
++  init
  ^+  cor
  %-  emil
  :~  render-full:tui
      register:tui
  ==
::
++  save
  ^-  vase
  !>  ~
::
++  load
  |=  vaz=vase
  ^+  cor
  init
::
--
::
^-  agent:gall
|_  =bowl:gall
+*  this  .
    cor  ~(. +> [bowl ~])
::
++  on-init
  ^-  (quip card _this)
  =^  cards  state  abet:init:cor
  :-  cards  this
::
++  on-save
  ^-  vase
  =<  save  cor
::
++  on-load
  |=  =vase
  ^-  (quip card _this)
  =^  cards  state  abet:(load:cor vase)
  :-  cards  this
::
++  on-poke
  |=  [=mark =vase]
  ^-  (quip card _this)
  =^  cards  state  abet:(poke:cor mark vase)
  :-  cards  this
::
++  on-watch
  |=  =path
  ^-  (quip card _this)
  =^  cards  state  abet:(watch:cor path)
  :-  cards  this
::
++  on-leave
  |=  =path
  ^-  (quip card _this)
  =^  cards  state  abet:(leave:cor path)
  :-  cards  this
::
++  on-peek
  |=  =path
  ^-  (unit (unit cage))
  %-  peek:cor  path
::
++  on-agent
  |=  [=wire =sign:agent:gall]
  ^-  (quip card _this)
  =^  cards  state  abet:(agent:cor wire sign)
  :-  cards  this
::
++  on-arvo
  |=  [=wire sign=sign-arvo]
  ^-  (quip card _this)
  =^  cards  state  abet:(arvo:cor wire sign)
  :-  cards  this
::
++  on-fail
  |=  [=term =tang]
  ^-  (quip card _this)
  =^  cards  state  abet:(fail:cor term tang)
  :-  cards  this
--

