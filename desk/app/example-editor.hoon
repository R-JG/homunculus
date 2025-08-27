/-  homunculus
|%
+$  tabs   (list place)
+$  place  $@(~ [=desk =path])
+$  state
  $:  explorer=$~([%base ~] place)
      editor-source=place
      editor-tabs=tabs
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
  :~  ~(render-full tui bol)
      ~(register tui bol)
  ==
++  on-save
  ^-  vase
  !>(~)
++  on-load
  |=  old=vase
  ^-  (quip card _this)
  :_  this
  :~  ~(render-full tui bol)
      ~(register tui bol)
  ==
::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  
++  on-poke
  |=  [=mark =vase]
  ^-  (quip card _this)
  ?>  =(our.bol src.bol)
  ?+  mark  !!
    ::
      %homunculus-event
    =/  eve  !<(event:homunculus vase)
    ?+  -.eve  !!
      ::
        %open
      :_  this
      :~  ~(render-full tui bol)
          ~(register tui bol)
      ==
      ::
        %select
      :: ~&  >>  eve
      [~ this]
      ::
        %act
      ?+  p.eve  [~ this]
        ::
          [%explorer *]
        ?~  t.p.eve  [~ this]
        =/  next=place  [i.t.p.eve t.t.p.eve]
        ?:  (file-exists next bol)
          =:  editor-source  next
              editor-tabs    [next editor-tabs]
              explorer       ?~(explorer ~ explorer(path (snip path.explorer)))
            ==
          :_  this
          :~  ~(render-full tui bol)
          ==
        =.  explorer  next
        :_  this
        :~  ~(render-explorer-panel tui bol)
        ==
        ::
          [%explorer-back ~]
        ?:  =(~ explorer)  [~ this]
        =.  explorer
          ?:  ?|  ?=(~ explorer)
                  ?=(~ path.explorer)
              ==
            ~
          %=  explorer
            path  (snip `path`path.explorer)
          ==
        :_  this
        :~  ~(render-explorer-panel tui bol)
        ==
        ::
          [%tab *]
        ?~  t.p.eve  [~ this]
        =/  next=place  [i.t.p.eve t.t.p.eve]
        ?:  =(next editor-source)
          [~ this]
        =.  editor-source  next
        :_  this
        :~  ~(render-full tui bol)
        ==
        ::
      ==
      ::
        %form
      :: ~&  >  eve
      [~ this]
      ::
    ==
    ::
  ==
::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  
++  on-watch  |=(path ^-((quip card _this) !!))
++  on-leave  |=(path ^-((quip card _this) !!))
++  on-peek   |=(path ^-((unit (unit cage)) !!))
++  on-agent  |=([wire sign:agent:gall] ^-((quip card _this) !!))
++  on-arvo   |=([wire sign-arvo] ^-((quip card _this) !!))
++  on-fail   |=([term tang] ^-((quip card _this) !!))
::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  
--
::
|%
::
++  tui
  |_  bol=bowl:gall
  ::
  ++  register
    ^-  card
    :*  %pass  /homunculus  %agent  [our.bol %homunculus]
        %poke  %homunculus-register  !>(~)
    ==
  ::
  ++  make-update-card
    |=  upd=update:homunculus
    ^-  card
    :*  %pass  /homunculus  %agent  [our.bol %homunculus]
        %poke  %homunculus-update  !>(upd)
    ==
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
--

