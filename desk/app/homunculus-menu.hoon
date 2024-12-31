/-  homunculus
|%
::
+$  menu-state
  $:  =ship-info
      =open-session-mode
      active-frame=@
      =frames:homunculus
      =register:homunculus
  ==
+$  open-session-mode  (unit @tas)
+$  ship-info
  $:  kelvin=@
  ==
+$  card  card:agent:gall
--
::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  
=|  state=menu-state
^-  agent:gall
=<
|_  bol=bowl:gall
+*  this  .
++  on-init
  ^-  (quip card _this)
  =.  kelvin.ship-info.state  (get-kelvin bol)
  :_  this
  :~  ~(root-update tui bol)
      (make-menu-update-card our.bol [%load-state ~])
  ==
++  on-save
  ^-  vase
  !>(state)
++  on-load
  |=  old=vase
  ^-  (quip card _this)
  =.  state
    =/  old-state  (mole |.(!<(menu-state old)))
    ?^  old-state  u.old-state
    %_  state
      kelvin.ship-info  (get-kelvin bol)
    ==
  :_  this
  :~  ~(root-update tui bol)
      (make-menu-update-card our.bol [%load-state ~])
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
      :~  ~(root-update tui bol)
      ==
      ::
        %select
      :: ~&  >>  eve
      [~ this]
      ::
        %act
      :: ~&  >>>  eve
      ?+  p.eve  !!
        ::
          [%agent @ta ~]
        =/  =layout:homunculus  (snag active-frame.state frames.state)
        ?:  &(?=(%$ -.layout) ?=(%$ q.p.layout) ?=(~ open-session-mode.state))
          :_  this
          :~  %+  make-menu-update-card  our.bol
              [%open-session [our.bol i.t.p.eve] [%current-frame %c ~]]
          ==
        =.  open-session-mode.state
          ?~  open-session-mode.state  [~ i.t.p.eve]
          ?:  =(u.open-session-mode.state i.t.p.eve)  ~
          [~ i.t.p.eve]
        :_  this
        :~  ~(root-update tui bol)
        ==
        ::
          [%frame @ta ~]
        =/  i  (slav %ud i.t.p.eve)
        :_  this
        :~  (make-menu-update-card our.bol [%change-frame i])
        ==
        ::
          [%open-session %current @ta @ta ~]
        ?~  open-session-mode.state  [~ this]
        =/  key  (layout-key:homunculus (cue (slav %ud i.t.t.t.p.eve)))
        =/  dir  (layout-dir:homunculus i.t.t.p.eve)
        :_  this(open-session-mode.state ~)
        :~  %+  make-menu-update-card  our.bol
            [%open-session [our.bol u.open-session-mode.state] [%current-frame dir key]]
        ==
        ::
          [%open-session %new @ta ~]
        ?~  open-session-mode.state  [~ this]
        =/  dir  (?(%l %r) i.t.t.p.eve)
        :_  this(open-session-mode.state ~)
        :~  %+  make-menu-update-card  our.bol
            [%open-session [our.bol u.open-session-mode.state] [%new-frame dir]]
        ==
        ::
          [%close-session @ta ~]
        :_  this
        :~  %+  make-menu-update-card  our.bol
            [%close-session [our.bol i.t.p.eve]]
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
      %homunculus-menu-diff
    =/  diff  !<(menu-diff:homunculus vase)
    ?-  -.diff
      ::
        %put-register
      =.  register.state  (~(put in register.state) p.diff)
      :_  this
      :~  ~(register-update tui bol)
      ==
      ::
        %del-register
      =.  register.state  (~(del in register.state) p.diff)
      :_  this
      :~  ~(register-update tui bol)
      ==
      ::
        %all-frames
      =:  active-frame.state  p.diff
          frames.state        q.diff
        ==
      :_  this
      :~  ~(root-update tui bol)
      ==
      ::
        %active-frame
      =.  active-frame.state  p.diff
      :_  this
      :~  ~(frame-update tui bol)
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
  ++  root-update
    ^-  card
    (make-menu-update-card our.bol [%update ~[[%element root]]])
  ::
  ++  register-update
    ^-  card
    %+  make-menu-update-card  our.bol
    [%update ~[[%element register-container]]]
  ::
  ++  frame-update
    ^-  card
    %+  make-menu-update-card  our.bol
    [%update ~[[%element frame-container] [%element frames-list]]]
  ::
  ++  open-session-update
    ^-  card
    %+  make-menu-update-card  our.bol
    [%update ~[[%element register-container] [%element frame-container] [%element frames-list]]]
  ::
  ++  root
    ^-  manx
    ;layer(fx "center", fy "center")
      ;col(w "82", h "28", px "2", py "1", fg white, bg green-4, b "heavy", b-fg green-2)
        ;+  header
        ;row(w "100%", mt "2", mb "1", bg green-3)
          ;+  ?~  open-session-mode.state  
                ;row(w "1", h "1");
              ;select/"open-session/new/l"(w "1", h "3", pt "1", mt "5", bg green-1, fg green-3, select-bg cyan-2, select-fg white)
                ;+  ;/  "⢾"
              ==
          ;+  frame-container
          ;+  ?~  open-session-mode.state
                ;row(w "1", h "1");
              ;select/"open-session/new/r"(w "1", h "3", pt "1", mt "5", bg green-1, fg green-3, select-bg cyan-2, select-fg white)
                ;+  ;/  "⡷"
              ==
          ;+  register-container
        ==
        ;+  frames-list
      ==
    ==
  ::
  ++  register-container
    ^-  manx
    =/  open-agents  get-all-open-agents
    ;col/"register-container"(w "grow", h "10", px "2", mt "1", ml "2")
      ;+  ?~  open-session-mode.state
            ;row(w "100%", h "1", mb "1", fx "center", bg green-2, fg green-1):"Agents:"
          ;row(w "100%", h "1", mb "1", fx "center", bg cyan-2, fg white):"Open:"
      ;scroll/"register-scroll"(w "100%", h "grow")
        ;*  %+  turn
              %+  sort  ~(tap in register.state)
              |=  $:  a=session-source:homunculus
                      b=session-source:homunculus
                  ==
              (aor q.a q.b)
            |=  i=session-source:homunculus
            ^-  manx
            =/  name  (trip q.i)
            ?:  (~(has in open-agents) i)
              ;row(w "100%", h "1", fg green-2):"{name}"
            ?:  ?&  ?=(^ open-session-mode.state)
                    =(q.i u.open-session-mode.state)
                ==
              ;select/"agent/{name}"(w "100%", h "1", bg cyan-2, fg white):"{name}"
            ;select/"agent/{name}"(w "100%", h "1", select-bg white, select-fg green-3):"{name}"
      ==
    ==
  ::
  ++  frame-container
    ^-  manx
    =|  key=layout-key:homunculus
    =/  [wid=@ hei=@]       [51 11]
    =/  =layout:homunculus  (snag active-frame.state frames.state)
    =;  frame=manx
      ;col/"frame-container"(fg green-2, bg green-3, b "arc")
        =w  ((d-co:co 1) +(+(wid)))
        =h  ((d-co:co 1) +(+(hei)))
        ;+  frame
      ==
    ?:  &(?=(%$ -.layout) ?=(%$ q.p.layout))
      ;row(w "100%", h "100%")
        ;layer(fx "center", fy "center")
          ;+  ;/  "No active windows"
        ==
        ;pattern(w "100%", h "100%"): ╱
      ==
    |-  ^-  manx
    =+  [w=((d-co:co 1) wid) h=((d-co:co 1) hei)]
    ?-  -.layout
        %$
      =/  k  (trip (scot %ud (jam (flop key))))
      =/  name  (trip q.p.layout)
      ;row(w w, h h, fx "center", fy "center")
        ;*  ?^  open-session-mode.state  ~
            ;=  ;layer(fx "end")
                  ;select/"close-session/{name}"(px "1", select-fg red):"✖"
                ==
            ==
        ;*  ?~  open-session-mode.state  ~
            ?:  ?=(%$ q.p.layout)  ~
            ;=  ;layer(py "1")
                  ;select/"open-session/current/l/{k}"(w "2", h "grow", select-fg cyan-1)
                    ;pattern(w "100%", h "100%"):"⣿"
                  ==
                ==
                ;layer(py "1", fx "end")
                  ;select/"open-session/current/r/{k}"(w "2", h "grow", select-fg cyan-1)
                    ;pattern(w "100%", h "100%"):"⣿"
                  ==
                ==
                ;layer(px "1")
                  ;select/"open-session/current/t/{k}"(w "grow", h "1", select-fg cyan-1)
                    ;pattern(w "100%", h "100%"):"⣿"
                  ==
                ==
                ;layer(px "1", fy "end")
                  ;select/"open-session/current/b/{k}"(w "grow", h "1", select-fg cyan-1)
                    ;pattern(w "100%", h "100%"):"⣿"
                  ==
                ==
            ==
        ;row(fg "white"):"{name}"
      ==
        %v
      =/  lef  (div (mul p.layout wid) 100)
      =/  rig  (sub wid lef)
      =?  rig  !=(0 rig)  (dec rig)
      ;row(w w, h h)
        ;+  $(wid lef, layout l.layout, key [%0 key])
        ;line-v(fg green-1);
        ;+  $(wid rig, layout r.layout, key [%1 key])
      ==
        %h
      =/  top  (div (mul p.layout hei) 100)
      =/  bot  (sub hei top)
      =?  bot  !=(0 bot)  (dec bot)
      ;col(w w, h h)
        ;+  $(hei top, layout t.layout, key [%0 key])
        ;line-h(fg green-1);
        ;+  $(hei bot, layout b.layout, key [%1 key])
      ==
    ==
  ::
  ++  frames-list
    ^-  manx
    ;row/"frames-list"(w "100%", h "1")
      ;*  %+  spun  frames.state
          |=  [i=frame:homunculus a=@]
          ^-  [manx @]
          :_  +(a)
          =/  n=tape   (scow %ud a)
          =/  bg=tape  ?:(=(a active-frame.state) green-2 green-3)
          ;select/"frame/{n}"(w "7", h "1", mx "1", bg bg, fx "center", select-d "underline"):"{n}"
    ==
  ::
  ++  header
    ^-  manx
    ;row(w "100%", px "2", py "1", bg green-3)
      ;col
        ;art(fg green-2)
          ;+  ;/
            """
            ╭     ╮ ╭─────╮ ╭────╮  ──┬── ╭──┬──╮
            """
        ==
        ;art(fg green-1)
          ;+  ;/
            """
            │     │ ├────┬╯ ├────┴╮   │      │   
            """
        ==
        ;art(fg green-2)
          ;+  ;/
            """
            ╰─────╯ ╰    ╰─ ╰─────╯ ──┴──    ┴    
            """
        ==
      ==
      ;col(w "grow", fx "end", fg green-1)
        ;row:"{(trip (scot %p our.bol))}"
        ;line-h(w "3");
        ;row
          ;+  ;/  (trip (scot %ud kelvin.ship-info.state))
          ;+  ;/  "K"
        ==
      ==
    ==
  ::
  ++  black     "#000000"
  ++  red       "#A72608"
  ++  orange-1  "#fc8021"
  ++  orange-2  "#cc5a02"
  ++  green-1   "#0dc40a"
  ++  green-2   "#228721"
  ++  green-3   "#022801"
  ++  green-4   "#000f00"
  ++  cyan-1    "#9effda"
  ++  cyan-2    "#38d99b"
  ++  blue-1    "#4384bf"
  ++  blue-2    "#335C81"
  ++  blue-3    "#27394a"
  ++  white     "#F5EDF0"
  ::
  --
::
++  get-all-open-agents
  ^-  (set session-source:homunculus)
  %+  roll  frames.state
  |=  $:  i=layout:homunculus
          a=(set session-source:homunculus)
      ==
  (~(uni in a) (get-agents-in-layout i))
::
++  get-agents-in-layout
  |=  lay=layout:homunculus
  ^-  (set session-source:homunculus)
  ?-  -.lay
    %$  [p.lay ~ ~]
    %v  (~(uni in $(lay l.lay)) $(lay r.lay))
    %h  (~(uni in $(lay t.lay)) $(lay b.lay))
  ==
::
++  make-menu-update-card
  |=  [our=@p upd=menu-update:homunculus]
  ^-  card
  :*  %pass  /homunculus  %agent  [our %homunculus]
      %poke  %homunculus-menu-update  !>(upd)
  ==
::
++  get-kelvin
  |=  bol=bowl:gall
  ^-  @
  =/  waf  .^(waft:clay %cx (en-beam [[our.bol %base [%da now.bol]] /sys/kelvin]))
  ?@  -.waf  num.waf
  (~(rep in p.waf) |=([i=weft a=$~(500 @)] (min a num.i)))
::
--

