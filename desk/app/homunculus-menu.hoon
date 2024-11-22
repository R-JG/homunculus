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
  !>(~)
++  on-load
  |=  old=vase
  ^-  (quip card _this)
  =.  kelvin.ship-info.state  (get-kelvin bol)
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
    (make-menu-update-card our.bol [%update %root root])
  ::
  ++  register-update
    ^-  card
    (make-menu-update-card our.bol [%update %branch ~[register-container]])
  ::
  ++  frame-update
    ^-  card
    (make-menu-update-card our.bol [%update %branch ~[frame-container frames-list]])
  ::
  ++  open-session-update
    ^-  card
    (make-menu-update-card our.bol [%update %branch ~[register-container frame-container frames-list]])
  ::
  ++  root
    ^-  manx
    ;layer(fx "center", fy "center")
      ;col(w "60%", h "60%", px "2", py "1", bg "magenta")
        ;+  header
        ;row(my "1")
          ;+  register-container
          ;*  ?~  open-session-mode.state  ~[frame-container]
              ;=  ;select/"open-session/new/l"(w "1", h "3", pt "1", bg "red"):"◀"
                  ;+  frame-container
                  ;select/"open-session/new/r"(w "1", h "3", pt "1", bg "red"):"▶"
              ==
        ==
        ;+  frames-list
      ==
    ==
  ::
  ++  register-container
    ^-  manx
    ;col/"register-container"(w "20", h "10", mr "2", bg "blue")
      ;*  ?~  open-session-mode.state  ~
          :_  ~
          ;row(px "2", mb "1", bg "red"):"OPEN: {(trip u.open-session-mode.state)}"
      ;*  %+  turn
            %+  sort  ~(tap in register.state)
            |=  $:  a=session-source:homunculus
                    b=session-source:homunculus
                ==
            (aor q.a q.b)
          |=  i=session-source:homunculus
          ^-  manx
          =/  name  (trip q.i)
          ;select/"agent/{name}"(w "100%", h "1", select-bg "red"):"{name}"
    ==
  ::
  ++  frame-container
    ^-  manx
    =|  key=layout-key:homunculus
    =/  [wid=@ hei=@]       [51 11]
    =/  =layout:homunculus  (snag active-frame.state frames.state)
    =;  frame=manx
      ;col/"frame-container"(bg "green", b "arc")
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
                  ;select/"close-session/{name}"(bg "red"):"X"
                ==
            ==
        ;*  ?~  open-session-mode.state  ~
            ?:  ?=(%$ q.p.layout)
              ;=  ;layer(fx "center", fy "center")
                    ;select/"open-session/current/c/{k}"(w "1", h "1", bg "red", select-d "underline"):"C"
                  ==
              ==
            ;=  ;layer(fy "center")
                  ;select/"open-session/current/l/{k}"(w "1", h "1", bg "red", select-d "underline"):"L"
                ==
                ;layer(fx "end", fy "center")
                  ;select/"open-session/current/r/{k}"(w "1", h "1", bg "red", select-d "underline"):"R"
                ==
                ;layer(fx "center")
                  ;select/"open-session/current/t/{k}"(w "1", h "1", bg "red", select-d "underline"):"T"
                ==
                ;layer(fx "center", fy "end")
                  ;select/"open-session/current/b/{k}"(w "1", h "1", bg "red", select-d "underline"):"B"
                ==
            ==
        ;+  ;/  name
      ==
        %v
      =/  lef  (div (mul p.layout wid) 100)
      =/  rig  (sub wid lef)
      =?  rig  !=(0 rig)  (dec rig)
      ;row(w w, h h)
        ;+  $(wid lef, layout l.layout, key [%0 key])
        ;line-v;
        ;+  $(wid rig, layout r.layout, key [%1 key])
      ==
        %h
      =/  top  (div (mul p.layout hei) 100)
      =/  bot  (sub hei top)
      =?  bot  !=(0 bot)  (dec bot)
      ;col(w w, h h)
        ;+  $(hei top, layout t.layout, key [%0 key])
        ;line-h;
        ;+  $(hei bot, layout b.layout, key [%1 key])
      ==
    ==
  ::
  ++  frames-list
    ^-  manx
    ;row/"frames-list"(w "100%", h "1", fx "center")
      ;*  %+  spun  frames.state
          |=  [i=frame:homunculus a=@]
          ^-  [manx @]
          :_  +(a)
          =/  n=tape   (scow %ud a)
          =/  bg=tape  ?:(=(a active-frame.state) "red" "blue")
          ;select/"frame/{n}"(w "6", h "1", px "1", mx "1", bg bg, select-d "underline"):"{n}"
    ==
  ::
  ++  header
    ^-  manx
    ;row(w "100%", px "2", py "1", bg black, fg "#fc8021")
      ;col
        ;art(fg "#cc5a02")
          ;+  ;/
            """
            ╭     ╮ ╭─────╮ ╭────╮  ──┬── ╭──┬──╮
            """
        ==
        ;art(fg "#fc8021")
          ;+  ;/
            """
            │     │ ├────┬╯ ├────┴╮   │      │   
            """
        ==
        ;art(fg "#cc5a02")
          ;+  ;/
            """
            ╰─────╯ ╰    ╰─ ╰─────╯ ──┴──    ┴    
            """
        ==
      ==
      ;col(w "grow", fx "end")
        ;row:"{(trip (scot %p our.bol))}"
        ;line-h(w "3");
        ;row
          ;+  ;/  (trip (scot %ud kelvin.ship-info.state))
          ;+  ;/  "K"
        ==
      ==
    ==
  ::
  ++  black  "#000000"
  ::
  --
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
