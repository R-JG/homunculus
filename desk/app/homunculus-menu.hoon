/-  homunculus
|%
::
+$  menu-state
  $:  =ship-info
      =open-session-mode
      =active-frame:homunculus
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
        =.  open-session-mode.state
          ?~  open-session-mode.state  [~ i.t.p.eve]
          ?:  =(u.open-session-mode.state i.t.p.eve)  ~
          [~ i.t.p.eve]
        :_  this
        :~  ~(root-update tui bol)
        ==
        ::
          [%new-session @ta @ta ~]
        ?~  open-session-mode.state  [~ this]
        =/  key  (layout-key:homunculus (cue (slav %ud i.t.t.p.eve)))
        =/  dir  (layout-dir:homunculus i.t.p.eve)
        :_  this(open-session-mode.state ~)
        :~  (make-session-open-card our.bol u.open-session-mode.state [%current-frame dir key])
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
  ++  root-update
    ^-  card
    (make-menu-update-card our.bol [%update %root root])
  ::
  ++  register-update
    ^-  card
    (make-menu-update-card our.bol [%update %branch ~[register-container]])
  ::
  ++  open-session-update
    ^-  card
    (make-menu-update-card our.bol [%update %branch ~[register-container frame-container]])
  ::
  ++  root
    ^-  manx
    ;layer(fx "center", fy "center")
      ;col(w "60%", h "60%", px "2", py "1", bg "magenta")
        ;+  header
        ;row(mt "1")
          ;+  register-container
          ;+  frame-container
        ==
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
    =;  frame
      ;col/"frame-container"(bg "green", b "arc")
        =w  ((d-co:co 1) +(+(wid)))
        =h  ((d-co:co 1) +(+(hei)))
        ;+  frame
      ==
    |-  ^-  manx
    =+  [w=((d-co:co 1) wid) h=((d-co:co 1) hei)]
    ?-  -.layout
        %$
      =/  k  (trip (scot %ud (jam (flop key))))
      ;row(w w, h h, fx "center", fy "center")
        ;*  ?~  open-session-mode.state  ~
            ?:  ?=(%$ q.p.layout)
              :~  ;layer(fx "center", fy "center")
                    ;select/"new-session/c/{k}"(w "1", h "1", bg "red", select-d "underline"):"C"
                  ==
              ==
            :~  ;layer(fy "center")
                  ;select/"new-session/l/{k}"(w "1", h "1", bg "red", select-d "underline"):"L"
                ==
                ;layer(fx "end", fy "center")
                  ;select/"new-session/r/{k}"(w "1", h "1", bg "red", select-d "underline"):"R"
                ==
                ;layer(fx "center")
                  ;select/"new-session/t/{k}"(w "1", h "1", bg "red", select-d "underline"):"T"
                ==
                ;layer(fx "center", fy "end")
                  ;select/"new-session/b/{k}"(w "1", h "1", bg "red", select-d "underline"):"B"
                ==
            ==
        ;+  ;/  (trip q.p.layout)
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
  ++  header
    ^-  manx
    ;col(w "47", h "8", px "2", pt "1", bg "#000000", fg "#fc8021", fx "center")
      ;border-l(b "heavy");
      ;border-r(b "heavy");
      ;border-t(b "heavy");
      ;border-b(bg "#38d99b", fg "#000000")
        ;row(w "100%", h "1", px "2")
          ;select(fl "row", select-d "bold", select-fg "#9effda")
            ;+  ;/  (trip (scot %ud kelvin.ship-info.state))
            ;+  ;/  "K"
          ==
          ;row(w "grow", h "1");
          ;row(mr "1"): ship:
          ;select(select-d "bold", select-fg "#9effda"): {(trip (scot %p our.bol))}
        ==
      ==
      ;select(select-d "blink")
        ;art(fg "#cc5a02")
          ;+  ;/
            """
            ╭     ╮ ╭─────╮ ╭────╮  ──┬── ╭──┬──╮
            """
        ==
        ;art
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
      ;col(w "100%", h "1", mt "1")
        ;layer(fx "center")
          ;pattern(w "9", h "1", fg "#38d99b"):"▮"
        ==
        ;layer(fx "center")
          ;pattern(w "13", h "1", fg "#34CB91"):"▮"
        ==
        ;layer(fx "center")
          ;pattern(w "17", h "1", fg "#2DAE7C"):"▮"
        ==
        ;layer(fx "center")
          ;pattern(w "21", h "1", fg "#248B63"):"▮"
        ==
        ;layer(fx "center")
          ;pattern(w "25", h "1", fg "#1D6F4F"):"▮"
        ==
        ;layer(fx "center")
          ;pattern(w "29", h "1", fg "#17593F"):"▮"
        ==
        ;layer(fx "center")
          ;pattern(w "33", h "1", fg "#124732"):"▮"
        ==
        ;layer(fx "center")
          ;pattern(w "37", h "1", fg "#0E3928"):"▮"
        ==
        ;layer
          ;pattern(w "100%", h "1", fg "#0B2E20"):"▮"
        ==
      ==
    ==
  ::
  --
::
++  make-session-open-card
  |=  [our=@p ses=@tas ope=session-open:homunculus]
  ^-  card
  (make-menu-update-card our [%open-session [our ses] ope])
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
