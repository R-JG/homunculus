/-  homunculus
|%
+$  state
  $:  kelvin=@
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
  =.  kelvin  (get-kelvin bol)
  :_  this
  :~  ~(render tui bol)
  ==
++  on-save
  ^-  vase
  !>(~)
++  on-load
  |=  old=vase
  ^-  (quip card _this)
  =.  kelvin  (get-kelvin bol)
  :_  this
  :~  ~(render tui bol)
  ==
::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  
++  on-poke
  |=  [=mark =vase]
  ^-  (quip card _this)
  ?>  =(our.bol src.bol)
  ?+  mark  !!
    ::
      %open
    :_  this
    :~  ~(render tui bol)
    ==
    ::
      %homunculus-event
    =/  eve  !<(event:homunculus vase)
    ?+  -.eve  !!
      ::
        %select
      ~&  >>  eve
      [~ this]
      ::
        %act
      ~&  >>>  eve
      [~ this]
      ::
        %form
      ~&  >  eve
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
  ++  render
    ^-  card
    :*  %pass  /homunculus  %agent  [our.bol %homunculus]
        %poke  %homunculus-menu  !>(`menu:homunculus`[%update %full root])
    ==
  ::
  ++  root
    ^-  manx
    ;layer(fx "center", fy "center")
      ;box(w "47", h "8", px "2", pt "1", bg "#000000", fg "#fc8021", fx "center")
        ;border-l(b "heavy");
        ;border-r(b "heavy");
        ;border-t(b "heavy");
        ;border-b(bg "#38d99b", fg "#000000")
          ;box(w "100%", h "1", px "2", fl "row")
            ;select(fl "row", select-d "bold", select-fg "#9effda")
              ;+  ;/  (trip (scot %ud kelvin))
              ;+  ;/  "K"
            ==
            ;box(w "grow", h "1");
            ;box(mr "1"): ship:
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
        ;box(w "100%", h "1", mt "1")
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
    ==
  ::
  --
::
++  get-kelvin
  |=  bol=bowl:gall
  ^-  @
  =/  waf  .^(waft:clay %cx (en-beam [[our.bol %base [%da now.bol]] /sys/kelvin]))
  ?@  -.waf  num.waf
  (~(rep in p.waf) |=([i=weft a=$~(500 @)] (min a num.i)))
::
--
