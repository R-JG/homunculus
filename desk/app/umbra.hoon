/-  homunculus
|%
+$  window  [agent=term l=@ud r=@ud t=@ud b=@ud]
+$  frame  (list window)
+$  frames  (list frame)
+$  poke-key
  $@  ~
  $:  name=cord
      =ship
      agent=term
      =mark
      data=vase
      data-txt=cord
  ==
+$  poke-keys
  $:  p1=poke-key
      p2=poke-key
      p3=poke-key
      p4=poke-key
      p5=poke-key
      p6=poke-key
      p7=poke-key
      p8=poke-key
      p9=poke-key
      p0=poke-key
  ==
+$  num  ?(%1 %2 %3 %4 %5 %6 %7 %8 %9 %0)
+$  state
  $:  =frames
      sel-poke-num=(unit num)
      poke-message=tape
      poke-edit-mode=$~(| ?)
      =poke-keys
  ==
::
+$  card  card:agent:gall
--
::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  
=|  state
=*  state  -
^-  agent:gall
=<
|_  =bowl:gall
+*  this  .
++  on-init
  ^-  (quip card _this)
  [~ this(state *^state)]
++  on-save
  ^-  vase
  !>(~)
++  on-load
  |=  old=vase
  ^-  (quip card _this)
  [[(sail-card our.bowl) ~] this]
::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  
++  on-poke
  |=  [=mark =vase]
  ^-  (quip card _this)
  ?+  mark  !!
    ::
      %refresh
    =.  frames  !<(^frames vase)
    [[(sail-card our.bowl) ~] this]
    ::
      %poke-key
    =/  pok=poke-key  (get-poke-key !<(num vase))
    ?~  pok  [~ this]
    :_  this
    :~  [%pass /poke-key %agent [ship.pok agent.pok] %poke mark.pok data.pok]
    ==
    ::
      %homunculus-event
    =/  eve  !<(event:homunculus vase)
    ?+  -.eve  !!
      ::
        %act
      =/  pat=path  (stab id.eve)
      ?~  pat  !!
      ?+  i.pat  !!
        ::
          %select-poke
        ?~  t.pat  !!
        =/  =num  (num (slav %ud i.t.pat))
        =/  pok  (get-poke-key num)
        =:  sel-poke-num  [~ num]
            poke-edit-mode  ?:(?=(~ pok) & |)
          ==
        [[(sail-card our.bowl) ~] this]
        ::
          %unselect-poke
        =:  sel-poke-num  ~
            poke-message  ~
            poke-edit-mode  |
          ==
        [[(sail-card our.bowl) ~] this]
        ::
          %edit-poke
        =:  poke-edit-mode  &
            poke-message  ~
          ==
        [[(sail-card our.bowl) ~] this]
        ::
          %send-poke
        ?~  sel-poke-num  [~ this]
        =/  pok=poke-key  (get-poke-key u.sel-poke-num)
        ?~  pok  [~ this]
        =.  poke-message  "+ Poke sent"
        :_  this
        :~  (sail-card our.bowl)
            [%pass /poke-key %agent [ship.pok agent.pok] %poke mark.pok data.pok]
        ==
        ::
      ==
      ::
        %form
      =/  pat=path  (stab id.eve)
      ?+  pat  !!
          [%poke-form ~]
        ?~  sel-poke-num  [~ this]
        =/  n=@t  (~(got by data.eve) '/poke-form/name')
        =/  s=@t  (~(got by data.eve) '/poke-form/ship')
        =/  a=@t  (~(got by data.eve) '/poke-form/agent')
        =/  m=@t  (~(got by data.eve) '/poke-form/mark')
        =/  d=@t  (~(got by data.eve) '/poke-form/data')
        =/  ag=@tas  =/(t (trip a) ?:(&(?=(^ t) =('%' i.t)) (crip t.t) a))
        =/  ma=@tas  =/(t (trip m) ?:(&(?=(^ t) =('%' i.t)) (crip t.t) m))
        =/  sh=(unit @p)  (slaw %p s)
        =/  da=(unit ^vase)
          =/  hoo=(unit hoon)  (rush d vest)
          ?~  hoo  ~
          [~ (slap !>(~) u.hoo)]
        ?~  sh
          =.  poke-message  "! Invalid ship"
          [[(sail-card our.bowl) ~] this]
        ?~  da
          =.  poke-message  "! Invalid poke data"
          [[(sail-card our.bowl) ~] this]
        =:  poke-keys  (put-poke-key u.sel-poke-num [n u.sh ag ma u.da d])
            poke-message  ~
            poke-edit-mode  |
          ==
        [[(sail-card our.bowl) ~] this]
      ==
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
++  root
  ^-  manx
  ;layer(w "100%", h "100%", px "5%", py "5%", cb black, cf orange, fl "column")
    ;layer
      ;box(w "47", px "2", pt "1", fl "column", fx "center", cb black, cf orange)
        ;border-left(b "heavy");
        ;border-right(b "heavy");
        ;border-top(b "heavy");
        ;border-bottom(b "blank", fx "center", cb cyan, cf black)
          ;box(mx "2")
            ;txt(mr "1"): kelvin:
            ;txt: 411
          ==
          ;box(mx "2")
            ;txt(mr "1"): homunculus:
            ;txt: [1 0 0]
          ==
        ==
        ;art
          ;+  ;/
            """
            ╭     ╮ ╭─────╮ ╭────╮  ──┬── ╭──┬──╮
            │     │ ├────┬╯ ├────┴╮   │      │   
            ╰─────╯ ╰    ╰─ ╰─────╯ ──┴──    ┴    
            """
        ==
        ;pattern(w "100%", h "1", mt "2", cf cyan): ⣶
      ==
    ==
    ;layer(fx "start", fy "80")
      ;box(w "40%", h "30%", cb cyan, cf black, fl "column")
        ;txt(w "100%", d "bold", fx "center"): Windows
        ;+  frame-display
      ==
    ==
    ;layer(fx "75", fy "75")
      ;box(w "25%", h "50%", cb cyan, cf black, fl "column")
        ;txt(w "100%", d "bold", fx "center"): Pokes
        ;+  ?~  sel-poke-num  poke-list  poke-view
      ==
    ==
  ==
::
++  frame-display
  ^-  manx
  ;scroll(w "100%", h "grow", b "arc", cb black, cf orange, fl "row")
    ;*  %+  turn  frames
      |=  =frame
      ^-  manx
      ;box(mx "2", my "1", cb "cyan")
        ;*  %+  turn  frame
          |=  =window
          ^-  manx
          ;box(mx "2", my "1", cb "blue"): {(trip agent.window)}
      ==
  ==
::
++  poke-list
  ^-  manx
  ;scroll(w "100%", h "grow", b "arc", cb black, cf orange)
    ;*  ^-  marl
    :~  (poke-item %1)
        (poke-item %2)
        (poke-item %3)
        (poke-item %4)
        (poke-item %5)
        (poke-item %6)
        (poke-item %7)
        (poke-item %8)
        (poke-item %9)
        (poke-item %0)
    ==
  ==
::
++  poke-item
  |=  =num
  ^-  manx
  =/  nut=tape  (trip (scot %ud ^-(@ num)))
  =/  pok=poke-key  (get-poke-key num)
  ;select(w "100%", cf ?~(pok "#658867" cyan), select-cb cyan, select-cf black)
    =id  "/select-poke/{nut}"
    ;+  ;/  "{nut} — {?~(pok "empty" (trip name.pok))}"
  ==
::
++  poke-view
  ^-  manx
  ;box(w "100%", fl "column")
    ;box(w "100%", px "1", fx "end")
      ;+  ?:  poke-edit-mode  ;null;
        ;select(cb "#FFFFFF", cf black, select-d "underline")
          =id  "/send-poke"
          ;+  ;/  "☞ send poke"
        ==
      ;+  ?:  poke-edit-mode  ;null;
        ;select(mx "1", cb "#FFFFFF", cf black, select-d "underline")
          =id  "/edit-poke"
          ;+  ;/  "▤ edit"
        ==
      ;select(cb "#FFFFFF", cf black, select-d "underline")
        =id  "/unselect-poke"
        ;+  ;/  "◀ back"
      ==
    ==
    ;+  ?~  poke-message  ;null;
      ;txt(w "100%", cb ?:(=('!' i.poke-message) "#c70042" cyan), fx "center")
        ;+  ;/  poke-message
      ==
    ;scroll(w "100%", h "grow", px "1", b "arc")
      ;+  ?:  poke-edit-mode  poke-form  poke-info
    ==
  ==
::
++  poke-info
  ^-  manx
  ?~  sel-poke-num  ;null;
  =/  sel-poke-key  (get-poke-key u.sel-poke-num)
  ;box(w "100%", fl "column")
    ;txt(d "bold"): Poke Name:
    ;txt: {?^(sel-poke-key (trip name.sel-poke-key) "")}
    ;txt(d "bold"): Ship:
    ;txt: {?^(sel-poke-key (trip (scot %p ship.sel-poke-key)) "")}
    ;txt(d "bold"): Agent:
    ;txt: {?^(sel-poke-key ['%' (trip agent.sel-poke-key)] "")}
    ;txt(d "bold"): Mark:
    ;txt: {?^(sel-poke-key ['%' (trip mark.sel-poke-key)] "")}
    ;txt(d "bold"): Data:
    ;txt: {?^(sel-poke-key (trip data-txt.sel-poke-key) "")}
  ==
::
++  poke-form
  ^-  manx
  ?~  sel-poke-num  ;null;
  =/  sel-poke-key  (get-poke-key u.sel-poke-num)
  ;form(w "100%", id "/poke-form")
    ;txt: Poke Name:
    ;input(w "100%", mb "1", id "/poke-form/name", default ?^(sel-poke-key (trip name.sel-poke-key) ""));
    ;txt: Ship:
    ;input(w "100%", id "/poke-form/ship", default ?^(sel-poke-key (trip (scot %p ship.sel-poke-key)) ""));
    ;txt: Agent:
    ;input(w "100%", id "/poke-form/agent", default ?^(sel-poke-key ['%' (trip agent.sel-poke-key)] ""));
    ;txt: Mark:
    ;input(w "100%", id "/poke-form/mark", default ?^(sel-poke-key ['%' (trip mark.sel-poke-key)] ""));
    ;txt: Data:
    ;input(w "100%", h "3", id "/poke-form/data", default ?^(sel-poke-key (trip data-txt.sel-poke-key) ""));
    ;submit(mt "1", cb "#FFFFFF", cf black, select-d "underline")
      ;+  ;/  "Save Poke"
    ==
  ==
::
::  ::  ::
++  get-poke-key
  |=  =num
  ^-  poke-key
  ?-  num
    %1  p1.poke-keys  %2  p2.poke-keys  %3  p3.poke-keys  %4  p4.poke-keys  %5  p5.poke-keys
    %6  p6.poke-keys  %7  p7.poke-keys  %8  p8.poke-keys  %9  p9.poke-keys  %0  p0.poke-keys
  ==
::
++  put-poke-key
  |=  [=num pok=poke-key]
  ^-  ^poke-keys
  ?-  num
    %1  poke-keys(p1 pok)  %2  poke-keys(p2 pok)  %3  poke-keys(p3 pok)
    %4  poke-keys(p4 pok)  %5  poke-keys(p5 pok)  %6  poke-keys(p6 pok)
    %7  poke-keys(p7 pok)  %8  poke-keys(p8 pok)  %9  poke-keys(p9 pok)
    %0  poke-keys(p0 pok)
  ==
::
++  pink  "#da4167"
++  red  "#e83313"
++  orange  "#fc8021"
++  light-green  "#5ae885"
++  green  "#0dc40a"
++  cyan  "#38d99b"
++  purple  "#8364cc"
++  white  "#ffffff"
++  black  "#000000"
::
++  sail-card
  |=  orb=@p  ^-  card  [%pass /homunculus %agent [orb %homunculus] %poke %umbra !>([~ root])]
::
--