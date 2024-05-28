|%
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
  $:  sel-poke-num=(unit num)
      poke-message=tape
      poke-edit-mode=$~(| ?)
      =poke-keys
  ==
::
+$  event-data
  $%  [%select p=@t]
      [%act p=@t]
      [%form p=@t q=(map @t @t)]
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
      %vela
    [[(sail-card our.bowl) ~] this]
    ::
      %poke-key
    =/  nu=num  !<(num vase)
    =/  pok=poke-key  (get-poke-key nu)
    ?~  pok  [~ this]
    :_  this
    :~  [%pass /poke-key %agent [ship.pok agent.pok] %poke mark.pok data.pok]
    ==
    ::
      %homunculus
    =/  dat  !<(event-data vase)
    ?-  -.dat
      ::
        %select
      [~ this]
      ::
        %act
      =/  pat=path  (stab p.dat)
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
      =/  pat=path  (stab p.dat)
      ?+  pat  !!
          [%poke-form ~]
        ?~  sel-poke-num  [~ this]
        =/  n=@t  (~(got by q.dat) '/poke-form/name')
        =/  s=@t  (~(got by q.dat) '/poke-form/ship')
        =/  a=@t  (~(got by q.dat) '/poke-form/agent')
        =/  m=@t  (~(got by q.dat) '/poke-form/mark')
        =/  d=@t  (~(got by q.dat) '/poke-form/data')
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
  ;layer(w "100%", h "100%", cb "#000000", cf "#FFFFFF", fl "column")
    ;layer(h "30%", pt "1", fx "center", fy "center")
      ;asciiart(w "11", px "1", mr "2", cf red)
        ;+  ;/
          """
           ▄▀▀▄  ▄▀
          ▀    ▀▀
          """
      ==      
      ;asciiart(cf red)
        ;+  ;/
          """
          █ █ ██  ██  █ ███
          █ █ █ █ █ █ █  █
          █ █ █ █ █ █ █  █
          █ █ ██  ██  █  █
          █ █ █ █ █ █ █  █
          ███ █ █ █ █ █  █
           █  █ █ ██  █  █
          """
      ==
      ;box(w "11", ml "2", cf red, fl "column")
        ;txt: kelvin:
        ;txt(mb "1"): 411
        ;txt: homunculus:
        ;txt: [1 0 0]
      ==
    ==
    ;layer(fx "25", fy "75")
      ;box(w "25%", h "50%", cb red, fl "column")
        ;txt(w "100%", d "bold", fx "center"): Windows
      ==
    ==
    ;layer(fx "75", fy "75")
      ;box(w "25%", h "50%", cb red, fl "column")
        ;txt(w "100%", d "bold", fx "center"): Pokes
        ;+  ?~  sel-poke-num  poke-list  poke-view
      ==
    ==
  ==
::
++  poke-list
  ^-  manx
  ;scroll(w "100%", h "grow", b "arc")
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
  ;select(w "100%", cf ?~(pok "#FFABC0" "#FFFFFF"), select-cb "#FFFFFF", select-cf red)
    =id  "/select-poke/{nut}"
    ;+  ;/  "{nut} — {?~(pok "empty" (trip name.pok))}"
  ==
::
++  poke-view
  ^-  manx
  ;box(w "100%", fl "column")
    ;box(w "100%", px "1", fx "end")
      ;+  ?:  poke-edit-mode  ;null;
        ;select(cb "#FFFFFF", cf red, select-d "underline")
          =id  "/send-poke"
          ;+  ;/  "☞ send poke"
        ==
      ;+  ?:  poke-edit-mode  ;null;
        ;select(mx "1", cb "#FFFFFF", cf red, select-d "underline")
          =id  "/edit-poke"
          ;+  ;/  "▤ edit"
        ==
      ;select(cb "#FFFFFF", cf red, select-d "underline")
        =id  "/unselect-poke"
        ;+  ;/  "◀ back"
      ==
    ==
    ;+  ?~  poke-message  ;null;
      ;txt(w "100%", cb ?:(=('!' i.poke-message) "#c70042" red), fx "center")
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
    ;submit(mt "1", cb "#FFFFFF", cf red, select-d "underline")
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
++  red  "#DA4167"
::
++  sail-card
  |=  orb=@p  ^-  card  [%pass /homunculus %agent [orb %homunculus] %poke %umbra !>(root)]
::
--