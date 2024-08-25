/-  homunculus
|%
+$  window  term
+$  frame  (list window)
+$  frames  (list frame)
+$  poke-key
  $@  ~
  $:  name=cord  =ship
      agent=term  =mark
      data=vase  data-txt=cord
  ==
+$  poke-keys
  $:  p1=poke-key  p2=poke-key
      p3=poke-key  p4=poke-key
      p5=poke-key  p6=poke-key
      p7=poke-key  p8=poke-key
      p9=poke-key  p0=poke-key
  ==
+$  num  ?(%1 %2 %3 %4 %5 %6 %7 %8 %9 %0)
+$  state
  $:  base-kelvin=@
      active-window=window
      =frames
      move-mode=?(%char %full)
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
|_  bol=bowl:gall
+*  this  .
++  on-init
  ^-  (quip card _this)
  =.  base-kelvin
    =/  waf  .^(waft:clay %cx (en-beam [[our.bol %base [%da now.bol]] /sys/kelvin]))
    ?@  -.waf  num.waf
    (~(rep in p.waf) |=([i=weft a=$~(500 @)] (min a num.i)))
  =.  p1.poke-keys
    ['File Manager' our.bol %nox %open !>(~) '']
  [[~(render tui our.bol) ~] this]
++  on-save
  ^-  vase
  !>(~)
++  on-load
  |=  old=vase
  ^-  (quip card _this)
  =.  base-kelvin
    =/  waf  .^(waft:clay %cx (en-beam [[our.bol %base [%da now.bol]] /sys/kelvin]))
    ?@  -.waf  num.waf
    (~(rep in p.waf) |=([i=weft a=$~(500 @)] (min a num.i)))
  =.  p1.poke-keys
    ['File Manager' our.bol %nox %open !>(~) '']
  [[~(render tui our.bol) ~] this]
::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  
++  on-poke
  |=  [=mark =vase]
  ^-  (quip card _this)
  ?>  =(our.bol src.bol)
  ?+  mark  !!
    ::
      %refresh
    =/  dat  !<((pair window ^frames) vase)
    =:  active-window  p.dat
        frames         ?:(=(~[~] q.dat) ~ q.dat)
      ==
    [[~(render tui our.bol) ~] this]
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
      ?+  pat  !!
        ::
          [%change-window @ta ~]
        :_  this
        :~  :*  %pass  /homunculus  %agent  [our.bol %homunculus]
                %poke  %umbra  !>([%change [our.bol i.t.pat]])
        ==  ==
        ::
          [%close-window @ta ~]
        :_  this
        :~  :*  %pass  /homunculus  %agent  [our.bol %homunculus]
                %poke  %umbra  !>([%close [our.bol i.t.pat]])
        ==  ==
        ::
          [%move-window @ta @ta ~]
        :_  this
        :~  :*  %pass  /homunculus  %agent  [our.bol %homunculus]  %poke  %umbra  
                !>([%move [our.bol i.t.pat] [move-mode (?(%l %r %u %d) i.t.t.pat)]])
        ==  ==
        ::
          [%toggle-move-mode ~]
        =.  move-mode  ?:(?=(%full move-mode) %char %full)
        :_  this
        :~  ~(render tui our.bol)
        ==
        ::
          [%select-poke @ta ~]
        =/  =num  (num (slav %ud i.t.pat))
        =/  pok  (get-poke-key num)
        =:  sel-poke-num    [~ num]
            poke-edit-mode  ?:(?=(~ pok) & |)
          ==
        [[~(render tui our.bol) ~] this]
        ::
          [%unselect-poke ~]
        =:  sel-poke-num  ~
            poke-message  ~
            poke-edit-mode  |
          ==
        [[~(render tui our.bol) ~] this]
        ::
          [%edit-poke ~]
        =:  poke-edit-mode  &
            poke-message    ~
          ==
        [[~(render tui our.bol) ~] this]
        ::
          [%send-poke ~]
        ?~  sel-poke-num  [~ this]
        =/  pok=poke-key  (get-poke-key u.sel-poke-num)
        ?~  pok  [~ this]
        =.  poke-message  "+ Poke sent"
        :_  this
        :~  ~(render tui our.bol)
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
          ?:  =('' d)  [~ !>(~)]
          =/  hoo=(unit hoon)  (rush d vest)
          ?~  hoo  ~
          [~ (slap !>(~) u.hoo)]
        ?~  sh
          =.  poke-message  "! Invalid ship"
          [[~(render tui our.bol) ~] this]
        ?~  da
          =.  poke-message  "! Invalid poke data"
          [[~(render tui our.bol) ~] this]
        =:  poke-keys  (put-poke-key u.sel-poke-num [n u.sh ag ma u.da d])
            poke-message  ~
            poke-edit-mode  |
          ==
        [[~(render tui our.bol) ~] this]
      ==
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
--
::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  
|%
::
++  tui
  |_  orb=ship
  ::
  ++  render
    ^-  card
    [%pass /homunculus %agent [orb %homunculus] %poke %umbra !>([%session [~ root]])]
  ::
  ++  root
    ^-  manx
    ;layer(pl "10%", pt "10%", cb black, cf orange, fl "column")
      ;layer
        ;box(w "47", h "8", px "2", pt "1", fl "column", fx "center", cb black, cf orange)
          ;border-left(b "heavy");
          ;border-right(b "heavy");
          ;border-top(b "heavy");
          ;border-bottom(px "4", b "blank", cb cyan, cf black)
            ;txt
              ;+  ;/  (trip (scot %ud base-kelvin))
              ;+  ;/  "K"
            ==
            ;box(w "grow", h "1");
            ;txt(mr "1"): ship:
            ;txt: {(trip (scot %p orb))}
          ==
          ;art(fl "column")
            ;art(cf dark-orange)
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
            ;art(cf dark-orange)
              ;+  ;/
                """
                ╰─────╯ ╰    ╰─ ╰─────╯ ──┴──    ┴    
                """
            ==
          ==
          ;box(w "100%", h "1", mt "1")
            ;layer(fx "center")
              ;pattern(w "9", h "1", cf cyan): ▮
            ==
            ;layer(fx "center")
              ;pattern(w "13", h "1", cf "#34CB91"): ▮
            ==
            ;layer(fx "center")
              ;pattern(w "17", h "1", cf "#2DAE7C"): ▮
            ==
            ;layer(fx "center")
              ;pattern(w "21", h "1", cf "#248B63"): ▮
            ==
            ;layer(fx "center")
              ;pattern(w "25", h "1", cf "#1D6F4F"): ▮
            ==
            ;layer(fx "center")
              ;pattern(w "29", h "1", cf "#17593F"): ▮
            ==
            ;layer(fx "center")
              ;pattern(w "33", h "1", cf "#124732"): ▮
            ==
            ;layer(fx "center")
              ;pattern(w "37", h "1", cf "#0E3928"): ▮
            ==
            ;layer
              ;pattern(w "100%", h "1", cf "#0B2E20"): ▮
            ==
          ==
        ==
      ==
      ;layer(mt "12")
        ;box(w "47", h "10", fl "column")
          ;box(w "100%", h "1", d "bold", cb cyan, cf black, fx "center"): Windows
          ;+  ?^  frames  frame-display
            ;box(w "100%", h "grow", b "arc")
              ;layer(fx "center", fy "center")
                ;+  ;/  "No active windows"
              ==
              ;pattern(w "100%", h "100%"): ╱
            ==
        ==
      ==
      ;layer(ml "55", mt "5")
        ;box(w "30", h "17", cb cyan, cf black, fl "column")
          ;box(w "100%", h "1", d "bold", fx "center"): Pokes
          ;+  ?~  sel-poke-num  poke-list  poke-view
        ==
      ==
    ==
  ::
  ++  frame-display
    ^-  manx
    =/  act  (trip active-window)
    ;box(w "100%", h "grow")
      ;scroll(w "grow", h "100%", b "arc", cb black, cf orange, fl "row")
        ;*  %+  turn  frames
          |=  =frame
          ^-  manx
          ;box(mx "2", my "1", fl "column")
            ;*  %+  turn  frame
              |=  =window
              ^-  manx
              =/  win  (trip window)
              ;select(cf ?:(=(active-window window) white orange), select-d "bold")
                  =id  "/change-window/{win}"
                ;+  ;/  ['%' win]
              ==
          ==
      ==
      ;box(w "18", h "100%", b "heavy", cf cyan, fl "column")
        ;border-top(fx "center"): Active Window:
        ;txt(w "100%", cb "#1D6F4F", cf white, fx "center"): {['%' act]}
        ;line-h(l "heavy", cf "#1D6F4F");
        ;box(w "100%", h "3", fy "center")
          ;txt(ml "1", mr "2"): Move:
          ;box(w "5", h "3", fl "column", fx "center")
            ;select(select-cf white)
                =id  "/move-window/{act}/u"
              ;+  ;/  "▲"
            ==
            ;box
              ;select(select-cf white)
                  =id  "/move-window/{act}/l"
                ;+  ;/  "◀"
              ==
              ;+  ;/  ?:(?=(%full move-mode) "╺" "╶")
              ;select
                  =id  "/toggle-move-mode"
                ;+  ;/  ?:(?=(%full move-mode) "╋" "┼")
              ==
              ;+  ;/  ?:(?=(%full move-mode) "╸" "╴")
              ;select(select-cf white)
                  =id  "/move-window/{act}/r"
                ;+  ;/  "▶"
              ==
            ==
            ;select(select-cf white)
                =id  "/move-window/{act}/d"
              ;+  ;/  "▼"
            ==
          ==
        ==
        ;line-h(l "heavy", cf "#1D6F4F");
        ;select(ml "1", select-d "bold", select-cf "#c70042")
            =id  "/close-window/{act}"
          ;+  ;/   "Close ◉"
        ==
      ==
    ==
  ::
  ++  poke-list
    ^-  manx
    ;box(w "100%", h "grow", b "arc", cb black, cf orange, fl "column")
      ;*  ^-  marl
      :~  (poke-item %1)  (poke-item %2)
          (poke-item %3)  (poke-item %4)
          (poke-item %5)  (poke-item %6)
          (poke-item %7)  (poke-item %8)
          (poke-item %9)  (poke-item %0)
      ==
    ==
  ::
  ++  poke-item
    |=  =num
    ^-  manx
    =/  nut=tape  (trip (scot %ud ^-(@ num)))
    =/  pok=poke-key  (get-poke-key num)
    =/  cul=tape
      ?-  num
        %1  "#071e15"  %2  "#061b12"  %3  "#051810"
        %4  "#04150e"  %5  "#04120c"  %6  "#030f0a"
        %7  "#020c08"  %8  "#020906"  %9  "#010604"
        %0  "#000302"
      ==
    =/  sel=tape
      ?-  num
        %1  "#17593f"  %2  "#17593f"  %3  "#145038"
        %4  "#145038"  %5  "#124732"  %6  "#124732"
        %7  "#103e2c"  %8  "#103e2c"  %9  "#0d3525"
        %0  "#0d3525"
      ==
    ;select(w "100%", h "1", px "1")
        =id         "/select-poke/{nut}"
        =cb         cul
        =cf         ?~(pok "#658867" cyan)
        =select-cb  ?~(pok sel cyan)
        =select-cf  white
      ;txt(mr "1"): {nut}
      ;box(w "grow", h "1")
        ;layer(fx "center")
          ;txt(h "1", px "1"): {?~(pok "empty" (trip name.pok))}
        ==
        ;pattern(w "100%", h "1"): ┈
      ==
    ==
  ::
  ++  poke-view
    ^-  manx
    ;box(w "100%", h "grow", fl "column")
      ;box(w "100%", h "1", px "1")
        ;+  ?:  poke-edit-mode  ;null;
          ;select(cb white, cf black, select-cf orange, select-d "bold")
              =id  "/send-poke"
            ;+  ;/  "☛ send poke"
          ==
        ;box(w "grow", h "1");
        ;+  ?:  poke-edit-mode  ;null;
          ;select(mx "1", cb white, cf black, select-cf orange, select-d "bold")
              =id  "/edit-poke"
            ;+  ;/  "▤ edit"
          ==
        ;select(cb white, cf black, select-cf orange, select-d "bold")
            =id  "/unselect-poke"
          ;+  ;/  "◀ back"
        ==
      ==
      ;+  ?~  poke-message  ;null;
        ;txt(w "100%", h "1", cb ?:(=('!' i.poke-message) "#c70042" orange), fx "center")
          ;+  ;/  poke-message
        ==
      ;+  ?:  poke-edit-mode  poke-form  poke-info
    ==
  ::
  ++  poke-info
    ^-  manx
    ?~  sel-poke-num  ;null;
    =/  sel-poke-key  (get-poke-key u.sel-poke-num)
    ;box(w "100%", h "grow", b "arc", cf white, fl "column")
      ;box(w "100%", h "1", px "1")
        ;txt: Poke Name:
        ;box(w "grow", h "1");
        ;txt(d "bold"): {?^(sel-poke-key (trip name.sel-poke-key) "")}
      ==
      ;line-h;
      ;box(w "100%", h "1", px "1")
        ;txt: Ship:
        ;box(w "grow", h "1");
        ;txt(d "bold"): {?^(sel-poke-key (trip (scot %p ship.sel-poke-key)) "")}
      ==
      ;line-h;
      ;box(w "100%", h "1", px "1")
        ;txt: Agent:
        ;box(w "grow", h "1");
        ;txt(d "bold"): {?^(sel-poke-key ['%' (trip agent.sel-poke-key)] "")}
      ==
      ;line-h;
      ;box(w "100%", h "1", px "1")
        ;txt: Mark:
        ;box(w "grow", h "1");
        ;txt(d "bold"): {?^(sel-poke-key ['%' (trip mark.sel-poke-key)] "")}
      ==
      ;line-h;
      ;box(w "100%", px "1")
        ;txt: Data:
        ;box(w "grow", h "1");
        ;txt(w "60%", fx "end", d "bold"): {?^(sel-poke-key (trip data-txt.sel-poke-key) "")}
      ==
    ==
  ::
  ++  poke-form
    ^-  manx
    ?~  sel-poke-num  ;null;
    =/  sel-poke-key  (get-poke-key u.sel-poke-num)
    ;form(w "100%", h "grow", b "arc", cf white)
        =id  "/poke-form"
      ;box(w "100%", h "1", px "1")
        ;txt: Poke Name:
        ;box(w "grow", h "1");
        ;input
          =id  "/poke-form/name"
          =cb  "#1D6F4F"
          =cf  cyan
          =default  ?^(sel-poke-key (trip name.sel-poke-key) "")
          ;*  ~
        ==
      ==
      ;line-h;
      ;box(w "100%", h "1", px "1")
        ;txt: Ship:
        ;box(w "grow", h "1");
        ;input
          =id  "/poke-form/ship"
          =cb  "#1D6F4F"
          =cf  cyan
          =default  ?^(sel-poke-key (trip (scot %p ship.sel-poke-key)) "")
          ;*  ~
        ==
      ==
      ;line-h;
      ;box(w "100%", h "1", px "1")
        ;txt: Agent:
        ;box(w "grow", h "1");
        ;input
          =id  "/poke-form/agent"
          =cb  "#1D6F4F"
          =cf  cyan
          =default  ?^(sel-poke-key ['%' (trip agent.sel-poke-key)] "")
          ;*  ~
        ==
      ==
      ;line-h;
      ;box(w "100%", h "1", px "1")
        ;txt: Mark:
        ;box(w "grow", h "1");
        ;input
          =id  "/poke-form/mark"
          =cb  "#1D6F4F"
          =cf  cyan
          =default  ?^(sel-poke-key ['%' (trip mark.sel-poke-key)] "")
          ;*  ~
        ==
      ==
      ;line-h;
      ;box(w "100%", px "1")
        ;box(fl "column")
          ;txt: Data:
          ;submit(px "1", mt "1", cb "#FFFFFF", cf black, select-cf orange, select-d "bold")
            ;+  ;/  "+ Save Poke"
          ==
        ==
        ;box(w "grow", h "1");
        ;input
          =id  "/poke-form/data"
          =h   "3"
          =cb  "#1D6F4F"
          =cf  cyan
          =default  ?^(sel-poke-key (trip data-txt.sel-poke-key) "")
          ;*  ~
        ==
      ==
    ==
  ::
  ++  pink         "#da4167"
  ++  red          "#e83313"
  ++  orange       "#fc8021"
  ++  dark-orange  "#cc5a02"
  ++  light-green  "#5ae885"
  ++  green        "#0dc40a"
  ++  cyan         "#38d99b"
  ++  light-cyan   "#9effda"
  ++  purple       "#8364cc"
  ++  white        "#ffffff"
  ++  black        "#000000"
  ::
  --
::
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
--
