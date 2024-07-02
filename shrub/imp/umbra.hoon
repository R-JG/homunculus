/-  homunculus-api
/-  homunculus-umbra
=,  homunculus-umbra
=;  dor
^-  kook:neo
|%
++  state  pro/%umbra
++  poke   (sy %homunculus-to-umbra ~)
++  kids   *kids:neo
++  deps   *deps:neo
++  form
  ^-  form:neo
  |_  [=bowl:neo =aeon:neo =pail:neo]
  ++  init
    |=  pal=(unit pail:neo)
    ^-  (quip card:neo pail:neo)
    =+  ~(. dor *umbra-state)
    :_  umbra/!>(umbra-state)
    :~  ~(render tui our.bowl here.bowl)
    ==
  ++  poke
    |=  [sud=stud:neo vaz=vase]
    ^-  (quip card:neo pail:neo)
    =+  ~(. dor !<(umbra-state q.pail))
    ?+  sud  !!
      ::
        %homunculus-to-umbra
      =/  umb  !<(to-umbra vaz)
      ?-  -.umb
        ::
          %refresh
        =:  active-window.umbra-state  p.umb
            frames.umbra-state  ?:(=(~[~] q.umb) ~ q.umb)
          ==
        :_  umbra/!>(umbra-state)
        :~  ~(render tui our.bowl here.bowl)
        ==
        ::
          %poke-key
        !!
        ::
      ==
      ::
        %homunculus-event
      =/  eve  !<(event.homunculus-api vaz)
      ?+  -.eve  !!
        ::
          %act
        =/  pat=path  (stab id.eve)
        ?+  pat  !!
          ::
            [%change-window @ta ~]
          :_  umbra/!>(umbra-state)
          :~  [#/[p/our.bowl]/homunculus %poke homunculus-from-umbra/!>([%change ~])]   :: NEED PITH
          ==
          ::
            [%close-window @ta ~]
          :_  umbra/!>(umbra-state)
          :~  [#/[p/our.bowl]/homunculus %poke homunculus-from-umbra/!>([%close ~])] :: NEED PITH
          ==
          ::
            [%move-window @ta @ta ~]
          :_  umbra/!>(umbra-state)
          :~  :*  #/[p/our.bowl]/homunculus
                  %poke  %homunculus-from-umbra
                  !>  :+  %move
                    ~                                                  :: NEED PITH
                  [move-mode.umbra-state (?(%l %r %u %d) i.t.t.pat)]
          ==  ==
          ::
            [%toggle-move-mode ~]
          =.  move-mode.umbra-state  ?:(?=(%full move-mode.umbra-state) %char %full)
          :_  umbra/!>(umbra-state)
          :~  ~(render tui our.bowl here.bowl)
          ==
          ::
            [%select-poke @ta ~]
          =/  =num  (num (slav %ud i.t.pat))
          =/  pok  (get-poke-key num)
          =:  sel-poke-num.umbra-state    [~ num]
              poke-edit-mode.umbra-state  ?:(?=(~ pok) & |)
            ==
          :_  umbra/!>(umbra-state)
          :~  ~(render tui our.bowl here.bowl)
          ==
          ::
            [%unselect-poke ~]
          =:  sel-poke-num.umbra-state    ~
              poke-message.umbra-state    ~
              poke-edit-mode.umbra-state  |
            ==
          :_  umbra/!>(umbra-state)
          :~  ~(render tui our.bowl here.bowl)
          ==
          ::
            [%edit-poke ~]
          =:  poke-edit-mode.umbra-state  &
              poke-message.umbra-state    ~
            ==
          :_  umbra/!>(umbra-state)
          :~  ~(render tui our.bowl here.bowl)
          ==
          ::
            [%send-poke ~]
          ?~  sel-poke-num.umbra-state  !!
          =/  pok=poke-key  (get-poke-key u.sel-poke-num.umbra-state)
          ?~  pok  !!
          =.  poke-message.umbra-state  "+ Poke sent"
          :_  umbra/!>(umbra-state)
          :~  ~(render tui our.bowl here.bowl)                 :: ADD POKE CARD
          ==
          ::
        ==
        ::
          %form
        =/  pat=path  (stab id.eve)
        ?+  pat  !!
          ::
            [%poke-form ~]
          ?~  sel-poke-num.umbra-state  !!
          =/  n=@t  (~(got by data.eve) '/poke-form/name')
          =/  s=@t  (~(got by data.eve) '/poke-form/ship')
          =/  a=@t  (~(got by data.eve) '/poke-form/agent')
          =/  m=@t  (~(got by data.eve) '/poke-form/mark')
          =/  d=@t  (~(got by data.eve) '/poke-form/data')
          =/  ag=@tas  =/(t (trip a) ?:(&(?=(^ t) =('%' i.t)) (crip t.t) a))
          =/  ma=@tas  =/(t (trip m) ?:(&(?=(^ t) =('%' i.t)) (crip t.t) m))
          =/  sh=(unit @p)  (slaw %p s)
          =/  da=(unit vase)
            ?:  =('' d)  [~ !>(~)]
            =/  hoo=(unit hoon)  (rush d vest)
            ?~  hoo  ~
            [~ (slap !>(~) u.hoo)]
          ?~  sh
            =.  poke-message.umbra-state  "! Invalid ship"
            :_  umbra/!>(umbra-state)
            :~  ~(render tui our.bowl here.bowl)
            ==
          ?~  da
            =.  poke-message.umbra-state  "! Invalid poke data"
            :_  umbra/!>(umbra-state)
            :~  ~(render tui our.bowl here.bowl)
            ==
          =:  poke-message.umbra-state    ~
              poke-edit-mode.umbra-state  |
              poke-keys.umbra-state
                (put-poke-key u.sel-poke-num.umbra-state [n u.sh ag ma u.da d])
            ==
          :_  umbra/!>(umbra-state)
          :~  ~(render tui our.bowl here.bowl)
          ==
          ::
        ==
      ==
      ::
    ==
  --
--
::
|_  =umbra-state
++  tui
  |_  [orb=@p here=pith:neo]  :: FIX HERE
  ::
  ++  render
    ^-  card:neo
    [#/[p/orb]/homunculus %poke homunculus-from-umbra/!>([%session ~ root])]
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
              ;+  ;/  (trip (scot %ud base-kelvin.umbra-state))
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
          ;+  ?^  frames.umbra-state  frame-display
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
          ;+  ?~  sel-poke-num.umbra-state  poke-list  poke-view
        ==
      ==
    ==
  ::
  ++  frame-display
    ^-  manx
    =/  act  ""  :: (trip active-window.umbra-state)
    ;box(w "100%", h "grow")
      ;scroll(w "grow", h "100%", b "arc", cb black, cf orange, fl "row")
        ;*  %+  turn  frames.umbra-state
          |=  =frame
          ^-  manx
          ;box(mx "2", my "1", fl "column")
            ;*  %+  turn  frame
              |=  =window
              ^-  manx
              =/  win  ""  :: (trip window)
              ;select(cf ?:(=(active-window.umbra-state window) white orange), select-d "bold")
                  =id  "/change-window/{win}"  :: NEED TO SEND PITH
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
              ;+  ;/  ?:(?=(%full move-mode.umbra-state) "╺" "╶")
              ;select
                  =id  "/toggle-move-mode"
                ;+  ;/  ?:(?=(%full move-mode.umbra-state) "╋" "┼")
              ==
              ;+  ;/  ?:(?=(%full move-mode.umbra-state) "╸" "╴")
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
        =cb         cul
        =cf         ?~(pok "#658867" cyan)
        =select-cb  ?~(pok sel cyan)
        =select-cf  white
        =id         "/select-poke/{nut}"
      ;txt(mr "1"): {nut}
      ;pattern(w "7", h "1"): ┈
      ;txt(mx "1"): {?~(pok "empty" (trip name.pok))}
      ;pattern(w "grow", h "1"): ┈
    ==
  ::
  ++  poke-view
    ^-  manx
    ;box(w "100%", h "grow", fl "column")
      ;box(w "100%", h "1", px "1")
        ;+  ?:  poke-edit-mode.umbra-state  ;null;
          ;select(cb white, cf black, select-cf orange, select-d "bold")
              =id  "/send-poke"
            ;+  ;/  "☛ send poke"
          ==
        ;box(w "grow", h "1");
        ;+  ?:  poke-edit-mode.umbra-state  ;null;
          ;select(mx "1", cb white, cf black, select-cf orange, select-d "bold")
              =id  "/edit-poke"
            ;+  ;/  "▤ edit"
          ==
        ;select(cb white, cf black, select-cf orange, select-d "bold")
            =id  "/unselect-poke"
          ;+  ;/  "◀ back"
        ==
      ==
      ;+  ?~  poke-message.umbra-state  ;null;
        ;txt(w "100%", h "1", cb ?:(=('!' i.poke-message.umbra-state) "#c70042" orange), fx "center")
          ;+  ;/  poke-message.umbra-state
        ==
      ;+  ?:  poke-edit-mode.umbra-state  poke-form  poke-info
    ==
  ::
  ++  poke-info
    ^-  manx
    ?~  sel-poke-num.umbra-state  ;null;
    =/  sel-poke-key  (get-poke-key u.sel-poke-num.umbra-state)
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
    ?~  sel-poke-num.umbra-state  ;null;
    =/  sel-poke-key  (get-poke-key u.sel-poke-num.umbra-state)
    ;form(w "100%", h "grow", b "arc", cf white)
        =id  "/poke-form"
      ;box(w "100%", h "1", px "1")
        ;txt: Poke Name:
        ;box(w "grow", h "1");
        ;input(cb white, cf black, id "/poke-form/name", default ?^(sel-poke-key (trip name.sel-poke-key) ""));
      ==
      ;line-h;
      ;box(w "100%", h "1", px "1")
        ;txt: Ship:
        ;box(w "grow", h "1");
        ;input(cb white, cf black, id "/poke-form/ship", default ?^(sel-poke-key (trip (scot %p ship.sel-poke-key)) ""));
      ==
      ;line-h;
      ;box(w "100%", h "1", px "1")
        ;txt: Agent:
        ;box(w "grow", h "1");
        ;input(cb white, cf black, id "/poke-form/agent", default ?^(sel-poke-key ['%' (trip agent.sel-poke-key)] ""));
      ==
      ;line-h;
      ;box(w "100%", h "1", px "1")
        ;txt: Mark:
        ;box(w "grow", h "1");
        ;input(cb white, cf black, id "/poke-form/mark", default ?^(sel-poke-key ['%' (trip mark.sel-poke-key)] ""));
      ==
      ;line-h;
      ;box(w "100%", px "1")
        ;box(fl "column")
          ;txt: Data:
          ;submit(px "1", mt "1", cb "#FFFFFF", cf black, select-cf orange, select-d "bold"): + Save Poke
        ==
        ;box(w "grow", h "1");
        ;input(h "3", cb white, cf black, id "/poke-form/data", default ?^(sel-poke-key (trip data-txt.sel-poke-key) ""));
      ==
    ==
  ::
  ++  pink  "#da4167"
  ++  red  "#e83313"
  ++  orange  "#fc8021"
  ++  dark-orange  "#cc5a02"
  ++  light-green  "#5ae885"
  ++  green  "#0dc40a"
  ++  cyan  "#38d99b"
  ++  purple  "#8364cc"
  ++  white  "#ffffff"
  ++  black  "#000000"
  ::
  --
::
++  get-poke-key
  |=  =num
  ^-  poke-key
  ?-  num
    %1  p1.poke-keys.umbra-state  %2  p2.poke-keys.umbra-state
    %3  p3.poke-keys.umbra-state  %4  p4.poke-keys.umbra-state
    %5  p5.poke-keys.umbra-state  %6  p6.poke-keys.umbra-state
    %7  p7.poke-keys.umbra-state  %8  p8.poke-keys.umbra-state
    %9  p9.poke-keys.umbra-state  %0  p0.poke-keys.umbra-state
  ==
::
++  put-poke-key
  |=  [=num pok=poke-key]
  ^-  poke-keys
  ?-  num
    %1  poke-keys.umbra-state(p1 pok)  %2  poke-keys.umbra-state(p2 pok)
    %3  poke-keys.umbra-state(p3 pok)  %4  poke-keys.umbra-state(p4 pok)
    %5  poke-keys.umbra-state(p5 pok)  %6  poke-keys.umbra-state(p6 pok)
    %7  poke-keys.umbra-state(p7 pok)  %8  poke-keys.umbra-state(p8 pok)
    %9  poke-keys.umbra-state(p9 pok)  %0  poke-keys.umbra-state(p0 pok)
  ==
::
--
