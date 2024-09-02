=>
|%
+$  state  %stateless
+$  cause
  $%  [%load ~]
      [%homunculus event]
  ==
+$  effect
  $%  [%homunculus p=manx]
  ==
::
+$  event
  $%  [%select =id]
      [%act =id]
      [%form =id data=(map id @t)]
      [%hotkey =id]
  ==
+$  id  @t
+$  hotkey
  $@  @t
  $%  [%delete ~]
      [%enter ~]
      [%back ~]
      [%tab ~]
      [%arrow ?(%l %r %u %d)]
  ==
::
+$  knob  [t=type f=nock]
++  moat  (keep state)
--
::
=<  $
~&  %homunculus
%-  moat
^-  fort:moat
|_  sat=state
::
++  load
  |=  arg=*
  ^-  [(list *) *]
  !!
::
++  peek
  |=  path=*
  ^-  (unit (unit *))
  !!
::
++  poke
  |=  [eny=@ our=@ux now=@da dat=*]
  ^-  (quip effect state)
  =/  coz=(unit cause)  ((soft cause) dat)
  ?~  coz  ~&(%malformed-cause !!)
  |^  ^-  (quip effect state)
  ?-  -.u.coz  ::  ~&(%unhandled-cause !!)
    ::
      %load
    :_  sat
    :~  [%homunculus root]
    ==
    ::
      %homunculus
    :_  sat
    ~
    ::
  ==
  ::
  ++  root
    ^-  manx
    ;box(w "100%", h "100%", px "4", py "2", cb black, cf white, b "arc", fx "center", fy "center", fl "column")
      ;select(w "100%", h "1", select-cb white, select-cf black): test 1
      ;select(w "100%", h "1", select-cb white, select-cf black): test 2
      ;select(w "100%", h "1", select-cb white, select-cf black): test 3
      ;select(w "100%", h "1", select-cb white, select-cf black): test 4
      ;select(w "100%", h "1", select-cb white, select-cf black): test 5
      ;select(w "100%", h "1", select-cb white, select-cf black): test 6
      ;select(w "100%", h "1", select-cb white, select-cf black): test 7
      ;select(w "100%", h "1", select-cb white, select-cf black): test 8
      ;select(w "100%", h "1", select-cb white, select-cf black): test 9
    ==
  ::
  ++  white  "#FFFFFF"
  ++  black  "#000000"
  ::
  --
::
--
