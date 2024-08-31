=>
|%
+$  state  %stateless
+$  cause
  $%  [%test p=@t]
      [%rez p=@ud q=@ud]
  ==
+$  effect
  $%  [%text p=*]
      [%bel ~]
  ==
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
  ^-  (quip * state)
  =/  coz=(unit cause)  ((soft cause) dat)
  ?~  coz  ~&(%malformed-cause !!)
  |^  ^-  (quip * state)
  ?+  -.u.coz  ~&(%unhandled-cause !!)
    ::
      %test
    =-  [[%jam - ~] sat]
    (test p.u.coz)
    ::
  ==
  ::
  ++  test
    |=  [p=@t]
    ~&  >  p
    p
  ::
  --
::
--
