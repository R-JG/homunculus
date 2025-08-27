/-  mast, *example
^-  mast:mast
:-  ~
    :: :~  menu-mode+%homunculus-menu-mode
    ::     active-frame-index+%atom
    ::     frames+%homunculus-frames
    ::     bindings+%homunculus-bindings
    :: ==
|_  =hull:mast
::
+*  get-state
  ~
  :: :*
  :: !<  =menu-mode:homunculus            fil:(~(got by res.hull) %menu-mode)
  :: !<  active-frame-index=@             fil:(~(got by res.hull) %active-frame-index)
  :: !<  frames=(list layout:homunculus)  fil:(~(got by res.hull) %frames)
  :: !<  =bindings:homunculus             fil:(~(got by res.hull) %bindings)
  :: ==
::
++  spar
  |=  =crow:mast
  ^-  blow:mast
  =+  get-state
  =/  poe  `(pole @ta)`path.crow
  ?+  poe  !!
    ::
    ~  ~
    ::
  ==
::
++  sail
  =+  get-state
  |^
  ^-  manx
  ;row(w "100%", h "100%", bg "red", fx "center", fy "center")
    ;+  test
  ==
  ::
  ++  test
    ^-  manx
    ;col
      ;select(select-fg "cyan"):"test"
      ;select(select-fg "blue"):"test"
    ==
  ::
  --
::
--

