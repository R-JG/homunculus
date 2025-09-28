/-  mast, *example
^-  mast:mast
:-  :~  mode+%atom
    ==
|_  =hull:mast
::
+*  get-state
  :*
  !<  mode=cord  fil:(~(got by res.hull) %mode)
  ==
::
++  spar
  |=  =crow:mast
  ^-  blow:mast
  =+  get-state
  =/  poe  `(pole @ta)`path.crow
  ?+  poe  ~
    ::
    [%act %clay-test ~]
      :~  [%clay-test !>(~)]
      ==
    ::
    [%act %toggle-mode ~]
      :~  [%test !>(~)]
      ==
    ::
  ==
::
++  sail
  =+  get-state
  |^
  ^-  manx
  ;col(w "100%", h "100%", bg "red", fx "center", fy "center")
    ;select/"toggle-mode"(select-d "underline"):"toggle mode"
    ;+  ?+  mode
            ;row(fg "yellow"):"nope"
          %test-1
            ;row(bg "black", fg "white"):"test 1"
          %test-2
            ;row(bg "white", fg "black"):"test 2"
        ==
    ;+  test
    ;select/"clay-test"(select-d "underline", bg "black", fg "cyan", m "2"):"CLAY"
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

