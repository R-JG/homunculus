/-  mast, homunculus
^-  mast:mast
:-  :~  menu-mode+%homunculus-menu-mode
        active-frame-index+%atom
        frames+%homunculus-frames
        bindings+%homunculus-bindings
    ==
|_  =hull:mast
::
+*  get-state
  :*
  !<  =menu-mode:homunculus            fil:(~(got by res.hull) %menu-mode)
  !<  active-frame-index=@             fil:(~(got by res.hull) %active-frame-index)
  !<  frames=(list layout:homunculus)  fil:(~(got by res.hull) %frames)
  !<  =bindings:homunculus             fil:(~(got by res.hull) %bindings)
  ==
::
++  spar
  |=  =crow:mast
  ^-  blow:mast
  =+  get-state
  =/  poe  `(pole @ta)`path.crow
  ?+  poe  !!
    ::
      [%form %browser-bar ~]
    =/  search  (~(got by data.crow) '/browser-input')
    =/  parsed  (parse-url:homunculus search)
    ?~  parsed  !!  :: TODO: set to fail mode
    =/  =layout:homunculus  (snag active-frame-index frames)
    ?:  ?&  ?=(%$ -.layout)
            ?=(%$ p.r.layout)
        ==
      :: if there is no other window open in this frame, open as the center window
      :~  [%homunculus-system-action !>(`system-action:homunculus`[%open-session p.parsed q.parsed [%current-frame %c ~]])]
      ==
    :: else set menu mode in order to select the window position
    :~  [%homunculus-system-action !>(`system-action:homunculus`[%set-menu-mode %open-session search])]
    ==
    ::
      [%act %open-session rest=*]
    ?>  ?=(%open-session -.menu-mode)
    =/  url  (parse-url:homunculus p.menu-mode)
    ?>  ?=(^ url)
    =/  ops
      ^-  session-open:homunculus
      ?+  rest.poe  !!
          [%new side=@ta ~]
        [%new-frame (?(%l %r) side.rest.poe)]
        ::
          [%current side=@ta key=@ta ~]
        [%current-frame (layout-dir:homunculus side.rest.poe) (layout-key:homunculus (cue (slav %ud key.rest.poe)))]
      ==
    :~  [%homunculus-system-action !>(`system-action:homunculus`[%open-session p.url q.url ops])]
        [%homunculus-system-action !>(`system-action:homunculus`[%set-menu-mode *menu-mode:homunculus])]
    ==
    ::
      [%act %close-session id=@ta ~]
    :~  [%homunculus-system-action !>(`system-action:homunculus`[%close-session id.poe])]
    ==
    ::
  ==
::
++  sail
  =+  get-state
  |^
  ^-  manx
  ;layer(fx "center", fy "center")
    ;col(w "82", h "28", px "2", py "1", fg white, bg green-4, b "heavy", b-fg green-2)
      ;+  header
      ;+  browser-bar
      ;row(w "100%", mb "1", bg green-3)
        ;+  ?.  ?=(%open-session -.menu-mode)
              ;row(w "1", h "1");
            ;select/"open-session/new/l"(w "1", h "3", pt "1", mt "5", bg green-1, fg green-3, select-bg cyan-2, select-fg white)
              ;+  ;/  "⢾"
            ==
        ;+  frame-container
        ;+  ?.  ?=(%open-session -.menu-mode)
              ;row(w "1", h "1");
            ;select/"open-session/new/r"(w "1", h "3", pt "1", mt "5", bg green-1, fg green-3, select-bg cyan-2, select-fg white)
              ;+  ;/  "⡷"
            ==
      ==
      ;+  frames-list
    ==
  ==
  ::
  ++  browser-bar
    ^-  manx
    ?:  ?=(%open-session -.menu-mode)
      ;row(w "100%", h "1", mt "2", fl "row", bg green-2, fg white)
        ;row(mx "2"):"Open:"
        ;row:"{(trip p.menu-mode)}"
      ==
    ;form/"browser-bar"(w "100%", h "1", mt "2", fl "row")
      ;input/"browser-input"(w "grow", h "1", bg white, fg green-4);
      ;submit(fg green-1, select-fg green-4, select-bg green-1):"⠒⠗"
    ==
  ::
  ++  frame-container
    ^-  manx
    =|  key=layout-key:homunculus
    =/  [wid=@ hei=@]       [51 11]
    =/  =layout:homunculus  (snag active-frame-index frames)
    =;  frame=manx
      ;col/"frame-container"(fg green-2, bg green-3, b "arc")
        =w  ((d-co:co 1) +(+(wid)))
        =h  ((d-co:co 1) +(+(hei)))
        ;+  frame
      ==
    ?:  ?&  ?=(%$ -.layout)
            ?=(%$ p.r.layout)
        ==
      ;row(w "100%", h "100%")
        ;layer(fx "center", fy "center")
          ;+  ;/  "No active windows"
        ==
        ;pattern(w "100%", h "100%"): ╱
      ==
    |-  ^-  manx
    =+  [w=((d-co:co 1) wid) h=((d-co:co 1) hei)]
    ?-  -.layout
        %$
      =/  k  (trip (scot %ud (jam (flop key))))
      ;row(w w, h h, fx "center", fy "center")
        ;*  ?:  ?=(%open-session -.menu-mode)  ~
            ;=  ;layer(fx "end")
                  ;select/"close-session/{(trip p.layout)}"(px "1", select-fg red):"✖"
                ==
            ==
        ;*  ?.  ?=(%open-session -.menu-mode)  ~
            ?:  ?=(%$ p.r.layout)  ~
            ;=  ;layer(py "1")
                  ;select/"open-session/current/l/{k}"(w "2", h "grow", select-fg cyan-1)
                    ;pattern(w "100%", h "100%"):"⣿"
                  ==
                ==
                ;layer(py "1", fx "end")
                  ;select/"open-session/current/r/{k}"(w "2", h "grow", select-fg cyan-1)
                    ;pattern(w "100%", h "100%"):"⣿"
                  ==
                ==
                ;layer(px "1")
                  ;select/"open-session/current/t/{k}"(w "grow", h "1", select-fg cyan-1)
                    ;pattern(w "100%", h "100%"):"⣿"
                  ==
                ==
                ;layer(px "1", fy "end")
                  ;select/"open-session/current/b/{k}"(w "grow", h "1", select-fg cyan-1)
                    ;pattern(w "100%", h "100%"):"⣿"
                  ==
                ==
            ==
        ;row(fg "white"):"{(print-url:homunculus q.layout r.layout)}"
      ==
        %v
      =/  lef  (div (mul p.layout wid) 100)
      =/  rig  (sub wid lef)
      =?  rig  !=(0 rig)  (dec rig)
      ;row(w w, h h)
        ;+  $(wid lef, layout l.layout, key [%0 key])
        ;line-v(fg green-1);
        ;+  $(wid rig, layout r.layout, key [%1 key])
      ==
        %h
      =/  top  (div (mul p.layout hei) 100)
      =/  bot  (sub hei top)
      =?  bot  !=(0 bot)  (dec bot)
      ;col(w w, h h)
        ;+  $(hei top, layout t.layout, key [%0 key])
        ;line-h(fg green-1);
        ;+  $(hei bot, layout b.layout, key [%1 key])
      ==
    ==
  ::
  ++  frames-list
    ^-  manx
    ;row/"frames-list"(w "100%", h "1")
      ;*  %+  spun  frames
          |=  [i=frame:homunculus a=@]
          ^-  [manx @]
          :_  +(a)
          =/  n=tape   (scow %ud a)
          =/  bg=tape  ?:(=(a active-frame-index) green-2 green-3)
          ;select/"frame/{n}"(w "7", h "1", mx "1", bg bg, fx "center", select-d "underline"):"{n}"
    ==
  ::
  ++  header
    ^-  manx
    ;row(w "100%", px "2", py "1", bg green-3)
      ;col
        ;art(fg green-2)
          ;+  ;/
            """
            ╭     ╮ ╭─────╮ ╭────╮  ──┬── ╭──┬──╮
            """
        ==
        ;art(fg green-1)
          ;+  ;/
            """
            │     │ ├────┬╯ ├────┴╮   │      │   
            """
        ==
        ;art(fg green-2)
          ;+  ;/
            """
            ╰─────╯ ╰    ╰─ ╰─────╯ ──┴──    ┴    
            """
        ==
      ==
      ;col(w "grow", fx "end", fg green-1)
        ;row:"{(trip (scot %p our.hull))}"
        ;line-h(w "3");
        ;row
          ;+  ;/  "410" :: TODO: scry
          ;+  ;/  "K"
        ==
      ==
    ==
  ::
  ++  black     "#000000"
  ++  red       "#A72608"
  ++  orange-1  "#fc8021"
  ++  orange-2  "#cc5a02"
  ++  green-1   "#0dc40a"
  ++  green-2   "#228721"
  ++  green-3   "#022801"
  ++  green-4   "#000f00"
  ++  cyan-1    "#9effda"
  ++  cyan-2    "#38d99b"
  ++  blue-1    "#4384bf"
  ++  blue-2    "#335C81"
  ++  blue-3    "#27394a"
  ++  white     "#F5EDF0"
  ::
  --
::
--

