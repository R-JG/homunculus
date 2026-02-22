/-  homunculus
|%
::
+$  xeno-sources  (map desk (pair ship desk))
::
+$  place  $@(~ [=desk =path])
+$  tabs   (list place)
::
+$  remote-search
  $@  ~
  $:  who=ship
      res=(list desk)
  ==
::
+$  state-0
  $:  =xeno-sources
      =remote-search
      explorer=place
      editor-source=place
      editor-tabs=tabs
  ==
+$  state-n
  $%  [%0 state-0]
  ==
+$  card  card:agent:gall
--
::
=|  $>(%0 state-n)
=*  state  -
=>
::
|_  [bol=bowl:gall cards=(list card)]
++  cor   .
++  abet  :-  (flop cards)  state
++  emit  |=  =card  cor(cards [card cards])
++  emil  |=  caz=(list card)  cor(cards (welp (flop caz) cards))
::
++  poke
  |=  [mak=mark vaz=vase]
  ^+  cor
  ?+  mak  ~|(bad-poke/mak !!) 
  ::
      %xeno-pr
    =+  !<([from=desk into=desk] vaz)
    %-  emit
    %:  merge-pr
        src.bol
        from
        into
    ==
  ::
      %homunculus-event
    ?>  =(src our):bol
    =/  eve  !<(event:homunculus vaz)
    ?+  -.eve  !!
    ::
        %open
      %-  emil
      :~  render-full:tui
          register:tui
      ==
    ::
        %select
      cor
    ::
        %act
      ?+  p.eve  !!
      ::
          [%desks-item @ta ~]
        =.  explorer  [i.t.p.eve ~]
        %-  emil
        :~  render-explorer-panel:tui
        ==
      ::
          [%clone-desk-item @ta ~]
        ?>  ?=(^ remote-search)
        =*  des  i.t.p.eve
        %-  emit
        %:  clone-desk
            who.remote-search
            des
        ==
      ::
          [%upstream-desk-pr ~]
        ?>  ?=(^ explorer)
        =/  xeo  (~(got by xeno-sources) desk.explorer)
        =/  dat  [desk.explorer q.xeo]
        %-  emit
        :*  %pass  /upstream-desk-pr/[now-ta]  %agent  [p.xeo dap.bol]
            %poke  %xeno-pr  !>(dat)
        ==
      ::
          [%explorer-item *]
        ?~  t.p.eve  cor
        =/  next=place  [i.t.p.eve t.t.p.eve]
        ?:  (file-exists next)
          =:  editor-source  next
              editor-tabs    [next editor-tabs]
              explorer       ?~(explorer ~ explorer(path (snip path.explorer)))
            ==
          %-  emil
          :~  render-full:tui
          ==
        =.  explorer  next
        %-  emil
        :~  render-explorer-panel:tui
        ==
      ::
          [%explorer-back ~]
        ?:  =(~ explorer)  !!
        =.  explorer
          ?:  ?|  ?=(~ explorer)
                  ?=(~ path.explorer)
              ==
            ~
          %=  explorer
            path  (snip `path`path.explorer)
          ==
        %-  emil
        :~  render-explorer-panel:tui
        ==
      ::
          [%tab *]
        ?~  t.p.eve  !!
        =/  next=place  [i.t.p.eve t.t.p.eve]
        ?:  =(next editor-source)  cor
        =.  editor-source  next
        %-  emil
        :~  render-full:tui
        ==
      ::
      ==
    ::
        %form
      ?+  p.eve  !!
      ::
          [%remote-desks-form ~]
        =/  sif  (~(got by q.eve) /remote-source-input)
        =/  sip  (slav %p sif)
        %-  emit
        :*  %pass  /read-remote-desks/[sif]/[now-ta]  %agent  [sip dap.bol]
            %watch  /read-desks
        ==
      ::
          [%new-desk-form ~]
        =/  ned  (~(got by q.eve) /new-desk-input)
        %-  emil
        :~  (create-new-desk ned)
        ==
      ::
          [%new-file-form ~]
        ?>  ?=(^ explorer)
        =/  rel  (stab (~(got by q.eve) /new-file-input))
        =/  abs  (weld path.explorer rel)
        =/  dat  [%noun !>(~)]
        %-  emil
        :~  (write-file desk.explorer abs dat)
            (check-file-exists desk.explorer abs)
        ==
      ::
      ==
    ::
    ==
  ::
  == 
::
++  peek
  |=  poe=(pole @ta)
  ^-  (unit (unit cage))
  ~
::
++  watch
  |=  poe=(pole @ta)
  ^+  cor
  ?+  poe  !!
  ::
      [%read-desks ~]
    =/  dez  scry-desks
    %-  emil
    :~  [%give %fact ~ %xeno-desks !>(dez)]
        [%give %kick ~ ~]
    ==
  ::
  ==
::
++  leave
  |=  poe=(pole @ta)
  ^+  cor
  cor
::
++  fail
  |=  [tem=term tan=tang]
  ^+  cor
  cor
::
++  arvo
  |=  [wir=(pole @ta) sin=sign-arvo]
  ^+  cor
  ?+  wir  cor
  ::
      [%create-new-desk *]
    ?.  ?=([%clay %mere *] sin)  cor
    =/  pez  grant-test-perms
    %-  emil
    :*  render-explorer-panel:tui
        pez
    ==
  ::
      [%clone-desk who=@ta des=@ta *]
    ?.  ?=([%clay %mere *] sin)  cor
    =.  xeno-sources
      %+  ~(put by xeno-sources)
          des.wir
      :-  (slav %p who.wir)
          des.wir
    %-  emit
        render-explorer-panel:tui
  ::
      [%merge-pr *]
    %-  emit
        render-full:tui
  ::
      [%check-file-exists *]
    %-  emit
        render-explorer-panel:tui
    
  ::
  ==
::
++  agent
  |=  [wir=wire sin=sign:agent:gall]
  ^+  cor
  ?+  wir  cor
  ::
      [%read-remote-desks *]
    ?.  ?=(%fact -.sin)  cor
    ?+  p.cage.sin  !!
    ::
        %xeno-desks
      =/  dez  !<((list desk) q.cage.sin)
      =.  remote-search  [src.bol dez]
      %-  emil
      :~  render-explorer-panel:tui
      ==
    ::
    ==
  ::
  ==
::
  ::
::
++  our-ta  (scot %p our.bol)
++  now-ta  (scot %da now.bol)
++  bek  /[our-ta]/[q.byk.bol]/[now-ta]
++  bak  |=  =desk  /[our-ta]/[desk]/[now-ta]
++  bem  |=  =path  (welp bek path)
++  bam  |=  [=desk =path]  (welp (bak desk) path)
::
++  file-exists
  |=  at=place
  ^-  ?
  ?~  at  |
  .^  ?  %cu  (bam desk.at path.at)
  ==
::
++  scry-desks
  ^-  (list desk)
  %+  sort
    %~  tap  in
    .^  (set desk)  %cd  (bam %$ ~)
    ==
  aor
::
++  scry-explorer-list
  ^-  (list @t)
  ?~  explorer  ~
  %+  sort
    %~  tap  in
    %~  key  by
    =<  dir
    .^  arch  %cy  (bam desk.explorer path.explorer)
    ==
  aor
::
++  scry-kiln-sources
  .^  (map desk (pair ship desk))  %gx  (bam %hood /kiln/sources/noun)
  ==
::
++  grant-test-perms                :: NOTE: temporary: auto grant permissions to test ships
  ^-  (list card)
  =/  one  ~walnut-nidlep-sivrec
  =/  two  ~walrus-nidlep-sivrec
  ?.  |(=(one our.bol) =(two our.bol))  ~
  =/  you  ?:(=(one our.bol) two one)
  %+  turn  scry-desks
  |=  des=desk
  %:  grant-read-desk-permissions
      you
      des
  ==
::
++  grant-read-desk-permissions
  |=  [who=ship des=desk]
  ^-  card
  =/  pax  *path
  :*  %pass  /grant-read-desk-permissions  %arvo  %c
      %perm  des  pax
      [%r ~ %white (silt [%& who] ~)]
  ==
::
++  create-new-desk
  |=  new=desk
  ^-  card
  :*  %pass  /create-new-desk/[new]  %arvo  %c
      %merg  new
      our.bol  %base  da+now.bol
      %init
  ==
::
++  clone-desk
  |=  [who=ship des=desk]
  ^-  card
  :*  %pass  /clone-desk/[(scot %p who)]/[des]  %arvo  %c
      %merg  des
      who  des  da+now.bol
      %init
  ==
::
++  merge-pr
  |=  [who=ship from=desk into=desk]
  ^-  card
  :*  %pass  /merge-pr/[(scot %p who)]/[from]/[into]  %arvo  %c
      %merg  into
      who  from  da+now.bol
      %meld
  ==
::
++  write-file
  |=  [des=desk paf=path dat=cage]
  ^-  card
  :*  %pass  (weld /write-file/[des] paf)  %arvo  %c
      %info  des  %&   [[paf %ins dat] ~]
  ==
::
++  check-file-exists
  |=  [des=desk paf=path]
  ^-  card
  :*  %pass  (weld /check-file-exists/[des] paf)  %arvo  %c
      %warp  our.bol  des  ~
      %sing  %u  da+now.bol  paf
  ==
::
++  tui
  |%
  ::
  ++  render-full
    ^-  card
    %-  make-update-card
    :~  [%element root]
    ==
  ::
  ++  render-explorer-panel
    ^-  card
    %-  make-update-card
    :~  [%element desks-panel]
        [%element explorer-panel]
        [%set-scroll-position %c 0 /explorer-list]
    ==
  ::
  ++  make-update-card
    |=  upd=update:homunculus
    ^-  card
    :*  %pass  /homunculus  %agent  [our.bol %homunculus]
        %poke  %homunculus-update  !>(upd)
    ==
  ::
  ++  register
    ^-  card
    :*  %pass  /homunculus  %agent  [our.bol %homunculus]
        %poke  %homunculus-register  !>(~)
    ==
  ::
  ++  root
    ^-  manx
    ;row(w "100%", h "100%", bg dark-gray, fg light-blue-1)
      ;+  desks-panel
      ;+  explorer-panel
      ;+  editor-panel
    ==
  ::
  ++  desks-panel
    ^-  manx
    =/  kiln-sources  scry-kiln-sources
    ;col/"desks-panel"(w "35%", h "100%", bg dark-blue-2)
      ;scroll/"desks-list"(w "100%", h "grow")
        ;col/"remote-desks-list"(w "100%", b "arc")
          ;form/"remote-desks-form"(w "100%", fl "row")
            ;input/"remote-source-input"(w "grow", h "1");
            ;submit(select-fg light-blue-1): search
          ==
          ;*  ?~  remote-search  ~
              :+  ;row: {(scow %p who.remote-search)}
                  ;line-h;
              %+  turn  res.remote-search
              |=  des=desk
              ^-  manx
              =/  det  (trip des)
              ;row(w "100%")
                ;row(w "grow"): {det}
                ;select/"clone-desk-item/{det}"(select-fg light-blue-1): clone
              ==
        ==
        ;*  %+  turn  scry-desks
            |=  des=desk
            ^-  manx
            =/  det  (trip des)
            =/  sel
              ^-  ?
              ?~  explorer  |
              .=  desk.explorer
                  des
            ?:  (~(has by kiln-sources) des)
              =/  kin
                ^-  tape
                =/  sor  (~(get by kiln-sources) des)
                ?~  sor  "none"
                %+  weld
                    (scow %p p.u.sor)
                    "/{(trip q.u.sor)}"
              ;select/"desks-item/{det}"(w "100%", mb "1", px "1", fl "col")
              =bg  ?.(sel "#885053" "#FABC3C")
              =select-bg  "#FABC3C"
              =select-fg  dark-gray
                ;row: {det}
                ;row(w "100%", fg dark-gray)
                  ;row(w "grow"): kiln upstream:
                  ;row: {kin}
                ==
              ==
            =/  reo
              ^-  tape
              =/  sor  (~(get by xeno-sources) des)
              ?~  sor  "none"
              %+  weld
                  (scow %p p.u.sor)
                  "/{(trip q.u.sor)}"
            ;select/"desks-item/{det}"(w "100%", mb "1", px "1", fl "col")
            =bg  ?.(sel dark-blue-1 cyan-1)
            =select-bg  cyan-1
            =select-fg  dark-gray
              ;row: {det}
              ;row(w "100%", fg dark-gray)
                ;row(w "grow"): xeno upstream:
                ;row: {reo}
              ==
              ;select/"upstream-desk-pr"(bg orange): PR
            ==
      ==
      ;+  new-desk-form
    ==
  ::
  ++  new-desk-form
    ^-  manx
    ;form/"new-desk-form"(w "100%", fl "row", b "arc")
      ;input/"new-desk-input"(w "grow", h "1");
      ;submit(px "1", select-fg light-blue-1): new desk
    ==
  ::
  ++  explorer-panel
    ^-  manx
    ;col/"explorer-panel"(w "25%", h "100%", fx "center", bg dark-blue-2)
      ;row(w "100%", h "1", px "2", fg cyan-1)
        ;select/"explorer-back"(px "1", select-bg cyan-1, select-fg dark-gray):"◀"
        ;row(ml "2")
          ;+  ;/
            ?~  explorer  "~"
            (spud explorer)
        ==
      ==
      ;scroll/"explorer-list"(w "85%", h "grow")
        ;*  %+  turn  scry-explorer-list
            |=  i=@t
            ^-  manx
            =/  id=tape
              %+  weld  "explorer-item"
              ?~  explorer  "/{(trip i)}"
              (spud `path`explorer(path (snoc path.explorer i)))
            ;select/"{id}"(w "100%", px "1", mt "1", bg dark-blue-1, select-bg cyan-1, select-fg dark-gray):"{(trip i)}"
      ==
      ;+  new-file-form
    ==
  ::
  ++  new-file-form
    ^-  manx
    ;form/"new-file-form"(w "100%", fl "row", b "arc")
      ;input/"new-file-input"(w "grow", h "1");
      ;submit(px "1", select-fg light-blue-1): new file
    ==
  ::
  ++  editor-panel
    ^-  manx
    ;col(w "grow", h "100%", bg dark-blue-2)
      ;*  ?~  editor-source
            ;=  ;row(w "100%", h "1", fx "center", fg cyan-1):"~"
                ;col(w "100%", h "grow", pb "1", pr "2")
                  ;pattern(w "100%", h "100%", bg dark-gray, fg dark-blue-1):" □"
                ==
            ==
          =/  src  (spud editor-source)
          ;=  ;row(w "100%", h "1", px "2", fg cyan-1)
                ;*  %+  turn  editor-tabs
                    |=  i=place
                    ^-  manx
                    ?>  ?=(^ i)
                    =/  s  (spud i)
                    =/  is-active=?  =(i editor-source)
                    ;select/"tab{s}"(h "1", mx "1")
                      ;+  ?:  is-active
                            ;row(px "1", bg light-blue-1, fg blue-1):"{s}"
                          ;row(px "1", bg dark-blue-1, fg light-blue-1, select-fg white):"{s}"
                    ==
              ==
              ;col(w "100%", h "grow", pb "1", pr "2")
                ;editor/"{src}"(w "100%", h "100%", bg dark-gray, fg light-blue-2);
              ==
          ==
    ==
  ::
  ++  white         "#FFFFFF"
  ++  black         "#020202"
  ++  dark-gray     "#171C1D"
  ++  light-blue-1  "#51D6FF"
  ++  light-blue-2  "#A8E0FF"
  ++  blue-1        "#2B59C3"
  ++  dark-blue-1   "#12355B"
  ++  dark-blue-2   "#13293D"
  ++  cyan-1        "#bdfff6"
  ++  orange        "#FF570A"
  ::
  --
::
  ::
::
++  init
  ^+  cor
  =/  pez  grant-test-perms
  %-  emil
  :*  render-full:tui
      register:tui
      pez
  ==
::
++  save
  ^-  vase
  !>  ~
::
++  load
  |=  vaz=vase
  ^+  cor
  init
::
--
::
^-  agent:gall
|_  =bowl:gall
+*  this  .
    cor  ~(. +> [bowl ~])
::
++  on-init
  ^-  (quip card _this)
  =^  cards  state  abet:init:cor
  :-  cards  this
::
++  on-save
  ^-  vase
  =<  save  cor
::
++  on-load
  |=  =vase
  ^-  (quip card _this)
  =^  cards  state  abet:(load:cor vase)
  :-  cards  this
::
++  on-poke
  |=  [=mark =vase]
  ^-  (quip card _this)
  =^  cards  state  abet:(poke:cor mark vase)
  :-  cards  this
::
++  on-watch
  |=  =path
  ^-  (quip card _this)
  =^  cards  state  abet:(watch:cor path)
  :-  cards  this
::
++  on-leave
  |=  =path
  ^-  (quip card _this)
  =^  cards  state  abet:(leave:cor path)
  :-  cards  this
::
++  on-peek
  |=  =path
  ^-  (unit (unit cage))
  %-  peek:cor  path
::
++  on-agent
  |=  [=wire =sign:agent:gall]
  ^-  (quip card _this)
  =^  cards  state  abet:(agent:cor wire sign)
  :-  cards  this
::
++  on-arvo
  |=  [=wire sign=sign-arvo]
  ^-  (quip card _this)
  =^  cards  state  abet:(arvo:cor wire sign)
  :-  cards  this
::
++  on-fail
  |=  [=term =tang]
  ^-  (quip card _this)
  =^  cards  state  abet:(fail:cor term tang)
  :-  cards  this
--

