|%
+$  component-event
  $:  =session-id
      =route
      com-key=(list @t)
      =path
      data=(map @t @t)
  ==
+$  event
  $%  [%act p=path]
      [%form p=path q=form-data]
      [%select p=path]
      [%keybinding p=path]
      [%scroll-trigger-up p=path]
      [%scroll-trigger-down p=path]
  ==
+$  form-data  (map path @t)
+$  keybindings  (list (pair keybinding path))
+$  keybinding
  $@  @t
  $%  [%delete ~]
      [%enter ~]
      [%back ~]
      [%tab ~]
      [%arrow ?(%l %r %u %d)]
  ==
+$  update  (lest update-card)
+$  update-card
  $%  [%element p=manx]
      :: [%set-select p=path]
      [%set-scroll-position p=?(%c %p) q=@ r=path]
  ==
+$  action
  $%  [%bind p=base-segment q=agent-name]
      [%route-request p=session-id q=route]
      [%route-respond p=session-id q=agent-name]
      [%route-missing p=session-id q=route]
  ==
+$  system-action
  $%  [%open-session p=ship q=route r=session-open]
      [%close-session p=session-id]
      [%change-frame p=frame-index]
  ==
+$  session-open
  $%  [%new-frame p=?(%l %r)]
      [%current-frame p=layout-dir q=layout-key]
  ==
+$  bindings  (map base-segment agent-name)
+$  agent-name  @tas
+$  base-segment  @t
+$  route  (trel base-segment path query)
+$  query  (map @t @t)
+$  session-id  @t  ::  @da
+$  frame-index  @
+$  frames  $~(~[*frame] (list frame))
+$  frame
  $:  =layout
  ==
+$  layout-dir  ?(%l %r %t %b %c)
+$  layout-key  (list ?(%0 %1))
+$  layout 
  $~  [%$ *session-id]
  $%  [%$ p=session-id]
      [%v p=@ l=layout r=layout]
      [%h p=@ t=layout b=layout]
  ==
::
  ::
::
++  parse-url
  |=  cod=cord
  |^  ^-  $@(~ [ship route])
  =/  tap  (trip cod)
  =/  sip  (find ['/' ~] tap)
  ?~  sip  ~
  =^  sap  tap  [(scag u.sip tap) (slag u.sip tap)]
  =/  who  (slaw %p (crip sap))
  ?~  who  ~
  =/  sep  (find ['?' ~] tap)
  =/  [paf=tape par=tape]
    ?~  sep  [tap ~]
    :-  (scag u.sep tap)  (slag u.sep tap)
  =/  puf  (parse-path paf)
  ?~  puf  ~
  =/  que  (parse-query par)
  ?~  que  ~
  ?~  u.puf
    :^  u.who
        %$
        ~
        u.que
  :^  u.who
      i.u.puf
      t.u.puf
      u.que
  ::
  ++  parse-path
    |=  tap=tape
    ^-  (unit path)
    %+  rust  tap
    %+  cook
      |=  p=path
      ^-  path
      ?.  .?(p)  ~
      ?.  =(%$ (rear p))  p
      %-  snip  p
    ;~  pfix  fas  (most fas url-segment)
    ==
  ++  url-segment
    %+  cook  |=(a=tape (rap 3 ^-((list @) a)))
    %-  star
    ;~  pose
        ;~(less fas prn)
        ;~(less tis prn)
        ;~(less pam prn)
        ;~(less mic prn)
        ;~(less wut prn)
    ==
  ++  parse-query
    |=  tap=tape
    ^-  (unit query)
    =;  quo  ?~(quo ~ [~ (malt u.quo)])
    ^-  (unit (list (pair @t @t)))
    %+  rust  tap
    ;~  pfix  wut  (most ;~(pose pam mic) query-param)
    ==
  ++  query-param
    %+  cook  |=([k=@t rest=(list @t)] [k ?~(rest '' ?>(?=(~ t.rest) i.rest))])
    %+  most  tis  url-segment
  --
::
++  print-url
  |=  [who=@p rut=route]
  ^-  tape
  %+  weld  (scow %p who)
  %+  weld  (trip p.rut)
  %+  weld  (spud q.rut)
  =/  qus  ~(tap by r.rut)
  ?~  qus  ~
  :-  '?'
  |-  ^-  tape
  %+  weld
    ?:  =('' q.i.qus)  (trip p.i.qus)
    %+  weld  (trip p.i.qus)
    :-  '='  (trip q.i.qus)
  ?~  t.qus  ~
  :-  '&'
  %=  $
    qus  t.qus
  ==
::
--

