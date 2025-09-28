/-  *mast, homunculus
/~  deck  mast  /tui
::
:: O╭ ┌┬╮╭─╮╭─╮┌┬┐
:: ╭╯ │││├─┤╰─╮ │
:: ╯O ┴ ┴┴ ┴╰─╯ ┴
::
=>
|%
+$  state-0
  $:  swab=@da
      =gulf
  ==
+$  state-n
  $%  [%state-0 state-0]
  ==
+$  card  card:agent:gall
--
::
|=  you=agent:gall
=>  [[*$>(%state-0 state-n) you=you] +>]
=*  state  -
=<
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
::
|_  [=bowl:gall cards=(list card)]
++  cor   .
++  abet  [(flop cards) state]
++  emit  |=  =card  cor(cards [card cards])
++  emil  |=  caz=(list card)  cor(cards (welp (flop caz) cards))
++  our-ta  (scot %p our.bowl)
++  now-ta  (scot %da now.bowl)
++  bek  /[our-ta]/[q.byk.bowl]/[now-ta]
++  bak  |=  =desk  /[our-ta]/[desk]/[now-ta]
++  bem  |=  =path  (welp bek path)
++  bam  |=  [=desk =path]  (welp (bak desk) path)
::
:: ++  cleanup-timer
::   =/  tim  `@da`(add ~d1 now.bowl)
::   =/  wir  `wire`/mast/cleanup
::   =/  wab  swab
::   |%
::   ++  set
::     ^+  cor
::     =.  swab  tim
::     %-  emit  wait
::   ++  reset
::     ^+  cor
::     =.  swab  tim
::     %-  emil  [wait rest ~]
::   ++  rest  `card`[%pass wir %arvo %b %rest wab]
::   ++  wait  `card`[%pass wir %arvo %b %wait tim]
::   --
::
++  make-update-card
  |=  [src=ship ses=buoy dat=update:homunculus]
  ^-  card
  :: /mast/ship/session-id
  =/  paf  /mast/[(scot %p src)]/[ses]
  :*  %give  %fact  [paf ~]  %homunculus-update  !>(dat)
  ==
::
++  parse-url
  |=  cod=cord
  |^  ^-  (unit rope)
  =/  tap  (trip cod)
  =/  sep  (find ['?' ~] tap)
  =/  [paf=tape par=tape]
    ?~  sep  [tap ~]
    :-  (scag u.sep tap)  (slag u.sep tap)
  =/  puf  (parse-path paf)
  ?~  puf  ~
  ?~  u.puf
    :-  ~
    :+  %$
        ~
        (parse-query par)
  :-  ~
  :+  i.u.puf
      t.u.puf
      (parse-query par)
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
    ;~  pose  alf  nud  hep  dot  sig  cab  cen-encoded
    ==
  ++  cen-encoded
    %+  cook  |=(a=(list @) `@t`(rap 3 a))
    ;~  pfix  cen  (most cen mes)
    ==
  ++  parse-query
    |=  tap=tape
    ^-  quay
    %-  malt
    =<  ?~(. ~ u)
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
  |=  rop=rope
  ^-  cord
  %-  crip
  %+  weld  (trip bas.rop)
  %+  weld  (spud rut.rop)
  =/  qus  ~(tap by que.rop)
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
++  init
  ^+  cor
  =^  caz  you  ~(on-init you bowl)
  :: TODO:
  :: =.  cor  set:cleanup-timer
  %-  emil  caz
::
++  save
  ^-  vase
  !>  [%mast `state-n`-:state ~(on-save you bowl)]
::
++  load
  |=  vaz=vase
  ^+  cor
  ?.  ?=([%mast *] +.vaz)
    =^  caz  you  (~(on-load you bowl) vaz)
    %-  emil  caz
  =+  !<  [%mast maz=state-n nez=vase]  vaz
  =^  caz  you  (~(on-load you bowl) nez)
  =.  cor  (emil caz)
  ?-  -.maz
    ::
      %state-0
    ::=.  -.state  maz
    :: TODO:
    :: =.  cor  reset:cleanup-timer
    cor
    ::
  ==
::
++  peek
  |=  poe=(pole @ta)
  ^-  (unit (unit cage))
  %-  ~(on-peek you bowl)  poe
::
++  watch
  |=  poe=(pole @ta)
  ^+  cor
  :: /mast/ship/session-id
  ?.  ?=([%mast src=@t ses=@t ~] poe)
    ::
    =^  caz  you  (~(on-watch you bowl) poe)
    %-  emil  caz
    ::
  cor
::
++  leave
  |=  poe=(pole @ta)
  ^+  cor
  :: /mast/session-id
  ?.  ?=([%mast ses=@t ~] poe)
    ::
    =^  caz  you  (~(on-leave you bowl) poe)
    %-  emil  caz
    ::
  =/  tui-core  (tui-abed:tui [%| src.bowl ses.poe])
  =^  caz=(list card)  tui-core  tui-kill:tui-core
  =.  gulf  (~(del by gulf) [src.bowl ses.poe])
  %-  emil  caz
::
++  fail
  |=  [tem=term tan=tang]
  ^+  cor
  =^  caz  you  (~(on-fail you bowl) tem tan)
  %-  emil  caz
::
++  arvo
  |=  [wir=wire sin=sign-arvo]
  ^+  cor
  ?.  ?=([%mast *] wir)
    =^  caz  you  (~(on-arvo you bowl) wir sin)
    %-  emil  caz
  ?+  sin  cor
    ::
      [%behn %wake *]
    ?+  wir  cor
      ::
        [%mast %cleanup ~]
      cor
      :: TODO:
      :: =.  cor  set:cleanup-timer
      :: ?^  error.sin  cor
      :: =/  nel  .^(channel-state:eyre %e (bak %channel-state))
      :: =/  kil
      ::   %+  skip  ~(tap in ~(key by gulf))
      ::   |=  [s=ship b=buoy]
      ::   %-  ~(has by session.nel)  `@t`b
      :: %-  del-component-state  [%clean kil]
      ::
    ==
    ::
  ==
::
++  poke
  |=  [mak=mark vaz=vase]
  ^+  cor
  ?+  mak
    ::
    =^  caz  you  (~(on-poke you bowl) mak vaz)
    %-  emil  caz
    ::
      %mast-open
    :: open a session
    =/  opa  !<(mast-open:homunculus vaz)
    =/  tui-core  (tui-abed:tui [%& src.bowl session-id.opa route.opa])
    =^  [caz=(list card) sal=manx]  tui-core  tui-full:tui-core
    =.  gulf  tui-abet:tui-core
    %-  emil
    :-  (make-update-card src.bowl session-id.opa [[%element sal] ~])
        caz
    ::
      %mast-event
    :: apply an event poke to a component
    =/  eve  !<(mast-event:homunculus vaz)
    =^  caz  you
        %:  tui-event:(tui-abed:tui [%| src.bowl session-id.eve])
            com-key.eve
            path.eve
            data.eve
        ==
    %-  emil  caz
    ::
  ==
::
++  agent
  |=  [wir=(pole @ta) sin=sign:agent:gall]
  ^+  cor
  ?.  ?=([%mast *] wir)
    ::
    =^  caz  you  (~(on-agent you bowl) wir sin)
    %-  emil  caz
    ::
  ?+  wir  cor
    ::
      :: /mast/res/ship/session-id/res-jam/component-ancestry-keys...
      [%mast %res sip=@ta ses=@t res=@t ros=*]
    ?.  ?=(%fact -.sin)  cor
    =/  who  (slav %p sip.wir)
    =/  tui-core  (tui-abed:tui [%| who ses.wir])
    =^  [caz=(list card) sal=manx]  tui-core  (tui-update:tui-core ros.wir)
    =.  gulf  tui-abet:tui-core
    %-  emil
    :-  (make-update-card who ses.wir [[%element sal] ~])
        caz
    ::
  ==
::
++  tui
  |_  [src=ship ses=buoy rop=rope yel=isle]
  ++  tui-core  .
  ++  tui-abet  (~(put by gulf) [src ses] [rop yel])
  ++  tui-abed
    |=  %+  each
            [s=ship b=buoy r=rope]
            [s=ship b=buoy]
    ?-  +<-
    %&
        :: open a new session
        =/  bin
            ;;  (pair term line)
            .^  *  %gx  (bam %homunculus /binding/[bas.r.p]/noun)
            ==
        %_  tui-core
          src  s.p
          ses  b.p
          rop  r.p
          yel  [q.bin ~]
        ==
    %|
        :: get an existing session
        =/  q=[r=rope y=isle]  (~(got by gulf) [s.p b.p])
        %_  tui-core
          src  s.p
          ses  b.p
          rop  r.q
          yel  y.q
        ==
    ==
  ::
  ++  tui-full
    ^-  [[(list card) manx] _tui-core]
    :: the root component's key is null
    =/  key  ''
    =/  [yul=isle sal=manx]  (render key p.yel)
    :_  tui-core(yel yul)
    :_  sal
    %+  weld  (diff-resources [key ~] yel yul)
    %^  make-resource-subscription-cards
        %add
        [key ~]
        ~(val by res.p.yul)
  ::
  ++  tui-kill
    ^-  [(list card) _tui-core]
    =/  key  ''
    =/  kil  [p.yel ~]
    :_  tui-core(yel kil)
    %+  weld  (diff-resources [key ~] yel kil)
    %^  make-resource-subscription-cards
        %del
        [key ~]
        ~(val by res.p.yel)
  ::
  ++  tui-update
    |=  ros=(list rode)
    ^-  [[(list card) manx] _tui-core]
    =/  old  (get-isle ?>(?=(^ ros) ros))
    =/  [new=isle sal=manx]  (render (rear ros) p.old)
    :_  tui-core(yel (set-isle ?>(?=(^ ros) ros) new))
    :-  (diff-resources ros old new)
        sal
  ::
  ++  tui-event
    |=  [ros=(list rode) cro=crow]
    ^-  [(list card) _you]
    =/  yul  (get-isle ?>(?=(^ ros) ros))
    =/  com  (~(got by deck) com.p.yul)
    =/  blo  (~(spar com (make-hull boom.com p.yul)) cro)
    =/  caz  *(list card)
    =/  pof  ~(. you spoof-bowl)
    |-  ^+  [caz you]
    ?~  blo  [caz you]
    :: TODO:
    :: ?:  =(%mast-action p.i.blo)
    ::   %=  $
    ::     blo  t.blo
    ::     caz  [[%pass /mast/action %agent [our.bowl dap.bowl] %poke i.blo] caz]
    ::   ==
    =^  cuz  you  (on-poke:pof i.blo)
    %=  $
      blo  t.blo
      caz  (weld caz cuz)
    ==
  ::
  ++  get-isle
    |=  ros=(lest rode)
    ^-  isle
    ?~  t.ros  yel
    %=  $
        ros  t.ros
        yel  (~(got by q.yel) i.ros)
    ==
  ::
  ++  set-isle
    |=  [ros=(lest rode) yul=isle]
    ^-  isle
    ?~  t.ros  yul
    %_  yel
        q
        %+  ~(put by q.yel)  i.ros
        %=  $
            ros  t.ros
            yel  (~(got by q.yel) i.ros)
        ==
    ==
  ::
  ++  render
    |=  [rod=rode lin=line]
    ^-  [isle manx]
    :: rod is the key for the current component
    :: passed in on iteration of render
    =/  com  (~(got by deck) com.lin)
    =/  mal  `marl`[~(sail com (make-hull boom.com lin)) ~]
    =-  ?>  ?=(^ p)
        :-  [lin q]
        :: add this component's key to the root element as a %mast attribute
        %_  i.p
            a.g  [[%mast (trip rod)] a.g.i.p]
        ==
    |-  ^-  (pair marl (map rode isle))
    %^  spin
        mal
        *(map rode isle)
    |=  [m=manx a=(map rode isle)]
    =^  b  m
        ^-  [(map rode isle) manx]
        ?.  =(%mast n.g.m)
            =+  ^$(mal c.m)
            :-  q
                m(c p)
        =/  l  (parse-component-element m)
        :: make a key for the nested component
        :: using its parent's key and its line
        =/  k  (crip ((v-co:co 1) (mug [rod l])))
        =^  i  m
            %=  ^^$
                rod  k
                lin  l
            ==
        :-  [[k i] ~ ~]
            m
    :-  m
        (~(uni by a) b)
  ::
  ++  diff-resources
    |=  [unc=(list rode) old=isle new=isle]
    |^
    ^-  (list card)
    %+  weld  (del-or-add %del (~(dif by q.old) q.new))
    %+  weld  (del-or-add %add (~(dif by q.new) q.old))
    ^-  (list card)
    %-  ~(rep in ~(key by (~(int by q.old) q.new)))
    |=  [r=rode a=(list card)]
    ^-  (list card)
    %+  weld  a
    %=  ^$
        unc  (snoc unc r)
        old  (~(got by q.old) r)
        new  (~(got by q.new) r)
    ==
    ::
    ++  del-or-add
    |=  [wat=?(%del %add) dis=(map rode isle)]
    ^-  (list card)
    %-  ~(rep by dis)
    |=  [[k=rode v=isle] a=(list card)]
    =.  unc  (snoc unc k)
    %+  weld  a
    %+  weld
        %^  make-resource-subscription-cards
            wat
            unc
            ~(val by res.p.v)
    %=  ^$
        dis  q.v
    ==
    ::
    --
  ::
  ++  make-resource-subscription-cards
    |=  [act=?(%add %del) unc=(list rode) res=(list path)]
    %+  turn  res
    |=  paf=path
    ^-  card
    :: /mast/res/ship/session-id/res-jam/component-ancestry-keys...
    =/  wir  [%mast %res (scot %p src) ses (scot %ud (jam paf)) unc]
    ?-  act
        %add  [%pass wir %agent [our.bowl dap.bowl] %watch [%x paf]]
        %del  [%pass wir %agent [our.bowl dap.bowl] %leave ~]
    ==
  ::
  ++  parse-component-element
    |=  sal=manx
    ^-  line
    ?>  =(%mast n.g.sal)
    %+  roll  a.g.sal
    |=  [[k=mane v=tape] a=line]
    ?+  k  a
      [%hook @]  a(com +.k)
      [%gust @]  a(par (~(put by par.a) +.k (crip v)))
      [%gale @]  a(res (~(put by res.a) +.k (scan v stap)))
    ==
  ::
  ++  make-hull
    |=  [bom=boom lin=line]
    ^-  hull
    :*  our.bowl
        src
        ses
        bas.rop
        rut.rop
        que.rop
        now.bowl
        eny.bowl
        par.lin
        (hydrate-component bom res.lin)
    ==
  ::
  ++  hydrate-component
    |=  [bom=boom res=pool]
    ^-  gale
    :: load the nested agent with a modified bowl
    =/  pof  ~(. you spoof-bowl)
    %-  malt
    %+  murn  bom
    |=  [nam=@tas mak=@tas]
    ^-  (unit [term path vase])
    =/  paf  (~(get by res) nam)
    ?~  paf  ~
    =/  dat  (on-peek:pof [%x u.paf])
    ?:  ?|  ?=(~ dat)
            ?=(~ u.dat)
        ==
      ~&  >>>  [%mast dap.bowl %scry-failed `path`[%x u.paf]]
      ~
    ?:  =(p.u.u.dat mak)
      :-  ~
      :+  nam
          u.paf
          q.u.u.dat
    =/  tub  .^(tube:clay %cc (bem /[p.u.u.dat]/[mak]))
    :-  ~
    :+  nam
        u.paf
        (tub q.u.u.dat)
  ::
  ++  spoof-bowl
    %_  bowl
      src  src
      sap  /gall/[dap.bowl]
    ==
  ::
  --
::
--

