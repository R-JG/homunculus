/-  *example
/+  mast
|%
+$  mode  ?(%test-1 %test-2)
+$  state-0
  $:  =mode
      =posts
      =user-sessions
  ==
+$  state-n
  $%  [%state-0 state-0]
  ==
+$  card  card:agent:gall
--
::
%-  mast
=|  state-n
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
  ^*  (quip card _this)
::
++  on-leave
  |=  =path
  ^*  (quip card _this)
::
++  on-peek
  |=  pax=path
  ^-  (unit (unit cage))
  =/  poe  `(pole @ta)`pax
  ?+  poe  ~
    ::
    [%x %mode ~]
      :+  ~  ~
      :-  %atom
      !>  mode
    ::
    [%x %posts ~]
      :+  ~  ~
      :-  %posts
      !>  (get-post-key-paths posts)
    ::
    [%x %post rest=^]
      |-  ^-  (unit (unit cage))
      =/  =post-id  (slav %da -.rest.poe)
      =/  =post-node  (~(got by posts) post-id)
      ?^  +.rest.poe
        %=  $
          posts  replies.post-node
          rest.poe  +.rest.poe
        ==
      :+  ~  ~
      :-  %post
      !>  [post.post-node (get-post-key-paths replies.post-node)]
    ::
  ==
::
++  on-agent  |=([wire sign:agent:gall] ^-((quip card _this) !!))
::
++  on-arvo
  |=  [=wire sign=sign-arvo]
  ^-  (quip card _this)
  =^  cards  state  abet:(arvo:cor wire sign)
  :-  cards  this
::
++  on-fail   |=([term tang] ^-((quip card _this) !!))
--
::
|_  [=bowl:gall cards=(list card)]
++  cor   .
++  abet  :-  (flop cards)  state
++  emit  |=  =card  cor(cards [card cards])
++  emil  |=  caz=(list card)  cor(cards (welp (flop caz) cards))
::
++  init
  ^+  cor
  %-  emit  tui-bind
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
++  arvo
  |=  [wir=(pole @ta) sin=sign-arvo]
  ^+  cor
  ~&  >  [wir -.sin]
  ?+  wir  !!
    [%clay-test ~]
      ?>  ?=([%clay %writ *] sin)
      ?~  p.sin
        ~&  >  'not found'
        cor
      ~&  >  p.r.u.p.sin
      ~&  >  q.r.u.p.sin
      cor
  ==
::
++  poke
  |=  [=mark =vase]
  ^+  cor
  ?+  mark  ~|(bad-poke/mark !!) 
    ::
      %clay-test
    =/  des=desk  %homunculus
    =/  pat=path  /desk/docket-0
    %-  emit  [%pass /clay-test %arvo %c %warp our.bowl des ~ %sing %x da+now.bowl pat]
    ::
      %test
    =.  mode  ?-(mode %test-2 %test-1, %test-1 %test-2)
    %-  emit  (make-fact-card /x/mode)
    ::
      %example-action
    =/  act  !<  action  vase
    ?-  -.act
      ::
        %put-post
      %+  put-post  post-at.act  content.act
      ::
        %del-post
      %-  del-post  at.act
      ::
    ==
    ::
  == 
::
++  put-post
  |=  [post-at=path dat=@t]
  ^+  cor
  =/  new-post=post  [src.bowl dat]
  =/  new-id=post-id  now.bowl
  =.  posts
    |-  ^-  ^posts
    ?~  post-at  (~(put by posts) new-id [new-post ~])
    =/  id  (slav %da i.post-at)
    =/  [poz=post rez=^posts]  (~(got by posts) id)
    %+  ~(put by posts)  id
    :-  poz
    %=  $
      post-at  t.post-at
      posts  rez
    ==
  %-  emit
  %-  make-fact-card  (weld /x/posts post-at)
::
++  del-post
  |=  at=path
  ^+  cor
  =.  posts
    |-  ^-  ^posts
    ?~  at  !!
    =/  id  (slav %da i.at)
    =/  [poz=post rez=^posts]  (~(got by posts) id)
    ?~  t.at
      ?>  ?|  =(author.poz src.bowl)
              =(src.bowl our.bowl)
          ==
      %-  ~(del by posts)  id
    %+  ~(put by posts)  id
    :-  poz
    %=  $
      at  t.at
      posts  rez
    ==
  %-  emit
  %-  make-fact-card  (weld /x/posts (snip at))
::
++  get-post-key-paths
  |=  poz=^posts
  ^-  (list path)
  %+  turn  (sort ~(tap in ~(key by poz)) lte)
  |=  k=post-id
  /[(scot %da k)]
::
++  make-fact-card
  |=  =path
  ^-  card
  :*  %give  %fact  ~[path]  %noun  !>(~)
  ==
::
++  tui-bind
  ^-  card
  :*  %pass  /tui-bind  %agent  [our.bowl %homunculus]  %poke
      %homunculus-action
      !>([%bind 'example' 'example' [%example-main ~ (malt [mode+/mode ~])]])
  ==
::
--

