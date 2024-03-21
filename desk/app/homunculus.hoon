::
::  O┬┬ ┬┌─┐┌┬┐┬ ┬┌┐┌┌─┐┬ ┬┬  ┬ ┬┌─┐
::  ┌┘├─┤│ │││││ │││││  │ ││  │ │└─┐
::  ┴O┴ ┴└─┘┴ ┴└─┘┘└┘└─┘└─┘┴─┘└─┘└─┘
::
|%
+$  ens   (map rami [=res =visa =lar =modi =iter =aves =ars])
+$  rami  (lest [=axis =ager])
+$  res
  $:  size=[w=@ud h=@ud]
      padd=[l=@ud r=@ud t=@ud b=@ud]
      marg=[l=@ud r=@ud t=@ud b=@ud]
      flex=[x=@ud y=@ud]
      flow=[d=?(%col %row) b=?(%wrap %clip)]
      look=fila
  ==
+$  visa  (map loci [=fila @c ~])
+$  loci  [x=@ud y=@ud]
+$  lar   loci
+$  modi  loci
+$  axis  ?(%b %l %~)
+$  ager  @ud
+$  aves  (map ?(%sel %act) @t)
+$  iter  [x=@ud y=@ud]
+$  vox   tape
+$  fila  [d=(set deco) b=tint f=tint]
+$  acia  [d=(unit (set deco)) b=(unit tint) f=(unit tint)]
+$  ad    ?(%left %right %top %bottom)
+$  pila  ?(%light %heavy %double %blank %~)
+$  muri  [l=@ud r=@ud t=@ud b=@ud]
+$  sola  [x=@ud y=@ud]
+$  ars
  $%  [%text =vox]  [%layer ~]  [%border =ad =pila]
      [%select =acia]  [%input i=@ud ab=@ud =vox]
      [%scroll =iter =muri =sola]  [%$ ~]
  ==
::
+$  omen  (map nota lex)
+$  zona  dill-belt:dill
+$  nota
  $%  [%mod ?(%ctl %met %hyp) bolt:dill]  [%aro ?(%d %l %r %u)]
      [%txt ~]  [%bac ~]  [%hit ~]  [%ret ~]
  ==
+$  lex
  $?  %nav-l  %nav-r  %nav-u  %nav-d
      %cur-l  %cur-r  %cur-u  %cur-d
      %scr-l  %scr-r  %scr-u  %scr-d
      %inp  %del  %tog  %act  %def
  ==
+$  mus   (map loci rami)
+$  equi  (set rami)
+$  dux   [l=@ud r=@ud t=@ud b=@ud k=rami]
+$  rex   $@(~ dux)
+$  ordo  (list dux)
+$  gens  (map @t rami)
::
+$  opus  [=ens =visa]
+$  cura  [=omen =equi =ordo =rex]
+$  as    $%((pair %c @ud) (pair %p @ud) (pair %i @ud))
+$  rei
  $:  size=[w=as h=as]
      padd=[l=as r=as t=as b=as]
      marg=[l=as r=as t=as b=as]
      =flex
      =flow
      look=acia
  ==
+$  lux   blit:dill
+$  flex  [x=@ud y=@ud]
+$  flow  [d=?(%col %row) b=?(%wrap %clip)]
::
+$  vela  manx
+$  urbs  $~([50 25] [x=@ud y=@ud])
+$  arx   [x=@ud y=@ud]
+$  ara
  $:  =ens  =visa  =vela
      =omen  =rex  =ordo
      =equi  =gens  =arx
      =lar
  ==
+$  arae  (map @tas ara)
+$  ego
  $:  =urbs  =ara
  ==
::
+$  card  card:agent:gall
--
::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  
=|  ego
=*  ego  -
^-  agent:gall
=<
|_  =bowl:gall
+*  hoc  .
++  on-init
  ^-  (quip card _hoc)
  [~ hoc(ego *^ego)]
++  on-save
  ^-  vase
  !>(urbs)
++  on-load
  |=  old=vase
  ^-  (quip card _hoc)
  =/  ol  !<(^urbs old)
  =/  term=^urbs  ?:(=([0 0] ol) [75 25] ol)
  [~ hoc(ego =+(*^ego -(urbs term, arx.ara term)))]
::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  
++  on-poke
  |=  [=mark =vase]
  ^-  (quip card _hoc)
  ?+  mark  ~|("homunculus poke failed with mark: {<mark>}" !!)
    ::
      %noun
    :_  hoc
    :~  [%pass /get-terminal %arvo %d %open %homunculus ~]
        [%pass /get-terminal %arvo %d %flee ~]
    ==
    ::
      %elem
    ~&  >  'element received'
    =/  elem=manx  !<(manx vase)
    =/  gen=opus  (geno elem)
    =/  dic=cura  (dico ens.gen)
    :-  ~
    %=  hoc
      ens.ara   ens.gen
      visa.ara  visa.gen
      vela.ara  elem
      omen.ara  omen.dic
      ordo.ara  ordo.dic
      equi.ara  equi.dic
      arx.ara   urbs
      lar.ara   [0 0]
    ==
    ::
      %dill-poke
    =/  z=zona  +:!<(poke:dill vase)
    ::
    ?:  ?=(%del -.z)                        ::  press the del key to exit the tui
      :_  hoc
      :~  [%pass /give-terminal %arvo %d %open %hood ~]
          [%pass /give-terminal %arvo %d %flee ~]
      ==
    ::
    ?:  ?=(%rez -.z)                        ::  resize urbs to new terminal dimensions
      =/  gen=opus  (geno vela.ara)
      =/  dic=cura  (dico ens.gen)
      =/  =lux  (put-blit visa.gen)
      :_  hoc(urbs [(@ -.+.z) (@ +.+.z)], ara ara(ens ens.gen, visa visa.gen, ordo ordo.dic, arx [(@ -.+.z) (@ +.+.z)]))
      :~  [%give %fact ~[/dill/$] %dill-blit !>(lux)]
      ==
    ::
    ?:  ?|  ?=(%mod -.z)  ?=(%aro -.z)  ?=(%txt -.z)
            ?=(%bac -.z)  ?=(%ret -.z)  ?=(%hit -.z)
        ==
      =/  n=nota  ?:(?=(%txt -.z) [-.z ~] ?:(?=(%hit -.z) [-.z ~] ?:(?=(%mod -.z) z ?:(?=(%aro -.z) z ?:(?=(%bac -.z) z ?:(?=(%ret -.z) z !!))))))
      =/  l=(unit lex)  (~(get by omen.ara) n)
      ?~  l
        ~&  >>>  [z 'NO EVENT']
        [~ hoc]
      =^  cards  ego
        (novo u.l z)
      [cards hoc]
    ::
    [~ hoc]
  ==
::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  
++  on-watch
  |=  =path
  ?+  path  [~ hoc]
      [%dill *]
    =/  =lux
      :-  %mor
      :~  (put-blit visa.ara)
          :-  %hop
          ?~  rex.ara
            [0 0]
          [l.rex.ara t.rex.ara]
      ==
    :_  hoc
    :~  [%give %fact ~[/dill/$] %dill-blit !>(lux)]
    ==
  ==
::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  
++  on-leave  |=(path ^-((quip card _hoc) !!))
++  on-peek   |=(path ^-((unit (unit cage)) !!))
++  on-agent  |=([wire sign:agent:gall] ^-((quip card _hoc) !!))
::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  
++  on-arvo
  |=  [=wire sign=sign-arvo]
  ^-  (quip card _hoc)
  [~[[%pass /flea %arvo %d %flee ~]] hoc]
::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  
++  on-fail   |=([term tang] ^-((quip card _hoc) !!))
--
::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  
|%
++  hnav                    :: hotkey context group for navigation
  ^-  omen
  %-  malt
  ^-  (list [nota lex])
  :~  [[%aro %l] %nav-l]  [[%aro %r] %nav-r]
      [[%aro %u] %nav-u]  [[%aro %d] %nav-d]
  ==
::
++  htog                    :: hotkey context group for toggling between nav and element interaction
  ^-  omen
  %-  malt
  ^-  (list [nota lex])
  :~  [[%mod mod=%ctl key=~-i] %tog]
  ==
::
++  hinp                    :: hotkey context group for an input element
  ^-  omen
  %-  malt
  ^-  (list [nota lex])
  :~  [[%aro %l] %cur-l]  [[%aro %r] %cur-r]
      [[%aro %u] %cur-u]  [[%aro %d] %cur-d]
      [[%txt ~] %inp]  [[%bac ~] %del]
  ==
::
:: ++  hscr                    :: hotkey context group for a scroll element ---- REMOVED
::   ^-  omen
::   %-  malt
::   ^-  (list [nota lex])
::   :~  [[%aro %l] %scr-l]  [[%aro %r] %scr-r]
::       [[%aro %u] %scr-u]  [[%aro %d] %scr-d]
::   ==
::  ::  ::
++  novo                    :: handle an event from the hotkey context
  |=  [=lex =zona]
  ^-  (quip card _ego)
  ::
  ?:  |(?=(%nav-l lex) ?=(%nav-r lex) ?=(%nav-u lex) ?=(%nav-d lex))
    |^
    =/  scro=$@(~ rami)
      ?~  rex.ara  ~
      |-
      ?:  (~(has in equi.ara) k.rex.ara)
        k.rex.ara
      ?~  t.k.rex.ara  ~
      $(k.rex.ara t.k.rex.ara)
    =/  spar  ?~(scro ~ (~(get by ens.ara) scro))
    =/  navs=ordo  (navi rex.ara ordo.ara)
    =/  snav=ordo
      ?~  scro  ~
      %+  skim  `ordo`navs
      |=  =dux
      (chil scro k.dux)
    =/  next=rex  ?~(navs ~ ?~(snav i.navs i.snav))
    =/  upd=$@(~ [opus cura])
      ?~  scro  ~
      ?:  ?=(^ snav)  ~
      %+  abeo  scro
      ?+  lex   lex
        %nav-l  %scr-l
        %nav-r  %scr-r
        %nav-u  %scr-u
        %nav-d  %scr-d
      ==
    =?  ara  ?=(^ upd)
      %_  ara
        rex   rex.upd
        ordo  ordo.upd
        omen  omen.upd
        ens   ens.upd
      ==
    =?  next  ?=(^ upd)
      ?~  scro  ~
      =<  ?~(. ~ i)
      %+  skim  `ordo`(navi rex.ara ordo.upd)
      |=  =dux
      (chil scro k.dux)
    =/  oel  ?~(rex.ara ~ (~(get by ens.ara) k.rex.ara))
    =/  nel  ?~(next ~ (~(get by ens.ara) k.next))
    =/  prae=opus
      ?~  rex.ara  [~ ~]
      (duco ens.ara k.rex.ara next)
    =.  rex.ara
      ?:  ?=(^ next)
        next
      ?:  |(?=(~ scro) ?=(~ spar) ?=(~ upd) ?=(^ (find ~[rex.ara] ordo.upd)))
        rex.ara
      :*  ?:((gth x.iter.u.spar x.lar.u.spar) 0 (sub x.lar.u.spar x.iter.u.spar))
          =/  r  (add x.lar.u.spar ?:(=(0 w.size.res.u.spar) 0 (dec w.size.res.u.spar)))
          ?:((gth x.iter.u.spar r) 0 (sub r x.iter.u.spar))
          ?:((gth y.iter.u.spar y.lar.u.spar) 0 (sub y.lar.u.spar y.iter.u.spar))
          =/  b  (add y.lar.u.spar ?:(=(0 h.size.res.u.spar) 0 (dec h.size.res.u.spar)))
          ?:((gth y.iter.u.spar b) 0 (sub b y.iter.u.spar))
          scro
      ==
    =/  post=opus
      ?:  ?=(^ next)
        (duco ens.ara k.next rex.ara)
      ?:  ?=(^ rex.ara)
        (duco ens.ara k.rex.ara rex.ara)
      [~ ~]
    =.  omen.ara
      ?~  oel  omen.ara
      ?+  -.ars.u.oel  omen.ara
        %input   (~(uni by (~(dif by omen.ara) hinp)) hnav)
        :: %scroll  (~(uni by omen.ara) hnav) --- REMOVED
      ==
    =.  omen.ara
      ?~  nel  omen.ara
      ?+  -.ars.u.nel  omen.ara
        %input   (~(uni by omen.ara) hinp)
        :: %scroll  (~(uni by omen.ara) hscr) --- REMOVED
      ==
    =.  ens.ara  (~(uni by ens.ara) (~(uni by ens.prae) ens.post))
    =.  visa.ara  (~(uni by ?~(upd visa.ara visa.upd)) (~(uni by visa.prae) visa.post))
    =/  =lux
      :-  %mor
      :~  (put-blit visa.ara)  :: change to dono 
          ?:  &(?=(^ nel) ?=(%input -.ars.u.nel))
            (vado ab.ars.u.nel i.ars.u.nel w.size.res.u.nel lar.u.nel iter.u.nel)
          :-  %hop
          ?~  rex.ara
            [0 0]
          ?:  |(?=(~ spar) &(?=(~ upd) ?=(~ snav) ?=(^ navs)))
            [l.rex.ara t.rex.ara]
          :-  ?:((lth l.rex.ara x.lar.u.spar) x.lar.u.spar l.rex.ara)
          ?:((lth t.rex.ara y.lar.u.spar) y.lar.u.spar t.rex.ara)
      ==
    :_  ego
    [[%give %fact ~[/dill/$] %dill-blit !>(lux)] ~]
    ::
    ++  navi
      |=  [r=rex o=ordo]
      ^-  ordo
      ?~  r  o
      =/  chis=ordo
        ?.  |(=(%nav-r lex) =(%nav-d lex))
          ~
        %+  sort
          ^-  ordo
          %+  skim  `ordo`o
          |=  =dux
          (chil k.r k.dux)
        |=  [a=dux b=dux]
        (lth (pyth a r) (pyth b r))
      ?:  ?=(^ chis)
        chis
      =/  tars=ordo
        %+  sort  `ordo`(skim `ordo`o (keep r))
        |=  [a=dux b=dux]
        (lth (pyth a r) (pyth b r))
      ?~  tars
        ~
      =/  pars=ordo
        %+  sort
          ^-  ordo
          %+  skim  `ordo`o
          |=  =dux
          ^-  bean
          ?:  =(k.dux k.r)
            |
          ?:  (chil k.dux k.i.tars)
            !(chil k.dux k.r)
          |
        |=  [a=dux b=dux]
        (lth (lent k.a) (lent k.b))
      ?~  pars
        tars
      ?:  ((keep r) i.pars)
        pars
      ~
    ::
    ++  chil
      |=  [a=rami b=rami]
      ^-  bean
      ?:  =(a b)
        |
      |-
      ?:  =(a b)
        &
      ?~  t.b
        |
      $(b t.b)
    ++  abov
      |=  [a=rami b=rami]
      ^-  ?(%a %b %~)
      =/  a  (flop a)
      =/  b  (flop b)
      |- 
      ?~  a  ~
      ?~  b  ~
      ?~  t.a  ~
      ?~  t.b  ~
      ?:  !=(ager.i.t.a ager.i.t.b)
        ?:  &(?=(%l axis.i.t.a) ?=(%l axis.i.t.b))
          ?:  (gth ager.i.t.a ager.i.t.b)
            %b
          %a
        ~
      $(a t.a, b t.b)
    ++  lap
      |=  [=dux rex=dux]
      ^-  bean
      ?|  ?&  (lte l.dux l.rex)  (gte r.dux l.rex)
              (lte t.dux t.rex)  (gte b.dux t.rex)
          ==
          ?&  (lte l.dux r.rex)  (gte r.dux r.rex)
              (lte t.dux t.rex)  (gte b.dux t.rex)
          ==
          ?&  (lte l.dux l.rex)  (gte r.dux l.rex)
              (lte t.dux b.rex)  (gte b.dux b.rex)
          ==
          ?&  (lte l.dux r.rex)  (gte r.dux r.rex)
              (lte t.dux b.rex)  (gte b.dux b.rex)
          ==
          ?&  (lte l.rex l.dux)  (gte r.rex l.dux)
              (lte t.rex t.dux)  (gte b.rex t.dux)
          ==
          ?&  (lte l.rex r.dux)  (gte r.rex r.dux)
              (lte t.rex t.dux)  (gte b.rex t.dux)
          ==
          ?&  (lte l.rex l.dux)  (gte r.rex l.dux)
              (lte t.rex b.dux)  (gte b.rex b.dux)
          ==
          ?&  (lte l.rex r.dux)  (gte r.rex r.dux)
              (lte t.rex b.dux)  (gte b.rex b.dux)
          ==
      ==
    ++  keep
      |=  rex=dux
      |=  =dux
      ^-  bean
      ?:  =(k.dux k.rex)  |
      ?+  lex  |
          %nav-l
        ?:  (lap dux rex)
          ?:  =(l.dux l.rex)
            |((chil k.dux k.rex) =(%b (abov k.rex k.dux)))
          (lth l.dux l.rex)
        ?.  =(~ (abov k.rex k.dux))
          (lth l.dux l.rex)
        (lth r.dux l.rex)
          %nav-u
        ?:  (lap dux rex)
          ?:  =(t.dux t.rex)
            |((chil k.dux k.rex) =(%b (abov k.rex k.dux)))
          (lth t.dux t.rex)
        ?.  =(~ (abov k.rex k.dux))
          (lth t.dux t.rex)
        (lth b.dux t.rex)
          %nav-r
        ?:  (lap dux rex)
          ?:  =(l.dux l.rex)
            |((chil k.rex k.dux) =(%a (abov k.rex k.dux)))
          (gth l.dux l.rex)
        ?.  =(~ (abov k.rex k.dux))
          (gth l.dux l.rex)
        (gth l.dux r.rex)
          %nav-d
        ?:  (lap dux rex)
          ?:  =(t.dux t.rex)
            |((chil k.rex k.dux) =(%a (abov k.rex k.dux)))
          (gth t.dux t.rex)
        ?.  =(~ (abov k.rex k.dux))
          (gth t.dux t.rex)
        (gth t.dux b.rex)
      ==
    ++  pyth
      |=  [=dux rex=dux]
      ^-  @ud
      =;  pyt=(pair @ud @ud)
        (add (mul 10 p.pyt) q.pyt)
      ?+  lex  [0 0]
          %nav-l
        ?:  |((lap dux rex) !=(~ (abov k.rex k.dux)))
          %-  sqt  %+  add
            (pow ?:((lte l.dux l.rex) (sub l.rex l.dux) (sub l.dux l.rex)) 2)
          (pow (mul ?:((gte t.rex t.dux) (sub t.rex t.dux) (sub t.dux t.rex)) 6) 2)
        %-  sqt  %+  add
          (pow ?:((lte r.dux l.rex) (sub l.rex r.dux) (sub r.dux l.rex)) 2)
        (pow (mul ?:((gte t.rex t.dux) (sub t.rex t.dux) (sub t.dux t.rex)) 6) 2)
          %nav-u
        ?:  |((lap dux rex) !=(~ (abov k.rex k.dux)))
          %-  sqt  %+  add
            (pow (mul ?:((gte l.dux l.rex) (sub l.dux l.rex) (sub l.rex l.dux)) 2) 2)
          (pow (mul ?:((lte t.dux t.rex) (sub t.rex t.dux) (sub t.dux t.rex)) 2) 2)
        %-  sqt  %+  add
          (pow (mul ?:((gte l.dux l.rex) (sub l.dux l.rex) (sub l.rex l.dux)) 2) 2)
        (pow (mul ?:((lte b.dux t.rex) (sub t.rex b.dux) (sub b.dux t.rex)) 2) 2)
          %nav-r
        ?:  |((lap dux rex) !=(~ (abov k.rex k.dux)))
          %-  sqt  %+  add
            (pow ?:((lte l.rex l.dux) (sub l.dux l.rex) (sub l.rex l.dux)) 2)
          (pow (mul ?:((gte t.dux t.rex) (sub t.dux t.rex) (sub t.rex t.dux)) 6) 2)
        %-  sqt  %+  add
          (pow ?:((lte r.rex l.dux) (sub l.dux r.rex) (sub r.rex l.dux)) 2)
        (pow (mul ?:((gte t.dux t.rex) (sub t.dux t.rex) (sub t.rex t.dux)) 6) 2)
          %nav-d
        ?:  |((lap dux rex) !=(~ (abov k.rex k.dux)))
          %-  sqt  %+  add
            (pow (mul ?:((gte l.rex l.dux) (sub l.rex l.dux) (sub l.dux l.rex)) 2) 2)
          (pow (mul ?:((lte t.rex t.dux) (sub t.dux t.rex) (sub t.rex t.dux)) 2) 2)
        %-  sqt  %+  add
          (pow (mul ?:((gte l.rex l.dux) (sub l.rex l.dux) (sub l.dux l.rex)) 2) 2)
        (pow (mul ?:((lte b.rex t.dux) (sub t.dux b.rex) (sub b.rex t.dux)) 2) 2)
      ==
    --
  ::
  ?:  ?=(%tog lex)
    ?~  rex.ara  [~ ego]
    =/  el  (~(get by ens.ara) k.rex.ara)
    ?~  el  [~ ego]
    =/  ev  (~(get by omen.ara) [%aro %l])
    ?~  ev  [~ ego]
    ?+  -.ars.u.el  [~ ego]
        %input
      ?.  ?=(%nav-l u.ev)
        :_  ego(omen.ara (~(uni by (~(dif by omen.ara) hinp)) hnav))
        [[%give %fact ~[/dill/$] %dill-blit !>([%hop x.lar.u.el y.lar.u.el])] ~]
      :_  ego(omen.ara (~(uni by omen.ara) hinp))
      :~  :*  %give  %fact  ~[/dill/$]  %dill-blit  
        !>((vado ab.ars.u.el i.ars.u.el w.size.res.u.el lar.u.el iter.u.el))
      ==  ==
      ::   %scroll
      :: ?.  ?=(%nav-l u.ev)
      ::   [~ ego(omen.ara (~(uni by omen.ara) hnav))] ------- REMOVED
      :: [~ ego(omen.ara (~(uni by omen.ara) hscr))]
    ==
  ::
  ?:  ?=(%inp lex)
    ?.  ?=(%txt -.zona)  [~ ego]
    ?~  p.zona  [~ ego]
    ?~  rex.ara  [~ ego]
    =/  el  (~(get by ens.ara) k.rex.ara)
    ?~  el  [~ ego]
    ?.  ?=(%input -.ars.u.el)  [~ ego]
    =:  vox.ars.u.el
          ?~  t.p.zona
            (into vox.ars.u.el i.ars.u.el (tuft i.p.zona))
          %+  weld  (weld (scag +(i.ars.u.el) vox.ars.u.el) (tufa p.zona))
          (slag +(i.ars.u.el) vox.ars.u.el)
        i.ars.u.el
          (add i.ars.u.el (lent p.zona))
      ==
    =.  ab.ars.u.el
      ?:  (gte (sub i.ars.u.el ab.ars.u.el) (mul w.size.res.u.el h.size.res.u.el))
        ?:  =(1 h.size.res.u.el)
          +(ab.ars.u.el)
        (add ab.ars.u.el w.size.res.u.el)
      ab.ars.u.el
    =/  lim  [(add x.lar.u.el w.size.res.u.el) (add y.lar.u.el h.size.res.u.el)]
    =/  vi=visa  (rinp lar.u.el lim res.u.el ab.ars.u.el vox.ars.u.el)
    =?  vi  !&(=(0 x.iter.u.el) =(0 y.iter.u.el))
      %-  %~  rep
            by
          vi
      |=  [[l=loci c=[fila @c ~]] acc=visa]
      ?:  |((gth x.iter.u.el x.l) (gth y.iter.u.el y.l))
        acc
      (~(put by acc) [(sub x.l x.iter.u.el) (sub y.l y.iter.u.el)] c)
    =.  vi  (~(int by visa.u.el) vi)
    =.  visa.u.el  vi
    :_  ego(ens.ara (~(put by ens.ara) k.rex.ara u.el), visa.ara (~(uni by visa.ara) vi))
    :~  :*  %give  %fact  ~[/dill/$]  %dill-blit
      !>  :-  %mor
      :~  (dono visa.ara vi)
      (vado ab.ars.u.el i.ars.u.el w.size.res.u.el lar.u.el iter.u.el)
    ==  ==  ==
  ::
  ?:  ?=(%del lex)
    ?~  rex.ara  [~ ego]
    =/  el  (~(get by ens.ara) k.rex.ara)
    ?~  el  [~ ego]
    ?.  ?=(%input -.ars.u.el)  [~ ego]
    =.  i.ars.u.el  ?:(=(0 i.ars.u.el) 0 (dec i.ars.u.el))
    =.  vox.ars.u.el  (oust [i.ars.u.el 1] vox.ars.u.el)
    =.  ab.ars.u.el
      ?:  &((lte i.ars.u.el ab.ars.u.el) !=(0 ab.ars.u.el))
        ?:  =(1 h.size.res.u.el)
          ?:((gth w.size.res.u.el ab.ars.u.el) 0 +((sub ab.ars.u.el w.size.res.u.el)))
        ?:((gth w.size.res.u.el ab.ars.u.el) 0 (sub ab.ars.u.el w.size.res.u.el))
      ab.ars.u.el
    =/  lim  [(add x.lar.u.el w.size.res.u.el) (add y.lar.u.el h.size.res.u.el)]
    =/  vi=visa  (rinp lar.u.el lim res.u.el ab.ars.u.el vox.ars.u.el)
    =?  vi  !&(=(0 x.iter.u.el) =(0 y.iter.u.el))
      %-  %~  rep
            by
          vi
      |=  [[l=loci c=[fila @c ~]] acc=visa]
      ?:  |((gth x.iter.u.el x.l) (gth y.iter.u.el y.l))
        acc
      (~(put by acc) [(sub x.l x.iter.u.el) (sub y.l y.iter.u.el)] c)
    =.  vi  (~(int by visa.u.el) vi)
    =.  visa.u.el  vi
    :_  ego(ens.ara (~(put by ens.ara) k.rex.ara u.el), visa.ara (~(uni by visa.ara) vi))
    :~  :*  %give  %fact  ~[/dill/$]  %dill-blit
      !>  :-  %mor
      :~  (dono visa.ara vi)
      (vado ab.ars.u.el i.ars.u.el w.size.res.u.el lar.u.el iter.u.el)
    ==  ==  ==
  ::
  ?:  |(?=(%cur-l lex) ?=(%cur-r lex) ?=(%cur-u lex) ?=(%cur-d lex))
    ?~  rex.ara  [~ ego]
    =/  el  (~(get by ens.ara) k.rex.ara)
    ?~  el  [~ ego]
    ?.  ?=(%input -.ars.u.el)  [~ ego]
    =/  oi=@ud  i.ars.u.el
    =.  i.ars.u.el
      ?+  lex  i.ars.u.el
          %cur-l
        ?:(=(0 i.ars.u.el) 0 (dec i.ars.u.el))
          %cur-r
        =/  i  +(i.ars.u.el)
        ?:  (gth i (lent vox.ars.u.el))
          i.ars.u.el
        i
          %cur-u
        ?:  =(1 h.size.res.u.el)
          0
        ?:  (lth i.ars.u.el w.size.res.u.el)
          0
        (sub i.ars.u.el w.size.res.u.el)
          %cur-d
        ?:  =(1 h.size.res.u.el)
          (lent vox.ars.u.el)
        =/  i  (add i.ars.u.el w.size.res.u.el)
        =/  l  (lent vox.ars.u.el)
        ?:((gth i l) l i)
      ==
    ?:  =(oi i.ars.u.el)
      %+  novo
        ?+  lex   lex
          %cur-l  %nav-l
          %cur-r  %nav-r
          %cur-u  %nav-u
          %cur-d  %nav-d
        ==
      zona
    =/  nab=@ud
      ?:  |(?=(%cur-l lex) ?=(%cur-u lex))
        ?:  &(?=(%cur-u lex) =(1 h.size.res.u.el))
          0
        ?:  &(!=(0 ab.ars.u.el) (lte i.ars.u.el ab.ars.u.el))
          ?:  =(1 h.size.res.u.el)
            (dec ab.ars.u.el)
          ?:((gth w.size.res.u.el ab.ars.u.el) 0 (sub ab.ars.u.el w.size.res.u.el))
        ab.ars.u.el
      ?:  |(?=(%cur-r lex) ?=(%cur-d lex))
        ?:  &(?=(%cur-d lex) =(1 h.size.res.u.el))
          ?.  (gte (sub i.ars.u.el ab.ars.u.el) w.size.res.u.el)
            ab.ars.u.el
          =/  m  (mod i.ars.u.el w.size.res.u.el)
          =/  l  (lent vox.ars.u.el)
          ?:((gth m l) 0 (sub l m))
        ?:  (gte (sub i.ars.u.el ab.ars.u.el) (mul w.size.res.u.el h.size.res.u.el))
          ?:  =(1 h.size.res.u.el)
            +(ab.ars.u.el)
          (add ab.ars.u.el w.size.res.u.el)
        ab.ars.u.el
      ab.ars.u.el
    ?:  =(ab.ars.u.el nab)
      :_  ego(ens.ara (~(put by ens.ara) k.rex.ara u.el))
      :~  :*  %give  %fact  ~[/dill/$]  %dill-blit  
        !>((vado ab.ars.u.el i.ars.u.el w.size.res.u.el lar.u.el iter.u.el))
      ==  ==
    =.  ab.ars.u.el  nab
    =/  lim  [(add x.lar.u.el w.size.res.u.el) (add y.lar.u.el h.size.res.u.el)]
    =/  vi=visa  (rinp lar.u.el lim res.u.el ab.ars.u.el vox.ars.u.el)
    =?  vi  !&(=(0 x.iter.u.el) =(0 y.iter.u.el))
      %-  %~  rep
            by
          vi
      |=  [[l=loci c=[fila @c ~]] acc=visa]
      ?:  |((gth x.iter.u.el x.l) (gth y.iter.u.el y.l))
        acc
      (~(put by acc) [(sub x.l x.iter.u.el) (sub y.l y.iter.u.el)] c)
    =.  vi  (~(int by visa.u.el) vi)
    =.  visa.u.el  vi
    :_  ego(ens.ara (~(put by ens.ara) k.rex.ara u.el), visa.ara (~(uni by visa.ara) vi))
    :~  :*  %give  %fact  ~[/dill/$]  %dill-blit
      !>  :-  %mor
      :~  (dono visa.ara vi)
      (vado ab.ars.u.el i.ars.u.el w.size.res.u.el lar.u.el iter.u.el)
    ==  ==  ==
  ::
  ?:  |(?=(%scr-l lex) ?=(%scr-r lex) ?=(%scr-u lex) ?=(%scr-d lex))
    ?~  rex.ara  [~ ego]
    =/  upd=$@(~ [opus cura])  (abeo k.rex.ara lex)
    ?~  upd  [~ ego]
    :_  %_  ego
          ens.ara   ens.upd
          visa.ara  visa.upd
          ordo.ara  ordo.upd
          rex.ara   ?~(rex.upd rex.ara rex.upd)
        ==
    :~  :*  %give  %fact  ~[/dill/$]  %dill-blit
      !>([%mor ~[(dono visa.ara visa.upd) [%hop [l.rex.ara t.rex.ara]]]])
    ==  ==
  ::
  [~ ego]
::
++  abeo                    :: handle a scroll event for a scroll element by key
  |=  [pk=rami =lex]
  ^-  $@(~ [opus cura])
  =/  par  (~(get by ens.ara) pk)
  ?~  par  ~
  ?.  ?=(%scroll -.ars.u.par)  ~
  ?:  ?|  &(?=(%scr-l lex) =(0 x.iter.ars.u.par))
          &(?=(%scr-r lex) =(x.sola.ars.u.par x.iter.ars.u.par))
          &(?=(%scr-u lex) =(0 y.iter.ars.u.par))
          &(?=(%scr-d lex) =(y.sola.ars.u.par y.iter.ars.u.par))
      ==
    ~
  =.  iter.ars.u.par
    ?+  lex  iter.ars.u.par
      %scr-l  ?:(=(0 x.iter.ars.u.par) iter.ars.u.par [(dec x.iter.ars.u.par) y.iter.ars.u.par])
      %scr-r  [+(x.iter.ars.u.par) y.iter.ars.u.par]  
      %scr-u  ?:(=(0 y.iter.ars.u.par) iter.ars.u.par [x.iter.ars.u.par (dec y.iter.ars.u.par)])
      %scr-d  [x.iter.ars.u.par +(y.iter.ars.u.par)]
    ==
  =/  prl  ;:(add x.lar.u.par l.muri.ars.u.par l.padd.res.u.par)
  =/  prr
    =/  r  ;:(add 1 r.muri.ars.u.par r.padd.res.u.par)
    (add x.lar.u.par ?:((gth r w.size.res.u.par) 0 (sub w.size.res.u.par r)))
  =/  prt  ;:(add y.lar.u.par t.muri.ars.u.par t.padd.res.u.par)
  =/  prb
    =/  b  ;:(add 1 b.muri.ars.u.par b.padd.res.u.par)
    (add y.lar.u.par ?:((gth b h.size.res.u.par) 0 (sub h.size.res.u.par b)))
  =/  opu=opus  (eo ens.ara visa.ara lex ~ pk visa.u.par prl prt prr prb)
  =/  pvi=visa
    (~(dif by (rbox lar.u.par [(add x.lar.u.par w.size.res.u.par) (add y.lar.u.par h.size.res.u.par)] res.u.par)) visa.opu)
  =.  visa.u.par  pvi
  =:  ens.opu   (~(put by ens.opu) pk u.par)
      visa.opu  (~(uni by visa.opu) visa.u.par)
    ==
  [opu (dico ens.opu)]
::
++  eo                      :: perform a scroll on the elements within a scroll element
  |=  $:  e=ens  v=visa  lx=$@(~ lex)
          nitr=$@(~ loci)  pk=rami  pv=visa
          prl=@ud  prt=@ud  prr=@ud  prb=@ud
      ==
  ^-  opus
  =/  olv=visa
    =/  k=rami  [[%l 0] pk]
    =/  a=visa  pv
    |-
    =/  chi  (~(get by e) k)
    ?~  chi
      ?:  ?=(%b axis.i.k)  $(k [[%l 0] t.k])
      ?:  ?=(%l axis.i.k)  $(k [[%~ 0] t.k])
      a
    =.  a  (~(uni by a) visa.u.chi)
    $(ager.i.k +(ager.i.k), a $(k [[%b 0] k]))
  =;  opu=opus
    %_  opu
      ens   (~(uni by e) ens.opu)
      visa  (~(uni by (~(dif by v) olv)) visa.opu)  :: note: adding dif by olv from the other one, and here we are rendering and adding the parent which was not in the other one
    ==
  =/  k=rami           [[%l 0] pk]
  =/  a=opus           [~ ~]
  =/  slar=$@(~ modi)  ~
  =/  slim=$@(~ modi)  ~
  |-  ^-  opus
  =/  chi  (~(get by e) k)
  ?~  chi
    ?:  ?=(%b axis.i.k)  $(k [[%l 0] t.k])
    ?:  ?=(%l axis.i.k)  $(k [[%~ 0] t.k])
    a
  =.  iter.u.chi
    ?:  ?=(^ nitr)
      [(add x.iter.u.chi x.nitr) (add y.iter.u.chi y.nitr)]
    ?+  lx  iter.u.chi
      %scr-l  ?:(=(0 x.iter.u.chi) iter.u.chi [(dec x.iter.u.chi) y.iter.u.chi])
      %scr-r  [+(x.iter.u.chi) y.iter.u.chi]  
      %scr-u  ?:(=(0 y.iter.u.chi) iter.u.chi [x.iter.u.chi (dec y.iter.u.chi)])
      %scr-d  [x.iter.u.chi +(y.iter.u.chi)]
    ==
  =.  a
    %=  $
      k     [[%b 0] k]
      slar  ?:  ?=(^ slar)  slar
            ?.  ?=(%scroll -.ars.u.chi)  ~
            =/  [x=@ud y=@ud]
              :-  (add x.lar.u.chi (add l.padd.res.u.chi l.muri.ars.u.chi))
              (add y.lar.u.chi (add t.padd.res.u.chi t.muri.ars.u.chi))
            :-  ?:((lte x.iter.u.chi x) (sub x x.iter.u.chi) 0)
            ?:((lte y.iter.u.chi y) (sub y y.iter.u.chi) 0)
      slim  ?:  ?=(^ slim)  slim
            ?.  ?=(%scroll -.ars.u.chi)  ~
            :-  ?:((lte x.iter.u.chi x.modi.u.chi) (sub x.modi.u.chi x.iter.u.chi) 0)
            ?:((lte y.iter.u.chi y.modi.u.chi) (sub y.modi.u.chi y.iter.u.chi) 0)
    ==
  =?  slar  ?=(%border -.ars.u.chi)  ~
  =/  crig  (add x.lar.u.chi ?:(=(0 w.size.res.u.chi) 0 (dec w.size.res.u.chi)))
  =/  cbot  (add y.lar.u.chi ?:(=(0 h.size.res.u.chi) 0 (dec h.size.res.u.chi)))
  ?:  ?|  (gth y.iter.u.chi cbot)
          (lth (sub cbot y.iter.u.chi) prt)
          &((lte y.iter.u.chi y.lar.u.chi) (gth (sub y.lar.u.chi y.iter.u.chi) prb))
          (gth x.iter.u.chi crig)
          (lth (sub crig x.iter.u.chi) prl)
          &((lte x.iter.u.chi x.lar.u.chi) (gth (sub x.lar.u.chi x.iter.u.chi) prr))
      ==
    =.  ens.a  (~(put by ens.a) k u.chi(visa ~))
    $(ager.i.k +(ager.i.k))
  =/  vi=visa  (viso lar.u.chi res.u.chi ars.u.chi modi.u.chi)
  =.  vi
    %-  %~  rep
          by
        vi
    |=  [[l=loci c=[fila @c ~]] acc=visa]
    ?:  |((gth x.iter.u.chi x.l) (gth y.iter.u.chi y.l))
      acc
    =.  l  [(sub x.l x.iter.u.chi) (sub y.l y.iter.u.chi)]
    ?:  |((lth y.l prt) (gth y.l prb) (lth x.l prl) (gth x.l prr))
      acc
    ?:  ?|  &(?=(^ slim) |((gth y.l y.slim) (gth x.l x.slim)))
            &(?=(^ slar) |((lth x.l x.slar) (lth y.l y.slar)))
        ==
      acc
    (~(put by acc) l c)
  =.  vi  (~(int by olv) (~(dif by vi) visa.a))
  =.  ens.a  (~(put by ens.a) k u.chi(visa vi))
  =.  visa.a  (~(uni by visa.a) vi)
  $(ager.i.k +(ager.i.k))
::
++  vado                    :: move the cursor to a given input character index
  |=  [ab=@ud i=@ud w=@ud =lar =iter]
  ^-  lux
  =/  n=@ud  (sub i ab)
  =/  col=@ud  (mod n w)
  =/  row=@ud
    ?:  =(0 w)
      0
    ?:  =(0 (mod +(n) w))
      (div +(n) w)
    +((div +(n) w))
  =.  row  ?:(=(0 row) 0 (dec row))
  =/  [x=@ud y=@ud]  [(add x.lar col) (add y.lar row)]
  :+  %hop
    ?:((lte x.iter x) (sub x x.iter) 0)
  ?:((lte y.iter y) (sub y y.iter) 0)
::  ::  ::  ::  ::  ::  ::
++  dolo                    :: get default styles for a semantic element
  |=  el=@tas
  ^-  rei
  ?+  el
    :*  size=[[%i 0] [%i 0]]
        padd=[[%c 0] [%c 0] [%c 0] [%c 0]]
        marg=[[%c 0] [%c 0] [%c 0] [%c 0]]
        flex=[0 0]
        flow=[%row %clip]
        look=[~ ~ ~]
    ==
      %text
    :*  size=[[%i 0] [%i 0]]
        padd=[[%c 0] [%c 0] [%c 0] [%c 0]]
        marg=[[%c 0] [%c 0] [%c 0] [%c 0]]
        flex=[0 0]
        flow=[%row %clip]
        look=[~ ~ ~]
    ==
      %layer
    :*  size=[[%p 100] [%p 100]]
        padd=[[%c 0] [%c 0] [%c 0] [%c 0]]
        marg=[[%c 0] [%c 0] [%c 0] [%c 0]]
        flex=[0 0]
        flow=[%row %clip]
        look=[~ ~ ~]
    ==
      %border-left
    :*  size=[[%c 1] [%p 100]]
        padd=[[%c 0] [%c 0] [%c 0] [%c 0]]
        marg=[[%c 0] [%c 0] [%c 0] [%c 0]]
        flex=[0 0]
        flow=[%col %clip]
        look=[~ ~ ~]
    ==
      %border-right
    :*  size=[[%c 1] [%p 100]]
        padd=[[%c 0] [%c 0] [%c 0] [%c 0]]
        marg=[[%c 0] [%c 0] [%c 0] [%c 0]]
        flex=[0 0]
        flow=[%col %clip]
        look=[~ ~ ~]
    ==
      %border-top
    :*  size=[[%p 100] [%c 1]]
        padd=[[%c 0] [%c 0] [%c 0] [%c 0]]
        marg=[[%c 0] [%c 0] [%c 0] [%c 0]]
        flex=[0 0]
        flow=[%row %clip]
        look=[~ ~ ~]
    ==
      %border-bottom
    :*  size=[[%p 100] [%c 1]]
        padd=[[%c 0] [%c 0] [%c 0] [%c 0]]
        marg=[[%c 0] [%c 0] [%c 0] [%c 0]]
        flex=[0 0]
        flow=[%row %clip]
        look=[~ ~ ~]
    ==
      %input
    :*  size=[[%c 10] [%c 1]]
        padd=[[%c 0] [%c 0] [%c 0] [%c 0]]
        marg=[[%c 0] [%c 0] [%c 0] [%c 0]]
        flex=[0 0]
        flow=[%row %clip]
        look=[~ [~ %w] [~ %k]]
    ==
      %scroll
    :*  size=[[%i 0] [%i 0]]
        padd=[[%c 0] [%c 0] [%c 0] [%c 0]]
        marg=[[%c 0] [%c 0] [%c 0] [%c 0]]
        flex=[0 0]
        flow=[%col %clip]
        look=[~ ~ ~]
    ==
  ==
::
++  suo                     :: process a sail element's name and attribute list for geno
  |=  [n=mane a=mart]
  ^-  [rei aves ars mart]
  =+  ^-  [=rei =aves =ars velo=mart]
      ?+  n             [(dolo %$) ~ [%$ ~] ~]
        %$              [(dolo %text) ~ [%text ?~(a ~ v.i.a)] ~]
        %layer          [(dolo %layer) ~ [%layer ~] ~]
        %select         [(dolo %$) ~ [%select ~ ~ ~] ~]
        %border-left    [(dolo %border-left) ~ [%border %left %~] ~]
        %border-right   [(dolo %border-right) ~ [%border %right %~] ~]
        %border-top     [(dolo %border-top) ~ [%border %top %~] ~]
        %border-bottom  [(dolo %border-bottom) ~ [%border %bottom %~] ~]
        %input          [(dolo %input) ~ [%input 0 0 ~] ~]
        %scroll         [(dolo %scroll) ~ [%scroll *iter *muri *sola] ~]
      ==
  |-
  ?~  a  [rei aves ars velo]
  ?+  n.i.a  $(a t.a)
      %w
    ?:  &(?=(%border -.ars) |(?=(%top ad.ars) ?=(%bottom ad.ars)))
      $(a t.a)
    $(w.size.rei (pars v.i.a), a t.a)
      %h
    ?:  &(?=(%border -.ars) |(?=(%left ad.ars) ?=(%right ad.ars)))
      $(a t.a)
    $(h.size.rei (pars v.i.a), a t.a)
      %p
    =/  v  (pars v.i.a)
    $(padd.rei [v v v v], a t.a)
      %px
    =/  v  (pars v.i.a)
    $(l.padd.rei v, r.padd.rei v, a t.a)
      %py
    =/  v  (pars v.i.a)
    $(t.padd.rei v, b.padd.rei v, a t.a)
      %pl
    $(l.padd.rei (pars v.i.a), a t.a)
      %pr
    $(r.padd.rei (pars v.i.a), a t.a)
      %pt
    $(t.padd.rei (pars v.i.a), a t.a)
      %pb
    $(b.padd.rei (pars v.i.a), a t.a)
      %m
    ?:  ?=(%border -.ars)
      $(a t.a)
    =/  v  (pars v.i.a)
    $(marg.rei [v v v v], a t.a)
      %mx
    ?:  ?=(%border -.ars)
      $(a t.a)
    =/  v  (pars v.i.a)
    $(l.marg.rei v, r.marg.rei v, a t.a)
      %my
    ?:  ?=(%border -.ars)
      $(a t.a)
    =/  v  (pars v.i.a)
    $(t.marg.rei v, b.marg.rei v, a t.a)
      %ml
    ?:  ?=(%border -.ars)
      $(a t.a)
    $(l.marg.rei (pars v.i.a), a t.a)
      %mr
    ?:  ?=(%border -.ars)
      $(a t.a)
    $(r.marg.rei (pars v.i.a), a t.a)
      %mt
    ?:  ?=(%border -.ars)
      $(a t.a)
    $(t.marg.rei (pars v.i.a), a t.a)
      %mb
    ?:  ?=(%border -.ars)
      $(a t.a)
    $(b.marg.rei (pars v.i.a), a t.a)
      %fx
    =/  num=(unit @ud)  (slaw %ud (crip v.i.a))
    ?:  ?=(^ num)
      $(x.flex.rei ?:((lte u.num 100) u.num 100), a t.a)
    ?+  (@tas (crip v.i.a))  $(a t.a)
      %start   $(x.flex.rei 0, a t.a)
      %center  $(x.flex.rei 50, a t.a)
      %end     $(x.flex.rei 100, a t.a)
    ==
      %fy
    =/  num=(unit @ud)  (slaw %ud (crip v.i.a))
    ?:  ?=(^ num)
      $(y.flex.rei ?:((lte u.num 100) u.num 100), a t.a)
    ?+  (@tas (crip v.i.a))  $(a t.a)
      %start   $(y.flex.rei 0, a t.a)
      %center  $(y.flex.rei 50, a t.a)
      %end     $(y.flex.rei 100, a t.a)
    ==
      %fl
    ?+  (@tas (crip v.i.a))  $(a t.a)
      %row          $(flow.rei [%row %clip], a t.a)
      %row-clip     $(flow.rei [%row %clip], a t.a)
      %row-wrap     $(flow.rei [%row %wrap], a t.a)
      %column       $(flow.rei [%col %clip], a t.a)
      %column-clip  $(flow.rei [%col %clip], a t.a)
      %column-wrap  $(flow.rei [%col %wrap], a t.a)
    ==
      %cb
    ?:  &(?=(^ v.i.a) =('#' i.v.i.a))
      $(b.look.rei [~ (seco v.i.a)], a t.a)
    ?+  (@tas (crip v.i.a))  $(b.look.rei ~, a t.a)
      %red      $(b.look.rei [~ %r], a t.a)
      %green    $(b.look.rei [~ %g], a t.a)
      %blue     $(b.look.rei [~ %b], a t.a)
      %cyan     $(b.look.rei [~ %c], a t.a)
      %magenta  $(b.look.rei [~ %m], a t.a)
      %yellow   $(b.look.rei [~ %y], a t.a)
      %black    $(b.look.rei [~ %k], a t.a)
      %white    $(b.look.rei [~ %w], a t.a)
    ==
      %cf
    ?:  &(?=(^ v.i.a) =('#' i.v.i.a))
      $(f.look.rei [~ (seco v.i.a)], a t.a)
    ?+  (@tas (crip v.i.a))  $(f.look.rei ~, a t.a)
      %red      $(f.look.rei [~ %r], a t.a)
      %green    $(f.look.rei [~ %g], a t.a)
      %blue     $(f.look.rei [~ %b], a t.a)
      %cyan     $(f.look.rei [~ %c], a t.a)
      %magenta  $(f.look.rei [~ %m], a t.a)
      %yellow   $(f.look.rei [~ %y], a t.a)
      %black    $(f.look.rei [~ %k], a t.a)
      %white    $(f.look.rei [~ %w], a t.a)
    ==
      %d
    ?+  (@tas (crip v.i.a))  $(d.look.rei ~, a t.a)
      %bold       $(d.look.rei [~ (silt ~[%br])], a t.a)
      %blink      $(d.look.rei [~ (silt ~[%bl])], a t.a)
      %underline  $(d.look.rei [~ (silt ~[%un])], a t.a)
    ==
      %b
    ?.  ?=(%border -.ars)
      $(velo [i.a velo], a t.a)
    ?+  (@tas (crip v.i.a))  $(a t.a)
      %light   $(pila.ars %light, a t.a)
      %heavy   $(pila.ars %heavy, a t.a)
      %double  $(pila.ars %double, a t.a)
      %blank   $(pila.ars %blank, a t.a)
    ==
      %b-cb
    ?.  ?=(%border -.ars)
      $(velo [i.a velo], a t.a)
    ?:  &(?=(^ v.i.a) =('#' i.v.i.a))
      $(b.look.rei [~ (seco v.i.a)], a t.a)
    ?+  (@tas (crip v.i.a))  $(b.look.rei ~, a t.a)
      %red      $(b.look.rei [~ %r], a t.a)
      %green    $(b.look.rei [~ %g], a t.a)
      %blue     $(b.look.rei [~ %b], a t.a)
      %cyan     $(b.look.rei [~ %c], a t.a)
      %magenta  $(b.look.rei [~ %m], a t.a)
      %yellow   $(b.look.rei [~ %y], a t.a)
      %black    $(b.look.rei [~ %k], a t.a)
      %white    $(b.look.rei [~ %w], a t.a)
    ==
      %b-cf
    ?.  ?=(%border -.ars)
      $(velo [i.a velo], a t.a)
    ?:  &(?=(^ v.i.a) =('#' i.v.i.a))
      $(f.look.rei [~ (seco v.i.a)], a t.a)
    ?+  (@tas (crip v.i.a))  $(f.look.rei ~, a t.a)
      %red      $(f.look.rei [~ %r], a t.a)
      %green    $(f.look.rei [~ %g], a t.a)
      %blue     $(f.look.rei [~ %b], a t.a)
      %cyan     $(f.look.rei [~ %c], a t.a)
      %magenta  $(f.look.rei [~ %m], a t.a)
      %yellow   $(f.look.rei [~ %y], a t.a)
      %black    $(f.look.rei [~ %k], a t.a)
      %white    $(f.look.rei [~ %w], a t.a)
    ==
      %b-d
    ?.  ?=(%border -.ars)
      $(velo [i.a velo], a t.a)
    ?+  (@tas (crip v.i.a))  $(d.look.rei ~, a t.a)
      %bold       $(d.look.rei [~ (silt ~[%br])], a t.a)
      %blink      $(d.look.rei [~ (silt ~[%bl])], a t.a)
      %underline  $(d.look.rei [~ (silt ~[%un])], a t.a)
    ==
      %sel
    $(aves (~(put by aves) %sel (crip v.i.a)), a t.a)
      %sel-cb
    ?.  ?=(%select -.ars)
      $(a t.a)
    ?:  &(?=(^ v.i.a) =('#' i.v.i.a))
      $(b.acia.ars [~ (seco v.i.a)], a t.a)
    ?+  (@tas (crip v.i.a))  $(a t.a)
      %red      $(b.acia.ars [~ %r], a t.a)
      %green    $(b.acia.ars [~ %g], a t.a)
      %blue     $(b.acia.ars [~ %b], a t.a)
      %cyan     $(b.acia.ars [~ %c], a t.a)
      %magenta  $(b.acia.ars [~ %m], a t.a)
      %yellow   $(b.acia.ars [~ %y], a t.a)
      %black    $(b.acia.ars [~ %k], a t.a)
      %white    $(b.acia.ars [~ %w], a t.a)
    ==
      %sel-cf
    ?.  ?=(%select -.ars)
      $(a t.a)
    ?:  &(?=(^ v.i.a) =('#' i.v.i.a))
      $(f.acia.ars [~ (seco v.i.a)], a t.a)
    ?+  (@tas (crip v.i.a))  $(a t.a)
      %red      $(f.acia.ars [~ %r], a t.a)
      %green    $(f.acia.ars [~ %g], a t.a)
      %blue     $(f.acia.ars [~ %b], a t.a)
      %cyan     $(f.acia.ars [~ %c], a t.a)
      %magenta  $(f.acia.ars [~ %m], a t.a)
      %yellow   $(f.acia.ars [~ %y], a t.a)
      %black    $(f.acia.ars [~ %k], a t.a)
      %white    $(f.acia.ars [~ %w], a t.a)
    ==
      %sel-d
    ?.  ?=(%select -.ars)
      $(a t.a)
    ?+  (@tas (crip v.i.a))  $(a t.a)
      %bold       $(d.acia.ars [~ (silt ~[%br])], a t.a)
      %blink      $(d.acia.ars [~ (silt ~[%bl])], a t.a)
      %underline  $(d.acia.ars [~ (silt ~[%un])], a t.a)
    ==
      %act
    $(aves (~(put by aves) %act (crip v.i.a)), a t.a)
  ==
::
++  pars                    :: parse a tape to a sizing unit
  |=  v=tape
  ^-  as
  ?:  =(~ v)  [%c 0]
  ?:  =('%' (rear v))
    =/  n=(unit @ud)  (slaw %ud (crip (snip v)))
    ?~  n  [%c 0]
    [%p ?:((gth u.n 100) 100 u.n)]
  =/  n=(unit @ud)  (slaw %ud (crip v))
  ?~  n  [%c 0]
  [%c u.n]
::
++  seco                    :: parse a tape to a hex color
  |=  v=tape
  ^-  [r=@uxD g=@uxD b=@uxD]
  ?.  ?&  ?=(^ v)  ?=(^ t.v)  ?=(^ t.t.v)  ?=(^ t.t.t.v)
          ?=(^ t.t.t.t.v)  ?=(^ t.t.t.t.t.v)  ?=(^ t.t.t.t.t.t.v)
      ==
    [0x0 0x0 0x0]
  =/  r=(unit @uxD)
    %+  slaw  %ux
    %-  crip
    :+  '0'  'x'
    %-  cass
    ?:  =('0' i.t.v)
      [i.t.t.v ~]
    [i.t.v i.t.t.v ~]
  =/  g=(unit @uxD)
    %+  slaw  %ux
    %-  crip
    :+  '0'  'x'
    %-  cass
    ?:  =('0' i.t.t.t.v)
      [i.t.t.t.t.v ~]
    [i.t.t.t.v i.t.t.t.t.v ~]
  =/  b=(unit @uxD)
    %+  slaw  %ux
    %-  crip
    :+  '0'  'x'
    %-  cass
    ?:  =('0' i.t.t.t.t.t.v)
      [i.t.t.t.t.t.t.v ~]
    [i.t.t.t.t.t.v i.t.t.t.t.t.t.v ~]
  ?.  &(?=(^ r) ?=(^ g) ?=(^ b))
    [0x0 0x0 0x0]
  [u.r u.g u.b]
::
++  obeo                    :: get border sizes from a list of border elements
  |=  bor=marl              :: defaults to 1 if an element is found and no valid size is specified
  ^-  [bl=@ud br=@ud bt=@ud bb=@ud]
  =+  [i=0 bl=0 br=0 bt=0 bb=0]
  |-
  ?~  bor  [bl br bt bb]
  ?+  n.g.i.bor  $(bor t.bor)
      %border-left
    ?~  a.g.i.bor
      $(bor t.bor, bl ?:(=(0 bl) 1 bl))
    ?:  =(%w n.i.a.g.i.bor)
      =/  n=(unit @ud)  (slaw %ud (crip v.i.a.g.i.bor))
      %=  $
        bor  t.bor
        bl   ?~(n ?:(=(0 bl) 1 bl) ?:((gth u.n bl) u.n bl))
      ==
    $(a.g.i.bor t.a.g.i.bor)
      %border-right
    ?~  a.g.i.bor
      $(bor t.bor, br ?:(=(0 br) 1 br))
    ?:  =(%w n.i.a.g.i.bor)
      =/  n=(unit @ud)  (slaw %ud (crip v.i.a.g.i.bor))
      %=  $
        bor  t.bor
        br   ?~(n ?:(=(0 br) 1 br) ?:((gth u.n br) u.n br))
      ==
    $(a.g.i.bor t.a.g.i.bor)
      %border-top
    ?~  a.g.i.bor
      $(bor t.bor, bt ?:(=(0 bt) 1 bt))
    ?:  =(%h n.i.a.g.i.bor)
      =/  n=(unit @ud)  (slaw %ud (crip v.i.a.g.i.bor))
      %=  $
        bor  t.bor
        bt   ?~(n ?:(=(0 bt) 1 bt) ?:((gth u.n bt) u.n bt))
      ==
    $(a.g.i.bor t.a.g.i.bor)
      %border-bottom
    ?~  a.g.i.bor
      $(bor t.bor, bb ?:(=(0 bb) 1 bb))
    ?:  =(%h n.i.a.g.i.bor)
      =/  n=(unit @ud)  (slaw %ud (crip v.i.a.g.i.bor))
      %=  $
        bor  t.bor
        bb   ?~(n ?:(=(0 bb) 1 bb) ?:((gth u.n bb) u.n bb))
      ==
    $(a.g.i.bor t.a.g.i.bor)
  ==
::
++  duco                    :: render a navigation style change update
  |=  [o=ens k=rami r=rex]  :: returns just the ens and visa of the updated elements
  ^-  opus
  ?~  r  [~ ~]
  =/  par  (~(get by o) k)
  ?~  par  [~ ~]
  ?.  ?=(%select -.ars.u.par)
    [~ ~]
  =/  sel=bean  =(k k.r)
  =/  v=visa
    %+  fuco  visa.u.par
    ?.  sel
      look.res.u.par
    :+  ?~(d.acia.ars.u.par d.look.res.u.par u.d.acia.ars.u.par)
      ?~(b.acia.ars.u.par b.look.res.u.par u.b.acia.ars.u.par)
    ?~(f.acia.ars.u.par f.look.res.u.par u.f.acia.ars.u.par)
  =/  e=ens  (malt ~[[k u.par(visa v)]])
  =.  k  [[%b 0] k]
  |-
  =/  chi  (~(get by o) k)
  ?~  chi
    ?:  ?=(%b axis.i.k)  $(k [[%~ 0] t.k])
    [e v]
  ?.  |(?=(%border -.ars.u.chi) ?=(%text -.ars.u.chi))
    $(ager.i.k +(ager.i.k))
  =.  visa.u.chi
    %+  fuco  visa.u.chi
    ?.  sel
      look.res.u.par
    :+  ?~(d.acia.ars.u.par d.look.res.u.chi u.d.acia.ars.u.par)
      ?~(b.acia.ars.u.par b.look.res.u.chi u.b.acia.ars.u.par)
    ?~(f.acia.ars.u.par f.look.res.u.chi u.f.acia.ars.u.par)
  %=  $
    e  (~(put by e) k u.chi)
    v  (~(uni by v) visa.u.chi)
    ager.i.k  +(ager.i.k)
  ==
::
++  fuco                    :: change the style of a set of characters
  |=  [vi=visa fi=fila]
  ^-  visa
  (~(urn by vi) |=([* va=[fila @c ~]] [fi +.va]))
::
++  viso                    :: take an element and render it 
  |=  [=lar =res =ars lim=modi]
  ^-  visa
  ?+  -.ars  (rbox lar lim res)
    %text    (rtxt lar lim look.res w.size.res vox.ars)
    %border  (rbor lar lim res +.ars)
    %input   (rinp lar lim res ab.ars vox.ars)
    %layer   ~
  ==
::
++  rbox                    :: render a generic box
  |=  [=lar lim=loci =res]
  ^-  visa
  =+  [w=0 h=0 *a=visa]
  |-
  =/  x  (add x.lar w)
  =/  y  (add y.lar h)
  =/  nrow=bean  (gte +(w) w.size.res)
  ?:  |((gte h h.size.res) (gth y y.lim))
    a
  ?:  (gth x x.lim)
    $(w ?:(nrow 0 +(w)), h ?:(nrow +(h) h))
  %=  $
    w  ?:(nrow 0 +(w))
    h  ?:(nrow +(h) h)
    a  (~(put by a) [x y] [[~ +.look.res] ~-. ~])
  ==
::
++  rtxt                    :: render a text element
  |=  [=lar lim=loci =fila cols=@ud =vox]
  ^-  visa
  =+  [w=0 h=0 *a=visa]
  |-
  ?~  vox    a
  =/  x  (add x.lar w)
  =/  y  (add y.lar h)
  =/  nrow=bean  (gte +(w) cols)
  ?:  (gth y y.lim)
    a
  ?:  (gth x x.lim)
    $(w ?:(nrow 0 +(w)), h ?:(nrow +(h) h), vox t.vox)
  %=  $
    w    ?:(nrow 0 +(w))
    h    ?:(nrow +(h) h)
    a    (~(put by a) [x y] [fila (@c i.vox) ~])
    vox  t.vox
  ==
::
++  rinp                    :: render an input element
  |=  [=lar lim=loci =res ab=@ud =vox]
  ^-  visa
  =.  vox
    |-
    ?~  vox  ~
    ?:  =(0 ab)
      vox
    $(vox t.vox, ab (dec ab))
  %-  %~  uni
        by
      (rbox lar lim res)
  (rtxt lar lim look.res w.size.res vox)
::
++  rbor                    :: render a border element
  |=  [=lar lim=loci =res =ad =pila]
  ^-  visa
  =+  [w=0 h=0 *a=visa]
  ?:  ?=(%~ pila)  a
  |-
  =/  x  (add x.lar w)
  =/  y  (add y.lar h)
  =/  nrow=bean  (gte +(w) w.size.res)
  ?:  |((gte h h.size.res) (gth y y.lim))
    a
  ?:  (gth x x.lim)
    $(w ?:(nrow 0 +(w)), h ?:(nrow +(h) h))
  =/  c=@c
    ?:  ?=(%blank pila)
      ~-.
    ?-  ad
        %left
      ?:  =(0 w)
        ?:  =(0 h)
          ?-(pila %light ~-~250c., %heavy ~-~250f., %double ~-~2554.)  ::  ┌  ┏  ╔
        ?:  =(h.size.res +(h))
          ?-(pila %light ~-~2514., %heavy ~-~2517., %double ~-~255a.)  ::  └  ┗  ╚
        ?-(pila %light ~-~2502., %heavy ~-~2503., %double ~-~2551.)    ::  │  ┃  ║
      ~-.
        %right
      ?:  nrow
        ?:  =(0 h)
          ?-(pila %light ~-~2510., %heavy ~-~2513., %double ~-~2557.)  ::  ┐  ┓  ╗
        ?:  =(h.size.res +(h))
          ?-(pila %light ~-~2518., %heavy ~-~251b., %double ~-~255d.)  ::  ┘  ┛  ╝
        ?-(pila %light ~-~2502., %heavy ~-~2503., %double ~-~2551.)    ::  │  ┃  ║
      ~-.
        %top
      ?:  =(0 h)
        ?:  =(0 w)
          ?-(pila %light ~-~250c., %heavy ~-~250f., %double ~-~2554.)  ::  ┌  ┏  ╔
        ?:  nrow
          ?-(pila %light ~-~2510., %heavy ~-~2513., %double ~-~2557.)  ::  ┐  ┓  ╗
        ?-(pila %light ~-~2500., %heavy ~-~2501., %double ~-~2550.)    ::  ─  ━  ═
      ~-.
        %bottom
      ?:  =(h.size.res +(h))
        ?:  =(0 w)
          ?-(pila %light ~-~2514., %heavy ~-~2517., %double ~-~255a.)  ::  └  ┗  ╚
        ?:  nrow
          ?-(pila %light ~-~2518., %heavy ~-~251b., %double ~-~255d.)  ::  ┘  ┛  ╝
        ?-(pila %light ~-~2500., %heavy ~-~2501., %double ~-~2550.)    ::  ─  ━  ═
      ~-.
    ==
  %=  $
    w  ?:(nrow 0 +(w))
    h  ?:(nrow +(h) h)
    a  (~(put by a) [x y] [look.res c ~])
  ==
::
++  geno                    :: turn sail into session display state
  |=  x=manx
  ^-  opus
  ?:  |(=(0 x.arx.ara) =(0 y.arx.ara))
    [~ ~]
  =/  a=opus        [~ ~]
  =/  m=marl        ~[x]
  =/  k=rami        ~[[%~ 0]]
  =/  pl=fila       [~ ~ %w]
  =/  px=as         [%c x.arx.ara]
  =/  py=as         [%c y.arx.ara]
  =/  pow=flow      [%row %clip]
  =/  prx=@ud       x.arx.ara
  =/  pry=@ud       y.arx.ara
  =/  plar=lar      lar.ara
  =/  plim=modi     [(dec x.arx.ara) (dec y.arx.ara)]
  =/  pitr=iter     [0 0]
  =/  pscr=[x=bean y=bean]  [%.n %.n]
  =/  slim=$@(~ loci)  ~
  =/  vlar=lar      lar.ara
  =/  vir=[n=@ud o=@ud i=@ud]  [0 0 0]
  |-
  ?~  m  a
  =+  ^-  [=rei =aves =ars velo=mart]
    (suo g.i.m)
  =+  ^-  [bor=marl lay=marl nor=marl]
    ?:  ?=(%input -.ars)
      [~ ~ ~]
    =|  [bor=marl lay=marl nor=marl]
    |-
    ?~  c.i.m  [bor (flop lay) (flop nor)]
    ?+  n.g.i.c.i.m   $(nor [i.c.i.m nor], c.i.m t.c.i.m)
      %border-left    $(bor [i.c.i.m bor], c.i.m t.c.i.m)
      %border-right   $(bor [i.c.i.m bor], c.i.m t.c.i.m)
      %border-top     $(bor [i.c.i.m bor], c.i.m t.c.i.m)
      %border-bottom  $(bor [i.c.i.m bor], c.i.m t.c.i.m)
      %layer          $(lay [i.c.i.m lay], c.i.m t.c.i.m)
    ==
  =?  bor  &(?=(^ velo) !?=(%input -.ars))
    %+  weld  bor
    ^-  marl
    :~  [[%border-left velo] ~]
        [[%border-right velo] ~]
        [[%border-top velo] ~]
        [[%border-bottom velo] ~]
    ==
  =+  ^-  [bl=@ud br=@ud bt=@ud bb=@ud]
    (obeo bor)
  =/  wcen=bean  =(%p p.w.size.rei)
  =/  hcen=bean  =(%p p.h.size.rei)
  =?  w.size.rei  wcen
    ?:  |(=(%i p.px) =(%p p.px))
      [%i 0]
    [%c (div (mul q.w.size.rei q.px) 100)]
  =?  h.size.rei  hcen
    ?:  |(=(%i p.py) =(%p p.py))
      [%i 0]
    [%c (div (mul q.h.size.rei q.py) 100)]
  =?  t.marg.rei  =(%p p.t.marg.rei)
    ?:  |(=(%i p.h.size.rei) =(%p p.h.size.rei))
      [%c 0]
    [%c (div (mul q.t.marg.rei q.h.size.rei) 100)]
  =?  r.marg.rei  =(%p p.r.marg.rei)
    ?:  |(=(%i p.w.size.rei) =(%p p.w.size.rei))
      [%c 0]
    [%c (div (mul q.r.marg.rei q.w.size.rei) 100)]
  =?  b.marg.rei  =(%p p.b.marg.rei)
    ?:  |(=(%i p.h.size.rei) =(%p p.h.size.rei))
      [%c 0]
    [%c (div (mul q.b.marg.rei q.h.size.rei) 100)]
  =?  l.marg.rei  =(%p p.l.marg.rei)
    ?:  |(=(%i p.w.size.rei) =(%p p.w.size.rei))
      [%c 0]
    [%c (div (mul q.l.marg.rei q.w.size.rei) 100)]
  =?  t.padd.rei  =(%p p.t.padd.rei)
    ?:  |(=(%i p.h.size.rei) =(%p p.h.size.rei))
      [%c 0]
    [%c (div (mul q.t.padd.rei q.h.size.rei) 100)]
  =?  r.padd.rei  =(%p p.r.padd.rei)
    ?:  |(=(%i p.w.size.rei) =(%p p.w.size.rei))
      [%c 0]
    [%c (div (mul q.r.padd.rei q.w.size.rei) 100)]
  =?  b.padd.rei  =(%p p.b.padd.rei)
    ?:  |(=(%i p.h.size.rei) =(%p p.h.size.rei))
      [%c 0]
    [%c (div (mul q.b.padd.rei q.h.size.rei) 100)]
  =?  l.padd.rei  =(%p p.l.padd.rei)
    ?:  |(=(%i p.w.size.rei) =(%p p.w.size.rei))
      [%c 0]
    [%c (div (mul q.l.padd.rei q.w.size.rei) 100)]
  =?  q.w.size.rei  &(wcen !=(%i p.w.size.rei))
    =/  m  (add q.l.marg.rei q.r.marg.rei)
    ?:  (gth m q.w.size.rei)  0  (sub q.w.size.rei m)
  =?  q.h.size.rei  &(hcen !=(%i p.h.size.rei))
    =/  m  (add q.t.marg.rei q.b.marg.rei)
    ?:  (gth m q.h.size.rei)  0  (sub q.h.size.rei m)
  =/  imp=bean
    ?|  =(%i p.w.size.rei)
        =(%i p.h.size.rei)
    ==
  =/  repo=bean
    ?|  &(!=(0 x.flex.rei) =(%c p.px)) 
        &(!=(0 y.flex.rei) =(%c p.py))
    ==
  =/  wrap=bean
    ?&  !?=(%border -.ars)
        ?|  ?&  =([%row %wrap] pow)  =(%c p.w.size.rei)
                (gth (add q.w.size.rei (add q.l.marg.rei q.r.marg.rei)) (sub prx n.vir))
            ==
            ?&  =([%col %wrap] pow)  =(%c p.h.size.rei)
                (gth (add q.h.size.rei (add q.t.marg.rei q.b.marg.rei)) (sub pry n.vir))
    ==  ==  ==
  =/  wrim=bean
    ?&  !?=(%border -.ars)
        ?|  &(=([%row %wrap] pow) =(%i p.w.size.rei))
            &(=([%col %wrap] pow) =(%i p.h.size.rei))
    ==  ==
  =/  tvir  vir
  =?  vir  |(wrap wrim)
    ?-  d.pow
        %row
      ?:  wrap
        :-  0
        :-  i.vir
        (add (add q.h.size.rei (add q.t.marg.rei q.b.marg.rei)) i.vir)
      ?:  wrim
        :-  0
        :-  o.vir
        i.vir
      vir
        %col
      ?:  wrap
        :-  0
        :-  i.vir
        (add (add q.w.size.rei (add q.l.marg.rei q.r.marg.rei)) i.vir)
      ?:  wrim
        :-  0
        :-  o.vir
        i.vir
      vir
    ==
  =.  vlar
    ?:  ?=(%border -.ars)
      ?-  ad.ars
          %left
        vlar
          %right
        :_  y.vlar
        =/  x  (add x.vlar ?:(=(0 q.px) 0 (dec q.px)))
        =/  w  ?:(=(0 q.w.size.rei) 0 (dec q.w.size.rei))
        ?:((gth w x) 0 (sub x w))
          %top
        vlar
          %bottom
        :-  x.vlar
        =/  y  (add y.vlar ?:(=(0 q.py) 0 (dec q.py)))
        =/  h  ?:(=(0 q.h.size.rei) 0 (dec q.h.size.rei))
        ?:((gth h y) 0 (sub y h))
      ==
    =?  vlar  |(wrap wrim)
      ?-  d.pow
          %row
        :-  x.plar
        (add y.plar o.vir)
          %col
        :_  y.plar
        (add x.plar o.vir)
      ==
    :-  (add x.vlar q.l.marg.rei)
    (add y.vlar q.t.marg.rei)
  =/  alar=lar  vlar
  =.  vlar
    :-  ;:(add bl q.l.padd.rei x.vlar)
    ;:(add bt q.t.padd.rei y.vlar)
  =/  arx=@ud
    ?+  p.w.size.rei  0
        %c
      =/  w  ;:(add bl br q.l.padd.rei q.r.padd.rei)
      ?:((gth w q.w.size.rei) 0 (sub q.w.size.rei w))
        %i
      =/  w
        ;:  add
          q.l.marg.rei  ?:(=(%row d.pow) n.vir o.vir)
          bl  br  q.l.padd.rei  q.r.padd.rei
        ==
      ?:((gth w prx) 0 (sub prx w))
    ==
  =/  ary=@ud
    ?+  p.h.size.rei  0
        %c
      =/  h  ;:(add bt bb q.t.padd.rei q.b.padd.rei)
      ?:((gth h q.h.size.rei) 0 (sub q.h.size.rei h))
        %i
      =/  h
        ;:  add 
          q.t.marg.rei  ?:(=(%row d.pow) o.vir n.vir)
          bt  bb  q.t.padd.rei  q.b.padd.rei
        ==
      ?:((gth h pry) 0 (sub pry h))
    ==
  =/  alim=loci
    :-  ?:  &(x.pscr ?=(%c p.w.size.rei))
          =/  r  ;:(add 1 br q.r.padd.rei)
          ;:(add x.alar bl q.l.padd.rei ?:((gth r q.w.size.rei) 0 (sub q.w.size.rei r)))
        =/  x  ;:(add x.alar bl q.l.padd.rei ?:(=(0 arx) 0 (dec arx)))
        ?:  (gth x x.plim)
          x.plim
        x
    ?:  &(y.pscr ?=(%c p.h.size.rei))
      =/  b  ;:(add 1 bb q.b.padd.rei)
      ;:(add y.alar bt q.t.padd.rei ?:((gth b q.h.size.rei) 0 (sub q.h.size.rei b)))
    =/  y  ;:(add y.alar bt q.t.padd.rei ?:(=(0 ary) 0 (dec ary)))
    ?:  (gth y y.plim)
      y.plim
    y
  =/  nscr=[x=bean y=bean]
    :-  |(?=(%scroll -.ars) &(x.pscr ?=(%i p.w.size.rei)))
    |(?=(%scroll -.ars) &(y.pscr ?=(%i p.h.size.rei)))
  =/  nsli
    ?.  |(x.nscr y.nscr)
      slim
    ?:  ?=(~ slim)
      alim
    :-  ?:((lth x.alim x.slim) x.alim x.slim)
    ?:((lth y.alim y.slim) y.alim y.slim)
  =/  fi=fila
    :+  ?~(d.look.rei d.pl u.d.look.rei)
      ?~(b.look.rei b.pl u.b.look.rei)
    ?~(f.look.rei f.pl u.f.look.rei)
  =/  b  *opus
  =>  ?.  ?=(^ lay)
      .
    ?.  |(repo wrim)
      %_  .
        a
          %=  $
            m     lay
            k     [[%l 0] k]
            px    w.size.rei
            py    h.size.rei
            pl    fi
            pow   flow.rei
            prx   arx
            pry   ary
            plar  vlar
            plim  alim
            pscr  nscr
            slim  nsli
            vir   [0 0 0]
      ==  ==
    %_  .
      b
        %=  $
          a     *opus
          m     lay
          k     [[%l 0] k]
          px    w.size.rei
          py    h.size.rei
          pl    fi
          pow   flow.rei
          prx   arx
          pry   ary
          plar  vlar
          plim  alim
          pscr  nscr
          slim  nsli
          vir   [0 0 0]
    ==  ==
  =>  ?.  ?=(^ nor)
      .
    ?.  |(repo wrim)
      %_  .
        a
          %=  $
            m     nor
            k     [[%~ 0] k]
            px    w.size.rei
            py    h.size.rei
            pl    fi
            pow   flow.rei
            prx   arx
            pry   ary
            plar  vlar
            plim  alim
            pscr  nscr
            slim  nsli
            vir   [0 0 0]
      ==  ==
    %_  .
      b
        %=  $
          a     *opus
          m     nor
          k     [[%~ 0] k]
          px    w.size.rei
          py    h.size.rei
          pl    fi
          pow   flow.rei
          prx   arx
          pry   ary
          plar  vlar
          plim  alim
          pscr  nscr
          slim  nsli
          vir   [0 0 0]
    ==  ==
  =/  csiz=$@(~ [w=@ud h=@ud])
    ?.  |(repo imp ?=(%scroll -.ars))
      ~
    =/  i=@ud    0
    =/  ax=axis  %l
    =/  rig=@ud  x.vlar
    =/  bot=@ud  y.vlar
    |-
    =/  el  (~(get by ?:(|(repo wrim) ens.b ens.a)) [[ax i] k])
    ?:  ?=(^ el)
      =/  el-r=@ud
        %+  add  x.lar.u.el
        %+  add  r.marg.res.u.el
        ?:(=(0 w.size.res.u.el) 0 (dec w.size.res.u.el))
      =/  el-b=@ud
        %+  add  y.lar.u.el
        %+  add  b.marg.res.u.el
        ?:(=(0 h.size.res.u.el) 0 (dec h.size.res.u.el))
      =?  rig  (gth el-r rig)  el-r
      =?  bot  (gth el-b bot)  el-b
      $(i +(i))
    ?:  ?=(%l ax)
      $(ax %~, i 0)
    :-  ?:  (gth x.vlar +(rig))
          0
        (sub +(rig) x.vlar)
    ?:  (gth y.vlar +(bot))
      0
    (sub +(bot) y.vlar)
  =?  size.rei  &(imp ?=(^ csiz))
    :-  ?:  =(%i p.w.size.rei)  
          [%c ;:(add bl br q.l.padd.rei q.r.padd.rei w.csiz)]
        w.size.rei
    ?:  =(%i p.h.size.rei)  
      [%c ;:(add bt bb q.t.padd.rei q.b.padd.rei h.csiz)]
    h.size.rei
  =/  wris=bean
    ?&  wrim
      ?|  (gth n.vir prx)  (gth n.vir pry)
          ?&  =(%row d.pow)
              (gth (add q.w.size.rei (add q.l.marg.rei q.r.marg.rei)) (sub prx n.tvir))
          ==
          ?&  =(%col d.pow)
              (gth (add q.h.size.rei (add q.t.marg.rei q.b.marg.rei)) (sub pry n.tvir))
          ==
    ==  ==
  =?  vir  wrim
    ?:  wris
      ?-  d.pow
          %row
        :-  0
        :-  i.vir
        (add i.vir (add q.h.size.rei (add q.t.marg.rei q.b.marg.rei)))
          %col
        :-  0
        :-  i.vir
        (add i.vir (add q.w.size.rei (add q.l.marg.rei q.r.marg.rei)))
      ==
    tvir
  =?  vlar  wrim
    ?:  wris
      ?-  d.pow
          %row
        :-  x.vlar
        (add y.vlar (sub i.tvir o.tvir))
          %col
        :_  y.vlar
        (add x.vlar (sub i.tvir o.tvir))
      ==
    ?-  d.pow
        %row
      :_  y.vlar
      (add x.vlar n.tvir)
        %col
      :-  x.vlar
      (add y.vlar n.tvir)
    ==
  =?  alar  wrim
    ?:  wris
      ?-  d.pow
          %row
        :-  x.alar
        (add y.alar (sub i.tvir o.tvir))
          %col
        :_  y.alar
        (add x.alar (sub i.tvir o.tvir))
      ==
    ?-  d.pow
        %row
      :_  y.alar
      (add x.alar n.tvir)
        %col
      :-  x.alar
      (add y.alar n.tvir)
    ==
  =?  arx  wrim
    =/  bp  ;:(add bl br q.l.padd.rei q.r.padd.rei)
    ?:((gth bp q.w.size.rei) 0 (sub q.w.size.rei bp))
  =?  ary  wrim
    =/  bp  ;:(add bt bb q.t.padd.rei q.b.padd.rei)
    ?:((gth bp q.h.size.rei) 0 (sub q.h.size.rei bp))
  =?  alim  |(wrim x.pscr y.pscr)
    :-  ?:  x.pscr
          =/  r  ;:(add 1 br q.r.padd.rei)
          ;:(add x.alar bl q.l.padd.rei ?:((gth r q.w.size.rei) 0 (sub q.w.size.rei r)))
        ?:  wrim
          =/  x  (add x.vlar ?:(=(0 arx) 0 (dec arx)))
          ?:  (gth x x.plim)
            x.plim
          x
        x.alim
    ?:  y.pscr
      =/  b  ;:(add 1 bb q.b.padd.rei)
      ;:(add y.alar bt q.t.padd.rei ?:((gth b q.h.size.rei) 0 (sub q.h.size.rei b)))
    ?:  wrim
      =/  y  (add y.vlar ?:(=(0 ary) 0 (dec ary)))
      ?:  (gth y y.plim)
        y.plim
      y
    y.alim
  =?  b  &(|(repo wrim) ?=(^ csiz))
    |^
    =/  movx=@ud
      ?:  wrim
        ?:  wris
          ?-  d.pow
              %row
            ?:  (gth w.csiz arx)  0
            (div (mul x.flex.rei (sub arx w.csiz)) 100)
              %col
            %+  add  (sub i.tvir o.tvir)
            ?:  (gth w.csiz arx)  0
            (div (mul x.flex.rei (sub arx w.csiz)) 100)
          ==
        ?-  d.pow
            %row
          %+  add  n.tvir
          ?:  (gth w.csiz arx)  0
          (div (mul x.flex.rei (sub arx w.csiz)) 100)
            %col
          ?:  (gth w.csiz arx)  0
          (div (mul x.flex.rei (sub arx w.csiz)) 100)
        ==
      ?:  (gth w.csiz arx)  0
      (div (mul x.flex.rei (sub arx w.csiz)) 100)
    =/  movy=@ud
      ?:  wrim
        ?:  wris
          ?-  d.pow
              %row
            %+  add  (sub i.tvir o.tvir)
            ?:  (gth h.csiz ary)  0
            (div (mul y.flex.rei (sub ary h.csiz)) 100)
              %col
            ?:  (gth h.csiz ary)  0
            (div (mul y.flex.rei (sub ary h.csiz)) 100)
          ==
        ?-  d.pow
            %row
          ?:  (gth h.csiz ary)  0
          (div (mul y.flex.rei (sub ary h.csiz)) 100)
            %col
          %+  add  n.tvir
          ?:  (gth h.csiz ary)  0
          (div (mul y.flex.rei (sub ary h.csiz)) 100)
        ==
      ?:  (gth h.csiz ary)  0
      (div (mul y.flex.rei (sub ary h.csiz)) 100)
    ?:  &(=(0 movx) =(0 movy))
      b
    =.  ens.b
      =/  ax=axis  %b
      =/  i=@ud    0
      |-
      =/  el  (~(get by ens.b) [[ax i] k])
      ?~  el
        ?:  ?=(%b ax)  $(ax %l, i 0)
        ?:  ?=(%l ax)  $(ax %~, i 0)
        ens.b
      =.  ens.b  $(ax %b, i 0, k [[ax i] k])
      =:  visa.u.el    (muto movx movy ~(tap by visa.u.el))
          x.lar.u.el   (add x.lar.u.el movx)
          y.lar.u.el   (add y.lar.u.el movy)
          x.modi.u.el  (add x.modi.u.el movx)
          y.modi.u.el  (add y.modi.u.el movy)
        ==
      $(ens.b (~(put by ens.b) [[ax i] k] u.el), i +(i))
    =.  visa.b  (muto movx movy ~(tap by visa.b))
    b
    ++  muto
      |=  [movx=@ud movy=@ud v=(list [=loci fila @c ~])]
      ^-  visa
      %-  %~  dif
          by
        %-  malt
        |-
        ?~  v  ~
        =/  x  (add x.loci.i.v movx)
        =/  y  (add y.loci.i.v movy)
        ?:  |((gth x x.alim) (gth y y.alim) &(?=(^ slim) |((gth x x.slim) (gth y y.slim))))
          $(v t.v)
        [[[x y] +.i.v] $(v t.v)]
      visa.a
    --
  =?  a  |(repo wrim)
    :-  (~(uni by ens.b) ens.a)
    (~(uni by visa.b) visa.a)
  =/  ares=res
    ?+  -.ars
      :*  [q.w.size.rei q.h.size.rei]
          [q.l.padd.rei q.r.padd.rei q.t.padd.rei q.b.padd.rei]
          [q.l.marg.rei q.r.marg.rei q.t.marg.rei q.b.marg.rei]
          flex.rei
          flow.rei
          fi
      ==
        %text
      =/  len  (lent vox.ars)
      =/  lim  (sub arx ?:(?=(%row d.pow) n.vir o.vir))
      :*  :-  ?:((lth len lim) len lim)
            ?:  =(0 lim)  0
            ?:(=(0 (mod len lim)) (div len lim) +((div len lim)))
          [0 0 0 0]
          [0 0 0 0]
          [0 0]
          [%row %wrap]
          pl
      ==
    ==
  =?  alim  &(|(x.pscr y.pscr) ?=(%text -.ars))
    :-  ?:(x.pscr (add x.alar ?:(=(0 w.size.ares) 0 (dec w.size.ares))) x.alim)
    ?:(y.pscr (add y.alar ?:(=(0 h.size.ares) 0 (dec h.size.ares))) y.alim)
  =?  a  ?=(^ bor)
    %=  $
      m     bor
      k     [[%b 0] k]
      px    w.size.rei
      py    h.size.rei
      pl    fi
      pow   flow.ares
      prx   w.size.ares
      pry   h.size.ares
      plar  alar
      plim  =?  plim  |(x.pscr y.pscr)  [?:(x.pscr x.alim x.plim) ?:(y.pscr y.alim y.plim)]
            :-  =/  x  (add x.alim ;:(add bl br l.padd.ares r.padd.ares))
                ?:((gth x x.plim) x.plim x)
            =/  y  (add y.alim ;:(add bt bb t.padd.ares b.padd.ares))
            ?:((gth y y.plim) y.plim y)
      vlar  alar
      vir   [0 0 0]
    ==
  =?  ars  |(?=(%input -.ars) ?=(%scroll -.ars))
    ?:  ?=(%input -.ars)
      =/  old  (~(get by ens.ara) k)
      ?~  old  ars
      ?.  ?=(%input -.ars.u.old)
        ars
      ars.u.old
    ?:  ?=(%scroll -.ars)
      =/  sol=sola
        ?~  csiz  [0 0]
        :-  ?:((gth arx w.csiz) 0 (sub w.csiz arx))
        ?:((gth ary h.csiz) 0 (sub h.csiz ary))
      =/  old  (~(get by ens.ara) k)
      =/  itr=iter
        ?~  old
          [0 0]
        ?.  ?=(%scroll -.ars.u.old)
          [0 0]
        :-  ?:((gth x.iter.ars.u.old x.sol) x.sol x.iter.ars.u.old)
        ?:((gth y.iter.ars.u.old y.sol) y.sol y.iter.ars.u.old)
      [%scroll itr [bl br bt bb] sol]
    ars
  =?  a  &(?=(%scroll -.ars) !&(=(0 x.iter.ars) =(0 y.iter.ars)))
    =/  pv=visa  (~(dif by (rbox alar plim ares)) visa.a)
    =/  opu=opus
      (eo ens.a visa.a ~ iter.ars k pv x.vlar y.vlar x.alim y.alim)
    =?  opu  ?=(^ rex.ara)
      =/  duc=opus  (duco ens.opu k.rex.ara rex.ara)
      [(~(uni by ens.opu) ens.duc) (~(uni by visa.opu) visa.duc)]
    opu
  =/  rend=visa
    %:  viso
      alar
      ares
      ars
      =?  plim  |(x.pscr y.pscr)  [?:(x.pscr x.alim x.plim) ?:(y.pscr y.alim y.plim)]
      ?:  ?=(~ slim)
        plim
      [?:((lth x.slim x.plim) x.slim x.plim) ?:((lth y.slim y.plim) y.slim y.plim)]
    ==
  =?  rend  !?=(%layer -.ars)
    (~(dif by rend) visa.a)
  =?  vir  !?=(%layer -.ars)
    ?:  ?=(%border -.ars)
      [0 0 0]
    =/  el-x  (add w.size.ares (add l.marg.ares r.marg.ares))
    =/  el-y  (add h.size.ares (add t.marg.ares b.marg.ares))
    ?-  d.pow
        %row
      :+  (add n.vir el-x)
        o.vir
      ?:((gth el-y (sub i.vir o.vir)) (add o.vir el-y) i.vir)
        %col
      :+  (add n.vir el-y)
        o.vir
      ?:((gth el-x (sub i.vir o.vir)) (add o.vir el-x) i.vir)
    ==
  =?  vlar  !?=(%layer -.ars)
    ?:  ?=(%border -.ars)
      plar
    =/  vx  ?-(d.pow %row n.vir, %col o.vir)
    =/  vy  ?-(d.pow %row o.vir, %col n.vir)
    [(add x.plar vx) (add y.plar vy)]
  =.  a
    =?  plim  |(x.pscr y.pscr)  [?:(x.pscr x.alim x.plim) ?:(y.pscr y.alim y.plim)]
    :-  (~(put by ens.a) k [ares rend alar plim pitr aves ars])
    (~(uni by visa.a) rend)
  =?  a  &(?=(%select -.ars) ?=(^ rex.ara) =(k k.rex.ara))
    =/  opu=opus  (duco ens.a k rex.ara)
    a(ens (~(uni by ens.a) ens.opu), visa (~(uni by visa.a) visa.opu))
  %=  $
    m     t.m
    k     [[axis.i.k +(ager.i.k)] t.k]
  ==
::  ::  ::  ::  ::  ::  ::
++  dico                    :: derive hotkey and navigation context from display state
  |=  e=ens
  ^-  cura
  ?:  |(=(0 x.arx.ara) =(0 y.arx.ara))
    [~ ~ ~ rex.ara]
  =/  k=rami                [[%~ 0] ~]
  =/  rk=$@(~ rami)         ?~(rex.ara ~ k.rex.ara)
  =/  plim=modi             [(dec x.arx.ara) (dec y.arx.ara)]
  =/  acc=[rend=bean cura]  [%.n ~ ~ ~ rex.ara]
  =<  +
  |-  ^-  [bean cura]
  =/  el  (~(get by e) k)
  ?~  el
    ?:  ?=(%b axis.i.k)  $(k [[%l 0] t.k])
    ?:  ?=(%l axis.i.k)  $(k [[%~ 0] t.k])
      acc
  =/  nacc=[rend=bean cura]
    %=  $
      k         [[%b 0] k]
      rend.acc  %.n
      plim      :-  ?:((gth x.modi.u.el x.plim) x.plim x.modi.u.el)
                ?:((gth y.modi.u.el y.plim) y.plim y.modi.u.el)
    ==
  =.  rend.nacc  |(?=(^ visa.u.el) rend.nacc)
  ?:  !rend.nacc
    $(ager.i.k +(ager.i.k), acc nacc(rend |(rend.acc rend.nacc)))
  =.  nacc
    =/  sel=(unit @t)  (~(get by aves.u.el) %sel)
    =/  nav=bean
      ?|  &(?=(^ sel) !?=(%layer -.ars.u.el))
          ?=(%select -.ars.u.el)
          ?=(%input -.ars.u.el)
          ?=(%scroll -.ars.u.el)
      ==
    =?  ordo.nacc  nav
      :_  ordo.nacc
      :*  ?:((gth x.iter.u.el x.lar.u.el) 0 (sub x.lar.u.el x.iter.u.el))
          =/  r  (add x.lar.u.el ?:(=(0 w.size.res.u.el) 0 (dec w.size.res.u.el)))
          ?:((gth x.iter.u.el r) 0 (sub r x.iter.u.el))
          ?:((gth y.iter.u.el y.lar.u.el) 0 (sub y.lar.u.el y.iter.u.el))
          =/  b  (add y.lar.u.el ?:(=(0 h.size.res.u.el) 0 (dec h.size.res.u.el)))
          ?:((gth y.iter.u.el b) 0 (sub b y.iter.u.el))
          k
      ==
    =?  rex.nacc  &(?=(^ ordo.nacc) =(rk k.i.ordo.nacc))
      i.ordo.nacc
    =?  equi.nacc  ?=(%scroll -.ars.u.el)
      (~(put in equi.nacc) k)
    =?  omen.nacc  &(nav !(~(has by omen.nacc) [%aro %l]))
      (~(uni by omen.nacc) hnav)
    =?  omen.nacc  |(?=(%input -.ars.u.el) ?=(%scroll -.ars.u.el))
      (~(uni by omen.nacc) htog)
    nacc
  $(ager.i.k +(ager.i.k), acc nacc(rend |(rend.acc rend.nacc)))
::  ::  ::  ::  ::  ::  ::  ::
::  ::  ::  ::  ::  ::  ::  ::
::  ::  ::  ::  ::  ::  ::  ::
::  dill test:
::  
++  push-blit
  |=  v=visa
  ^-  lux
  =/  y=@ud  0
  :-  %mor
  ^-  (list blit:dill)
  :-  [%clr ~]
  :-  [%nel ~]
  |-
  ?:  =(+(y) y.urbs)
    [[%klr (make-stub y v)] ~]
  :-  [%klr (make-stub y v)]
  [[%nel ~] $(y +(y))]
::
++  put-blit
  |=  v=visa
  ^-  lux
  =/  y=@ud  0
  :-  %mor
  ^-  (list blit:dill)
  |-
  :-  [%hop 0 y]
  :-  [%klr (make-stub y v)] 
  ?:(=(+(y) y.urbs) ~ $(y +(y)))
::
++  make-stub
  |=  [y=@ud v=visa]
  ^-  stub
  =/  x=@ud  0
  |-
  ?:  (gte x x.urbs)
    ~
  =/  val  (~(get by v) [x y])
  =/  char=(pair stye (list @c))
    ?~(val [[~ ~ ~] ~[(@c 'x')]] u.val)
  [char $(x +(x))]
::
++  dono                   :: display update diff
  |=  [old=visa new=visa]
  ^-  lux
  =/  n=(list [=loci [fila @c ~]])  ~(tap by new)
  :-  %mor
  |-
  ?~  n  ~
  =/  v  (~(get by old) loci.i.n)
  ?.  |(&(?=(^ v) !=(u.v +.i.n)) ?=(~ v))
    $(n t.n)
  [[%hop loci.i.n] [%klr ~[+.i.n]] $(n t.n)]
--
