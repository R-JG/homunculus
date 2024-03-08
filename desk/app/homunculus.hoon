::
::  O┬┬ ┬┌─┐┌┬┐┬ ┬┌┐┌┌─┐┬ ┬┬  ┬ ┬┌─┐
::  ┌┘├─┤│ │││││ │││││  │ ││  │ │└─┐
::  ┴O┴ ┴└─┘┴ ┴└─┘┘└┘└─┘└─┘┴─┘└─┘└─┘
::
|%
+$  ens   (map rami [=res =visa =lar =aves =ars])
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
+$  vox   tape
+$  loci  [x=@ud y=@ud]
+$  lar   loci
+$  axis  ?(%xy %z)
+$  ager  @ud
+$  aves  (map ?(%sel %act) @t)
+$  iter  [x=@ud y=@ud]
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
+$  dux   [l=@ud r=@ud t=@ud b=@ud k=rami]
+$  rex   $@(~ dux)
+$  ordo  (list dux)
+$  gens  (map @t rami) :: todo: element id to ens key
::
+$  cura  [=omen =ordo]
+$  opus  [=ens =visa]
+$  as    $%((pair %c @ud) (pair %p @ud) (pair %i @ud))
+$  rei
  $:  size=[w=as h=as]
      padd=[l=as r=as t=as b=as]
      marg=[l=as r=as t=as b=as]
      =flex
      =flow
      look=acia
  ==
+$  lux   dill-blit:dill
+$  flex  [x=@ud y=@ud]
+$  flow  [d=?(%col %row) b=?(%wrap %clip)]
::
+$  arca  $~([50 10] [x=@ud y=@ud])
+$  vela  manx
+$  ara
  $:  =ens  =visa  =vela
      =omen  =rex  =ordo
  ==
+$  arae  (map @tas ara)
+$  ego
  $:  =arca  =ara
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
  !>(arca)
++  on-load
  |=  old=vase
  ^-  (quip card _hoc)
  [~ hoc(ego =+(*^ego -(arca !<(^^arca old))))]
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
    [~ hoc(ara [ens.gen visa.gen elem omen.dic ~ ordo.dic])]
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
    ?:  ?=(%rez -.z)                        ::  resize arca to new terminal dimensions
      =/  gen=opus  (geno vela.ara)
      =/  dic=cura  (dico ens.gen)
      =/  =lux  (put-blit visa.gen)
      :_  hoc(arca [(@ -.+.z) (@ +.+.z)], ara ara(ens ens.gen, visa visa.gen, ordo ordo.dic))
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
    =/  =lux  (put-blit visa.ara)
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
::  ::  ::  ::  ::  ::  ::    ::  ::  ::  ::  ::  ::  ::    ::  ::  ::  ::  ::  ::  ::
::  ::  ::  ::  ::  ::  ::    ::  ::  ::  ::  ::  ::  ::    ::  ::  ::  ::  ::  ::  ::
::  ::  ::  ::  ::  ::  ::    ::  ::  ::  ::  ::  ::  ::    ::  ::  ::  ::  ::  ::  ::
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
++  hscr                    :: hotkey context group for a scroll element
  ^-  omen
  %-  malt
  ^-  (list [nota lex])
  :~  [[%aro %l] %scr-l]  [[%aro %r] %scr-r]
      [[%aro %u] %scr-u]  [[%aro %d] %scr-d]
  ==
::  ::  ::
++  novo                    :: handle an event from the hotkey context
  |=  [=lex =zona]
  ^-  (quip card ^ego)
  ::
  ?:  |(?=(%nav-l lex) ?=(%nav-r lex) ?=(%nav-u lex) ?=(%nav-d lex))
    |^
    ?~  ordo.ara  [~ ego]
    =/  next=rex
      ?~  rex.ara
        =/  fake=dux
          [0 (dec x.arca) 0 (dec y.arca) `rami`[(rear k.i.ordo.ara) ~]]
        =.  lex
          ?:  =(%nav-l lex)
            %nav-r
          ?:  =(%nav-u lex)
            %nav-d
          lex
        =/  i=ordo
          %+  sort  ordo.ara
          |=  [a=dux b=dux]
          (lth (pyth a fake) (pyth b fake))
        ?~  i
          rex.ara
        i.i
      =/  i=ordo
        ?.  |(=(%nav-r lex) =(%nav-d lex))
          ~
        %+  sort
          ^-  ordo
          %+  skim  `ordo`ordo.ara
          |=  =dux
          (chil k.rex.ara k.dux)
        |=  [a=dux b=dux]
        (lth (pyth a rex.ara) (pyth b rex.ara))
      ?:  ?=(^ i)
        i.i
      =/  ii=ordo
        %+  sort  `ordo`(skim `ordo`ordo.ara (keep rex.ara))
        |=  [a=dux b=dux]
        (lth (pyth a rex.ara) (pyth b rex.ara))
      ?~  ii
        rex.ara
      =/  iii=ordo
        %+  sort
          ^-  ordo
          %+  skim  `ordo`ordo.ara
          |=  =dux
          ^-  bean
          ?:  =(k.dux k.rex.ara)
            |
          ?:  (chil k.dux k.i.ii)
            !(chil k.dux k.rex.ara)
          |
        |=  [a=dux b=dux]
        (lth (lent k.a) (lent k.b))
      ?~  iii
        i.ii
      ?:  ((keep rex.ara) i.iii)
        i.iii
      rex.ara
    ?:  =(rex.ara next)
      [~ ego]
    =/  prae=opus
      ?:  ?=(~ rex.ara)
        [~ ~]
      (duco ens.ara k.rex.ara next)
    =.  rex.ara  next
    =/  post=opus
      ?:  ?=(~ next)
        [~ ~]
      (duco ens.ara k.next rex.ara)
    =.  ens.ara  (~(uni by ens.ara) (~(uni by ens.prae) ens.post))
    =.  visa.ara  (~(uni by visa.ara) (~(uni by visa.prae) visa.post))
    =/  =lux
      :-  %mor
      :~  (blit:dill (put-blit visa.ara))
          :-  %hop
          ?~  rex.ara
            [0 0]
          [l.rex.ara t.rex.ara]
      ==
    :_  ego
    [[%give %fact ~[/dill/$] %dill-blit !>(lux)] ~]
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
        ?:  &(=(%z axis.i.t.a) =(%z axis.i.t.b))
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
      =<  -
      ?+  lex  0
          %nav-l
        ?:  |((lap dux rex) !=(~ (abov k.rex k.dux)))
          %-  sqt  %+  add
            (pow (sub l.rex l.dux) 2)
          (pow (mul ?:((gte t.rex t.dux) (sub t.rex t.dux) (sub t.dux t.rex)) 6) 2)
        %-  sqt  %+  add
          (pow (sub l.rex r.dux) 2)
        (pow (mul ?:((gte t.rex t.dux) (sub t.rex t.dux) (sub t.dux t.rex)) 6) 2)
          %nav-u
        ?:  |((lap dux rex) !=(~ (abov k.rex k.dux)))
          %-  sqt  %+  add
            (pow (mul ?:((gte l.dux l.rex) (sub l.dux l.rex) (sub l.rex l.dux)) 2) 2)
          (pow (mul (sub t.rex t.dux) 2) 2)
        %-  sqt  %+  add
          (pow (mul ?:((gte l.dux l.rex) (sub l.dux l.rex) (sub l.rex l.dux)) 2) 2)
        (pow (mul (sub t.rex b.dux) 2) 2)
          %nav-r
        ?:  |((lap dux rex) !=(~ (abov k.rex k.dux)))
          %-  sqt  %+  add
            (pow (sub l.dux l.rex) 2)
          (pow (mul ?:((gte t.dux t.rex) (sub t.dux t.rex) (sub t.rex t.dux)) 6) 2)
        %-  sqt  %+  add
          (pow (sub l.dux r.rex) 2)
        (pow (mul ?:((gte t.dux t.rex) (sub t.dux t.rex) (sub t.rex t.dux)) 6) 2)
          %nav-d
        ?:  |((lap dux rex) !=(~ (abov k.rex k.dux)))
          %-  sqt  %+  add
            (pow (mul ?:((gte l.rex l.dux) (sub l.rex l.dux) (sub l.dux l.rex)) 2) 2)
          (pow (mul (sub t.dux t.rex) 2) 2)
        %-  sqt  %+  add
          (pow (mul ?:((gte l.rex l.dux) (sub l.rex l.dux) (sub l.dux l.rex)) 2) 2)
        (pow (mul (sub t.dux b.rex) 2) 2)
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
        !>((vado ab.ars.u.el i.ars.u.el w.size.res.u.el lar.u.el))
      ==  ==
        %scroll
      ?.  ?=(%nav-l u.ev)
        [~ ego(omen.ara (~(uni by omen.ara) hnav))]
      [~ ego(omen.ara (~(uni by omen.ara) hscr))]
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
    =.  vi  (~(int by visa.u.el) vi)
    =.  visa.u.el  vi
    :_  ego(ens.ara (~(put by ens.ara) k.rex.ara u.el), visa.ara (~(uni by visa.ara) vi))
    :~  :*  %give  %fact  ~[/dill/$]  %dill-blit
      !>  :-  %mor
      :~  (dono visa.ara vi)
      (vado ab.ars.u.el i.ars.u.el w.size.res.u.el lar.u.el)
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
    =.  vi  (~(int by visa.u.el) vi)
    =.  visa.u.el  vi
    :_  ego(ens.ara (~(put by ens.ara) k.rex.ara u.el), visa.ara (~(uni by visa.ara) vi))
    :~  :*  %give  %fact  ~[/dill/$]  %dill-blit
      !>  :-  %mor
      :~  (dono visa.ara vi)
      (vado ab.ars.u.el i.ars.u.el w.size.res.u.el lar.u.el)
    ==  ==  ==
  ::
  ?:  |(?=(%cur-l lex) ?=(%cur-r lex) ?=(%cur-u lex) ?=(%cur-d lex))
    ?~  rex.ara  [~ ego]
    =/  el  (~(get by ens.ara) k.rex.ara)
    ?~  el  [~ ego]
    ?.  ?=(%input -.ars.u.el)  [~ ego]
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
        ?:  (gth i l)
          l
        i
      ==
    =/  nab
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
        !>((vado ab.ars.u.el i.ars.u.el w.size.res.u.el lar.u.el))
      ==  ==
    =.  ab.ars.u.el  nab
    =/  lim  [(add x.lar.u.el w.size.res.u.el) (add y.lar.u.el h.size.res.u.el)]
    =/  vi=visa  (rinp lar.u.el lim res.u.el ab.ars.u.el vox.ars.u.el)
    =.  vi  (~(int by visa.u.el) vi)
    =.  visa.u.el  vi
    :_  ego(ens.ara (~(put by ens.ara) k.rex.ara u.el), visa.ara (~(uni by visa.ara) vi))
    :~  :*  %give  %fact  ~[/dill/$]  %dill-blit
      !>  :-  %mor
      :~  (dono visa.ara vi)
      (vado ab.ars.u.el i.ars.u.el w.size.res.u.el lar.u.el)
    ==  ==  ==
  ::
  ?:  |(?=(%scr-l lex) ?=(%scr-r lex) ?=(%scr-u lex) ?=(%scr-d lex))
    ?~  rex.ara  [~ ego]
    =/  par  (~(get by ens.ara) k.rex.ara)
    ?~  par  [~ ego]
    ?.  ?=(%scroll -.ars.u.par)  [~ ego]
    ?:  ?|  &(?=(%scr-l lex) =(0 x.iter.ars.u.par))
        &(?=(%scr-r lex) =(x.sola.ars.u.par x.iter.ars.u.par))
        &(?=(%scr-u lex) =(0 y.iter.ars.u.par))
        &(?=(%scr-d lex) =(y.sola.ars.u.par y.iter.ars.u.par))
    ==
      [~ ego]
    =.  iter.ars.u.par
      ?+  lex  iter.ars.u.par
        %scr-l  ?:(=(0 x.iter.ars.u.par) iter.ars.u.par [(dec x.iter.ars.u.par) y.iter.ars.u.par])
        %scr-r  [+(x.iter.ars.u.par) y.iter.ars.u.par]  
        %scr-u  ?:(=(0 y.iter.ars.u.par) iter.ars.u.par [x.iter.ars.u.par (dec y.iter.ars.u.par)])
        %scr-d  [x.iter.ars.u.par +(y.iter.ars.u.par)]
      ==
    =/  plef  ;:(add x.lar.u.par l.muri.ars.u.par l.padd.res.u.par)
    =/  prig
      =/  r  ;:(add 1 r.muri.ars.u.par r.padd.res.u.par)
      (add x.lar.u.par ?:((gth r w.size.res.u.par) 0 (sub w.size.res.u.par r)))
    =/  ptop  ;:(add y.lar.u.par t.muri.ars.u.par t.padd.res.u.par)
    =/  pbot
      =/  b  ;:(add 1 b.muri.ars.u.par b.padd.res.u.par)
      (add y.lar.u.par ?:((gth b h.size.res.u.par) 0 (sub h.size.res.u.par b)))
    =/  olv=visa
      =/  k=rami  [[%z 0] k.rex.ara]
      =/  a=visa  visa.u.par
      |-
      =/  chi  (~(get by ens.ara) k)
      ?~  chi
        ?:  ?=(%xy axis.i.k)
          a
        $(k [[%xy 0] t.k])
      =.  a  (~(uni by a) visa.u.chi)
      $(ager.i.k +(ager.i.k), a $(k [[%z 0] k]))
    =/  op=opus
      =/  k=rami  [[%z 0] k.rex.ara]
      =/  a=opus  [~ ~]
      |-
      =/  chi  (~(get by ens.ara) k)
      ?~  chi
        ?:  ?=(%xy axis.i.k)
          a
        $(k [[%xy 0] t.k])
      ?+  lex  $(ager.i.k +(ager.i.k), a $(k [[%z 0] k]))
          %scr-l
        a
          %scr-r
        a
          %scr-u
        a
          %scr-d
        =.  a  $(k [[%z 0] k])
        =/  cbot  (add y.lar.u.chi ?:(=(0 h.size.res.u.chi) 0 (dec h.size.res.u.chi)))
        ?:  ?|  (gth y.iter.ars.u.par cbot)
                (lth (sub cbot y.iter.ars.u.par) ptop)
                &((lte y.iter.ars.u.par y.lar.u.chi) (gth (sub y.lar.u.chi y.iter.ars.u.par) pbot))
            ==
          =.  ens.a  (~(put by ens.a) k u.chi(visa ~))
          $(ager.i.k +(ager.i.k))
        =/  lim  [(add x.lar.u.chi w.size.res.u.chi) (add y.lar.u.chi h.size.res.u.chi)]
        =/  vi=visa  (viso lar.u.chi res.u.chi ars.u.chi lim)  :: FIX --> NEED ACTUAL LIM (overflow cannot be handled otherwise since I am not doing a one to one int by element)
        =.  vi
          %-  %~  rep
                by
              vi
          |=  [[l=loci c=[fila @c ~]] acc=visa]
          ?:  (gth y.iter.ars.u.par y.l)
            acc
          =.  y.l  (sub y.l y.iter.ars.u.par)
          ?:  (lth y.l ptop)
            acc
          (~(put by acc) l c)
        =.  vi  (~(int by olv) (~(dif by vi) visa.a))
        =.  ens.a  (~(put by ens.a) k u.chi(visa vi))
        =.  visa.a  (~(uni by visa.a) vi)
        $(ager.i.k +(ager.i.k))
      ==
    =/  pend
      :-  ?:(=(0 w.size.res.u.par) 0 (dec w.size.res.u.par))
      ?:(=(0 h.size.res.u.par) 0 (dec h.size.res.u.par))
    =/  pvi  (~(dif by (~(int by olv) (rbox lar.u.par pend res.u.par))) visa.op)
    =.  visa.u.par  pvi
    =:  visa.op  (~(uni by pvi) visa.op)
        ens.op   (~(put by ens.op) k.rex.ara u.par)
      ==
    =:  ens.ara   (~(uni by ens.ara) ens.op)
        visa.ara  (~(uni by visa.ara) visa.op)
      ==
    :_  ego
    [[%give %fact ~[/dill/$] %dill-blit !>((put-blit visa.ara))] ~]
  ::
  [~ ego]
::
++  vado                    :: move the cursor to a given input character index
  |=  [ab=@ud i=@ud w=@ud =lar]
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
  [%hop (add x.lar col) (add y.lar row)]
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
  |=  [e=ens k=rami r=rex]  :: returns just the ens and visa of the updated elements
  ^-  opus
  ?~  r  [~ ~]
  =/  par  (~(get by e) k)
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
  =.  visa.u.par  v
  =:  e  (~(put by e) k u.par)
      k  [[%xy 0] k]
    ==
  |-
  =/  chi  (~(get by e) k)
  ?~  chi  [e v]
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
  |=  [=lar =res =ars lim=loci]
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
  ?:  |((gth +(x) x.arca) (gth x x.lim))
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
  ?:  |((gth +(x) x.arca) (gth x x.lim))
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
  ?:  |((gth +(x) x.arca) (gth x x.lim))
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
  ?:  |(=(0 x.arca) =(0 y.arca))
    [~ ~]
  =/  a=opus        [~ ~]
  =/  m=marl        ~[x]
  =/  k=rami        ~[[%xy 0]]
  =/  cc=@ud        0
  =/  cl=@ud        0
  =/  pl=fila       [~ ~ %w]
  =/  px=as         [%c x.arca]
  =/  py=as         [%c y.arca]
  =/  pex=flex      [0 0]
  =/  pow=flow      [%row %clip]
  =/  prx=@ud       x.arca
  =/  pry=@ud       y.arca
  =/  plar=lar      [0 0]
  =/  plim=loci     [(dec x.arca) (dec y.arca)]
  =/  vlar=lar      [0 0]
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
    :-  =/  x  ;:(add x.alar bl q.l.padd.rei ?:(=(0 arx) 0 (dec arx)))
        ?:  (gth x x.plim)
          x.plim
        x
    =/  y  ;:(add y.alar bt q.t.padd.rei ?:(=(0 ary) 0 (dec ary)))
    ?:  (gth y y.plim)
      y.plim
    y
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
          %_  $
            m     lay
            k     [[%z 0] k]
            cc    0
            cl    0
            px    w.size.rei
            py    h.size.rei
            pl    fi
            pex   flex.rei
            pow   flow.rei
            prx   arx
            pry   ary
            plar  vlar
            plim  alim
            vir   [0 0 0]
      ==  ==
    %_  .
      b
        %_  $
          a     *opus
          m     lay
          k     [[%z 0] k]
          cc    0
          cl    0
          px    w.size.rei
          py    h.size.rei
          pl    fi
          pex   flex.rei
          pow   flow.rei
          prx   arx
          pry   ary
          plar  vlar
          plim  alim
          vir   [0 0 0]
    ==  ==
  =>  ?.  ?=(^ nor)
      .
    ?.  |(repo wrim)
      %_  .
        a
          %_  $
            m     nor
            k     [[%xy 0] k]
            cc    0
            cl    0
            px    w.size.rei
            py    h.size.rei
            pl    fi
            pex   flex.rei
            pow   flow.rei
            prx   arx
            pry   ary
            plar  vlar
            plim  alim
            vir   [0 0 0]
      ==  ==
    %_  .
      b
        %_  $
          a     *opus
          m     nor
          k     [[%xy 0] k]
          cc    0
          cl    0
          px    w.size.rei
          py    h.size.rei
          pl    fi
          pex   flex.rei
          pow   flow.rei
          prx   arx
          pry   ary
          plar  vlar
          plim  alim
          vir   [0 0 0]
    ==  ==
  =/  imp=bean
    ?|  =(%i p.w.size.rei)
        =(%i p.h.size.rei)
    ==
  =/  csiz=$@(~ [w=@ud h=@ud])
    ?.  |(repo imp ?=(%scroll -.ars))
      ~
    =/  i=@ud    0
    =/  ax=axis  %xy
    =/  rig=@ud  x.vlar
    =/  bot=@ud  y.vlar
    |-
    =/  el  (~(get by ?:(|(repo wrim) ens.b ens.a)) [[ax i] k])
    ?:  ?=(^ el)
      ?:  ?=(%border -.ars.u.el)
        $(i +(i))
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
    ?:  =(%xy ax)
      $(ax %z, i 0)
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
  =?  alim  wrim
    :-  =/  x  (add x.vlar ?:(=(0 arx) 0 (dec arx)))
        ?:  (gth x x.plim)
          x.plim
        x
    =/  y  (add y.vlar ?:(=(0 ary) 0 (dec ary)))
    ?:  (gth y y.plim)
      y.plim
    y
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
      =/  ax=axis  %xy
      =/  i=@ud  0
      |-
      =/  el  (~(get by ens.b) [[ax i] k])
      ?~  el
        ?:  =(%xy ax)
          $(ax %z, i 0)
        ens.b
      =.  ens.b  $(ax %xy, i 0, k [[ax i] k])
      =:  visa.u.el   (muto movx movy ~(tap by visa.u.el))
          x.lar.u.el  (add x.lar.u.el movx)
          y.lar.u.el  (add y.lar.u.el movy)
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
        ?:  |((gth x x.alim) (gth y y.alim))
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
      =/  lim  (sub prx ?:(?=(%row d.pow) n.vir o.vir))
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
  =/  ncc  (lent nor)
  =?  a  ?=(^ bor)
    %_  $
      m     bor
      k     [[%xy ncc] k]
      cc    ncc
      cl    0
      px    w.size.rei
      py    h.size.rei
      pl    fi
      pex   flex.ares
      pow   flow.ares
      prx   w.size.ares
      pry   h.size.ares
      plar  alar
      plim  :-  =/  x  (add x.alim ;:(add bl br l.padd.ares r.padd.ares))
                ?:((gth x x.plim) x.plim x)
            =/  y  (add y.alim ;:(add bt bb t.padd.ares b.padd.ares))
            ?:((gth y y.plim) y.plim y)
      vlar  alar
      vir   [0 0 0]
    ==
  =/  rend=visa  (viso alar ares ars plim)
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
  =?  ars  ?=(%scroll -.ars)  :: probably just add this ars part to the geno state transition section for scroll
    =/  lims
      ?~  csiz  [0 0]
      :-  ?:((gth arx w.csiz) 0 (sub w.csiz arx))
      ?:((gth ary h.csiz) 0 (sub h.csiz ary))
    [-.ars iter.ars [bl br bt bb] lims]
  %=  $
    a  
      %=  a
        ens   (~(put by ens.a) k [ares rend alar aves ars])
        visa  (~(uni by rend) visa.a)
      ==
    m     t.m
    k     [[axis.i.k +(ager.i.k)] t.k]
    cc    +(cc)
    cl    +(cl)
  ==
::  ::  ::  ::  ::  ::  ::
++  dico                    :: derive hotkey and navigation context from display state
  |=  =ens
  ^-  cura
  =/  k=rami  [[%z 0] ~]
  =/  pow=flow  [%row %clip]
  =/  oob=bean  %.n
  =/  pend=loci  [(dec x.arca) (dec y.arca)]
  =|  acc=cura
  |-
  =/  el  (~(get by ens) k)
  ?~  el
    ?:  =(%xy axis.i.k)
      acc
    $(k [[%xy 0] t.k])
  =/  boo  |(oob (gth x.lar.u.el x.pend) (gth y.lar.u.el y.pend))
  ?:  |(boo ?=(%z axis.i.k))
    %=  $
      ager.i.k  +(ager.i.k)
      acc
        %=  $
          k    [[%z 0] k]
          pow  flow.res.u.el
          oob  boo
          pend  :-  (add x.lar.u.el w.size.res.u.el)
                (add y.lar.u.el h.size.res.u.el)
    ==  ==
  =.  acc
    =/  sel=(unit @t)  (~(get by aves.u.el) %sel)
    =/  nav=bean
      ?|  &(?=(^ sel) !?=(%layer -.ars.u.el))
          ?=(%select -.ars.u.el)
          ?=(%input -.ars.u.el)
          ?=(%scroll -.ars.u.el)
      ==
    =?  ordo.acc  nav
      :_  ordo.acc
      :*  x.lar.u.el
          (add x.lar.u.el ?:(=(0 w.size.res.u.el) 0 (dec w.size.res.u.el)))
          y.lar.u.el
          (add y.lar.u.el ?:(=(0 h.size.res.u.el) 0 (dec h.size.res.u.el)))
          k
      ==
    =?  omen.acc  &(nav !(~(has by omen.acc) [%aro %l]))
      (~(uni by omen.acc) hnav)
    =?  omen.acc  |(?=(%input -.ars.u.el) ?=(%scroll -.ars.u.el))
      (~(uni by omen.acc) htog)
    acc
  %=  $
    ager.i.k  +(ager.i.k)
    acc
      %=  $
        k    [[%z 0] k]
        pow  flow.res.u.el
        oob  boo
        pend  :-  (add x.lar.u.el w.size.res.u.el)
              (add y.lar.u.el h.size.res.u.el)
  ==  ==
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
  ?:  =(+(y) y.arca)
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
  ?:(=(+(y) y.arca) ~ $(y +(y)))
::
++  make-stub
  |=  [y=@ud v=visa]
  ^-  stub
  =/  x=@ud  0
  |-
  ?:  (gte x x.arca)
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
