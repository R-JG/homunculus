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
      padd=[t=@ud r=@ud b=@ud l=@ud]
      marg=[t=@ud r=@ud b=@ud l=@ud]
      flex=[x=@ud y=@ud]
      flow=[d=?(%col %row) b=?(%wrap %clip)]
      look=fila
  ==
+$  visa  (map loci [=fila @c ~])
+$  vox   tape
+$  loci  [x=@ud y=@ud]
+$  lar   [x=(unit @ud) y=(unit @ud)]
+$  axis  ?(%xy %z)
+$  ager  @ud
+$  fila  [d=(set deco) b=tint f=tint]
+$  aves  (map ?(%sel %act) @t)
+$  ad    ?(%left %right %top %bottom)
+$  acia  ?(%light %heavy %double %blank %~)
+$  ars
  $%  [%text =vox]  [%select d=(unit (set deco)) b=(unit tint) f=(unit tint)]
      [%border =ad =acia]  [%layer ~]  [%$ ~]
  ==
::
+$  omen  (map nota lex)
+$  nota  dill-belt:dill
+$  lex   ?(%nav-l %nav-r %nav-u %nav-d %act %def)
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
      padd=[t=as r=as b=as l=as]
      marg=[t=as r=as b=as l=as]
      =flex
      =flow
      look=[d=(unit (set deco)) b=(unit tint) f=(unit tint)]
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
    =/  n=nota  +:!<(poke:dill vase)
    ::
    ?:  =(%del -.n)                            ::  press the del key to exit the tui
      :_  hoc
      :~  [%pass /give-terminal %arvo %d %open %hood ~]
          [%pass /give-terminal %arvo %d %flee ~]
      ==
    ::
    ?:  =(%rez -.n)                            ::  resize arca to new terminal dimensions
      =/  gen=opus  (geno vela.ara)
      =/  dic=cura  (dico ens.gen)
      =/  =lux  (put-blit visa.gen)
      :_  hoc(arca [(@ -.+.n) (@ +.+.n)], ara ara(ens ens.gen, visa visa.gen, ordo ordo.dic))
      :~  [%give %fact ~[/dill/$] %dill-blit !>(lux)]
      ==
    ::
    =/  l=(unit lex)  (~(get by omen.ara) n)
    ?~  l
      ~&  >>>  'NO EVENT'
      [~ hoc]
    :: ~&  >>>  u.l
    :: ~&  >>>  [%current-selection rex.ara]
    =^  cards  ego
      (novo u.l)
    :: ~&  >>>  [%new-selection rex.ara]
    [cards hoc]
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
++  novo
  |=  =lex
  ^-  (quip card ^ego)
  ::
  ?:  |(=(%nav-l lex) =(%nav-r lex) =(%nav-u lex) =(%nav-d lex))
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
  [~ ego]
::  ::  ::  ::  ::  ::  ::  ::
++  dolo
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
        flow=[%row %wrap]
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
  ==
::
++  suo
  |=  [n=mane a=mart]
  ^-  [rei aves ars acia]
  =+  ^-  [=rei =aves =ars =acia]
      ?+  n  [(dolo %$) ~ [%$ ~] ~]
        %$         [(dolo %text) ~ [%text ?~(a ~ v.i.a)] ~]
        %layer     [(dolo %layer) ~ [%layer ~] ~]
        %select    [(dolo %$) ~ [%select ~ ~ ~] ~]
        %border-left    [(dolo %border-left) ~ [%border %left %~] ~]
        %border-right   [(dolo %border-right) ~ [%border %right %~] ~]
        %border-top     [(dolo %border-top) ~ [%border %top %~] ~]
        %border-bottom  [(dolo %border-bottom) ~ [%border %bottom %~] ~]
      ==
  |-
  ?~  a  [rei aves ars acia]
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
    ?:  ?=(%border -.ars)
      ?+  (@tas (crip v.i.a))  $(a t.a)
        %light  $(acia.ars %light, a t.a)
        %heavy    $(acia.ars %heavy, a t.a)
        %double  $(acia.ars %double, a t.a)
        %blank   $(acia.ars %blank, a t.a)
      ==
    ?+  (@tas (crip v.i.a))  $(a t.a)
      %light  $(acia %light, a t.a)
      %heavy    $(acia %heavy, a t.a)
      %double  $(acia %double, a t.a)
      %blank   $(acia %blank, a t.a)
    ==
      %sel
    $(aves (~(put by aves) %sel (crip v.i.a)), a t.a)
      %sel-cb
    ?+  -.ars  $(a t.a)
        %select
      ?+  (@tas (crip v.i.a))  $(a t.a)
        %red      $(b.ars [~ %r], a t.a)
        %green    $(b.ars [~ %g], a t.a)
        %blue     $(b.ars [~ %b], a t.a)
        %cyan     $(b.ars [~ %c], a t.a)
        %magenta  $(b.ars [~ %m], a t.a)
        %yellow   $(b.ars [~ %y], a t.a)
        %black    $(b.ars [~ %k], a t.a)
        %white    $(b.ars [~ %w], a t.a)
      ==
    ==
      %sel-cf
    ?+  -.ars  $(a t.a)
        %select
      ?+  (@tas (crip v.i.a))  $(a t.a)
        %red      $(f.ars [~ %r], a t.a)
        %green    $(f.ars [~ %g], a t.a)
        %blue     $(f.ars [~ %b], a t.a)
        %cyan     $(f.ars [~ %c], a t.a)
        %magenta  $(f.ars [~ %m], a t.a)
        %yellow   $(f.ars [~ %y], a t.a)
        %black    $(f.ars [~ %k], a t.a)
        %white    $(f.ars [~ %w], a t.a)
      ==
    ==
      %sel-d
    ?+  -.ars  $(a t.a)
        %select
      ?+  (@tas (crip v.i.a))  $(a t.a)
        %bold       $(d.ars [~ (silt ~[%br])], a t.a)
        %blink      $(d.ars [~ (silt ~[%bl])], a t.a)
        %underline  $(d.ars [~ (silt ~[%un])], a t.a)
      ==
    ==
      %act
    $(aves (~(put by aves) %act (crip v.i.a)), a t.a)
  ==
::
++  pars
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
++  obeo                    :: get border sizes from the border element list
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
++  duco                    :: render a navigation update
  |=  [e=ens k=rami r=rex]  :: return just the ens and visa of the updated elements
  ^-  opus
  ?~  r  [~ ~]
  =/  tar  (~(get by e) k)
  ?~  tar  [~ ~]
  ?.  ?=(%select -.ars.u.tar)
    [~ ~]
  =/  sel=bean  =(k k.r)
  =/  v=visa
    %+  fuco  visa.u.tar
    ?.  sel
      look.res.u.tar
    :+  ?~(d.ars.u.tar d.look.res.u.tar u.d.ars.u.tar)
      ?~(b.ars.u.tar b.look.res.u.tar u.b.ars.u.tar)
    ?~(f.ars.u.tar f.look.res.u.tar u.f.ars.u.tar)
  =.  visa.u.tar  v
  =:  e  (~(put by e) k u.tar)
      k  [[%xy 0] k]
    ==
  |-
  =/  el  (~(get by e) k)
  ?~  el  [e v]
  ?.  |(?=(%border -.ars.u.el) ?=(%text -.ars.u.el))
    $(ager.i.k +(ager.i.k))
  =.  visa.u.el
    %+  fuco  visa.u.el
    ?.  sel
      look.res.u.tar
    :+  ?~(d.ars.u.tar d.look.res.u.el u.d.ars.u.tar)
      ?~(b.ars.u.tar b.look.res.u.el u.b.ars.u.tar)
    ?~(f.ars.u.tar f.look.res.u.el u.f.ars.u.tar)
  %=  $
    e  (~(put by e) k u.el)
    v  (~(uni by v) visa.u.el)
    ager.i.k  +(ager.i.k)
  ==
::
++  fuco
  |=  [vi=visa fi=fila]
  ^-  visa
  (~(urn by vi) |=([* va=[fila @c ~]] [fi +.va]))
::
++  rbox
  |=  [=lar lim=loci =res]
  ^-  visa
  =+  [w=0 h=0 *a=visa]
  ?~  x.lar  a
  ?~  y.lar  a
  |-
  =/  x  (add u.x.lar w)
  =/  y  (add u.y.lar h)
  =/  nrow=bean  (gte +(w) w.size.res)
  ?:  |((gte h h.size.res) (gth y y.lim))
    a
  ?:  |((gth +(x) x.arca) (gth x x.lim))
    $(w ?:(nrow 0 +(w)), h ?:(nrow +(h) h))
  %=  $
    w  ?:(nrow 0 +(w))
    h  ?:(nrow +(h) h)
    a  (~(put by a) [x y] [[~ +.look.res] (@c ' ') ~])
  ==
::
++  rtxt
  |=  [=lar lim=loci =fila cols=@ud =vox]
  ^-  visa
  =+  [w=0 h=0 *a=visa]
  ?~  x.lar  a
  ?~  y.lar  a
  |-
  ?~  vox    a
  =/  x  (add u.x.lar w)
  =/  y  (add u.y.lar h)
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
++  rbor
  |=  [=lar lim=loci =res =ad =acia]
  ^-  visa
  =+  [w=0 h=0 *a=visa]
  ?~  x.lar  a
  ?~  y.lar  a
  ?:  ?=(%~ acia)  a
  |-
  =/  x  (add u.x.lar w)
  =/  y  (add u.y.lar h)
  =/  nrow=bean  (gte +(w) w.size.res)
  ?:  |((gte h h.size.res) (gth y y.lim))
    a
  ?:  |((gth +(x) x.arca) (gth x x.lim))
    $(w ?:(nrow 0 +(w)), h ?:(nrow +(h) h))
  =/  c=@c
    %-  @c
    ?:  ?=(%blank acia)
      ' '
    ?-  ad
        %left
      ?:  =(0 w)
        ?:  =(0 h)
          ?-(acia %light '┌', %heavy '┏', %double '╔')
        ?:  =(h.size.res +(h))
          ?-(acia %light '└', %heavy '┗', %double '╚')
        ?-(acia %light '│', %heavy '┃', %double '║')
      ' '
        %right
      ?:  nrow
        ?:  =(0 h)
          ?-(acia %light '┐', %heavy '┓', %double '╗')
        ?:  =(h.size.res +(h))
          ?-(acia %light '┘', %heavy '┛', %double '╝')
        ?-(acia %light '│', %heavy '┃', %double '║')
      ' '
        %top
      ?:  =(0 h)
        ?:  =(0 w)
          ?-(acia %light '┌', %heavy '┏', %double '╔')
        ?:  nrow
          ?-(acia %light '┐', %heavy '┓', %double '╗')
        ?-(acia %light '─', %heavy '━', %double '═')
      ' '
        %bottom
      ?:  =(h.size.res +(h))
        ?:  =(0 w)
          ?-(acia %light '└', %heavy '┗', %double '╚')
        ?:  nrow
          ?-(acia %light '┘', %heavy '┛', %double '╝')
        ?-(acia %light '─', %heavy '━', %double '═')
      ' '
    ==
  %=  $
    w  ?:(nrow 0 +(w))
    h  ?:(nrow +(h) h)
    a  (~(put by a) [x y] [[~ +.look.res] c ~])
  ==
::
++  geno
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
  =/  plar=lar      [`0 `0]
  =/  plim=loci     [(dec x.arca) (dec y.arca)]
  =/  vlar=lar      [`0 `0]
  =/  vir=[n=@ud o=@ud i=@ud]  [0 0 0]
  |-
  ?~  m  a
  =+  ^-  [=rei =aves =ars =acia]
    (suo g.i.m)
  =+  ^-  [bor=marl lay=marl nor=marl]
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
  =?  bor  !=(%~ acia)
    =/  v  (trip acia)
    %+  weld  bor
    ^-  marl
    ;=  ;border-left(b v);
        ;border-right(b v);
        ;border-top(b v);
        ;border-bottom(b v);
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
    ?&  !?=(%border -.ars)
        ?|  &(!=(0 x.flex.rei) =(%c p.px)) 
            &(!=(0 y.flex.rei) =(%c p.py))
    ==  ==
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
        ?~  x.vlar  ~
        =/  x  (add u.x.vlar ?:(=(0 q.px) 0 (dec q.px)))
        =.  x
          =/  w  ?:(=(0 q.w.size.rei) 0 (dec q.w.size.rei))
          ?:((gth w x) 0 (sub x w))
        ?:  (gth x x.plim)
          ~
        `x
          %top
        vlar
          %bottom
        :-  x.vlar
        ?~  y.vlar  ~
        =/  y  (add u.y.vlar ?:(=(0 q.py) 0 (dec q.py)))
        =.  y
          =/  h  ?:(=(0 q.h.size.rei) 0 (dec q.h.size.rei))
          ?:((gth h y) 0 (sub y h))
        ?:  (gth y y.plim)
          ~
        `y
      ==
    =?  vlar  |(wrap wrim)
      ?-  d.pow
          %row
        :-  x.plar
        ?~  y.plar
          ~
        `(add u.y.plar o.vir)
          %col
        :_  y.plar
        ?~  x.plar
          ~
        `(add u.x.plar o.vir)
      ==
    :-  ?~  x.vlar
          ~
        =/  x  (add u.x.vlar q.l.marg.rei)
        ?:  (gth x x.plim)
          ~
        `x
    ?~  y.vlar
      ~
    =/  y  (add u.y.vlar q.t.marg.rei)
    ?:  (gth y y.plim)
      ~
    `y
  =/  alar=lar  vlar
  =.  vlar
  :-  ?~  x.vlar
        ~
      =/  x  ;:(add bl q.l.padd.rei u.x.vlar)
      ?:  (gth x x.plim)
        ~
      `x
  ?~  y.vlar
    ~
  =/  y  ;:(add bt q.t.padd.rei u.y.vlar)
  ?:  (gth y y.plim)
    ~
  `y
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
    :-  ?~  x.alar
          x.plim
        =/  x  (add u.x.alar ?:(=(0 arx) 0 (dec arx)))
        ?:  (gth x x.plim)
          x.plim
        x
    ?~  y.alar
      y.plim
    =/  y  (add u.y.alar ?:(=(0 ary) 0 (dec ary)))
    ?:  (gth y y.plim)
      y.plim
    y
  =/  fi=fila
    :+  ?~(d.look.rei d.pl u.d.look.rei)
      ?~(b.look.rei b.pl u.b.look.rei)
    ?~(f.look.rei f.pl u.f.look.rei)
  =/  ncc  (lent bor)
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
            k     [[%xy ncc] k]
            cc    ncc
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
          k     [[%xy ncc] k]
          cc    ncc
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
    ?:  |(&(=(~ x.vlar) =(~ y.vlar)) !|(repo imp) ?=(%text -.ars))
      ~
    =/  i=@ud    0
    =/  ax=axis  %xy
    =/  rig=@ud  ?~(x.vlar x.alim u.x.vlar)
    =/  bot=@ud  ?~(y.vlar y.alim u.y.vlar)
    |-
    =/  el  (~(get by ?:(|(repo wrim) ens.b ens.a)) [[ax i] k])
    ?:  &(?=(^ el) !?=(%border -.ars.u.el))
      =/  el-r=@ud
        ?~  x.lar.u.el
          x.alim
        %+  add  u.x.lar.u.el
        %+  add  r.marg.res.u.el
        ?:(=(0 w.size.res.u.el) 0 (dec w.size.res.u.el))
      =/  el-b=@ud
        ?~  y.lar.u.el
          y.alim
        %+  add  u.y.lar.u.el
        %+  add  b.marg.res.u.el
        ?:(=(0 h.size.res.u.el) 0 (dec h.size.res.u.el))
      =?  rig  (gth el-r rig)  el-r
      =?  bot  (gth el-b bot)  el-b
      $(i +(i))
    ?:  =(%xy ax)
      $(ax %z, i 0)
    =?  rig  (gth rig x.alim)  x.alim
    =?  bot  (gth bot y.alim)  y.alim
    :-  ?~  x.vlar
          0
        ?:  (gth u.x.vlar +(rig))
          0
        (sub +(rig) u.x.vlar)
    ?~  y.vlar
      0
    ?:  (gth u.y.vlar +(bot))
      0
    (sub +(bot) u.y.vlar)
  =?  size.rei  &(imp ?=(^ csiz))
    :-  ?:  =(%i p.w.size.rei)  
          [%c ;:(add bl br q.l.padd.rei q.r.padd.rei ?:((gth w.csiz arx) arx w.csiz))]  
        w.size.rei
    ?:  =(%i p.h.size.rei)  
      [%c ;:(add bt bb q.t.padd.rei q.b.padd.rei ?:((gth h.csiz ary) ary h.csiz))]
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
        ?~  y.vlar
          ~
        [~ (add u.y.vlar (sub i.tvir o.tvir))]
          %col
        :_  y.vlar
        ?~  x.vlar
          ~
        [~ (add u.x.vlar (sub i.tvir o.tvir))]
      ==
    ?-  d.pow
        %row
      :_  y.vlar
      ?~  x.vlar
        ~
      [~ (add u.x.vlar n.tvir)]
        %col
      :-  x.vlar
      ?~  y.vlar
        ~
      [~ (add u.y.vlar n.tvir)]
    ==
  =?  alar  wrim
    ?:  wris
      ?-  d.pow
          %row
        :-  x.alar
        ?~  y.alar
          ~
        [~ (add u.y.alar (sub i.tvir o.tvir))]
          %col
        :_  y.alar
        ?~  x.alar
          ~
        [~ (add u.x.alar (sub i.tvir o.tvir))]
      ==
    ?-  d.pow
        %row
      :_  y.alar
      ?~  x.alar
        ~
      [~ (add u.x.alar n.tvir)]
        %col
      :-  x.alar
      ?~  y.alar
        ~
      [~ (add u.y.alar n.tvir)]
    ==
  =?  arx  wrim
    =/  bp  ;:(add bl br q.l.padd.rei q.r.padd.rei)
    ?:  (gth bp q.w.size.rei)
      0
    (sub q.w.size.rei bp)
  =?  ary  wrim
    =/  bp  ;:(add bt bb q.t.padd.rei q.b.padd.rei)
    ?:  (gth bp q.h.size.rei)
      0
    (sub q.h.size.rei bp)
  =?  alim  wrim
    :-  ?~  x.vlar
          x.plim
        =/  x  (add u.x.vlar ?:(=(0 arx) 0 (dec arx)))
        ?:  (gth x x.plim)
          x.plim
        x
    ?~  y.vlar
      y.plim
    =/  y  (add u.y.vlar ?:(=(0 ary) 0 (dec ary)))
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
      =.  visa.u.el  (muto movx movy ~(tap by visa.u.el))
      =?  x.lar.u.el  ?=(^ x.lar.u.el)
        =/  x  (add u.x.lar.u.el movx)
        ?:  (gth x x.alim)
          ~
        `x
      =?  y.lar.u.el  ?=(^ y.lar.u.el)
        =/  y  (add u.y.lar.u.el movy)
        ?:  (gth y y.alim)
          ~
        `y
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
          [q.t.padd.rei q.r.padd.rei q.b.padd.rei q.l.padd.rei]
          [q.t.marg.rei q.r.marg.rei q.b.marg.rei q.l.marg.rei]
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
  =?  a  ?=(^ bor)
    %_  $
      m     bor
      k     [[%xy 0] k]
      cc    0
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
  =/  rend=visa
    ?+  -.ars    (rbox alar plim ares)
        %text    (rtxt alar plim pl w.size.ares vox.ars)
        %layer   ~
        %border  (rbor alar plim ares +.ars)
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
      =.  vir
        [(add n.vir el-x) o.vir ?:((gth el-y (sub i.vir o.vir)) (add o.vir el-y) i.vir)]
      [?:((gth n.vir prx) prx n.vir) ?:((gth o.vir pry) pry o.vir) ?:((gth i.vir pry) pry i.vir)]
        %col
      =.  vir
        [(add n.vir el-y) o.vir ?:((gth el-x (sub i.vir o.vir)) (add o.vir el-x) i.vir)]
      [?:((gth n.vir pry) pry n.vir) ?:((gth o.vir prx) prx o.vir) ?:((gth i.vir prx) prx i.vir)]
    ==
  =?  vlar  !?=(%layer -.ars)
    ?:  ?=(%border -.ars)
      plar
    =/  vx  ?-(d.pow %row n.vir, %col o.vir)
    =/  vy  ?-(d.pow %row o.vir, %col n.vir)
    :-  ?~  x.plar
          ~
        =/  x  (add u.x.plar vx)
        ?:  (gth x x.plim)
          ~
        `x
    ?~  y.plar
      ~
    =/  y  (add u.y.plar vy)
    ?:  (gth y y.plim)
      ~
    `y
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
++  dico
  |=  =ens
  ^-  cura
  =/  i=@ud   0
  =/  a=axis  %z
  =/  k=rami  [[a i] ~]
  =/  pow=flow  [%row %clip]
  =|  acc=cura
  |-
  =/  el  (~(get by ens) k)
  ?~  el
    ?.  =(%z a)
      acc
    $(a %xy, i 0, k [[%xy 0] t.k])
  ?:  |(=(%z a) =(~ x.lar.u.el) =(~ y.lar.u.el))
    %=  $
      i  +(i)
      k  k(ager.i +(ager.i.k))
      acc
        %=  $
          i    0
          a    %z
          k    [[%z 0] k]
          pow  flow.res.u.el
    ==  ==
  =.  acc
    =/  sel=(unit @t)  (~(get by aves.u.el) %sel)
    =/  nav=bean
      ?|  &(?=(^ sel) !?=(%layer -.ars.u.el))
          ?=(%select -.ars.u.el)
      ==
    =?  ordo.acc  nav
      ?~  x.lar.u.el  ordo.acc
      ?~  y.lar.u.el  ordo.acc
      :_  ordo.acc
      :*  u.x.lar.u.el
          (add u.x.lar.u.el ?:(=(0 w.size.res.u.el) 0 (dec w.size.res.u.el)))
          u.y.lar.u.el
          (add u.y.lar.u.el ?:(=(0 h.size.res.u.el) 0 (dec h.size.res.u.el)))
          k
      ==
    =?  omen.acc  &(nav !(~(has by omen.acc) [%aro %l]))
      %-  %~  gas
            by
          omen.acc
      [[[%aro %l] %nav-l] [[%aro %r] %nav-r] [[%aro %u] %nav-u] [[%aro %d] %nav-d] ~]
    acc
  %=  $
    i  +(i)
    k  k(ager.i +(ager.i.k))
    acc
      %=  $
        i    0
        a    %z
        k    [[%z 0] k]
        pow  flow.res.u.el
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
++  viso                   :: display update diff
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
