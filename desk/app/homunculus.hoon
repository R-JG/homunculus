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
+$  ars
  $%  [%text =vox]  [%select =fila]
      [%border-top ~]  [%border-left ~]  [%border-bottom ~]  [%border-right ~]
      [%layer ~]  [%$ ~]
  ==
::
+$  omen  (map nota lex)
+$  nota  dill-belt:dill
+$  lex   ?(%nav-l %nav-r %nav-u %nav-d %act %def)
+$  mus   (map loci rami)
+$  dux   [l=@ud r=@ud t=@ud b=@ud k=rami]
+$  rex   $@(~ dux)
+$  ordo  (list dux)
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
      look=fila
  ==
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
+$  aeon
  $:  =arca  =ara
  ==
::
+$  card  card:agent:gall
--
::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  
=|  aeon
=*  aeon  -
^-  agent:gall
=<
|_  =bowl:gall
+*  hoc  .
++  on-init
  ^-  (quip card _hoc)
  [~ hoc(aeon *^aeon)]
++  on-save
  ^-  vase
  !>(arca)
++  on-load
  |=  old=vase
  ^-  (quip card _hoc)
  [~ hoc(aeon =+(*^aeon -(arca !<(^^arca old))))]
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
      =/  vis=dill-blit:dill  (put-blit visa.gen)
      :_  hoc(arca [(@ -.+.n) (@ +.+.n)], ara ara(ens ens.gen, visa visa.gen, ordo ordo.dic))
      :~  [%give %fact ~[/dill/$] %dill-blit !>(vis)]
      ==
    ::
    ~&  >>  n
    =/  l=(unit lex)  (~(get by omen.ara) n)
    ?~  l
      ~&  >>>  'NO EVENT'
      [~ hoc]
    ~&  >>>  u.l
    ~&  >>>  [%current-selection rex.ara]
    =^  cards  aeon
      (novo u.l)
    ~&  >>>  [%new-selection rex.ara]
    [cards hoc]
  ==
::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  
++  on-watch
  |=  =path
  ?+  path  [~ hoc]
      [%dill *]
    =/  vis=dill-blit:dill  (put-blit visa.ara)
    :_  hoc
    :~  [%give %fact ~[/dill/$] %dill-blit !>(vis)]
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
  ^-  (quip card ^aeon)
  ::
  ?:  |(=(%nav-l lex) =(%nav-r lex) =(%nav-u lex) =(%nav-d lex))
    |^
    ?~  ordo.ara  [~ aeon]
    ?~  rex.ara
      =/  fake=dux
        [0 (dec x.arca) (dec y.arca) 0 `rami`[(rear k.i.ordo.ara) ~]]
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
      [~ aeon(rex.ara ?~(i ~ i.i))]
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
      [~ aeon(rex.ara i.i)]
    =/  ii=ordo
      %+  sort  `ordo`(skim `ordo`ordo.ara (keep rex.ara))
      |=  [a=dux b=dux]
      (lth (pyth a rex.ara) (pyth b rex.ara))
    ?~  ii
      [~ aeon]
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
      [~ aeon(rex.ara i.ii)]
    ?:  ((keep rex.ara) i.iii)
      [~ aeon(rex.ara i.iii)]
    [~ aeon]
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
              (gte t.dux t.rex)  (lte b.dux t.rex)
          ==
          ?&  (lte l.dux r.rex)  (gte r.dux r.rex)
              (gte t.dux t.rex)  (lte b.dux t.rex)
          ==
          ?&  (lte l.dux l.rex)  (gte r.dux l.rex)
              (gte t.dux b.rex)  (lte b.dux b.rex)
          ==
          ?&  (lte l.dux r.rex)  (gte r.dux r.rex)
              (gte t.dux b.rex)  (lte b.dux b.rex)
          ==
          ?&  (lte l.rex l.dux)  (gte r.rex l.dux)
              (gte t.rex t.dux)  (lte b.rex t.dux)
          ==
          ?&  (lte l.rex r.dux)  (gte r.rex r.dux)
              (gte t.rex t.dux)  (lte b.rex t.dux)
          ==
          ?&  (lte l.rex l.dux)  (gte r.rex l.dux)
              (gte t.rex b.dux)  (lte b.rex b.dux)
          ==
          ?&  (lte l.rex r.dux)  (gte r.rex r.dux)
              (gte t.rex b.dux)  (lte b.rex b.dux)
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
          (gth t.dux t.rex)
        ?.  =(~ (abov k.rex k.dux))
          (gth t.dux t.rex)
        (gth b.dux t.rex)
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
          (lth t.dux t.rex)
        ?.  =(~ (abov k.rex k.dux))
          (lth t.dux t.rex)
        (lth t.dux b.rex)
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
          (pow (mul (sub t.dux t.rex) 2) 2)
        %-  sqt  %+  add
          (pow (mul ?:((gte l.dux l.rex) (sub l.dux l.rex) (sub l.rex l.dux)) 2) 2)
        (pow (mul (sub b.dux t.rex) 2) 2)
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
          (pow (mul (sub t.rex t.dux) 2) 2)
        %-  sqt  %+  add
          (pow (mul ?:((gte l.rex l.dux) (sub l.rex l.dux) (sub l.dux l.rex)) 2) 2)
        (pow (mul (sub b.rex t.dux) 2) 2)
      ==
    --
  ::
  [~ aeon]
::  ::  ::  ::  ::  ::  ::  ::
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
  =/  plar=lar      [`0 `(dec y.arca)]
  =/  plim=loci     [(dec x.arca) 0]
  =/  vir=[n=@ud o=@ud i=@ud]  [0 0 0]
  =/  vlar=lar      [`0 `(dec y.arca)]
  |-
  ?~  m  a
  =+  ^-  [=rei =aves =ars]
      ?+  n.g.i.m  [(dolo %$) ~ [%$ ~]]
        %$         [(dolo %text) ~ [%text `tape`+.-.+.-.-.m]]
        %layer     [(dolo %layer) ~ [%layer ~]]
      ==
  |-
  ?:  ?=(^ a.g.i.m)
    ?+  n.i.a.g.i.m  $(a.g.i.m t.a.g.i.m)
        %w
      $(w.size.rei (pars v.i.a.g.i.m), a.g.i.m t.a.g.i.m)
        %h
      $(h.size.rei (pars v.i.a.g.i.m), a.g.i.m t.a.g.i.m)
        %p
      =/  v  (pars v.i.a.g.i.m)
      $(padd.rei [v v v v], a.g.i.m t.a.g.i.m)
        %px
      =/  v  (pars v.i.a.g.i.m)
      $(l.padd.rei v, r.padd.rei v, a.g.i.m t.a.g.i.m)
        %py
      =/  v  (pars v.i.a.g.i.m)
      $(t.padd.rei v, b.padd.rei v, a.g.i.m t.a.g.i.m)
        %pl
      $(l.padd.rei (pars v.i.a.g.i.m), a.g.i.m t.a.g.i.m)
        %pr
      $(r.padd.rei (pars v.i.a.g.i.m), a.g.i.m t.a.g.i.m)
        %pt
      $(t.padd.rei (pars v.i.a.g.i.m), a.g.i.m t.a.g.i.m)
        %pb
      $(b.padd.rei (pars v.i.a.g.i.m), a.g.i.m t.a.g.i.m)
        %m
      =/  v  (pars v.i.a.g.i.m)
      $(marg.rei [v v v v], a.g.i.m t.a.g.i.m)
        %mx
      =/  v  (pars v.i.a.g.i.m)
      $(l.marg.rei v, r.marg.rei v, a.g.i.m t.a.g.i.m)
        %my
      =/  v  (pars v.i.a.g.i.m)
      $(t.marg.rei v, b.marg.rei v, a.g.i.m t.a.g.i.m)
        %ml
      $(l.marg.rei (pars v.i.a.g.i.m), a.g.i.m t.a.g.i.m)
        %mr
      $(r.marg.rei (pars v.i.a.g.i.m), a.g.i.m t.a.g.i.m)
        %mt
      $(t.marg.rei (pars v.i.a.g.i.m), a.g.i.m t.a.g.i.m)
        %mb
      $(b.marg.rei (pars v.i.a.g.i.m), a.g.i.m t.a.g.i.m)
        %fx
      =/  num=(unit @ud)  (slaw %ud (crip v.i.a.g.i.m))
      ?:  ?=(^ num)
        $(x.flex.rei ?:((lte u.num 100) u.num 100), a.g.i.m t.a.g.i.m)
      ?+  (@tas (crip v.i.a.g.i.m))  $(a.g.i.m t.a.g.i.m)
        %start   $(x.flex.rei 0, a.g.i.m t.a.g.i.m)
        %center  $(x.flex.rei 50, a.g.i.m t.a.g.i.m)
        %end     $(x.flex.rei 100, a.g.i.m t.a.g.i.m)
      ==
        %fy
      =/  num=(unit @ud)  (slaw %ud (crip v.i.a.g.i.m))
      ?:  ?=(^ num)
        $(y.flex.rei ?:((lte u.num 100) u.num 100), a.g.i.m t.a.g.i.m)
      ?+  (@tas (crip v.i.a.g.i.m))  $(a.g.i.m t.a.g.i.m)
        %start   $(y.flex.rei 0, a.g.i.m t.a.g.i.m)
        %center  $(y.flex.rei 50, a.g.i.m t.a.g.i.m)
        %end     $(y.flex.rei 100, a.g.i.m t.a.g.i.m)
      ==
        %fl
      ?+  (@tas (crip v.i.a.g.i.m))  $(a.g.i.m t.a.g.i.m)
        %row          $(flow.rei [%row %clip], a.g.i.m t.a.g.i.m)
        %row-clip     $(flow.rei [%row %clip], a.g.i.m t.a.g.i.m)
        %row-wrap     $(flow.rei [%row %wrap], a.g.i.m t.a.g.i.m)
        %column       $(flow.rei [%col %clip], a.g.i.m t.a.g.i.m)
        %column-clip  $(flow.rei [%col %clip], a.g.i.m t.a.g.i.m)
        %column-wrap  $(flow.rei [%col %wrap], a.g.i.m t.a.g.i.m)
      ==
        %cb
      ?+  (@tas (crip v.i.a.g.i.m))  $(b.look.rei %~, a.g.i.m t.a.g.i.m)
        %red      $(b.look.rei %r, a.g.i.m t.a.g.i.m)
        %green    $(b.look.rei %g, a.g.i.m t.a.g.i.m)
        %blue     $(b.look.rei %b, a.g.i.m t.a.g.i.m)
        %cyan     $(b.look.rei %c, a.g.i.m t.a.g.i.m)
        %magenta  $(b.look.rei %m, a.g.i.m t.a.g.i.m)
        %yellow   $(b.look.rei %y, a.g.i.m t.a.g.i.m)
        %black    $(b.look.rei %k, a.g.i.m t.a.g.i.m)
        %white    $(b.look.rei %w, a.g.i.m t.a.g.i.m)
      ==
        %cf
      ?+  (@tas (crip v.i.a.g.i.m))  $(f.look.rei %~, a.g.i.m t.a.g.i.m)
        %red      $(f.look.rei %r, a.g.i.m t.a.g.i.m)
        %green    $(f.look.rei %g, a.g.i.m t.a.g.i.m)
        %blue     $(f.look.rei %b, a.g.i.m t.a.g.i.m)
        %cyan     $(f.look.rei %c, a.g.i.m t.a.g.i.m)
        %magenta  $(f.look.rei %m, a.g.i.m t.a.g.i.m)
        %yellow   $(f.look.rei %y, a.g.i.m t.a.g.i.m)
        %black    $(f.look.rei %k, a.g.i.m t.a.g.i.m)
        %white    $(f.look.rei %w, a.g.i.m t.a.g.i.m)
      ==
        %d
      ?+  (@tas (crip v.i.a.g.i.m))  $(d.look.rei ~, a.g.i.m t.a.g.i.m)
        %bold       $(d.look.rei (silt ~[%br]), a.g.i.m t.a.g.i.m)
        %blink      $(d.look.rei (silt ~[%bl]), a.g.i.m t.a.g.i.m)
        %underline  $(d.look.rei (silt ~[%un]), a.g.i.m t.a.g.i.m)
      ==
        %sel
      $(aves (~(put by aves) %sel (crip v.i.a.g.i.m)), a.g.i.m t.a.g.i.m)
        %act
      $(aves (~(put by aves) %act (crip v.i.a.g.i.m)), a.g.i.m t.a.g.i.m)
    ==
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
  ::
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
  ::
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
  ::
  =?  q.w.size.rei  &(wcen !=(%i p.w.size.rei))
    =/  m  (add q.l.marg.rei q.r.marg.rei)
    ?:  (gth m q.w.size.rei)  0  (sub q.w.size.rei m)
  =?  q.h.size.rei  &(hcen !=(%i p.h.size.rei))
    =/  m  (add q.t.marg.rei q.b.marg.rei)
    ?:  (gth m q.h.size.rei)  0  (sub q.h.size.rei m)
  =|  [bor=marl lay=marl nor=marl]
  |-
  ?:  ?=(^ c.i.m)
    ?+  n.g.i.c.i.m     $(nor [i.c.i.m nor], c.i.m t.c.i.m)
      %border-left    $(bor [i.c.i.m bor], c.i.m t.c.i.m)
      %border-right   $(bor [i.c.i.m bor], c.i.m t.c.i.m)
      %border-top     $(bor [i.c.i.m bor], c.i.m t.c.i.m)
      %border-bottom  $(bor [i.c.i.m bor], c.i.m t.c.i.m)
      %layer          $(lay [i.c.i.m lay], c.i.m t.c.i.m)
    ==
  =:  lay  (flop lay)
      nor  (flop nor)
    ==
  =/  repo=bean
    ?|  &(!=(0 x.flex.rei) =(%c p.px)) 
        &(!=(0 y.flex.rei) =(%c p.py))
    ==
  =/  wrap=bean
    ?|  ?&  =([%row %wrap] pow)  =(%c p.w.size.rei)
            (gth (add q.w.size.rei (add q.l.marg.rei q.r.marg.rei)) (sub prx n.vir))
        ==
        ?&  =([%col %wrap] pow)  =(%c p.h.size.rei)
            (gth (add q.h.size.rei (add q.t.marg.rei q.b.marg.rei)) (sub pry n.vir))
        ==
    ==
  =/  wrim=bean
    ?|  &(=([%row %wrap] pow) =(%i p.w.size.rei))
        &(=([%col %wrap] pow) =(%i p.h.size.rei))
    ==
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
    =?  vlar  |(wrap wrim)
      ?-  d.pow
          %row
        :-  x.plar
        ?~  y.plar
          ~
        `(sub u.y.plar o.vir)
          %col
        :_  y.plar
        ?~  x.plar
          ~
        `(add u.x.plar o.vir)
      ==
    :-  ?~  x.vlar
          ~
        =/  x  (add u.x.vlar q.l.marg.rei)
        ?:  |((gth +(x) x.arca) (gth x x.plim))
          ~
        `x
    ?~  y.vlar
      ~
    ?:  (gth q.t.marg.rei u.y.vlar)
      ~
    =/  y  (sub u.y.vlar q.t.marg.rei)
    ?:  (lth y y.plim)
      ~
    `y
  =/  alar=lar  vlar
  =/  arx=@ud
    ?:  =(%c p.w.size.rei)
      q.w.size.rei
    =/  w  (add q.l.marg.rei ?:(=(%row d.pow) n.vir o.vir))
    ?:  (gth w prx)  0  (sub prx w)
  =/  ary=@ud
    ?:  =(%c p.h.size.rei)  
      q.h.size.rei
    =/  h  (add q.t.marg.rei ?:(=(%row d.pow) o.vir n.vir))
    ?:  (gth h pry)  0  (sub pry h)
  =/  alim=loci
    :-  ?~  x.vlar
          x.plim
        =/  x  (add u.x.vlar ?:(=(0 arx) 0 (dec arx)))
        ?:  |((gth +(x) x.arca) (gth x x.plim))
          x.plim
        x
    ?~  y.vlar
      y.plim
    ?:  (gth ary u.y.vlar)
      y.plim
    =/  y  +((sub u.y.vlar ary))  :: add condition: if ary=0 then y lim is y lar...
    ?:  (lth y y.plim)
      y.plim
    y
  =/  b  *opus
  =>  ?.  ?=(^ bor)
      .
    ?.  |(repo wrim)
      %_  .
        a
          %_  ^^$
            m     bor
            k     [[%xy 0] k]
            cc    0
            cl    0
            px    w.size.rei
            py    h.size.rei
            pl    look.rei
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
        %_  ^^$
          a     *opus
          m     bor
          k     [[%xy 0] k]
          cc    0
          cl    0
          px    w.size.rei
          py    h.size.rei
          pl    look.rei
          pex   flex.rei
          pow   flow.rei
          prx   arx
          pry   ary
          plar  vlar
          plim  alim
          vir   [0 0 0]
    ==  ==
  =+  ^-  [bl=@ud br=@ud bt=@ud bb=@ud]
    ?~  bor  [0 0 0 0]
    =+  [i=0 bl=0 br=0 bt=0 bb=0]
    |-
    =/  el  (~(get by ?:(|(repo wrim) ens.b ens.a)) [[%xy i] k])
    ?~  el  [bl br bt bb]
    ?+  -.ars.u.el      $(i +(i))
      %border-left    $(i +(i), bl w.size.res.u.el)
      %border-right   $(i +(i), br w.size.res.u.el)
      %border-top     $(i +(i), bt h.size.res.u.el)
      %border-bottom  $(i +(i), bb h.size.res.u.el)
    ==
  =:  arx
        =/  w  ;:(add bl br q.l.padd.rei q.r.padd.rei)
        ?:  (gth w arx)  0  (sub arx w)
      ary
        =/  h  ;:(add bt bb q.t.padd.rei q.b.padd.rei)
        ?:  (gth h ary)  0  (sub ary h)
    ==
  =.  vlar
    :-  ?~  x.vlar
          ~
        =/  x  ;:(add bl q.l.padd.rei u.x.vlar)
        ?:  |((gth +(x) x.arca) (gth x x.plim))
          ~
        `x
    =/  btpt  (add bt q.t.padd.rei)
    ?~  y.vlar
      ~
    ?:  (gth btpt u.y.vlar)
      ~
    =/  y  (sub u.y.vlar btpt)
    ?:  (lth y y.plim)
      ~
    `y
  =.  alim
    :-  =/  r  (add br q.r.padd.rei)
        ?:((gth r x.alim) 0 (sub x.alim r))
    =/  y  (add y.alim (add bt q.b.padd.rei))
    ?:  (gth +(y) y.arca)
      (dec y.arca)
    y
  =/  ncc  (lent bor)
  =>  ?.  ?=(^ lay)
      .
    ?.  |(repo wrim)
      %_  .
        a
          %_  ^^$
            m     lay
            k     [[%z 0] k]
            cc    0
            cl    0
            px    w.size.rei
            py    h.size.rei
            pl    look.rei
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
        %_  ^^$
          a     *opus
          m     lay
          k     [[%z 0] k]
          cc    0
          cl    0
          px    w.size.rei
          py    h.size.rei
          pl    look.rei
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
          %_  ^^$
            m     nor
            k     [[%xy ncc] k]
            cc    ncc
            cl    0
            px    w.size.rei
            py    h.size.rei
            pl    look.rei
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
        %_  ^^$
          a     *opus
          m     nor
          k     [[%xy ncc] k]
          cc    ncc
          cl    0
          px    w.size.rei
          py    h.size.rei
          pl    look.rei
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
    ?:  |(&(=(~ x.vlar) =(~ y.vlar)) !|(repo imp) =(%text -.ars))
      ~
    =/  i=@ud    0
    =/  ax=axis  %xy
    =/  rig=@ud  ?~(x.vlar x.alim u.x.vlar)
    =/  bot=@ud  ?~(y.vlar y.alim u.y.vlar)
    |-
    =/  el  (~(get by ?:(|(repo wrim) ens.b ens.a)) [[ax i] k])
    ?:  ?&  ?=(^ el)  ?!  ?|
            =(%border-left -.ars.u.el)  =(%border-right -.ars.u.el) :: change this to handle borders instead of excluding them
            =(%border-top -.ars.u.el)  =(%border-bottom -.ars.u.el)
        ==  ==
      =/  el-r=@ud
        ?~  x.lar.u.el
          x.alim
        %+  add  u.x.lar.u.el
          %+  add  r.marg.res.u.el
            ?:(=(0 w.size.res.u.el) 0 (dec w.size.res.u.el))
      =/  el-b=@ud
        ?~  y.lar.u.el
          y.alim
        =/  b  %+  add  b.marg.res.u.el
          ?:(=(0 h.size.res.u.el) 0 (dec h.size.res.u.el))
        ?:((gth b u.y.lar.u.el) 0 (sub u.y.lar.u.el b))
      =?  rig  (gth el-r rig)  el-r
      =?  bot  (lth el-b bot)  el-b
      $(i +(i))
    ?:  =(%xy ax)
      $(ax %z, i 0)
    =?  rig  (gth rig x.alim)  x.alim
    =?  bot  (lth bot y.alim)  y.alim
    :-  ?~  x.vlar
          0
        ?:  (gth u.x.vlar +(rig))
          0
        (sub +(rig) u.x.vlar)
    ?~  y.vlar
      0
    ?:  (gth bot +(u.y.vlar))
      0
    (sub +(u.y.vlar) bot)
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
        =/  rh  (sub i.tvir o.tvir)
        ?:  (gth rh u.y.vlar)
          ~
        [~ (sub u.y.vlar rh)]
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
      ?:  (gth n.tvir u.y.vlar)
        ~
      [~ (sub u.y.vlar n.tvir)]
    ==
  =?  alar  wrim
    ?:  wris
      ?-  d.pow
          %row
        :-  x.alar
        ?~  y.alar
          ~
        =/  rh  (sub i.tvir o.tvir)
        ?:  (gth rh u.y.alar)
          ~
        [~ (sub u.y.alar rh)]
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
      ?:  (gth n.tvir u.y.alar)
        ~
      [~ (sub u.y.alar n.tvir)]
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
        ?:  |((gth +(x) x.arca) (gth x x.plim))
          x.plim
        x
    ?~  y.vlar
      y.plim
    ?:  (gth ary u.y.vlar)
      y.plim
    =/  y  +((sub u.y.vlar ary))
    ?:  (lth y y.plim)
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
        ?:  (gth movy u.y.lar.u.el)
          ~
        =/  y  (sub u.y.lar.u.el movy)
        ?:  (lth y y.alim)
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
        ?:  |((gth +(x) x.arca) (gth movy y.loci.i.v))
          $(v t.v)
        =/  y  (sub y.loci.i.v movy)
        ?:  |((gth x x.alim) (lth y y.alim))
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
          look.rei
      ==
        %text
      =/  len  (lent `vox`?:(=(%text -.ars) vox.ars ~))
      =/  lim  (sub prx ?:(=(%row d.pow) n.vir o.vir))
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
  =/  rend=visa
    ?+  -.ars     (rbox alar plim ares)
        %text   (rtxt alar plim pl w.size.ares `vox`?:(=(%text -.ars) vox.ars ~))
        %layer  ~
    ==
  =?  rend  !=(%layer -.ars)  (~(dif by rend) visa.a)
  =?  vir  
      ?!  ?|  =(%layer -.ars)
        =(%border-left -.ars)
        =(%border-right -.ars)
        =(%border-top -.ars)
        =(%border-bottom -.ars)
      ==
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
  =.  vlar
    =/  vx  ?-(d.pow %row n.vir, %col o.vir)
    =/  vy  ?-(d.pow %row o.vir, %col n.vir)
    :-  ?~  x.plar
          ~
        =/  x  (add u.x.plar vx)
        ?:  |((gth +(x) x.arca) (gth x x.plim))
          ~
        `x
    ?~  y.plar
      ~
    ?:  (gth vy u.y.plar)
      ~
    =/  y  (sub u.y.plar vy)
    ?:  (lth y y.plim)
      ~
    `y
  %=  ^^$
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
::  ::  ::  ::  ::  ::  ::  ::
++  rbox
  |=  [=lar lim=loci =res]
  ^-  visa
  =+  [w=0 h=0 *a=visa]
  |-
  ?~  x.lar  a
  ?~  y.lar  a
  =/  x  (add u.x.lar w)
  ?:  (gth h u.y.lar)
    a
  =/  y  (sub u.y.lar h)
  =/  nrow=bean  (gte +(w) w.size.res)
  ?:  |((gte h h.size.res) (lth y y.lim))
    a
  ?:  |((gth +(x) x.arca) (gth x x.lim))
    $(w ?:(nrow 0 +(w)), h ?:(nrow +(h) h))
  %=  $
    w  ?:(nrow 0 +(w))
    h  ?:(nrow +(h) h)
    a  (~(put by a) [x y] [look.res (@c ' ') ~])
  ==
::
++  rtxt
  |=  [=lar lim=loci =fila cols=@ud =vox]
  ^-  visa
  =+  [w=0 h=0 *a=visa]
  |-
  ?~  vox    a
  ?~  x.lar  a
  ?~  y.lar  a
  =/  x  (add u.x.lar w)
  ?:  (gth h u.y.lar)
    a
  =/  y  (sub u.y.lar h)
  =/  nrow=bean  (gte +(w) cols)
  ?:  (lth y y.lim)
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
++  dolo
  |=  el=@tas
  ^-  rei
  ?+  el
    ::
    :*  size=[[%i 0] [%i 0]]
        padd=[[%c 0] [%c 0] [%c 0] [%c 0]]
        marg=[[%c 0] [%c 0] [%c 0] [%c 0]]
        flex=[0 0]
        flow=[%row %clip]
        look=[~ ~ %w]
    ==
    ::
      %text
    :*  size=[[%i 0] [%i 0]]
        padd=[[%c 0] [%c 0] [%c 0] [%c 0]]
        marg=[[%c 0] [%c 0] [%c 0] [%c 0]]
        flex=[0 0]
        flow=[%row %wrap]
        look=[~ ~ %w]
    ==
    ::
      %layer
    :*  size=[[%p 100] [%p 100]]
        padd=[[%c 0] [%c 0] [%c 0] [%c 0]]
        marg=[[%c 0] [%c 0] [%c 0] [%c 0]]
        flex=[0 0]
        flow=[%row %clip]
        look=[~ ~ %w]
    ==
  ==
::
++  pars
  |=  v=tape
  ^-  as
  ?:  =(~ v)  [%c 0]
  =/  r  (rear v)
  ?:  =('%' r)
    =/  n=(unit @ud)  (slaw %ud (crip (snip v)))
    ?~  n  [%c 0]
    [%p ?:((gth u.n 100) 100 u.n)]
  ?:  =('c' r)
    =/  n=(unit @ud)  (slaw %ud (crip (snip v)))
    ?~  n  [%c 0]
    [%c u.n]
  =/  n=(unit @ud)  (slaw %ud (crip v))
  ?~  n  [%c 0]
  [%c u.n]
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
    ?+  -.ars.u.el  acc
      ::
        %$
      =/  sel=(unit @t)  (~(get by aves.u.el) %sel)
      =/  act=(unit @t)  (~(get by aves.u.el) %act)
      =?  ordo.acc  ?=(^ sel)
        ?~  x.lar.u.el  ordo.acc
        ?~  y.lar.u.el  ordo.acc
        :_  ordo.acc
        :*  u.x.lar.u.el
            (add u.x.lar.u.el ?:(=(0 w.size.res.u.el) 0 (dec w.size.res.u.el)))
            u.y.lar.u.el
            ?:((gth h.size.res.u.el u.y.lar.u.el) 0 +((sub u.y.lar.u.el h.size.res.u.el)))
            k
        ==
      =?  omen.acc  &(?=(^ sel) !(~(has by omen.acc) [%aro %l]))
        %-  %~  gas
              by
            omen.acc
        [[[%aro %l] %nav-l] [[%aro %r] %nav-r] [[%aro %u] %nav-u] [[%aro %d] %nav-d] ~]
      acc
      ::
    ==
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
  ^-  dill-blit:dill
  =/  y=@ud  (dec y.arca)
  :-  %mor
  ^-  (list blit:dill)
  :-  [%clr ~]
  :-  [%nel ~]
  |-
  ?:  =(0 y)
    [[%klr (make-stub y v)] ~]
  :-  [%klr (make-stub y v)]
  [[%nel ~] $(y (dec y))]
::
++  put-blit
  |=  v=visa
  ^-  dill-blit:dill
  =/  y=@ud  0
  :-  %mor
  ^-  (list blit:dill)
  |-
  :-  [%hop 0 y]
  :-  [%klr (make-stub (sub y.arca +(y)) v)] 
  ?:(=(y.arca +(y)) ~ $(y +(y)))
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
--
