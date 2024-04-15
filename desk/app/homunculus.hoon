::
::  O┬┬ ┬┌─┐┌┬┐┬ ┬┌┐┌┌─┐┬ ┬┬  ┬ ┬┌─┐
::  ┌┘├─┤│ │││││ │││││  │ ││  │ │└─┐
::  ┴O┴ ┴└─┘┴ ┴└─┘┘└┘└─┘└─┘┴─┘└─┘└─┘
::
|%
+$  esse  (map rami ens)
+$  ens   [=res =visa =lar =modi =iter =aves =ars]
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
+$  lar   $~([1 1] loci)
+$  modi  loci
+$  axis  ?(%b %l %~)
+$  ager  @ud
+$  aves  (map ?(%sel %act %key) @t)
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
      [%select =acia pro=?(%submit %~)]  [%form ~]
      [%input i=@ud ab=@ud =vox]  [%checkbox v=bean]
      [%radio ~]  [%scroll =iter =muri =sola]  [%$ ~]
  ==
::
+$  omen  (map nota lex)
+$  zona
  $?  [%clk p=?(%d %u) x=@ud y=@ud]
      [%whe p=?(%d %u) x=@ud y=@ud]
      belt:dill
  ==
+$  nota
  $%  [%mod ?(%ctl %met %hyp) bolt:dill]  [%aro ?(%d %l %r %u)]
      [%txt ~]  [%bac ~]  [%ret ~]  [%hit ~]
      [%clk ?(%d %u)]  [%whe ?(%d %u)]
  ==
+$  lex
  $?  %nav-l  %nav-r  %nav-u  %nav-d
      %cur-l  %cur-r  %cur-u  %cur-d
      %scr-l  %scr-r  %scr-u  %scr-d
      %inp  %del  %tog  %act  %clk  %def
  ==
+$  mus   (map loci rami)
+$  equi  (set rami)
+$  dux   [l=@ud r=@ud t=@ud b=@ud k=rami]
+$  rex   $@(~ dux)
+$  ordo  (list dux)
+$  gens  (map @t rami)
::
+$  opus  [=esse =visa]
+$  cura  [=omen =equi =mus =ordo =rex]
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
+$  data
  $%  [%sel p=@t]
      [%act p=@t]
      [%form form]
  ==
+$  form  (pair @t (map @t @t))
::
+$  vela  manx
+$  urbs  $~([50 25] [x=@ud y=@ud])
+$  arx   [x=@ud y=@ud]
+$  fons  [@p @tas]
+$  ara
  $:  =esse  =visa  =vela
      =omen  =rex  =ordo
      =equi  =gens  =mus
      =arx  =lar  =fons
  ==
+$  arae  (map fons ara)
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
  =/  siz=^urbs  ?:(=([0 0] ol) *^urbs ol)
  [~ hoc(ego =+(*^ego -(urbs siz, arx.ara siz)))]
::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  
++  on-poke
  |=  [=mark =vase]
  ^-  (quip card _hoc)
  ?+  mark  ~|("homunculus poke failed with mark: {<mark>}" !!)
    ::
      %elem
    =/  elem       !<(manx vase)
    =/  gen=opus   (geno elem)
    =/  dic=cura   (dico esse.gen)
    =/  lu=lux
      :-  %mor
      :~  (supo visa.gen)
          (fero rex.dic esse.gen)
      ==
    :_  %_  hoc
          esse.ara  esse.gen
          visa.ara  visa.gen
          vela.ara  elem
          rex.ara   rex.dic
          mus.ara   mus.dic
          omen.ara  omen.dic
          ordo.ara  ordo.dic
          equi.ara  equi.dic
          fons.ara
            [src.bowl ?:(&(?=(^ sap.bowl) ?=(^ t.sap.bowl)) i.t.sap.bowl %$)]
        ==
    :~  [%give %fact ~[/homunculus-http] %json !>(^-(json [%s (crip (volo lu))]))]
    ==
    ::
      %json
    =/  jsn  !<(json vase)
    ?:  ?=(%a -.jsn)
      ?.  &(?=(^ p.jsn) ?=(%n -.i.p.jsn) ?=(^ t.p.jsn) ?=(%n -.i.t.p.jsn))
        [~ hoc]
      =/  siz=[@ud @ud]  [(slav %ud p.i.p.jsn) (slav %ud p.i.t.p.jsn)]
      =:  urbs     siz
          arx.ara  siz
        ==
      =/  gen=opus   (geno vela.ara)
      =/  dic=cura   (dico esse.gen)
      =/  lu=lux
        :-  %mor
        :~  (supo visa.gen)
            (fero rex.dic esse.gen)
        ==
      :_  %_  hoc
            esse.ara  esse.gen
            visa.ara  visa.gen
            rex.ara   rex.dic
            mus.ara   mus.dic
            omen.ara  omen.dic
            ordo.ara  ordo.dic
            equi.ara  equi.dic
          ==
      :~  [%give %fact ~[/homunculus-http] %json !>(^-(json [%s (crip (volo lu))]))]
      ==
    ?.  ?=(%s -.jsn)  [~ hoc]
    =/  inp=tape  (trip p.jsn)
    =;  z=(unit zona)
      ?~  z  [~ hoc]
      =/  l=(unit lex)  (~(get by omen.ara) (noto u.z))
      ?~  l  [~ hoc]
      =^  crds  ego
        (novo u.l u.z)
      [crds hoc]
    ?~  inp  ~
    ?.  =('\\' i.inp)     [~ [%txt (tuba inp)]]
    ?~  t.inp             [~ [%txt ~[~-~5c.]]]
    ?~  t.t.inp
      ?:  =('t' i.t.inp)  [~ [%mod %ctl ~-i]]
      ?:  =('n' i.t.inp)  [~ [%ret ~]]
      ~
    ?~  t.t.t.inp  ~
    ?:  =(['1' '7' '7' ~] t.inp)  [~ [%bac ~]]
    =/  seq=tape
      ?:  ?|  &(=('e' i.t.inp) =('[' i.t.t.inp))
              &(=('E' i.t.inp) =('[' i.t.t.inp))
          ==
        t.t.t.inp 
      ?:  ?&  ?=(^ t.t.t.t.inp)
              ?|  ?&  =('0' i.t.inp)  =('3' i.t.t.inp) 
                      =('3' i.t.t.t.inp)  =('[' i.t.t.t.t.inp)
                  ==
                  ?&  =('x' i.t.inp)  =('1' i.t.t.inp) 
                      =('B' i.t.t.t.inp)  =('[' i.t.t.t.t.inp)
                  ==
                  ?&  =('x' i.t.inp)  =('9' i.t.t.inp) 
                      =('B' i.t.t.t.inp)  =('[' i.t.t.t.t.inp)
          ==  ==  ==
        t.t.t.t.inp
      ~
    ?~  seq  ~
    ?~  t.seq
      ?:  =('A' i.seq)  [~ [%aro %u]]
      ?:  =('B' i.seq)  [~ [%aro %d]]
      ?:  =('C' i.seq)  [~ [%aro %r]]
      ?:  =('D' i.seq)  [~ [%aro %l]]
      ~
    ?~  t.t.seq
      ?:  =(['3' '~' ~] seq)  [~ [%del ~]]
      ~
    ?:  ?|  =(['<' '0'] [i.seq i.t.seq])  =(['<' '6'] [i.seq i.t.seq])
            =(['3' '2'] [i.seq i.t.seq])  =(['3' '5'] [i.seq i.t.seq])
            =(['9' '6'] [i.seq i.t.seq])  =(['9' '7'] [i.seq i.t.seq])
        ==
      =/  loc=tape
        ?:  |(=('0' i.t.seq) =('3' i.seq) =('9' i.seq))
          t.t.t.seq
        ?:  =('6' i.t.seq)
          ?~(t.t.t.seq ~ t.t.t.t.seq)
        ~
      =|  [xt=tape yt=tape]
      |-  ^-  (unit zona)
      ?~  loc  ~
      ?.  =(';' i.loc)
        $(loc t.loc, xt [i.loc xt])
      |-  ^-  (unit zona)
      ?~  t.loc  ~
      ?.  |(=('M' i.t.loc) =('m' i.t.loc))
        $(t.loc t.t.loc, yt [i.t.loc yt])
      ?:  |(=(['<' '0'] [i.seq i.t.seq]) =('3' i.seq))
        :^    ~
            %clk
          ?:  =(['<' '0'] [i.seq i.t.seq])
            ?:  =('M' i.t.loc)  %d
            ?:  =('m' i.t.loc)  %u  !!
          ?:  =('3' i.seq)
            ?:  =('2' i.t.seq)  %d
            ?:  =('5' i.t.seq)  %u  !!  !!
        :-  ^-(@ud (slav %ud (crip ^-(tape (flop xt)))))
        ^-(@ud (slav %ud (crip ^-(tape (flop yt)))))
      ?:  |(=(['<' '6'] [i.seq i.t.seq]) =('9' i.seq))
        :^    ~
            %whe
          ?:  =(['<' '6'] [i.seq i.t.seq])
            ?:  =('4' i.t.t.seq)  %u
            ?:  =('5' i.t.t.seq)  %d  !!
          ?:  =('9' i.seq)
            ?:  =('6' i.t.seq)  %u
            ?:  =('7' i.t.seq)  %d  !!  !!
        :-  ^-(@ud (slav %ud (crip ^-(tape (flop xt)))))
        ^-(@ud (slav %ud (crip ^-(tape (flop yt)))))
      ~
    ~  :: TODO: parse mod+arrow
    ::
  ==
::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  
++  on-watch
  |=  =path
  ?+  path  [~ hoc]
    ::
      [%homunculus-http ~]
    =/  =lux
      :-  %mor
      :~  (supo visa.ara)
          (fero rex.ara esse.ara)
      ==
    =/  ansi=@t  (crip ^-(tape ['\\e[1;1H\\e[3J\\e[0J' (volo lux)]))
    :_  hoc
    :~  [%give %fact ~[/homunculus-http] %json !>(^-(json [%s ansi]))]
    ==
    ::
  ==
::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  
++  on-leave  |=(path ^-((quip card _hoc) !!))
++  on-peek   |=(path ^-((unit (unit cage)) !!))
++  on-agent  |=([wire sign:agent:gall] ^-((quip card _hoc) !!))
++  on-arvo   |=([=wire sign=sign-arvo] ^-((quip card _hoc) !!))
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
      [[%whe %u] %scr-u]  [[%whe %d] %scr-d]
      [[%clk %u] %clk]  [[%clk %d] %clk]
      [[%ret ~] %act]
  ==
::
++  hinp                    :: hotkey context group for an input element
  ^-  omen
  %-  malt
  ^-  (list [nota lex])
  :~  [[%aro %l] %cur-l]  [[%aro %r] %cur-r]
      [[%aro %u] %cur-u]  [[%aro %d] %cur-d]
      [[%whe %u] %scr-u]  [[%whe %d] %scr-d]
      [[%clk %u] %clk]  [[%clk %d] %clk]
      [[%txt ~] %inp]  [[%bac ~] %del]
  ==
::  ::  ::
++  novo                    :: handle an event from the hotkey context
  |=  [=lex =zona]
  ^-  (quip card _ego)
  ::
  ?:  |(?=(%nav-l lex) ?=(%nav-r lex) ?=(%nav-u lex) ?=(%nav-d lex))
    |^
    =/  scr=$@(~ rami)  ?~(rex.ara ~ (ligo k.rex.ara))
    =/  spar=(unit ens)  ?~(scr ~ (~(get by esse.ara) scr))
    =/  navs=ordo  (navi rex.ara ordo.ara)
    =/  snav=ordo
      ?~  scr  ~
      %+  skim  ^-(ordo navs)
      |=  =dux
      (chil scr k.dux)
    =/  next=rex  ?~(navs ~ ?~(snav i.navs i.snav))
    =/  send=bean
      ?&  ?=(^ rex.ara)  ?=(^ spar)  ?=(%scroll -.ars.u.spar)  =(scr k.rex.ara)
          ?|  &(?=(%nav-l lex) =(0 x.iter.ars.u.spar))
              &(?=(%nav-r lex) =(x.sola.ars.u.spar x.iter.ars.u.spar))
              &(?=(%nav-u lex) =(0 y.iter.ars.u.spar))
              &(?=(%nav-d lex) =(y.sola.ars.u.spar y.iter.ars.u.spar))
      ==  ==
    =?  scr   &(send ?=(^ scr) ?=(^ t.scr))  
      =/  pscr=$@(~ rami)  (ligo t.scr)
      ?~(pscr scr pscr)
    =?  spar  &(send ?=(^ scr))  (~(get by esse.ara) scr)
    =?  next  &(send ?=(~ scr) ?=(^ navs))  i.navs
    =?  snav  &(send ?=(^ scr))
      %+  skim  ^-(ordo navs)
      |=  =dux
      (chil scr k.dux)
    =?  next  &(send ?=(^ scr) ?=(^ snav))  i.snav
    =/  upd=$@(~ [opus cura])
      ?~  scr  ~
      ?:  ?=(^ snav)  ~
      %+  abeo  scr
      ?+  lex   lex
        %nav-l  %scr-l
        %nav-r  %scr-r
        %nav-u  %scr-u
        %nav-d  %scr-d
      ==
    =?  ara  ?=(^ upd)
      %_  ara
        esse  esse.upd
        rex   rex.upd
        mus   mus.upd
        omen  omen.upd
        ordo  ordo.upd
        equi  equi.upd
      ==
    =?  next  ?=(^ upd)
      ?~  scr  ~
      =<  ?~(. ~ i)
      %+  skim  ^-(ordo (navi rex.ara ordo.upd))
      |=  =dux
      (chil scr k.dux)
    =/  prae=opus
      ?~  rex.ara  [~ ~]
      (duco esse.ara k.rex.ara next)
    =.  rex.ara
      ?:  ?=(^ next)
        next
      ?:  |(?=(~ scr) ?=(~ spar) ?=(~ upd) ?=(^ (find ~[rex.ara] ordo.upd)))
        rex.ara
      (rogo scr ordo.ara)
    =/  post=opus
      ?:  ?=(^ next)
        (duco esse.ara k.next rex.ara)
      ?:  ?=(^ rex.ara)
        (duco esse.ara k.rex.ara rex.ara)
      [~ ~]
    =/  sel=(unit ens)  ?~(rex.ara ~ (~(get by esse.ara) k.rex.ara))
    =.  omen.ara
      ?~  sel  omen.ara
      ?+  -.ars.u.sel  (~(uni by omen.ara) hnav)
        %input         (~(uni by omen.ara) hinp)
      ==
    =.  esse.ara  (~(uni by esse.ara) (~(uni by esse.prae) esse.post))
    =/  ppv=visa  (~(uni by visa.prae) visa.post)
    =?  upd  ?=(^ upd)  upd(visa (~(uni by visa.upd) ppv))
    =/  =lux
      :-  %mor
      :~  (dono visa.ara ?~(upd ppv visa.upd))
          ^-  lux
          ?:  &(?=(^ sel) ?=(%input -.ars.u.sel))
            (vado ab.ars.u.sel i.ars.u.sel w.size.res.u.sel lar.u.sel iter.u.sel)
          ?~  rex.ara  [%hop [1 1]]
          ?:  |(?=(~ spar) &(?=(~ upd) ?=(~ snav) ?=(^ navs)) =(k.rex.ara scr))
            [%hop [l.rex.ara t.rex.ara]]
          (cedo rex.ara scr u.spar)
      ==
    =/  avis=(unit @t)  ?:(|(?=(~ next) ?=(~ sel)) ~ (~(get by aves.u.sel) %sel))
    :_  ego(visa.ara ?~(upd (~(uni by visa.ara) ppv) visa.upd))
    :-  [%give %fact ~[/homunculus-http] %json !>(^-(json [%s (crip (volo lux))]))]
    ?~  avis  ~
    [[%pass /sel %agent fons.ara %poke %homunculus !>(^-(data [%sel u.avis]))] ~]
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
      |-  ^-  bean
      ?:  =(a b)
        &
      ?~  t.b
        |
      $(b t.b)
    ++  abov
      |=  [a=rami b=rami]
      =/  a=$@(~ rami)  (flop a)
      =/  b=$@(~ rami)  (flop b)
      |-  ^-  ?(%a %b %~)
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
          (pow (mul ?:((gte t.rex t.dux) (sub t.rex t.dux) (sub t.dux t.rex)) 2) 2)
        %-  sqt  %+  add
          (pow ?:((lte r.dux l.rex) (sub l.rex r.dux) (sub r.dux l.rex)) 2)
        (pow (mul ?:((gte t.rex t.dux) (sub t.rex t.dux) (sub t.dux t.rex)) 2) 2)
          %nav-u
        ?:  |((lap dux rex) !=(~ (abov k.rex k.dux)))
          %-  sqt  %+  add
            (pow ?:((gte l.dux l.rex) (sub l.dux l.rex) (sub l.rex l.dux)) 2)
          (pow (mul ?:((lte t.dux t.rex) (sub t.rex t.dux) (sub t.dux t.rex)) 2) 2)
        %-  sqt  %+  add
          (pow ?:((gte l.dux l.rex) (sub l.dux l.rex) (sub l.rex l.dux)) 2)
        (pow (mul ?:((lte b.dux t.rex) (sub t.rex b.dux) (sub b.dux t.rex)) 2) 2)
          %nav-r
        ?:  |((lap dux rex) !=(~ (abov k.rex k.dux)))
          %-  sqt  %+  add
            (pow ?:((lte l.rex l.dux) (sub l.dux l.rex) (sub l.rex l.dux)) 2)
          (pow (mul ?:((gte t.dux t.rex) (sub t.dux t.rex) (sub t.rex t.dux)) 2) 2)
        %-  sqt  %+  add
          (pow ?:((lte r.rex l.dux) (sub l.dux r.rex) (sub r.rex l.dux)) 2)
        (pow (mul ?:((gte t.dux t.rex) (sub t.dux t.rex) (sub t.rex t.dux)) 2) 2)
          %nav-d
        ?:  |((lap dux rex) !=(~ (abov k.rex k.dux)))
          %-  sqt  %+  add
            (pow ?:((gte l.rex l.dux) (sub l.rex l.dux) (sub l.dux l.rex)) 2)
          (pow (mul ?:((lte t.rex t.dux) (sub t.dux t.rex) (sub t.rex t.dux)) 2) 2)
        %-  sqt  %+  add
          (pow ?:((gte l.rex l.dux) (sub l.rex l.dux) (sub l.dux l.rex)) 2)
        (pow (mul ?:((lte b.rex t.dux) (sub t.dux b.rex) (sub b.rex t.dux)) 2) 2)
      ==
    --
  ::
  :: ?:  ?=(%tog lex)                                :: temporarily removed
  ::   ?~  rex.ara  [~ ego]
  ::   =/  el=(unit ens)  (~(get by esse.ara) k.rex.ara)
  ::   ?~  el  [~ ego]
  ::   =/  ev=(unit ^lex)  (~(get by omen.ara) [%aro %l])
  ::   ?~  ev  [~ ego]
  ::   ?+  -.ars.u.el  [~ ego]
  ::       %input
  ::     ?.  ?=(%nav-l u.ev)
  ::       :_  ego(omen.ara (~(uni by (~(dif by omen.ara) hinp)) hnav))
  ::       [[%give %fact ~[/dill/$] %dill-blit !>([%hop x.lar.u.el y.lar.u.el])] ~]
  ::     :_  ego(omen.ara (~(uni by omen.ara) hinp))
  ::     :~  :*  %give  %fact  ~[/dill/$]  %dill-blit  
  ::       !>((vado ab.ars.u.el i.ars.u.el w.size.res.u.el lar.u.el iter.u.el))
  ::     ==  ==
  ::   ==
  ::
  ?:  ?=(%act lex)
    ?~  rex.ara  [~ ego]
    =/  el=(unit ens)  (~(get by esse.ara) k.rex.ara)
    ?~  el  [~ ego]
    ?:  ?=(%input -.ars.u.el)  [~ ego]
    =/  fupd=$@(~ [opus =form])
      ?.  &(?=(%select -.ars.u.el) ?=(%submit pro.ars.u.el))
        ~
      (lego k.rex.ara)
    =/  cupd=$@(~ opus)
      ?.  ?=(%checkbox -.ars.u.el)
        ~
      (opto k.rex.ara u.el)
    =/  avis=(unit @t)  (~(get by aves.u.el) %act)
    ?:  &(?=(~ fupd) ?=(~ cupd))
      ?~  avis  [~ ego]
      :_  ego
      :~  [%pass /act %agent fons.ara %poke %homunculus !>(^-(data [%act u.avis]))]
      ==
    =.  esse.ara
      ?^  fupd
        (~(uni by esse.ara) esse.fupd)
      ?^  cupd
        (~(uni by esse.ara) esse.cupd)
      esse.ara
    =/  =lux
      :-  %mor
      :~  (dono visa.ara ?^(fupd visa.fupd ?^(cupd visa.cupd ~)))
          (fero rex.ara esse.ara)
      ==
    :_  ego(visa.ara (~(uni by visa.ara) ?^(fupd visa.fupd ?^(cupd visa.cupd ~))))
    :-  [%give %fact ~[/homunculus-http] %json !>(^-(json [%s (crip (volo lux))]))]
    ?:  &(?=(^ fupd) ?=(^ avis))
      :-  [%pass /act %agent fons.ara %poke %homunculus !>(^-(data [%form form.fupd]))]
      [[%pass /act %agent fons.ara %poke %homunculus !>(^-(data [%act u.avis]))] ~]
    ?^  fupd
      [[%pass /act %agent fons.ara %poke %homunculus !>(^-(data [%form form.fupd]))] ~]
    ?^  avis
      [[%pass /act %agent fons.ara %poke %homunculus !>(^-(data [%act u.avis]))] ~]
    ~
  ::
  ?:  ?=(%clk lex)
    ?.  ?=(%clk -.zona)  [~ ego]
    =/  mk=(unit rami)  (~(get by mus.ara) [x.zona y.zona])
    ?~  mk  [~ ego]
    ?:  &(?=(^ rex.ara) =(u.mk k.rex.ara))
      ?.  ?=(%u p.zona)  [~ ego]
      (novo %act zona)
    =/  el=(unit ens)  (~(get by esse.ara) u.mk)
    ?~  el  [~ ego]
    =/  nrex=rex  (rogo u.mk ordo.ara)
    =/  prae=opus  ?~(rex.ara [~ ~] (duco esse.ara k.rex.ara nrex))
    =.  rex.ara  nrex
    =/  post=opus  ?~(nrex [~ ~] (duco esse.ara k.nrex rex.ara))
    =.  omen.ara
      ?+  -.ars.u.el   (~(uni by omen.ara) hnav)
        %input         (~(uni by omen.ara) hinp)
      ==
    =.  esse.ara  (~(uni by esse.ara) (~(uni by esse.prae) esse.post))
    =/  nvis=visa  (~(uni by visa.prae) visa.post)
    =/  =lux
      :-  %mor
      :~  (dono visa.ara nvis)
          (fero rex.ara esse.ara)
      ==
    =/  avis=(unit @t)  (~(get by aves.u.el) %sel)
    :_  ego(visa.ara (~(uni by visa.ara) nvis))
    :-  [%give %fact ~[/homunculus-http] %json !>(^-(json [%s (crip (volo lux))]))]
    ?~  avis  ~
    [[%pass /sel %agent fons.ara %poke %homunculus !>(^-(data [%sel u.avis]))] ~]
  ::
  ?:  ?=(%inp lex)
    ?.  ?=(%txt -.zona)  [~ ego]
    ?~  p.zona  [~ ego]
    ?~  rex.ara  [~ ego]
    =/  el=(unit ens)  (~(get by esse.ara) k.rex.ara)
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
    =/  lim=modi  [(add x.lar.u.el w.size.res.u.el) (add y.lar.u.el h.size.res.u.el)]
    =/  vi=visa  (rinp lar.u.el lim res.u.el ab.ars.u.el vox.ars.u.el)
    =?  vi  !&(=(0 x.iter.u.el) =(0 y.iter.u.el))
      %-  %~  rep
            by
          vi
      |=  [[l=loci c=[fila @c ~]] acc=visa]
      ^-  visa
      ?:  |((gth x.iter.u.el x.l) (gth y.iter.u.el y.l))
        acc
      (~(put by acc) [(sub x.l x.iter.u.el) (sub y.l y.iter.u.el)] c)
    =.  vi  (~(int by visa.u.el) vi)
    =.  visa.u.el  vi
    =/  =lux
      :-  %mor
      :~  (dono visa.ara vi)
          (vado ab.ars.u.el i.ars.u.el w.size.res.u.el lar.u.el iter.u.el)
      ==
    :_  ego(esse.ara (~(put by esse.ara) k.rex.ara u.el), visa.ara (~(uni by visa.ara) vi))
    :~  [%give %fact ~[/homunculus-http] %json !>(^-(json [%s (crip (volo lux))]))]
    ==
  ::
  ?:  ?=(%del lex)
    ?~  rex.ara  [~ ego]
    =/  el=(unit ens)  (~(get by esse.ara) k.rex.ara)
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
    =/  lim=modi  [(add x.lar.u.el w.size.res.u.el) (add y.lar.u.el h.size.res.u.el)]
    =/  vi=visa  (rinp lar.u.el lim res.u.el ab.ars.u.el vox.ars.u.el)
    =?  vi  !&(=(0 x.iter.u.el) =(0 y.iter.u.el))
      %-  %~  rep
            by
          vi
      |=  [[l=loci c=[fila @c ~]] acc=visa]
      ^-  visa
      ?:  |((gth x.iter.u.el x.l) (gth y.iter.u.el y.l))
        acc
      (~(put by acc) [(sub x.l x.iter.u.el) (sub y.l y.iter.u.el)] c)
    =.  vi  (~(int by visa.u.el) vi)
    =.  visa.u.el  vi
    =/  =lux
      :-  %mor
      :~  (dono visa.ara vi)
          (vado ab.ars.u.el i.ars.u.el w.size.res.u.el lar.u.el iter.u.el)
      ==
    :_  ego(esse.ara (~(put by esse.ara) k.rex.ara u.el), visa.ara (~(uni by visa.ara) vi))
    :~  [%give %fact ~[/homunculus-http] %json !>(^-(json [%s (crip (volo lux))]))]
    ==
  ::
  ?:  |(?=(%cur-l lex) ?=(%cur-r lex) ?=(%cur-u lex) ?=(%cur-d lex))
    ?~  rex.ara  [~ ego]
    =/  el=(unit ens)  (~(get by esse.ara) k.rex.ara)
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
          =/  m=@ud  (mod i.ars.u.el w.size.res.u.el)
          =/  l=@ud  (lent vox.ars.u.el)
          ?:((gth m l) 0 (sub l m))
        ?:  (gte (sub i.ars.u.el ab.ars.u.el) (mul w.size.res.u.el h.size.res.u.el))
          ?:  =(1 h.size.res.u.el)
            +(ab.ars.u.el)
          (add ab.ars.u.el w.size.res.u.el)
        ab.ars.u.el
      ab.ars.u.el
    ?:  =(ab.ars.u.el nab)
      =/  ansi=@t
        (crip (volo (vado ab.ars.u.el i.ars.u.el w.size.res.u.el lar.u.el iter.u.el)))
      :_  ego(esse.ara (~(put by esse.ara) k.rex.ara u.el))
      :~  [%give %fact ~[/homunculus-http] %json !>(^-(json [%s ansi]))]
      ==
    =.  ab.ars.u.el  nab
    =/  lim=modi  [(add x.lar.u.el w.size.res.u.el) (add y.lar.u.el h.size.res.u.el)]
    =/  vi=visa  (rinp lar.u.el lim res.u.el ab.ars.u.el vox.ars.u.el)
    =?  vi  !&(=(0 x.iter.u.el) =(0 y.iter.u.el))
      %-  %~  rep
            by
          vi
      |=  [[l=loci c=[fila @c ~]] acc=visa]
      ^-  visa
      ?:  |((gth x.iter.u.el x.l) (gth y.iter.u.el y.l))
        acc
      (~(put by acc) [(sub x.l x.iter.u.el) (sub y.l y.iter.u.el)] c)
    =.  vi  (~(int by visa.u.el) vi)
    =.  visa.u.el  vi
    =/  =lux
      :-  %mor
      :~  (dono visa.ara vi)
          (vado ab.ars.u.el i.ars.u.el w.size.res.u.el lar.u.el iter.u.el)
      ==
    :_  ego(esse.ara (~(put by esse.ara) k.rex.ara u.el), visa.ara (~(uni by visa.ara) vi))
    :~  [%give %fact ~[/homunculus-http] %json !>(^-(json [%s (crip (volo lux))]))]
    ==
  ::
  ?:  |(?=(%scr-u lex) ?=(%scr-d lex))
    ?.  ?=(%whe -.zona)  [~ ego]
    =/  mk=(unit rami)  (~(get by mus.ara) [x.zona y.zona])
    ?~  mk  [~ ego]
    =/  sk=$@(~ rami)  (ligo u.mk)
    ?~  sk  [~ ego]
    =/  upd=$@(~ [opus cura])
      ?:  |(?=(~ rex.ara) =(sk k.rex.ara))
        (abeo sk lex)
      =/  nrex=rex  (rogo sk ordo.ara)
      ?~  nrex  ~
      =/  prae=opus  (duco esse.ara k.rex.ara nrex)
      =.  rex.ara  nrex
      =:  esse.ara  ?~(esse.prae esse.ara (~(uni by esse.ara) ^-(esse esse.prae)))
          visa.ara  ?~(visa.prae visa.ara (~(uni by visa.ara) ^-(visa visa.prae)))
        ==
      (abeo sk lex)
    ?~  upd  [~ ego]
    =:  esse.ara  esse.upd
        rex.ara   rex.upd
        mus.ara   mus.upd
        omen.ara  omen.upd
        ordo.ara  ordo.upd
        equi.ara  equi.upd
      ==
    =/  =lux
      :-  %mor
      :~  (dono visa.ara visa.upd)
          (fero rex.ara esse.ara)
      ==
    :_  ego(visa.ara visa.upd)
    :~  [%give %fact ~[/homunculus-http] %json !>(^-(json [%s (crip (volo lux))]))]
    ==
  ::
  [~ ego]
::
++  abeo                    :: handle a scroll event for a scroll element by key
  |=  [pk=rami =lex]        :: returns an update from the root
  ^-  $@(~ [opus cura])
  =/  par=(unit ens)  (~(get by esse.ara) pk)
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
  =/  prl=@ud
    =/  l=@ud  ;:(add x.lar.u.par l.muri.ars.u.par l.padd.res.u.par)
    ?:((lth x.iter.u.par l) (sub l x.iter.u.par) 1)
  =/  prr=@ud
    =/  r=@ud  +((add r.muri.ars.u.par r.padd.res.u.par))
    =.  r  (add x.lar.u.par ?:((gth r w.size.res.u.par) 0 (sub w.size.res.u.par r)))
    ?:((lth x.iter.u.par r) (sub r x.iter.u.par) 1)
  =/  prt=@ud
    =/  t=@ud  ;:(add y.lar.u.par t.muri.ars.u.par t.padd.res.u.par)
    ?:((lth y.iter.u.par t) (sub t y.iter.u.par) 1)
  =/  prb=@ud
    =/  b=@ud  +((add b.muri.ars.u.par b.padd.res.u.par))
    =.  b  (add y.lar.u.par ?:((gth b h.size.res.u.par) 0 (sub h.size.res.u.par b)))
    ?:((lth y.iter.u.par b) (sub b y.iter.u.par) 1)
  =/  opu=opus  (eo esse.ara visa.ara lex ~ pk visa.u.par prl prt prr prb)
  =/  pvi=visa
    %-  %~  dif
          by
        =.  lar.u.par
          :-  ?:((lth x.iter.u.par x.lar.u.par) (sub x.lar.u.par x.iter.u.par) 1)
          ?:((lth y.iter.u.par y.lar.u.par) (sub y.lar.u.par y.iter.u.par) 1)
        %^  rbox  lar.u.par
          [(add x.lar.u.par w.size.res.u.par) (add y.lar.u.par h.size.res.u.par)]
        res.u.par
    visa.opu
  =.  visa.u.par  pvi
  =:  esse.opu  (~(put by esse.opu) pk u.par)
      visa.opu  (~(uni by visa.opu) visa.u.par)
    ==
  [opu (dico esse.opu)]
::
++  eo                      :: perform a scroll on the elements within a scroll element
  |=  $:  e=esse  v=visa  lx=$@(~ lex)  :: returns the esse and visa passed in with the update applied
          nitr=$@(~ loci)  pk=rami  pv=visa
          prl=@ud  prt=@ud  prr=@ud  prb=@ud
      ==
  ^-  opus
  =/  olv=visa
    =/  k=rami  [[%l 0] pk]
    =/  a=visa  pv
    |-  ^-  visa
    =/  chi=(unit ens)  (~(get by e) k)
    ?~  chi
      ?:  ?=(%b axis.i.k)  $(k [[%l 0] t.k])
      ?:  ?=(%l axis.i.k)  $(k [[%~ 0] t.k])
      a
    =.  a  (~(uni by a) visa.u.chi)
    $(ager.i.k +(ager.i.k), a $(k [[%b 0] k]))
  =;  opu=opus
    %_  opu
      esse  (~(uni by e) esse.opu)
      visa  (~(uni by (~(dif by v) olv)) visa.opu)
    ==
  =/  k=rami           [[%l 0] pk]
  =/  a=opus           [~ ~]
  =/  slar=$@(~ modi)  ~
  =/  slim=$@(~ modi)  ~
  |-  ^-  opus
  =/  chi=(unit ens)  (~(get by e) k)
  ?~  chi
    ?:  ?=(%b axis.i.k)
      a
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
  =.  a  $(k [[%b 0] k])
  =.  a
    %=  $
      k     [[%l 0] k]
      slar  ?.  ?=(%scroll -.ars.u.chi)  slar
            =/  [x=@ud y=@ud]
              :-  (add x.lar.u.chi (add l.padd.res.u.chi l.muri.ars.u.chi))
              (add y.lar.u.chi (add t.padd.res.u.chi t.muri.ars.u.chi))
            :-  ?:((lth x.iter.u.chi x) (sub x x.iter.u.chi) 1)
            ?:((lth y.iter.u.chi y) (sub y y.iter.u.chi) 1)
      slim  ?.  ?=(%scroll -.ars.u.chi)  slim
            =/  [xmov=@ud ymov=@ud]
              :-  ;:(add r.padd.res.u.chi r.muri.ars.u.chi x.iter.u.chi)
              ;:(add b.padd.res.u.chi b.muri.ars.u.chi y.iter.u.chi)
            =/  [x=@ud y=@ud]
              :-  ?:((lth xmov x.modi.u.chi) (sub x.modi.u.chi xmov) 1)
              ?:((lth ymov y.modi.u.chi) (sub y.modi.u.chi ymov) 1)
            ?~  slim  [x y]
            [?:((lth x x.slim) x x.slim) ?:((lth y y.slim) y y.slim)]
    ==
  =/  crig=@ud  (add x.lar.u.chi ?:(=(0 w.size.res.u.chi) 0 (dec w.size.res.u.chi)))
  =/  cbot=@ud  (add y.lar.u.chi ?:(=(0 h.size.res.u.chi) 0 (dec h.size.res.u.chi)))
  ?:  ?|  (gth y.iter.u.chi cbot)
          (lth (sub cbot y.iter.u.chi) prt)
          &((lte y.iter.u.chi y.lar.u.chi) (gth (sub y.lar.u.chi y.iter.u.chi) prb))
          (gth x.iter.u.chi crig)
          (lth (sub crig x.iter.u.chi) prl)
          &((lte x.iter.u.chi x.lar.u.chi) (gth (sub x.lar.u.chi x.iter.u.chi) prr))
      ==
    =.  esse.a  (~(put by esse.a) k u.chi(visa ~))
    $(ager.i.k +(ager.i.k))
  =/  vi=visa  (viso lar.u.chi res.u.chi ars.u.chi modi.u.chi)
  =.  vi
    %-  %~  rep
          by
        vi
    |=  [[l=loci c=[fila @c ~]] acc=visa]
    ^-  visa
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
  =.  esse.a  (~(put by esse.a) k u.chi(visa vi))
  =.  visa.a  (~(uni by visa.a) vi)
  $(ager.i.k +(ager.i.k))
::
++  ligo                    :: find the key of a potential scroll parent by child key
  |=  r=rami
  ^-  $@(~ rami)
  ?:  (~(has in equi.ara) r)
    r
  ?~  t.r  ~
  $(r t.r)
::
++  cedo                    :: move the cursor within a scroll element
  |=  [=rex sk=$@(~ rami) spar=ens]
  ^-  lux
  :-  %hop
  ?~  rex
    [1 1]
  =/  loc=loci
    ?:  |-  ^-  bean
        ?:  &(?=(%b axis.i.k.rex) =(sk t.k.rex))
          &
        ?~(t.k.rex | $(k.rex t.k.rex))
      lar.spar
    ?.  ?=(%scroll -.ars.spar)
      lar.spar
    :-  (add x.lar.spar (add l.muri.ars.spar l.padd.res.spar))
    (add y.lar.spar (add t.muri.ars.spar t.padd.res.spar))
  =.  loc
    :-  ?:((lth x.iter.spar x.loc) (sub x.loc x.iter.spar) 1)
    ?:((lth y.iter.spar y.loc) (sub y.loc y.iter.spar) 1)
  :-  ?:((lth l.rex x.loc) x.loc l.rex)
  ?:((lth t.rex y.loc) y.loc t.rex)
::
++  vado                    :: move the cursor to an input character index
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
    ?:((lth x.iter x) (sub x x.iter) 1)
  ?:((lth y.iter y) (sub y y.iter) 1)
::
++  fero                    :: make a general cursor move update
  |=  [r=rex e=esse]
  ^-  lux
  ?~  r  [%hop [1 1]]
  =/  el=(unit ens)  (~(get by e) k.r)
  ?~  el  [%hop [1 1]]
  ?:  ?=(%input -.ars.u.el)
    (vado ab.ars.u.el i.ars.u.el w.size.res.u.el lar.u.el iter.u.el)
  ?:  &(=(0 x.iter.u.el) =(0 y.iter.u.el))
    [%hop [l.r t.r]]
  =/  sk=$@(~ rami)  (ligo k.r)
  ?~  sk  [%hop [l.r t.r]]
  =/  spar=(unit ens)  (~(get by e) sk)
  ?~  spar  [%hop [l.r t.r]]
  (cedo r sk u.spar)
::
++  rogo                    :: find a dux in ordo by key
  |=  [k=rami o=ordo]
  ^-  $@(~ dux)
  ?~  o  ~
  ?:  =(k k.i.o)
    i.o
  $(o t.o)
::
++  opto                    :: process a checkbox, potentially in a radio group
  |=  [k=rami e=ens]
  ^-  opus
  ?.  ?=(%checkbox -.ars.e)  [~ ~]
  =/  rad
    |-  ^-  $@(~ [k=rami e=ens])
    ?~  t.k  ~
    =/  el=(unit ens)  (~(get by esse.ara) t.k)
    ?~  el  $(k t.k)
    ?:  ?=(%radio -.ars.u.el)
      [t.k u.el]
    $(k t.k)
  ?:  |(?=(~ rad) v.ars.e)
    =.  v.ars.e  !v.ars.e
    =.  visa.e   (~(int by visa.e) (ruo iter.e (viso lar.e res.e ars.e modi.e)))
    [^-(esse (malt ~[[k e]])) visa.e]
  =/  opu
    =/  [rk=rami acc=opus]  [[[%b 0] k.rad] [~ ~]]
    |-  ^-  opus
    =/  el=(unit ens)  (~(get by esse.ara) rk)
    ?~  el
      ?:  ?=(%b axis.i.rk)  $(rk [[%l 0] t.rk])
      ?:  ?=(%l axis.i.rk)  $(rk [[%~ 0] t.rk])
      acc
    ?.  ?=(%checkbox -.ars.u.el)
      $(ager.i.rk +(ager.i.rk), acc $(rk [[%b 0] rk]))
    ?:  !v.ars.u.el
      $(ager.i.rk +(ager.i.rk), acc $(rk [[%b 0] rk]))
    =.  v.ars.u.el  |
    =.  visa.u.el
      (~(int by visa.u.el) (ruo iter.u.el (viso lar.u.el res.u.el ars.u.el modi.u.el)))
    %=  $
      ager.i.rk  +(ager.i.rk)
      esse.acc   (~(put by esse.acc) rk u.el)
      visa.acc   (~(uni by visa.acc) visa.u.el)
    ==
  =.  v.ars.e  !v.ars.e
  =.  visa.e   (~(int by visa.e) (ruo iter.e (viso lar.e res.e ars.e modi.e)))
  opu(esse (~(put by esse.opu) k e), visa (~(uni by visa.opu) visa.e))
::
++  lego                    :: reset and collect the values of inputs under a form element
  |=  sk=rami
  ^-  [opus form]
  =|  acc=[opus =form]
  =/  fk
    |-  ^-  $@(~ rami)
    ?~  t.sk  ~
    =/  el=(unit ens)  (~(get by esse.ara) t.sk)
    ?~  el  ~
    ?:  ?=(%form -.ars.u.el)
      t.sk
    $(sk t.sk)
  ?~  fk  acc
  |-  ^-  [opus form]
  =/  el=(unit ens)  (~(get by esse.ara) fk)        :: TODO: ADD a case here for checkbox and radio 
  ?~  el
    ?:  ?=(%b axis.i.fk)  $(fk [[%l 0] t.fk])
    ?:  ?=(%l axis.i.fk)  $(fk [[%~ 0] t.fk])
    acc
  ?:  &(?=(%form -.ars.u.el) =(~ p.form.acc))
    =/  key=(unit @t)  (~(get by aves.u.el) %key)
    %=  $
      ager.i.fk  +(ager.i.fk)
      acc        $(fk [[%b 0] fk], p.form.acc ?~(key '' u.key))
    ==
  ?:  ?=(%input -.ars.u.el)
    =/  key=(unit @t)  (~(get by aves.u.el) %key)
    ?~  key
      ~&  'key missing on form submit'
      $(ager.i.fk +(ager.i.fk), acc $(fk [[%b 0] fk]))
    =/  val=@t  (crip vox.ars.u.el)
    =:  vox.ars.u.el  ~
        ab.ars.u.el   0
        i.ars.u.el    0
        visa.u.el
          %-  %~  int
                by
              visa.u.el
          (ruo iter.u.el (rinp lar.u.el modi.u.el res.u.el 0 ~))
      ==
    =:  esse.acc  (~(put by esse.acc) fk u.el)
        visa.acc  (~(uni by visa.acc) visa.u.el)
        q.form.acc  (~(put by q.form.acc) u.key val)
      ==
    $(ager.i.fk +(ager.i.fk), acc $(fk [[%b 0] fk]))
  ?:  ?=(%checkbox -.ars.u.el)
    =/  key=(unit @t)  (~(get by aves.u.el) %key)
    ?~  key
      ~&  'key missing on form submit'
      $(ager.i.fk +(ager.i.fk), acc $(fk [[%b 0] fk]))
    =/  val=@t  ?:(v.ars.u.el '%.y' '%.n')
    =.  v.ars.u.el  |
    =.  visa.u.el
          %-  %~  int
                by
              visa.u.el
          (ruo iter.u.el (viso lar.u.el res.u.el ars.u.el modi.u.el))
    =:  esse.acc  (~(put by esse.acc) fk u.el)
        visa.acc  (~(uni by visa.acc) visa.u.el)
        q.form.acc  (~(put by q.form.acc) u.key val)
      ==
    $(ager.i.fk +(ager.i.fk), acc $(fk [[%b 0] fk]))
  $(ager.i.fk +(ager.i.fk), acc $(fk [[%b 0] fk]))
::
++  noto                    :: parse belt to nota
  |=  z=zona
  ^-  nota
  ?:  ?=(%txt -.z)  [-.z ~]
  ?:  ?=(%hit -.z)  [-.z ~]
  ?:  ?=(%clk -.z)  [-.z -.+.z]
  ?:  ?=(%whe -.z)  [-.z -.+.z]
  ?:  ?=(%mod -.z)  z
  ?:  ?=(%aro -.z)  z
  ?:  ?=(%bac -.z)  z
  ?:  ?=(%ret -.z)  z
  !!
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
      %checkbox
    :*  size=[[%c 2] [%c 1]]
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
  =/  [=rei =aves =ars velo=mart]
      ?+  n             [(dolo %$) ~ [%$ ~] ~]
        %$              [(dolo %text) ~ [%text ?~(a ~ v.i.a)] ~]
        %layer          [(dolo %layer) ~ [%layer ~] ~]
        %select         [(dolo %$) ~ [%select [~ ~ ~] %~] ~]
        %border-left    [(dolo %border-left) ~ [%border %left %~] ~]
        %border-right   [(dolo %border-right) ~ [%border %right %~] ~]
        %border-top     [(dolo %border-top) ~ [%border %top %~] ~]
        %border-bottom  [(dolo %border-bottom) ~ [%border %bottom %~] ~]
        %scroll         [(dolo %scroll) ~ [%scroll *iter *muri *sola] ~]
        %input          [(dolo %input) ~ [%input 0 0 ~] ~]
        %checkbox       [(dolo %checkbox) ~ [%checkbox %.n] ~]
        %radio          [(dolo %$) ~ [%radio ~] ~]
        %form           [(dolo %$) ~ [%form ~] ~]
        %submit         [(dolo %$) ~ [%select [~ ~ ~] %submit] ~]
      ==
  |-  ^-  [^rei ^aves ^ars mart]
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
    =/  v=as  (pars v.i.a)
    $(padd.rei [v v v v], a t.a)
      %px
    =/  v=as  (pars v.i.a)
    $(l.padd.rei v, r.padd.rei v, a t.a)
      %py
    =/  v=as  (pars v.i.a)
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
    =/  v=as  (pars v.i.a)
    $(marg.rei [v v v v], a t.a)
      %mx
    ?:  ?=(%border -.ars)
      $(a t.a)
    =/  v=as  (pars v.i.a)
    $(l.marg.rei v, r.marg.rei v, a t.a)
      %my
    ?:  ?=(%border -.ars)
      $(a t.a)
    =/  v=as  (pars v.i.a)
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
      %key
    $(aves (~(put by aves) %key (crip v.i.a)), a t.a)
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
  =+  [i=0 bl=0 br=0 bt=0 bb=0]
  |-  ^-  [bl=@ud br=@ud bt=@ud bb=@ud]
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
  |=  [o=esse k=rami r=rex]  :: returns just the esse and visa of the updated elements
  ^-  opus
  ?~  r  [~ ~]
  =/  par=(unit ens)  (~(get by o) k)
  ?~  par  [~ ~]
  ?.  ?=(%select -.ars.u.par)
    [~ ~]
  =/  sel=bean  =(k k.r)
  =/  v=visa
    %+  fuco  visa.u.par
    ?.  sel
      look.res.u.par
    :+  ~
      ?~(b.acia.ars.u.par b.look.res.u.par u.b.acia.ars.u.par)
    ?~(f.acia.ars.u.par f.look.res.u.par u.f.acia.ars.u.par)
  =/  e=esse  (malt ~[[k u.par(visa v)]])
  =.  k  [[%b 0] k]
  |-  ^-  opus
  =/  chi=(unit ens)  (~(get by o) k)
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
++  ruo                     :: shift a set of characters by scroll position
  |=  [it=iter vi=visa]
  ^-  visa
  ?:  &(=(0 x.it) =(0 y.it))
    vi
  %-  %~  rep
        by
      vi
  |=  [[l=loci c=[fila @c ~]] ac=visa]
  ^-  visa
  ?:  |((gth x.it x.l) (gth y.it y.l))
    ac
  (~(put by ac) [(sub x.l x.it) (sub y.l y.it)] c)
::
++  viso                    :: take an element and render it 
  |=  [=lar =res =ars lim=modi]
  ^-  visa
  ?+  -.ars    (rbox lar lim res)
    %text      (rtxt lar lim look.res w.size.res vox.ars)
    %border    (rbor lar lim res +.ars)
    %input     (rinp lar lim res ab.ars vox.ars)
    %checkbox  (rbox lar lim ?:(v.ars res(b.look f.look.res, f.look b.look.res) res))
    %layer     ~
  ==
::
++  rbox                    :: render a generic box
  |=  [=lar lim=loci =res]
  =+  [w=0 h=0 *a=visa]
  |-  ^-  visa
  =/  x=@ud  (add x.lar w)
  =/  y=@ud  (add y.lar h)
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
  =+  [w=0 h=0 *a=visa]
  |-  ^-  visa
  ?~  vox    a
  =/  x=@ud  (add x.lar w)
  =/  y=@ud  (add y.lar h)
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
    |-  ^-  tape
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
  =+  [w=0 h=0 *a=visa]
  ?:  ?=(%~ pila)  a
  |-  ^-  visa
  =/  x=@ud  (add x.lar w)
  =/  y=@ud  (add y.lar h)
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
  =/  plim=modi     [x.arx.ara y.arx.ara]
  =/  pitr=iter     [0 0]
  =/  pscr=[x=bean y=bean]  [%.n %.n]
  =/  slim=$@(~ loci)  ~
  =/  vlar=lar      lar.ara
  =/  vir=[n=@ud o=@ud i=@ud]  [0 0 0]
  |-  ^-  opus
  ?~  m  a
  =/  [=rei =aves =ars velo=mart]
    (suo g.i.m)
  =/  [bor=marl lay=marl nor=marl]
    ?:  ?=(%input -.ars)
      [~ ~ ~]
    =|  [bor=marl lay=marl nor=marl]
    |-  ^-  [marl marl marl]
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
    =/  m=@ud  (add q.l.marg.rei q.r.marg.rei)
    ?:  (gth m q.w.size.rei)  0  (sub q.w.size.rei m)
  =?  q.h.size.rei  &(hcen !=(%i p.h.size.rei))
    =/  m=@ud  (add q.t.marg.rei q.b.marg.rei)
    ?:  (gth m q.h.size.rei)  0  (sub q.h.size.rei m)
  =?  x.flex.rei  =(%i p.w.size.rei)  0
  =?  y.flex.rei  =(%i p.h.size.rei)  0
  =/  imp=bean
    ?|  =(%i p.w.size.rei)
        =(%i p.h.size.rei)
    ==
  =/  repo=bean
    ?|  &(!=(0 x.flex.rei) =(%c p.w.size.rei)) 
        &(!=(0 y.flex.rei) =(%c p.h.size.rei))
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
  =/  tvir=[n=@ud o=@ud i=@ud]  vir
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
        =/  x=@ud  (add x.vlar ?:(=(0 q.px) 0 (dec q.px)))
        =/  w=@ud  ?:(=(0 q.w.size.rei) 0 (dec q.w.size.rei))
        ?:((lth w x) (sub x w) 1)
          %top
        vlar
          %bottom
        :-  x.vlar
        =/  y=@ud  (add y.vlar ?:(=(0 q.py) 0 (dec q.py)))
        =/  h=@ud  ?:(=(0 q.h.size.rei) 0 (dec q.h.size.rei))
        ?:((lth h y) (sub y h) 1)
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
      =/  w=@ud  ;:(add bl br q.l.padd.rei q.r.padd.rei)
      ?:((gth w q.w.size.rei) 0 (sub q.w.size.rei w))
        %i
      =/  w=@ud
        ;:  add
          q.l.marg.rei  ?:(=(%row d.pow) n.vir o.vir)
          bl  br  q.l.padd.rei  q.r.padd.rei
        ==
      ?:((gth w prx) 0 (sub prx w))
    ==
  =/  ary=@ud
    ?+  p.h.size.rei  0
        %c
      =/  h=@ud  ;:(add bt bb q.t.padd.rei q.b.padd.rei)
      ?:((gth h q.h.size.rei) 0 (sub q.h.size.rei h))
        %i
      =/  h=@ud
        ;:  add 
          q.t.marg.rei  ?:(=(%row d.pow) o.vir n.vir)
          bt  bb  q.t.padd.rei  q.b.padd.rei
        ==
      ?:((gth h pry) 0 (sub pry h))
    ==
  =/  alim=loci
    :-  ?:  &(x.pscr ?=(%c p.w.size.rei))
          ;:(add x.alar bl q.l.padd.rei ?:(=(0 arx) 0 (dec arx)))
        =/  x=@ud  ;:(add x.alar bl q.l.padd.rei ?:(=(0 arx) 0 (dec arx)))
        ?:  (gth x x.plim)
          x.plim
        x
    ?:  &(y.pscr ?=(%c p.h.size.rei))
      ;:(add y.alar bt q.t.padd.rei ?:(=(0 ary) 0 (dec ary)))
    =/  y=@ud  ;:(add y.alar bt q.t.padd.rei ?:(=(0 ary) 0 (dec ary)))
    ?:  (gth y y.plim)
      y.plim
    y
  =/  nscr=[x=bean y=bean]
    :-  |(?=(%scroll -.ars) &(x.pscr ?=(%i p.w.size.rei)))
    |(?=(%scroll -.ars) &(y.pscr ?=(%i p.h.size.rei)))
  =/  nsli=$@(~ modi)
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
  =/  b=opus  [~ ~]
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
          a     b
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
          a     b
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
    =|  [rig=(unit @ud) bot=(unit @ud)]
    =.  -
      |-  ^-  [(unit @ud) (unit @ud)]
      =/  el=(unit ens)  (~(get by ?:(|(repo wrim) esse.b esse.a)) [[ax i] k])
      ?~  el
        ?:  ?=(%b ax)  $(ax %l, i 0)
        ?:  ?=(%l ax)  $(ax %~, i 0)
        [rig bot]
      ?:  ?=(%l ax)
        =/  lchi=[r=(unit @ud) b=(unit @ud)]  $(k [[ax i] k])
        =?  rig  &(?=(^ r.lchi) |(?=(~ rig) (gth u.r.lchi u.rig)))  r.lchi
        =?  bot  &(?=(^ b.lchi) |(?=(~ bot) (gth u.b.lchi u.bot)))  b.lchi
        $(i +(i))
      =/  el-r=@ud
        %+  add  x.lar.u.el
        %+  add  r.marg.res.u.el
        ?:(=(0 w.size.res.u.el) 0 (dec w.size.res.u.el))
      =/  el-b=@ud
        %+  add  y.lar.u.el
        %+  add  b.marg.res.u.el
        ?:(=(0 h.size.res.u.el) 0 (dec h.size.res.u.el))
      =?  rig  |(?=(~ rig) (gth el-r u.rig))  [~ el-r]
      =?  bot  |(?=(~ bot) (gth el-b u.bot))  [~ el-b]
      $(i +(i))
    :-  ?:  |(?=(~ rig) (gth x.vlar +(u.rig)))
          0
        (sub +(u.rig) x.vlar)
    ?:  |(?=(~ bot) (gth y.vlar +(u.bot)))
      0
    (sub +(u.bot) y.vlar)
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
    =/  bp=@ud  ;:(add bl br q.l.padd.rei q.r.padd.rei)
    ?:((gth bp q.w.size.rei) 0 (sub q.w.size.rei bp))
  =?  ary  wrim
    =/  bp=@ud  ;:(add bt bb q.t.padd.rei q.b.padd.rei)
    ?:((gth bp q.h.size.rei) 0 (sub q.h.size.rei bp))
  =?  alim  |(wrim x.pscr y.pscr)
    :-  ?:  x.pscr
          =/  r=@ud  +((add br q.r.padd.rei))
          ;:(add x.alar bl q.l.padd.rei ?:((gth r q.w.size.rei) 0 (sub q.w.size.rei r)))
        ?:  wrim
          =/  x=@ud  (add x.vlar ?:(=(0 arx) 0 (dec arx)))
          ?:  (gth x x.plim)
            x.plim
          x
        x.alim
    ?:  y.pscr
      =/  b=@ud  +((add bb q.b.padd.rei))
      ;:(add y.alar bt q.t.padd.rei ?:((gth b q.h.size.rei) 0 (sub q.h.size.rei b)))
    ?:  wrim
      =/  y=@ud  (add y.vlar ?:(=(0 ary) 0 (dec ary)))
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
    =.  esse.b
      =/  ax=axis  %b
      =/  i=@ud    0
      |-  ^-  esse
      =/  el=(unit ens)  (~(get by esse.b) [[ax i] k])
      ?~  el
        ?:  ?=(%b ax)  $(ax %l, i 0)
        ?:  ?=(%l ax)  $(ax %~, i 0)
        esse.b
      =.  esse.b  $(ax %b, i 0, k [[ax i] k])
      =:  visa.u.el    (muto movx movy ~(tap by visa.u.el))
          x.lar.u.el   (add x.lar.u.el movx)
          y.lar.u.el   (add y.lar.u.el movy)
          x.modi.u.el  (add x.modi.u.el movx)
          y.modi.u.el  (add y.modi.u.el movy)
        ==
      $(esse.b (~(put by esse.b) [[ax i] k] u.el), i +(i))
    =.  visa.b  (muto movx movy ~(tap by visa.b))
    b
    ++  muto
      |=  [movx=@ud movy=@ud v=(list [=loci fila @c ~])]
      ^-  visa
      %-  %~  dif
          by
        %-  malt
        |-  ^-  (list [=loci fila @c ~])
        ?~  v  ~
        =/  x=@ud  (add x.loci.i.v movx)
        =/  y=@ud  (add y.loci.i.v movy)
        ?:  |((gth x x.alim) (gth y y.alim) &(?=(^ slim) |((gth x x.slim) (gth y y.slim))))
          $(v t.v)
        [[[x y] +.i.v] $(v t.v)]
      visa.a
    --
  =?  a  |(repo wrim)
    :-  (~(uni by esse.b) esse.a)
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
      =/  len=@ud  (lent vox.ars)
      =/  lim=@ud  (sub arx ?:(?=(%row d.pow) n.vir o.vir))
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
      plim  =?  plim  |(x.pscr y.pscr)
              :-  ?:(x.pscr (add x.alar ?:(=(0 w.size.ares) 0 (dec w.size.ares))) x.plim)
              ?:(y.pscr (add y.alar ?:(=(0 h.size.ares) 0 (dec h.size.ares))) y.plim)
            :-  =/  x=@ud  ;:(add x.alim bl br l.padd.ares r.padd.ares)
                ?:((gth x x.plim) x.plim x)
            =/  y=@ud  ;:(add y.alim bt bb t.padd.ares b.padd.ares)
            ?:((gth y y.plim) y.plim y)
      vlar  alar
      vir   [0 0 0]
    ==
  =?  ars  |(?=(%input -.ars) ?=(%scroll -.ars))
    ?:  ?=(%input -.ars)
      =/  old=(unit ens)  (~(get by esse.ara) k)
      ?~  old  ars
      ?.  ?=(%input -.ars.u.old)  ars
      ars.u.old
    ?:  ?=(%checkbox -.ars)
      =/  old=(unit ens)  (~(get by esse.ara) k)
      ?~  old  ars
      ?.  ?=(%checkbox -.ars.u.old)  ars
      ars.u.old
    ?:  ?=(%scroll -.ars)
      =/  sol=sola
        ?~  csiz  [0 0]
        :-  ?:((gth arx w.csiz) 0 (sub w.csiz arx))
        ?:((gth ary h.csiz) 0 (sub h.csiz ary))
      =/  old=(unit ens)  (~(get by esse.ara) k)
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
      (eo esse.a visa.a ~ iter.ars k pv x.vlar y.vlar x.alim y.alim)
    =?  opu  ?=(^ rex.ara)
      =/  duc=opus  (duco esse.opu k.rex.ara rex.ara)
      [(~(uni by esse.opu) esse.duc) (~(uni by visa.opu) visa.duc)]
    opu
  =/  rend=visa
    %:  viso
      alar
      ares
      ars
      ?~  slim  plim
      [?:((lth x.slim x.plim) x.slim x.plim) ?:((lth y.slim y.plim) y.slim y.plim)]
    ==
  =?  rend  !?=(%layer -.ars)
    (~(dif by rend) visa.a)
  =?  vir  !?=(%layer -.ars)
    ?:  ?=(%border -.ars)
      [0 0 0]
    =/  el-x=@ud  (add w.size.ares (add l.marg.ares r.marg.ares))
    =/  el-y=@ud  (add h.size.ares (add t.marg.ares b.marg.ares))
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
    =/  vx=@ud  ?-(d.pow %row n.vir, %col o.vir)
    =/  vy=@ud  ?-(d.pow %row o.vir, %col n.vir)
    [(add x.plar vx) (add y.plar vy)]
  =.  a
    =/  [mox=@ud moy=@ud]
      :-  (add x.alar ?:(=(0 w.size.ares) 0 (dec w.size.ares)))
      (add y.alar ?:(=(0 h.size.ares) 0 (dec h.size.ares)))
    =?  mox  &(!x.pscr (lth x.plim mox))  x.plim
    =?  moy  &(!y.pscr (lth y.plim moy))  y.plim
    :-  (~(put by esse.a) k ^-(ens [ares rend alar [mox moy] pitr aves ars]))
    (~(uni by visa.a) rend)
  =?  a  &(?=(%select -.ars) ?=(^ rex.ara) =(k k.rex.ara))
    =/  opu=opus  (duco esse.a k rex.ara)
    a(esse (~(uni by esse.a) esse.opu), visa (~(uni by visa.a) visa.opu))
  %=  $
    m  t.m
    k  [[axis.i.k +(ager.i.k)] t.k]
  ==
::  ::  ::  ::  ::  ::  ::
++  dico                    :: derive hotkey and navigation context from display state
  |=  e=esse
  ^-  cura
  ?:  |(=(0 x.arx.ara) =(0 y.arx.ara))
    [~ ~ ~ ~ rex.ara]
  =/  k=rami                      [[%~ 0] ~]
  =/  rk=$@(~ rami)               ?~(rex.ara ~ k.rex.ara)
  =/  plim=modi                   [x.arx.ara y.arx.ara]
  =/  acc=[rend=bean =visa cura]  [%.n ~ ~ ~ ~ ~ ~]
  =;  dic=[bean visa cura]
    =.  omen.dic
      ?:  ?=(^ rex.dic)
        =/  el=(unit ens)  (~(get by e) k.rex.dic)
        ?:  &(?=(^ el) ?=(%input -.ars.u.el))
          hinp
        hnav
      ?:  ?=(^ ordo.dic)
        hnav
      ~
    +.+.dic
  |-  ^-  [bean visa cura]
  =/  el=(unit ens)  (~(get by e) k)
  ?~  el
    ?:  ?=(%b axis.i.k)  $(k [[%l 0] t.k])
    ?:  ?=(%l axis.i.k)  $(k [[%~ 0] t.k])
      acc
  =/  nacc=[rend=bean =visa cura]
    %=  $
      k     [[%b 0] k]
      acc   [%.n ~ ~ ~ ~ ~ rex.acc]
      plim  :-  ?:((gth x.modi.u.el x.plim) x.plim x.modi.u.el)
            ?:((gth y.modi.u.el y.plim) y.plim y.modi.u.el)
    ==
  =.  rend.nacc  |(?=(^ visa.u.el) rend.nacc)
  ?.  rend.nacc
    $(ager.i.k +(ager.i.k), acc acc(rend |(rend.acc rend.nacc)))
  =/  sel=(unit @t)  (~(get by aves.u.el) %sel)
  =/  nav=bean
    ?|  &(?=(^ sel) !?=(%layer -.ars.u.el))
        ?=(%select -.ars.u.el)  ?=(%scroll -.ars.u.el)
        ?=(%input -.ars.u.el)  ?=(%checkbox -.ars.u.el)
    ==
  =.  ordo.acc  
    ?.  nav
      (weld ordo.nacc ordo.acc)
    %+  weld
      ^-  ordo
      :_  ordo.nacc
      :*  ?:((lth x.iter.u.el x.lar.u.el) (sub x.lar.u.el x.iter.u.el) 1)
          =/  r=@ud  (add x.lar.u.el ?:(=(0 w.size.res.u.el) 0 (dec w.size.res.u.el)))
          ?:((lth x.iter.u.el r) (sub r x.iter.u.el) 1)
          ?:((lth y.iter.u.el y.lar.u.el) (sub y.lar.u.el y.iter.u.el) 1)
          =/  b=@ud  (add y.lar.u.el ?:(=(0 h.size.res.u.el) 0 (dec h.size.res.u.el)))
          ?:((lth y.iter.u.el b) (sub b y.iter.u.el) 1)
          k
      ==
    ordo.acc
  =?  rex.acc  ?=(~ rex.acc) 
    ?^  rex.nacc  rex.nacc
    ?:  &(=(rk k) ?=(^ ordo.acc))
      i.ordo.acc
    rex.acc
  =.  visa.nacc  (~(uni by visa.u.el) visa.nacc)
  =.  mus.acc
    ?.  nav
      (~(uni by mus.acc) mus.nacc)
    %-  %~  uni
          by 
      ^-  mus
      %-  malt
      ^-  (list [loci rami])
      %+  turn
        ^-((list [loci [fila @c ~]]) ~(tap by visa.nacc))
      |=([loc=loci [fila @c ~]] [loc k])
    (~(uni by mus.acc) mus.nacc)
  =.  equi.acc 
    ?.  ?=(%scroll -.ars.u.el)
      (~(uni by equi.acc) equi.nacc)
    (~(put in (~(uni by equi.acc) equi.nacc)) k)
  %=  $
    ager.i.k  +(ager.i.k)
    rend.acc  |(rend.acc rend.nacc)
    visa.acc  (~(uni by visa.nacc) visa.acc)
  ==
::  ::  ::  ::  ::  ::  ::  ::
++  supo                    :: make a full display update
  |=  v=visa
  ^-  lux
  =/  y=@ud  1
  :-  %mor
  |-  ^-  (list lux)
  =/  s=stub
    =/  x=@ud  1
    |-  ^-  stub
    ?:  (gte x x.urbs)
      ~
    =/  val=(unit [fila @c ~])  (~(get by v) [x y])
    =/  char=(pair stye (list @c))
      ?~(val [[~ ~ ~] ~[(@c 'x')]] u.val)
    [char $(x +(x))]
  :+  [%hop 1 y]
    [%klr s] 
  ?:(=(y y.urbs) ~ $(y +(y)))
::
++  dono                    :: make a display update diff
  |=  [old=visa new=visa]
  ^-  lux
  =/  n=(list [=loci [fila @c ~]])  ~(tap by new)
  :-  %mor
  |-  ^-  (list lux)
  ?~  n  ~
  =/  v=(unit [fila @c ~])  (~(get by old) loci.i.n)
  ?.  |(&(?=(^ v) !=(u.v +.i.n)) ?=(~ v))
    $(n t.n)
  [[%hop loci.i.n] [%klr ~[+.i.n]] $(n t.n)]
::
++  volo                    :: turn a display update blit into ansi sequences
  |=  =lux
  ^-  tape
  ?:  ?=(%hop -.lux)
    ?@  p.lux  ~
    ['\\' 'e' '[' (scot %ud y.p.lux) ';' (scot %ud x.p.lux) 'H' ~]
  ?.  ?=(%mor -.lux)  ~
  =/  prev=stye  [~ ~ ~]
  |-  ^-  tape
  ?~  p.lux
    ?.  =([~ ~ ~] prev)
      :^  '\\'  'e'  '['
      :+  '0'  'm'  ~
    ~
  ?:  ?=(%mor -.i.p.lux)
    $(p.lux ^-((list ^lux) (weld p.i.p.lux t.p.lux)))
  ?:  ?=(%hop -.i.p.lux)
    ?@  p.i.p.lux  $(p.lux t.p.lux)
    :*  '\\'  'e'  '['
        (scot %ud y.p.i.p.lux)  ';'
        (scot %ud x.p.i.p.lux)  'H'
        $(p.lux t.p.lux)
    ==
  ?:  ?=(%klr -.i.p.lux)
    |-  ^-  tape
    ?~  p.i.p.lux  ^$(p.lux t.p.lux)
    =/  newb=(unit tint)  ?:(=(p.q.prev p.q.p.i.p.i.p.lux) ~ [~ p.q.p.i.p.i.p.lux])
    =/  newf=(unit tint)  ?:(=(q.q.prev q.q.p.i.p.i.p.lux) ~ [~ q.q.p.i.p.i.p.lux])
    =/  oldd=(list deco)  ~(tap in (~(dif in p.prev) p.p.i.p.i.p.lux))
    =/  newd=(list deco)  ~(tap in (~(dif in p.p.i.p.i.p.lux) p.prev))
    =/  newt=tape  (tufa q.i.p.i.p.lux)
    |-  ^-  tape
    ?^  newb
      :^  '\\'  'e'  '['
      ?@  u.newb
        :^    '4'
            ?-  u.newb
              %r  '1'  %g  '2'  %b  '4'
              %c  '6'  %m  '5'  %y  '3'
              %k  '0'  %w  '7'  %~  '9'
            ==
          'm'
        $(newb ~)
      :^  '4'  '8'  ';'
      :+  '2'  ';'
      :+  (scot %ud (@ r.u.newb))  ';'
      :+  (scot %ud (@ g.u.newb))  ';'
      :+  (scot %ud (@ b.u.newb))  'm'
      $(newb ~)
    ?^  newf
      :^  '\\'  'e'  '['
      ?@  u.newf
        :^    '3'
            ?-  u.newf
              %r  '1'  %g  '2'  %b  '4'
              %c  '6'  %m  '5'  %y  '3'
              %k  '0'  %w  '7'  %~  '9'
            ==
          'm'
        $(newf ~)
      :^  '3'  '8'  ';'
      :+  '2'  ';'
      :+  (scot %ud (@ r.u.newf))  ';'
      :+  (scot %ud (@ g.u.newf))  ';'
      :+  (scot %ud (@ b.u.newf))  'm'
      $(newf ~)
    ?^  oldd
      ?:  ?=(~ i.oldd)  $(oldd t.oldd)
      :^  '\\'  'e'  '['
      :+  ?-(i.oldd %bl '25', %br '22', %un '24')
        'm'
      $(oldd t.oldd)
    ?^  newd
      ?:  ?=(~ i.newd)  $(newd t.newd)
      :^  '\\'  'e'  '['
      :+  ?-(i.newd %bl '5', %br '1', %un '4')
        'm'
      $(newd t.newd)
    ?^  newt
      :-  i.newt
      $(newt t.newt)
    ^$(p.i.p.lux t.p.i.p.lux, prev p.i.p.i.p.lux)
  $(p.lux t.p.lux)
--
