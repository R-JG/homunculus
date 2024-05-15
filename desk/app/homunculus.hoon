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
      flow=fuga
      look=fila
  ==
+$  loci  [x=@ud y=@ud]
+$  axis  ?(%b %l %~)
+$  ager  @ud
+$  lar   $~([1 1] loci)
+$  modi  loci
+$  iter  [x=@ud y=@ud]
+$  fuga  [d=?(%col %row) b=?(%wrap %clip)]
+$  aves  (map ?(%sel %act %key) @t)
+$  ad    ?(%l %r %t %b)
+$  via   ?(%h %v)
+$  ora   ?(%light %heavy %double %arc %blank %~)
+$  muri  [l=@ud r=@ud t=@ud b=@ud]
+$  sola  [x=@ud y=@ud]
+$  ars
  $%  [%text =vox]  [%layer ~]  [%scroll =iter =muri =sola]
      [%border =ad =ora]  [%line =via =ora]
      [%select =acia pro=?(%submit %~)]
      [%input ab=@ud i=loci =vox]  [%checkbox v=bean]
      [%radio ~]  [%form ~]  [%$ ~]
  ==
+$  visa  (map loci nodi)
+$  nodi  (pair fila @c)
+$  lina  (list @c)
+$  vox   (list lina)
+$  fila  [d=(set deco) b=tint f=tint]
+$  acia  [d=(unit (set deco)) b=(unit tint) f=(unit tint)]
+$  lux
  $~  [%klr ~]
  $%  [%hop p=$@(@ud [x=@ud y=@ud])]
      [%klr p=(list nodi)]
      [%mor p=(list lux)]
  ==
+$  zona
  $~  [%txt ~]
  $%  [%clk p=?(%d %u) x=@ud y=@ud]
      [%whe p=?(%d %u) x=@ud y=@ud]
      [%mod mod=?(%ctl %met %hyp) key=$~([%txt ~] zona)]
      [%aro p=?(%d %l %r %u)]
      [%txt p=lina]
      [%bac ~]
      [%del ~]
      [%ret ~]
  ==
+$  nota
  $%  [%clk ?(%d %u)]  [%whe ?(%d %u)]
      [%mod ?(%ctl %met %hyp) @tas]  [%aro ?(%d %l %r %u)]
      [%txt ~]  [%bac ~]  [%del ~]  [%ret ~]
  ==
+$  lex
  $?  %nav-l  %nav-r  %nav-u  %nav-d
      %cur-l  %cur-r  %cur-u  %cur-d
      %scr-l  %scr-r  %scr-u  %scr-d
      %inp  %del  %tog  %act  %clk  %def
  ==
+$  omen  (map nota lex)
+$  mus   (map loci rami)
+$  equi  (set rami)
+$  dux   [l=@ud r=@ud t=@ud b=@ud k=rami]
+$  rex   $@(~ dux)
+$  ordo  (list dux)
+$  gens  (map @t rami)
+$  os
  $%  [%h p=?(%border %line) x1=@ud x2=@ud y=@ud =ora =visa]
      [%v p=?(%border %line) x=@ud y1=@ud y2=@ud =ora =visa]
  ==
+$  ossa  (set os)
+$  crux  [c=(pair via ora) l=ora r=ora t=ora b=ora]
+$  viae  (map loci (pair fila crux))
+$  aqua  (list [i=@ud size=@ud marg=@ud])
+$  opus  [=esse =visa]
+$  cura  [=visa =omen =gens =ordo =rex =equi =mus]
+$  as    $%((pair %c @ud) (pair %p @ud) (pair %i @ud))
+$  ovum
  $:  size=[w=as h=as]
      padd=[l=as r=as t=as b=as]
      marg=[l=as r=as t=as b=as]
      flex=[x=@ud y=@ud]
      flow=fuga
      look=acia
  ==
+$  cor   @
+$  fons  [@p @tas]
+$  aula  (list (set fons))
+$  vela  manx
+$  ara
  $:  =vela
      =esse  =visa
      =omen  =gens
      =ordo  =rex
      =equi  =mus
  ==
+$  arae  (map fons ara)
+$  urbs  $~([50 25] [x=@ud y=@ud])
+$  ego
  $:  =cor  =fons  =urbs
      =aula  =arae
  ==
::
+$  data
  $%  [%sel p=@t]
      [%act p=@t]
      [%form form]
  ==
+$  form  (pair @t (map @t @t))
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
  [~ hoc(ego =+(*^ego -(urbs siz)))]
::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  
++  on-poke
  |=  [=mark =vase]
  ^-  (quip card _hoc)
  ?+  mark  ~|("homunculus poke failed with mark: {<mark>}" !!)
    ::
      %elem
    =/  elem  !<(manx vase)
    =/  fon=^fons
      [src.bowl ?:(&(?=(^ sap.bowl) ?=(^ t.sap.bowl)) i.t.sap.bowl %$)]
    =/  aru=(unit ara)  (~(get by arae) fon)
    ?^  aru
      =/  gen=ara  (geno elem u.aru)
      :_  hoc(arae (~(put by arae) fon gen))
      ?.  =/(aul=(set ^fons) (snag cor aula) (~(has in aul) fon))
        ~
      :_  ~
      :*  %give  %fact  ~[/homunculus-http]  %json
          !>  ^-  json  :-  %s  %-  crip  %-  volo
          ^-(lux [%mor ~[(dono visa.u.aru visa.gen) (fero rex.gen equi.gen esse.gen)]])
      ==
    =/  gen=ara  (geno elem *ara)
    =:  aula  (into aula cor (silt ~[fon]))
        fons  fon
      ==
    :_  hoc(arae (~(put by arae) fon gen))
    :_  ~
    :*  %give  %fact  ~[/homunculus-http]  %json
        !>  ^-  json  :-  %s  %-  crip  %-  volo
        ^-(lux [%mor ~[(supo visa.gen) (fero rex.gen equi.gen esse.gen)]])
    ==
    ::
      %json
    =/  jsn  !<(json vase)
    ?:  ?=(%a -.jsn)
      ?.  &(?=(^ p.jsn) ?=(%n -.i.p.jsn) ?=(^ t.p.jsn) ?=(%n -.i.t.p.jsn))
        [~ hoc]
      =.  urbs  [(slav %ud p.i.p.jsn) (slav %ud p.i.t.p.jsn)]
      =^  vis=visa  arae
        =/  aul=(set ^fons)  (snag cor aula)
        =/  kes=(list ^fons)  ~(tap in ~(key by arae))
        =|  vis=visa
        |-  ^-  [visa ^arae]
        ?~  kes  [vis arae]
        =/  aru=(unit ara)  (~(get by arae) i.kes)
        ?~  aru  $(kes t.kes)
        =/  gen=ara  (geno vela.u.aru u.aru)
        %=  $
          kes   t.kes
          arae  (~(put by arae) i.kes gen)
          vis   ?:((~(has in aul) i.kes) (~(uni by vis) visa.gen) vis)
        ==
      =/  aru=(unit ara)  (~(get by arae) fons)
      =/  bac=visa  (rbox [1 1] urbs =+(*res -(size urbs, look [~ ~ ~])))
      =/  lus=lux  (supo ^-(visa (~(uni by bac) vis)))
      :_  hoc
      :_  ~
      :*  %give  %fact  ~[/homunculus-http]  %json
          !>  ^-  json  :-  %s  %-  crip  %-  volo
          ^-(lux [%mor ?~(aru ~[lus] ~[lus (fero rex.u.aru equi.u.aru esse.u.aru)])])
      ==
    ?.  ?=(%s -.jsn)  [~ hoc]
    =/  inp=tape  (trip p.jsn)
    ?:  =("\\" inp)  [~ hoc]
    =;  z=(unit zona)
      ?~  z  [~ hoc]
      =/  arf=$@(~ (pair ara ^fons))
        ?.  |(?=(%clk -.u.z) ?=(%whe -.u.z))
          =/  aru=(unit ara)  (~(get by arae) fons)
          ?~  aru  ~
          [u.aru fons]
        =/  aul=(list ^fons)  ~(tap in ^-((set ^fons) (snag cor aula)))
        |-  ^-  $@(~ (pair ara ^fons))
        ?~  aul  ~
        =/  aru=(unit ara)  (~(get by arae) i.aul)
        ?~  aru  $(aul t.aul)
        =/  roo=(unit ens)  (~(get by esse.u.aru) [[%~ 0] ~])
        ?~  roo  $(aul t.aul)
        =/  [x=@ud y=@ud]
          ?:(?=(%clk -.u.z) [x.u.z y.u.z] ?:(?=(%whe -.u.z) [x.u.z y.u.z] [0 0]))
        ?:  ?&  (gte x x.lar.u.roo)
                (lte x (add x.lar.u.roo ?:(=(0 w.size.res.u.roo) 0 (dec w.size.res.u.roo))))
                (lte y (add y.lar.u.roo ?:(=(0 h.size.res.u.roo) 0 (dec h.size.res.u.roo))))
                (gte y y.lar.u.roo)
            ==
          [u.aru i.aul]
        $(aul t.aul)
      ?~  arf  [~ hoc]
      =/  l=(unit lex)  (~(get by omen.p.arf) (noto u.z))
      ?~  l  [~ hoc]
      =^  crds  p.arf  (muto u.l u.z q.arf p.arf)
      [crds hoc(arae (~(put by arae) q.arf p.arf), fons q.arf)]
    ?~  inp  ~
    ?.  =('\\' i.inp)     [~ [%txt (tuba inp)]]
    ?~  t.inp             [~ [%txt ~[~-~5c.]]]
    ?~  t.t.inp
      ?:  =('t' i.t.inp)  [~ [%mod %ctl [%txt ~[~-i]]]]
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
  ^-  (quip card _hoc)
  ?+  path  [~ hoc]
    ::
      [%homunculus-http ~]
    =/  aul=(list ^fons)  ~(tap in ^-((set ^fons) (snag cor aula)))
    =/  bac=visa  (rbox [1 1] urbs =+(*res -(size urbs, look [~ ~ ~])))
    =/  vis
      |-  ^-  visa
      ?~  aul  bac
      =/  aru=(unit ara)  (~(get by arae) i.aul)
      ?~  aru  $(aul t.aul)
      (~(uni by $(aul t.aul)) visa.u.aru)
    =/  aru=(unit ara)  (~(get by arae) fons)
    :_  hoc
    :_  ~
    :*  %give  %fact  ~[/homunculus-http]  %json
        !>  ^-  json  :-  %s  %-  crip  
        ^-  tape  :-  '\\e[1;1H\\e[3J\\e[0J'
        %-  volo  ^-  lux  :-  %mor
        ~[(supo vis) ?^(aru (fero rex.u.aru equi.u.aru esse.u.aru) (fero ~ ~ ~))]
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
++  muto                    :: handle an event from the hotkey context
  |=  [=lex zon=zona fon=^fons =ara]
  |^  ^-  (quip card ^ara)  novo
  ::
  ++  novo                  :: process an event update
    ^-  (quip card ^ara)
    ?:  |(?=(%nav-l lex) ?=(%nav-r lex) ?=(%nav-u lex) ?=(%nav-d lex))
      =/  scr=$@(~ rami)  ?~(rex.ara ~ (ligo k.rex.ara equi.ara))
      =/  spar=(unit ens)  ?~(scr ~ (~(get by esse.ara) scr))
      =/  navs=ordo  (gero rex.ara ordo.ara)
      =/  snav=ordo
        ?~  scr  ~
        %+  skim  ^-(ordo navs)
        |=  =dux
        (alo scr k.dux)
      =/  next=rex  ?~(navs ~ ?~(snav i.navs i.snav))
      =/  send=bean
        ?&  ?=(^ rex.ara)  ?=(^ spar)  ?=(%scroll -.ars.u.spar)  =(scr k.rex.ara)
            ?|  &(?=(%nav-l lex) =(0 x.iter.ars.u.spar))
                &(?=(%nav-r lex) =(x.sola.ars.u.spar x.iter.ars.u.spar))
                &(?=(%nav-u lex) =(0 y.iter.ars.u.spar))
                &(?=(%nav-d lex) =(y.sola.ars.u.spar y.iter.ars.u.spar))
        ==  ==
      =?  scr   &(send ?=(^ scr) ?=(^ t.scr))  
        =/  pscr=$@(~ rami)  (ligo t.scr equi.ara)
        ?~(pscr scr pscr)
      =?  spar  &(send ?=(^ scr))  (~(get by esse.ara) scr)
      =?  next  &(send ?=(~ scr) ?=(^ navs))  i.navs
      =?  snav  &(send ?=(^ scr))
        %+  skim  ^-(ordo navs)
        |=  =dux
        (alo scr k.dux)
      =?  next  &(send ?=(^ scr) ?=(^ snav))  i.snav
      =/  upd=$@(~ [=esse cura])
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
        %+  skim  ^-(ordo (gero rex.ara ordo.upd))
        |=  =dux
        (alo scr k.dux)
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
              (vado ab.ars.u.sel i.ars.u.sel size.res.u.sel lar.u.sel iter.u.sel)
            ?~  rex.ara  [%hop [1 1]]
            ?:  |(?=(~ spar) &(?=(~ upd) ?=(~ snav) ?=(^ navs)) =(k.rex.ara scr))
              [%hop [l.rex.ara t.rex.ara]]
            (cedo rex.ara scr u.spar)
        ==
      =/  avis=(unit @t)  ?:(|(?=(~ next) ?=(~ sel)) ~ (~(get by aves.u.sel) %sel))
      :_  ara(visa ?~(upd (~(uni by visa.ara) ppv) visa.upd))
      :-  [%give %fact ~[/homunculus-http] %json !>(^-(json [%s (crip (volo lux))]))]
      ?~  avis  ~
      [[%pass /sel %agent fon %poke %homunculus !>(^-(data [%sel u.avis]))] ~]
    ::
    ?:  ?=(%act lex)
      ?~  rex.ara  [~ ara]
      =/  el=(unit ens)  (~(get by esse.ara) k.rex.ara)
      ?~  el  [~ ara]
      ?:  ?=(%input -.ars.u.el)  [~ ara]
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
        ?~  avis  [~ ara]
        :_  ara
        :~  [%pass /act %agent fon %poke %homunculus !>(^-(data [%act u.avis]))]
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
            (fero rex.ara equi.ara esse.ara)
        ==
      :_  ara(visa (~(uni by visa.ara) ?^(fupd visa.fupd ?^(cupd visa.cupd ~))))
      :-  [%give %fact ~[/homunculus-http] %json !>(^-(json [%s (crip (volo lux))]))]
      ?:  &(?=(^ fupd) ?=(^ avis))
        :-  [%pass /act %agent fon %poke %homunculus !>(^-(data [%form form.fupd]))]
        [[%pass /act %agent fon %poke %homunculus !>(^-(data [%act u.avis]))] ~]
      ?^  fupd
        [[%pass /act %agent fon %poke %homunculus !>(^-(data [%form form.fupd]))] ~]
      ?^  avis
        [[%pass /act %agent fon %poke %homunculus !>(^-(data [%act u.avis]))] ~]
      ~
    ::
    ?:  ?=(%clk lex)
      ?.  ?=(%clk -.zon)  [~ ara]
      =/  mk=(unit rami)  (~(get by mus.ara) [x.zon y.zon])
      ?~  mk  [~ ara]
      ?:  ?=(%u p.zon)
        ?.  &(?=(^ rex.ara) =(u.mk k.rex.ara))
          [~ ara]
        %_(novo lex %act)
      =/  el=(unit ens)  (~(get by esse.ara) u.mk)
      ?~  el  [~ ara]
      ?:  &(?=(^ rex.ara) =(u.mk k.rex.ara) !?=(%input -.ars.u.el))
        [~ ara]
      =/  nrex=rex  (rogo u.mk ordo.ara)
      =/  prae=opus  ?~(rex.ara [~ ~] (duco esse.ara k.rex.ara nrex))
      =.  rex.ara  nrex
      =/  post=opus  ?~(nrex [~ ~] (duco esse.ara k.nrex rex.ara))
      =?  esse.ara  |(?=(^ esse.prae) ?=(^ esse.post))
        (~(uni by esse.ara) (~(uni by esse.prae) esse.post))
      =?  u.el  |(?=(^ esse.prae) ?=(^ esse.post))
        ?~(nel=(~(get by esse.ara) u.mk) u.el u.nel)
      =?  esse.ara  &(?=(%input -.ars.u.el) ?=(^ vox.ars.u.el))
        =/  [x=@ud y=@ud]
          =:  x.zon  (add x.zon x.iter.u.el)
              y.zon  (add y.zon y.iter.u.el)
            ==
          :-  ?:((lte x.lar.u.el x.zon) (sub x.zon x.lar.u.el) 0)
          ?:((lte y.lar.u.el y.zon) (sub y.zon y.lar.u.el) 0)
        ?:  =(1 h.size.res.u.el)
          ?.  =(0 y)  esse.ara
          =.  x  (add x ab.ars.u.el)
          =/  l=@ud  (lent i.vox.ars.u.el)
          (~(put by esse.ara) u.mk u.el(i.ars [?:((lth x l) x l) 0]))
        =.  y  (add y ab.ars.u.el)
        =/  l=@ud  (lent vox.ars.u.el)
        =/  rlen=@ud
          ?:  (gth +(y) l)
            (pono ^-(lina (rear ^-(vox vox.ars.u.el))))
          (pono ^-(lina (snag y ^-(vox vox.ars.u.el))))
        %+  %~  put
              by
            esse.ara
          u.mk
        u.el(i.ars [?:((lth x rlen) x rlen) ?:((gth +(y) l) (dec l) y)])
      =.  omen.ara
        ?+  -.ars.u.el   (~(uni by omen.ara) hnav)
          %input         (~(uni by omen.ara) hinp)
        ==
      =/  nvis=visa
        ?:(|(?=(^ visa.prae) ?=(^ visa.post)) (~(uni by visa.prae) visa.post) ~)
      =/  =lux
        :-  %mor
        :~  (dono visa.ara nvis)
            (fero rex.ara equi.ara esse.ara)
        ==
      =/  avis=(unit @t)  (~(get by aves.u.el) %sel)
      :_  ara(visa ?~(nvis visa.ara (~(uni by visa.ara) ^-(visa nvis))))
      :-  [%give %fact ~[/homunculus-http] %json !>(^-(json [%s (crip (volo lux))]))]
      ?~  avis  ~
      [[%pass /sel %agent fon %poke %homunculus !>(^-(data [%sel u.avis]))] ~]
    ::
    ?:  ?=(%inp lex)
      ?.  ?=(%txt -.zon)  [~ ara]
      ?~  p.zon  [~ ara]
      ?~  rex.ara  [~ ara]
      =/  el=(unit ens)  (~(get by esse.ara) k.rex.ara)
      ?~  el  [~ ara]
      ?.  ?=(%input -.ars.u.el)  [~ ara]
      =.  ars.u.el
        ?:  =(1 h.size.res.u.el)
          =.  vox.ars.u.el
            :_  ~
            ?~  vox.ars.u.el  ?~(t.p.zon ~[i.p.zon] p.zon)
            ?~  t.p.zon  (into i.vox.ars.u.el x.i.ars.u.el i.p.zon)
            %+  weld  (weld (scag +(x.i.ars.u.el) i.vox.ars.u.el) p.zon)
            (slag +(x.i.ars.u.el) i.vox.ars.u.el)
          =.  i.ars.u.el
            ?~  vox.ars.u.el  i.ars.u.el
            =/  x=@ud  +(x.i.ars.u.el)
            ?:  (gth x (lent i.vox.ars.u.el))
              i.ars.u.el
            [x y.i.ars.u.el]
          %_  ars.u.el
            ab  ?:  (lth (sub x.i.ars.u.el ab.ars.u.el) w.size.res.u.el)
                  ab.ars.u.el
                +(ab.ars.u.el)
          ==
        =/  row=lina  ?~(vox.ars.u.el ~ (snag y.i.ars.u.el ^-(vox vox.ars.u.el)))
        =/  wup=bean
          ?.  &(=(~-. i.p.zon) !=(0 y.i.ars.u.el) (lth x.i.ars.u.el (sumo row)))
            |
          %+  gte  w.size.res.u.el
          %+  add  x.i.ars.u.el
          %-  lent
          ?~  vox.ars.u.el  ~
          (snag (dec y.i.ars.u.el) ^-(vox vox.ars.u.el))
        =.  row
          ?~  t.p.zon  (into row x.i.ars.u.el i.p.zon)
          %+  weld  (weld (scag +(x.i.ars.u.el) row) p.zon)
          (slag +(x.i.ars.u.el) row)
        ?:  wup
          =.  vox.ars.u.el
            %+  weld
              (scag (dec y.i.ars.u.el) vox.ars.u.el)
            %:  oro
              w.size.res.u.el  h.size.res.u.el
              ^-  lina
              %-  zing
              :+  ^-(lina (snag (dec y.i.ars.u.el) ^-(vox vox.ars.u.el)))
                row
              (slag +(y.i.ars.u.el) vox.ars.u.el)
            ==
          ars.u.el(i [?~(t.p.zon 0 (pono (flop p.zon))) y.i.ars.u.el])
        =/  len=@ud  (pono row)
        ?:  (lte len w.size.res.u.el)
          %_  ars.u.el
            vox  (snap vox.ars.u.el y.i.ars.u.el row)
            x.i  +(x.i.ars.u.el)
          ==
        =.  vox.ars.u.el
          %+  weld
            (scag y.i.ars.u.el vox.ars.u.el)
          %:  oro
            w.size.res.u.el  h.size.res.u.el
            ^-(lina (zing [row (slag +(y.i.ars.u.el) vox.ars.u.el)]))
          ==
        =.  i.ars.u.el
          =/  nlen=(unit @ud)
            ?~  vox.ars.u.el  ~
            [~ (lent ^-(lina (snag y.i.ars.u.el ^-(vox vox.ars.u.el))))]
          =/  npos=(unit @ud)
            ?:(|(?=(~ nlen) (lte +(x.i.ars.u.el) u.nlen)) ~ [~ (sub +(x.i.ars.u.el) u.nlen)])
          ?~  npos
            [+(x.i.ars.u.el) y.i.ars.u.el]
          [u.npos +(y.i.ars.u.el)]
        =.  ab.ars.u.el
          ?:  (lth (sub y.i.ars.u.el ab.ars.u.el) h.size.res.u.el)
            ab.ars.u.el
          +(ab.ars.u.el)
        ars.u.el
      =/  vi=visa  (rinp lar.u.el modi.u.el res.u.el ab.ars.u.el vox.ars.u.el)
      =.  vi  (~(int by visa.u.el) (ruo iter.u.el vi))
      =.  visa.u.el  vi
      =/  =lux
        :-  %mor
        :~  (dono visa.ara vi)
            (vado ab.ars.u.el i.ars.u.el size.res.u.el lar.u.el iter.u.el)
        ==
      :_  ara(esse (~(put by esse.ara) k.rex.ara u.el), visa (~(uni by visa.ara) vi))
      :~  [%give %fact ~[/homunculus-http] %json !>(^-(json [%s (crip (volo lux))]))]
      ==
    ::
    ?:  ?=(%del lex)
      ?~  rex.ara  [~ ara]
      =/  el=(unit ens)  (~(get by esse.ara) k.rex.ara)
      ?~  el  [~ ara]
      ?.  ?=(%input -.ars.u.el)  [~ ara]
      =.  ars.u.el
        ?:  =(1 h.size.res.u.el)
          ?~  vox.ars.u.el  ars.u.el
          ?:  =(0 x.i.ars.u.el)  ars.u.el
          =.  x.i.ars.u.el  (dec x.i.ars.u.el)
          =.  i.vox.ars.u.el  (oust [x.i.ars.u.el 1] i.vox.ars.u.el)
          =.  ab.ars.u.el
            ?:  &((lte x.i.ars.u.el ab.ars.u.el) !=(0 ab.ars.u.el))
              ?:((gth w.size.res.u.el ab.ars.u.el) 0 +((sub ab.ars.u.el w.size.res.u.el)))
            ab.ars.u.el
          ars.u.el
        ?:  =([0 0] i.ars.u.el)  ars.u.el
        =/  arow=lina
          ?:  |(=(0 y.i.ars.u.el) ?=(~ vox.ars.u.el))  ~
          (snag (dec y.i.ars.u.el) ^-(vox vox.ars.u.el))
        =?  arow  =(0 x.i.ars.u.el)
          (snip arow)
        =/  alen=@ud  (lent arow)
        =/  row=lina  ?~(vox.ars.u.el ~ (snag y.i.ars.u.el ^-(vox vox.ars.u.el)))
        =?  row  !=(0 x.i.ars.u.el)
          (oust [(dec x.i.ars.u.el) 1] row)
        ?:  ?&  ?=(^ arow)
                ?|  ?&  (lth alen w.size.res.u.el)
                        %+  lte  (sumo row)
                        ?:((lte alen w.size.res.u.el) (sub w.size.res.u.el alen) 0)
                    ==
                    &(|(=(0 x.i.ars.u.el) =(1 x.i.ars.u.el)) !=(~-. (rear arow)))
            ==  ==
          =.  vox.ars.u.el
            %+  weld
              ?:  =(0 y.i.ars.u.el)  ~
              ^-(vox (scag (dec y.i.ars.u.el) ^-(vox vox.ars.u.el)))
            %:  oro
              w.size.res.u.el  h.size.res.u.el
              ^-  lina
              %-  zing
              [arow row ^-(vox (slag +(y.i.ars.u.el) ^-(vox vox.ars.u.el)))]
            ==
          =.  i.ars.u.el
            =/  nlen=@ud
              ?:  |(=(0 y.i.ars.u.el) ?=(~ vox.ars.u.el))  0
              (lent (snag (dec y.i.ars.u.el) ^-(vox vox.ars.u.el)))
            =/  lend=@ud  ?:((lte nlen alen) (sub alen nlen) 0)
            ?.  =(0 lend)
              [lend y.i.ars.u.el]
            :_  ?:(=(0 y.i.ars.u.el) 0 (dec y.i.ars.u.el))
            ?:  =(0 x.i.ars.u.el)  alen
            (add ?:(=(0 alen) 0 (dec alen)) x.i.ars.u.el)
          ars.u.el(ab ?:((lth y.i.ars.u.el ab.ars.u.el) (dec ab.ars.u.el) ab.ars.u.el))
        =/  len=@ud  (lent row)
        =/  brow=lina
          ?:  |(?=(~ vox.ars.u.el) (lth (lent vox.ars.u.el) +(+(y.i.ars.u.el))))
            ~
          (snag +(y.i.ars.u.el) ^-(vox vox.ars.u.el))
        =/  bwor=@ud  (sumo brow)
        ?.  ?|  ?&  (lth len w.size.res.u.el)
                    (lte bwor (sub w.size.res.u.el len))
                ==
                &(?=(^ row) =(~-. (rear row)))
            ==
          %_  ars.u.el
            vox  (snap vox.ars.u.el y.i.ars.u.el row)
            x.i  ?:(=(0 x.i.ars.u.el) 0 (dec x.i.ars.u.el))
          ==
        =.  vox.ars.u.el
          %+  weld
            (scag y.i.ars.u.el vox.ars.u.el)
          %:  oro
            w.size.res.u.el  h.size.res.u.el
            ^-(lina (zing [row brow (slag +(+(y.i.ars.u.el)) vox.ars.u.el)]))
          ==
        =.  i.ars.u.el
          ?:  &(=(0 y.i.ars.u.el) !=(0 x.i.ars.u.el))
            [(dec x.i.ars.u.el) 0]
          ?.  |(=(0 x.i.ars.u.el) =(1 x.i.ars.u.el))
            [(dec x.i.ars.u.el) y.i.ars.u.el]
          =.  y.i.ars.u.el  (dec y.i.ars.u.el)
          =/  l=@ud
            (lent ^-(lina (snag y.i.ars.u.el ^-(vox vox.ars.u.el))))
          [?:(=(0 l) 0 (dec l)) y.i.ars.u.el]
        ars.u.el(ab ?:((lth y.i.ars.u.el ab.ars.u.el) (dec ab.ars.u.el) ab.ars.u.el))
      =/  vi=visa  (rinp lar.u.el modi.u.el res.u.el ab.ars.u.el vox.ars.u.el)
      =.  vi  (~(int by visa.u.el) (ruo iter.u.el vi))
      =.  visa.u.el  vi
      =/  =lux
        :-  %mor
        :~  (dono visa.ara vi)
            (vado ab.ars.u.el i.ars.u.el size.res.u.el lar.u.el iter.u.el)
        ==
      :_  ara(esse (~(put by esse.ara) k.rex.ara u.el), visa (~(uni by visa.ara) vi))
      :~  [%give %fact ~[/homunculus-http] %json !>(^-(json [%s (crip (volo lux))]))]
      ==
    ::
    ?:  |(?=(%cur-l lex) ?=(%cur-r lex) ?=(%cur-u lex) ?=(%cur-d lex))
      ?~  rex.ara  [~ ara]
      =/  el=(unit ens)  (~(get by esse.ara) k.rex.ara)
      ?~  el  [~ ara]
      ?.  ?=(%input -.ars.u.el)  [~ ara]
      =/  oi=loci  i.ars.u.el
      =.  i.ars.u.el
        ?~  vox.ars.u.el  i.ars.u.el
        ?+  lex  i.ars.u.el
            %cur-l
          ?:  =(1 h.size.res.u.el)
            [?:(=(0 x.i.ars.u.el) 0 (dec x.i.ars.u.el)) y.i.ars.u.el]
          ?:  =(0 x.i.ars.u.el)
            ?:  =(0 y.i.ars.u.el)
              i.ars.u.el
            =.  y.i.ars.u.el  (dec y.i.ars.u.el)
            =/  l=@ud  (lent ^-(lina (snag y.i.ars.u.el ^-(vox vox.ars.u.el))))
            [?:(=(0 l) 0 (dec l)) y.i.ars.u.el]
          [(dec x.i.ars.u.el) y.i.ars.u.el]
            %cur-r
          ?:  =(1 h.size.res.u.el)
            =/  x=@ud  +(x.i.ars.u.el)
            ?:  (gth x (lent i.vox.ars.u.el))
              i.ars.u.el
            [x y.i.ars.u.el]
          =/  x=@ud  +(x.i.ars.u.el)
          =/  l=@ud  (lent ^-(lina (snag y.i.ars.u.el ^-(vox vox.ars.u.el))))
          ?:  (gth x l)
            =/  y=@ud  +(y.i.ars.u.el)
            ?:  (gte y (lent vox.ars.u.el))
              i.ars.u.el
            [0 y]
          [x y.i.ars.u.el]
            %cur-u
          ?:  =(1 h.size.res.u.el)
            [0 0]
          ?:  =(0 y.i.ars.u.el)
            [0 0]
          =.  y.i.ars.u.el  (dec y.i.ars.u.el)
          =/  l=@ud  (pono (snag y.i.ars.u.el ^-(vox vox.ars.u.el)))
          :_  y.i.ars.u.el
          ?:  (lte x.i.ars.u.el l)
            x.i.ars.u.el
          l
            %cur-d
          ?:  =(1 h.size.res.u.el)
            [(lent i.vox.ars.u.el) y.i.ars.u.el]
          =/  y=@ud  +(y.i.ars.u.el)
          ?:  (gte y (lent vox.ars.u.el))
            [(lent (rear vox.ars.u.el)) y.i.ars.u.el]
          =.  y.i.ars.u.el  y
          =/  l=@ud  (pono (snag y.i.ars.u.el ^-(vox vox.ars.u.el)))
          :_  y.i.ars.u.el
          ?:  (lte x.i.ars.u.el l)
            x.i.ars.u.el
          l
        ==
      ?:  =(oi i.ars.u.el)
        %_    novo
            lex
          ?+  lex   lex
            %cur-l  %nav-l
            %cur-r  %nav-r
            %cur-u  %nav-u
            %cur-d  %nav-d
          ==
        ==
      =/  oab=@ud  ab.ars.u.el
      =.  ab.ars.u.el
        ?~  vox.ars.u.el  ab.ars.u.el
        ?:  |(?=(%cur-l lex) ?=(%cur-u lex))
          ?:  =(0 ab.ars.u.el)  0
          ?:  =(1 h.size.res.u.el)
            ?:  ?=(%cur-u lex)  0
            ?:  (lte x.i.ars.u.el ab.ars.u.el)
              (dec ab.ars.u.el)
            ab.ars.u.el
          ?:  (lth y.i.ars.u.el ab.ars.u.el)
            (dec ab.ars.u.el)
          ab.ars.u.el
        ?:  |(?=(%cur-r lex) ?=(%cur-d lex))
          ?:  =(1 h.size.res.u.el)
            ?:  (lth (sub x.i.ars.u.el ab.ars.u.el) w.size.res.u.el)
              ab.ars.u.el
            ?:  ?=(%cur-d lex)
              =/  l=@ud  (lent i.vox.ars.u.el)
              ?:((lte w.size.res.u.el l) +((sub l w.size.res.u.el)) 0)
            +(ab.ars.u.el)
          ?:  (lth (sub y.i.ars.u.el ab.ars.u.el) h.size.res.u.el)
            ab.ars.u.el
          +(ab.ars.u.el)
        ab.ars.u.el
      ?:  =(oab ab.ars.u.el)
        =/  ansi=@t
          %-  crip
          %-  volo
          (vado ab.ars.u.el i.ars.u.el size.res.u.el lar.u.el iter.u.el)
        :_  ara(esse (~(put by esse.ara) k.rex.ara u.el))
        :~  [%give %fact ~[/homunculus-http] %json !>(^-(json [%s ansi]))]
        ==
      =/  vi=visa  (rinp lar.u.el modi.u.el res.u.el ab.ars.u.el vox.ars.u.el)
      =.  vi  (~(int by visa.u.el) (ruo iter.u.el vi))
      =.  visa.u.el  vi
      =/  =lux
        :-  %mor
        :~  (dono visa.ara vi)
            (vado ab.ars.u.el i.ars.u.el size.res.u.el lar.u.el iter.u.el)
        ==
      :_  ara(esse (~(put by esse.ara) k.rex.ara u.el), visa (~(uni by visa.ara) vi))
      :~  [%give %fact ~[/homunculus-http] %json !>(^-(json [%s (crip (volo lux))]))]
      ==
    ::
    ?:  |(?=(%scr-u lex) ?=(%scr-d lex))
      ?.  ?=(%whe -.zon)  [~ ara]
      =/  mk=(unit rami)  (~(get by mus.ara) [x.zon y.zon])
      ?~  mk  [~ ara]
      =/  sk=$@(~ rami)  (ligo u.mk equi.ara)
      ?~  sk  [~ ara]
      =/  upd=$@(~ [=esse cura])
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
      ?~  upd  [~ ara]
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
            (fero rex.ara equi.ara esse.ara)
        ==
      :_  ara(visa visa.upd)
      :~  [%give %fact ~[/homunculus-http] %json !>(^-(json [%s (crip (volo lux))]))]
      ==
    ::
    [~ ara]
  ::
  ++  gero                  :: build an ordered list of navigation points
    |=  [r=rex o=ordo]
    ^-  ordo
    ?~  r  o
    =/  chis=ordo
      ?.  |(=(%nav-r lex) =(%nav-d lex))
        ~
      %+  sort
        ^-  ordo
        %+  skim  ^-(ordo o)
        |=  =dux
        (alo k.r k.dux)
      |=  [a=dux b=dux]
      (lth (reor a r) (reor b r))
    ?:  ?=(^ chis)
      chis
    =/  tars=ordo
      %+  sort  ^-(ordo (skim ^-(ordo o) (cieo r)))
      |=  [a=dux b=dux]
      (lth (reor a r) (reor b r))
    ?~  tars
      ~
    =/  pars=ordo
      %+  sort
        ^-  ordo
        %+  skim  ^-(ordo o)
        |=  =dux
        ^-  bean
        ?:  =(k.dux k.r)
          |
        ?:  (alo k.dux k.i.tars)
          !(alo k.dux k.r)
        |
      |=  [a=dux b=dux]
      (lth (lent k.a) (lent k.b))
    ?~  pars
      tars
    ?:  ((cieo r) i.pars)
      pars
    ~
  ::
  ++  alo                   :: determine whether element b is a child of element a
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
  ::
  ++  tego                  :: check whether element a or element b layers over the other
    |=  [a=rami b=rami]
    =/  a=$@(~ rami)  (flop a)
    =/  b=$@(~ rami)  (flop b)
    |-  ^-  ?(%a %b %~)
    ?~  a  %~
    ?~  b  %~
    ?~  t.a  %~
    ?~  t.b  %~
    ?.  =(axis.i.t.a axis.i.t.b)
      ?:  &(?=(%l axis.i.t.a) ?=(%~ axis.i.t.b))
        %a
      ?:  &(?=(%~ axis.i.t.a) ?=(%l axis.i.t.b))
        %b
      %~
    ?.  =(ager.i.t.a ager.i.t.b)
      ?:  &(?=(%l axis.i.t.a) ?=(%l axis.i.t.b))
        ?:  (gth ager.i.t.a ager.i.t.b)
          %b
        %a
      %~
    $(a t.a, b t.b)
  ::
  ++  abdo                  :: determine if any coordinate in dux or rex overlap
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
  ::
  ++  cieo                  :: check whether a navigation point is viable given the current selection
    |=  rex=dux
    |=  =dux
    ^-  bean
    ?:  =(k.dux k.rex)  |
    ?+  lex  |
        %nav-l
      ?:  (abdo dux rex)
        ?:  =(l.dux l.rex)
          |((alo k.dux k.rex) =(%b (tego k.rex k.dux)))
        (lth l.dux l.rex)
      ?.  =(%~ (tego k.rex k.dux))
        (lth l.dux l.rex)
      (lth r.dux l.rex)
        %nav-u
      ?:  (abdo dux rex)
        ?:  =(t.dux t.rex)
          |((alo k.dux k.rex) =(%b (tego k.rex k.dux)))
        (lth t.dux t.rex)
      ?.  =(%~ (tego k.rex k.dux))
        (lth t.dux t.rex)
      (lth b.dux t.rex)
        %nav-r
      ?:  (abdo dux rex)
        ?:  =(l.dux l.rex)
          |((alo k.rex k.dux) =(%a (tego k.rex k.dux)))
        (gth l.dux l.rex)
      ?.  =(%~ (tego k.rex k.dux))
        (gth l.dux l.rex)
      (gth l.dux r.rex)
        %nav-d
      ?:  (abdo dux rex)
        ?:  =(t.dux t.rex)
          |((alo k.rex k.dux) =(%a (tego k.rex k.dux)))
        (gth t.dux t.rex)
      ?.  =(%~ (tego k.rex k.dux))
        (gth t.dux t.rex)
      (gth t.dux b.rex)
    ==
  ::
  ++  reor                  :: get the euclidean distance between a selection and a navigation point
    |=  [=dux rex=dux]
    ^-  @ud
    =;  pyt=(pair @ud @ud)
      (add (mul 10 p.pyt) q.pyt)
    ?+  lex  [0 0]
        %nav-l
      ?:  |((abdo dux rex) !=(%~ (tego k.rex k.dux)))
        %-  sqt  %+  add
          (pow ?:((lte l.dux l.rex) (sub l.rex l.dux) (sub l.dux l.rex)) 2)
        (pow (mul ?:((gte t.rex t.dux) (sub t.rex t.dux) (sub t.dux t.rex)) 2) 2)
      %-  sqt  %+  add
        (pow ?:((lte r.dux l.rex) (sub l.rex r.dux) (sub r.dux l.rex)) 2)
      (pow (mul ?:((gte t.rex t.dux) (sub t.rex t.dux) (sub t.dux t.rex)) 2) 2)
        %nav-u
      ?:  |((abdo dux rex) !=(%~ (tego k.rex k.dux)))
        %-  sqt  %+  add
          (pow ?:((gte l.dux l.rex) (sub l.dux l.rex) (sub l.rex l.dux)) 2)
        (pow (mul ?:((lte t.dux t.rex) (sub t.rex t.dux) (sub t.dux t.rex)) 2) 2)
      %-  sqt  %+  add
        (pow ?:((gte l.dux l.rex) (sub l.dux l.rex) (sub l.rex l.dux)) 2)
      (pow (mul ?:((lte b.dux t.rex) (sub t.rex b.dux) (sub b.dux t.rex)) 2) 2)
        %nav-r
      ?:  |((abdo dux rex) !=(%~ (tego k.rex k.dux)))
        %-  sqt  %+  add
          (pow ?:((lte l.rex l.dux) (sub l.dux l.rex) (sub l.rex l.dux)) 2)
        (pow (mul ?:((gte t.dux t.rex) (sub t.dux t.rex) (sub t.rex t.dux)) 2) 2)
      %-  sqt  %+  add
        (pow ?:((lte r.rex l.dux) (sub l.dux r.rex) (sub r.rex l.dux)) 2)
      (pow (mul ?:((gte t.dux t.rex) (sub t.dux t.rex) (sub t.rex t.dux)) 2) 2)
        %nav-d
      ?:  |((abdo dux rex) !=(%~ (tego k.rex k.dux)))
        %-  sqt  %+  add
          (pow ?:((gte l.rex l.dux) (sub l.rex l.dux) (sub l.dux l.rex)) 2)
        (pow (mul ?:((lte t.rex t.dux) (sub t.dux t.rex) (sub t.rex t.dux)) 2) 2)
      %-  sqt  %+  add
        (pow ?:((gte l.rex l.dux) (sub l.rex l.dux) (sub l.dux l.rex)) 2)
      (pow (mul ?:((lte b.rex t.dux) (sub t.dux b.rex) (sub b.rex t.dux)) 2) 2)
    ==
  ::
  ++  rogo                  :: find a dux in ordo by key
    |=  [k=rami o=ordo]
    ^-  $@(~ dux)
    ?~  o  ~
    ?:  =(k k.i.o)
      i.o
    $(o t.o)
  ::
  ++  opto                  :: process a checkbox, potentially in a radio group
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
  ++  lego                  :: reset and collect the values of inputs under a form element
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
    =/  el=(unit ens)  (~(get by esse.ara) fk) 
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
      =/  val=@t  (crip (tufa ^-(lina (zing vox.ars.u.el))))
      =:  vox.ars.u.el  ~
          ab.ars.u.el   0
          i.ars.u.el    [0 0]
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
  ++  abeo                  :: handle a scroll event for a scroll element by key
    |=  [pk=rami le=^lex]   :: returns an update from the root
    ^-  $@(~ [esse cura])
    =/  par=(unit ens)  (~(get by esse.ara) pk)
    ?~  par  ~
    ?.  ?=(%scroll -.ars.u.par)  ~
    ?:  ?|  &(?=(%scr-l le) =(0 x.iter.ars.u.par))
            &(?=(%scr-r le) =(x.sola.ars.u.par x.iter.ars.u.par))
            &(?=(%scr-u le) =(0 y.iter.ars.u.par))
            &(?=(%scr-d le) =(y.sola.ars.u.par y.iter.ars.u.par))
        ==
      ~
    =.  iter.ars.u.par
      ?+  le  iter.ars.u.par
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
    =/  opu=opus  (eo esse.ara visa.ara le ~ pk visa.u.par prl prt prr prb)
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
    =/  dic=cura  (dico esse.opu ?^(rex.ara k.rex.ara ~))
    [esse.opu dic]
  ::
  --
::
++  eo                                  :: perform a scroll on the elements within a scroll element
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
    |=  [[l=loci n=nodi] acc=visa]
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
    (~(put by acc) l n)
  =.  vi  (~(int by olv) (~(dif by vi) visa.a))
  =.  esse.a  (~(put by esse.a) k u.chi(visa vi))
  =.  visa.a  (~(uni by visa.a) vi)
  $(ager.i.k +(ager.i.k))
::
++  ligo                    :: find the key of a potential scroll parent by child key
  |=  [r=rami q=equi]
  ^-  $@(~ rami)
  ?:  (~(has in q) r)
    r
  ?~  t.r  ~
  $(r t.r)
::
++  pono                    :: get the length of a row in vox without the trailing whitespace
  |=  lop=lina
  =.  lop  (flop lop)
  |-  ^-  @ud
  ?~  lop  0
  ?.  =(~-. i.lop)  (lent lop)
  $(lop t.lop)
::
++  sumo                    :: get the length of the first word in a vox row
  |=  ro=lina
  =|  n=@ud
  |-  ^-  @ud
  ?~  ro  n
  ?:  =(~-. i.ro)  n
  $(n +(n), ro t.ro)
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
++  vado                    :: determine the cursor coordinate of an input character index
  |=  [ab=@ud i=loci [w=@ud h=@ud] =lar =iter]
  ^-  lux
  =/  [x=@ud y=@ud]
    ?:  =(1 h)
      [(add x.lar (sub x.i ab)) y.lar]
    ?:  (lth x.i w)
      [(add x.lar x.i) (add y.lar (sub y.i ab))]
    =/  ran=@ud  (sub y.i ab)
    ?.  (lth +(ran) h)
      [(add x.lar ?:(=(0 w) 0 (dec w))) (add y.lar ran)]
    [x.lar +((add y.lar ran))]
  :+  %hop
    ?:((lth x.iter x) (sub x x.iter) 1)
  ?:((lth y.iter y) (sub y y.iter) 1)
::
++  fero                    :: make a general cursor move update
  |=  [r=rex q=equi e=esse]
  ^-  lux
  ?~  r  [%hop [1 1]]
  =/  el=(unit ens)  (~(get by e) k.r)
  ?~  el  [%hop [1 1]]
  ?:  ?=(%input -.ars.u.el)
    (vado ab.ars.u.el i.ars.u.el size.res.u.el lar.u.el iter.u.el)
  ?:  &(=(0 x.iter.u.el) =(0 y.iter.u.el))
    [%hop [l.r t.r]]
  =/  sk=$@(~ rami)  (ligo k.r q)
  ?~  sk  [%hop [l.r t.r]]
  =/  spar=(unit ens)  (~(get by e) sk)
  ?~  spar  [%hop [l.r t.r]]
  (cedo r sk u.spar)
::
++  noto                    :: parse belt to nota
  |=  z=zona
  ^-  nota
  ?:  ?=(%mod -.z)  [-.z -.+.z -.+.+.z]
  ?:  ?=(%clk -.z)  [-.z -.+.z]
  ?:  ?=(%whe -.z)  [-.z -.+.z]
  ?:  ?=(%txt -.z)  [-.z ~]
  ?:  ?=(%aro -.z)  z
  ?:  ?=(%bac -.z)  z
  ?:  ?=(%ret -.z)  z
  !!
::  ::  ::  ::  ::  ::  ::
++  dolo                    :: get default styles for a semantic element
  |=  el=@tas
  ^-  ovum
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
      %line-h
    :*  size=[[%p 100] [%c 1]]
        padd=[[%c 0] [%c 0] [%c 0] [%c 0]]
        marg=[[%c 0] [%c 0] [%c 0] [%c 0]]
        flex=[0 0]
        flow=[%row %clip]
        look=[~ ~ ~]
    ==
      %line-v
    :*  size=[[%c 1] [%p 100]]
        padd=[[%c 0] [%c 0] [%c 0] [%c 0]]
        marg=[[%c 0] [%c 0] [%c 0] [%c 0]]
        flex=[0 0]
        flow=[%col %clip]
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
  =/  [=ovum =aves =ars =lina velo=mart]
      ?+  n             [(dolo %$) ~ [%$ ~] ~ ~]
        %$              [(dolo %text) ~ [%text ~] ?~(a ~ (tuba v.i.a)) ~]
        %layer          [(dolo %layer) ~ [%layer ~] ~ ~]
        %select         [(dolo %$) ~ [%select [~ ~ ~] %~] ~ ~]
        %border-left    [(dolo %border-left) ~ [%border %l %~] ~ ~]
        %border-right   [(dolo %border-right) ~ [%border %r %~] ~ ~]
        %border-top     [(dolo %border-top) ~ [%border %t %~] ~ ~]
        %border-bottom  [(dolo %border-bottom) ~ [%border %b %~] ~ ~]
        %line-h         [(dolo %line-h) ~ [%line %h %light] ~ ~]
        %line-v         [(dolo %line-v) ~ [%line %v %light] ~ ~]
        %scroll         [(dolo %scroll) ~ [%scroll *iter *muri *sola] ~ ~]
        %input          [(dolo %input) ~ [%input 0 [0 0] ~] ~ ~]
        %checkbox       [(dolo %checkbox) ~ [%checkbox %.n] ~ ~]
        %radio          [(dolo %$) ~ [%radio ~] ~ ~]
        %form           [(dolo %$) ~ [%form ~] ~ ~]
        %submit         [(dolo %$) ~ [%select [~ ~ ~] %submit] ~ ~]
      ==
  |-  ^-  [^ovum ^aves ^ars ^lina mart]
  ?~  a  [ovum aves ars lina velo]
  ?+  n.i.a  $(a t.a)
      %w
    ?:  &(?=(%border -.ars) |(?=(%t ad.ars) ?=(%b ad.ars)))
      $(a t.a)
    $(w.size.ovum (pars v.i.a), a t.a)
      %h
    ?:  &(?=(%border -.ars) |(?=(%l ad.ars) ?=(%r ad.ars)))
      $(a t.a)
    $(h.size.ovum (pars v.i.a), a t.a)
      %p
    =/  v=as  (pars v.i.a)
    $(padd.ovum [v v v v], a t.a)
      %px
    =/  v=as  (pars v.i.a)
    $(l.padd.ovum v, r.padd.ovum v, a t.a)
      %py
    =/  v=as  (pars v.i.a)
    $(t.padd.ovum v, b.padd.ovum v, a t.a)
      %pl
    $(l.padd.ovum (pars v.i.a), a t.a)
      %pr
    $(r.padd.ovum (pars v.i.a), a t.a)
      %pt
    $(t.padd.ovum (pars v.i.a), a t.a)
      %pb
    $(b.padd.ovum (pars v.i.a), a t.a)
      %m
    ?:  ?=(%border -.ars)
      $(a t.a)
    =/  v=as  (pars v.i.a)
    $(marg.ovum [v v v v], a t.a)
      %mx
    ?:  ?=(%border -.ars)
      $(a t.a)
    =/  v=as  (pars v.i.a)
    $(l.marg.ovum v, r.marg.ovum v, a t.a)
      %my
    ?:  ?=(%border -.ars)
      $(a t.a)
    =/  v=as  (pars v.i.a)
    $(t.marg.ovum v, b.marg.ovum v, a t.a)
      %ml
    ?:  ?=(%border -.ars)
      $(a t.a)
    $(l.marg.ovum (pars v.i.a), a t.a)
      %mr
    ?:  ?=(%border -.ars)
      $(a t.a)
    $(r.marg.ovum (pars v.i.a), a t.a)
      %mt
    ?:  ?=(%border -.ars)
      $(a t.a)
    $(t.marg.ovum (pars v.i.a), a t.a)
      %mb
    ?:  ?=(%border -.ars)
      $(a t.a)
    $(b.marg.ovum (pars v.i.a), a t.a)
      %fx
    =/  num=(unit @ud)  (slaw %ud (crip v.i.a))
    ?:  ?=(^ num)
      $(x.flex.ovum ?:((lte u.num 100) u.num 100), a t.a)
    ?+  (@tas (crip v.i.a))  $(a t.a)
      %start   $(x.flex.ovum 0, a t.a)
      %center  $(x.flex.ovum 50, a t.a)
      %end     $(x.flex.ovum 100, a t.a)
    ==
      %fy
    =/  num=(unit @ud)  (slaw %ud (crip v.i.a))
    ?:  ?=(^ num)
      $(y.flex.ovum ?:((lte u.num 100) u.num 100), a t.a)
    ?+  (@tas (crip v.i.a))  $(a t.a)
      %start   $(y.flex.ovum 0, a t.a)
      %center  $(y.flex.ovum 50, a t.a)
      %end     $(y.flex.ovum 100, a t.a)
    ==
      %fl
    ?+  (@tas (crip v.i.a))  $(a t.a)
      %row          $(flow.ovum [%row %clip], a t.a)
      %row-clip     $(flow.ovum [%row %clip], a t.a)
      %row-wrap     $(flow.ovum [%row %wrap], a t.a)
      %column       $(flow.ovum [%col %clip], a t.a)
      %column-clip  $(flow.ovum [%col %clip], a t.a)
      %column-wrap  $(flow.ovum [%col %wrap], a t.a)
    ==
      %cb
    ?:  &(?=(^ v.i.a) =('#' i.v.i.a))
      $(b.look.ovum [~ (seco v.i.a)], a t.a)
    ?+  (@tas (crip v.i.a))  $(b.look.ovum ~, a t.a)
      %red      $(b.look.ovum [~ %r], a t.a)
      %green    $(b.look.ovum [~ %g], a t.a)
      %blue     $(b.look.ovum [~ %b], a t.a)
      %cyan     $(b.look.ovum [~ %c], a t.a)
      %magenta  $(b.look.ovum [~ %m], a t.a)
      %yellow   $(b.look.ovum [~ %y], a t.a)
      %black    $(b.look.ovum [~ %k], a t.a)
      %white    $(b.look.ovum [~ %w], a t.a)
    ==
      %cf
    ?:  &(?=(^ v.i.a) =('#' i.v.i.a))
      $(f.look.ovum [~ (seco v.i.a)], a t.a)
    ?+  (@tas (crip v.i.a))  $(f.look.ovum ~, a t.a)
      %red      $(f.look.ovum [~ %r], a t.a)
      %green    $(f.look.ovum [~ %g], a t.a)
      %blue     $(f.look.ovum [~ %b], a t.a)
      %cyan     $(f.look.ovum [~ %c], a t.a)
      %magenta  $(f.look.ovum [~ %m], a t.a)
      %yellow   $(f.look.ovum [~ %y], a t.a)
      %black    $(f.look.ovum [~ %k], a t.a)
      %white    $(f.look.ovum [~ %w], a t.a)
    ==
      %d
    ?+  (@tas (crip v.i.a))  $(d.look.ovum ~, a t.a)
      %bold       $(d.look.ovum [~ (silt ~[%br])], a t.a)
      %blink      $(d.look.ovum [~ (silt ~[%bl])], a t.a)
      %underline  $(d.look.ovum [~ (silt ~[%un])], a t.a)
    ==
      %b
    ?.  ?=(%border -.ars)
      $(velo [i.a velo], a t.a)
    ?+  (@tas (crip v.i.a))  $(a t.a)
      %light   $(ora.ars %light, a t.a)
      %heavy   $(ora.ars %heavy, a t.a)
      %double  $(ora.ars %double, a t.a)
      %arc     $(ora.ars %arc, a t.a)
      %blank   $(ora.ars %blank, a t.a)
    ==
      %b-cb
    ?.  ?=(%border -.ars)
      $(velo [i.a velo], a t.a)
    ?:  &(?=(^ v.i.a) =('#' i.v.i.a))
      $(b.look.ovum [~ (seco v.i.a)], a t.a)
    ?+  (@tas (crip v.i.a))  $(b.look.ovum ~, a t.a)
      %red      $(b.look.ovum [~ %r], a t.a)
      %green    $(b.look.ovum [~ %g], a t.a)
      %blue     $(b.look.ovum [~ %b], a t.a)
      %cyan     $(b.look.ovum [~ %c], a t.a)
      %magenta  $(b.look.ovum [~ %m], a t.a)
      %yellow   $(b.look.ovum [~ %y], a t.a)
      %black    $(b.look.ovum [~ %k], a t.a)
      %white    $(b.look.ovum [~ %w], a t.a)
    ==
      %b-cf
    ?.  ?=(%border -.ars)
      $(velo [i.a velo], a t.a)
    ?:  &(?=(^ v.i.a) =('#' i.v.i.a))
      $(f.look.ovum [~ (seco v.i.a)], a t.a)
    ?+  (@tas (crip v.i.a))  $(f.look.ovum ~, a t.a)
      %red      $(f.look.ovum [~ %r], a t.a)
      %green    $(f.look.ovum [~ %g], a t.a)
      %blue     $(f.look.ovum [~ %b], a t.a)
      %cyan     $(f.look.ovum [~ %c], a t.a)
      %magenta  $(f.look.ovum [~ %m], a t.a)
      %yellow   $(f.look.ovum [~ %y], a t.a)
      %black    $(f.look.ovum [~ %k], a t.a)
      %white    $(f.look.ovum [~ %w], a t.a)
    ==
      %b-d
    ?.  ?=(%border -.ars)
      $(velo [i.a velo], a t.a)
    ?+  (@tas (crip v.i.a))  $(d.look.ovum ~, a t.a)
      %bold       $(d.look.ovum [~ (silt ~[%br])], a t.a)
      %blink      $(d.look.ovum [~ (silt ~[%bl])], a t.a)
      %underline  $(d.look.ovum [~ (silt ~[%un])], a t.a)
    ==
      %l
    ?.  ?=(%line -.ars)  $(a t.a)
    ?+  (@tas (crip v.i.a))  $(a t.a)
      %light   $(ora.ars %light, a t.a)
      %heavy   $(ora.ars %heavy, a t.a)
      %double  $(ora.ars %double, a t.a)
      %arc     $(ora.ars %arc, a t.a)
      %blank   $(ora.ars %blank, a t.a)
    ==
      %sel
    $(aves (~(put by aves) %sel (crip v.i.a)), a t.a)
      %sel-cb
    ?.  ?=(%select -.ars)  $(a t.a)
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
    ?.  ?=(%select -.ars)  $(a t.a)
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
    ?.  ?=(%select -.ars)  $(a t.a)
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
++  sero                    :: separate grow elements from the normal element list
  |=  [plow=fuga norm=marl]
  =|  [i=@ud gro=marl aqu=aqua nor=marl]
  |-  ^-  [[marl aqua] marl]
  ?~  norm  [[(flop gro) (flop aqu)] (flop nor)]
  =/  [w=bean h=bean]
    [?=(^ (find ~[[%w "grow"]] a.g.i.norm)) ?=(^ (find ~[[%h "grow"]] a.g.i.norm))]
  ?.  |(w h)  $(nor [i.norm nor], norm t.norm, i +(i))
  ?:  ?=([%row %clip] plow)
    %=  $
      gro   ?:(w ?:(h [i.norm(a.g (snoc a.g.i.norm [%h "100%"])) gro] [i.norm gro]) gro)
      aqu   ?:(w [[i 0 0] aqu] aqu)
      nor   ?:(w nor [i.norm(a.g (snoc a.g.i.norm [%h "100%"])) nor])
      norm  t.norm
      i     +(i)
    ==
  ?:  ?=([%col %clip] plow)
    %=  $
      gro   ?:(h ?:(w [i.norm(a.g (snoc a.g.i.norm [%w "100%"])) gro] [i.norm gro]) gro)
      aqu   ?:(h [[i 0 0] aqu] aqu)
      nor   ?:(h nor [i.norm(a.g (snoc a.g.i.norm [%w "100%"])) nor])
      norm  t.norm
      i     +(i)
    ==
  ?:  ?=(%wrap b.plow)
    %=  $
      nor
        :_  nor
        %_  i.norm
          a.g
            ^-  mart
            ?:  &(w h)  (weld a.g.i.norm ^-(mart ~[[%w "100%"] [%h "100%"]]))
            (snoc a.g.i.norm ?:(w [%w "100%"] [%h "100%"]))
        ==
      norm  t.norm
      i     +(i)
    ==
  $(nor [i.norm nor], norm t.norm, i +(i))
::
++  duco                     :: render a navigation style change update
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
  (~(urn by vi) |=([* no=nodi] [fi q.no]))
::
++  ruo                     :: shift a set of characters by scroll position
  |=  [it=iter vi=visa]
  ^-  visa
  ?:  &(=(0 x.it) =(0 y.it))
    vi
  %-  %~  rep
        by
      vi
  |=  [[l=loci n=nodi] ac=visa]
  ^-  visa
  ?:  |((gth x.it x.l) (gth y.it y.l))
    ac
  (~(put by ac) [(sub x.l x.it) (sub y.l y.it)] n)
::
++  cogo                    :: get the collective size of an element's child elements
  |=  [vlar=lar k=rami e=esse]
  ^-  [w=@ud h=@ud]
  =/  [i=@ud ax=axis]  [0 %~]
  =|  [rig=(unit @ud) bot=(unit @ud)]
  =.  -
    |-  ^-  [(unit @ud) (unit @ud)]
    =/  el=(unit ens)  (~(get by e) [[ax i] k])
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
::
++  mino                    :: reposition an element
  |=  [movx=@ud movy=@ud lim=modi vdif=visa el=ens]
  ^-  ens
  %_  el
    visa
      =/  v=(list [=loci =nodi])  ~(tap by visa.el)
      %-  %~  dif  by
        ^-  visa  %-  malt
        |-  ^-  (list [loci nodi])
        ?~  v  ~
        =/  x=@ud  ?:(=(0 movx) x.loci.i.v (add x.loci.i.v movx))
        =/  y=@ud  ?:(=(0 movy) y.loci.i.v (add y.loci.i.v movy))
        ?:  |((gth x x.lim) (gth y y.lim))  $(v t.v)
        [[[x y] nodi.i.v] $(v t.v)]
      vdif
    lar   [(add x.lar.el movx) (add y.lar.el movy)]
    modi  [(add x.modi.el movx) (add y.modi.el movy)]
  ==
::
++  oro                     :: turn lina into vox
  |=  [wid=@ud hei=@ud lin=lina]
  ?:  =(1 hei)  ^-(vox ~[lin])
  =|  [v=vox col=@ud wod=@ud]
  |-  ^-  vox
  ?~  lin
    (flop ?:(&(?=(^ v) ?=(^ i.v)) v(i (flop i.v)) v))
  ?~  v
    ?:  (gte +(col) wid)
      $(lin t.lin, col 0, wod 0, v [~ [i.lin ~] ~])
    $(lin t.lin, col +(col), wod +(wod), v [[i.lin ~] ~])
  ?:  =(~-. i.lin)
    $(lin t.lin, col +(col), wod +(wod), v [[i.lin i.v] t.v])
  ?:  &(?=(^ i.v) =(~-. i.i.v))
    ?:  |((gth col wid) &(=(wid col) !&(?=(^ t.lin) =(~-. i.t.lin))))
      $(lin t.lin, col 1, wod 1, v [[i.lin ~] ^-(lina (flop i.v)) t.v])
    $(lin t.lin, col +(col), wod 1, v [[i.lin i.v] t.v])
  ?:  (gte col wid)
    ?:  (lth +(wod) wid)
      %=  $
        lin  t.lin
        col  +(wod)
        wod  +(wod)
        v     
          :+  [i.lin ^-(lina (scag wod ^-(lina i.v)))]
            ^-(lina (flop (oust [0 wod] ^-(lina i.v))))
          t.v
      ==
    ?:  (gte col wid)
      $(lin t.lin, col 1, wod 1, v [[i.lin ~] ^-(lina (flop i.v)) t.v])
    $(lin t.lin, col 0, wod 0, v ?~(t.lin [[i.lin i.v] t.v] [~ ^-(lina (flop [i.lin i.v])) t.v]))
  $(lin t.lin, col +(col), wod +(wod), v [[i.lin i.v] t.v])
::
++  iugo                    :: make a line intersection character
  |=  crux
  ^-  @c
  ?:  ?&  ?=(%~ l)  ?=(%~ t)
          ?|  &(?=(%arc r) ?=(%arc b))
              &(?=(%arc r) ?=(%light b))
              &(?=(%light r) ?=(%arc b))
      ==  ==
    ~-~256d.  ::  ╭
  ?:  ?&  ?=(%~ r)  ?=(%~ t)
          ?|  &(?=(%arc l) ?=(%arc b))
              &(?=(%arc l) ?=(%light b))
              &(?=(%light l) ?=(%arc b))
      ==  ==
    ~-~256e.  ::  ╮
  ?:  ?&  ?=(%~ l)  ?=(%~ b)
          ?|  &(?=(%arc r) ?=(%arc t))
              &(?=(%arc r) ?=(%light t))
              &(?=(%light r) ?=(%arc t))
      ==  ==
    ~-~2570.  ::  ╰
  ?:  ?&  ?=(%~ r)  ?=(%~ b)
          ?|  &(?=(%arc l) ?=(%arc t))
              &(?=(%arc l) ?=(%light t))
              &(?=(%light l) ?=(%arc t))
      ==  ==
    ~-~256f.  ::  ╯
  =?  +<  |(?=(%arc q.c) ?=(%arc l) ?=(%arc r) ?=(%arc t) ?=(%arc b))
    %_  +<
      q.c  ?:(?=(%arc q.c) %light q.c)
      l    ?:(?=(%arc l) %light l)
      r    ?:(?=(%arc r) %light r)
      t    ?:(?=(%arc t) %light t)
      b    ?:(?=(%arc b) %light b)
    ==
  ?:  &(?=(%~ l) ?=(%~ t) ?=(%light r) ?=(%light b))  ~-~250c.  ::  ┌
  ?:  &(?=(%~ l) ?=(%~ t) ?=(%heavy r) ?=(%light b))  ~-~250d.  ::  ┍
  ?:  &(?=(%~ l) ?=(%~ t) ?=(%light r) ?=(%heavy b))  ~-~250e.  ::  ┎
  ?:  &(?=(%~ l) ?=(%~ t) ?=(%heavy r) ?=(%heavy b))  ~-~250f.  ::  ┏
  ?:  &(?=(%~ l) ?=(%~ t) ?=(%double r) ?=(%light b))  ~-~2552.  ::  ╒
  ?:  &(?=(%~ l) ?=(%~ t) ?=(%light r) ?=(%double b))  ~-~2553.  ::  ╓
  ?:  &(?=(%~ l) ?=(%~ t) ?=(%double r) ?=(%double b))  ~-~2554.  ::  ╔
  ?:  &(?=(%~ r) ?=(%~ t) ?=(%light l) ?=(%light b))  ~-~2510.  ::  ┐
  ?:  &(?=(%~ r) ?=(%~ t) ?=(%heavy l) ?=(%light b))  ~-~2511.  ::  ┑
  ?:  &(?=(%~ r) ?=(%~ t) ?=(%light l) ?=(%heavy b))  ~-~2512.  ::  ┒
  ?:  &(?=(%~ r) ?=(%~ t) ?=(%heavy l) ?=(%heavy b))  ~-~2513.  ::  ┓
  ?:  &(?=(%~ r) ?=(%~ t) ?=(%double l) ?=(%light b))  ~-~2555.  ::  ╕
  ?:  &(?=(%~ r) ?=(%~ t) ?=(%light l) ?=(%double b))  ~-~2556.  ::  ╖
  ?:  &(?=(%~ r) ?=(%~ t) ?=(%double l) ?=(%double b))  ~-~2557.  ::  ╗
  ?:  &(?=(%~ l) ?=(%~ b) ?=(%light r) ?=(%light t))  ~-~2514.  ::  └
  ?:  &(?=(%~ l) ?=(%~ b) ?=(%heavy r) ?=(%light t))  ~-~2515.  ::  ┕
  ?:  &(?=(%~ l) ?=(%~ b) ?=(%light r) ?=(%heavy t))  ~-~2516.  ::  ┖
  ?:  &(?=(%~ l) ?=(%~ b) ?=(%heavy r) ?=(%heavy t))  ~-~2517.  ::  ┗
  ?:  &(?=(%~ l) ?=(%~ b) ?=(%double r) ?=(%light t))  ~-~2558.  ::  ╘
  ?:  &(?=(%~ l) ?=(%~ b) ?=(%light r) ?=(%double t))  ~-~2559.  ::  ╙
  ?:  &(?=(%~ l) ?=(%~ b) ?=(%double r) ?=(%double t))  ~-~255a.  ::  ╚
  ?:  &(?=(%~ r) ?=(%~ b) ?=(%light l) ?=(%light t))  ~-~2518.  ::  ┘
  ?:  &(?=(%~ r) ?=(%~ b) ?=(%heavy l) ?=(%light t))  ~-~2519.  ::  ┙
  ?:  &(?=(%~ r) ?=(%~ b) ?=(%light l) ?=(%heavy t))  ~-~251a.  ::  ┚
  ?:  &(?=(%~ r) ?=(%~ b) ?=(%heavy l) ?=(%heavy t))  ~-~251b.  ::  ┛
  ?:  &(?=(%~ r) ?=(%~ b) ?=(%double l) ?=(%light t))  ~-~255b.  ::  ╛
  ?:  &(?=(%~ r) ?=(%~ b) ?=(%light l) ?=(%double t))  ~-~255c.  ::  ╜
  ?:  &(?=(%~ r) ?=(%~ b) ?=(%double l) ?=(%double t))  ~-~255d.  ::  ╝
  ?:  ?&  ?=(%~ l)
          ?|  &(?=(%light t) ?=(%light b) ?=(%light r))
              &(?=(%~ t) ?=(%~ b) ?=(%light r) ?=([%v %light] c))
      ==  ==
    ~-~251c.  ::  ├
  ?:  ?&  ?=(%~ l)
          ?|  &(?=(%light t) ?=(%light b) ?=(%heavy r))
              &(?=(%~ t) ?=(%~ b) ?=(%heavy r) ?=([%v %light] c))
      ==  ==
    ~-~251d.  ::  ┝
  ?:  ?&  ?=(%~ l)
          ?|  &(?=(%light t) ?=(%light b) ?=(%double r))
              &(?=(%~ t) ?=(%~ b) ?=(%double r) ?=([%v %light] c))
      ==  ==
    ~-~255e.  ::  ╞
  ?:  ?&  ?=(%~ l)
          ?|  &(?=(%heavy t) ?=(%heavy b) ?=(%heavy r))
              &(?=(%~ t) ?=(%~ b) ?=(%heavy r) ?=([%v %heavy] c))
      ==  ==
    ~-~2523.  ::  ┣
  ?:  ?&  ?=(%~ l)
          ?|  &(?=(%heavy t) ?=(%heavy b) ?=(%light r))
              &(?=(%~ t) ?=(%~ b) ?=(%light r) ?=([%v %heavy] c))
      ==  ==
    ~-~2520.  ::  ┠
  ?:  ?&  ?=(%~ l)
          ?|  &(?=(%double t) ?=(%double b) ?=(%double r))
              &(?=(%~ t) ?=(%~ b) ?=(%double r) ?=([%v %double] c))
      ==  ==
    ~-~2560.  ::  ╠
  ?:  ?&  ?=(%~ l)
          ?|  &(?=(%double t) ?=(%double b) ?=(%light r))
              &(?=(%~ t) ?=(%~ b) ?=(%light r) ?=([%v %double] c))
      ==  ==
    ~-~255f.  ::  ╟
  ?:  &(?=(%~ l) ?=(%heavy t) ?=(%light b) ?=(%light r))  ~-~251e.  ::  ┞
  ?:  &(?=(%~ l) ?=(%light t) ?=(%heavy b) ?=(%light r))  ~-~251f.  ::  ┟
  ?:  &(?=(%~ l) ?=(%heavy t) ?=(%light b) ?=(%heavy r))  ~-~2521.  ::  ┡
  ?:  &(?=(%~ l) ?=(%light t) ?=(%heavy b) ?=(%heavy r))  ~-~2522.  ::  ┢
  ?:  ?&  ?=(%~ r)
          ?|  &(?=(%light t) ?=(%light b) ?=(%light l))
              &(?=(%~ t) ?=(%~ b) ?=(%light l) ?=([%v %light] c))
      ==  ==
    ~-~2524.  ::  ┤
  ?:  ?&  ?=(%~ r)
          ?|  &(?=(%light t) ?=(%light b) ?=(%heavy l))
              &(?=(%~ t) ?=(%~ b) ?=(%heavy l) ?=([%v %light] c))
      ==  ==
    ~-~2525.  ::  ┥
  ?:  ?&  ?=(%~ r)
          ?|  &(?=(%light t) ?=(%light b) ?=(%double l))
              &(?=(%~ t) ?=(%~ b) ?=(%double l) ?=([%v %light] c))
      ==  ==
    ~-~2561.  ::  ╡
  ?:  ?&  ?=(%~ r)
          ?|  &(?=(%heavy t) ?=(%heavy b) ?=(%heavy l))
              &(?=(%~ t) ?=(%~ b) ?=(%heavy l) ?=([%v %heavy] c))
      ==  ==
    ~-~252b.  ::  ┫
  ?:  ?&  ?=(%~ r)
          ?|  &(?=(%heavy t) ?=(%heavy b) ?=(%light l))
              &(?=(%~ t) ?=(%~ b) ?=(%light l) ?=([%v %heavy] c))
      ==  ==
    ~-~2528.  ::  ┨
  ?:  ?&  ?=(%~ r)
          ?|  &(?=(%double t) ?=(%double b) ?=(%double l))
              &(?=(%~ t) ?=(%~ b) ?=(%double l) ?=([%v %double] c))
      ==  ==
    ~-~2563.  ::  ╣
  ?:  ?&  ?=(%~ r)
          ?|  &(?=(%double t) ?=(%double b) ?=(%light l))
              &(?=(%~ t) ?=(%~ b) ?=(%light l) ?=([%v %double] c))
      ==  ==
      ~-~2562.  ::  ╢
  ?:  &(?=(%~ r) ?=(%heavy t) ?=(%light b) ?=(%light l))  ~-~2526.  ::  ┦
  ?:  &(?=(%~ r) ?=(%light t) ?=(%heavy b) ?=(%light l))  ~-~2527.  ::  ┧
  ?:  &(?=(%~ r) ?=(%heavy t) ?=(%light b) ?=(%heavy l))  ~-~2529.  ::  ┩
  ?:  &(?=(%~ r) ?=(%light t) ?=(%heavy b) ?=(%heavy l))  ~-~252a.  ::  ┪
  ?:  ?&  ?=(%~ t)
          ?|  &(?=(%light l) ?=(%light r) ?=(%light b))
              &(?=(%~ l) ?=(%~ r) ?=(%light b) ?=([%h %light] c))
      ==  ==
    ~-~252c.  ::  ┬
  ?:  ?&  ?=(%~ t)
          ?|  &(?=(%light l) ?=(%light r) ?=(%heavy b))
              &(?=(%~ l) ?=(%~ r) ?=(%heavy b) ?=([%h %light] c))
      ==  ==
    ~-~2530.  ::  ┰
  ?:  ?&  ?=(%~ t)
          ?|  &(?=(%light l) ?=(%light r) ?=(%double b))
              &(?=(%~ l) ?=(%~ r) ?=(%double b) ?=([%h %light] c))
      ==  ==
    ~-~2565.  ::  ╥
  ?:  ?&  ?=(%~ t)
          ?|  &(?=(%heavy l) ?=(%heavy r) ?=(%heavy b))
              &(?=(%~ l) ?=(%~ r) ?=(%heavy b) ?=([%h %heavy] c))
      ==  ==
    ~-~2533.  ::  ┳
  ?:  ?&  ?=(%~ t)
          ?|  &(?=(%heavy l) ?=(%heavy r) ?=(%light b))
              &(?=(%~ l) ?=(%~ r) ?=(%light b) ?=([%h %heavy] c))
      ==  ==
    ~-~252f.  ::  ┯
  ?:  ?&  ?=(%~ t)
          ?|  &(?=(%double l) ?=(%double r) ?=(%double b))
              &(?=(%~ l) ?=(%~ r) ?=(%double b) ?=([%h %double] c))
      ==  ==
    ~-~2566.  ::  ╦
  ?:  ?&  ?=(%~ t)
          ?|  &(?=(%double l) ?=(%double r) ?=(%light b))
              &(?=(%~ l) ?=(%~ r) ?=(%light b) ?=([%h %double] c))
      ==  ==
    ~-~2564.  ::  ╤
  ?:  &(?=(%~ t) ?=(%heavy l) ?=(%light r) ?=(%light b))  ~-~252d.  ::  ┭
  ?:  &(?=(%~ t) ?=(%light l) ?=(%heavy r) ?=(%light b))  ~-~252e.  ::  ┮
  ?:  &(?=(%~ t) ?=(%heavy l) ?=(%light r) ?=(%heavy b))  ~-~2531.  ::  ┱
  ?:  &(?=(%~ t) ?=(%light l) ?=(%heavy r) ?=(%heavy b))  ~-~2532.  ::  ┲
  ?:  ?&  ?=(%~ b)
          ?|  &(?=(%light l) ?=(%light r) ?=(%light t))
              &(?=(%~ l) ?=(%~ r) ?=(%light t) ?=([%h %light] c))
      ==  ==
    ~-~2534.  ::  ┴
  ?:  ?&  ?=(%~ b)
          ?|  &(?=(%light l) ?=(%light r) ?=(%heavy t))
              &(?=(%~ l) ?=(%~ r) ?=(%heavy t) ?=([%h %light] c))
      ==  ==
    ~-~2538.  ::  ┸
  ?:  ?&  ?=(%~ b)
          ?|  &(?=(%light l) ?=(%light r) ?=(%double t))
              &(?=(%~ l) ?=(%~ r) ?=(%double t) ?=([%h %light] c))
      ==  ==
    ~-~2568.  ::  ╨
  ?:  ?&  ?=(%~ b)
          ?|  &(?=(%heavy l) ?=(%heavy r) ?=(%heavy t))
              &(?=(%~ l) ?=(%~ r) ?=(%heavy t) ?=([%h %heavy] c))
      ==  ==
    ~-~253b.  ::  ┻
  ?:  ?&  ?=(%~ b)
          ?|  &(?=(%heavy l) ?=(%heavy r) ?=(%light t))
              &(?=(%~ l) ?=(%~ r) ?=(%light t) ?=([%h %heavy] c))
      ==  ==
    ~-~2537.  ::  ┷
  ?:  ?&  ?=(%~ b)
          ?|  &(?=(%double l) ?=(%double r) ?=(%double t))
              &(?=(%~ l) ?=(%~ r) ?=(%double t) ?=([%h %double] c))
      ==  ==
    ~-~2569.  ::  ╩
  ?:  ?&  ?=(%~ b)
          ?|  &(?=(%double l) ?=(%double r) ?=(%light t))
              &(?=(%~ l) ?=(%~ r) ?=(%light t) ?=([%h %double] c))
      ==  ==
    ~-~2567.  ::  ╧
  ?:  &(?=(%~ b) ?=(%heavy l) ?=(%light r) ?=(%light t))  ~-~2535.  ::  ┵
  ?:  &(?=(%~ b) ?=(%light l) ?=(%heavy r) ?=(%light t))  ~-~2536.  ::  ┶
  ?:  &(?=(%~ b) ?=(%heavy l) ?=(%light r) ?=(%heavy t))  ~-~2539.  ::  ┹
  ?:  &(?=(%~ b) ?=(%light l) ?=(%heavy r) ?=(%heavy t))  ~-~253a.  ::  ┺
  ?:  ?|  &(?=(%light l) ?=(%light r) ?=(%light t) ?=(%light b))
          &(?=(%light l) ?=(%light r) ?=(%~ t) ?=(%~ b) ?=([%v %light] c))
          &(?=(%light t) ?=(%light b) ?=(%~ l) ?=(%~ r) ?=([%h %light] c))
      ==
    ~-~253c.  ::  ┼
  ?:  ?|  &(?=(%heavy l) ?=(%heavy r) ?=(%heavy t) ?=(%heavy b))
          &(?=(%heavy l) ?=(%heavy r) ?=(%~ t) ?=(%~ b) ?=([%v %heavy] c))
          &(?=(%heavy t) ?=(%heavy b) ?=(%~ l) ?=(%~ r) ?=([%h %heavy] c))
      ==
    ~-~254b.  ::  ╋
  ?:  ?|  &(?=(%double l) ?=(%double r) ?=(%double t) ?=(%double b))
          &(?=(%double l) ?=(%double r) ?=(%~ t) ?=(%~ b) ?=([%v %double] c))
          &(?=(%double t) ?=(%double b) ?=(%~ l) ?=(%~ r) ?=([%h %double] c))
      ==
    ~-~256c.  ::  ╬
  ?:  ?|  &(?=(%light l) ?=(%light r) ?=(%heavy t) ?=(%heavy b))
          &(?=(%light l) ?=(%light r) ?=(%~ t) ?=(%~ b) ?=([%v %heavy] c))
          &(?=(%heavy t) ?=(%heavy b) ?=(%~ l) ?=(%~ r) ?=([%h %light] c))
      ==
    ~-~2542.  ::  ╂
  ?:  ?|  &(?=(%heavy l) ?=(%heavy r) ?=(%light t) ?=(%light b))
          &(?=(%heavy l) ?=(%heavy r) ?=(%~ t) ?=(%~ b) ?=([%v %light] c))
          &(?=(%light t) ?=(%light b) ?=(%~ l) ?=(%~ r) ?=([%h %heavy] c))
      ==
    ~-~253f.  ::  ┿
  ?:  ?|  &(?=(%light l) ?=(%light r) ?=(%double t) ?=(%double b))
          &(?=(%light l) ?=(%light r) ?=(%~ t) ?=(%~ b) ?=([%v %double] c))
          &(?=(%double t) ?=(%double b) ?=(%~ l) ?=(%~ r) ?=([%h %light] c))
      ==
    ~-~256b.  ::  ╫
  ?:  ?|  &(?=(%double l) ?=(%double r) ?=(%light t) ?=(%light b))
          &(?=(%double l) ?=(%double r) ?=(%~ t) ?=(%~ b) ?=([%v %light] c))
          &(?=(%light t) ?=(%light b) ?=(%~ l) ?=(%~ r) ?=([%h %double] c))
      ==
    ~-~256a.  ::  ╪
  ?:  ?|  &(?=(%light l) ?=(%heavy r) ?=(%light t) ?=(%light b))
          &(?=(%light l) ?=(%heavy r) ?=(%~ t) ?=(%~ b) ?=([%v %light] c))
      ==
    ~-~253e.  ::  ┾
  ?:  ?|  &(?=(%heavy l) ?=(%light r) ?=(%light t) ?=(%light b))
          &(?=(%heavy l) ?=(%light r) ?=(%~ t) ?=(%~ b) ?=([%v %light] c))
      ==
    ~-~253d.  ::  ┽
  ?:  ?|  &(?=(%heavy l) ?=(%light r) ?=(%heavy t) ?=(%heavy b))
          &(?=(%heavy l) ?=(%light r) ?=(%~ t) ?=(%~ b) ?=([%v %heavy] c))
      ==
    ~-~2549.  ::  ╉
  ?:  ?|  &(?=(%light l) ?=(%heavy r) ?=(%heavy t) ?=(%heavy b))
          &(?=(%light l) ?=(%heavy r) ?=(%~ t) ?=(%~ b) ?=([%v %heavy] c))
      ==
    ~-~254a.  ::  ╊
  ?:  ?|  &(?=(%light t) ?=(%heavy b) ?=(%light l) ?=(%light r))
          &(?=(%light t) ?=(%heavy b) ?=(%~ l) ?=(%~ r) ?=([%h %light] c))
      ==
    ~-~2541.  ::  ╁
  ?:  ?|  &(?=(%heavy t) ?=(%light b) ?=(%light l) ?=(%light r))
          &(?=(%heavy t) ?=(%light b) ?=(%~ l) ?=(%~ r) ?=([%h %light] c))
      ==
    ~-~2540.  ::  ╀
  ?:  ?|  &(?=(%heavy t) ?=(%light b) ?=(%heavy l) ?=(%heavy r))
          &(?=(%heavy t) ?=(%light b) ?=(%~ l) ?=(%~ r) ?=([%h %heavy] c))
      ==
    ~-~2547.  ::  ╇
  ?:  ?|  &(?=(%light t) ?=(%heavy b) ?=(%heavy l) ?=(%heavy r))
          &(?=(%light t) ?=(%heavy b) ?=(%~ l) ?=(%~ r) ?=([%h %heavy] c))
      ==
    ~-~2548.  ::  ╈
  ?:  &(?=(%light l) ?=(%heavy r) ?=(%light t) ?=(%heavy b))  ~-~2546.  ::  ╆
  ?:  &(?=(%light l) ?=(%heavy r) ?=(%heavy t) ?=(%light b))  ~-~2544.  ::  ╄
  ?:  &(?=(%heavy l) ?=(%light r) ?=(%heavy t) ?=(%light b))  ~-~2543.  ::  ╃
  ?:  &(?=(%heavy l) ?=(%light r) ?=(%light t) ?=(%heavy b))  ~-~2545.  ::  ╅
  ~-.
::
++  figo                    :: parse a target character for a line intersection
  |=  c=@c
  ^-  $@(~ (pair via ora))
  ?:  =(~-~2500. c)  [%h %light]
  ?:  =(~-~2501. c)  [%h %heavy]
  ?:  =(~-~2550. c)  [%h %double]
  ?:  =(~-~2502. c)  [%v %light]
  ?:  =(~-~2503. c)  [%v %heavy]
  ?:  =(~-~2551. c)  [%v %double]
  ~
::
++  coeo                    :: apply line intersections
  |=  oz=(list os)
  =;  ints
    |-  ^-  (list [loci nodi])
    ?~  ints  ~
    =/  char=@c  (iugo crux.i.ints)
    ?:  =(~-. char)  $(ints t.ints)
    :-  [loci.i.ints fila.i.ints char]
    $(ints t.ints)
  =/  [zo=(list os) a=viae]  [oz ~]
  |-  ^-  (list [=loci =fila =crux])
  =/  o=(list os)  zo
  ?~  oz  ~(tap by a)
  =/  [hit1=bean hit2=bean]  [| |]
  |-  ^-  (list [=loci =fila =crux])
  ?:  |(?=(~ o) &(hit1 hit2))  ^$(oz t.oz)
  ?:  |(&(?=(%h -.i.oz) ?=(%h -.i.o)) &(?=(%v -.i.oz) ?=(%v -.i.o)))
    $(o t.o)
  =/  v=$@(~ (pair ?(%1 %2) (pair loci nodi)))
    ?-  -.i.oz
        %h
      =/  h1=(unit nodi)  (~(get by visa.i.o) [x1.i.oz y.i.oz])
      ?^  h1  [%1 [[x1.i.oz y.i.oz] u.h1]]
      =/  h2=(unit nodi)  (~(get by visa.i.o) [x2.i.oz y.i.oz])
      ?^  h2  [%2 [[x2.i.oz y.i.oz] u.h2]]  ~
        %v
      =/  v1=(unit nodi)  (~(get by visa.i.o) [x.i.oz y1.i.oz])
      ?^  v1  [%1 [[x.i.oz y1.i.oz] u.v1]]
      =/  v2=(unit nodi)  (~(get by visa.i.o) [x.i.oz y2.i.oz])
      ?^  v2  [%2 [[x.i.oz y2.i.oz] u.v2]]  ~
    ==
  ?~  v  $(o t.o)
  =/  c=$@(~ (pair via ora))  (figo q.q.q.v)
  ?~  c  $(o t.o)
  =/  ac=(unit (pair fila crux))  (~(get by a) p.q.v)
  =/  nc=(pair fila crux)
    ?^  ac
      ?:  ?=(%h -.i.oz)
        ?-(p.v %1 u.ac(r.q ora.i.oz), %2 u.ac(l.q ora.i.oz))
      ?-(p.v %1 u.ac(b.q ora.i.oz), %2 u.ac(t.q ora.i.oz))
    =?  i.o  ?=(%line p.i.o)
      ?-  -.i.o
        %h  i.o(x1 +(x1.i.o), x2 ?:(=(0 x2.i.o) 0 (dec x2.i.o)))
        %v  i.o(y1 +(y1.i.o), y2 ?:(=(0 y2.i.o) 0 (dec y2.i.o)))
      ==
    :+  p.q.q.v  c
    ?-  -.i.oz
        %h
      ?>  ?=(%v -.i.o)
      :+  ?-(p.v %1 %~, %2 ora.i.oz)
        ?-(p.v %1 ora.i.oz, %2 %~)
      ?:  &((gth y.i.oz y1.i.o) (lth y.i.oz y2.i.o))  [ora.i.o ora.i.o]
      ?:  &((gth y.i.oz y1.i.o) =(y.i.oz y2.i.o))     [ora.i.o %~]
      ?:  &((lth y.i.oz y2.i.o) =(y.i.oz y1.i.o))     [%~ ora.i.o]
      [%~ %~]
        %v
      ?>  ?=(%h -.i.o)
      =+  [?-(p.v %1 %~, %2 ora.i.oz) ?-(p.v %1 ora.i.oz, %2 %~)]
      ?:  &((gth x.i.oz x1.i.o) (lth x.i.oz x2.i.o))  [ora.i.o ora.i.o -]
      ?:  &((gth x.i.oz x1.i.o) =(x.i.oz x2.i.o))     [ora.i.o %~ -]
      ?:  &((lth x.i.oz x2.i.o) =(x.i.oz x1.i.o))     [%~ ora.i.o -]
      [%~ %~ -]
    ==
  %=  $
    a     (~(put by a) p.q.v nc)
    hit1  |(hit1 ?=(%1 p.v))
    hit2  |(hit2 ?=(%2 p.v))
    o     t.o
  ==
::
++  viso                    :: take an element and render it 
  |=  [=lar =res =ars lim=modi]
  ^-  visa
  ?+  -.ars    (rbox lar lim res)
    %text      (rtxt lar lim look.res size.res vox.ars)
    %border    (rlin lar lim res ?:(|(?=(%t ad.ars) ?=(%b ad.ars)) %h %v) ora.ars)
    %line      (rlin lar lim res +.ars)
    %input     (rinp lar lim res ab.ars vox.ars)
    %checkbox  (rbox lar lim ?:(v.ars res(b.look f.look.res, f.look b.look.res) res))
    %layer     ~
  ==
::
++  rbox                    :: render a generic box
  |=  [=lar lim=loci =res]
  =|  [w=@ud h=@ud a=visa]
  |-  ^-  visa
  =/  x=@ud  (add x.lar w)
  =/  y=@ud  (add y.lar h)
  =/  nrow=bean  (gte +(w) w.size.res)
  ?:  |((gte h h.size.res) (gth y y.lim))
    a
  %=  $
    w  ?:(nrow 0 +(w))
    h  ?:(nrow +(h) h)
    a  ?:((lte x x.lim) (~(put by a) [x y] [[~ +.look.res] ~-.]) a)
  ==
::
++  rtxt                    :: render a text element
  |=  [=lar lim=loci fil=fila [wid=@ud hei=@ud] =vox]
  =|  [w=@ud h=@ud a=visa spac=visa]
  |-  ^-  visa
  ?~  vox  a
  ?~  i.vox  $(vox t.vox, w 0, h +(h), spac ~)
  =/  [x=@ud y=@ud]  [(add x.lar w) (add y.lar h)]
  ?:  |((gte h hei) (gth y y.lim))  a
  ?:  |((gte w wid) (gth x x.lim))  $(w +(w), i.vox t.i.vox)
  ?:  =(~-. i.i.vox)
    %=  $
      w      +(w)
      i.vox  t.i.vox
      spac   (~(put by spac) [x y] [fil i.i.vox])
    ==
  %=  $
    w      +(w)
    i.vox  t.i.vox
    spac   ~
    a      (~(put by (~(uni by a) spac)) [x y] [fil i.i.vox])
  ==
::
++  rtxb                    :: render basic text (no word wrapping)
  |=  [=lar lim=loci fil=fila [wid=@ud hei=@ud] v=lina]
  =|  [w=@ud h=@ud a=visa]
  |-  ^-  visa
  ?~  v  a
  =/  [x=@ud y=@ud nrow=bean]
    [(add x.lar w) (add y.lar h) (gte +(w) wid)]
  ?:  |((gte h hei) (gth y y.lim))  a
  %=  $
    w  ?:(nrow 0 +(w))
    h  ?:(nrow +(h) h)
    a  ?:((lte x x.lim) (~(put by a) [x y] [fil i.v]) a)
    v  t.v
  ==
::
++  rinp                    :: render an input element
  |=  [=lar lim=loci =res ab=@ud v=vox]
  ^-  visa
  %-  %~  uni
        by
      (rbox lar lim res)
  ?:  =(1 h.size.res)
    ?~  v  ~
    %:  rtxb
      lar  lim  look.res  size.res
      |-  ^-  lina
      ?~  i.v  ~
      ?:  =(0 ab)  i.v
      $(i.v t.i.v, ab (dec ab))
    ==
  %:  rtxt
    lar  lim  look.res  size.res
    |-  ^-  vox
    ?~  v  ~
    ?:  =(0 ab)  v
    $(v t.v, ab (dec ab))
  ==
::
++  rlin                    :: render a line or border element
  |=  [=lar lim=loci =res =via =ora]
  =|  [w=@ud h=@ud a=visa]
  ?:  ?=(%~ ora)  a
  ?:  |(=(0 w.size.res) =(0 h.size.res))  a
  |-  ^-  visa
  =/  x=@ud  (add x.lar w)
  =/  y=@ud  (add y.lar h)
  =/  nrow=bean  (gte +(w) w.size.res)
  ?:  |((gte h h.size.res) (gth y y.lim))  a
  ?:  (gth x x.lim)
    $(w ?:(nrow 0 +(w)), h ?:(nrow +(h) h))
  =/  c=@c
    ?:  ?=(%blank ora)  ~-.
    ?-  via
      %h  ?+(ora ~-~2500. %light ~-~2500., %heavy ~-~2501., %double ~-~2550.)  ::  ─  ━  ═
      %v  ?+(ora ~-~2502. %light ~-~2502., %heavy ~-~2503., %double ~-~2551.)  ::  │  ┃  ║
    ==
  %=  $
    w  ?:(nrow 0 +(w))
    h  ?:(nrow +(h) h)
    a  (~(put by a) [x y] [look.res c])
  ==
::
++  geno                    :: turn sail into session state
  |=  [x=manx =ara]
  ^-  ^ara
  =.  x.urbs  ?:(=(0 x.urbs) 0 (dec x.urbs))
  =/  old=(unit ens)  (~(get by esse.ara) ~[[%~ 0]])
  =/  a=opus                   [~ ~]
  =/  m=marl                   ~[x]
  =/  k=rami                   ~[[%~ 0]]
  =/  pl=fila                  [~ ~ %w]
  =/  px=as                    [%c x.urbs]
  =/  py=as                    [%c y.urbs]
  =/  pow=fuga                 [%row %clip]
  =/  prx=@ud                  x.urbs
  =/  pry=@ud                  y.urbs
  =/  plar=lar                 ?^(old lar.u.old [1 1])
  =/  plim=modi                [x.urbs y.urbs]
  =/  pitr=iter                [0 0]
  =/  pscr=[x=bean y=bean]     [%.n %.n]
  =/  slim=$@(~ loci)          ~
  =/  vlar=lar                 plar
  =/  vir=[n=@ud o=@ud i=@ud]  [0 0 0]
  =;  =opus
    :+  x
      esse.opus
    (dico esse.opus ?^(rex.ara k.rex.ara ~))
  |-  ^-  opus
  ?~  m  a
  =/  [=ovum =aves =ars =lina velo=mart]
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
  =/  [bl=@ud br=@ud bt=@ud bb=@ud]
    (obeo bor)
  =^  [gro=marl aqu=aqua]  nor
    (sero flow.ovum nor)
  =/  wcen=bean  =(%p p.w.size.ovum)
  =/  hcen=bean  =(%p p.h.size.ovum)
  =?  w.size.ovum  wcen
    ?:  &(=(%i p.px) ?=(%layer -.ars))
      [%i 0]
    [%c (div (mul q.w.size.ovum prx) 100)]
  =?  h.size.ovum  hcen
    ?:  &(=(%i p.py) ?=(%layer -.ars))
      [%i 0]
    [%c (div (mul q.h.size.ovum pry) 100)]
  =?  t.marg.ovum  =(%p p.t.marg.ovum)
    ?:  |(=(%i p.h.size.ovum) =(%p p.h.size.ovum))
      [%c 0]
    [%c (div (mul q.t.marg.ovum q.h.size.ovum) 100)]
  =?  r.marg.ovum  =(%p p.r.marg.ovum)
    ?:  |(=(%i p.w.size.ovum) =(%p p.w.size.ovum))
      [%c 0]
    [%c (div (mul q.r.marg.ovum q.w.size.ovum) 100)]
  =?  b.marg.ovum  =(%p p.b.marg.ovum)
    ?:  |(=(%i p.h.size.ovum) =(%p p.h.size.ovum))
      [%c 0]
    [%c (div (mul q.b.marg.ovum q.h.size.ovum) 100)]
  =?  l.marg.ovum  =(%p p.l.marg.ovum)
    ?:  |(=(%i p.w.size.ovum) =(%p p.w.size.ovum))
      [%c 0]
    [%c (div (mul q.l.marg.ovum q.w.size.ovum) 100)]
  =?  t.padd.ovum  =(%p p.t.padd.ovum)
    ?:  |(=(%i p.h.size.ovum) =(%p p.h.size.ovum))
      [%c 0]
    [%c (div (mul q.t.padd.ovum q.h.size.ovum) 100)]
  =?  r.padd.ovum  =(%p p.r.padd.ovum)
    ?:  |(=(%i p.w.size.ovum) =(%p p.w.size.ovum))
      [%c 0]
    [%c (div (mul q.r.padd.ovum q.w.size.ovum) 100)]
  =?  b.padd.ovum  =(%p p.b.padd.ovum)
    ?:  |(=(%i p.h.size.ovum) =(%p p.h.size.ovum))
      [%c 0]
    [%c (div (mul q.b.padd.ovum q.h.size.ovum) 100)]
  =?  l.padd.ovum  =(%p p.l.padd.ovum)
    ?:  |(=(%i p.w.size.ovum) =(%p p.w.size.ovum))
      [%c 0]
    [%c (div (mul q.l.padd.ovum q.w.size.ovum) 100)]
  =?  q.w.size.ovum  &(wcen !=(%i p.w.size.ovum))
    =/  m=@ud  (add q.l.marg.ovum q.r.marg.ovum)
    ?:  (gth m q.w.size.ovum)  0  (sub q.w.size.ovum m)
  =?  q.h.size.ovum  &(hcen !=(%i p.h.size.ovum))
    =/  m=@ud  (add q.t.marg.ovum q.b.marg.ovum)
    ?:  (gth m q.h.size.ovum)  0  (sub q.h.size.ovum m)
  =?  x.flex.ovum  =(%i p.w.size.ovum)  0
  =?  y.flex.ovum  =(%i p.h.size.ovum)  0
  =/  imp=bean
    ?&  !?=(%text -.ars)
        ?|  =(%i p.w.size.ovum)
            =(%i p.h.size.ovum)
    ==  ==
  =/  fex=bean
    ?|  &(!=(0 x.flex.ovum) =(%c p.w.size.ovum)) 
        &(!=(0 y.flex.ovum) =(%c p.h.size.ovum))
    ==
  =/  wrap=bean
    ?&  !?=(%border -.ars)
        ?|  ?&  =([%row %wrap] pow)  =(%c p.w.size.ovum)
                %+  gth  (add q.w.size.ovum (add q.l.marg.ovum q.r.marg.ovum))
                ?:((lte n.vir prx) (sub prx n.vir) 0)
            ==
            ?&  =([%col %wrap] pow)  =(%c p.h.size.ovum)
                %+  gth  (add q.h.size.ovum (add q.t.marg.ovum q.b.marg.ovum))
                ?:((lte n.vir pry) (sub pry n.vir) 0)
    ==  ==  ==
  =/  wrim=bean
    ?&  !?=(%border -.ars)
        ?|  &(=([%row %wrap] pow) =(%i p.w.size.ovum))
            &(=([%col %wrap] pow) =(%i p.h.size.ovum))
    ==  ==
  =/  tvir=[n=@ud o=@ud i=@ud]  vir
  =?  vir  |(wrap wrim)
    ?-  d.pow
        %row
      ?:  wrap
        :-  0
        :-  i.vir
        ;:(add q.h.size.ovum q.t.marg.ovum q.b.marg.ovum i.vir)
      ?:  wrim
        :-  0
        :-  o.vir
        i.vir
      vir
        %col
      ?:  wrap
        :-  0
        :-  i.vir
        ;:(add q.w.size.ovum q.l.marg.ovum q.r.marg.ovum i.vir)
      ?:  wrim
        :-  0
        :-  o.vir
        i.vir
      vir
    ==
  =.  vlar
    ?:  ?=(%border -.ars)
      ?-  ad.ars
          %l
        vlar
          %r
        :_  y.vlar
        =/  x=@ud  (add x.vlar ?:(=(0 q.px) 0 (dec q.px)))
        =/  w=@ud  ?:(=(0 q.w.size.ovum) 0 (dec q.w.size.ovum))
        ?:((lth w x) (sub x w) 1)
          %t
        vlar
          %b
        :-  x.vlar
        =/  y=@ud  (add y.vlar ?:(=(0 q.py) 0 (dec q.py)))
        =/  h=@ud  ?:(=(0 q.h.size.ovum) 0 (dec q.h.size.ovum))
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
    :-  (add x.vlar q.l.marg.ovum)
    (add y.vlar q.t.marg.ovum)
  =/  alar=lar  vlar
  =.  vlar
    :-  ;:(add bl q.l.padd.ovum x.vlar)
    ;:(add bt q.t.padd.ovum y.vlar)
  =/  arx=@ud
    ?+  p.w.size.ovum  0
        %c
      =/  w=@ud  ;:(add bl br q.l.padd.ovum q.r.padd.ovum)
      ?:((gth w q.w.size.ovum) 0 (sub q.w.size.ovum w))
        %i
      =/  w=@ud
        ;:  add
          q.l.marg.ovum  ?:(=(%row d.pow) n.vir o.vir)
          bl  br  q.l.padd.ovum  q.r.padd.ovum
        ==
      ?:((gth w prx) 0 (sub prx w))
    ==
  =/  ary=@ud
    ?+  p.h.size.ovum  0
        %c
      =/  h=@ud  ;:(add bt bb q.t.padd.ovum q.b.padd.ovum)
      ?:((gth h q.h.size.ovum) 0 (sub q.h.size.ovum h))
        %i
      =/  h=@ud
        ;:  add 
          q.t.marg.ovum  ?:(=(%row d.pow) o.vir n.vir)
          bt  bb  q.t.padd.ovum  q.b.padd.ovum
        ==
      ?:((gth h pry) 0 (sub pry h))
    ==
  =/  alim=loci
    :-  ?:  =(0 arx)  0
        ?:  &(x.pscr ?=(%c p.w.size.ovum))
          ;:(add x.alar bl q.l.padd.ovum ?:(=(0 arx) 0 (dec arx)))
        =/  x=@ud  ;:(add x.alar bl q.l.padd.ovum ?:(=(0 arx) 0 (dec arx)))
        ?:  (gth x x.plim)
          x.plim
        x
    ?:  =(0 ary)  0
    ?:  &(y.pscr ?=(%c p.h.size.ovum))
      ;:(add y.alar bt q.t.padd.ovum ?:(=(0 ary) 0 (dec ary)))
    =/  y=@ud  ;:(add y.alar bt q.t.padd.ovum ?:(=(0 ary) 0 (dec ary)))
    ?:  (gth y y.plim)
      y.plim
    y
  =/  nscr=[x=bean y=bean]
    :-  |(?=(%scroll -.ars) &(x.pscr ?=(%i p.w.size.ovum)))
    |(?=(%scroll -.ars) &(y.pscr ?=(%i p.h.size.ovum)))
  =/  nsli=$@(~ modi)
    ?.  |(x.nscr y.nscr)
      slim
    ?:  ?=(~ slim)
      alim
    :-  ?:((lth x.alim x.slim) x.alim x.slim)
    ?:((lth y.alim y.slim) y.alim y.slim)
  =/  fi=fila
    :+  ?~(d.look.ovum d.pl u.d.look.ovum)
      ?~(b.look.ovum b.pl u.b.look.ovum)
    ?~(f.look.ovum f.pl u.f.look.ovum)
  =|  [b=opus c=opus]
  =>  ?.  ?=(^ lay)
      .
    ?.  wrim
      %_    .
          a
        %=  $
          m     lay
          k     [[%l 0] k]
          px    w.size.ovum
          py    h.size.ovum
          pl    fi
          pow   flow.ovum
          prx   arx
          pry   ary
          plar  vlar
          plim  alim
          pscr  nscr
          slim  nsli
          vir   [0 0 0]
      ==  ==
    %_    .
        b
      %=  $
        a     b
        m     lay
        k     [[%l 0] k]
        px    w.size.ovum
        py    h.size.ovum
        pl    fi
        pow   flow.ovum
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
    ?:  |(fex ?=(^ gro))
      %_    .
          c
        %=  $
          a     c
          m     nor
          k     [[%~ 0] k]
          px    w.size.ovum
          py    h.size.ovum
          pl    fi
          pow   flow.ovum
          prx   arx
          pry   ary
          plar  vlar
          plim  alim
          pscr  nscr
          slim  nsli
          vir   [0 0 0]
      ==  ==
    ?:  wrim
      %_    .
          b
        %=  $
          a     b
          m     nor
          k     [[%~ 0] k]
          px    w.size.ovum
          py    h.size.ovum
          pl    fi
          pow   flow.ovum
          prx   arx
          pry   ary
          plar  vlar
          plim  alim
          pscr  nscr
          slim  nsli
          vir   [0 0 0]
      ==  ==
    %_    .
        a
      %=  $
        m     nor
        k     [[%~ 0] k]
        px    w.size.ovum
        py    h.size.ovum
        pl    fi
        pow   flow.ovum
        prx   arx
        pry   ary
        plar  vlar
        plim  alim
        pscr  nscr
        slim  nsli
        vir   [0 0 0]
    ==  ==
  =/  csiz=$@(~ [w=@ud h=@ud])
    ?.  |(fex imp ?=(^ gro) ?=(%scroll -.ars))  ~
    (cogo vlar k ?:(|(fex ?=(^ gro)) esse.c ?:(wrim esse.b esse.a)))
  =?  aqu  !=(~ aqu)
    =/  len=@ud  (lent aqu)
    =/  rom=@ud
      ?:  ?=(%wrap b.flow.ovum)  0
      ?-  d.flow.ovum
        %row  ?:(?=(~ csiz) arx ?:((lte w.csiz arx) (sub arx w.csiz) 0))
        %col  ?:(?=(~ csiz) ary ?:((lte h.csiz ary) (sub ary h.csiz) 0))
      ==
    ?:  =(0 rom)  aqu
    =/  bas=@ud  (div rom len)
    =/  rem=@ud  (mod rom len)
    |-  ^-  aqua
    ?~  aqu  ~
    :-  i.aqu(size ?:(=(0 rem) bas +(bas)))
    $(aqu t.aqu, rem ?:(=(0 rem) 0 (dec rem)))
  =^  opc=opus  aqu
    ?~  gro  [[~ ~] ~]
    =/  ek=rami  [[%~ 0] k]
    =|  [i=@ud marg=@ud move=@ud op=opus aq=aqua]
    |-  ^-  [opus aqua]
    ?:  &(?=(^ aqu) =(i i.i.aqu))
      %=  $
        i     +(i)
        aq    [i.aqu(marg marg) aq]
        aqu   t.aqu
        move  (add move size.i.aqu)
        marg  0
      ==
    =/  el=(unit ens)  (~(get by esse.c) ek)
    ?~  el  [op (flop aq)]
    =?  u.el  !=(0 move)
      %:  mino
        ?-(d.flow.ovum %row move, %col 0)  ?-(d.flow.ovum %row 0, %col move)
        alim  ?:(fex ~ ?:(wrim visa.b visa.a))  u.el
      ==
    =:  esse.op  (~(put by esse.op) ek u.el)
        visa.op  (~(uni by visa.u.el) visa.op)
      ==
    %=  $
      i          +(i)
      ager.i.ek  +(ager.i.ek)
      marg
        ?-  d.flow.ovum
          %row  ;:(add marg l.marg.res.u.el r.marg.res.u.el w.size.res.u.el)
          %col  ;:(add marg t.marg.res.u.el b.marg.res.u.el h.size.res.u.el)
        ==
      op
        =/  cek=rami  [[%b 0] ek]
        |-  ^-  opus
        =/  cel=(unit ens)  (~(get by esse.c) cek)
        ?~  cel
          ?:  ?=(%b axis.i.cek)  $(cek [[%l 0] t.cek])
          ?:  ?=(%l axis.i.cek)  $(cek [[%~ 0] t.cek])
          op
        =?  u.cel  !=(0 move)
          %:  mino
            ?-(d.flow.ovum %row move, %col 0)  ?-(d.flow.ovum %row 0, %col move)
            alim  ?:(fex ~ ?:(wrim visa.b visa.a))  u.cel
          ==
        =:  esse.op  (~(put by esse.op) cek u.cel)
            visa.op  (~(uni by visa.u.cel) visa.op)
          ==
        %=  $
          ager.i.cek  +(ager.i.cek)
          op          $(cek [[%b 0] cek])
        ==
    ==
  =?  c  !?=([~ ~] opc)  opc
  =?  gro  !=(~ gro)
    |-  ^-  marl
    ?:  |(?=(~ gro) ?=(~ aqu))  ~
    :_  $(gro t.gro, aqu t.aqu)
    %_    i.gro
        a.g   
      %+  weld  a.g.i.gro
      ^-  mart
      ?-  d.flow.ovum
          %row
        :~  [%w (trip (scot %ud size.i.aqu))]
            [%ml (trip (scot %ud marg.i.aqu))]
            [%mr "0"]
        ==
          %col
        :~  [%h (trip (scot %ud size.i.aqu))]
            [%mt (trip (scot %ud marg.i.aqu))]
            [%mb "0"]
        ==
      ==
    ==
  =>  ?:  =(~ gro)  .
    %_    .
        c
      %=  $
        a     c
        m     gro
        k     [[%~ (lent nor)] k]
        px    w.size.ovum
        py    h.size.ovum
        pl    fi
        pow   flow.ovum
        prx   arx
        pry   ary
        plar  vlar
        plim  alim
        pscr  nscr
        slim  nsli
        vir   [0 0 0]
    ==  ==
  =?  csiz  &(?=(^ gro) |(fex imp ?=(%scroll -.ars)))
    (cogo vlar k esse.c)
  =?  c  fex
    =/  [ek=rami op=opus]  [[[%~ 0] k] [~ ~]]
    =/  wra
      =|  [out=@ud siz=@ud acc=(map @ud @ud)]
      ?:  ?=(%clip b.flow.ovum)  acc
      |-  ^-  (map @ud @ud)
      =/  el=(unit ens)  (~(get by esse.c) ek)
      ?~  el  ?:(=(0 siz) acc (~(put by acc) out siz))
      =/  nut=@ud
        ?-  d.flow.ovum
          %row  ?:((lth t.marg.res.u.el y.lar.u.el) (sub y.lar.u.el t.marg.res.u.el) 1)
          %col  ?:((lth l.marg.res.u.el x.lar.u.el) (sub x.lar.u.el l.marg.res.u.el) 1)
        ==
      =/  niz=@ud
        ?-  d.flow.ovum
          %row  (add w.size.res.u.el (add l.marg.res.u.el r.marg.res.u.el))
          %col  (add h.size.res.u.el (add t.marg.res.u.el b.marg.res.u.el))
        ==
      ?:  =(out nut)
        $(ager.i.ek +(ager.i.ek), siz (add siz niz))
      %=  $
        ager.i.ek  +(ager.i.ek)
        acc        (~(put by acc) out siz)
        out        nut
        siz        niz
      ==
    |-  ^-  opus
    =/  el=(unit ens)  (~(get by esse.c) ek)
    ?~  el  op
    =/  movx=@ud
      =;  x=@ud
        ?:  (gte x arx)  0
        (div (mul x.flex.ovum (sub arx x)) 100)
      ?:  |(?=([%row %clip] flow.ovum) ?=([%col %wrap] flow.ovum))
        ?^(csiz w.csiz 0)
      ?:  ?=([%col %clip] flow.ovum)
        (add w.size.res.u.el (add l.marg.res.u.el r.marg.res.u.el))
      ?:  ?=([%row %wrap] flow.ovum)
        =/  w=(unit @ud)
          %-  %~  get  by  wra
          ?:((lth t.marg.res.u.el y.lar.u.el) (sub y.lar.u.el t.marg.res.u.el) 1)
        ?^(w u.w 0)
      0
    =/  movy=@ud
      =;  y=@ud
        ?:  (gte y ary)  0
        (div (mul y.flex.ovum (sub ary y)) 100)
      ?:  |(?=([%col %clip] flow.ovum) ?=([%row %wrap] flow.ovum))     
        ?^(csiz h.csiz 0)
      ?:  ?=([%row %clip] flow.ovum)
        (add h.size.res.u.el (add t.marg.res.u.el b.marg.res.u.el))
      ?:  ?=([%col %wrap] flow.ovum)
        =/  h=(unit @ud)
          %-  %~  get  by  wra
          ?:((lth l.marg.res.u.el x.lar.u.el) (sub x.lar.u.el l.marg.res.u.el) 1)
        ?^(h u.h 0)
      0
    =?  u.el  !&(=(0 movx) =(0 movy))
      (mino movx movy alim ?:(wrim visa.b visa.a) u.el)
    =:  esse.op  (~(put by esse.op) ek u.el)
        visa.op  (~(uni by visa.u.el) visa.op)
      ==
    %=  $
      ager.i.ek  +(ager.i.ek)
      op
        =/  cek=rami  [[%b 0] ek]
        |-  ^-  opus
        =/  cel=(unit ens)  (~(get by esse.c) cek)
        ?~  cel
          ?:  ?=(%b axis.i.cek)  $(cek [[%l 0] t.cek])
          ?:  ?=(%l axis.i.cek)  $(cek [[%~ 0] t.cek])
          op
        =?  u.cel  !&(=(0 movx) =(0 movy))
          (mino movx movy alim ?:(wrim visa.b visa.a) u.cel)
        =:  esse.op  (~(put by esse.op) cek u.cel)
            visa.op  (~(uni by visa.u.cel) visa.op)
          ==
        %=  $
          ager.i.cek  +(ager.i.cek)
          op          $(cek [[%b 0] cek])
        ==
    ==
  =?  b  &(!?=([~ ~] c) wrim)
    [(~(uni by esse.c) esse.b) (~(uni by visa.c) visa.b)]
  =?  a  !&(?=([~ ~] c) wrim)
    [(~(uni by esse.c) esse.a) (~(uni by visa.c) visa.a)]
  =?  csiz  &(!?=([~ ~] c) |(imp ?=(%scroll -.ars)))
    (cogo vlar k ?:(wrim esse.b esse.a))
  =?  size.ovum  &(imp ?=(^ csiz))
    :-  ?:  =(%i p.w.size.ovum)  
          [%c ;:(add bl br q.l.padd.ovum q.r.padd.ovum w.csiz)]
        w.size.ovum
    ?:  =(%i p.h.size.ovum)  
      [%c ;:(add bt bb q.t.padd.ovum q.b.padd.ovum h.csiz)]
    h.size.ovum
  =/  wris=bean
    ?&  wrim
        ?|  (gth n.vir prx)  (gth n.vir pry)
            ?&  =(%row d.pow)
                %+  gth  (add q.w.size.ovum (add q.l.marg.ovum q.r.marg.ovum))
                ?:((lte n.tvir prx) (sub prx n.tvir) 0)
            ==
            ?&  =(%col d.pow)
                %+  gth  (add q.h.size.ovum (add q.t.marg.ovum q.b.marg.ovum))
                ?:((lte n.tvir pry) (sub pry n.tvir) 0)
    ==  ==  ==
  =?  vir  wrim
    ?:  wris
      ?-  d.pow
          %row
        :-  0
        :-  i.vir
        (add i.vir (add q.h.size.ovum (add q.t.marg.ovum q.b.marg.ovum)))
          %col
        :-  0
        :-  i.vir
        (add i.vir (add q.w.size.ovum (add q.l.marg.ovum q.r.marg.ovum)))
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
    =/  bp=@ud  ;:(add bl br q.l.padd.ovum q.r.padd.ovum)
    ?:((gth bp q.w.size.ovum) 0 (sub q.w.size.ovum bp))
  =?  ary  wrim
    =/  bp=@ud  ;:(add bt bb q.t.padd.ovum q.b.padd.ovum)
    ?:((gth bp q.h.size.ovum) 0 (sub q.h.size.ovum bp))
  =?  alim  |(wrim x.pscr y.pscr)
    :-  ?:  x.pscr
          =/  r=@ud  +((add br q.r.padd.ovum))
          ;:(add x.alar bl q.l.padd.ovum ?:((gth r q.w.size.ovum) 0 (sub q.w.size.ovum r)))
        ?:  wrim
          =/  x=@ud  (add x.vlar ?:(=(0 arx) 0 (dec arx)))
          ?:  (gth x x.plim)
            x.plim
          x
        x.alim
    ?:  y.pscr
      =/  b=@ud  +((add bb q.b.padd.ovum))
      ;:(add y.alar bt q.t.padd.ovum ?:((gth b q.h.size.ovum) 0 (sub q.h.size.ovum b)))
    ?:  wrim
      =/  y=@ud  (add y.vlar ?:(=(0 ary) 0 (dec ary)))
      ?:  (gth y y.plim)
        y.plim
      y
    y.alim
  =?  a  wrim
    =/  movx=@ud
      ?:  &(wris ?=(%col d.pow))
        (sub i.tvir o.tvir)
      ?:  &(!wris ?=(%row d.pow))
        n.tvir
      0
    =/  movy=@ud
      ?:  &(wris ?=(%row d.pow))
        (sub i.tvir o.tvir)
      ?:  &(!wris ?=(%col d.pow))
        n.tvir
      0
    =/  ek=rami  [[%l 0] k]
    |-  ^-  opus
    =/  el=(unit ens)  (~(get by esse.b) ek)
    ?~  el
      ?:  ?=(%b axis.i.ek)  $(ek [[%l 0] t.ek])
      ?:  ?=(%l axis.i.ek)  $(ek [[%~ 0] t.ek])
      a
    =.  u.el  (mino movx movy alim visa.a u.el)
    %=  $
      ager.i.ek  +(ager.i.ek)
      a
        %=  $
          ek  [[%b 0] ek]
          a  [(~(put by esse.a) ek u.el) (~(uni by visa.u.el) visa.a)]
    ==  ==
  =?  ars  ?=(%text -.ars)
    :-  -.ars
    =/  [x=@ud y=@ud]
      :-  ?:(?=(%row d.pow) n.vir o.vir)
      ?:(?=(%col d.pow) n.vir o.vir)
    %^  oro  ?:((lte x prx) (sub prx x) 0)
      ?:((lte y pry) (sub pry y) 0)
    lina
  =/  ares=res
    ?+  -.ars
      :*  [q.w.size.ovum q.h.size.ovum]
          [q.l.padd.ovum q.r.padd.ovum q.t.padd.ovum q.b.padd.ovum]
          [q.l.marg.ovum q.r.marg.ovum q.t.marg.ovum q.b.marg.ovum]
          flex.ovum
          flow.ovum
          fi
      ==
        %text
      =/  len=@ud
        (roll ^-(vox vox.ars) |=([i=^lina a=@ud] =/(l=@ud (pono i) ?:((lth a l) l a))))
      =/  lim=@ud  (sub prx ?:(?=(%row d.pow) n.vir o.vir))
      :*  [?:((lth len lim) len lim) (lent vox.ars)]
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
      px    w.size.ovum
      py    h.size.ovum
      pl    fi
      pow   flow.ares
      prx   w.size.ares
      pry   h.size.ares
      plar  alar
      plim  =?  plim  |(x.pscr y.pscr)
              :-  ?:(x.pscr (add x.alar ?:(=(0 w.size.ares) 0 (dec w.size.ares))) x.plim)
              ?:(y.pscr (add y.alar ?:(=(0 h.size.ares) 0 (dec h.size.ares))) y.plim)
            :-  ?:  =(0 arx)  0
                =/  x=@ud  ;:(add x.alim bl br l.padd.ares r.padd.ares)
                ?:((gth x x.plim) x.plim x)
            ?:  =(0 ary)  0
            =/  y=@ud  ;:(add y.alim bt bb t.padd.ares b.padd.ares)
            ?:((gth y y.plim) y.plim y)
      pscr  nscr
      vlar  alar
      vir   [0 0 0]
    ==
  =?  ars  |(?=(%input -.ars) ?=(%scroll -.ars) ?=(%checkbox -.ars))
    ?:  ?=(%input -.ars)
      =/  old=(unit ens)  (~(get by esse.ara) k)
      ?~  old  ars
      ?.  ?=(%input -.ars.u.old)  ars
      ?:  =(size.res.u.old size.ares)
        ars.u.old
      %_  ars.u.old
        ab   0
        i    [0 0]
        vox  (oro w.size.ares h.size.ares ^-(^lina (zing vox.ars.u.old)))
      ==
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
  =.  vir
    ?:  |(?=(%layer -.ars) ?=(%border -.ars))  [0 0 0]
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
  =.  vlar
    ?:  |(?=(%layer -.ars) ?=(%border -.ars))  plar
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
++  dico                    :: derive intersections, hotkeys, and navigation context from display state
  |=  [e=esse rk=$@(~ rami)]
  ^-  cura
  =/  k=rami                      [[%~ 0] ~]
  =/  plim=modi                   [x.urbs y.urbs]
  =/  acc=[rend=bean =ossa cura]  [%.n ~ *cura]
  =;  dic
    =.  omen.dic
      ?:  ?=(^ rex.dic)
        =/  el=(unit ens)  (~(get by e) k.rex.dic)
        ?:  &(?=(^ el) ?=(%input -.ars.u.el))
          hinp
        hnav
      ?:  ?=(^ ordo.dic)
        hnav
      ~
    ?~  ossa.dic  +>.dic
    %_  +>.dic
      visa  (~(gas by visa.dic) (coeo ~(tap in ^-(ossa ossa.dic))))
    ==
  |-  ^-  [bean =ossa cura]
  =/  el=(unit ens)  (~(get by e) k)
  ?~  el
    ?:  ?=(%l axis.i.k)  $(k [[%b 0] t.k])
    ?:  ?=(%b axis.i.k)  $(k [[%~ 0] t.k])
    acc
  =/  nacc=[rend=bean =ossa cura]
    %=  $
      k     [[%l 0] k]
      acc   [| ~ =+(*cura -(rex rex.acc))]
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
    ?.  nav  (weld ordo.nacc ordo.acc)
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
        ^-((list [loci nodi]) ~(tap by visa.nacc))
      |=([loc=loci nodi] [loc k])
    (~(uni by mus.acc) mus.nacc)
  =.  equi.acc 
    ?.  ?=(%scroll -.ars.u.el)
      (~(uni by equi.acc) equi.nacc)
    (~(put in (~(uni by equi.acc) equi.nacc)) k)
  =.  visa.acc  (~(uni by visa.nacc) visa.acc)
  =?  ossa.acc  ?=(^ ossa.nacc)  (~(uni in ossa.acc) ^-(ossa ossa.nacc))
  =?  ossa.acc
      ?|  &(?=(%border -.ars.u.el) !|(?=(%~ ora.ars.u.el) ?=(%blank ora.ars.u.el)))
          &(?=(%line -.ars.u.el) !|(?=(%~ ora.ars.u.el) ?=(%blank ora.ars.u.el)))
      ==
    %-  %~  put
          in
        ossa.acc
    =/  x=@ud  ?:((lte x.iter.u.el x.lar.u.el) (sub x.lar.u.el x.iter.u.el) 0)
    =/  y=@ud  ?:((lte y.iter.u.el y.lar.u.el) (sub y.lar.u.el y.iter.u.el) 0)
    ?:  ?|  &(?=(%line -.ars.u.el) ?=(%h via.ars.u.el))
            &(?=(%border -.ars.u.el) |(?=(%t ad.ars.u.el) ?=(%b ad.ars.u.el)))
        ==
      =/  xend=@ud  (add x.lar.u.el w.size.res.u.el)
      =?  xend  (lte x.iter.u.el xend)  (sub xend x.iter.u.el)
      ?+  -.ars.u.el  !!
        %border  [%h -.ars.u.el x ?:(=(0 xend) 0 (dec xend)) y ora.ars.u.el visa.u.el]
        %line    [%h -.ars.u.el ?:(=(0 x) 0 (dec x)) xend y ora.ars.u.el visa.u.el]
      ==
    =/  yend=@ud  (add y.lar.u.el h.size.res.u.el)
    =?  yend  (lte y.iter.u.el yend)  (sub yend y.iter.u.el)
    ?+  -.ars.u.el  !!
      %border  [%v -.ars.u.el x y ?:(=(0 yend) 0 (dec yend)) ora.ars.u.el visa.u.el]
      %line    [%v -.ars.u.el x ?:(=(0 y) 0 (dec y)) yend ora.ars.u.el visa.u.el]
    ==
  ?:  ?=(%l axis.i.k)
    %=  $
      ager.i.k  +(ager.i.k)
      rend.acc  |(rend.acc rend.nacc)
      ossa.acc  ~
      visa.acc
        ?~  ossa.acc  visa.acc
        (~(gas by visa.acc) (coeo ~(tap in ^-(ossa ossa.acc))))
    ==
  %=  $
    ager.i.k  +(ager.i.k)
    rend.acc  |(rend.acc rend.nacc)
  ==
::  ::  ::  ::  ::  ::  ::  ::
++  supo                    :: make a full display update
  |=  v=visa
  ^-  lux
  =/  y=@ud  1
  :-  %mor
  |-  ^-  (list lux)
  =/  nod=(list nodi)
    =/  x=@ud  1
    |-  ^-  (list nodi)
    ?:  (gte x x.urbs)
      ~
    =/  val=(unit nodi)  (~(get by v) [x y])
    [?~(val [[~ ~ ~] ~-x] u.val) $(x +(x))]
  :+  [%hop 1 y]
    [%klr nod] 
  ?:(=(y y.urbs) ~ $(y +(y)))
::
++  dono                    :: make a display update diff
  |=  [old=visa new=visa]
  ^-  lux
  =/  n=(list [=loci nodi])  ~(tap by new)
  :-  %mor
  |-  ^-  (list lux)
  ?~  n  ~
  =/  v=(unit nodi)  (~(get by old) loci.i.n)
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
  =/  prev=fila  [~ ~ ~]
  |-  ^-  tape
  ?~  p.lux
    ?.  =([~ ~ ~] prev)
      :^  '\\'  'e'  '['
      :+  '0'  'm'  ~
    ~
  ?-  -.i.p.lux
      %mor
    $(p.lux ^-((list ^lux) (weld p.i.p.lux t.p.lux)))
      %hop
    ?@  p.i.p.lux  $(p.lux t.p.lux)
    :*  '\\'  'e'  '['
        (scot %ud y.p.i.p.lux)  ';'
        (scot %ud x.p.i.p.lux)  'H'
        $(p.lux t.p.lux)
    ==
      %klr
    |-  ^-  tape
    ?~  p.i.p.lux  ^$(p.lux t.p.lux)
    =/  newb=(unit tint)  ?:(=(b.prev b.p.i.p.i.p.lux) ~ [~ b.p.i.p.i.p.lux])
    =/  newf=(unit tint)  ?:(=(f.prev f.p.i.p.i.p.lux) ~ [~ f.p.i.p.i.p.lux])
    =/  oldd=(list deco)  ~(tap in (~(dif in d.prev) d.p.i.p.i.p.lux))
    =/  newd=(list deco)  ~(tap in (~(dif in d.p.i.p.i.p.lux) d.prev))
    =/  newt=@t  ?:((lte q.i.p.i.p.lux 127) ^-(@ q.i.p.i.p.lux) (tuft q.i.p.i.p.lux))
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
    :-  newt
    ^$(p.i.p.lux t.p.i.p.lux, prev p.i.p.i.p.lux)
  ==
--
