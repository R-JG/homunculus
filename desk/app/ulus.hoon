::
::  O┬┬ ┬┌─┐┌┬┐┬ ┬┌┐┌┌─┐┬ ┬┬  ┬ ┬┌─┐
::  ┌┘├─┤│ │││││ │││││  │ ││  │ │└─┐
::  ┴O┴ ┴└─┘┴ ┴└─┘┘└┘└─┘└─┘┴─┘└─┘└─┘
::
/-  homunculus
|%
+$  deus  [=cor =gens]                                                 :: an element
+$  dei   (list deus)                                                  :: elements
+$  gens  $~(~^~^~ [b=dei l=dei n=dei])                                :: elements within an element
+$  cor   [=apex =avis =res =ars]                                      :: attributes of an element
+$  res                                                                :: perceptible attributes of an element
  $:  size=[w=@ud h=@ud]                                               :: size
      padd=muri                                                        :: padding
      marg=muri                                                        :: margin
      bord=muri                                                        :: border sizes
      flex=modi                                                        :: child element positioning: justify
      flow=fuga                                                        :: child element positioning: sequence
      look=fila                                                        :: style
      sele=acia                                                        :: select style
  ==                                                                   ::
+$  ars                                                                :: special element types
  $%  [%text =vox]                                                     ::
      [%pattern =vox]                                                  ::
      [%layer ~]                                                       ::
      [%scroll =iter =sola]                                            ::
      [%border =ad =ora]                                               ::
      [%line =via =ora]                                                ::
      [%select pro=?(%submit %~)]                                      ::
      [%input ab=@ud i=loci =vox]                                      ::
      [%checkbox v=bean]                                               ::
      [%radio ~]                                                       ::
      [%form ~]                                                        ::
      [%$ ~]                                                           ::
  ==                                                                   ::
+$  avis  (unit @t)                                                    :: element id
+$  apex  $~([1 1] loci)                                               :: top left coordinate of an element
+$  loci  [x=@ud y=@ud]                                                :: coordinates
+$  modi  [x=@ud y=@ud]                                                :: box extent
+$  muri  [l=@ud r=@ud t=@ud b=@ud]                                    :: box sides
+$  rami  (list [=axis =ager])                                         :: element key (null is root)
+$  axis  ?(%n %l %b)                                                  :: element positioning category
+$  ager  @ud                                                          :: element list index
+$  vox   (list lina)                                                  :: rows of text
+$  lina  (list @c)                                                    :: a row of text
+$  fila  [d=(set deco) b=tint f=tint]                                 :: element style
+$  acia  [d=(unit (set deco)) b=(unit tint) f=(unit tint)]            :: alternate element style
+$  fuga  [d=?(%col %row) b=?(%wrap %clip)]                            :: positioning flow
+$  iter  modi                                                         :: scroll position
+$  sola  modi                                                         :: scroll content dimensions
+$  ad    ?(%l %r %t %b)                                               :: direction
+$  via   ?(%h %v)                                                     :: orientation
+$  ora   ?(%light %heavy %double %arc %~)                             :: line style
+$  luna  (map @ud (list lux))                                         :: render blocking context
+$  sol   (list (list lux))                                            :: render schematic
+$  lux                                                                :: render token
  $:  x1=@ud                                                           ::
      x2=@ud                                                           ::
      $=  p                                                            ::
      $@  ~                                                            :: in rendering, null is a gap
      $:  fil=fila                                                     ::
          nav=rex                                                      ::
          txt=lina                                                     ::
  ==  ==                                                               ::
+$  aer   [=iter =muri =rex =luna]                                     :: branch render context (scroll, limits, select hierarchy, blocking)
+$  opus  (list [=apex =sol])                                          :: render batch (hop on null sol)
+$  dux   [k=rami muri]                                                :: navigation point
+$  rex   $@(~ dux)                                                    :: selection
+$  ordo  (list dux)                                                   :: navigation context
+$  omen  (map nota lex)
+$  zona
  $~  [%txt ~]
  $%  [%rez p=@ud q=@ud]
      [%clk p=?(%d %u) x=@ud y=@ud]
      [%whe p=?(%d %u) x=@ud y=@ud]
      [%mod mod=?(%ctl %alt %shf) key=$~([%txt ~] zona)]
      [%aro p=?(%d %l %r %u)]
      [%txt p=lina]
      [%chr p=@c]
      [%bac ~]
      [%del ~]
      [%ret ~]
      [%esc ~]
  ==
+$  nota                 :: FIX NOTA - make it a derived modification of zona
  $~  [%txt ~]
  $%  [%clk ?(%d %u)]
      [%whe ?(%d %u)]
      [%mod ?(%ctl %alt %shf) $~([%txt ~] nota)]
      [%aro ?(%d %l %r %u)]
      [%txt ~]
      [%chr @c]
      [%bac ~]
      [%del ~]
      [%ret ~]
      [%esc ~]
  ==
+$  lex
  $?  %nav-l  %nav-r  %nav-u  %nav-d
      %cur-l  %cur-r  %cur-u  %cur-d
      %scr-l  %scr-r  %scr-u  %scr-d
      %inp  %del  %tog  %act  %clk  %def
  ==
+$  ossa  (map rami (map rami os))
+$  os
  $%  [%h p=?(%border %line) x1=@ud x2=@ud y=@ud =ora]
      [%v p=?(%border %line) x=@ud y1=@ud y2=@ud =ora]
  ==
+$  crux  [v=via i=@ud c=ora l=ora r=ora t=ora b=ora]
+$  aqua  (list [i=@ud size=@ud marg=@ud])
+$  vena
  $:  size=[w=as h=as]
      padd=[l=as r=as t=as b=as]
      marg=[l=as r=as t=as b=as]
      flex=modi
      flow=fuga
      look=acia
  ==
+$  as    $%((pair %c @ud) (pair %p @ud) (pair %i @ud))
+$  data  [p=@t q=(map @t @t)]
+$  vela  manx
+$  urbs  $~([50 25] [x=@ud y=@ud])
+$  aula  @tas
+$  cura  aula
+$  ara
  $:  =vela
      =ossa
      =ordo
      =rex
      =deus
  ==
+$  arae  (map aula ara)
+$  ego
  $:  =urbs  =cura  =arae
  ==
::
+$  card  card:agent:gall
--
::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  
=|  =ego
^-  agent:gall
=<
|_  bol=bowl:gall
+*  hoc  .
++  on-init
  ^-  (quip card _hoc)
  [~ hoc]
++  on-save
  ^-  vase
  !>(urbs.ego)
++  on-load
  |=  old=vase
  ^-  (quip card _hoc)
  =/  ol  (mole |.(!<(urbs old)))
  :-  ~
  %_  hoc
    ego  ?~(ol *^ego =|(^ego -(urbs u.ol)))
  ==
::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  
++  on-poke
  |=  [=mark =vase]
  ^-  (quip card _hoc)
  ?>  =(our.bol src.bol)
  ?+  mark  !!
    ::
      %homunculus-session
    =+  !<(ses=session:homunculus vase)
    =/  aul=aula  ?:(&(?=(^ sap.bol) ?=(^ t.sap.bol)) i.t.sap.bol %$)
    =|  [ram=rami ayr=aer]
    =.  muri.ayr  [1 x.urbs.ego 1 y.urbs.ego]
    =/  aru  (~(get by arae.ego) aul)
    ?~  aru
      =|  new=ara
      =:  cura.ego  aul
          vela.new  +.ses
          deus.new  (geno ~ +.ses)
        ==
      =.  ossa.new  (humo deus.new)
      =/  vis       (viso ram rex.new ossa.new ayr deus.new)
      =.  ordo.new  (gyro apex.cor.deus.new vis)
      :_  hoc(arae.ego (~(put by arae.ego) aul new))
      :~  (fio ~[[apex.cor.deus.new vis]])
      ==
      ::
    =:  vela.u.aru  +.ses
        deus.u.aru  (geno ~ +.ses)
      ==
    =.  ossa.u.aru  (humo deus.u.aru)
    =/  vis         (viso ram rex.u.aru ossa.u.aru ayr deus.u.aru)
    =.  ordo.u.aru  (gyro apex.cor.deus.u.aru vis)
    :_  hoc(arae.ego (~(put by arae.ego) aul u.aru))
    ?.  =(aul cura.ego)  ~
    :~  (fio ~[[apex.cor.deus.u.aru vis]])
    ==
    ::
      %json
    =/  zon  (ineo !<(json vase))
    ?~  zon  [~ hoc]
    :: ~&  >  zon
    ?.  ?=(%rez -.u.zon)  [~ hoc]
    =.  urbs.ego  +.u.zon
    =/  aru  (~(get by arae.ego) cura.ego)
    ?~  aru  [~ hoc]
    =|  [ram=rami ayr=aer]
    =.  muri.ayr  [1 x.urbs.ego 1 y.urbs.ego]
    =.  deus.u.aru  (geno ~ vela.u.aru)
    =.  ossa.u.aru  (humo deus.u.aru)
    =/  vis         (viso ram rex.u.aru ossa.u.aru ayr deus.u.aru)
    =.  ordo.u.aru  (gyro apex.cor.deus.u.aru vis)
    :_  hoc(arae.ego (~(put by arae.ego) cura.ego u.aru))
    :~  (fio ~[[apex.cor.deus.u.aru vis]])
    ==
    ::
  ==
::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  
++  on-watch
  |=  =path
  ^-  (quip card _hoc)
  ?+  path  !!
    ::
      [%homunculus-http ~]
    [~ hoc]
    ::
  ==
  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  
++  on-leave
  |=  =path
  ^-  (quip card _hoc)
  ?+  path  !!
    ::
      [%dill @ ~]
    [~ hoc]
    ::
  ==
::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  
++  on-peek   |=(path ^-((unit (unit cage)) !!))
++  on-agent  |=([wire sign:agent:gall] ^-((quip card _hoc) !!))
++  on-arvo   |=([wire sign-arvo] ^-((quip card _hoc) !!))
++  on-fail   |=([term tang] ^-((quip card _hoc) !!))
--
::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  
|%
::
++  fio                            :: make a display update card
  |=  =opus
  ^-  card
  =;  txt=@t
    [%give %fact ~[/homunculus-http] %json !>(`json`[%s txt])]
  %-  crip
  %-  zing
  ^-  wall
  %+  turn  opus
  |=  i=[apex sol]
  (dico i)
::
++  ineo                           :: parse input text
  |=  jon=json
  ^-  (unit zona)
  ::
  ?:  ?=(%a -.jon)
    ?.  &(?=(^ p.jon) ?=(%n -.i.p.jon) ?=(^ t.p.jon) ?=(%n -.i.t.p.jon))
      ~
    :+  ~  %rez
    [(slav %ud p.i.p.jon) (slav %ud p.i.t.p.jon)]
  ::
  ?.  ?=(%s -.jon)
    ~
  =/  inp=tape  (trip p.jon)
  ?:  =("\\" inp)
    ~
  ?~  inp  ~
  ?.  =('\\' i.inp)     [~ [%txt (tuba inp)]]
  ?~  t.inp             [~ [%txt ~[~-~5c.]]]
  ?~  t.t.inp
    ?:  =('t' i.t.inp)  [~ [%mod %ctl [%chr ~-i]]]
    ?:  =('n' i.t.inp)  [~ [%ret ~]]
    ?:  |(=('e' i.t.inp) =('E' i.t.inp))  [~ [%esc ~]]
    ~
  ?~  t.t.t.inp
    ?:  |(=('e' i.t.inp) =('E' i.t.inp))
      [~ [%mod %alt [%txt (tuba t.t.inp)]]]
    ~
  ?:  =(['1' '7' '7' ~] t.inp)  [~ [%bac ~]]
  =/  seq=tape
    ?:  ?|  &(=('e' i.t.inp) =('[' i.t.t.inp))
            &(=('E' i.t.inp) =('[' i.t.t.inp))
        ==
      t.t.t.inp
    ?:  ?&  ?=(^ t.t.t.t.inp)
            ?|  ?&  =('x' i.t.inp)  =('1' i.t.t.inp)
                    =('B' i.t.t.t.inp)  =('[' i.t.t.t.t.inp)
                ==
                ?&  =('0' i.t.inp)  =('3' i.t.t.inp)
                    =('3' i.t.t.t.inp)  =('[' i.t.t.t.t.inp)
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
  ?:  &(=('1' i.seq) =(';' i.t.seq) ?=(^ t.t.t.seq))
    :^  ~  %mod
      ?:  =('2' i.t.t.seq)  %shf
      ?:  =('3' i.t.t.seq)  %alt
      ?:  =('5' i.t.t.seq)  %ctl
      !!
    ?:  =('A' i.t.t.t.seq)  [%aro %u]
    ?:  =('B' i.t.t.t.seq)  [%aro %d]
    ?:  =('C' i.t.t.t.seq)  [%aro %r]
    ?:  =('D' i.t.t.t.seq)  [%aro %l]
    !!
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
  ~
::
++  muto                           :: handle a hotkey event
  |_  [=lex zon=zona aul=aula =ara]
  ++  $
    ^-  (quip card ^ara)
    ?:  ?|  ?=(%nav-l lex)  ?=(%nav-r lex)
            ?=(%nav-u lex)  ?=(%nav-d lex)
        ==
      meo

      :: :: ------------- OLD --------------
      :: ::
      :: :: find the closest scroll parent (ligo), or none.
      :: ::
      :: =/  scr=$@(~ rami)  ?~(rex.ara ~ (ligo k.rex.ara equi.ara))
      :: =/  spar=(unit ens)  ?~(scr ~ (~(get by esse.ara) scr))
      :: ::
      :: :: get and order the navigation points by current selection and nav direction (gero).
      :: ::
      :: =/  navs=ordo  (gero rex.ara ordo.ara)
      :: ::
      :: :: get a skimmed list of the ordered navigation points with those in the same closest scroll element.
      :: ::
      :: =/  snav=ordo
      ::   ?~  scr  ~
      ::   %+  skim  ^-(ordo navs)
      ::   |=  =dux
      ::   (alo scr k.dux)
      :: ::
      :: :: if the skimmed list has an item, that is the prospective next selection, else the initial ordered list, else none.
      :: ::
      :: =/  nex=rex  ?~(navs ~ ?~(snav i.navs i.snav))
      :: ::
      :: :: check if the closest scroll parent is scrolled to its maximum
      :: ::
      :: =/  send=bean
      ::   ?&  ?=(^ rex.ara)  ?=(^ spar)  ?=(%scroll -.ars.u.spar)  =(scr k.rex.ara)
      ::       ?|  &(?=(%nav-l lex) =(0 x.iter.ars.u.spar))
      ::           &(?=(%nav-r lex) =(x.sola.ars.u.spar x.iter.ars.u.spar))
      ::           &(?=(%nav-u lex) =(0 y.iter.ars.u.spar))
      ::           &(?=(%nav-d lex) =(y.sola.ars.u.spar y.iter.ars.u.spar))
      ::   ==  ==
      :: ::
      :: :: if the nearest scroll element is at its maximum, get the next nearest and redo the following values
      :: ::
      :: =?  scr   &(send ?=(^ scr) ?=(^ t.scr))  
      ::   =/  pscr=$@(~ rami)  (ligo t.scr equi.ara)
      ::   ?~(pscr scr pscr)
      :: =?  spar  &(send ?=(^ scr))  (~(get by esse.ara) scr)
      :: =?  nex  &(send ?=(~ scr) ?=(^ navs))  i.navs
      :: =?  snav  &(send ?=(^ scr))
      ::   %+  skim  ^-(ordo navs)
      ::   |=  =dux
      ::   (alo scr k.dux)
      :: =?  nex  &(send ?=(^ scr) ?=(^ snav))  i.snav
      :: ::
      :: :: perform a scroll update (abeo)
      :: ::
      :: =/  abe=$@(~ [=esse cura])
      ::   ?~  scr  ~
      ::   ?:  ?=(^ snav)  ~
      ::   %+  abeo  scr
      ::   ?+  lex   lex
      ::     %nav-l  %scr-l   %nav-r  %scr-r
      ::     %nav-u  %scr-u   %nav-d  %scr-d
      ::   ==
      :: ::
      :: :: update session state with the scroll update result
      :: ::
      :: =?  ara  ?=(^ abe)
      ::   %_  ara
      ::     esse  esse.abe   omen  omen.abe
      ::     aves  aves.abe   gens  gens.abe
      ::     ordo  ordo.abe   rex   rex.abe
      ::     equi  equi.abe   mus   mus.abe
      ::   ==
      :: ::
      :: :: if there was a scroll update filter the navigation points again and see if a selection is now available in the scroll element 
      :: ::
      :: =?  nex  ?=(^ abe)
      ::   ?~  scr  ~
      ::   =<  ?~(. ~ i)
      ::   %+  skim  ^-(ordo (gero rex.ara ordo.abe))
      ::   |=  =dux
      ::   (alo scr k.dux)
      :: ::
      :: :: save the old selection and change selection state to the new one
      :: ::
      :: =/  orx  rex.ara
      :: =.  rex.ara
      ::   ?:  ?=(^ nex)  nex
      ::   ?:  ?|  ?=(~ scr)  ?=(~ spar)  ?=(~ abe)
      ::           ?=(^ (find ~[rex.ara] ordo.abe))
      ::       ==
      ::     rex.ara
      ::   (rogo scr ordo.ara)
      :: ::
      :: :: produce a selection style update
      :: ::
      :: =/  duc=opus  (duco orx rex.ara esse.ara ?~(abe visa.ara visa.abe))
      :: ::
      :: :: get the element that is the new selection
      :: ::
      :: =/  sel=(unit ens)  ?~(rex.ara ~ (~(get by esse.ara) k.rex.ara))
      :: ::
      :: :: make the hotkey context either normal nav or input depending on whether or not the new selection is an input or not
      :: ::
      :: =.  omen.ara
      ::   ?~  sel  omen.ara
      ::   ?+  -.ars.u.sel  (~(uni by omen.ara) hnav)
      ::     %input         (~(uni by omen.ara) hinp)
      ::   ==
      :: ::
      :: :: finally, resolve all element and character updates, make a display update, and a cursor move
      :: ::
      :: =?  esse.ara  ?=(^ esse.duc)  (~(uni by esse.ara) ^-(esse esse.duc))
      :: =?  abe  ?=(^ abe)  abe(visa (~(uni by visa.abe) ^-(visa visa.duc)))
      :: :_  ara(visa ?~(abe (~(uni by visa.ara) visa.duc) visa.abe))
      :: :_  ?:  |(?=(~ nex) ?=(~ sel) ?=(~ avis.u.sel))
      ::       ~
      ::     :~  :*  %pass  /select  %agent  fon  %poke  %homunculus-event
      ::             !>(^-(event:homunculus [%select u.avis.u.sel]))
      ::     ==  ==
      :: :+  ~  %mor
      :: :~  (supo visa.ara ?~(abe visa.duc visa.abe))
      ::     ^-  lux
      ::     ?:  &(?=(^ sel) ?=(%input -.ars.u.sel))
      ::       (vado ab.ars.u.sel i.ars.u.sel size.res.u.sel lar.u.sel iter.u.sel)
      ::     ?~  rex.ara  [%hop [1 1]]
      ::     ?:  |(?=(~ spar) &(?=(~ abe) ?=(~ snav) ?=(^ navs)) =(k.rex.ara scr))
      ::       [%hop [l.rex.ara t.rex.ara]]
      ::     (cedo rex.ara scr u.spar)
      :: ==

    ::
    [~ aru]
  ::
  ++  meo                          :: handle a navigation event
    ^-  (quip card ^ara)
    :: 1) get and order the navigation points by current selection and nav direction (gero).
    =/  navs=ordo  (gero rex.ara ordo.ara)
    :: 2) find the closest scroll parent or none (fluo).
    =/  active-scroll=(unit rami)  fluo
    :: 3) get a skimmed list of the ordered navigation points with those in the same closest scroll element.
    =/  navs-in-scroll=ordo
      ?~  active-scroll  ~
      %+  skim  `ordo`navs
      |=  =dux
      (alo u.active-scroll k.dux)
    :: 4) if the skimmed list has an item, that is the prospective next selection, else the initial ordered list, else none.
    =/  next=rex
      ?^  snav  i.snav
      ?^  navs  i.navs
      ~
    :: 5) if fluo returned defined, and the skimmed list is null, set a flag to perform a scroll update.
    =/  do-scroll=?
      ?&  ?=(^ active-scroll)
          ?=(~ navs-in-scroll)
          |(?=(%nav-u lex) ?=(%nav-d lex))
      ==
    :: 6) to perform a scroll update, change iter on the scroll parent, and then produce a new render schematic and from this new interactivity context
    ?:  do-scroll
      =^  rend=[=apex =sol]  ara  (eo (need active-scroll))
      :_  ara
      :~  (fio ~[rend [(fero ) ~]])
      ==

    
    
        :: there are two(?) different cases for the format of render produced:
        :: 1) if there is a scroll update performed (iter chaged), just produce one render with that whole scroll element rerendered, once rex is comletely resolved,
        :: 2) if there isn't a scroll update performed, attempt to produce one render for the old rex, and one render for the new rex.
        :: a cursor hop needs to follow either case




  ::
  ++  eo                           :: perform a scroll
    |=  key=rami
    ^-  [[apex sol] ^ara]
    =/  [ayr=aer scr=deus]  (creo key deus.ara)
    ?>  ?=(%scroll -.ars.cor.scr)
    =.  y.iter.ars.cor.scr
      ?+  lex   y.iter.ars.cor.scr
        %nav-u  (dec y.iter.ars.cor.scr)
        %nav-d  +(y.iter.ars.cor.scr)
      ==
    =/  rend=sol  (viso key rex.ara ossa.ara ayr scr)
    =.  ordo.ara  (ligo key (gyro apex.cor.scr rend) ordo.ara)
    =/  navs=ordo
      %+  skim  (gero rex.ara ordo.ara)
      |=  =dux
      (alo key k.dux)
    ?~  navs
      :-  [apex.cor.scr rend]
      %_  ara
        deus  (novo key scr)
      ==
    =.  rex.ara  i.navs
    =.  rend  (viso key rex.ara ossa.ara ayr scr)
    :-  [apex.cor.scr rend]
    %_  ara
      deus  (novo key scr)
    ==
  ::
  ++  fluo                         :: find the nearest scroll parent not maxed out in the nav direction
    ^-  (unit rami)
    ?~  rex.ara  ~
    =|  key=rami
    =|  acc=(list rami)
    |-  ^-  (unit rami)
    ?~  k.rex.ara
      ?~  acc  ~
      [~ i.acc]
    =.  key  (snoc key i.k.rex.ara)
    =?  acc
        ?&  ?=(%scroll -.ars.cor.deus.ara)
            ?!
            ?|  &(?=(%nav-l lex) =(0 x.iter.ars.cor.deus.ara))
                &(?=(%nav-r lex) =(x.sola.ars.cor.deus.ara x.iter.ars.cor.deus.ara))
                &(?=(%nav-u lex) =(0 y.iter.ars.cor.deus.ara))
                &(?=(%nav-d lex) =(y.sola.ars.cor.deus.ara y.iter.ars.cor.deus.ara))
        ==  ==
      [key acc]
    %=  $
      k.rex.ara  t.k.rex.ara
      deus.ara
        %+  snag  ager.i.k.rex.ara
        ?-  axis.i.k.rex.ara
          %n  n.gens.deus.ara
          %b  b.gens.deus.ara
          %l  l.gens.deus.ara
        ==
    ==
  ::
  ++  gero                         :: order a list of navigation points
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
        (alo k.r k.dux)
      |=  [a=dux b=dux]
      (lth (reor a r) (reor b r))
    ?:  ?=(^ chis)
      chis
    =/  tars=ordo
      %+  sort  `ordo`(skim `ordo`o (cieo r))
      |=  [a=dux b=dux]
      (lth (reor a r) (reor b r))
    ?~  tars
      ~
    =/  pars=ordo
      %+  sort
        ^-  ordo
        %+  skim  `ordo`o
        |=  d=dux
        ^-  bean
        ?:  =(k.d k.r)
          |
        ?:  (alo k.d k.i.tars)
          !(alo k.d k.r)
        |
      |=  [a=dux b=dux]
      (lth (lent k.a) (lent k.b))
    ?~  pars
      tars
    ?.  ((cieo r) i.pars)
      ~
    pars
  ::
  ++  rogo                         :: find a dux in ordo by key
    |=  [k=rami o=ordo]
    ^-  $@(~ dux)
    ?~  o  ~
    ?:  =(k k.i.o)
      i.o
    $(o t.o)
  ::
  ++  alo                          :: check if element b is a child of element a
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
  ++  tego                         :: check if either element a or element b is in a layer above
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
  ++  cieo                         :: check if a navigation point is viable given the current selection
    |=  rex=dux
    |=  =dux
    ^-  bean
    ?:  =(k.dux k.rex)  |
    ?+  lex  |
        %nav-l
      ?:  (taxo +.dux +.rex)
        ?:  =(l.dux l.rex)
          |((alo k.dux k.rex) =(%b (tego k.rex k.dux)))
        (lth l.dux l.rex)
      ?.  =(%~ (tego k.rex k.dux))
        (lth l.dux l.rex)
      (lth r.dux l.rex)
        %nav-u
      ?:  (taxo +.dux +.rex)
        ?:  =(t.dux t.rex)
          |((alo k.dux k.rex) =(%b (tego k.rex k.dux)))
        (lth t.dux t.rex)
      ?.  =(%~ (tego k.rex k.dux))
        (lth t.dux t.rex)
      (lth b.dux t.rex)
        %nav-r
      ?:  (taxo +.dux +.rex)
        ?:  =(l.dux l.rex)
          |((alo k.rex k.dux) =(%a (tego k.rex k.dux)))
        (gth l.dux l.rex)
      ?.  =(%~ (tego k.rex k.dux))
        (gth l.dux l.rex)
      (gth l.dux r.rex)
        %nav-d
      ?:  (taxo +.dux +.rex)
        ?:  =(t.dux t.rex)
          |((alo k.rex k.dux) =(%a (tego k.rex k.dux)))
        (gth t.dux t.rex)
      ?.  =(%~ (tego k.rex k.dux))
        (gth t.dux t.rex)
      (gth t.dux b.rex)
    ==
  ::
  ++  reor                         :: get the euclidean distance between a selection and a navigation point
    |=  [=dux rex=dux]
    ^-  @ud
    =;  pyt=(pair @ud @ud)
      (add (mul 10 p.pyt) q.pyt)
    ?+  lex  [0 0]
        %nav-l
      ?:  |((taxo +.dux +.rex) !=(%~ (tego k.rex k.dux)))
        %-  sqt  %+  add
          (pow ?:((lte l.dux l.rex) (sub l.rex l.dux) (sub l.dux l.rex)) 2)
        (pow (mul ?:((gte t.rex t.dux) (sub t.rex t.dux) (sub t.dux t.rex)) 2) 2)
      %-  sqt  %+  add
        (pow ?:((lte r.dux l.rex) (sub l.rex r.dux) (sub r.dux l.rex)) 2)
      (pow (mul ?:((gte t.rex t.dux) (sub t.rex t.dux) (sub t.dux t.rex)) 2) 2)
        %nav-u
      ?:  |((taxo +.dux +.rex) !=(%~ (tego k.rex k.dux)))
        %-  sqt  %+  add
          (pow ?:((gte l.dux l.rex) (sub l.dux l.rex) (sub l.rex l.dux)) 2)
        (pow (mul ?:((lte t.dux t.rex) (sub t.rex t.dux) (sub t.dux t.rex)) 2) 2)
      %-  sqt  %+  add
        (pow ?:((gte l.dux l.rex) (sub l.dux l.rex) (sub l.rex l.dux)) 2)
      (pow (mul ?:((lte b.dux t.rex) (sub t.rex b.dux) (sub b.dux t.rex)) 2) 2)
        %nav-r
      ?:  |((taxo +.dux +.rex) !=(%~ (tego k.rex k.dux)))
        %-  sqt  %+  add
          (pow ?:((lte l.rex l.dux) (sub l.dux l.rex) (sub l.rex l.dux)) 2)
        (pow (mul ?:((gte t.dux t.rex) (sub t.dux t.rex) (sub t.rex t.dux)) 2) 2)
      %-  sqt  %+  add
        (pow ?:((lte r.rex l.dux) (sub l.dux r.rex) (sub r.rex l.dux)) 2)
      (pow (mul ?:((gte t.dux t.rex) (sub t.dux t.rex) (sub t.rex t.dux)) 2) 2)
        %nav-d
      ?:  |((taxo +.dux +.rex) !=(%~ (tego k.rex k.dux)))
        %-  sqt  %+  add
          (pow ?:((gte l.rex l.dux) (sub l.rex l.dux) (sub l.dux l.rex)) 2)
        (pow (mul ?:((lte t.rex t.dux) (sub t.dux t.rex) (sub t.rex t.dux)) 2) 2)
      %-  sqt  %+  add
        (pow ?:((gte l.rex l.dux) (sub l.rex l.dux) (sub l.dux l.rex)) 2)
      (pow (mul ?:((lte b.rex t.dux) (sub t.dux b.rex) (sub b.rex t.dux)) 2) 2)
    ==
  ::
  ++  taxo                         :: compare two coordinate groups for an overlap
    |=  [a=muri b=muri]
    ^-  bean
    ?|  ?&  (lte l.a l.b)  (gte r.a l.b)
            (lte t.a t.b)  (gte b.a t.b)
        ==
        ?&  (lte l.a r.b)  (gte r.a r.b)
            (lte t.a t.b)  (gte b.a t.b)
        ==
        ?&  (lte l.a l.b)  (gte r.a l.b)
            (lte t.a b.b)  (gte b.a b.b)
        ==
        ?&  (lte l.a r.b)  (gte r.a r.b)
            (lte t.a b.b)  (gte b.a b.b)
        ==
        ?&  (lte l.a l.b)  (gte r.a r.b)
            (gte t.a t.b)  (lte b.a b.b)
        ==
        ?&  (lte l.b l.a)  (gte r.b l.a)
            (lte t.b t.a)  (gte b.b t.a)
        ==
        ?&  (lte l.b r.a)  (gte r.b r.a)
            (lte t.b t.a)  (gte b.b t.a)
        ==
        ?&  (lte l.b l.a)  (gte r.b l.a)
            (lte t.b b.a)  (gte b.b b.a)
        ==
        ?&  (lte l.b r.a)  (gte r.b r.a)
            (lte t.b b.a)  (gte b.b b.a)
        ==
        ?&  (lte l.b l.a)  (gte r.b r.a)
            (gte t.b t.a)  (lte b.b b.a)
        ==
    ==
  ::
  ++  novo                         :: replace an element in the tree
    |=  [key=rami deu=deus]
    ^-  deus
    ?~  key  deu
    =/  nex=deus
      %+  snag  ager.i.key
      ?-  axis.i.key
        %n  n.gens.deu
        %b  b.gens.deu
        %l  l.gens.deu
      ==
    ?-  axis.i.key
      %n  deus.ara(n (snap n.gens.deus.ara ager.i.key $(deus.ara nex, key t.key)))
      %b  deus.ara(b (snap b.gens.deus.ara ager.i.key $(deus.ara nex, key t.key)))
      %l  deus.ara(l (snap l.gens.deus.ara ager.i.key $(deus.ara nex, key t.key)))
    ==
  ::
  --
::
++  fero                           :: resolve cursor location
  |=  [=rex =deus]
  ^-  loci
  ?~  rex  [1 1]
  =/  [ayr=aer deu=^deus]  (creo k.rex deus)
  =/  [[x1=@ y1=@] [x2=@ y2=@] room=muri]
    (laxo iter.ayr apex.cor.deu res.cor.deu)
  ?:  ?=(%input -.ars.cor.deu)
    %:  vado
      [x1 y1]  size.res.cor.deu
      ab.ars.cor.deu  i.ars.cor.deu
    ==
  =:  x1  (max x1 l.muri.ayr)
      y1  (min y1 t.muri.ayr)
    ==
  :: process the selected element coordinate against layer blocking
  
  



::
++  vado                           :: resolve cursor location for an input
  |=  [=apex [w=@ud h=@ud] ab=@ud i=loci]
  ^-  loci
  ?:  =(1 h)
    [(add x.apex (sub x.i ab)) y.apex]
  ?:  (lth x.i w)
    [(add x.apex x.i) (add y.apex (sub y.i ab))]
  =/  ran=@ud  (sub y.i ab)
  ?.  (lth +(ran) h)
    [(add x.apex ?:(=(0 w) 0 (dec w))) (add y.apex ran)]
  [x.apex +((add y.apex ran))]
::
++  pono                           :: get the length of a row in vox without the trailing whitespace
  |=  lop=lina
  =.  lop  (flop lop)
  |-  ^-  @ud
  ?~  lop  0
  ?.  =(~-. i.lop)  (lent lop)
  $(lop t.lop)
::
++  dolo                           :: get default styles for a semantic element
  |=  el=@tas
  ^-  vena
  ?+  el
    :*  size=[[%i 0] [%i 0]]
        padd=[[%c 0] [%c 0] [%c 0] [%c 0]]
        marg=[[%c 0] [%c 0] [%c 0] [%c 0]]
        flex=[0 0]
        flow=[%col %clip]
        look=[~ ~ ~]
    ==
      %text
    :*  size=[[%c 0] [%c 0]]
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
        flow=[%col %clip]
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
      %form
    :*  size=[[%i 0] [%i 0]]
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
++  suo                            :: process a sail element's name and attribute list for geno
  |=  [n=mane a=mart]
  =|  [=avis =acia marv=mart]
  =/  [=vena =ars]
      ?+  n             [(dolo %$) [%$ ~]]
        %$              [(dolo %text) [%text ~]]
        %pattern        [(dolo %$) [%pattern ~]]
        %layer          [(dolo %layer) [%layer ~]]
        %select         [(dolo %$) [%select %~]]
        %border-left    [(dolo %border-left) [%border %l %~]]
        %border-right   [(dolo %border-right) [%border %r %~]]
        %border-top     [(dolo %border-top) [%border %t %~]]
        %border-bottom  [(dolo %border-bottom) [%border %b %~]]
        %line-h         [(dolo %line-h) [%line %h %light]]
        %line-v         [(dolo %line-v) [%line %v %light]]
        %scroll         [(dolo %scroll) [%scroll *iter *muri *sola]]
        %form           [(dolo %form) [%form ~]]
        %input          [(dolo %input) [%input 0 [0 0] ~]]
        %checkbox       [(dolo %checkbox) [%checkbox %.n]]
        %radio          [(dolo %$) [%radio ~]]
        %submit         [(dolo %$) [%select %submit]]
      ==
  =/  =lina  ?.(?=(%text -.ars) ~ ?~(a ~ (tuba v.i.a))) 
  |-  ^-  [^vena ^avis ^acia ^ars ^lina mart]
  ?~  a  [vena avis acia ars lina marv]
  ?+  n.i.a  $(a t.a)
      %w
    ?:  &(?=(%border -.ars) |(?=(%t ad.ars) ?=(%b ad.ars)))
      $(a t.a)
    $(w.size.vena (pars v.i.a), a t.a)
      %h
    ?:  &(?=(%border -.ars) |(?=(%l ad.ars) ?=(%r ad.ars)))
      $(a t.a)
    $(h.size.vena (pars v.i.a), a t.a)
      %p
    =/  v=as  (pars v.i.a)
    $(padd.vena [v v v v], a t.a)
      %px
    =/  v=as  (pars v.i.a)
    $(l.padd.vena v, r.padd.vena v, a t.a)
      %py
    =/  v=as  (pars v.i.a)
    $(t.padd.vena v, b.padd.vena v, a t.a)
      %pl
    $(l.padd.vena (pars v.i.a), a t.a)
      %pr
    $(r.padd.vena (pars v.i.a), a t.a)
      %pt
    $(t.padd.vena (pars v.i.a), a t.a)
      %pb
    $(b.padd.vena (pars v.i.a), a t.a)
      %m
    ?:  ?=(%border -.ars)
      $(a t.a)
    =/  v=as  (pars v.i.a)
    $(marg.vena [v v v v], a t.a)
      %mx
    ?:  ?=(%border -.ars)
      $(a t.a)
    =/  v=as  (pars v.i.a)
    $(l.marg.vena v, r.marg.vena v, a t.a)
      %my
    ?:  ?=(%border -.ars)
      $(a t.a)
    =/  v=as  (pars v.i.a)
    $(t.marg.vena v, b.marg.vena v, a t.a)
      %ml
    ?:  ?=(%border -.ars)
      $(a t.a)
    $(l.marg.vena (pars v.i.a), a t.a)
      %mr
    ?:  ?=(%border -.ars)
      $(a t.a)
    $(r.marg.vena (pars v.i.a), a t.a)
      %mt
    ?:  ?=(%border -.ars)
      $(a t.a)
    $(t.marg.vena (pars v.i.a), a t.a)
      %mb
    ?:  ?=(%border -.ars)
      $(a t.a)
    $(b.marg.vena (pars v.i.a), a t.a)
      %fx
    =/  num=(unit @ud)  (slaw %ud (crip v.i.a))
    ?:  ?=(^ num)
      $(x.flex.vena (min u.num 100), a t.a)
    ?+  (@tas (crip v.i.a))  $(a t.a)
      %start   $(x.flex.vena 0, a t.a)
      %center  $(x.flex.vena 50, a t.a)
      %end     $(x.flex.vena 100, a t.a)
    ==
      %fy
    =/  num=(unit @ud)  (slaw %ud (crip v.i.a))
    ?:  ?=(^ num)
      $(y.flex.vena (min u.num 100), a t.a)
    ?+  (@tas (crip v.i.a))  $(a t.a)
      %start   $(y.flex.vena 0, a t.a)
      %center  $(y.flex.vena 50, a t.a)
      %end     $(y.flex.vena 100, a t.a)
    ==
      %fl
    ?+  (@tas (crip v.i.a))  $(a t.a)
      %row          $(flow.vena [%row %clip], a t.a)
      %row-clip     $(flow.vena [%row %clip], a t.a)
      %row-wrap     $(flow.vena [%row %wrap], a t.a)
      %column       $(flow.vena [%col %clip], a t.a)
      %column-clip  $(flow.vena [%col %clip], a t.a)
      %column-wrap  $(flow.vena [%col %wrap], a t.a)
    ==
      %cb
    ?:  &(?=(^ v.i.a) =('#' i.v.i.a))
      $(b.look.vena [~ (seco v.i.a)], a t.a)
    ?+  (@tas (crip v.i.a))  $(b.look.vena ~, a t.a)
      %red      $(b.look.vena [~ %r], a t.a)
      %green    $(b.look.vena [~ %g], a t.a)
      %blue     $(b.look.vena [~ %b], a t.a)
      %cyan     $(b.look.vena [~ %c], a t.a)
      %magenta  $(b.look.vena [~ %m], a t.a)
      %yellow   $(b.look.vena [~ %y], a t.a)
      %black    $(b.look.vena [~ %k], a t.a)
      %white    $(b.look.vena [~ %w], a t.a)
    ==
      %cf
    ?:  &(?=(^ v.i.a) =('#' i.v.i.a))
      $(f.look.vena [~ (seco v.i.a)], a t.a)
    ?+  (@tas (crip v.i.a))  $(f.look.vena ~, a t.a)
      %red      $(f.look.vena [~ %r], a t.a)
      %green    $(f.look.vena [~ %g], a t.a)
      %blue     $(f.look.vena [~ %b], a t.a)
      %cyan     $(f.look.vena [~ %c], a t.a)
      %magenta  $(f.look.vena [~ %m], a t.a)
      %yellow   $(f.look.vena [~ %y], a t.a)
      %black    $(f.look.vena [~ %k], a t.a)
      %white    $(f.look.vena [~ %w], a t.a)
    ==
      %d
    ?+  (@tas (crip v.i.a))  $(a t.a)
      %bold       $(d.look.vena [~ ?~(d.look.vena (silt ~[%br]) (~(put in u.d.look.vena) %br))], a t.a)
      %blink      $(d.look.vena [~ ?~(d.look.vena (silt ~[%bl]) (~(put in u.d.look.vena) %bl))], a t.a)
      %underline  $(d.look.vena [~ ?~(d.look.vena (silt ~[%un]) (~(put in u.d.look.vena) %un))], a t.a)
      %none       $(d.look.vena [~ (silt ~[%~])], a t.a)
    ==
      %b
    ?.  ?=(%border -.ars)
      $(marv [i.a marv], a t.a)
    ?+  (@tas (crip v.i.a))  $(a t.a)
      %light   $(ora.ars %light, a t.a)
      %heavy   $(ora.ars %heavy, a t.a)
      %double  $(ora.ars %double, a t.a)
      %arc     $(ora.ars %arc, a t.a)
    ==
      %b-cb
    ?.  ?=(%border -.ars)
      $(marv [i.a marv], a t.a)
    ?:  &(?=(^ v.i.a) =('#' i.v.i.a))
      $(b.look.vena [~ (seco v.i.a)], a t.a)
    ?+  (@tas (crip v.i.a))  $(b.look.vena ~, a t.a)
      %red      $(b.look.vena [~ %r], a t.a)
      %green    $(b.look.vena [~ %g], a t.a)
      %blue     $(b.look.vena [~ %b], a t.a)
      %cyan     $(b.look.vena [~ %c], a t.a)
      %magenta  $(b.look.vena [~ %m], a t.a)
      %yellow   $(b.look.vena [~ %y], a t.a)
      %black    $(b.look.vena [~ %k], a t.a)
      %white    $(b.look.vena [~ %w], a t.a)
    ==
      %b-cf
    ?.  ?=(%border -.ars)
      $(marv [i.a marv], a t.a)
    ?:  &(?=(^ v.i.a) =('#' i.v.i.a))
      $(f.look.vena [~ (seco v.i.a)], a t.a)
    ?+  (@tas (crip v.i.a))  $(f.look.vena ~, a t.a)
      %red      $(f.look.vena [~ %r], a t.a)
      %green    $(f.look.vena [~ %g], a t.a)
      %blue     $(f.look.vena [~ %b], a t.a)
      %cyan     $(f.look.vena [~ %c], a t.a)
      %magenta  $(f.look.vena [~ %m], a t.a)
      %yellow   $(f.look.vena [~ %y], a t.a)
      %black    $(f.look.vena [~ %k], a t.a)
      %white    $(f.look.vena [~ %w], a t.a)
    ==
      %b-d
    ?.  ?=(%border -.ars)
      $(marv [i.a marv], a t.a)
    ?+  (@tas (crip v.i.a))  $(d.look.vena ~, a t.a)
      %bold       $(d.look.vena [~ (silt ~[%br])], a t.a)
      %blink      $(d.look.vena [~ (silt ~[%bl])], a t.a)
      %underline  $(d.look.vena [~ (silt ~[%un])], a t.a)
    ==
      %l
    ?.  ?=(%line -.ars)  $(a t.a)
    ?+  (@tas (crip v.i.a))  $(a t.a)
      %light   $(ora.ars %light, a t.a)
      %heavy   $(ora.ars %heavy, a t.a)
      %double  $(ora.ars %double, a t.a)
      %arc     $(ora.ars %arc, a t.a)
    ==
      %id
    $(avis [~ (crip v.i.a)], a t.a)
      %select-cb
    ?:  &(?=(^ v.i.a) =('#' i.v.i.a))
      $(b.acia [~ (seco v.i.a)], a t.a)
    ?+  (@tas (crip v.i.a))  $(a t.a)
      %red      $(b.acia [~ %r], a t.a)
      %green    $(b.acia [~ %g], a t.a)
      %blue     $(b.acia [~ %b], a t.a)
      %cyan     $(b.acia [~ %c], a t.a)
      %magenta  $(b.acia [~ %m], a t.a)
      %yellow   $(b.acia [~ %y], a t.a)
      %black    $(b.acia [~ %k], a t.a)
      %white    $(b.acia [~ %w], a t.a)
    ==
      %select-cf
    ?:  &(?=(^ v.i.a) =('#' i.v.i.a))
      $(f.acia [~ (seco v.i.a)], a t.a)
    ?+  (@tas (crip v.i.a))  $(a t.a)
      %red      $(f.acia [~ %r], a t.a)
      %green    $(f.acia [~ %g], a t.a)
      %blue     $(f.acia [~ %b], a t.a)
      %cyan     $(f.acia [~ %c], a t.a)
      %magenta  $(f.acia [~ %m], a t.a)
      %yellow   $(f.acia [~ %y], a t.a)
      %black    $(f.acia [~ %k], a t.a)
      %white    $(f.acia [~ %w], a t.a)
    ==
      %select-d
    ?+  (@tas (crip v.i.a))  $(a t.a)
      %bold       $(d.acia [~ ?~(d.acia (silt ~[%br]) (~(put in u.d.acia) %br))], a t.a)
      %blink      $(d.acia [~ ?~(d.acia (silt ~[%bl]) (~(put in u.d.acia) %bl))], a t.a)
      %underline  $(d.acia [~ ?~(d.acia (silt ~[%un]) (~(put in u.d.acia) %un))], a t.a)
      %none       $(d.acia [~ (silt ~[%~])], a t.a)
    ==
      %default
    ?.  ?=(%input -.ars)  $(a t.a)
    $(lina (tuba v.i.a), a t.a)
  ==
::
++  paro                           :: check whether an element is a navigation point
  |=  t=term
  ^-  ?
  ?|  =(%select t)
      =(%scroll t)
      =(%input t)
      =(%checkbox t)
  ==
::
++  apo                            :: make axis for a key
  |=  t=term
  ^-  axis
  ?+  t      %n
    %layer   %l
    %border  %b
  ==
::
++  pars                           :: parse a tape to a sizing unit
  |=  v=tape
  ^-  as
  ?:  =(~ v)  [%c 0]
  ?:  =('%' (rear v))
    =/  n=(unit @ud)  (slaw %ud (crip (snip v)))
    ?~  n  [%c 0]
    [%p (min u.n 100)]
  =/  n=(unit @ud)  (slaw %ud (crip v))
  ?~  n  [%c 0]
  [%c u.n]
::
++  seco                           :: parse a tape to a hex color
  |=  v=tape
  ^-  [r=@uxD g=@uxD b=@uxD]
  ?.  ?&  ?=(^ v)  ?=(^ t.v)  ?=(^ t.t.v)  ?=(^ t.t.t.v)
          ?=(^ t.t.t.t.v)  ?=(^ t.t.t.t.t.v)  ?=(^ t.t.t.t.t.t.v)     :: FIX
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
++  obeo                           :: get border sizes from a list of border elements
  |=  bor=marl                     :: defaults to 1 if an element is found and no valid size is specified
  =+  [i=0 bl=0 br=0 bt=0 bb=0]
  |-  ^-  muri
  ?~  bor  [bl br bt bb]
  ?+  n.g.i.bor  $(bor t.bor)
      %border-left
    ?~  a.g.i.bor
      $(bor t.bor, bl ?:(=(0 bl) 1 bl))
    ?:  =(%w n.i.a.g.i.bor)
      =/  n=(unit @ud)  (slaw %ud (crip v.i.a.g.i.bor))             :: CONSOLIDATE
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
++  sero                           :: separate grow elements from the normal element list
  |=  [plow=fuga norm=marl]
  =|  [i=@ud gro=marl aqu=aqua nor=marl]
  |-  ^-  [[marl aqua] marl]
  ?~  norm  [[(flop gro) (flop aqu)] (flop nor)]
  =/  [w=bean h=bean]
    [?=(^ (find ~[[%w "grow"]] a.g.i.norm)) ?=(^ (find ~[[%h "grow"]] a.g.i.norm))]
  ?.  |(w h)  $(nor [i.norm nor], norm t.norm, i +(i))
  ?:  ?=([%row %clip] plow)
    %=  $
      gro   ?:(w ?:(h [i.norm(a.g (snoc a.g.i.norm [%h "100%"])) gro] [i.norm gro]) gro)   :: CONSOLIDATE
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
++  oro                            :: turn lina into vox
  |=  [wid=(unit @ud) hei=(unit @ud) lin=lina]
  ?:  &(?=(^ hei) =(1 u.hei))  ^-(vox ~[lin])
  =|  [v=vox col=@ud wod=@ud]
  |-  ^-  vox
  ?~  lin
    (flop ?:(&(?=(^ v) ?=(^ i.v)) v(i (flop i.v)) v))
  ?:  =(~-~a. i.lin)
    ?~  v  $(lin t.lin)
    $(lin t.lin, col 0, wod 0, v [~ ^-(lina (flop i.v)) t.v])
  ?~  v
    ?:  &(?=(^ wid) (gte +(col) u.wid))
      $(lin t.lin, col 0, wod 0, v [~ [i.lin ~] ~])
    $(lin t.lin, col +(col), wod +(wod), v [[i.lin ~] ~])
  ?:  =(~-. i.lin)
    $(lin t.lin, col +(col), wod +(wod), v [[i.lin i.v] t.v])
  ?:  &(?=(^ i.v) =(~-. i.i.v))
    ?:  &(?=(^ wid) |((gth col u.wid) &(=(col u.wid) !&(?=(^ t.lin) =(~-. i.t.lin)))))
      $(lin t.lin, col 1, wod 1, v [[i.lin ~] ^-(lina (flop i.v)) t.v])
    $(lin t.lin, col +(col), wod 1, v [[i.lin i.v] t.v])
  ?:  &(?=(^ wid) (gte col u.wid))
    ?:  (lth +(wod) u.wid)
      %=  $
        lin  t.lin
        col  +(wod)
        wod  +(wod)
        v     
          :+  [i.lin ^-(lina (scag wod ^-(lina i.v)))]
            ^-(lina (flop (oust [0 wod] ^-(lina i.v))))
          t.v
      ==
    ?:  (gte col u.wid)
      $(lin t.lin, col 1, wod 1, v [[i.lin ~] ^-(lina (flop i.v)) t.v])
    $(lin t.lin, col 0, wod 0, v ?~(t.lin [[i.lin i.v] t.v] [~ ^-(lina (flop [i.lin i.v])) t.v]))
  $(lin t.lin, col +(col), wod +(wod), v [[i.lin i.v] t.v])
::
++  fuco                           :: cover a given area with a pattern
  |=  [wid=@ud hei=@ud bas=vox]
  ^-  vox
  ?:  |(=(0 wid) =(0 hei))  ~
  =/  cop=vox  bas
  =/  [x=@ud y=@ud cx=@ud iy=@ud]  [1 1 1 0]
  =/  len=@ud  (roll bas |=([i=lina a=@ud] (max a (lent i))))
  |-  ^-  vox
  ?~  bas  ~
  :-  |-  ^-  lina
      ?:  =(x +(wid))  ~
      ?~  i.bas
        :-  ~-.
        ?:  =(cx len)
          $(x +(x), cx 1, i.bas (snag iy cop))
        $(x +(x), cx +(cx))
      :-  i.i.bas
      ?:  =(cx len)
        $(x +(x), cx 1, i.bas (snag iy cop))
      $(x +(x), cx +(cx), i.bas t.i.bas)
  ?:  =(y hei)  ~
  ?~  t.bas
    $(y +(y), iy 0, bas cop)
  $(y +(y), iy +(iy), bas t.bas)
::
++  orno                           :: make a line as vox
  |=  [siz=[w=@ud h=@ud] dir=term =ora]
  ^-  vox
  ?:  ?=(%~ ora)  ~
  ?:  |(?=(%t dir) ?=(%b dir) ?=(%h dir))
    :_  ~
    %+  reap  w.siz
    ?-  ora
      %light   ~-~2500.  :: ─
      %heavy   ~-~2501.  :: ━
      %double  ~-~2550.  :: ═
      %arc     ~-~2500.  :: ─
    ==
  ?:  |(?=(%l dir) ?=(%r dir) ?=(%v dir))
    %+  reap  h.siz
    :_  ~
    ?-  ora
      %light   ~-~2502.  :: │
      %heavy   ~-~2503.  :: ┃
      %double  ~-~2551.  :: ║
      %arc     ~-~2502.  :: │
    ==
  ~
::
++  cogo                           :: get the collective size of elements in a list
  |=  =dei
  ^-  $@(~ [w=@ h=@])
  =/  [lef=@ top=@]
    ?~  dei
      [0 0]
    apex.cor.i.dei
  =|  [rig=@ bot=@]
  |-  ^-  $@(~ [w=@ h=@])
  ?~  dei
    ?:  |(=(0 lef) =(0 rig) =(0 top) =(0 bot))
      ~
    [(sub +(rig) lef) (sub +(bot) top)]
  =/  el-l=@
    %+  sub  x.apex.cor.i.dei
    l.marg.res.cor.i.dei
  =/  el-t=@
    %+  sub  y.apex.cor.i.dei
    t.marg.res.cor.i.dei
  =/  el-r=@
    %+  add  x.apex.cor.i.dei
    %+  add  r.marg.res.cor.i.dei
    ?:  =(0 w.size.res.cor.i.dei)
      0
    (dec w.size.res.cor.i.dei)
  =/  el-b=@
    %+  add  y.apex.cor.i.dei
    %+  add  b.marg.res.cor.i.dei
    ?:  =(0 h.size.res.cor.i.dei)
      0
    (dec h.size.res.cor.i.dei)
  %=  $
    dei  t.dei
    lef  (min el-l lef)
    top  (min el-t top)
    rig  (max el-r rig)
    bot  (max el-b bot)
  ==
::
++  mino                           :: reposition an element branch
  |=  [movx=@ movy=@ deu=deus]
  ^-  deus
  %_  deu
    x.apex.cor  (add x.apex.cor.deu movx)
    y.apex.cor  (add y.apex.cor.deu movy)
    b.gens      (turn b.gens.deu |=(d=deus ^$(deu d)))
    l.gens      (turn l.gens.deu |=(d=deus ^$(deu d)))
    n.gens      (turn n.gens.deu |=(d=deus ^$(deu d)))
  ==
::
++  laxo                           :: resolve all coordinates for an element with iter
  |=  [=iter =apex =res]
  ^-  [apex modi muri]
  =/  [x2-raw=@ y2-raw=@]
    :-  (add x.apex ?.(=(0 w.size.res) (dec w.size.res) 0))
    (add y.apex ?.(=(0 h.size.res) (dec h.size.res) 0))
  =/  [x1=@ y1=@]
    :-  ?:((lte x.iter x.apex) (sub x.apex x.iter) 0)
    ?:((lte y.iter y.apex) (sub y.apex y.iter) 0)
  =/  [x2=@ y2=@]
    :-  ?:((lte x.iter x2-raw) (sub x2-raw x.iter) 0)
    ?:((lte y.iter y2-raw) (sub y2-raw y.iter) 0)
  =/  mur=muri
    :^    =.  x.apex  ;:(add x.apex l.bord.res l.padd.res)
          ?:  (lte x.iter x.apex)  (sub x.apex x.iter)
          0
        =+  r=(add r.bord.res r.padd.res)
        =.  x2-raw  ?:((lte r x2-raw) (sub x2-raw r) 0)
        ?:  (lte x.iter x2-raw)  (sub x2-raw x.iter)
        0
      =.  y.apex  ;:(add y.apex t.bord.res t.padd.res)
      ?:  (lte y.iter y.apex)  (sub y.apex y.iter)
      0
    =+  b=(add b.bord.res b.padd.res)
    =.  y2-raw  ?:((lte b y2-raw) (sub y2-raw b) 0)
    ?:  (lte y.iter y2-raw)  (sub y2-raw y.iter)
    0
  [[x1 y1] [x2 y2] mur]
::
++  iugo                           :: make a line intersection character
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
  =?  +<  |(?=(%arc c) ?=(%arc l) ?=(%arc r) ?=(%arc t) ?=(%arc b))
    %_  +<
      c    ?:(?=(%arc c) %light c)
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
              &(?=(%~ t) ?=(%~ b) ?=(%light r) ?=([%v %light] [v c]))
      ==  ==
    ~-~251c.  ::  ├
  ?:  ?&  ?=(%~ l)
          ?|  &(?=(%light t) ?=(%light b) ?=(%heavy r))
              &(?=(%~ t) ?=(%~ b) ?=(%heavy r) ?=([%v %light] [v c]))
      ==  ==
    ~-~251d.  ::  ┝
  ?:  ?&  ?=(%~ l)
          ?|  &(?=(%light t) ?=(%light b) ?=(%double r))
              &(?=(%~ t) ?=(%~ b) ?=(%double r) ?=([%v %light] [v c]))
      ==  ==
    ~-~255e.  ::  ╞
  ?:  ?&  ?=(%~ l)
          ?|  &(?=(%heavy t) ?=(%heavy b) ?=(%heavy r))
              &(?=(%~ t) ?=(%~ b) ?=(%heavy r) ?=([%v %heavy] [v c]))
      ==  ==
    ~-~2523.  ::  ┣
  ?:  ?&  ?=(%~ l)
          ?|  &(?=(%heavy t) ?=(%heavy b) ?=(%light r))
              &(?=(%~ t) ?=(%~ b) ?=(%light r) ?=([%v %heavy] [v c]))
      ==  ==
    ~-~2520.  ::  ┠
  ?:  ?&  ?=(%~ l)
          ?|  &(?=(%double t) ?=(%double b) ?=(%double r))
              &(?=(%~ t) ?=(%~ b) ?=(%double r) ?=([%v %double] [v c]))
      ==  ==
    ~-~2560.  ::  ╠
  ?:  ?&  ?=(%~ l)
          ?|  &(?=(%double t) ?=(%double b) ?=(%light r))
              &(?=(%~ t) ?=(%~ b) ?=(%light r) ?=([%v %double] [v c]))
      ==  ==
    ~-~255f.  ::  ╟
  ?:  &(?=(%~ l) ?=(%heavy t) ?=(%light b) ?=(%light r))  ~-~251e.  ::  ┞
  ?:  &(?=(%~ l) ?=(%light t) ?=(%heavy b) ?=(%light r))  ~-~251f.  ::  ┟
  ?:  &(?=(%~ l) ?=(%heavy t) ?=(%light b) ?=(%heavy r))  ~-~2521.  ::  ┡
  ?:  &(?=(%~ l) ?=(%light t) ?=(%heavy b) ?=(%heavy r))  ~-~2522.  ::  ┢
  ?:  ?&  ?=(%~ r)
          ?|  &(?=(%light t) ?=(%light b) ?=(%light l))
              &(?=(%~ t) ?=(%~ b) ?=(%light l) ?=([%v %light] [v c]))
      ==  ==
    ~-~2524.  ::  ┤
  ?:  ?&  ?=(%~ r)
          ?|  &(?=(%light t) ?=(%light b) ?=(%heavy l))
              &(?=(%~ t) ?=(%~ b) ?=(%heavy l) ?=([%v %light] [v c]))
      ==  ==
    ~-~2525.  ::  ┥
  ?:  ?&  ?=(%~ r)
          ?|  &(?=(%light t) ?=(%light b) ?=(%double l))
              &(?=(%~ t) ?=(%~ b) ?=(%double l) ?=([%v %light] [v c]))
      ==  ==
    ~-~2561.  ::  ╡
  ?:  ?&  ?=(%~ r)
          ?|  &(?=(%heavy t) ?=(%heavy b) ?=(%heavy l))
              &(?=(%~ t) ?=(%~ b) ?=(%heavy l) ?=([%v %heavy] [v c]))
      ==  ==
    ~-~252b.  ::  ┫
  ?:  ?&  ?=(%~ r)
          ?|  &(?=(%heavy t) ?=(%heavy b) ?=(%light l))
              &(?=(%~ t) ?=(%~ b) ?=(%light l) ?=([%v %heavy] [v c]))
      ==  ==
    ~-~2528.  ::  ┨
  ?:  ?&  ?=(%~ r)
          ?|  &(?=(%double t) ?=(%double b) ?=(%double l))
              &(?=(%~ t) ?=(%~ b) ?=(%double l) ?=([%v %double] [v c]))
      ==  ==
    ~-~2563.  ::  ╣
  ?:  ?&  ?=(%~ r)
          ?|  &(?=(%double t) ?=(%double b) ?=(%light l))
              &(?=(%~ t) ?=(%~ b) ?=(%light l) ?=([%v %double] [v c]))
      ==  ==
      ~-~2562.  ::  ╢
  ?:  &(?=(%~ r) ?=(%heavy t) ?=(%light b) ?=(%light l))  ~-~2526.  ::  ┦
  ?:  &(?=(%~ r) ?=(%light t) ?=(%heavy b) ?=(%light l))  ~-~2527.  ::  ┧
  ?:  &(?=(%~ r) ?=(%heavy t) ?=(%light b) ?=(%heavy l))  ~-~2529.  ::  ┩
  ?:  &(?=(%~ r) ?=(%light t) ?=(%heavy b) ?=(%heavy l))  ~-~252a.  ::  ┪
  ?:  ?&  ?=(%~ t)
          ?|  &(?=(%light l) ?=(%light r) ?=(%light b))
              &(?=(%~ l) ?=(%~ r) ?=(%light b) ?=([%h %light] [v c]))
      ==  ==
    ~-~252c.  ::  ┬
  ?:  ?&  ?=(%~ t)
          ?|  &(?=(%light l) ?=(%light r) ?=(%heavy b))
              &(?=(%~ l) ?=(%~ r) ?=(%heavy b) ?=([%h %light] [v c]))
      ==  ==
    ~-~2530.  ::  ┰
  ?:  ?&  ?=(%~ t)
          ?|  &(?=(%light l) ?=(%light r) ?=(%double b))
              &(?=(%~ l) ?=(%~ r) ?=(%double b) ?=([%h %light] [v c]))
      ==  ==
    ~-~2565.  ::  ╥
  ?:  ?&  ?=(%~ t)
          ?|  &(?=(%heavy l) ?=(%heavy r) ?=(%heavy b))
              &(?=(%~ l) ?=(%~ r) ?=(%heavy b) ?=([%h %heavy] [v c]))
      ==  ==
    ~-~2533.  ::  ┳
  ?:  ?&  ?=(%~ t)
          ?|  &(?=(%heavy l) ?=(%heavy r) ?=(%light b))
              &(?=(%~ l) ?=(%~ r) ?=(%light b) ?=([%h %heavy] [v c]))
      ==  ==
    ~-~252f.  ::  ┯
  ?:  ?&  ?=(%~ t)
          ?|  &(?=(%double l) ?=(%double r) ?=(%double b))
              &(?=(%~ l) ?=(%~ r) ?=(%double b) ?=([%h %double] [v c]))
      ==  ==
    ~-~2566.  ::  ╦
  ?:  ?&  ?=(%~ t)
          ?|  &(?=(%double l) ?=(%double r) ?=(%light b))
              &(?=(%~ l) ?=(%~ r) ?=(%light b) ?=([%h %double] [v c]))
      ==  ==
    ~-~2564.  ::  ╤
  ?:  &(?=(%~ t) ?=(%heavy l) ?=(%light r) ?=(%light b))  ~-~252d.  ::  ┭
  ?:  &(?=(%~ t) ?=(%light l) ?=(%heavy r) ?=(%light b))  ~-~252e.  ::  ┮
  ?:  &(?=(%~ t) ?=(%heavy l) ?=(%light r) ?=(%heavy b))  ~-~2531.  ::  ┱
  ?:  &(?=(%~ t) ?=(%light l) ?=(%heavy r) ?=(%heavy b))  ~-~2532.  ::  ┲
  ?:  ?&  ?=(%~ b)
          ?|  &(?=(%light l) ?=(%light r) ?=(%light t))
              &(?=(%~ l) ?=(%~ r) ?=(%light t) ?=([%h %light] [v c]))
      ==  ==
    ~-~2534.  ::  ┴
  ?:  ?&  ?=(%~ b)
          ?|  &(?=(%light l) ?=(%light r) ?=(%heavy t))
              &(?=(%~ l) ?=(%~ r) ?=(%heavy t) ?=([%h %light] [v c]))
      ==  ==
    ~-~2538.  ::  ┸
  ?:  ?&  ?=(%~ b)
          ?|  &(?=(%light l) ?=(%light r) ?=(%double t))
              &(?=(%~ l) ?=(%~ r) ?=(%double t) ?=([%h %light] [v c]))
      ==  ==
    ~-~2568.  ::  ╨
  ?:  ?&  ?=(%~ b)
          ?|  &(?=(%heavy l) ?=(%heavy r) ?=(%heavy t))
              &(?=(%~ l) ?=(%~ r) ?=(%heavy t) ?=([%h %heavy] [v c]))
      ==  ==
    ~-~253b.  ::  ┻
  ?:  ?&  ?=(%~ b)
          ?|  &(?=(%heavy l) ?=(%heavy r) ?=(%light t))
              &(?=(%~ l) ?=(%~ r) ?=(%light t) ?=([%h %heavy] [v c]))
      ==  ==
    ~-~2537.  ::  ┷
  ?:  ?&  ?=(%~ b)
          ?|  &(?=(%double l) ?=(%double r) ?=(%double t))
              &(?=(%~ l) ?=(%~ r) ?=(%double t) ?=([%h %double] [v c]))
      ==  ==
    ~-~2569.  ::  ╩
  ?:  ?&  ?=(%~ b)
          ?|  &(?=(%double l) ?=(%double r) ?=(%light t))
              &(?=(%~ l) ?=(%~ r) ?=(%light t) ?=([%h %double] [v c]))
      ==  ==
    ~-~2567.  ::  ╧
  ?:  &(?=(%~ b) ?=(%heavy l) ?=(%light r) ?=(%light t))  ~-~2535.  ::  ┵
  ?:  &(?=(%~ b) ?=(%light l) ?=(%heavy r) ?=(%light t))  ~-~2536.  ::  ┶
  ?:  &(?=(%~ b) ?=(%heavy l) ?=(%light r) ?=(%heavy t))  ~-~2539.  ::  ┹
  ?:  &(?=(%~ b) ?=(%light l) ?=(%heavy r) ?=(%heavy t))  ~-~253a.  ::  ┺
  ?:  ?|  &(?=(%light l) ?=(%light r) ?=(%light t) ?=(%light b))
          &(?=(%light l) ?=(%light r) ?=(%~ t) ?=(%~ b) ?=([%v %light] [v c]))
          &(?=(%light t) ?=(%light b) ?=(%~ l) ?=(%~ r) ?=([%h %light] [v c]))
      ==
    ~-~253c.  ::  ┼
  ?:  ?|  &(?=(%heavy l) ?=(%heavy r) ?=(%heavy t) ?=(%heavy b))
          &(?=(%heavy l) ?=(%heavy r) ?=(%~ t) ?=(%~ b) ?=([%v %heavy] [v c]))
          &(?=(%heavy t) ?=(%heavy b) ?=(%~ l) ?=(%~ r) ?=([%h %heavy] [v c]))
      ==
    ~-~254b.  ::  ╋
  ?:  ?|  &(?=(%double l) ?=(%double r) ?=(%double t) ?=(%double b))
          &(?=(%double l) ?=(%double r) ?=(%~ t) ?=(%~ b) ?=([%v %double] [v c]))
          &(?=(%double t) ?=(%double b) ?=(%~ l) ?=(%~ r) ?=([%h %double] [v c]))
      ==
    ~-~256c.  ::  ╬
  ?:  ?|  &(?=(%light l) ?=(%light r) ?=(%heavy t) ?=(%heavy b))
          &(?=(%light l) ?=(%light r) ?=(%~ t) ?=(%~ b) ?=([%v %heavy] [v c]))
          &(?=(%heavy t) ?=(%heavy b) ?=(%~ l) ?=(%~ r) ?=([%h %light] [v c]))
      ==
    ~-~2542.  ::  ╂
  ?:  ?|  &(?=(%heavy l) ?=(%heavy r) ?=(%light t) ?=(%light b))
          &(?=(%heavy l) ?=(%heavy r) ?=(%~ t) ?=(%~ b) ?=([%v %light] [v c]))
          &(?=(%light t) ?=(%light b) ?=(%~ l) ?=(%~ r) ?=([%h %heavy] [v c]))
      ==
    ~-~253f.  ::  ┿
  ?:  ?|  &(?=(%light l) ?=(%light r) ?=(%double t) ?=(%double b))
          &(?=(%light l) ?=(%light r) ?=(%~ t) ?=(%~ b) ?=([%v %double] [v c]))
          &(?=(%double t) ?=(%double b) ?=(%~ l) ?=(%~ r) ?=([%h %light] [v c]))
      ==
    ~-~256b.  ::  ╫
  ?:  ?|  &(?=(%double l) ?=(%double r) ?=(%light t) ?=(%light b))
          &(?=(%double l) ?=(%double r) ?=(%~ t) ?=(%~ b) ?=([%v %light] [v c]))
          &(?=(%light t) ?=(%light b) ?=(%~ l) ?=(%~ r) ?=([%h %double] [v c]))
      ==
    ~-~256a.  ::  ╪
  ?:  ?|  &(?=(%light l) ?=(%heavy r) ?=(%light t) ?=(%light b))
          &(?=(%light l) ?=(%heavy r) ?=(%~ t) ?=(%~ b) ?=([%v %light] [v c]))
      ==
    ~-~253e.  ::  ┾
  ?:  ?|  &(?=(%heavy l) ?=(%light r) ?=(%light t) ?=(%light b))
          &(?=(%heavy l) ?=(%light r) ?=(%~ t) ?=(%~ b) ?=([%v %light] [v c]))
      ==
    ~-~253d.  ::  ┽
  ?:  ?|  &(?=(%heavy l) ?=(%light r) ?=(%heavy t) ?=(%heavy b))
          &(?=(%heavy l) ?=(%light r) ?=(%~ t) ?=(%~ b) ?=([%v %heavy] [v c]))
      ==
    ~-~2549.  ::  ╉
  ?:  ?|  &(?=(%light l) ?=(%heavy r) ?=(%heavy t) ?=(%heavy b))
          &(?=(%light l) ?=(%heavy r) ?=(%~ t) ?=(%~ b) ?=([%v %heavy] [v c]))
      ==
    ~-~254a.  ::  ╊
  ?:  ?|  &(?=(%light t) ?=(%heavy b) ?=(%light l) ?=(%light r))
          &(?=(%light t) ?=(%heavy b) ?=(%~ l) ?=(%~ r) ?=([%h %light] [v c]))
      ==
    ~-~2541.  ::  ╁
  ?:  ?|  &(?=(%heavy t) ?=(%light b) ?=(%light l) ?=(%light r))
          &(?=(%heavy t) ?=(%light b) ?=(%~ l) ?=(%~ r) ?=([%h %light] [v c]))
      ==
    ~-~2540.  ::  ╀
  ?:  ?|  &(?=(%heavy t) ?=(%light b) ?=(%heavy l) ?=(%heavy r))
          &(?=(%heavy t) ?=(%light b) ?=(%~ l) ?=(%~ r) ?=([%h %heavy] [v c]))
      ==
    ~-~2547.  ::  ╇
  ?:  ?|  &(?=(%light t) ?=(%heavy b) ?=(%heavy l) ?=(%heavy r))
          &(?=(%light t) ?=(%heavy b) ?=(%~ l) ?=(%~ r) ?=([%h %heavy] [v c]))
      ==
    ~-~2548.  ::  ╈
  ?:  &(?=(%light l) ?=(%heavy r) ?=(%light t) ?=(%heavy b))  ~-~2546.  ::  ╆
  ?:  &(?=(%light l) ?=(%heavy r) ?=(%heavy t) ?=(%light b))  ~-~2544.  ::  ╄
  ?:  &(?=(%heavy l) ?=(%light r) ?=(%heavy t) ?=(%light b))  ~-~2543.  ::  ╃
  ?:  &(?=(%heavy l) ?=(%light r) ?=(%light t) ?=(%heavy b))  ~-~2545.  ::  ╅
  ~-.
::
++  humo                           :: collect line intersection groups from the element tree
  |=  deu=deus
  =/  osa=ossa  [~^~ ~^~]
  =|  key=rami
  |-  ^-  ossa
  =.  osa
    ?:  ?|  ?=(%layer -.ars.cor.deu)
            ?=(%scroll -.ars.cor.deu)
        ==
      (~(put by osa) key ~)
    ?.  ?|  ?=(%border -.ars.cor.deu)
            ?=(%line -.ars.cor.deu)
        ==
      osa
    =/  ki  (feto key osa)
    =/  oz  (~(get by osa) ki)
    ?~  oz  osa
    %+  %~  put  by  osa  ki
    %+  %~  put  by  u.oz  key
    ^-  os
    =/  x  x.apex.cor.deu
    =/  y  y.apex.cor.deu
    =/  [w=@ h=@]  size.res.cor.deu
    ?+  -.ars.cor.deu  !!
      ::
        %border
      =/  o  ora.ars.cor.deu
      ?:  |(?=(%t ad.ars.cor.deu) ?=(%b ad.ars.cor.deu))
        [%h %border x ?.(=(0 w) (add x (dec w)) x) y o]
      [%v %border x y ?.(=(0 h) (add y (dec h)) y) o]
      ::
        %line
      =/  o  ora.ars.cor.deu
      ?:  ?=(%h via.ars.cor.deu)
        [%h %line x ?.(=(0 w) (add x (dec w)) x) y o]
      [%v %line x y ?.(=(0 h) (add y (dec h)) y) o]
      ::
    ==
  ?.  ?|  .?(b.gens.deu)
          .?(l.gens.deu)
          .?(n.gens.deu)
      ==
    osa
  =<  +>.q
  %^  spin
      ^-  dei
      %-  zing
      :~  b.gens.deu
          l.gens.deu
          n.gens.deu
      ==
    [*axis *ager osa]
  |=  [d=deus a=[n=axis i=ager o=ossa]]
  ^+  +<
  =/  x  (apo -.ars.cor.d)
  =?  i.a  !=(n.a x)  0
  :-  d
  %_  a
    n  x
    i  +(i.a)
    o
      %=  ^$
        deu  d
        key  (snoc key [x i.a])
        osa  o.a
      ==
  ==
::
++  feto                           :: find the key of the line intersection group to which a line belongs
  |=  [key=rami osa=ossa]
  ^-  rami
  =/  kez=(list rami)
    %+  sort  ~(tap in ~(key by osa))
    |=  [a=rami b=rami]
    (gth (lent a) (lent b))
  |-  ^-  rami
  ?~  kez  ~
  ?^  (find `(list *)`[%~ i.kez] `(list *)`[%~ key])
    i.kez
  $(kez t.kez)
::
++  coeo                           :: produce a line in vox with any intersections applied
  |=  [=cor key=rami osa=ossa]
  ^-  vox
  ?:  ?|  &(?=(%border -.ars.cor) ?=(%~ ora.ars.cor))
          &(?=(%line -.ars.cor) ?=(%~ ora.ars.cor))
      ==
    ~
  =/  k  (feto key osa)
  =/  o  (~(get by osa) k)
  ?~  o  ~
  =/  l  ~(val by u.o)
  =/  c
    ^-  (list crux)
    =/  [=via =ora]
      ?+  -.ars.cor  !!
        %border  [?:(|(?=(%t ad.ars.cor) ?=(%b ad.ars.cor)) %h %v) ora.ars.cor]
        %line    [via.ars.cor ora.ars.cor]
      ==
    =/  [x1=@ud x2=@ud]
      :-  x.apex.cor
      ?-(via %h (add x.apex.cor (dec w.size.res.cor)), %v x.apex.cor)
    =/  [y1=@ud y2=@ud]
      :-  y.apex.cor
      ?-(via %v (add y.apex.cor (dec h.size.res.cor)), %h y.apex.cor)
    %+  roll  l
    |=  [i=os a=(list crux)]
    =;  cru=(unit crux)
      ?~  cru  a
      |-  ^-  (list crux)
      ?~  a  [u.cru ~]
      ?.  =(i.u.cru i.i.a)  [i.a $(a t.a)]
      :_  t.a
      %_  i.a
        l  ?~(l.u.cru l.i.a l.u.cru)
        r  ?~(r.u.cru r.i.a r.u.cru)
        t  ?~(t.u.cru t.i.a t.u.cru)
        b  ?~(b.u.cru b.i.a b.u.cru)
      ==
    ?-  via
        %h
      ?.  ?=(%v -.i)  ~
      ?:  |((lth x.i x1) (gth x.i x2))  ~
      ?:  |(&(?=(%border p.i) =(y1 y2.i)) &(?=(%line p.i) =(y1 +(y2.i))))
        :^  ~  via  (sub x.i x1)
        ?:  =(x1 x.i)  [ora %~ ora ora.i %~]
        ?:  =(x2 x.i)  [ora ora %~ ora.i %~]
        [ora ora ora ora.i %~]
      ?:  |(&(?=(%border p.i) =(y2 y1.i)) &(?=(%line p.i) !=(0 y1.i) =(y2 (dec y1.i))))
        :^  ~  via  (sub x.i x1)
        ?:  =(x1 x.i)  [ora %~ ora %~ ora.i]
        ?:  =(x2 x.i)  [ora ora %~ %~ ora.i]
        [ora ora ora %~ ora.i]
      ~
        %v
      ?.  ?=(%h -.i)  ~
      ?:  |((lth y.i y1) (gth y.i y2))  ~
      ?:  |(&(?=(%border p.i) =(x1 x2.i)) &(?=(%line p.i) =(x1 +(x2.i))))
        :^  ~  via  (sub y.i y1)
        ?:  =(y1 y.i)  [ora ora.i %~ %~ ora]
        ?:  =(y2 y.i)  [ora ora.i %~ ora %~]
        [ora ora.i %~ ora ora]
      ?:  |(&(?=(%border p.i) =(x2 x1.i)) &(?=(%line p.i) !=(0 x1.i) =(x2 (dec x1.i))))
        :^  ~  via  (sub y.i y1)
        ?:  =(y1 y.i)  [ora %~ ora.i %~ ora]
        ?:  =(y2 y.i)  [ora %~ ora.i ora %~]
        [ora %~ ora.i ora ora]
      ~
    ==
  =<  q
  %^  spin  c
    ?+  -.ars.cor  ~
      %border  (orno size.res.cor [ad ora]:ars.cor)
      %line    (orno size.res.cor [via ora]:ars.cor)
    ==
  |=  [i=crux v=vox]
  ^+  +<
  :-  i
  =/  char  (iugo i)
  ?:  =(~-. char)  v
  ?-  -.i
    %h  ?~(v ~ v(i (snap i.v i.i char)))
    %v  (snap v i.i `lina`[char ~])
  ==
::
++  geno                           :: turn sail into element state
  |=  [loc=(unit loci) vel=vela]
  ^-  deus
  =/  m=marl                   ~[vel]
  =/  pl=fila                  [~ ~ %w]
  =/  pa=acia                  [~ ~ ~]
  =/  px=as                    [%c x.urbs.ego]
  =/  py=as                    [%c y.urbs.ego]
  =/  pow=fuga                 [%row %clip]
  =/  prx=@ud                  x.urbs.ego
  =/  pry=@ud                  y.urbs.ego
  =/  pape=apex                ?^(loc u.loc *apex)
  =/  vape=apex                pape
  =/  vir=[n=@ud o=@ud i=@ud]  [0 0 0]
  =<  ?>
      ?=(^ -)
      i
  |-  ^-  dei
  ?~  m  ~
  =/  [=vena =avis =acia =ars =lina marv=mart]
    (suo g.i.m)
  =/  wcen=bean  =(%p p.w.size.vena)
  =/  hcen=bean  =(%p p.h.size.vena)
  =?  w.size.vena  wcen
    ?:  &(=(%i p.px) ?=(%layer -.ars))
      [%i 0]
    [%c (div (mul q.w.size.vena prx) 100)]
  =?  h.size.vena  hcen
    ?:  &(=(%i p.py) ?=(%layer -.ars))
      [%i 0]
    [%c (div (mul q.h.size.vena pry) 100)]
  =?  t.marg.vena  =(%p p.t.marg.vena)
    ?:  |(=(%i p.h.size.vena) =(%p p.h.size.vena))
      [%c 0]
    [%c (div (mul q.t.marg.vena q.h.size.vena) 100)]
  =?  r.marg.vena  =(%p p.r.marg.vena)
    ?:  |(=(%i p.w.size.vena) =(%p p.w.size.vena))
      [%c 0]
    [%c (div (mul q.r.marg.vena q.w.size.vena) 100)]
  =?  b.marg.vena  =(%p p.b.marg.vena)
    ?:  |(=(%i p.h.size.vena) =(%p p.h.size.vena))
      [%c 0]
    [%c (div (mul q.b.marg.vena q.h.size.vena) 100)]
  =?  l.marg.vena  =(%p p.l.marg.vena)
    ?:  |(=(%i p.w.size.vena) =(%p p.w.size.vena))
      [%c 0]
    [%c (div (mul q.l.marg.vena q.w.size.vena) 100)]
  =?  t.padd.vena  =(%p p.t.padd.vena)
    ?:  |(=(%i p.h.size.vena) =(%p p.h.size.vena))
      [%c 0]
    [%c (div (mul q.t.padd.vena q.h.size.vena) 100)]
  =?  r.padd.vena  =(%p p.r.padd.vena)
    ?:  |(=(%i p.w.size.vena) =(%p p.w.size.vena))
      [%c 0]
    [%c (div (mul q.r.padd.vena q.w.size.vena) 100)]
  =?  b.padd.vena  =(%p p.b.padd.vena)
    ?:  |(=(%i p.h.size.vena) =(%p p.h.size.vena))
      [%c 0]
    [%c (div (mul q.b.padd.vena q.h.size.vena) 100)]
  =?  l.padd.vena  =(%p p.l.padd.vena)
    ?:  |(=(%i p.w.size.vena) =(%p p.w.size.vena))
      [%c 0]
    [%c (div (mul q.l.padd.vena q.w.size.vena) 100)]
  =?  x.flex.vena  =(%i p.w.size.vena)  0
  =?  y.flex.vena  =(%i p.h.size.vena)  0
  =?  ars  ?=(%pattern -.ars)
    ?.  &(?=(^ c.i.m) ?=(^ a.g.i.c.i.m))
      ars
    =/  bas=vox  (oro ~ ~ (tuba v.i.a.g.i.c.i.m))
    ?:  &(?=(%i p.w.size.vena) ?=(%i p.h.size.vena))
      =/  len=@ud  (roll bas |=([i=^lina a=@ud] (max a (lent i))))
      ars(vox (fuco len (lent bas) bas))
    ?:  ?=(%i p.w.size.vena)
      =/  len=@ud  (roll bas |=([i=^lina a=@ud] (max a (lent i))))
      ars(vox (fuco len q.h.size.vena bas))
    ?:  ?=(%i p.h.size.vena)
      ars(vox (fuco q.w.size.vena (lent bas) bas))
    ars(vox (fuco q.w.size.vena q.h.size.vena bas))
  =/  [bor=marl lay=marl nor=marl]
    ?:  |(?=(%text -.ars) ?=(%pattern -.ars) ?=(%input -.ars))
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
  =?  bor  &(?=(^ marv) !?=(%input -.ars))
    %+  weld  bor
    ^-  marl
    :~  [[%border-left marv] ~]  [[%border-right marv] ~]
        [[%border-top marv] ~]  [[%border-bottom marv] ~]
    ==
  =/  [bl=@ud br=@ud bt=@ud bb=@ud]
    (obeo bor)
  =^  [gro=marl aqu=aqua]  nor
    (sero flow.vena nor)
  =/  imp=bean
    ?&  !|(?=(%text -.ars) ?=(%pattern -.ars))
        ?|  =(%i p.w.size.vena)
            =(%i p.h.size.vena)
    ==  ==
  =/  fex=bean
    ?|  &(!=(0 x.flex.vena) =(%c p.w.size.vena)) 
        &(!=(0 y.flex.vena) =(%c p.h.size.vena))
    ==
  =/  wrap=bean
    ?&  !?=(%border -.ars)
        ?|  ?&  =([%row %wrap] pow)  =(%c p.w.size.vena)
                %+  gth  (add q.w.size.vena (add q.l.marg.vena q.r.marg.vena))
                ?:((lte n.vir prx) (sub prx n.vir) 0)
            ==
            ?&  =([%col %wrap] pow)  =(%c p.h.size.vena)
                %+  gth  (add q.h.size.vena (add q.t.marg.vena q.b.marg.vena))
                ?:((lte n.vir pry) (sub pry n.vir) 0)
    ==  ==  ==
  =/  wrim=bean
    ?&  !?=(%border -.ars)
        ?|  &(=([%row %wrap] pow) =(%i p.w.size.vena))
            &(=([%col %wrap] pow) =(%i p.h.size.vena))
    ==  ==
  =/  tvir=[n=@ud o=@ud i=@ud]  vir
  =?  vir  |(wrap wrim)
    ?-  d.pow
        %row
      ?:  wrap
        :-  0
        :-  i.vir
        ;:(add q.h.size.vena q.t.marg.vena q.b.marg.vena i.vir)
      ?:  wrim
        :-  0
        :-  o.vir
        i.vir
      vir
        %col
      ?:  wrap
        :-  0
        :-  i.vir
        ;:(add q.w.size.vena q.l.marg.vena q.r.marg.vena i.vir)
      ?:  wrim
        :-  0
        :-  o.vir
        i.vir
      vir
    ==
  =.  vape
    ?:  ?=(%border -.ars)
      ?-  ad.ars
          %l
        vape
          %r
        :_  y.vape
        =/  x=@ud  (add x.vape ?:(=(0 q.px) 0 (dec q.px)))
        =/  w=@ud  ?:(=(0 q.w.size.vena) 0 (dec q.w.size.vena))
        ?:((lth w x) (sub x w) 1)
          %t
        vape
          %b
        :-  x.vape
        =/  y=@ud  (add y.vape ?:(=(0 q.py) 0 (dec q.py)))
        =/  h=@ud  ?:(=(0 q.h.size.vena) 0 (dec q.h.size.vena))
        ?:((lth h y) (sub y h) 1)
      ==
    =?  vape  |(wrap wrim)
      ?-  d.pow
          %row
        :-  x.pape
        (add y.pape o.vir)
          %col
        :_  y.pape
        (add x.pape o.vir)
      ==
    :-  (add x.vape q.l.marg.vena)
    (add y.vape q.t.marg.vena)
  =/  aape=apex  vape
  =.  vape
    :-  ;:(add bl q.l.padd.vena x.vape)
    ;:(add bt q.t.padd.vena y.vape)
  =/  arx=@ud
    ?+  p.w.size.vena  0
        %c
      =/  w=@ud  ;:(add bl br q.l.padd.vena q.r.padd.vena)
      ?:((gth w q.w.size.vena) 0 (sub q.w.size.vena w))
        %i
      =/  w=@ud
        ;:  add
          q.l.marg.vena  ?:(=(%row d.pow) n.vir o.vir)
          bl  br  q.l.padd.vena  q.r.padd.vena
        ==
      ?:((gth w prx) 0 (sub prx w))
    ==
  =/  ary=@ud
    ?+  p.h.size.vena  0
        %c
      =/  h=@ud  ;:(add bt bb q.t.padd.vena q.b.padd.vena)
      ?:((gth h q.h.size.vena) 0 (sub q.h.size.vena h))
        %i
      =/  h=@ud
        ;:  add 
          q.t.marg.vena  ?:(=(%row d.pow) o.vir n.vir)
          bt  bb  q.t.padd.vena  q.b.padd.vena
        ==
      ?:((gth h pry) 0 (sub pry h))
    ==
  =/  fil=fila
    :+  ?~(d.look.vena d.pl u.d.look.vena)
      ?~(b.look.vena b.pl u.b.look.vena)
    ?~(f.look.vena f.pl u.f.look.vena)
  =/  aci=^acia
    :+  ?~(d.acia d.pa d.acia)
      ?~(b.acia b.pa b.acia)
    ?~(f.acia f.pa f.acia)
  =/  ldei=dei
    ?~  lay  ~
    %=  $
      m     lay
      px    w.size.vena
      py    h.size.vena
      pl    fil
      pa    aci
      pow   flow.vena
      prx   arx
      pry   ary
      pape  vape
      vir   [0 0 0]
    ==
  =/  ndei=dei
    ?~  nor  ~
    %=  $
      m     nor
      px    w.size.vena
      py    h.size.vena
      pl    fil
      pa    aci
      pow   flow.vena
      prx   arx
      pry   ary
      pape  vape
      vir   [0 0 0]
    ==
  =^  aqu  ndei
    ?.  .?(aqu)  [~ ndei]
    =.  aqu
      =/  nsiz  (cogo ndei)
      =/  len=@ud  (lent aqu)
      =/  rom=@ud
        ?:  ?=(%wrap b.flow.vena)  0
        ?-  d.flow.vena
          %row  ?:(?=(~ nsiz) arx ?:((lte w.nsiz arx) (sub arx w.nsiz) 0))
          %col  ?:(?=(~ nsiz) ary ?:((lte h.nsiz ary) (sub ary h.nsiz) 0))
        ==
      ?:  =(0 rom)  aqu
      =/  [bas=@ud rem=@ud]  (dvr rom len)
      =<  p
      %^  spin  aqu  [bas rem]
      |=  $:  n=[i=@ud size=@ud marg=@ud]
              a=[bas=@ud rem=@ud]
          ==
      ^+  +<
      :-  n(size ?:(=(0 rem) bas +(bas)))
      a(rem ?:(=(0 rem) 0 (dec rem)))
    =|  [i=@ud marg=@ud move=@ud de=dei aq=aqua]
    |-  ^-  [aqua dei]
    ?:  &(?=(^ aqu) =(i i.i.aqu))
      %=  $
        i     +(i)
        aq    [i.aqu(marg marg) aq]
        aqu   t.aqu
        move  (add move size.i.aqu)
        marg  0
      ==
    ?:  &(?=(~ aqu) ?=(~ ndei))
      [(flop aq) (flop de)]
    =/  [movx=@ movy=@]
      :-  ?-(d.flow.vena %row move, %col 0)
      ?-(d.flow.vena %row 0, %col move)
    ?~  ndei  $(i +(i))
    %=  $
      i     +(i)
      de    [(mino movx movy i.ndei) de]
      ndei  t.ndei
      marg
        ?-  d.flow.vena
            %row
          ;:  add
            marg
            l.marg.res.cor.i.ndei
            r.marg.res.cor.i.ndei
            w.size.res.cor.i.ndei
          ==
            %col
          ;:  add
            marg
            t.marg.res.cor.i.ndei
            b.marg.res.cor.i.ndei
            h.size.res.cor.i.ndei
          ==
        ==
    ==
  =?  gro  !=(~ gro)
    =<  p
    %^  spin  gro  aqu
    |=  [m=manx a=aqua]
    ^+  +<
    ?~  a  [m a]
    :_  t.a
    %_    m
        a.g   
      %+  weld  a.g.m
      ^-  mart
      ?-  d.flow.vena
          %row
        :~  [%w (trip (scot %ud size.i.a))]
            [%ml (trip (scot %ud marg.i.a))]
            [%mr "0"]
        ==
          %col
        :~  [%h (trip (scot %ud size.i.a))]
            [%mt (trip (scot %ud marg.i.a))]
            [%mb "0"]
        ==
      ==
    ==
  =?  ndei  .?(gro)
    %+  weld  ndei
    ^-  dei
    %=  $
      m     gro
      px    w.size.vena
      py    h.size.vena
      pl    fil
      pa    aci
      pow   flow.vena
      prx   arx
      pry   ary
      pape  vape
      vir   [0 0 0]
    ==
  =/  csiz
    ?.  |(fex imp ?=(%scroll -.ars))
      ~
    (cogo ndei)
  =?  ndei  fex
    =/  wra=(map @ud @ud)
      =|  [out=@ud siz=@ud acc=(map @ud @ud)]
      ?:  ?=(%clip b.flow.vena)
        acc
      |-  ^-  (map @ud @ud)
      ?~  ndei  ?:(=(0 siz) acc (~(put by acc) out siz))
      =/  nut=@ud
        ?-  d.flow.vena
            %row
          ?.  (lth t.marg.res.cor.i.ndei y.apex.cor.i.ndei)  1
          (sub y.apex.cor.i.ndei t.marg.res.cor.i.ndei)
            %col
          ?.  (lth l.marg.res.cor.i.ndei x.apex.cor.i.ndei)  1
          (sub x.apex.cor.i.ndei l.marg.res.cor.i.ndei)
        ==
      =/  niz=@ud
        ?-  d.flow.vena
            %row
          %+  add  w.size.res.cor.i.ndei
          (add l.marg.res.cor.i.ndei r.marg.res.cor.i.ndei)
            %col
          %+  add  h.size.res.cor.i.ndei
          (add t.marg.res.cor.i.ndei b.marg.res.cor.i.ndei)
        ==
      ?:  =(out nut)
        $(ndei t.ndei, siz (add siz niz))
      %=  $
        ndei  t.ndei
        acc   (~(put by acc) out siz)
        out   nut
        siz   niz
      ==
    %+  turn  ndei
    |=  i=deus
    =;  [movx=@ud movy=@ud]
      (mino movx movy i)
    :-  =;  x=@ud
          ?:  (gte x arx)  0
          (div (mul x.flex.vena (sub arx x)) 100)
        ?:  |(?=([%row %clip] flow.vena) ?=([%col %wrap] flow.vena))
          ?~  csiz  0
          w.csiz
        ?:  ?=([%col %clip] flow.vena)
          %+  add  w.size.res.cor.i
          (add l.marg.res.cor.i r.marg.res.cor.i)
        ?.  ?=([%row %wrap] flow.vena)  0
        =/  w=(unit @ud)
          %-  %~  get  by  wra
          ?.  (lth t.marg.res.cor.i y.apex.cor.i)  1
          (sub y.apex.cor.i t.marg.res.cor.i)
        ?^(w u.w 0)
    =;  y=@ud
      ?:  (gte y ary)  0
      (div (mul y.flex.vena (sub ary y)) 100)
    ?:  |(?=([%col %clip] flow.vena) ?=([%row %wrap] flow.vena))     
      ?~  csiz  0
      h.csiz
    ?:  ?=([%row %clip] flow.vena)
      %+  add  h.size.res.cor.i
      (add t.marg.res.cor.i b.marg.res.cor.i)
    ?.  ?=([%col %wrap] flow.vena)  0
    =/  h=(unit @ud)
      %-  %~  get  by  wra
      ?.  (lth l.marg.res.cor.i x.apex.cor.i)  1
      (sub x.apex.cor.i l.marg.res.cor.i)
    ?^(h u.h 0)
  =?  csiz  &(|(imp ?=(%scroll -.ars)) ?=(^ ldei))
    (cogo (weld ndei ldei))
  =?  size.vena  &(imp ?=(^ csiz))
    :-  ?:  =(%i p.w.size.vena)  
          [%c ;:(add bl br q.l.padd.vena q.r.padd.vena w.csiz)]
        w.size.vena
    ?:  =(%i p.h.size.vena)  
      [%c ;:(add bt bb q.t.padd.vena q.b.padd.vena h.csiz)]
    h.size.vena
  =/  wris=bean
    ?&  wrim
        ?|  (gth n.vir prx)  (gth n.vir pry)
            ?&  =(%row d.pow)
                %+  gth  (add q.w.size.vena (add q.l.marg.vena q.r.marg.vena))
                ?:((lte n.tvir prx) (sub prx n.tvir) 0)
            ==
            ?&  =(%col d.pow)
                %+  gth  (add q.h.size.vena (add q.t.marg.vena q.b.marg.vena))
                ?:((lte n.tvir pry) (sub pry n.tvir) 0)
    ==  ==  ==
  =?  vir  wrim
    ?.  wris  tvir
    ?-  d.pow
        %row
      :-  0
      :-  i.vir
      (add i.vir (add q.h.size.vena (add q.t.marg.vena q.b.marg.vena)))
        %col
      :-  0
      :-  i.vir
      (add i.vir (add q.w.size.vena (add q.l.marg.vena q.r.marg.vena)))
    ==
  =?  vape  wrim
    ?:  wris
      ?-  d.pow
          %row
        :-  x.vape
        (add y.vape (sub i.tvir o.tvir))
          %col
        :_  y.vape
        (add x.vape (sub i.tvir o.tvir))
      ==
    ?-  d.pow
        %row
      :_  y.vape
      (add x.vape n.tvir)
        %col
      :-  x.vape
      (add y.vape n.tvir)
    ==
  =?  aape  wrim
    ?:  wris
      ?-  d.pow
          %row
        :-  x.aape
        (add y.aape (sub i.tvir o.tvir))
          %col
        :_  y.aape
        (add x.aape (sub i.tvir o.tvir))
      ==
    ?-  d.pow
        %row
      :_  y.aape
      (add x.aape n.tvir)
        %col
      :-  x.aape
      (add y.aape n.tvir)
    ==
  =?  arx  wrim
    =/  bp=@ud  ;:(add bl br q.l.padd.vena q.r.padd.vena)
    ?:((gth bp q.w.size.vena) 0 (sub q.w.size.vena bp))
  =?  ary  wrim
    =/  bp=@ud  ;:(add bt bb q.t.padd.vena q.b.padd.vena)
    ?:((gth bp q.h.size.vena) 0 (sub q.h.size.vena bp))
  =>  ?.  wrim  .
    =;  [movx=@ud movy=@ud]
      %_  +
        ndei  (turn ndei |=(i=deus (mino movx movy i)))
        ldei  (turn ldei |=(i=deus (mino movx movy i)))
      ==
    :-  ?:  &(wris ?=(%col d.pow))
          (sub i.tvir o.tvir)
        ?:  &(!wris ?=(%row d.pow))
          n.tvir
        0
    ?:  &(wris ?=(%row d.pow))
      (sub i.tvir o.tvir)
    ?:  &(!wris ?=(%col d.pow))
      n.tvir
    0
  =?  ars  ?=(%text -.ars)
    =/  [x=@ud y=@ud]
      :-  ?:(?=(%row d.pow) n.vir o.vir)
      ?:(?=(%col d.pow) n.vir o.vir)
    %_    ars
        vox
      %^    oro
          ?:(?=(%i p.px) ~ [~ ?:((lte x prx) (sub prx x) 0)])
        ?:(?=(%i p.py) ~ [~ ?:((lte y pry) (sub pry y) 0)])
      lina
    ==
  =/  ares=res
    ?.  ?=(%text -.ars)
      :*  ?.  ?=(%pattern -.ars)  [q.w.size.vena q.h.size.vena]
          [?^(vox.ars (lent i.vox.ars) 0) (lent vox.ars)]
          [q.l.padd.vena q.r.padd.vena q.t.padd.vena q.b.padd.vena]
          [q.l.marg.vena q.r.marg.vena q.t.marg.vena q.b.marg.vena]
          [bl br bt bb]
          flex.vena
          flow.vena
          fil
          aci(d ~)
      ==
    =/  len=@ud
      (roll ^-(vox vox.ars) |=([i=^lina a=@ud] (max a (pono i))))
    =/  lim=(unit @ud)
      ?:(?=(%i p.px) ~ [~ (sub prx ?:(?=(%row d.pow) n.vir o.vir))])
    :*  [?~(lim len (min len u.lim)) (lent vox.ars)]
        [0 0 0 0]
        [0 0 0 0]
        [0 0]
        [%row %wrap]
        pl
        aci
    ==
  =/  bdei=dei
    ?~  bor  ~
    %=  $
      m     bor
      px    w.size.vena
      py    h.size.vena
      pl    fil
      pa    aci
      pow   flow.ares
      prx   w.size.ares
      pry   h.size.ares
      pape  aape
      vape  aape
      vir   [0 0 0]
    ==
  :: TO DO: geno state transitions require building rami
  :: 
  :: =?  ars
  ::     ?|  ?=(%input -.ars)
  ::         ?=(%scroll -.ars)
  ::         ?=(%checkbox -.ars)
  ::     ==
  ::   =/  key=(unit rami)
  ::     ?~  avis  [~ k]
  ::     (~(get by aves.ara) u.avis)
  ::   ?+  -.ars  ars
  ::       %input
  ::     =/  old=(unit ens)
  ::       ?~  key  ~
  ::       (~(get by esse.ara) u.key)
  ::     ?.  &(?=(^ old) ?=(%input -.ars.u.old))
  ::       ?~  lina  ars
  ::       ars(vox (oro [~ w.size.ares] [~ h.size.ares] lina))
  ::     ?:  =(size.res.u.old size.ares)
  ::       ars.u.old
  ::     %_  ars.u.old
  ::       ab   0
  ::       i    [0 0]
  ::       vox  (oro [~ w.size.ares] [~ h.size.ares] ^-(^lina (zing vox.ars.u.old)))
  ::     ==
  ::       %checkbox
  ::     ?~  key  ars
  ::     =/  old=(unit ens)  (~(get by esse.ara) u.key)
  ::     ?~  old  ars
  ::     ?.  ?=(%checkbox -.ars.u.old)  ars
  ::     ars.u.old
  ::       %scroll
  ::     ?~  key  ars
  ::     =/  sol=sola
  ::       ?~  csiz  [0 0]
  ::       :-  ?:((gth arx w.csiz) 0 (sub w.csiz arx))
  ::       ?:((gth ary h.csiz) 0 (sub h.csiz ary))
  ::     =/  old=(unit ens)  (~(get by esse.ara) u.key)
  ::     =/  itr=iter
  ::       ?~  old
  ::         [0 0]
  ::       ?.  ?=(%scroll -.ars.u.old)
  ::         [0 0]
  ::       :-  (min x.iter.ars.u.old x.sol)
  ::       (min y.iter.ars.u.old y.sol)
  ::     [%scroll itr [bl br bt bb] sol]
  ::   ==
  :: 
  =.  vir
    ?:  |(?=(%layer -.ars) ?=(%border -.ars))
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
  =.  vape
    ?:  |(?=(%layer -.ars) ?=(%border -.ars))  pape
    =/  vx=@ud  ?-(d.pow %row n.vir, %col o.vir)
    =/  vy=@ud  ?-(d.pow %row o.vir, %col n.vir)
    [(add x.pape vx) (add y.pape vy)]
  :-  ^-(deus [[aape avis ares ars] [bdei ldei ndei]])
  $(m t.m)
::
++  creo                           :: produce an element branch by key, along with rendering context
  |=  [key=rami deu=deus]
  ^-  [aer deus]
  =|  ki=rami
  =|  ayr=aer
  =.  muri.ayr  [1 x.urbs.ego 1 y.urbs.ego]
  ?~  key
    [ayr deu]
  |-  ^-  [aer deus]
  =<  ?~  t.key
        [ayr deu]
      $(key t.key, ki [i.key ki])
  =/  [[x1=@ y1=@] [x2=@ y2=@] room=muri]
    (laxo iter.ayr apex.cor.deu res.cor.deu)
  %_  .
    ::
    deu
      %+  snag  ager.i.key
      ?-  axis.i.key
        %n  n.gens.deu
        %b  b.gens.deu
        %l  l.gens.deu
      ==
    ::
    iter.ayr
      ?.  ?=(%scroll -.ars.cor.deu)  iter.ayr
      :-  (add x.iter.ayr x.iter.ars.cor.deu)
      (add y.iter.ayr y.iter.ars.cor.deu)
    ::
    muri.ayr
      :^    (max l.room l.muri.ayr)
          (min r.room r.muri.ayr)
        (max t.room t.muri.ayr)
      (min b.room b.muri.ayr)
    ::
    rex.ayr
      ?.  (paro -.ars.cor.deu)  rex.ayr
      [~ [(flop ki) x1 x2 y1 y2]]
    ::
    luna.ayr
      ?~  l.gens.deu  luna.ayr
      =/  els=dei
        %+  roll
          ?.  ?=(%l axis.i.key)
            l.gens.deu
          (scag ager.i.key `dei`l.gens.deu)
        |=  [d=deus a=dei]
        (weld a n.gens.d)
      |-  ^-  luna
      ?~  els  luna.ayr
      ?:  |(=(0 w.size.res.cor.i.els) =(0 h.size.res.cor.i.els))
        $(els t.els)
      =/  ros=(list lux)
        %+  reap  h.size.res.cor.i.els
        :+  x.apex.cor.i.els
          (add x.apex.cor.i.els (dec w.size.res.cor.i.els))
        ~
      %=  $
        els  t.els
        luna.ayr
          =<  +.q
          %^  spin  ros
            [y.apex.cor.i.els luna.ayr]
          |=  [i=lux a=[y=@ud =luna]]
          ^+  +<
          ?:  (gte y.iter.ayr y.a)
            [i +(y.a) luna.a]
          =/  y  (sub y.a y.iter.ayr)
          =/  l  (~(get by luna.a) y)
          :-  i
          :-  +(y.a)
          %+  %~  put
                by
              luna.a
            y
          ?~  l  [i ~]
          |-  ^-  (list lux)
          ?~  u.l
            [i ~]
          ?:  |((lth x2.i x1.i.u.l) (gth x1.i x2.i.u.l))
            [i u.l]
          ?:  &((gte x1.i x1.i.u.l) (lte x2.i x2.i.u.l))
            u.l
          ?:  (lth x1.i x1.i.u.l)
            u.l(x1.i x1.i)
          u.l(x2.i x2.i)
      ==
    ::
  ==
::
++  viso                           :: build a render schematic from a branch
  |=  [key=rami xer=rex osa=ossa ayr=aer deu=deus]
  ^-  sol
  ?:  ?|  =(0 w.size.res.cor.deu)
          =(0 h.size.res.cor.deu)
      ==
    ~
  =/  a-y1=@ud
    ?:  =(0 y.iter.ayr)  y.apex.cor.deu
    ?.  (lth y.iter.ayr y.apex.cor.deu)  1
    (max (sub y.apex.cor.deu y.iter.ayr) t.muri.ayr)
  =/  a-y2=@ud
    =/  y2  (add y.apex.cor.deu (dec h.size.res.cor.deu))
    =?  y2  !=(0 y.iter.ayr)
      ?.  (lte y.iter.ayr y2)  0
      (sub y2 y.iter.ayr)
    (min y2 b.muri.ayr)
  ?:  ?|  (gth a-y1 b.muri.ayr)
          (lth a-y2 t.muri.ayr)
      ==
    ~
  =/  acc=sol
    %+  reap
      +((sub a-y2 a-y1))
    *(list lux)
  =?  acc  .?(luna.ayr)
    %+  spun  acc
    |=  [i=(list lux) n=@ud]
    ^-  [(list lux) @ud]
    =/  y  (add y.apex.cor.deu n)
    ?:  (gte y.iter.ayr y)
      [i +(n)]
    =.  y  (sub y y.iter.ayr)
    =/  l  (~(get by luna.ayr) y)
    ?~  l
      [i +(n)]
    [u.l +(n)]
  |-  ^-  sol
  =/  [[x1=@ y1=@] [x2=@ y2=@] room=muri]
    (laxo iter.ayr apex.cor.deu res.cor.deu)
  ?:  ?|  =(0 w.size.res.cor.deu)
          =(0 h.size.res.cor.deu)
          (gth x1 r.muri.ayr)
          (lth x2 l.muri.ayr)
          (gth y1 b.muri.ayr)
          (lth y2 t.muri.ayr)
      ==
    acc
  =.  rex.ayr   ?:((paro -.ars.cor.deu) [key x1 x2 y1 y2] rex.ayr)
  =.  acc
    =:  iter.ayr
          ?.  ?=(%scroll -.ars.cor.deu)  iter.ayr
          :-  (add x.iter.ayr x.iter.ars.cor.deu)
          (add y.iter.ayr y.iter.ars.cor.deu)
        muri.ayr
          :^    (max l.room l.muri.ayr)
              (min r.room r.muri.ayr)
            (max t.room t.muri.ayr)
          (min b.room b.muri.ayr)
      ==
    =<  +>.q
    %^  spin
        ^-  dei
        %-  zing
        :~  b.gens.deu
            l.gens.deu
            n.gens.deu
        ==
      [*axis *ager acc]
    |=  [d=deus a=[n=axis i=ager s=sol]]
    ^+  +<
    =/  x  (apo -.ars.cor.d)
    =?  i.a  !=(n.a x)  0
    :-  d
    %_  a
      n  x
      i  +(i.a)
      s
        %=  ^$
          deu  d
          key  (snoc key [x i.a])
          acc  s.a
        ==
    ==
  ?:  ?|  ?=(%layer -.ars.cor.deu)
          &(?=(%border -.ars.cor.deu) ?=(%~ ora.ars.cor.deu))
      ==
    acc
  =/  a-i1=@ud   (sub (max y1 t.muri.ayr) a-y1)
  =/  a-i2=@ud   (sub (min y2 b.muri.ayr) a-y1)
  =/  look=fila
    ?.  ?&  ?=(^ xer)
            ?=(^ rex.ayr)
            =(k.xer k.rex.ayr)
        ==
      look.res.cor.deu
    sele.res.cor.deu
  =;  rend=sol
    %+  weld  (scag a-i1 acc)
    %+  weld  rend
    (slag +(a-i2) acc)
  =<  p
  %^  spin  `sol`(swag [a-i1 +((sub a-i2 a-i1))] acc)
    =;  v=vox
      ?:  (gte t.room t.muri.ayr)  v
      =/  n  (sub t.muri.ayr t.room)
      (oust [0 n] v)
    ?+  -.ars.cor.deu  ~
      %text      vox.ars.cor.deu
      %pattern   vox.ars.cor.deu
      %input     vox.ars.cor.deu
      %checkbox  ?:(v.ars.cor.deu [[~-~2588. ~] ~] ~)
      %border    (coeo cor.deu key osa)
      %line      (coeo cor.deu key osa)
    ==
  |=  [l=(list lux) xov=vox]
  ^+  +<
  :_  ?^(xov t.xov ~)
  |-  ^-  (list lux)
  =/  tok=lux
    :*  x1
        x2
        look
        rex.ayr
        ^-  lina
        ?~  xov  ~
        ?~  i.xov  ~
        =/  len  (lent i.xov)
        =/  wid  +((sub x2 x1))
        ?:  =(len wid)  i.xov
        ?:  (gth len wid)  (scag wid `lina`i.xov)
        %+  weld  i.xov
        %+  reap  (sub wid len)
        ~-.
    ==
  ?>  ?=(^ p.tok)
  ?~  l
    [tok l]
  ?:  (lth x2 x1.i.l)
    [tok l]
  ?:  (gth x1 x2.i.l)
    [i.l $(l t.l)]
  ?:  ?&  (gte x1 x1.i.l)
          (lte x2 x2.i.l)
      ==
    l
  ?:  ?&  (lth x1 x1.i.l)
          (gth x2 x2.i.l)
      ==
    :+  %_  tok
          x2     (dec x1.i.l)
          txt.p  ?:(.?(txt.p.tok) (scag (sub x1.i.l x1) txt.p.tok) ~)
        ==
      i.l
    %=  $
      l    t.l
      x1   +(x2.i.l)
      xov
        ?~  xov  ~
        xov(i (oust [0 +((sub x2.i.l x1))] i.xov))
    ==
  ?:  (lth x1 x1.i.l)
    :_  l
    %_  tok
      x2     (dec x1.i.l)
      txt.p  ?:(.?(txt.p.tok) (scag (sub x1.i.l x1) txt.p.tok) ~)
    ==
  :-  i.l
  %=  $
    l    t.l
    x1   +(x2.i.l)
    xov
      ?~  xov  ~
      xov(i (oust [0 +((sub x2.i.l x1))] i.xov))
  ==
::
++  gyro                           :: produce interactivity state from a render schematic
  |=  [=apex =sol]
  ^-  ordo
  %-  flop
  =<  +.q
  %^  spin  sol
    [y.apex *ordo]
  |=  [l=(list lux) a=[y=@ud =ordo]]
  ^+  +<
  :+  l
    +(y.a)
  =<  q
  %^  spin  l
    ordo.a
  |=  [i=lux ord=ordo]
  ^+  +<
  :-  i
  ?.  &(?=(^ p.i) ?=(^ nav.p.i))
    ord
  ?:  (lien ord |=(d=dux =(k.nav.p.i k.d)))
    ord
  [nav.p.i ord]
::
++  ligo                           :: merge interactivity state from a local update to global
  |=  [key=rami loc=ordo gob=ordo]
  ^-  ordo
  %+  weld  loc
  %+  skip  gob
  |=  i=dux
  ?|  =(key k.i)
      (alo key k.i)
  ==
::
++  dico                           :: turn a render schematic into text
  |=  [=apex =sol]
  ^-  tape
  ?.  .?  sol
    :~  '\\x1b['
        (scot %ud y.apex)  ';'
        (scot %ud x.apex)  'H'
    ==
  %-  zing
  =;  [p=wall q=[y=@ f=fila]]
    ^-  wall
    ?~  d.f.q  p
    (snoc p "\\x1b[0m")
  %^  spin  sol  [y.apex *fila]
  |=  [lis=(list lux) acc=[y=@ f=fila]]
  =;  [p=wall q=[? fil=fila]]
    ^-  [tape [@ fila]]
    :_  [+(y.acc) fil.q]
    %-  zing
    :_  p
    ^-  tape
    :~  '\\x1b['
        (scot %ud y.acc)   ';'
        (scot %ud x.apex)  'H'
    ==
  %^  spin  lis
    [| f.acc]
  |=  [=lux [jump=? fil=fila]]
  ^-  [tape [? fila]]
  ?~  p.lux
    [~ & fil]
  :_  [| fil.p.lux]
  =/  od=?  .?(d.fil)
  =/  nd=?  .?(d.fil.p.lux)
  =/  nb=(unit tint)  ?.(=(b.fil b.fil.p.lux) [~ b.fil.p.lux] ~)
  =/  nf=(unit tint)  ?.(=(f.fil f.fil.p.lux) [~ f.fil.p.lux] ~)
  |-  ^-  tape
  ?:  jump
    :-  '\\x1b['
    :+  (scot %ud y.acc)   ';'
    :+  (scot %ud x1.lux)  'H'
    $(jump |)
  ?:  od
    =/  ds=(list deco)  ~(tap in d.fil)
    |-  ^-  tape
    ?~  ds  ^$(od |)
    ?:  ?=(%~ i.ds)  $(ds t.ds)
    :-  '\\x1b['
    :+  ?-(i.ds %bl '25', %br '22', %un '24')
      'm'
    $(ds t.ds)
  ?:  nd
    =/  ds=(list deco)  ~(tap in d.fil.p.lux)
    |-  ^-  tape
    ?~  ds  ^$(nd |)
    ?:  ?=(%~ i.ds)  $(ds t.ds)
    :-  '\\x1b['
    :+  ?-(i.ds %bl '5', %br '1', %un '4')
      'm'
    $(ds t.ds)
  ?^  nb
    :-  '\\x1b['
    ?@  u.nb
      :^    '4'
          ?-  u.nb
            %r  '1'  %g  '2'  %b  '4'
            %c  '6'  %m  '5'  %y  '3'
            %k  '0'  %w  '7'  %~  '9'
          ==
        'm'
      $(nb ~)
    :^  '4'  '8'  ';'
    :+  '2'  ';'
    :+  (scot %ud (@ r.u.nb))  ';'
    :+  (scot %ud (@ g.u.nb))  ';'
    :+  (scot %ud (@ b.u.nb))  'm'
    $(nb ~)
  ?^  nf
    :-  '\\x1b['
    ?@  u.nf
      :^    '3'
          ?-  u.nf
            %r  '1'  %g  '2'  %b  '4'
            %c  '6'  %m  '5'  %y  '3'
            %k  '0'  %w  '7'  %~  '9'
          ==
        'm'
      $(nf ~)
    :^  '3'  '8'  ';'
    :+  '2'  ';'
    :+  (scot %ud (@ r.u.nf))  ';'
    :+  (scot %ud (@ g.u.nf))  ';'
    :+  (scot %ud (@ b.u.nf))  'm'
    $(nf ~)
  ?~  txt.p.lux
    (reap +((sub x2.lux x1.lux)) ' ')
  (tufa txt.p.lux)
::
--
