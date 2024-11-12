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
  $:  size=[w=@ud h=@ud]                                               ::   size
      padd=muri                                                        ::   padding
      marg=muri                                                        ::   margin
      bord=muri                                                        ::   border sizes
      flex=modi                                                        ::   child element positioning: justify
      flow=fuga                                                        ::   child element positioning: sequence
      look=fila                                                        ::   style
      sele=acia                                                        ::   select style
  ==                                                                   ::
+$  ars                                                                :: element types
  $%  [%text =vox]                                                     ::
      [%pattern =vox]                                                  ::
      [%layer ~]                                                       ::
      [%scroll =equi =iter =sola]                                      ::
      [%border =ad =ora]                                               ::
      [%line =ab =ora]                                                 ::
      [%select pro=?(%submit %~)]                                      ::
      [%input de=@ud i=loci =vox]                                      ::
      [%checkbox v=? t=vox f=vox]                                      ::
      [%radio ~]                                                       ::
      [%form ~]                                                        ::
      [%$ ~]                                                           ::
  ==                                                                   ::
+$  avis  path                                                         :: identifier
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
+$  equi  [u=(unit as) d=(unit as)]                                    :: scroll triggers
+$  ad    ?(%l %r %t %b)                                               :: direction
+$  ab    ?(%h %v)                                                     :: orientation
+$  ora   ?(%light %heavy %double %arc %~)                             :: line style
+$  luna  (map @ud (list lux))                                         :: render blocking context (key is y)
+$  sol   (list (list lux))                                            :: render schematic matrix
+$  lux                                                                :: render token
  $:  x1=@ud                                                           ::   beginning of segment
      x2=@ud                                                           ::   end of segment
      $=  p                                                            ::   segment content
      $@  ~                                                            ::   in rendering, null p is a gap
      $:  fil=fila                                                     ::   segment style
          nav=rex                                                      ::   possible navigation point
          txt=lina                                                     ::   possible character content (else space)
  ==  ==                                                               ::
+$  aer                                                                :: branch render context
  $:  =iter                                                            ::   cumulative scroll
      =muri                                                            ::   cumulative limit coordinates
      nav=[sty=? =rex]                                                 ::   nav hierarchy (do select style, originating element)
      =rex                                                             ::   global active selection
      =ossa                                                            ::   global line intersections
      =luna                                                            ::   layer blocking
  ==                                                                   ::
+$  opus  (list [=apex =sol])                                          :: render batch (hop on null sol)
+$  dux   [n=@tas k=rami =avis muri]                                   :: navigation point
+$  rex   $@(~ dux)                                                    :: selection
+$  ordo  (list dux)                                                   :: navigation context
+$  omen  (map nota lex)                                               :: internal hotkey context
+$  zona                                                               :: terminal input
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
      %ins  %del  %tog  %act  %clk  %def
  ==
+$  ales  (map nota avis)
+$  aves  (map avis rami)                                              :: ids to keys for a session
+$  ossa  (map rami (map rami os))                                     :: line groups for a session
+$  os                                                                 :: line data for intersections 
  $%  [%h p=?(%border %line) x1=@ud x2=@ud y=@ud =ora]                 ::
      [%v p=?(%border %line) x=@ud y1=@ud y2=@ud =ora]                 ::
  ==                                                                   ::
+$  crux  [v=ab i=@ud c=ora l=ora r=ora t=ora b=ora]                   :: line intersection
+$  as    $%((pair %c @ud) (pair %p @ud) (pair %i @ud))                :: size unit
+$  data  (map avis @t)                                                :: form data
+$  aqua  (list [i=@ud size=@ud marg=@ud])                             :: geno grow sizing state
+$  vena                                                               :: geno res building state
  $:  size=[w=as h=as]                                                 ::
      padd=[l=as r=as t=as b=as]                                       ::
      marg=[l=as r=as t=as b=as]                                       ::
      flex=modi                                                        ::
      flow=fuga                                                        ::
      look=acia                                                        ::
  ==                                                                   ::
+$  vela  manx                                                         :: sail
+$  fons  (pair @p @tas)                                               :: session source
+$  ara                                                                :: session state
  $:  =fons                                                            ::
      =vela                                                            ::
      =aves                                                            ::
      =ossa                                                            ::
      =deus                                                            ::
  ==                                                                   ::
+$  arae  (list ara)                                                   :: sessions
+$  via                                                                :: frame state
  $:  =rex                                                             ::
      =ordo                                                            ::
      =area                                                            ::
      =arae                                                            ::
  ==                                                                   ::
+$  viae  (list via)                                                   :: frames
+$  area  [=aula =deus]                                                :: frame layout state
+$  aula                                                               :: frame layout specification
  $%  [%h h=@]                                                         ::
      [%v v=@]                                                         ::
      [%h-v1 h=@ v1=@]                                                 ::
      [%h-v2 h=@ v2=@]                                                 ::
      [%v-h1 v=@ h1=@]                                                 ::
      [%v-h2 v=@ h2=@]                                                 ::
      [%h1-h2-v1-v2 h1=@ h2=@ v1=@ v2=@]                               ::
      [%$ ~]                                                           ::
  ==                                                                   ::
+$  arx   [open=? =via]                                                :: menu state
+$  urbs  [=arca =arx]                                                 :: system state
+$  arca  [x=@ y=@]                                                    :: terminal size
+$  cura  @                                                            :: active frame index
+$  ego                                                                :: %homunculus state
  $:  =cura                                                            ::
      =urbs                                                            ::
      =viae                                                            ::
  ==                                                                   ::
::                                                                     ::
+$  card  card:agent:gall                                              ::
--                                                                     
::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  
=|  =ego
^-  agent:gall
=<
|_  bol=bowl:gall
+*  hoc  .
++  on-init
  ^-  (quip card _hoc)
  =.  viae.ego  ~[*via]
  [~ hoc]
++  on-save
  ^-  vase
  !>(~)
++  on-load
  |=  old=vase
  =.  viae.ego  ~[*via]
  ^-  (quip card _hoc)
  [~ hoc]
::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  
++  on-poke
  |=  [=mark =vase]
  ^-  (quip card _hoc)
  ?>  =(our.bol src.bol)
  ?+  mark  !!
    ::
      %homunculus-update
    =+  !<(upd=update:homunculus vase)
    =/  fon=fons  (bibo bol)
    ?+  -.upd  [~ hoc]
      ::
        %full
      =/  ind  (rigo fon)
      ?~  ind
        =/  last=via  (rear viae.ego)
        :: temporary ::
        ?:  (gte (lent arae.last) 4)
          =|  =via
          =|  =ara
          =:  fons.ara  fon
              vela.ara  p.upd
            ==
          =:  cura.ego  (lent viae.ego)
              viae.ego  (snoc viae.ego via(arae ~[ara]))
            ==
          =^  cards  ego  (apto cura.ego)
          [cards hoc]
        ::
        =|  =ara
        =/  i          (dec (lent viae.ego))
        =.  arae.last  (snoc arae.last ara(fons fon, vela p.upd))
        =:  cura.ego   i
            viae.ego   (snap viae.ego i last)
          ==
        =^  cards  ego  (apto cura.ego)
        [cards hoc]
      ::
      =/  =via  (snag -.ind viae.ego)
      =/  =ara  (snag +.ind arae.via)
      =/  key=rami  ~[[%n +.ind]]
      =.  viae.ego
        %^  snap  viae.ego  -.ind
        %_    via
            arae
          %^  snap  arae.via  +.ind
          (curo key deus.ara(l.gens ~, n.gens ~) ara(vela p.upd))
        ==
      ?.  =(cura.ego -.ind)
        [~ hoc]
      =/  ren  (viso key)
      =?  viae.ego  !open.arx.urbs.ego  
        (snap viae.ego -.ind via(ordo (ligo key (duco ren) ordo.via)))
      :_  hoc
      :~  (fio ~[ren])
      ==
      ::
        %branch
      =/  ind  (rigo fon)
      ?~  ind  [~ hoc]
      =/  =via  (snag -.ind viae.ego)
      =/  =ara  (snag +.ind arae.via)
      =^  keys  ara  (gyro p.upd ara)
      =.  arae.via   (snap arae.via +.ind ara)
      =.  viae.ego   (snap viae.ego -.ind via)
      ?.  =(cura.ego -.ind)
        [~ hoc]
      =^  opus  via  (levo keys via)
      =.  viae.ego   (snap viae.ego -.ind via)
      :_  hoc
      :~  (fio opus)
      ==
      ::
    ==
    ::
      %homunculus-menu
    =+  !<(=menu:homunculus vase)
    ?+  -.menu  [~ hoc]
      ::
        %update
      ?+  -.p.menu  [~ hoc]
        ::
          %full
        =.  arae.via.arx.urbs.ego
          ?~  arae.via.arx.urbs.ego
            =|  =ara
            ~[ara(vela p.p.menu)]
          arae.via.arx.urbs.ego(vela.i p.p.menu)
        ?>  ?=(^ arae.via.arx.urbs.ego)
        =/  key=rami  ~[[%l 0]]
        =/  =via      (snag cura.ego viae.ego)
        =.  deus.i.arae.via.arx.urbs.ego
          %:  geno
            [key cor.deus.area.via]
            i.arae.via.arx.urbs.ego
            vela.i.arae.via.arx.urbs.ego
          ==
        =/  [ave=aves osa=ossa]
          (vivo key deus.i.arae.via.arx.urbs.ego)
        =:  aves.i.arae.via.arx.urbs.ego  ave
            ossa.i.arae.via.arx.urbs.ego  osa
          ==
        ?.  open.arx.urbs.ego
          [~ hoc]
        =/  ren  (viso key)
        =.  ordo.via.arx.urbs.ego  (duco ren)
        :_  hoc
        :~  (fio ~[ren])
        ==
        ::
          %branch
        ?~  arae.via.arx.urbs.ego  [~ hoc]
        =^  keys  i.arae.via.arx.urbs.ego
          (gyro p.p.menu i.arae.via.arx.urbs.ego)
        ?.  open.arx.urbs.ego
          [~ hoc]
        =^  =opus  via.arx.urbs.ego
          =<  ?>(?=(^ arae) .)
          (levo keys via.arx.urbs.ego)
        :_  hoc
        :~  (fio opus)
        ==
        ::
      ==
      ::
    ==
    ::
      %json
    =/  zon  (ineo !<(json vase))
    :: ~&  >  zon
    ?~  zon
      [~ hoc]
      ::
    ?:  ?=(%rez -.u.zon)
      =.  arca.urbs.ego  +.u.zon
      =^  cards  ego  (apto cura.ego)
      [cards hoc]
      ::
    ?:  ?=(%esc -.u.zon)
      =.  open.arx.urbs.ego  !open.arx.urbs.ego
      =/  ren  (viso ~)
      =.  ego
        ?:  open.arx.urbs.ego
          ego(ordo.via.arx.urbs (duco ren))
        =/  =via  (snag cura.ego viae.ego)
        ego(viae (snap viae.ego cura.ego via(ordo (duco ren))))
      :_  hoc
      :~  (fio ~[ren])
      ==
      ::
    =/  =via  ?.(open.arx.urbs.ego (snag cura.ego viae.ego) via.arx.urbs.ego)
    =/  lek=(unit lex)  (~(get by (scio rex.via)) (noto u.zon))
    ?~  lek  [~ hoc]
    =^  cards  ego  (muto u.lek u.zon via)
    [cards hoc]
    ::
  ==
::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  
++  on-watch
  |=  =path
  ^-  (quip card _hoc)
  ?>  =(our.bol src.bol)
  ?+  path  !!
    ::
      [%homunculus-http ~]
    [~ hoc]
    ::
  ==
++  on-leave
  |=  =path
  ^-  (quip card _hoc)
  ?+  path  !!
    ::
      [%dill @ ~]
    [~ hoc]
    ::
  ==
++  on-peek
  |=  =path
  ^-  (unit (unit cage))
  ?+  path  ~
    [%x %auth ~]  [~ [~ [%noun !>(~)]]]
  ==
::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  
++  on-agent  |=([wire sign:agent:gall] ^-((quip card _hoc) !!))
++  on-arvo   |=([wire sign-arvo] ^-((quip card _hoc) !!))
++  on-fail   |=([term tang] ^-((quip card _hoc) !!))
--
::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  
|%
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
++  bibo                           :: get the source from a bowl
  |=  bol=bowl:gall
  ^-  fons
  [src.bol ?:(&(?=(^ sap.bol) ?=(^ t.sap.bol)) i.t.sap.bol %$)]
::
++  fio                            :: make a display update card
  |=  =opus
  ^-  card
  =;  txt=@t
    [%give %fact ~[/homunculus-http] %json !>(`json`[%s txt])]
  %-  crip
  %-  zing
  ^-  wall
  %+  turn  (snoc opus [vado ~])
  |=  i=[apex sol]
  (dico i)
::
++  iuvo                           :: make an event card
  |=  [typ=@tas =fons =avis =data]
  ^-  card
  ?>  ?=(^ avis)
  =;  eve=event:homunculus
    [%pass ~ %agent fons %poke %homunculus-event !>(eve)]
  ?+  typ    !!
    %select  [typ avis]
    %act     [typ avis]
    %hotkey  [typ avis]
    %form    [typ avis data]
  ==
::
++  scio                           :: derive system hotkey context from a selection
  |_  =rex
  ++  $
    ^-  omen
    ?~  rex    nav
    ?+  n.rex  nav
      %input   inp
    ==
  ++  nav
    ^-  omen
    %-  malt
    ^-  (list [nota lex])
    :~  [[%aro %l] %nav-l]  [[%aro %r] %nav-r]
        [[%aro %u] %nav-u]  [[%aro %d] %nav-d]
        [[%whe %u] %scr-u]  [[%whe %d] %scr-d]
        [[%clk %u] %clk]  [[%clk %d] %clk]
        [[%ret ~] %act]
    ==
  ++  inp
    ^-  omen
    %-  malt
    ^-  (list [nota lex])
    :~  [[%aro %l] %cur-l]  [[%aro %r] %cur-r]
        [[%aro %u] %cur-u]  [[%aro %d] %cur-d]
        [[%whe %u] %scr-u]  [[%whe %d] %scr-d]
        [[%clk %u] %clk]  [[%clk %d] %clk]
        [[%txt ~] %ins]  [[%bac ~] %del]
    ==
  --
::
++  apto                           :: reevaluate a frame
  |=  =cura
  ^-  (quip card ^ego)
  =/  =via    (snag cura viae.ego)
  =/  len=@   (lent arae.via)
  :: temporary ::
  =/  w=tape  ((d-co:co 1) x.arca.urbs.ego)
  =/  h=tape  ((d-co:co 1) y.arca.urbs.ego)
  =/  vel=vela
    ;box(w w, h h, fl "row-wrap")
      ;*  ^-  marl
          ?+  len  ~
            %1  ~[;box(w "100%", h "100%");]
            %2  ~[;box(w "50%", h "100%"); ;box(w "50%", h "100%");]
            %3  ~[;box(w "50%", h "50%"); ;box(w "50%", h "50%"); ;box(w "100%", h "50%");]
            %4  ~[;box(w "50%", h "50%"); ;box(w "50%", h "50%"); ;box(w "50%", h "50%"); ;box(w "50%", h "50%");]
          ==
    ==
  ::  ::  ::  ::
  =/  deu=deus  (geno ~ ~ vel)
  =:  deus.area.via
        %_  deu
          gens  ~^~^~
        ==
      arae.via.arx.urbs.ego
        ?~  arae.via.arx.urbs.ego  arae.via.arx.urbs.ego
        =/  k=rami  ~[[%l 0]]
        =.  deus.i.arae.via.arx.urbs.ego
          %:  geno
            [k cor.deu]
            i.arae.via.arx.urbs.ego
            vela.i.arae.via.arx.urbs.ego
          ==
        =/  [ave=aves osa=ossa]
          (vivo k deus.i.arae.via.arx.urbs.ego)
        %_  arae.via.arx.urbs.ego
          aves.i  ave
          ossa.i  osa
        ==
      arae.via
        %+  spun  arae.via
        |=  [=ara i=@]
        :_  +(i)
        (curo ~[[%n i]] (snag i n.gens.deu) ara)
    ==
  =.  viae.ego  (snap viae.ego cura via)
  ?.  =(cura cura.ego)
    [~ ego]
  =/  ren  (viso ~)
  =.  ego
    ?.  open.arx.urbs.ego
      ego(viae (snap viae.ego cura via(ordo (duco ren))))
    ego(ordo.via.arx.urbs (duco ren))
  :_  ego
  :~  (fio ~[ren])
  ==
::
++  curo                           :: reevaluate a session's element state
  |=  [key=rami con=deus ses=ara]
  ^-  ara
  =/  axi=axis  (apo ?@(n.g.vela.ses n.g.vela.ses -.n.g.vela.ses))
  =.  deus.ses
    =.  key   ?>(?=(^ key) key(t ~[[axi 0]]))
    =/  =dei  ~[(geno [key cor.con] ses vela.ses)]
    ?+  axi  con
      %n  con(n.gens dei)
      %l  con(l.gens dei)
    ==
  =/  [ave=aves osa=ossa]  (vivo key deus.ses)
  ses(aves ave, ossa osa)
::
++  gyro                           :: resolve session state for a list of branch updates
  |=  [upd=(list vela) ses=ara]
  =|  keys=(list rami)
  |-  ^-  (quip rami ara)
  ?~  upd
    [keys ses]
  =^  key  ses  (indo i.upd ses)
  %=  $
    upd   t.upd
    keys  [key keys]
  ==
::
++  levo                           :: render a list of branches in a frame by key
  |=  [keys=(list rami) fam=via]
  =|  rens=opus
  |-  ^-  [opus via]
  ?~  keys
    [rens fam]
  =/  ren  (viso i.keys)
  %=  $
    keys      t.keys
    rens      [ren rens]
    ordo.fam  (ligo i.keys (duco ren) ordo.fam)
  ==
::
++  indo                           :: apply a branch update to a session
  |=  [new=vela =ara]
  ^-  [rami ^ara]
  =/  id-attr
    |-  ^-  [mane tape]
    ?~  a.g.new  ~&(>>> %missing-id-on-branch-update !!)
    ?:  =(%href n.i.a.g.new)  i.a.g.new
    $(a.g.new t.a.g.new)
  =/  [new-v=vena avi=avis *]  (suo g.new)
  =.  vela.ara
    |-  ^-  vela
    ?~  (find ~[id-attr] a.g.vela.ara)
      %_  vela.ara
        c  (turn c.vela.ara |=(i=vela ^$(vela.ara i)))
      ==
    =/  [old-v=vena *]  (suo g.vela.ara)
    ?:  ?|  ?=(%i p.w.size.old-v)
            ?=(%i p.h.size.old-v)
            !=(size.old-v size.new-v)
            !=(marg.old-v marg.new-v)
        ==
      ~&(>>> %invalid-branch-update !!)
    new
  =/  key=rami  (~(got by aves.ara) avi)
  =/  old-el    (exuo key deus.ara)
  =/  size=mart
    :~  [%w ((d-co:co 1) w.size.res.cor.old-el)]
        [%h ((d-co:co 1) h.size.res.cor.old-el)]
    ==
  =.  deus.ara
    %^    paco
        key
      (geno [key cor.old-el] ara new(a.g (weld a.g.new size)))
    deus.ara
  =/  [ave=aves osa=ossa]  (vivo ?^(key ~[i.key] ~) deus.ara)
  :-  key
  ara(aves ave, ossa osa)
::
++  rigo                           :: find an existing session in a frame by source, or null
  |=  =fons
  =|  i=@
  |-  ^-  $@(~ [@ @])
  ?~  viae.ego  ~
  =|  j=@
  |-  ^-  $@(~ [@ @])
  ?~  arae.i.viae.ego
    %=  ^$
      i  +(i)
      viae.ego  t.viae.ego
    ==
  ?:  =(fons fons.i.arae.i.viae.ego)
    [i j]
  %=  $
    j  +(j)
    arae.i.viae.ego  t.arae.i.viae.ego
  ==
::
++  voro                           :: relativize a key to a session branch
  |=  key=rami
  ^-  rami
  :: keys descend from the global root.
  :: each child element is a session branch.
  ?~  key  !!  t.key
::
++  exuo                           :: get an element from a session branch by key (or crash)
  |=  [key=rami deu=deus]
  ^-  deus
  =.  key  (voro key)
  |-  ^-  deus
  ?~  key  deu
  %=  $
    key  t.key
    deu
      %+  snag  ager.i.key
      ?-  axis.i.key
        %n  n.gens.deu
        %b  b.gens.deu
        %l  l.gens.deu
      ==
  ==
::
++  paco                           :: replace an element in a session branch by key
  |=  [key=rami new=deus ses=deus]
  ^-  deus
  =.  key  (voro key)
  |-  ^-  deus
  ?~  key  new
  =/  nex=deus
    %+  snag  ager.i.key
    ?-  axis.i.key
      %n  n.gens.ses
      %b  b.gens.ses
      %l  l.gens.ses
    ==
  ?-  axis.i.key
    %n  ses(n.gens (snap n.gens.ses ager.i.key $(ses nex, key t.key)))
    %b  ses(b.gens (snap b.gens.ses ager.i.key $(ses nex, key t.key)))
    %l  ses(l.gens (snap l.gens.ses ager.i.key $(ses nex, key t.key)))
  ==
::
++  muto                           :: handle an input event for a frame
  |_  [=lex zon=zona =via]
  ++  $
    ^-  (quip card ^ego)
    ?:  ?|  ?=(%nav-l lex)  ?=(%nav-r lex)
            ?=(%nav-u lex)  ?=(%nav-d lex)
        ==
      eo
    ?:  ?=(%ins lex)
      cibo
    ?:  ?=(%del lex)
      abdo
    ?:  ?|  ?=(%cur-l lex)  ?=(%cur-r lex)
            ?=(%cur-u lex)  ?=(%cur-d lex)
        ==
      loco
    ?:  ?=(%act lex)
      moto
    ::
    [~ ego]
  ::
  ++  sto                          :: get the active session
    ^-  ara
    ?.  &(?=(^ rex.via) ?=(^ k.rex.via))
      ?~  arae.via  !!
      i.arae.via
    (snag ager.i.k.rex.via arae.via)
  ::
  ++  sido                         :: save the active session in the active frame
    |=  =ara
    ^-  ^via
    ?.  &(?=(^ rex.via) ?=(^ k.rex.via))
      via
    via(arae (snap arae.via ager.i.k.rex.via ara))
  ::
  ++  novo                         :: save the active frame in ego
    |=  =^via
    ^-  ^ego
    ?:  open.arx.urbs.ego
      ego(via.arx.urbs via)
    ego(viae (snap viae.ego cura.ego via))
  ::
  ++  eo                           :: handle a navigation event
    ^-  (quip card ^ego)
    =/  navs  (gero rex.via ordo.via)
    =/  [cards=(list card) active-scroll=(unit [=iter =rami =deus])]  fluo
    =/  navs-in-scroll
      ^-  ordo
      ?~  active-scroll  ~
      %+  skim  `ordo`navs
      |=  =dux
      (alo rami.u.active-scroll k.dux)
    ?:  ?&  ?=(^ active-scroll)
            |(?=(%nav-u lex) ?=(%nav-d lex))
            ?|  ?=(~ navs-in-scroll)
                ^-  ?
                =;  [[x1=@ y1=@] [x2=@ y2=@] room=muri]
                  ?+  lex  |
                    %nav-u  (lte t.i.navs-in-scroll t.room)
                    %nav-d  (gte b.i.navs-in-scroll b.room)
                  ==
                %^    laxo
                    iter.u.active-scroll
                  apex.cor.deus.u.active-scroll
                res.cor.deus.u.active-scroll
        ==  ==
      ?>  ?=(%scroll -.ars.cor.deus.u.active-scroll)
      =/  ses=ara   sto
      =:  rex.via
            ?~  navs-in-scroll
              rex.via
            i.navs-in-scroll
          y.iter.ars.cor.deus.u.active-scroll
            ?+  lex     y.iter.ars.cor.deus.u.active-scroll
              %nav-u    (dec y.iter.ars.cor.deus.u.active-scroll)
              %nav-d    +(y.iter.ars.cor.deus.u.active-scroll)
            ==
        ==
      =.  via       (sido ses(deus (paco rami.u.active-scroll deus.u.active-scroll deus.ses)))
      =.  ego       (novo via)
      =/  ren       (viso rami.u.active-scroll)
      =.  ordo.via  (ligo rami.u.active-scroll (duco ren) ordo.via)
      =.  rex.via
        ?~  rex.via  ~
        =/  upd  (rogo k.rex.via ordo.via)
        ?~  upd  rex.via  upd
      :_  (novo via)
      %+  weld  cards
      :-  (fio ~[ren])
      ?.  &(?=(^ navs-in-scroll) ?=(^ rex.via) ?=(^ avis.rex.via))
        ~
      :~  (iuvo %select fons.ses avis.rex.via ~)
      ==
    =/  next=rex
      ?^  navs-in-scroll  i.navs-in-scroll
      ?^  navs  i.navs  ~
    =/  old-rex  rex.via
    ?:  ?|  ?=(~ next)
            &(?=(^ old-rex) =(k.old-rex k.next))
        ==
      [cards ego]
    =/  old=$@(~ deus)
      ?~  old-rex  ~
      (exuo k.old-rex deus:sto)
    =.  rex.via   next
    =/  new-ara   sto
    =/  new=deus  (exuo k.next deus.new-ara)
    =.  ego       (novo via)
    =/  rend-old
      ?.  ?&  ?=(^ old)
              ?=(^ old-rex)
              (adeo [-.ars.cor.old sele.res.cor.old])
          ==
        [*apex *sol]
      (viso k.old-rex)
    =/  rend-new
      ?.  (adeo -.ars.cor.new sele.res.cor.new)
        [*apex *sol]
      (viso k.next)
    :_  ego
    :-  (fio ~[rend-old rend-new])
    ?~  avis.cor.new
      cards
    :_  cards
    (iuvo %select fons.new-ara avis.cor.new ~)
  ::
  ++  fluo                         :: find the nearest scroll parent not maxed out in the nav direction + collect trigger events
    ^-  (quip card (unit [iter rami deus]))
    ?~  rex.via  ~^~
    =/  key=rami      ?>(?=(^ k.rex.via) [i.k.rex.via ~])
    =/  rex-key=rami  (voro k.rex.via)
    =|  itr=iter
    =|  [car=(list card) acc=(unit [iter rami deus])]
    =/  =ara  sto
    |-  ^-  (quip card (unit [iter rami deus]))
    =:  itr
          ?.  ?=(%scroll -.ars.cor.deus.ara)  itr
          itr(y (add y.itr y.iter.ars.cor.deus.ara))
        car
          ?.  ?=(%scroll -.ars.cor.deus.ara)  car
          ?:  ?&  ?=(%nav-u lex)
                  ?=(^ u.equi.ars.cor.deus.ara)
              ==
            =/  trig=@
              ?+  p.u.u.equi.ars.cor.deus.ara  0
                %c  q.u.u.equi.ars.cor.deus.ara
                %p
                  %+  div
                    %+  mul  q.u.u.equi.ars.cor.deus.ara
                    y.sola.ars.cor.deus.ara
                  100
              ==
            ?.  (lte y.iter.ars.cor.deus.ara trig)  car
            [(cedo fons.ara %up avis.cor.deus.ara) car]
          ?:  ?&  ?=(%nav-d lex)
                  ?=(^ d.equi.ars.cor.deus.ara)
              ==
            =/  trig=@
              ?+  p.u.d.equi.ars.cor.deus.ara  0
                %c  q.u.d.equi.ars.cor.deus.ara
                %p
                  %+  div
                    %+  mul  q.u.d.equi.ars.cor.deus.ara
                    y.sola.ars.cor.deus.ara
                  100
              ==
            ?.  %+  gte  (add trig y.iter.ars.cor.deus.ara)
                y.sola.ars.cor.deus.ara
              car
            [(cedo fons.ara %down avis.cor.deus.ara) car]
          car
        acc
          ?.  ?&  ?=(%scroll -.ars.cor.deus.ara)
                  ?!
                  ?|  &(?=(%nav-u lex) =(0 y.iter.ars.cor.deus.ara))
                      &(?=(%nav-d lex) =(y.sola.ars.cor.deus.ara y.iter.ars.cor.deus.ara))
              ==  ==
            acc
          [~ [itr key deus.ara]]
      ==
    ?~  rex-key
      [car acc]
    %=  $
      key      (snoc key i.rex-key)
      rex-key  t.rex-key
      deus.ara
        %+  snag  ager.i.rex-key
        ?-  axis.i.rex-key
          %n  n.gens.deus.ara
          %b  b.gens.deus.ara
          %l  l.gens.deus.ara
        ==
    ==
  ::
  ++  cedo                         :: make a scroll trigger event card
    |=  [=fons dir=?(%up %down) =avis]
    ^-  card
    =/  eve=event:homunculus  [%scroll %trigger dir avis]
    [%pass ~ %agent fons %poke %homunculus-event !>(eve)]
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
  ++  tego                         :: check if either element a or element b is in a layer above
    |=  [a=rami b=rami]
    ^-  ?(%a %b %~)
    ?~  a  %~
    ?~  b  %~
    ?~  t.a  %~
    ?~  t.b  %~
    ?.  =(axis.i.t.a axis.i.t.b)
      ?:  &(?=(%l axis.i.t.a) ?=(%n axis.i.t.b))
        %a
      ?:  &(?=(%n axis.i.t.a) ?=(%l axis.i.t.b))
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
    |_  rex=dux
    ++  $
      |=  =dux
      ^-  bean
      ?:  =(k.dux k.rex)  |
      ?+  lex  |
        %nav-l  (f lth dux l.dux r.dux l.rex l.rex)
        %nav-u  (f lth dux t.dux b.dux t.rex t.rex)
        %nav-r  (f gth dux l.dux l.dux l.rex r.rex)
        %nav-d  (f gth dux t.dux t.dux t.rex b.rex)
      ==
    ++  f
      |=  [than=$-([@ @] ?) =dux d1=@ d2=@ r1=@ r2=@]
      ^-  bean
      ?:  (taxo +>+.dux +>+.rex)
        ?:  =(d1 r1)
          |((alo k.dux k.rex) =(%b (tego k.rex k.dux)))
        (than d1 r1)
      ?.  =(%~ (tego k.rex k.dux))
        (than d1 r1)
      (than d2 r2)
    --
  ::
  ++  reor                         :: get the euclidean distance between a selection and a navigation point
    |=  [=dux rex=dux]
    ^-  @ud
    =;  pyt=(pair @ud @ud)
      (add (mul 10 p.pyt) q.pyt)
    ?+  lex  [0 0]
        %nav-l
      ?:  |((taxo +>+.dux +>+.rex) !=(%~ (tego k.rex k.dux)))
        %-  sqt  %+  add
          (pow ?:((lte l.dux l.rex) (sub l.rex l.dux) (sub l.dux l.rex)) 2)
        (pow (mul ?:((gte t.rex t.dux) (sub t.rex t.dux) (sub t.dux t.rex)) 2) 2)
      %-  sqt  %+  add
        (pow ?:((lte r.dux l.rex) (sub l.rex r.dux) (sub r.dux l.rex)) 2)
      (pow (mul ?:((gte t.rex t.dux) (sub t.rex t.dux) (sub t.dux t.rex)) 2) 2)
        %nav-u
      ?:  |((taxo +>+.dux +>+.rex) !=(%~ (tego k.rex k.dux)))
        %-  sqt  %+  add
          (pow ?:((gte l.dux l.rex) (sub l.dux l.rex) (sub l.rex l.dux)) 2)
        (pow (mul ?:((lte t.dux t.rex) (sub t.rex t.dux) (sub t.dux t.rex)) 2) 2)
      %-  sqt  %+  add
        (pow ?:((gte l.dux l.rex) (sub l.dux l.rex) (sub l.rex l.dux)) 2)
      (pow (mul ?:((lte b.dux t.rex) (sub t.rex b.dux) (sub b.dux t.rex)) 2) 2)
        %nav-r
      ?:  |((taxo +>+.dux +>+.rex) !=(%~ (tego k.rex k.dux)))
        %-  sqt  %+  add
          (pow ?:((lte l.rex l.dux) (sub l.dux l.rex) (sub l.rex l.dux)) 2)
        (pow (mul ?:((gte t.dux t.rex) (sub t.dux t.rex) (sub t.rex t.dux)) 2) 2)
      %-  sqt  %+  add
        (pow ?:((lte r.rex l.dux) (sub l.dux r.rex) (sub r.rex l.dux)) 2)
      (pow (mul ?:((gte t.dux t.rex) (sub t.dux t.rex) (sub t.rex t.dux)) 2) 2)
        %nav-d
      ?:  |((taxo +>+.dux +>+.rex) !=(%~ (tego k.rex k.dux)))
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
  ++  cibo                         :: handle an insert event
    ^-  (quip card ^ego)
    ?.  &(?=(%txt -.zon) ?=(^ p.zon) ?=(^ rex.via))
      [~ ego]
    =/  ses=ara  sto
    =/  el=deus  (exuo k.rex.via deus.ses)
    ?.  ?=(%input -.ars.cor.el)
      [~ ego]
    =.  ars.cor.el
      ?:  =(1 h.size.res.cor.el)
        =.  vox.ars.cor.el
          :_  ~
          ?~  vox.ars.cor.el  ?~(t.p.zon ~[i.p.zon] p.zon)
          ?~  t.p.zon  (into i.vox.ars.cor.el x.i.ars.cor.el i.p.zon)
          %+  weld  (weld (scag +(x.i.ars.cor.el) i.vox.ars.cor.el) p.zon)
          (slag +(x.i.ars.cor.el) i.vox.ars.cor.el)
        =.  i.ars.cor.el
          ?~  vox.ars.cor.el  i.ars.cor.el
          =/  x=@ud  +(x.i.ars.cor.el)
          ?:  (gth x (lent i.vox.ars.cor.el))
            i.ars.cor.el
          [x y.i.ars.cor.el]
        %_  ars.cor.el
          de
            ?:  (lth (sub x.i.ars.cor.el de.ars.cor.el) w.size.res.cor.el)
              de.ars.cor.el
            +(de.ars.cor.el)
        ==
      =/  row=lina
        ?~  vox.ars.cor.el  ~
        (snag y.i.ars.cor.el `vox`vox.ars.cor.el)
      =/  wup=bean
        ?.  ?&  =(~-. i.p.zon)
                !=(0 y.i.ars.cor.el)
                (lth x.i.ars.cor.el (sumo row))
            ==
          |
        %+  gte  w.size.res.cor.el
        %+  add  x.i.ars.cor.el
        %-  lent
        ?~  vox.ars.cor.el  ~
        (snag (dec y.i.ars.cor.el) `vox`vox.ars.cor.el)
      =.  row
        ?~  t.p.zon  (into row x.i.ars.cor.el i.p.zon)
        %+  weld  (weld (scag +(x.i.ars.cor.el) row) p.zon)
        (slag +(x.i.ars.cor.el) row)
      ?:  wup
        =.  vox.ars.cor.el
          %+  weld
            (scag (dec y.i.ars.cor.el) vox.ars.cor.el)
          %:  oro
            [~ w.size.res.cor.el]
            [~ h.size.res.cor.el]
            ^-  lina
            %-  zing
            :+  `lina`(snag (dec y.i.ars.cor.el) `vox`vox.ars.cor.el)
              row
            (slag +(y.i.ars.cor.el) vox.ars.cor.el)
          ==
        %_  ars.cor.el
          i  [?~(t.p.zon 0 (pono (flop p.zon))) y.i.ars.cor.el]
        ==
      =/  len=@ud  (pono row)
      ?:  (lte len w.size.res.cor.el)
        %_  ars.cor.el
          vox  (snap vox.ars.cor.el y.i.ars.cor.el row)
          x.i  +(x.i.ars.cor.el)
        ==
      =.  vox.ars.cor.el
        %+  weld
          (scag y.i.ars.cor.el vox.ars.cor.el)
        %:  oro
          [~ w.size.res.cor.el]
          [~ h.size.res.cor.el]
          `lina`(zing [row (slag +(y.i.ars.cor.el) vox.ars.cor.el)])
        ==
      =.  i.ars.cor.el
        =/  nlen=(unit @ud)
          ?~  vox.ars.cor.el  ~
          [~ (lent (snag y.i.ars.cor.el `vox`vox.ars.cor.el))]
        =/  npos=(unit @ud)
          ?:  |(?=(~ nlen) (lte +(x.i.ars.cor.el) u.nlen))  ~
          [~ (sub +(x.i.ars.cor.el) u.nlen)]
        ?~  npos
          [+(x.i.ars.cor.el) y.i.ars.cor.el]
        [u.npos +(y.i.ars.cor.el)]
      =.  de.ars.cor.el
        ?:  (lth (sub y.i.ars.cor.el de.ars.cor.el) h.size.res.cor.el)
          de.ars.cor.el
        +(de.ars.cor.el)
      ars.cor.el
    =.  ego  (novo (sido ses(deus (paco k.rex.via el deus.ses))))
    :_  ego
    :~  (fio ~[(viso k.rex.via)])
    ==
  ::
  ++  abdo                         :: handle a delete event
    ^-  (quip card ^ego)
    ?~  rex.via  [~ ego]
    =/  ses=ara  sto
    =/  el=deus  (exuo k.rex.via deus.ses)
    ?.  ?=(%input -.ars.cor.el)
      [~ ego]
    =.  ars.cor.el
      ?:  =(1 h.size.res.cor.el)
        ?~  vox.ars.cor.el  ars.cor.el
        ?:  =(0 x.i.ars.cor.el)  ars.cor.el
        =.  x.i.ars.cor.el  (dec x.i.ars.cor.el)
        =.  i.vox.ars.cor.el  (oust [x.i.ars.cor.el 1] i.vox.ars.cor.el)
        =.  de.ars.cor.el
          ?:  &((lte x.i.ars.cor.el de.ars.cor.el) !=(0 de.ars.cor.el))
            ?:  (gth w.size.res.cor.el de.ars.cor.el)  0
            +((sub de.ars.cor.el w.size.res.cor.el))
          de.ars.cor.el
        ars.cor.el
      ?:  =([0 0] i.ars.cor.el)  ars.cor.el
      =/  arow=lina
        ?:  |(=(0 y.i.ars.cor.el) ?=(~ vox.ars.cor.el))  ~
        (snag (dec y.i.ars.cor.el) `vox`vox.ars.cor.el)
      =?  arow  =(0 x.i.ars.cor.el)
        (snip arow)
      =/  alen=@ud  (lent arow)
      =/  row=lina
        ?~  vox.ars.cor.el  ~
        (snag y.i.ars.cor.el `vox`vox.ars.cor.el)
      =?  row  !=(0 x.i.ars.cor.el)
        (oust [(dec x.i.ars.cor.el) 1] row)
      ?:  ?&  ?=(^ arow)
              ?|  ?&  (lth alen w.size.res.cor.el)
                      %+  lte  (sumo row)
                      ?:  (lte alen w.size.res.cor.el)
                      (sub w.size.res.cor.el alen)  0
                  ==
                  ?&  |(=(0 x.i.ars.cor.el) =(1 x.i.ars.cor.el))
                      !=(~-. (rear arow))
          ==  ==  ==
        =.  vox.ars.cor.el
          %+  weld
            ?:  =(0 y.i.ars.cor.el)  ~
            `vox`(scag (dec y.i.ars.cor.el) `vox`vox.ars.cor.el)
          %:  oro
            [~ w.size.res.cor.el]
            [~ h.size.res.cor.el]
            ^-  lina
            %-  zing
            [arow row `vox`(slag +(y.i.ars.cor.el) `vox`vox.ars.cor.el)]
          ==
        =.  i.ars.cor.el
          =/  nlen=@ud
            ?:  |(=(0 y.i.ars.cor.el) ?=(~ vox.ars.cor.el))  0
            (lent (snag (dec y.i.ars.cor.el) `vox`vox.ars.cor.el))
          =/  lend=@ud  ?:((lte nlen alen) (sub alen nlen) 0)
          ?.  =(0 lend)
            [lend y.i.ars.cor.el]
          :_  ?:(=(0 y.i.ars.cor.el) 0 (dec y.i.ars.cor.el))
          ?:  =(0 x.i.ars.cor.el)  alen
          (add ?:(=(0 alen) 0 (dec alen)) x.i.ars.cor.el)
        %_  ars.cor.el
          de
            ?:  (lth y.i.ars.cor.el de.ars.cor.el)
              (dec de.ars.cor.el)
            de.ars.cor.el
        ==
      =/  len=@ud  (lent row)
      =/  brow=lina
        ?:  ?|  ?=(~ vox.ars.cor.el)
                (lth (lent vox.ars.cor.el) +(+(y.i.ars.cor.el)))
            ==
          ~
        (snag +(y.i.ars.cor.el) `vox`vox.ars.cor.el)
      =/  bwor=@ud  (sumo brow)
      ?.  ?|  ?&  (lth len w.size.res.cor.el)
                  (lte bwor (sub w.size.res.cor.el len))
              ==
              &(?=(^ row) =(~-. (rear row)))
          ==
        %_  ars.cor.el
          vox  (snap vox.ars.cor.el y.i.ars.cor.el row)
          x.i  ?:(=(0 x.i.ars.cor.el) 0 (dec x.i.ars.cor.el))
        ==
      =.  vox.ars.cor.el
        %+  weld
          (scag y.i.ars.cor.el vox.ars.cor.el)
        %:  oro
          [~ w.size.res.cor.el]
          [~ h.size.res.cor.el]
          `lina`(zing [row brow (slag +(+(y.i.ars.cor.el)) vox.ars.cor.el)])
        ==
      =.  i.ars.cor.el
        ?:  &(=(0 y.i.ars.cor.el) !=(0 x.i.ars.cor.el))
          [(dec x.i.ars.cor.el) 0]
        ?.  |(=(0 x.i.ars.cor.el) =(1 x.i.ars.cor.el))
          [(dec x.i.ars.cor.el) y.i.ars.cor.el]
        =.  y.i.ars.cor.el  (dec y.i.ars.cor.el)
        =/  l=@ud
          (lent `lina`(snag y.i.ars.cor.el `vox`vox.ars.cor.el))
        [?:(=(0 l) 0 (dec l)) y.i.ars.cor.el]
      %_  ars.cor.el
        de
          ?:  (lth y.i.ars.cor.el de.ars.cor.el)
            (dec de.ars.cor.el)
          de.ars.cor.el
      ==
    =.  ego  (novo (sido ses(deus (paco k.rex.via el deus.ses))))
    :_  ego
    :~  (fio ~[(viso k.rex.via)])
    ==
  ::
  ++  loco                         :: handle an input cursor move event
    ^-  (quip card ^ego)
    ?~  rex.via  [~ ego]
    =/  ses=ara  sto
    =/  el=deus  (exuo k.rex.via deus.ses)
    ?.  ?=(%input -.ars.cor.el)
      [~ ego]
    =/  oi=loci  i.ars.cor.el
    =.  i.ars.cor.el
      ?~  vox.ars.cor.el  i.ars.cor.el
      ?+  lex  i.ars.cor.el
          %cur-l
        ?:  =(1 h.size.res.cor.el)
          [?:(=(0 x.i.ars.cor.el) 0 (dec x.i.ars.cor.el)) y.i.ars.cor.el]
        ?:  =(0 x.i.ars.cor.el)
          ?:  =(0 y.i.ars.cor.el)  i.ars.cor.el
          =.  y.i.ars.cor.el  (dec y.i.ars.cor.el)
          =/  l=@ud  (lent `lina`(snag y.i.ars.cor.el `vox`vox.ars.cor.el))
          [?:(=(0 l) 0 (dec l)) y.i.ars.cor.el]
        [(dec x.i.ars.cor.el) y.i.ars.cor.el]
          %cur-r
        ?:  =(1 h.size.res.cor.el)
          =/  x=@ud  +(x.i.ars.cor.el)
          ?:  (gth x (lent i.vox.ars.cor.el))
            i.ars.cor.el
          [x y.i.ars.cor.el]
        =/  x=@ud  +(x.i.ars.cor.el)
        =/  l=@ud  (lent `lina`(snag y.i.ars.cor.el `vox`vox.ars.cor.el))
        ?:  (gth x l)
          =/  y=@ud  +(y.i.ars.cor.el)
          ?:  (gte y (lent vox.ars.cor.el))
            i.ars.cor.el
          [0 y]
        [x y.i.ars.cor.el]
          %cur-u
        ?:  =(1 h.size.res.cor.el)  [0 0]
        ?:  =(0 y.i.ars.cor.el)  [0 0]
        =.  y.i.ars.cor.el  (dec y.i.ars.cor.el)
        =/  l=@ud  (pono (snag y.i.ars.cor.el `vox`vox.ars.cor.el))
        :_  y.i.ars.cor.el
        ?:  (gth x.i.ars.cor.el l)  l
        x.i.ars.cor.el
          %cur-d
        ?:  =(1 h.size.res.cor.el)
          [(lent i.vox.ars.cor.el) y.i.ars.cor.el]
        =/  y=@ud  +(y.i.ars.cor.el)
        ?:  (gte y (lent vox.ars.cor.el))
          [(lent (rear vox.ars.cor.el)) y.i.ars.cor.el]
        =.  y.i.ars.cor.el  y
        =/  l=@ud  (pono (snag y.i.ars.cor.el `vox`vox.ars.cor.el))
        :_  y.i.ars.cor.el
        ?:  (gth x.i.ars.cor.el l)  l
        x.i.ars.cor.el
      ==
    ?:  =(oi i.ars.cor.el)
      =.  lex
        ?+  lex   lex
          %cur-l  %nav-l
          %cur-r  %nav-r
          %cur-u  %nav-u
          %cur-d  %nav-d
        ==
      eo
    =/  oab=@ud  de.ars.cor.el
    =.  de.ars.cor.el
      ?~  vox.ars.cor.el  de.ars.cor.el
      ?:  |(?=(%cur-l lex) ?=(%cur-u lex))
        ?:  =(0 de.ars.cor.el)  0
        ?:  =(1 h.size.res.cor.el)
          ?:  ?=(%cur-u lex)  0
          ?:  (lte x.i.ars.cor.el de.ars.cor.el)
            (dec de.ars.cor.el)
          de.ars.cor.el
        ?:  (lth y.i.ars.cor.el de.ars.cor.el)
          (dec de.ars.cor.el)
        de.ars.cor.el
      ?:  |(?=(%cur-r lex) ?=(%cur-d lex))
        ?:  =(1 h.size.res.cor.el)
          ?:  (lth (sub x.i.ars.cor.el de.ars.cor.el) w.size.res.cor.el)
            de.ars.cor.el
          ?:  ?=(%cur-d lex)
            =/  l=@ud  (lent i.vox.ars.cor.el)
            ?:((lte w.size.res.cor.el l) +((sub l w.size.res.cor.el)) 0)
          +(de.ars.cor.el)
        ?:  (lth (sub y.i.ars.cor.el de.ars.cor.el) h.size.res.cor.el)
          de.ars.cor.el
        +(de.ars.cor.el)
      de.ars.cor.el
    =.  ego  (novo (sido ses(deus (paco k.rex.via el deus.ses))))
    ?:  =(oab de.ars.cor.el)
      :_  ego
      :~  (fio ~)
      ==
    :_  ego
    :~  (fio ~[(viso k.rex.via)])
    ==
  ::
  ++  moto                         :: handle an act event
    ^-  (quip card ^ego)
    ?~  rex.via  [~ ego]
    =/  ses=ara  sto
    =/  el=deus  (exuo k.rex.via deus.ses)
    ?:  &(?=(%select -.ars.cor.el) ?=(%submit pro.ars.cor.el))
      lego
    ?:  ?=(%checkbox -.ars.cor.el)
      opto
    ?~  avis.cor.el  [~ ego]
    :_  ego
    :~  (iuvo %act fons.ses avis.cor.el ~)
    ==
  ::
  ++  lego                         :: handle a form submit
    ^-  (quip card ^ego)
    ?>  ?=(^ rex.via)
    =/  ses=ara  sto
    =/  form=$@(~ [key=rami el=deus])  (nudo %form)
    ?~  form  ~&(>>> %missing-form !!)
    =^  =data  el.form
      =|  dat=data
      |^  ^-  (pair data deus)
      =^  dat  b.gens.el.form  [q p]:(spin b.gens.el.form dat f)
      =^  dat  l.gens.el.form  [q p]:(spin l.gens.el.form dat f)
      =^  dat  n.gens.el.form  [q p]:(spin n.gens.el.form dat f)
      ?:  ?=(%input -.ars.cor.el.form)
        ?~  avis.cor.el.form  ~&(>>> %missing-input-id [dat el.form])
        =/  val=@t  (crip (tufa `lina`(zing vox.ars.cor.el.form)))
        :-  (~(put by dat) avis.cor.el.form val)
        %_  el.form
          vox.ars.cor  ~
          de.ars.cor  0
          i.ars.cor  [0 0]
        ==
      ?:  ?=(%checkbox -.ars.cor.el.form)
        ?~  avis.cor.el.form  ~&(>>> %missing-checkbox-id [dat el.form])
        =/  val=@t  ?:(v.ars.cor.el.form '%.y' '%.n')
        :-  (~(put by dat) avis.cor.el.form val)
        %_  el.form
          v.ars.cor  |
        ==
      [dat el.form]
      ++  f  |=([i=deus a=data] [q p]:^$(el.form i, dat a))
      --
    =.  ego  (novo (sido ses(deus (paco key.form el.form deus.ses))))
    :_  ego
    :~  (fio ~[(viso key.form)])
        (iuvo %form fons.ses avis.cor.el.form data)
    ==
  ::
  ++  opto                         :: handle a checkbox, potentially in a radio group
    ^-  (quip card ^ego)
    ?>  ?=(^ rex.via)
    =/  ses=ara  sto
    =/  rad=$@(~ [key=rami el=deus])  (nudo %radio)
    ?~  rad
      =/  el=deus  (exuo k.rex.via deus.ses)
      ?>  ?=(%checkbox -.ars.cor.el)
      =.  v.ars.cor.el  !v.ars.cor.el
      =.  ego  (novo (sido ses(deus (paco k.rex.via el deus.ses))))
      :_  ego
      :~  (fio ~[(viso k.rex.via)])
      ==
    =/  el=deus  (exuo key.rad deus.ses)
    =.  el
      |-  ^-  deus
      %_  el
        ars.cor
          ?.  ?=(%checkbox -.ars.cor.el)
            ars.cor.el
          %_  ars.cor.el
            v
              ?.  =(key.rad k.rex.via)
                |
              !v.ars.cor.el
          ==
        b.gens  (spun b.gens.el |=([i=deus a=@] [^$(el i, key.rad (snoc key.rad [%b a])) +(a)]))
        l.gens  (spun l.gens.el |=([i=deus a=@] [^$(el i, key.rad (snoc key.rad [%l a])) +(a)]))
        n.gens  (spun n.gens.el |=([i=deus a=@] [^$(el i, key.rad (snoc key.rad [%n a])) +(a)]))
      ==
    =.  ego  (novo (sido ses(deus (paco key.rad el.rad deus.ses))))
    :_  ego
    :~  (fio ~[(viso key.rad)])
    ==
  ::
  ++  nudo                         :: get the nearest element of some kind over the current selection
    |=  typ=@tas
    =|  k=rami
    =|  acc=$@(~ [key=rami el=deus])
    ?~  rex.via    acc
    =/  ses=ara    sto
    =:  k          ?^(k.rex.via ~[i.k.rex.via] k)
        k.rex.via  (voro k.rex.via)
      ==
    |-  ^+  acc
    =?  acc  =(typ -.ars.cor.deus.ses)
      [k deus.ses]
    ?~  k.rex.via
      acc
    %=  $
      k  (snoc k i.k.rex.via)
      k.rex.via  t.k.rex.via
      deus.ses
        %+  snag  ager.i.k.rex.via
        ?-  axis.i.k.rex.via
          %n  n.gens.deus.ses
          %b  b.gens.deus.ses
          %l  l.gens.deus.ses
        ==
    ==
  ::
  --
::
++  alo                            :: check if element b is a child of element a
  |=  [a=rami b=rami]
  ^-  ?
  ?:  =(a b)
    |
  ?~  a
    &
  |-  ^-  ?
  ?:  =(~ b)
    |
  ?:  =(a b)
    &
  $(b (snip b))
::
++  vado                           :: resolve cursor location
  ^-  loci
  =/  =rex
    ?:  open.arx.urbs.ego
      rex.via.arx.urbs.ego
    rex:(snag cura.ego viae.ego)
  ?~  rex  1^1
  =/  [ayr=aer deu=deus]  (creo k.rex)
  =/  [[x1=@ y1=@] [x2=@ y2=@] room=muri]
    (laxo iter.ayr apex.cor.deu res.cor.deu)
  ?:  ?=(%input -.ars.cor.deu)
    =/  [i=loci de=@ w=@ h=@]
      :+  i.ars.cor.deu
        de.ars.cor.deu
      size.res.cor.deu
    ?:  =(1 h)
      [(add x1 (sub x.i de)) y1]
    ?:  (lth x.i w)
      [(add x1 x.i) (add y1 (sub y.i de))]
    =/  ran=@ud  (sub y.i de)
    ?:  (lth +(ran) h)
      [x1 +((add y1 ran))]
    [(add x1 ?:(=(0 w) 0 (dec w))) (add y1 ran)]
  =:  x1  (min (max x1 l.muri.ayr) r.muri.ayr)
      y1  (min (max y1 t.muri.ayr) b.muri.ayr)
    ==
  =/  ox  x1
  |-  ^-  loci
  =/  ux  (~(get by luna.ayr) y1)
  ?~  ux  [x1 y1]
  |-  ^-  loci
  ?~  u.ux
    ?:  (gth y1 y2)
      [1 1]
    [x1 y1]
  ?:  (lth x1 x1.i.u.ux)
    [x1 y1]
  ?:  (gth x2 x2.i.u.ux)
    $(x1 +(x2.i.u.ux), u.ux t.u.ux)
  ^$(x1 ox, y1 +(y1))
::
++  sumo                           :: get the length of the first word in a vox row
  |=  ro=lina
  =|  n=@ud
  |-  ^-  @ud
  ?~  ro  n
  ?:  =(~-. i.ro)  n
  $(n +(n), ro t.ro)
::
++  pono                           :: get the length of a row in vox without the trailing whitespace
  |=  lop=lina
  =.  lop  (flop lop)
  |-  ^-  @ud
  ?~  lop  0
  ?.  =(~-. i.lop)  (lent lop)
  $(lop t.lop)
::
++  noto                           :: parse zona to nota
  |=  z=zona
  ^-  nota
  ?:  ?=(%mod -.z)  [-.z +<.z (noto +>.z)]
  ?:  ?=(%clk -.z)  [-.z +<.z]
  ?:  ?=(%whe -.z)  [-.z +<.z]
  ?:  ?=(%txt -.z)  [-.z ~]
  ?:  ?=(%chr -.z)  z
  ?:  ?=(%aro -.z)  z
  ?:  ?=(%bac -.z)  z
  ?:  ?=(%ret -.z)  z
  ?:  ?=(%esc -.z)  z
  ?:  ?=(%del -.z)  z
  !!
::
++  uro                            :: parse custom hotkey metadata to ales
  |=  h=hotkeys:homunculus
  ^-  ales
  %-  malt
  |-  ^-  (list [nota avis])
  ?~  h  ~
  :_  $(h t.h)
  :_  +.i.h
  ?@  -.i.h               [%chr (taft -.i.h)]
  ?:  ?=(%delete -<.i.h)  [%del ~]
  ?:  ?=(%enter -<.i.h)   [%ret ~]
  ?:  ?=(%back -<.i.h)    [%bac ~]
  ?:  ?=(%tab -<.i.h)     [%mod %ctl [%chr ~-i]]
  ?:  ?=(%arrow -<.i.h)   [%aro ->.i.h]
  !!
::
++  dolo                           :: get default styles for a semantic element
  |=  el=@tas
  ^-  vena
  =/  def=vena
    :*  size=[[%i 0] [%i 0]]
        padd=[[%c 0] [%c 0] [%c 0] [%c 0]]
        marg=[[%c 0] [%c 0] [%c 0] [%c 0]]
        flex=[0 0]
        flow=[%col %clip]
        look=[~ ~ ~]
    ==
  ?+  el  def
      %text
    %_  def
      size  [[%c 0] [%c 0]]
      flow  [%row %clip]
    ==
      %layer
    %_  def
      size  [[%p 100] [%p 100]]
    ==
      %border-l
    %_  def
      size  [[%c 1] [%p 100]]
    ==
      %border-r
    %_  def
      size  [[%c 1] [%p 100]]
    ==
      %border-t
    %_  def
      size  [[%p 100] [%c 1]]
      flow  [%row %clip]
    ==
      %border-b
    %_  def
      size  [[%p 100] [%c 1]]
      flow  [%row %clip]
    ==
      %line-h
    %_  def
      size  [[%p 100] [%c 1]]
      flow  [%row %clip]
    ==
      %line-v
    %_  def
      size  [[%c 1] [%p 100]]
    ==
      %input
    %_  def
      size  [[%c 10] [%c 1]]
      flow  [%row %clip]
      look  [~ [~ %w] [~ %k]]
    ==
      %checkbox
    %_  def
      size  [[%c 2] [%c 1]]
      look  [~ [~ %w] [~ %k]]
    ==
      %row
    %_  def
      flow  [%row %clip]
    ==
  ==
::
++  suo                            :: process a sail element's name and attribute list for geno
  |=  [n=mane a=mart]
  =|  [=avis =acia marv=mart]
  =/  [=vena =ars]
      ?+  n             [(dolo %$) [%$ ~]]
        %$              [(dolo %text) [%text ~]]
        %row            [(dolo %row) [%$ ~]]
        %pattern        [(dolo %$) [%pattern ~]]
        %layer          [(dolo %layer) [%layer ~]]
        %select         [(dolo %$) [%select %~]]
        %border-l       [(dolo %border-l) [%border %l %~]]
        %border-r       [(dolo %border-r) [%border %r %~]]
        %border-t       [(dolo %border-t) [%border %t %~]]
        %border-b       [(dolo %border-b) [%border %b %~]]
        %line-h         [(dolo %line-h) [%line %h %light]]
        %line-v         [(dolo %line-v) [%line %v %light]]
        %scroll         [(dolo %scroll) [%scroll *equi *iter *sola]]
        %form           [(dolo %form) [%form ~]]
        %input          [(dolo %input) [%input 0 [0 0] ~]]
        %checkbox       [(dolo %checkbox) [%checkbox | ~ ~]]
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
      %bg
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
      %fg
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
      %b-bg
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
      %b-fg
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
      %select-bg
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
      %select-fg
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
      %trigger
    ?.  ?=(%scroll -.ars)  $(a t.a)
    =/  v=as  (pars v.i.a)
    $(equi.ars [~^v ~^v], a t.a)
      %href
    $(avis (calo v.i.a), a t.a)
  ==
::
++  peto                           :: check whether an element is a navigation point
  |=  typ=@tas
  ^-  ?
  ?|  ?=(%select typ)
      ?=(%scroll typ)
      ?=(%input typ)
      ?=(%checkbox typ)
  ==
::
++  adeo                           :: check whether to apply select styles to a branch
  |=  [typ=@tas aci=acia]
  ^-  ?
  ?|  ?=(%select typ)
      &(?=(%input typ) ?=([~ ~ ~] aci))
  ==
::
++  apo                            :: make axis for a key
  |=  typ=@tas
  ^-  axis
  ?+  typ      %n
    %layer     %l
    %border    %b
    %border-l  %b
    %border-r  %b
    %border-t  %b
    %border-b  %b
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
++  calo                           :: parse an identifier path from a tape 
  |=  tap=tape
  ^-  avis
  ?~  tap  ~
  =?  tap  !=('/' i.tap)  ['/' tap]
  =/  rus  (rust tap stap)
  ?~  rus  ~
  u.rus
::
++  obeo                           :: get border sizes from a list of border elements
  |=  bor=marl                     :: defaults to 1 if an element is found and no valid size is specified
  =+  [i=0 bl=0 br=0 bt=0 bb=0]
  |-  ^-  muri
  ?~  bor  [bl br bt bb]
  ?+  n.g.i.bor  $(bor t.bor)
      %border-l
    ?~  a.g.i.bor
      $(bor t.bor, bl ?:(=(0 bl) 1 bl))
    ?:  =(%w n.i.a.g.i.bor)
      =/  n=(unit @ud)  (slaw %ud (crip v.i.a.g.i.bor))             :: CONSOLIDATE
      %=  $
        bor  t.bor
        bl   ?~(n ?:(=(0 bl) 1 bl) ?:((gth u.n bl) u.n bl))
      ==
    $(a.g.i.bor t.a.g.i.bor)
      %border-r
    ?~  a.g.i.bor
      $(bor t.bor, br ?:(=(0 br) 1 br))
    ?:  =(%w n.i.a.g.i.bor)
      =/  n=(unit @ud)  (slaw %ud (crip v.i.a.g.i.bor))
      %=  $
        bor  t.bor
        br   ?~(n ?:(=(0 br) 1 br) ?:((gth u.n br) u.n br))
      ==
    $(a.g.i.bor t.a.g.i.bor)
      %border-t
    ?~  a.g.i.bor
      $(bor t.bor, bt ?:(=(0 bt) 1 bt))
    ?:  =(%h n.i.a.g.i.bor)
      =/  n=(unit @ud)  (slaw %ud (crip v.i.a.g.i.bor))
      %=  $
        bor  t.bor
        bt   ?~(n ?:(=(0 bt) 1 bt) ?:((gth u.n bt) u.n bt))
      ==
    $(a.g.i.bor t.a.g.i.bor)
      %border-b
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
++  texo                           :: resolve an element's style
  |=  [typ=@tas sel=? gray=? fil=fila aci=acia]
  ^-  fila
  =.  fil
    =/  txt=?  |(?=(%text typ) ?=(%input typ) ?=(%pattern typ))
    ?.  sel
      fil(d ?.(txt ~ d.fil))
    :+  ?.  txt  ~
        ?~(d.aci d.fil u.d.aci)
      ?~(b.aci b.fil u.b.aci)
    ?~(f.aci f.fil u.f.aci)
  ?.  gray  fil
  %_  fil
    b  (cubo b.fil)
    f  (cubo f.fil)
  ==
::
++  cubo                           :: turn a color gray
  |=  tin=tint
  ^-  [r=@uxD g=@uxD b=@uxD]
  ?@  tin
    ?-  tin
      %r  [0x55 0x55 0x55]
      %g  [0x45 0x45 0x45]
      %b  [0x36 0x36 0x36]
      %c  [0x87 0x87 0x87]
      %m  [0x9b 0x9b 0x9b]
      %y  [0xb8 0xb8 0xb8]
      %w  [0xc8 0xc8 0xc8]
      %k  [0x0 0x0 0x0]
      %~  [0x0 0x0 0x0]
    ==
  =/  gex=@
    =-  (min - 200)
    ;:  add
      (div (mul 299 ^-(@ r.tin)) 1.000)
      (div (mul 587 ^-(@ g.tin)) 1.000)
      (div (mul 114 ^-(@ b.tin)) 1.000)
    ==
  [gex gex gex]
::
++  figo                           :: resolve an input's text view
  |=  [=res =ars]
  ^-  vox
  ?>  ?=(%input -.ars)
  ?.  =(1 h.size.res)
    (slag de.ars vox.ars)
  ?~  vox.ars  ~
  %_  vox.ars
    i  (slag de.ars i.vox.ars)
  ==
::
++  duro                           :: resolve the characters in a checkbox
  |=  =cor
  ^-  vox
  ?>  ?=(%checkbox -.ars.cor)
  ?.  v.ars.cor
    ?~  f.ars.cor  ~
    f.ars.cor
  ?^  t.ars.cor
    t.ars.cor
  =/  v=vox  [[~-~2588. ~] ~]
  (fuco w.size.res.cor h.size.res.cor v)
::
++  orno                           :: resolve a line as vox
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
  ^-  [^apex modi muri]
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
++  veho                           :: transition a scroll element's scroll state 
  |=  [[old-cor=cor new-top=@] old=dei new=dei]
  ^-  iter
  ?.  ?=(%scroll -.ars.old-cor)  *iter
  =/  old-top=@  ;:(add y.apex.old-cor t.bord.res.old-cor t.padd.res.old-cor)
  =/  old-bot=@
    =/  b=@  +((add b.bord.res.old-cor b.padd.res.old-cor))
    =.  y.apex.old-cor  (add y.apex.old-cor h.size.res.old-cor)
    ?:  (gte b y.apex.old-cor)  0
    (sub y.apex.old-cor b)
  =/  old-el
    |-  ^-  $@(~ deus)
    ?~  old  ~
    =/  el-t=@
      ?:  (gte y.iter.ars.old-cor y.apex.cor.i.old)  0
      (sub y.apex.cor.i.old y.iter.ars.old-cor)
    =/  el-b=@
      =.  y.apex.cor.i.old
        %+  add  y.apex.cor.i.old
        ?.(=(0 h.size.res.cor.i.old) (dec h.size.res.cor.i.old) 0)
      ?:  (gte y.iter.ars.old-cor y.apex.cor.i.old)  0
      (sub y.apex.cor.i.old y.iter.ars.old-cor)
    ?:  (gth el-t old-bot)  ~
    ?:  &((gte el-b old-top) ?=(^ avis.cor.i.old))
      i.old
    $(old t.old)
  ?~  old-el  iter.ars.old-cor
  =/  new-el
    |-  ^-  $@(~ deus)
    ?~  new  ~
    ?:  =(avis.cor.old-el avis.cor.i.new)
      i.new
    $(new t.new)
  ?~  new-el  iter.ars.old-cor
  =/  old-range=@  (sub y.apex.cor.old-el old-top)
  =/  new-range=@  (sub y.apex.cor.new-el new-top)
  ?:  =(old-range new-range)
    iter.ars.old-cor
  %_    iter.ars.old-cor
      y
    ?:  (gth old-range new-range)
      =/  dif  (sub old-range new-range)
      ?:((lth dif y.iter.ars.old-cor) (sub y.iter.ars.old-cor dif) 0)
    =/  dif  (sub new-range old-range)
    (add y.iter.ars.old-cor dif)
  ==
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
    =/  [=ab =ora]
      ?+  -.ars.cor  !!
        %border  [?:(|(?=(%t ad.ars.cor) ?=(%b ad.ars.cor)) %h %v) ora.ars.cor]
        %line    [ab.ars.cor ora.ars.cor]
      ==
    =/  [x1=@ud x2=@ud]
      :-  x.apex.cor
      ?-(ab %h (add x.apex.cor (dec w.size.res.cor)), %v x.apex.cor)
    =/  [y1=@ud y2=@ud]
      :-  y.apex.cor
      ?-(ab %v (add y.apex.cor (dec h.size.res.cor)), %h y.apex.cor)
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
    ?-  ab
        %h
      ?.  ?=(%v -.i)  ~
      ?:  |((lth x.i x1) (gth x.i x2))  ~
      ?:  |(&(?=(%border p.i) =(y1 y2.i)) &(?=(%line p.i) =(y1 +(y2.i))))
        :^  ~  ab  (sub x.i x1)
        ?:  =(x1 x.i)  [ora %~ ora ora.i %~]
        ?:  =(x2 x.i)  [ora ora %~ ora.i %~]
        [ora ora ora ora.i %~]
      ?:  |(&(?=(%border p.i) =(y2 y1.i)) &(?=(%line p.i) !=(0 y1.i) =(y2 (dec y1.i))))
        :^  ~  ab  (sub x.i x1)
        ?:  =(x1 x.i)  [ora %~ ora %~ ora.i]
        ?:  =(x2 x.i)  [ora ora %~ %~ ora.i]
        [ora ora ora %~ ora.i]
      ~
        %v
      ?.  ?=(%h -.i)  ~
      ?:  |((lth y.i y1) (gth y.i y2))  ~
      ?:  |(&(?=(%border p.i) =(x1 x2.i)) &(?=(%line p.i) =(x1 +(x2.i))))
        :^  ~  ab  (sub y.i y1)
        ?:  =(y1 y.i)  [ora ora.i %~ %~ ora]
        ?:  =(y2 y.i)  [ora ora.i %~ ora %~]
        [ora ora.i %~ ora ora]
      ?:  |(&(?=(%border p.i) =(x2 x1.i)) &(?=(%line p.i) !=(0 x1.i) =(x2 (dec x1.i))))
        :^  ~  ab  (sub y.i y1)
        ?:  =(y1 y.i)  [ora %~ ora.i %~ ora]
        ?:  =(y2 y.i)  [ora %~ ora.i ora %~]
        [ora %~ ora.i ora ora]
      ~
    ==
  =<  q
  %^  spin  c
    ?+  -.ars.cor  ~
      %border  (orno size.res.cor [ad ora]:ars.cor)
      %line    (orno size.res.cor [ab ora]:ars.cor)
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
++  vivo                           :: collect line intersection groups and ids from a session branch
  |=  [key=rami deu=deus]
  =/  acc=[=aves =ossa]  [~ [~^~ ~^~]]
  |-  ^-  [aves ossa]
  =:  aves.acc
        ?~  avis.cor.deu  aves.acc
        (~(put by aves.acc) avis.cor.deu key)
      ossa.acc
        ?:  ?|  ?=(%layer -.ars.cor.deu)
                ?=(%scroll -.ars.cor.deu)
                ?=([* ~] key)
            ==
          (~(put by ossa.acc) key ~)
        ?.  ?|  ?=(%border -.ars.cor.deu)
                ?=(%line -.ars.cor.deu)
            ==
          ossa.acc
        =/  ki  (feto key ossa.acc)
        =/  oz  (~(get by ossa.acc) ki)
        ?~  oz  ossa.acc
        %+  %~  put  by  ossa.acc  ki
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
          ?:  ?=(%h ab.ars.cor.deu)
            [%h %line x ?.(=(0 w) (add x (dec w)) x) y o]
          [%v %line x y ?.(=(0 h) (add y (dec h)) y) o]
          ::
        ==
    ==
  ?.  ?|  .?(b.gens.deu)
          .?(l.gens.deu)
          .?(n.gens.deu)
      ==
    acc
  =<  +>.q
  %^  spin
      ^-  dei
      %-  zing
      :~  b.gens.deu
          l.gens.deu
          n.gens.deu
      ==
    [*axis *ager acc]
  |=  [d=deus a=[n=axis i=ager c=_acc]]
  ^+  +<
  =/  x  (apo -.ars.cor.d)
  =?  i.a  !=(n.a x)  0
  :-  d
  %_  a
    n  x
    i  +(i.a)
    c
      %=  ^$
        deu  d
        key  (snoc key [x i.a])
        acc  c.a
      ==
  ==
::
++  geno                           :: turn sail into element state
  |=  $:  rel=$@(~ [=rami =cor])
          old=$@(~ ara)
          vel=vela
      ==
  ^-  deus
  =/  k=rami                   ?^(rel (flop rami.rel) ~)
  =/  m=marl                   ~[vel]
  =/  px=as                    [%c ?^(rel w.size.res.cor.rel x.arca.urbs.ego)]
  =/  py=as                    [%c ?^(rel h.size.res.cor.rel y.arca.urbs.ego)]
  =/  pl=fila                  ?^(rel look.res.cor.rel [~ ~ %w])
  =/  pa=acia                  ?^(rel sele.res.cor.rel [~ ~ ~])
  =/  pow=fuga                 ?^(rel flow.res.cor.rel [%row %clip])
  =/  prx=@ud                  ?^(rel w.size.res.cor.rel x.arca.urbs.ego)
  =/  pry=@ud                  ?^(rel h.size.res.cor.rel y.arca.urbs.ego)
  =/  pape=apex                ?^(rel apex.cor.rel *apex)
  =/  vape=apex                pape
  =/  vir=[n=@ud o=@ud i=@ud]  [0 0 0]
  =<  ?>
      ?=  ^  -
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
  =?  ars  |(?=(%pattern -.ars) ?=(%checkbox -.ars))
    ?.  &(?=(^ c.i.m) ?=(^ a.g.i.c.i.m))  ars
    ?+  -.ars  ars
        %pattern
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
        %checkbox
      =/  on=vox  (oro ~ ~ (tuba v.i.a.g.i.c.i.m))
      =/  off=vox
        ?.  &(?=(^ t.c.i.m) ?=(^ a.g.i.t.c.i.m))  ~
        (oro ~ ~ (tuba v.i.a.g.i.t.c.i.m))
      ars(t on, f off)
    ==
  =/  [bor=marl lay=marl nor=marl]
    ?:  ?|  ?=(%text -.ars)
            ?=(%pattern -.ars)
            ?=(%input -.ars)
            ?=(%checkbox -.ars)
        ==
      [~ ~ ~]
    =|  [bor=marl lay=marl nor=marl]
    |-  ^-  [marl marl marl]
    ?~  c.i.m  [bor (flop lay) (flop nor)]
    ?+  n.g.i.c.i.m  $(nor [i.c.i.m nor], c.i.m t.c.i.m)
      %border-l      $(bor [i.c.i.m bor], c.i.m t.c.i.m)
      %border-r      $(bor [i.c.i.m bor], c.i.m t.c.i.m)
      %border-t      $(bor [i.c.i.m bor], c.i.m t.c.i.m)
      %border-b      $(bor [i.c.i.m bor], c.i.m t.c.i.m)
      %layer         $(lay [i.c.i.m lay], c.i.m t.c.i.m)
    ==
  =?  bor  &(?=(^ marv) !?=(%input -.ars))
    %+  weld  bor
    ^-  marl
    :~  [[%border-l marv] ~]  [[%border-r marv] ~]
        [[%border-t marv] ~]  [[%border-b marv] ~]
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
      k     [[%l 0] k]
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
      k     [[%n 0] k]
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
      k     [[%n (lent ndei)] k]
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
          aci
      ==
    =/  len=@ud
      (roll ^-(vox vox.ars) |=([i=^lina a=@ud] (max a (pono i))))
    =/  lim=(unit @ud)
      ?:(?=(%i p.px) ~ [~ (sub prx ?:(?=(%row d.pow) n.vir o.vir))])
    :*  [?~(lim len (min len u.lim)) (lent vox.ars)]
        [0 0 0 0]
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
      k     [[%b 0] k]
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
  =?  ars
      ?|  ?=(%input -.ars)
          ?=(%scroll -.ars)
          ?=(%checkbox -.ars)
      ==
    =.  k  (flop k)
    =/  old-key=rami
      ?~  old   k
      ?~  avis  k
      =/  ok    (~(get by aves.old) avis)
      ?~  ok    k
      u.ok
    =/  old-el=(unit deus)
      ?~  old  ~
      (mole |.((exuo old-key deus.old)))
    ?+  -.ars  ars
        %input
      ?.  &(?=(^ old-el) ?=(%input -.ars.cor.u.old-el))
        ?~  lina  ars
        %_  ars
          vox  (oro [~ w.size.ares] [~ h.size.ares] lina)
        ==
      ?:  =(size.res.cor.u.old-el size.ares)
        ars.cor.u.old-el
      %_  ars
        de   0
        i    [0 0]
        vox
          %^    oro
              [~ w.size.ares]
            [~ h.size.ares]
          `^lina`(zing vox.ars.cor.u.old-el)
      ==
        %checkbox
      ?.  &(?=(^ old-el) ?=(%checkbox -.ars.cor.u.old-el))
        ars
      ars.cor.u.old-el
        %scroll
      =/  ola=sola
        ?~  csiz  [0 0]
        :-  ?:((gth arx w.csiz) 0 (sub w.csiz arx))
        ?:((gth ary h.csiz) 0 (sub h.csiz ary))
      %_  ars
        sola  ola
        iter
          ?~  old-el  *iter
          %^    veho
              [cor.u.old-el ;:(add y.aape t.bord.ares t.padd.ares)]
            (weld n.gens.u.old-el l.gens.u.old-el)
          (weld ndei ldei)
      ==
    ==
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
  :-  `deus`[[aape avis ares ars] [bdei ldei ndei]]
  $(k ?^(k k(ager.i +(ager.i.k)) ~), m t.m)
::
++  creo                           :: instantiate a branch by key, along with rendering context
  |=  key=rami
  ^-  [aer deus]
  =/  =via  (snag cura.ego viae.ego)
  =/  deu=deus
    %_  deus.area.via
      l.gens
        ?.  ?&  ?=(^ arae.via.arx.urbs.ego)
                open.arx.urbs.ego
            ==
          ~
        ~[deus.i.arae.via.arx.urbs.ego]
      n.gens
        (turn arae.via |=(=ara deus.ara))
    ==
  =|  ayr=aer
  =:  muri.ayr  [1 x.arca.urbs.ego 1 y.arca.urbs.ego]
      rex.ayr
        ?.  open.arx.urbs.ego
          rex.via
        rex.via.arx.urbs.ego
      ossa.ayr
        =/  osa=ossa
          (roll arae.via |=([i=ara o=ossa] (~(uni by o) ossa.i)))
        ?.  ?&  ?=(^ arae.via.arx.urbs.ego)
                open.arx.urbs.ego
            ==
          osa
        (~(uni by osa) ossa.i.arae.via.arx.urbs.ego)
    ==
  =|  ki=rami
  ?~  key  [ayr deu]
  |-  ^-  [aer deus]
  =<  ?~  t.key
        [ayr deu]
      $(key t.key, ki [i.key ki])
  =/  [[x1=@ y1=@] [x2=@ y2=@] room=muri]
    (laxo iter.ayr apex.cor.deu res.cor.deu)
  =:  iter.ayr
        ?.  ?=(%scroll -.ars.cor.deu)  iter.ayr
        :-  (add x.iter.ayr x.iter.ars.cor.deu)
        (add y.iter.ayr y.iter.ars.cor.deu)
      muri.ayr
        ?:  ?=(%b axis.i.key)
          muri.ayr
        :^    (max l.room l.muri.ayr)
            (min r.room r.muri.ayr)
          (max t.room t.muri.ayr)
        (min b.room b.muri.ayr)
      nav.ayr
        ?.  (peto -.ars.cor.deu)  nav.ayr
        :-  (adeo -.ars.cor.deu sele.res.cor.deu)
        [-.ars.cor.deu (flop ki) avis.cor.deu x1 x2 y1 y2]
    ==
  %_  .
    deu
      %+  snag  ager.i.key
      ?-  axis.i.key
        %n  n.gens.deu
        %b  b.gens.deu
        %l  l.gens.deu
      ==
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
      ?:  ?|  =(0 w.size.res.cor.i.els)
              =(0 h.size.res.cor.i.els)
          ==
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
          ?:  &((gte x1.i x1.i.u.l) (lte x2.i x2.i.u.l))
            u.l
          ?:  (lth x2.i x1.i.u.l)
            [i u.l]
          ?:  (gth x1.i x2.i.u.l)
            [i.u.l $(u.l t.u.l)]
          ?:  (lth x1.i x1.i.u.l)
            u.l(x1.i x1.i)
          u.l(x2.i x2.i)
      ==
  ==
::
++  viso                           :: render a branch by key
  |=  key=rami
  ^-  [=apex =sol]
  =/  [ayr=aer deu=deus]  (creo key)
  ?:  ?|  =(0 w.size.res.cor.deu)
          =(0 h.size.res.cor.deu)
      ==
    [1^1 ~]
  =/  a-y1=@ud
    %+  max  t.muri.ayr
    ?:  (gte y.iter.ayr y.apex.cor.deu)  1
    (sub y.apex.cor.deu y.iter.ayr)
  =/  a-y2=@ud
    =/  y2  (add y.apex.cor.deu (dec h.size.res.cor.deu))
    =?  y2  !=(0 y.iter.ayr)
      ?.  (lte y.iter.ayr y2)  0
      (sub y2 y.iter.ayr)
    (min y2 b.muri.ayr)
  ?:  ?|  (gth a-y1 b.muri.ayr)
          (lth a-y2 t.muri.ayr)
      ==
    [1^1 ~]
  =/  acc=sol
    %+  reap
      +((sub a-y2 a-y1))
    *(list lux)
  =?  acc  .?(luna.ayr)
    %+  spun  acc
    |=  [i=(list lux) n=@ud]
    ^-  [(list lux) @ud]
    =/  y  (add a-y1 n)
    =/  l  (~(get by luna.ayr) y)
    ?~  l
      [i +(n)]
    [u.l +(n)]
  :-  [x.apex.cor.deu a-y1]
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
  =:  x1  (max x1 l.muri.ayr)
      x2  (min x2 r.muri.ayr)
      y1  (max y1 t.muri.ayr)
      y2  (min y2 b.muri.ayr)
    ==
  =.  nav.ayr
    ?.  (peto -.ars.cor.deu)  nav.ayr
    :-  (adeo -.ars.cor.deu sele.res.cor.deu)
    [-.ars.cor.deu key avis.cor.deu x1 x2 y1 y2]
  =.  acc
    =/  itr=iter
      ?.  ?=(%scroll -.ars.cor.deu)  iter.ayr
      :-  (add x.iter.ayr x.iter.ars.cor.deu)
      (add y.iter.ayr y.iter.ars.cor.deu)
    =/  mur=muri
      :^    (max l.room l.muri.ayr)
          (min r.room r.muri.ayr)
        (max t.room t.muri.ayr)
      (min b.room b.muri.ayr)
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
          iter.ayr  ?.(?=(%border -.ars.cor.d) itr iter.ayr)
          muri.ayr  ?.(?=(%border -.ars.cor.d) mur muri.ayr)
        ==
    ==
  ?:  ?|  ?=(%layer -.ars.cor.deu)
          &(?=(%border -.ars.cor.deu) ?=(%~ ora.ars.cor.deu))
      ==
    acc
  =/  a-i1=@ud   =+(y=(max y1 t.muri.ayr) ?:((lte a-y1 y) (sub y a-y1) 0))
  =/  a-i2=@ud   =+(y=(min y2 b.muri.ayr) ?:((lte a-y1 y) (sub y a-y1) 0))
  =/  gray=?     &(open.arx.urbs.ego !?=([[%l @] *] key))
  =/  look=fila
    %:  texo
      -.ars.cor.deu
      &(sty.nav.ayr ?=(^ rex.nav.ayr) ?=(^ rex.ayr) =(k.rex.ayr k.rex.nav.ayr))
      gray
      look.res.cor.deu
      sele.res.cor.deu
    ==
  =;  rend=sol
    %+  weld  (scag a-i1 acc)
    %+  weld  rend
    (slag +(a-i2) acc)
  =<  p
  %^  spin  `sol`(swag [a-i1 +((sub a-i2 a-i1))] acc)
    =;  v=vox
      ?:  (gte t.room t.muri.ayr)  v
      =/  n=@
        %+  sub  (add t.muri.ayr y.iter.ayr)
        ;:  add
          y.apex.cor.deu
          t.bord.res.cor.deu
          t.padd.res.cor.deu
        ==
      (oust [0 n] v)
    ?+  -.ars.cor.deu  ~
      %text      vox.ars.cor.deu
      %pattern   vox.ars.cor.deu
      %input     (figo res.cor.deu ars.cor.deu)
      %checkbox  (duro cor.deu)
      %border    (coeo cor.deu key ossa.ayr)
      %line      (coeo cor.deu key ossa.ayr)
    ==
  |=  [l=(list lux) xov=vox]
  ^+  +<
  :_  ?^(xov t.xov ~)
  |-  ^-  (list lux)
  =/  tok=lux
    :*  x1
        x2
        look
        ?.(gray rex.nav.ayr ~)
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
++  duco                           :: produce interactivity state from a render schematic
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
  =;  [p=wall q=[@ fil=fila]]
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
    [x.apex f.acc]
  |=  [=lux [oldx=@ fil=fila]]
  ^-  [tape [@ fila]]
  ?~  p.lux
    [~ oldx fil]
  :_  [+(x2.lux) fil.p.lux]
  =/  od=?  .?(d.fil)
  =/  nd=?  .?(d.fil.p.lux)
  =/  nb=(unit tint)  ?.(=(b.fil b.fil.p.lux) [~ b.fil.p.lux] ~)
  =/  nf=(unit tint)  ?.(=(f.fil f.fil.p.lux) [~ f.fil.p.lux] ~)
  |-  ^-  tape
  ?.  =(oldx x1.lux)
    :-  '\\x1b['
    :+  (scot %ud y.acc)   ';'
    :+  (scot %ud x1.lux)  'H'
    $(oldx x1.lux)
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
