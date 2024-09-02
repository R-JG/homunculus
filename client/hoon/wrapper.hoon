|%
+$  goof    [mote=term =tang]
+$  wire    path
+$  ovum    [=wire =input]
+$  crud    [=goof =input]
+$  input   [eny=@ our=@ux now=@da cause=*]
::
++  keep
  |*  inner=mold
  =>
  |%
  +$  inner-state  inner
  +$  outer-state  [desk-hash=(unit @uvI) homunculus=ego internal=inner]
  +$  outer-fort
    $_  ^|
    |_  outer-state
    ++  load
      |~  arg=*
      [*(list *) **]
    ++  peek
      |~  arg=path
      *(unit (unit *))
    ++  poke
      |*  [num=@ ovum=*]
      [*(list *) *outer-state]
    ++  wish
      |~  txt=@
      **
    --
  ::
  +$  fort
    $_  ^|
    |_  state=inner-state
    ++  load
      |~  arg=*
      [*(list *) **]
    ++  peek
      |~  arg=path
      *(unit (unit *))
    ++  poke
      |~  arg=input
      [*(list *) *inner-state]
    --
  --
  ::
  |=  inner=fort
  |=  hash=@uvI
  =<  .(desk-hash.outer `hash)
  |_  outer=outer-state
  ++  load
    |=  arg=*
    ^-  [(list *) *]
    (load:inner arg)
  ::
  ++  peek
    |=  arg=path
    ^-  (unit (unit *))
    (peek:inner arg)
  ::
  ++  wish
    |=  txt=@t
    ^-  *
    (slap !>(~) (ream txt))
  ::
  ++  poke
    |=  [num=@ ovum=*]
    ^-  [(list *) _..poke]
    ?+   ovum  ~&("invalid arg: {<ovum>}" ~^..poke)
        [[%$ %arvo ~] *]
      =/  g  ((soft crud) ovum)
      ?~  g  ~&(%invalid-goof ~^..poke)
      =-  [~ ..poke]
      %+  turn  tang.goof.u.g
      ~>  %slog.[3 leaf+"crud"]
      |=(=tank ~>(%slog.[3 tank] 0))
    ::
        [[%poke ~] *]
      =/  ovum  ((soft ^ovum) ovum)
      ?~  ovum  ~&("invalid arg: {<ovum>}" ~^..poke)
      =/  o  ((soft input) input.u.ovum)
      ?~  o
        ~&  "could not mold poke type: {<ovum>}"
        =+  (road |.(;;(^^ovum ovum)))
        ~^..poke
      ::
      :: if cause.u.o is tagged with %homunculus, handle terminal input.
      :: if homunculus determines that there is an event poke for the nested core, send it (do the same as below)
      ?:  ?=([%homunculus *] cause.u.o)
        =/  zon=(unit zona)
          ?:  ?=([%input @] +.cause.u.o)     (ineo +>.cause.u.o)
          ?:  ?=([%resize @ @] +.cause.u.o)  [~ [%rez +>.cause.u.o]]
          ~
        ~&  zon
        [~ ..poke]
      ::
      =^  effects  internal.outer
        (poke:inner input.u.ovum)
      =.  state.inner  internal.outer
      :: 
      :: loop through effects until @ checking whether the head is a cell and the head of that is %homunculus
      :: this is a homunculus update card; handle it and proceed on to the next.
      ::
      [effects ..poke(internal.outer internal.outer)]
    ==
  --
::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::
+$  esse  (map rami ens)
+$  ens   [=res =lar =modi =iter =avis =acia =visa =ars]
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
+$  avis  (unit @t)
+$  ad    ?(%l %r %t %b)
+$  via   ?(%h %v)
+$  ora   ?(%light %heavy %double %arc %blank %~)
+$  muri  [l=@ud r=@ud t=@ud b=@ud]
+$  sola  [x=@ud y=@ud]
+$  ars
  $%  [%text =vox]  [%pattern =vox]
      [%layer ~]  [%scroll =iter =muri =sola]
      [%border =ad =ora]  [%line =via =ora]
      [%select pro=?(%submit %~)]
      [%input ab=@ud i=loci =vox]  [%checkbox v=bean]
      [%radio ~]  [%form ~]  [%$ ~]
  ==
+$  visa  (map loci nodi)
+$  nodi  (pair fila @c)
+$  lina  (list @c)
+$  vox   (list lina)
+$  fila  [d=(set deco) b=tint f=tint]
+$  acia  [d=(unit (set deco)) b=(unit tint) f=(unit tint)]
+$  lux   blit
+$  zona
  $~  [%txt ~]
  $%  [%clk p=?(%d %u) x=@ud y=@ud]
      [%whe p=?(%d %u) x=@ud y=@ud]
      [%mod mod=?(%ctl %alt %shf) key=$~([%txt ~] zona)]
      [%aro p=?(%d %l %r %u)]
      [%txt p=lina]
      [%chr p=@c]
      [%bac ~]  [%del ~]  [%ret ~]  [%esc ~]
      [%rez p=@ud q=@ud]
  ==
+$  nota
  $~  [%txt ~]
  $%  [%clk ?(%d %u)]  [%whe ?(%d %u)]
      [%mod ?(%ctl %alt %shf) $~([%txt ~] nota)]
      [%aro ?(%d %l %r %u)]
      [%txt ~]  [%chr @c]
      [%bac ~]  [%del ~]  [%ret ~]  [%esc ~]
  ==
+$  lex
  $?  %nav-l  %nav-r  %nav-u  %nav-d
      %cur-l  %cur-r  %cur-u  %cur-d
      %scr-l  %scr-r  %scr-u  %scr-d
      %inp  %del  %tog  %act  %clk  %def
  ==
+$  omen  (map nota lex)
+$  ales  (map nota @t)
+$  aves  (map @t rami)
+$  mus   (map loci rami)
+$  equi  (set rami)
+$  dux   [k=rami =avis muri]
+$  rex   $@(~ dux)
+$  ordo  (list dux)
+$  gens  (map rami (list rami))
+$  os
  $%  [%h p=?(%border %line) x1=@ud x2=@ud y=@ud =ora =visa]
      [%v p=?(%border %line) x=@ud y1=@ud y2=@ud =ora =visa]
  ==
+$  ossa  (set os)
+$  crux  [c=(pair via ora) l=ora r=ora t=ora b=ora]
+$  viae  (map loci (pair fila crux))
+$  aqua  (list [i=@ud size=@ud marg=@ud])
+$  opus  [=esse =visa]
+$  cura  [=visa =omen =aves =gens =ordo =rex =equi =mus]
+$  as    $%((pair %c @ud) (pair %p @ud) (pair %i @ud))
+$  data  [p=@t q=(map @t @t)]
+$  vena                             :: THIS NAME HAS BEEN CHANGED FROM OVUM
  $:  size=[w=as h=as]
      padd=[l=as r=as t=as b=as]
      marg=[l=as r=as t=as b=as]
      flex=[x=@ud y=@ud]
      flow=fuga
      look=acia
  ==
:: +$  vita
::   $%  [%session p=session:homunculus]
::       [%change p=fons]
::       [%close p=fons]
::       [%move p=fons q=(pair ?(%full %char) ?(%l %r %u %d))]
::   ==
:: +$  cor   @
:: +$  fons  (pair @p @tas)
:: +$  acro
::   $%  [%dill p=path]
::       [%http ~]
::   ==
:: +$  aula  $~(~[~] (list (set fons)))
+$  vela  manx
+$  ara
  $:  =vela  =muri
      =esse  =ales
      =visa  =omen
      =aves  =gens
      =ordo  =rex
      =equi  =mus
  ==
:: +$  arae  (map fons ara)
+$  luna  (pair bean ara)
+$  urbs  $~([50 25] [x=@ud y=@ud])
:: +$  ego
::   $:  =cor  =fons  =acro
::       =urbs  =luna  =aula
::       =arae
::   ==
+$  ego
  $:  =urbs  =luna  =ara
  ==
::  ::  ::  ::  ::  ::  ::
+$  event
  $%  [%select =id]
      [%act =id]
      [%form =id data=(map id @t)]
      [%hotkey =id]
  ==
+$  id  @t   
::  ::  COPIED  ::  ::
+$  deco  ?(~ %bl %br %un)
+$  tint
  $@  ?(%r %g %b %c %m %y %k %w %~)
  [r=@uxD g=@uxD b=@uxD]
+$  stub  (list (pair stye (list @c)))
+$  stye  (pair (set deco) (pair tint tint))
+$  blit
  $%  [%bel ~]
      [%clr ~]
      [%hop p=$@(@ud [x=@ud y=@ud])]
      [%klr p=stub]
      [%mor p=(list blit)]
      [%nel ~]
      [%put p=(list @c)]
      [%sag p=path q=*]
      [%sav p=path q=@]
      [%url p=@t]
      [%wyp ~]
  ==
+$  json
  $@  ~
  $%  [%a p=(list json)]
      [%b p=?]
      [%o p=(map @t json)]
      [%n p=@ta]
      [%s p=@t]
  ==
::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::
++  ineo
  |=  cod=cord
  ^-  (unit zona)
  =/  inp=tape  (trip cod)
  ?:  =("\\" inp)  ~
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


::
--
