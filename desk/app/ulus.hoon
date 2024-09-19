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
      flex=modi                                                        :: child element positioning: justify
      flow=fuga                                                        :: child element positioning: sequence
      look=fila                                                        :: style
      sele=acia                                                        :: select style
  ==                                                                   ::
+$  ars                                                                :: special element types
  $%  [%text =vox]
      [%pattern =vox]
      [%layer ~]
      [%scroll =iter =muri =sola]
      [%border =ad =ora]
      [%line =via =ora]
      [%select pro=?(%submit %~)]
      [%input ab=@ud i=loci =vox]
      [%checkbox v=bean]
      [%radio ~]
      [%form ~]
      [%$ ~]
  ==
+$  avis  (unit @t)                                                    :: element id
+$  apex  $~([1 1] loci)                                               :: top left coordinate of an element
+$  loci  [x=@ud y=@ud]                                                :: coordinates
+$  modi  [x=@ud y=@ud]                                                :: box extent
+$  muri  [l=@ud r=@ud t=@ud b=@ud]                                    :: box perimeter widths
+$  rami  (lest [=axis =ager])                                         :: element key
+$  axis  ?(%b %l %n)                                                  :: element positioning category
+$  ager  @ud                                                          :: element number
+$  vox   (list lina)                                                  :: rows of text
+$  lina  (list @c)                                                    :: a row of text
+$  fila  [d=(set deco) b=tint f=tint]                                 :: element style
+$  acia  [d=(unit (set deco)) b=(unit tint) f=(unit tint)]            :: alternate element style
+$  fuga  [d=?(%col %row) b=?(%wrap %clip)]                            :: positioning flow
+$  iter  modi                                                         :: scroll position
+$  sola  modi                                                         :: scroll content dimensions
+$  ad    ?(%l %r %t %b)                                               :: direction
+$  via   ?(%h %v)                                                     :: orientation
+$  ora   ?(%light %heavy %double %arc %blank %~)                      :: line style
+$  nodi  (pair fila @c)
+$  lux   blit:dill
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
:: ... interactivity context
:: ... intersection types
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
::  +$  opus  ... opus will be the name for the render schematic type
:: ...
+$  vela  manx
+$  urbs  $~([50 25] [x=@ud y=@ud])
+$  ego
  $:  =urbs  =deus
  ==
::
+$  card  card:agent:gall
--
::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  
=|  ego
=*  ego  -
^-  agent:gall
=<
|_  bol=bowl:gall
+*  hoc  .
++  on-init
  ^-  (quip card _hoc)
  [~ hoc]
++  on-save
  ^-  vase
  !>(urbs)
++  on-load
  |=  old=vase
  ^-  (quip card _hoc)
  =/  ol  (mole |.(!<(^urbs old)))
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
      %json
    =/  zon  (ineo !<(json vase))
    ~&  >  zon
    [~ hoc]
    ::
  ==
::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  
++  on-watch
  |=  =path
  ^-  (quip card _hoc)
  ?+  path  !!
    ::
      [%homunculus-http ~]
    :: =/  aul=(list ^fons)  ~(tap in ^-((set ^fons) (snag cor aula)))
    :: =/  bac=visa  (rbox [1 1] urbs =|(res -(size urbs, look [~ ~ ~])))
    :: =/  vis=visa  (gyro aul bac)
    :: =/  aru=(unit ara)  (~(get by arae) fons)
    :: :_  hoc
    :: :_  ~
    :: :*  %give  %fact  ~[/homunculus-http]  %json
    ::     !>  ^-  json  :-  %s  %-  crip
    ::     ^-  tape  :-  '\\x1b[1;1H\\x1b[3J\\x1b[0J'
    ::     %-  volo  ^-  lux  :-  %mor
    ::     :~  (supo ~ vis)
    ::         ?:  p.luna
    ::           =.(p.luna | (supo ~ visa.q.luna))
    ::           (fero rex.q.luna equi.q.luna esse.q.luna)
    ::         ?^  aru  (fero rex.u.aru equi.u.aru esse.u.aru)  (fero ~ ~ ~)
    ::     ==
    :: ==
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
++  dolo                           :: get default styles for a semantic element
  |=  el=@tas
  ^-  vena
  ?+  el
    :*  size=[[%i 0] [%i 0]]
        padd=[[%c 0] [%c 0] [%c 0] [%c 0]]
        marg=[[%c 0] [%c 0] [%c 0] [%c 0]]
        flex=[0 0]
        flow=[%row %clip]
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
      %blank   $(ora.ars %blank, a t.a)
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
      %blank   $(ora.ars %blank, a t.a)
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
++  obeo                           :: get border sizes from a list of border elements
  |=  bor=marl                     :: defaults to 1 if an element is found and no valid size is specified
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

::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  
::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  
::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  
::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  
::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  
::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  

++  geno                           :: turn sail into element state
  |=  [vel=vela loc=(unit loci) =ara]
  ^-  dei
  =/  m=marl                   ~[vel]
  =/  pl=fila                  [~ ~ %w]
  =/  pa=acia                  [~ ~ ~]
  =/  px=as                    [%c x.urbs]
  =/  py=as                    [%c y.urbs]
  =/  pow=fuga                 [%row %clip]
  =/  prx=@ud                  x.urbs
  =/  pry=@ud                  y.urbs
  =/  pape=apex                ?^(loc u.loc *apex)
  =/  vape=apex                pape
  =/  vir=[n=@ud o=@ud i=@ud]  [0 0 0]
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
  =?  q.w.size.vena  &(wcen !=(%i p.w.size.vena))
    =/  m=@ud  (add q.l.marg.vena q.r.marg.vena)
    ?:  (gth m q.w.size.vena)  0
    (sub q.w.size.vena m)
  =?  q.h.size.vena  &(hcen !=(%i p.h.size.vena))
    =/  m=@ud  (add q.t.marg.vena q.b.marg.vena)
    ?:  (gth m q.h.size.vena)  0
    (sub q.h.size.vena m)
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
  :: =/  csiz=$@(~ [w=@ud h=@ud])
  ::   ?.  |(fex imp ?=(^ gro) ?=(%scroll -.ars))  ~
  ::   (cogo vape k ?:(|(fex ?=(^ gro)) esse.c ?:(wrim esse.b esse.a)))
  =^  aqu  nor
    ?.  .?(aqu)  [~ nor]
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
      |-  ^-  aqua
      ?~  aqu  ~
      :-  i.aqu(size ?:(=(0 rem) bas +(bas)))
      $(aqu t.aqu, rem ?:(=(0 rem) 0 (dec rem)))

  :: this ultimately needs to move elements that are in ndei to make room for the grow elements
  :: CHANGE the product here to be [new-aqu updated-nor] and return it in the above tisket.
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
        ?-(d.flow.vena %row move, %col 0)  ?-(d.flow.vena %row 0, %col move)
        alim  ?:(fex ~ ?:(wrim visa.b visa.a))  u.el
      ==
    =:  esse.op  (~(put by esse.op) ek u.el)
        visa.op  (~(uni by visa.u.el) visa.op)
      ==
    %=  $
      i          +(i)
      ager.i.ek  +(ager.i.ek)
      marg
        ?-  d.flow.vena
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
            ?-(d.flow.vena %row move, %col 0)  ?-(d.flow.vena %row 0, %col move)
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
      ?-  d.flow.vena
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
        px    w.size.vena
        py    h.size.vena
        pl    fil
        pa    aci
        pow   flow.vena
        prx   arx
        pry   ary
        pape  vape
        plim  alim
        pscr  nscr
        slim  nsli
        brex  crex
        vir   [0 0 0]
    ==  ==
  =?  csiz  &(?=(^ gro) |(fex imp ?=(%scroll -.ars)))
    (cogo vape k esse.c)
  =?  c  fex
    =/  [ek=rami op=opus]  [[[%~ 0] k] [~ ~]]
    =/  wra
      =|  [out=@ud siz=@ud acc=(map @ud @ud)]
      ?:  ?=(%clip b.flow.vena)  acc
      |-  ^-  (map @ud @ud)
      =/  el=(unit ens)  (~(get by esse.c) ek)
      ?~  el  ?:(=(0 siz) acc (~(put by acc) out siz))
      =/  nut=@ud
        ?-  d.flow.vena
          %row  ?:((lth t.marg.res.u.el y.apex.u.el) (sub y.apex.u.el t.marg.res.u.el) 1)
          %col  ?:((lth l.marg.res.u.el x.apex.u.el) (sub x.apex.u.el l.marg.res.u.el) 1)
        ==
      =/  niz=@ud
        ?-  d.flow.vena
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
        (div (mul x.flex.vena (sub arx x)) 100)
      ?:  |(?=([%row %clip] flow.vena) ?=([%col %wrap] flow.vena))
        ?^(csiz w.csiz 0)
      ?:  ?=([%col %clip] flow.vena)
        (add w.size.res.u.el (add l.marg.res.u.el r.marg.res.u.el))
      ?:  ?=([%row %wrap] flow.vena)
        =/  w=(unit @ud)
          %-  %~  get  by  wra
          ?:((lth t.marg.res.u.el y.apex.u.el) (sub y.apex.u.el t.marg.res.u.el) 1)
        ?^(w u.w 0)
      0
    =/  movy=@ud
      =;  y=@ud
        ?:  (gte y ary)  0
        (div (mul y.flex.vena (sub ary y)) 100)
      ?:  |(?=([%col %clip] flow.vena) ?=([%row %wrap] flow.vena))     
        ?^(csiz h.csiz 0)
      ?:  ?=([%row %clip] flow.vena)
        (add h.size.res.u.el (add t.marg.res.u.el b.marg.res.u.el))
      ?:  ?=([%col %wrap] flow.vena)
        =/  h=(unit @ud)
          %-  %~  get  by  wra
          ?:((lth l.marg.res.u.el x.apex.u.el) (sub x.apex.u.el l.marg.res.u.el) 1)
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
    (cogo vape k ?:(wrim esse.b esse.a))
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
    ?:  wris
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
    tvir
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
  =?  alim  |(wrim x.pscr y.pscr)
    :-  ?:  x.pscr
          =/  r=@ud  +((add br q.r.padd.vena))
          ;:(add x.aape bl q.l.padd.vena ?:((gth r q.w.size.vena) 0 (sub q.w.size.vena r)))
        ?:  wrim
          =/  x=@ud  (add x.vape ?:(=(0 arx) 0 (dec arx)))
          ?:  (gth x x.plim)
            x.plim
          x
        x.alim
    ?:  y.pscr
      =/  b=@ud  +((add bb q.b.padd.vena))
      ;:(add y.aape bt q.t.padd.vena ?:((gth b q.h.size.vena) 0 (sub q.h.size.vena b)))
    ?:  wrim
      =/  y=@ud  (add y.vape ?:(=(0 ary) 0 (dec ary)))
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
    =.  u.el
      %:  mino
        movx  movy
        ?~(slim alim [(min x.slim x.alim) (min y.slim y.alim)])
        visa.a  u.el
      ==
    %=  $
      ager.i.ek  +(ager.i.ek)
      a
        %=  $
          ek  [[%b 0] ek]
          a   [(~(put by esse.a) ek u.el) (~(uni by visa.u.el) visa.a)]
    ==  ==
  =?  ars  ?=(%text -.ars)
    :-  -.ars
    =/  [x=@ud y=@ud]
      :-  ?:(?=(%row d.pow) n.vir o.vir)
      ?:(?=(%col d.pow) n.vir o.vir)
    %^  oro  ?:(?=(%i p.px) ~ [~ ?:((lte x prx) (sub prx x) 0)])
      ?:(?=(%i p.py) ~ [~ ?:((lte y pry) (sub pry y) 0)])
    lina
  =/  ares=res
    ?.  ?=(%text -.ars)
      :*  ?.  ?=(%pattern -.ars)  [q.w.size.vena q.h.size.vena]
          [?^(vox.ars (lent i.vox.ars) 0) (lent vox.ars)]
          [q.l.padd.vena q.r.padd.vena q.t.padd.vena q.b.padd.vena]
          [q.l.marg.vena q.r.marg.vena q.t.marg.vena q.b.marg.vena]
          flex.vena
          flow.vena
          fil
      ==
    =/  len=@ud
      (roll ^-(vox vox.ars) |=([i=^lina a=@ud] =/(l=@ud (pono i) (max a l))))
    =/  lim=(unit @ud)
      ?:(?=(%i p.px) ~ [~ (sub prx ?:(?=(%row d.pow) n.vir o.vir))])
    :*  [?~(lim len (min len u.lim)) (lent vox.ars)]
        [0 0 0 0]
        [0 0 0 0]
        [0 0]
        [%row %wrap]
        pl
    ==
  =?  a  ?=(^ bor)
    %=  $
      m     bor
      k     [[%b 0] k]
      px    w.size.vena
      py    h.size.vena
      pl    fil
      pa    aci
      pow   flow.ares
      prx   w.size.ares
      pry   h.size.ares
      pape  aape
      plim  =?  plim  |(x.pscr y.pscr)
              :-  ?:(x.pscr (add x.aape ?:(=(0 w.size.ares) 0 (dec w.size.ares))) x.plim)
              ?:(y.pscr (add y.aape ?:(=(0 h.size.ares) 0 (dec h.size.ares))) y.plim)
            :-  ?:  =(0 arx)  0
                =/  x=@ud  ;:(add x.alim bl br l.padd.ares r.padd.ares)
                (min x x.plim)
            ?:  =(0 ary)  0
            =/  y=@ud  ;:(add y.alim bt bb t.padd.ares b.padd.ares)
            (min y y.plim)
      pscr  nscr
      vape  aape
      brex  crex
      vir   [0 0 0]
    ==
  =?  ars  |(?=(%input -.ars) ?=(%scroll -.ars) ?=(%checkbox -.ars))
    =/  key=(unit rami)
      ?~  avis  [~ k]
      (~(get by aves.ara) u.avis)
    ?+  -.ars  ars
        %input
      =/  old=(unit ens)
        ?~  key  ~
        (~(get by esse.ara) u.key)
      ?.  &(?=(^ old) ?=(%input -.ars.u.old))
        ?~  lina  ars
        ars(vox (oro [~ w.size.ares] [~ h.size.ares] lina))
      ?:  =(size.res.u.old size.ares)
        ars.u.old
      %_  ars.u.old
        ab   0
        i    [0 0]
        vox  (oro [~ w.size.ares] [~ h.size.ares] ^-(^lina (zing vox.ars.u.old)))
      ==
        %checkbox
      ?~  key  ars
      =/  old=(unit ens)  (~(get by esse.ara) u.key)
      ?~  old  ars
      ?.  ?=(%checkbox -.ars.u.old)  ars
      ars.u.old
        %scroll
      ?~  key  ars
      =/  sol=sola
        ?~  csiz  [0 0]
        :-  ?:((gth arx w.csiz) 0 (sub w.csiz arx))
        ?:((gth ary h.csiz) 0 (sub h.csiz ary))
      =/  old=(unit ens)  (~(get by esse.ara) u.key)
      =/  itr=iter
        ?~  old
          [0 0]
        ?.  ?=(%scroll -.ars.u.old)
          [0 0]
        :-  (min x.iter.ars.u.old x.sol)
        (min y.iter.ars.u.old y.sol)
      [%scroll itr [bl br bt bb] sol]
    ==
  =?  a  &(?=(%scroll -.ars) !&(=(0 x.iter.ars) =(0 y.iter.ars)))
    =/  pv=visa  (~(dif by (rbox aape plim ares)) visa.a)
    (eo rex.ara ~ iter.ars k pv x.vape y.vape x.alim y.alim esse.a visa.a)
  =/  rend=visa
    %:  viso
      aape
      ?.  crex  ares
      %_  ares
        d.look  ?^(d.aci u.d.aci d.look.ares)
        b.look  ?^(b.aci u.b.aci b.look.ares)
        f.look  ?^(f.aci u.f.aci f.look.ares)
      ==
      ars
      ?~  slim  plim
      [(min x.slim x.plim) (min y.slim y.plim)]
    ==
  =?  rend  !?=(%layer -.ars)  (~(dif by rend) visa.a)
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
  %=  $
    m     t.m
    k     [[axis.i.k +(ager.i.k)] t.k]
    vape
      ?:  |(?=(%layer -.ars) ?=(%border -.ars))  pape
      =/  vx=@ud  ?-(d.pow %row n.vir, %col o.vir)
      =/  vy=@ud  ?-(d.pow %row o.vir, %col n.vir)
      [(add x.pape vx) (add y.pape vy)]
    a
      =/  [limx=@ud limy=@ud]
        :-  (add x.aape ?:(=(0 w.size.ares) 0 (dec w.size.ares)))
        (add y.aape ?:(=(0 h.size.ares) 0 (dec h.size.ares)))
      =?  limx  &(!x.pscr (lth x.plim limx))  x.plim
      =?  limy  &(!y.pscr (lth y.plim limy))  y.plim
      :_  (~(uni by visa.a) rend)
      %+  %~  put
            by
          esse.a
        k
      ^-  ens
      :*  ares  aape  [limx limy]  pitr  avis
          aci(d ?:(?=(%text -.ars) d.aci ~))
          rend  ars
      ==
  ==

::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  
::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  
::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  
::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  
::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  
::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  


::
--
