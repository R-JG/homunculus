/-  homunculus-api
|%
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
      [%mod mod=?(%ctl %alt %shf) key=$~([%txt ~] zona)]
      [%aro p=?(%d %l %r %u)]
      [%txt p=lina]
      [%chr p=@c]
      [%bac ~]  [%del ~]  [%ret ~]  [%esc ~]
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
+$  ovum
  $:  size=[w=as h=as]
      padd=[l=as r=as t=as b=as]
      marg=[l=as r=as t=as b=as]
      flex=[x=@ud y=@ud]
      flow=fuga
      look=acia
  ==
+$  cor   @
+$  fons  pith:neo
+$  aula  $~(~[~] (list (set fons)))
+$  vela  manx
+$  ara
  $:  =vela  =muri
      =esse  =ales
      =visa  =omen
      =aves  =gens
      =ordo  =rex
      =equi  =mus
  ==
+$  arae  (map fons ara)
+$  luna  (pair bean ara)
+$  urbs  $~([50 25] [x=@ud y=@ud])
+$  ego
  $:  =cor  =fons  =urbs
      =luna  =aula  =arae
  ==
--
