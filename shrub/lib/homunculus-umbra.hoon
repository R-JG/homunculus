/-  homunculus-api
|%
::
+$  to-umbra
  $%  [%refresh (pair window frames)]
      [%poke-key num]
  ==
::
+$  from-umbra
  $%  [%session p=session.homunculus-api]
      [%change p=pith:neo]
      [%close p=pith:neo]
      [%move p=pith:neo q=(pair ?(%full %char) ?(%l %r %u %d))]
  ==
::
+$  window  pith:neo
+$  frame   (list window)
+$  frames  (list frame)
+$  num     ?(%1 %2 %3 %4 %5 %6 %7 %8 %9 %0)
+$  poke-key
  $@  ~
  $:  name=cord  =ship
      agent=term  =mark
      data=vase  data-txt=cord
  ==
+$  poke-keys
  $:  p1=poke-key  p2=poke-key
      p3=poke-key  p4=poke-key
      p5=poke-key  p6=poke-key
      p7=poke-key  p8=poke-key
      p9=poke-key  p0=poke-key
  ==
+$  umbra-state
  $:  base-kelvin=@
      active-window=window
      =frames
      move-mode=?(%char %full)
      sel-poke-num=(unit num)
      poke-message=tape
      poke-edit-mode=$~(| ?)
      =poke-keys
  ==
::
--
