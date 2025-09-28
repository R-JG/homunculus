|%
+$  crow  [=path data=(map @t @t)]               :: event for a component
+$  blow  (list cage)                            :: pokes for an agent from a component
+$  gull                                         :: mast actions
  $%  [%navigate src=ship ses=buoy to=rope]      ::
  ==                                             ::
+$  hull                                         :: component sample
  $:  our=ship                                   ::   our
      src=ship                                   ::   src
      ses=buoy                                   ::   session id
    ::                                           ::
      bas=knot                                   ::   base url segment bound to
      rut=path                                   ::   rest of the url path
      que=quay                                   ::   query params
    ::                                           ::
      now=time                                   ::   time
      eny=@uvJ                                   ::   entropy
    ::                                           ::
      par=gust                                   ::   component params
      res=gale                                   ::   resources
  ==                                             ::
+$  quay  (map @t @t)                            :: query params
+$  gust  (map @tas @t)                          :: component params
+$  gale                                         :: component resources
  %+  map  @tas                                  ::
  $:  src=path                                   ::
      fil=vase                                   ::
  ==                                             ::
+$  boom                                         :: resource spec
  %-  list                                       ::
  $:  name=@tas                                  ::   map key
      mark=@tas                                  ::   mark, possibly converted to
  ==                                             ::
+$  mast                                         :: component
  $:  $:  =boom                                  ::
      ==                                         ::
      $_  ^|                                     ::
      |_  hull                                   ::
      ++  spar  *$-(crow blow)                   ::
      ++  sail  *manx                            ::
      --                                         ::
  ==                                             ::
+$  buoy  @                                      :: session id
+$  bind  (pair knot line)                       :: base url segment to root component
::+$  dock  (map knot line)                        :: bindings
::+$  deck  (map hook mast)                        :: component cache
+$  hook  term                                   :: component name
+$  rode  cord                                   :: component key
+$  rope                                         :: url
  $:  bas=knot                                   ::   base segment
      rut=path                                   ::   rest of path
      que=quay                                   ::   query params
  ==                                             ::
+$  line                                         :: component reference and inputs
  $:  com=hook                                   ::
      par=gust                                   ::
      res=pool                                   ::
  ==                                             ::
+$  pool  (map @tas path)                        :: resources for a component
::
+$  isle  $~([*line ~] (pair line (map rode isle)))
+$  gulf  (map [ship buoy] (pair rope isle))
::
::+$  tide  [?(%add %del) p=rode q=path]           :: resource subscription effect
::+$  wake                                         :: component creation effect
::  $:  res=(set tide)                             ::
::      new=isle                                   ::
::  ==                                             ::
::+$  cove                                         :: component state
::  $:  dif=atom                                   ::
::      bom=boom                                   ::
::      aft=manx                                   ::
::      lin=line                                   ::
::  ==                                             ::
::+$  isle  (map rode cove)                        ::
::+$  gulf  (map [ship buoy] (pair rope isle))     ::
::+$  navy                                         :: resource to client subscription state
::  %+  map  path                                  ::
::  %+  map  [ship buoy]                           ::
::  %-  set  rode                                  ::
::
::+$  grog
::  $%  [%diff component=rode counter=@ diff=(list mess)]
::      [%navigate to=@t]
::  ==
::+$  mess
::  $%  [%new parent-key=@t index=@t data=@t]
::      [%delete keys=(list @t)]
::      [%move key=@t index=@t]
::      [%change-attr key=@t del=(list @t) new=(list [k=@t v=@t])]
::      [%text container-key=@t data=@t]
::  ==
::+$  diff
::  %+  pair
::  $:  res=(set tide)
::      del=(set rode)
::      add=isle
::  ==
::  %-  list  mess
::
  ::
::
:: ++make
:: produce a %mast component element.
++  make
  |=  $:  component=hook
          params=(list [@tas cord])
          resources=(list [@tas path])
      ==
  ^-  manx
  :_  ~
  :*  %mast
      :-  [%hook component]  ~
      %+  weld
      ^-  mart
      %+  turn  params
      |=  [k=@tas v=cord]
      :-  [%gust k]  (trip v)
      ^-  mart
      %+  turn  resources
      |=  [k=@tas v=path]
      :-  [%gale k]  (spud v)
  ==
::
--

