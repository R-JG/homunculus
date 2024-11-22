|%
::
+$  update
  $%  [%register ~]
      [%root p=manx]
      [%branch p=(list manx)]
      :: [%hotkeys p=hotkeys]
      :: [%set-select p=path]
      :: [%set-scroll-position p=?(%c %p) q=@ r=path]
  ==
::
+$  event
  $%  [%open ~]
      [%close ~]
      [%select p=path]
      [%act p=path]
      [%form p=path q=form-data]
      [%hotkey p=path]
      [%scroll p=scroll-event]
  ==
::
+$  form-data  (map path @t)
::
+$  scroll-event
  $%  [%trigger p=?(%up %down) q=path]
  ==
::
+$  hotkeys  (list (pair hotkey path))
+$  hotkey
  $@  @t
  $%  [%delete ~]
      [%enter ~]
      [%back ~]
      [%tab ~]
      [%arrow ?(%l %r %u %d)]
  ==
::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  ::  
+$  menu-update
  $%  [%update p=update]
      [%load-state ~]
      [%open-session p=session-source q=session-open]
      [%close-session p=session-source]
  ==
+$  menu-diff
  $%  [%all-frames p=active-frame q=frames]
      [%active-frame p=active-frame]
      [%put-register p=session-source]
      [%del-register p=session-source]
  ==
::
+$  register  (set session-source)
+$  active-frame  @
+$  frames  $~(~[*frame] (list frame))
+$  frame
  $:  =layout
  ==
+$  session-source  (pair @p @tas)
+$  session-open
  $%  [%new-frame p=?(%l %r)]
      [%current-frame p=layout-dir q=layout-key]
  ==
+$  layout-dir  ?(%l %r %t %b %c)
+$  layout-key  (list ?(%0 %1))
+$  layout 
  $~  [%$ *session-source]
  $%  [%$ p=session-source]
      [%v p=@ l=layout r=layout]
      [%h p=@ t=layout b=layout]
  ==
::
--
