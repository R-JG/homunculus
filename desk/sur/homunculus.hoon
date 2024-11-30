|%
::
+$  update  (lest update-card)
::
+$  update-card
  $%  [%element p=manx]
      :: [%set-select p=path]
      [%set-scroll-position p=?(%c %p) q=@ r=path]
  ==
::
+$  event
  $%  [%open ~]
      [%close ~]
      [%select p=path]
      [%act p=path]
      [%form p=path q=form-data]
      [%hotkey p=path]
      [%scroll-trigger p=?(%up %down) q=path]
  ==
::
+$  form-data  (map path @t)
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
      [%change-frame p=frame-index]
  ==
+$  menu-diff
  $%  [%all-frames p=frame-index q=frames]
      [%active-frame p=frame-index]
      [%put-register p=session-source]
      [%del-register p=session-source]
  ==
::
+$  register  (set session-source)
+$  frame-index  @
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
