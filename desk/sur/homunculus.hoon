|%
::
+$  update
  $%  [%register ~]
      [%full p=manx]
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
::
+$  menu
  $%  [%update p=update]
      [%close-session p=@]
  ==
::
--
