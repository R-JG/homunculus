|%
::
+$  update
  $%  [%full p=manx]
      [%branch p=(list manx)]
      [%hotkeys p=hotkeys]
      [%set-select p=path]
  ==
::
+$  event
  $%  [%select p=path]
      [%act p=path]
      [%form p=path q=form-data]
      [%hotkey p=path]
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
::
+$  menu
  $%  [%update p=update]
      [%close-session p=@]
  ==
::
--
