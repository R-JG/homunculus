|%
+$  component-event
  $:  com-key=@t
      =path
      data=(map @t @t)
  ==
+$  event
  $%  :: [%open ~]
      :: [%close ~]
      [%act p=path]
      [%form p=path q=form-data]
      [%select p=path]
      [%keybinding p=path]
      [%scroll-trigger-up p=path]
      [%scroll-trigger-down p=path]
  ==
+$  form-data  (map path @t)
+$  keybindings  (list (pair keybinding path))
+$  keybinding
  $@  @t
  $%  [%delete ~]
      [%enter ~]
      [%back ~]
      [%tab ~]
      [%arrow ?(%l %r %u %d)]
  ==
+$  update  (lest update-card)
+$  update-card
  $%  [%element p=manx]
      :: [%set-select p=path]
      [%set-scroll-position p=?(%c %p) q=@ r=path]
  ==
+$  action
  $%  [%update p=update]
      [%register ~]
  ==
+$  system-action
  $%  [%update p=update]
      [%open-session p=session-source q=session-open]
      [%close-session p=session-source]
      [%change-frame p=frame-index]
  ==
+$  session-open
  $%  [%new-frame p=?(%l %r)]
      [%current-frame p=layout-dir q=layout-key]
  ==
+$  session-source  (pair @p @tas)
+$  register  (set session-source)
+$  frame-index  @
+$  frames  $~(~[*frame] (list frame))
+$  frame
  $:  =layout
  ==
+$  layout-dir  ?(%l %r %t %b %c)
+$  layout-key  (list ?(%0 %1))
+$  layout 
  $~  [%$ *session-source]
  $%  [%$ p=session-source]
      [%v p=@ l=layout r=layout]
      [%h p=@ t=layout b=layout]
  ==
--

