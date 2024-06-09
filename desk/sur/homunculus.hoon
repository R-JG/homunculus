|%                                  :: ::    %homunculus API    :: ::
::                                  ::
+$  session                         :: session ::
  $:  metadata                      :: :: poke data sent to homunculus to open or update a session
      manx                          :: :: the metadata can be null, and the manx must be the root.
  ==                                ::
::                                  ::
+$  event                           :: event ::
  $%  [%select =id]                 :: :: poke data sent from homunculus on an event for your session
      [%act =id]                    :: :: %select and %act send the id placed on the associated %select element.
      [%form =id data=(map id @t)]  :: :: %form sends the id of the %form element,
      [%hotkey =id]                 :: :: and a map of ids of its %input elements to their values on submit.
  ==                                :: :: %hotkey sends the id associated with a hotkey in the session metadata.
::                                  ::
+$  id  @t                          ::
::                                  ::
+$  metadata                        :: metadata ::
  $@  ~                             :: :: optionally null metadata, used for advanced session updates
  $:  =select-default               :: :: when select-default is included in an update,
      =hotkeys                      :: :: it sets the currently selected element to the one matching the id.
  ==                                :: :: the hotkey list defines events which are triggered by the key and contain the id.
::                                  ::
+$  select-default  (unit id)       ::
+$  hotkeys  (list [hotkey id])     ::
::                                  ::
+$  hotkey                          :: hotkey ::
  $@  @t                            :: :: this can be used to define a custom hotkey event.
  $%  [%delete ~]                   :: :: as a cord, the key can be set to any character,
      [%enter ~]                    :: :: otherwise, one of the head-tagged options.
      [%back ~]                     ::
      [%tab ~]                      ::
      [%arrow ?(%l %r %u %d)]       ::
  ==                                ::
::                                  :: 
--
