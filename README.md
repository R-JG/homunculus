# %homunculus

> Look there, a flash! - We now can really hope:
if we compound the human substance
by mixing many hundred substances
— the mixture is what matters — carefully
and seal it tight with clay in a retort,
then re-distill it properly,
our secret labors will be finished.
> 
> ~ Goethe, *Faust*, 6850

%homunculus is an agent that turns Sail elements into a terminal interface. For the user, %homunculus renders a desktop environment for your ship in the terminal — with interfaces that are not inherently CLIs — making the terminal an alternative to the browser. For the developer, %homunculus allows you to use XML/Sail to create interfaces as easily as if you were building for the Web.

## Users 🌞︎︎

To use %homunculus in your terminal, a simple script is run which connects to your ship via HTTP, meaning that you can interact with your ship remotely (or over localhost). With the %homunculus desk installed, you can fetch this script by running:

```bash
bash <( curl -s {your-ship-url}/homunculus -d {your-auth-code} )
```

Alternatively, you can save and run the script from the repo locally (or write your own; it just needs to send input characters and print the rendered characters from the SSE stream).

*Note: compatibility with different terminal emulators may vary.*

In %homunculus, you may have one or more windows open in a single frame, and one or more frames that may be switched between. 

Most app windows can be interacted with using the `arrow` keys to move around to different selection points. Pressing `enter` on a selection point activates it (which may or may not have an implementation in the app). %homunculus also supports mouse events; clicking on a selection point will select and activate it.

You can change which window is active by pressing `alt+arrow`. The location of the cursor will indicate which one is active. If there is another window in the direction of the arrow press, it will become active; if there isn't, the frame will be switched in that direction. Additionally, you can move windows (if they are smaller than the terminal size) with `shift+arrow` and `ctrl+arrow`. Pressing `alt+c` will close whichever window is active.

The `esc` key will bring up a menu that provides an interface for all of the above functionality as an alternative to the hotkeys. Importantly, the menu also contains a series of customizable poke forms associated with each number key. Pressing `alt+num` will send the poke in that form. This is useful for opening app windows within %homunculus.

Windows may also have their own custom hotkeys implemented.

You can exit %homunculus with `ctrl+c`.

## Developers 🌜︎︎

...

> It works! The moving mass grows clearer,
and my conviction the more certain:
what's been extolled as Nature's mystery
can be investigated, if but Reason dare,
and what she used to let be just organic
we can produce by crystallizing.
> 
> ~ Goethe, *Faust*, 6855
