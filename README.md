# %homunculus

> *Look there, a flash! — We now can really hope:* 
> *if we compound the human substance* 
> *by mixing many hundred substances* 
> *— the mixture is what matters — carefully* 
> *and seal it tight with clay in a retort,* 
> *then re-distill it properly,* 
> *our secret labors will be finished.* 
> 
> ~ Goethe, *Faust*, 6850

%homunculus is an agent that turns Sail elements into a terminal interface. For the user, %homunculus transforms the terminal into a UI for your ship, laid out in 2D space and interacted with similarly to the browser. For the developer, %homunculus allows you to use XML/Sail to create non-CLI TUIs as easily as if you were building a Web UI.

## Users

Note: %homunculus is under active development. Currently, you can test it out using either the Rust client or the Bash script. This will establish an Eyre channel connection to your ship in order to send terminal input, and stream rendered output from %homunculus.

Most app windows can be interacted with using the `arrow` keys to move around to different selection points. Pressing `enter` on a selection point activates it (which may or may not have an implementation in the app). %homunculus also supports mouse events; clicking on a selection point will select and activate it.

You can change which window is active by pressing `alt+arrow`. Pressing `alt+c` will close whichever window is active, and `ctrl+c` will exit the client.

Pressing `esc` will bring up a menu providing various window management options and a series of customizable poke forms associated with each number key.

Windows may also have their own custom hotkeys implemented.

## Developers

In order for an app to open a session with %homunculus — or update an existing session — it must send a poke with the `%homunculus-session` mark, where the vase contains a cell with optional hotkeys at the head, and manx at the tail. (Currently, session updates must contain the whole manx representing your interface from the root.)

If your app has a session open, %homunculus will send pokes with the mark `%homunculus-event`. These event pokes mainly consist in a tag for the kind of event, and an identifier that you will have placed on the element which triggered the event, specified with the `id` attribute.

Further details about the API can be found in `/sur/homunculus/hoon`.

#### *Sail*

%homunculus uses its own collection of elements and attributes, rather than HTML and CSS. However, it uses a similar "box model", meaning that each element potentially has margin, borders, padding, and nested child elements. Elements may be sized with numbers representing an amount of characters in the terminal, percentages, or implicitly sized based on its child content when no size attributes are specified.

*Elements:*

- `box` — Just a plain box; the default element
- `select` — A selection point that may be navigated to with the arrow keys and activated with enter, or clicked to select and activate it. If the id attribute is placed on it, %homunculus will send you %select events when it is selected, and %act events when it is activated, both containing the value of the id attribute (see the event handling section below). (Note: the scroll, input, and checkbox elements are also selection points, but do not send %select or %act events.)
- `layer` — A container element that creates a separate layer within a parent element (but doesn't render any characters itself). Elements in layers above will render over those in the layers below. 
- `border-left`, `-right`, `-top`, `-bottom` — Elements which serve as alternatives to the border attribute. This allows you to add borders independently for each side, or to have borders that contain child elements rendered along them (a common TUI design pattern).
- `line-h`, `-v` — Elements which render horizontal or vertical lines. When lines or borders touch each other, %homunculus will render intersection characters to connect them together, unless they are separated by layer.
- `scroll` — A container element that lets you make overflowing content scrollable. By default, anything that overflows is clipped, but a scroll element will let you use the arrow keys to scroll through it. (Currently, the performance is not great, so this should be used in limited situations.)
- `pattern` — A container for a text node that repeats the content of the text node to fill its width and height. This allows you to create patterns and textures, e.g. as backgrounds that may be layered over, or within border elements as custom border designs.
- `form` — A container element that defines a form. All inputs and checkboxes within the form will be included in the data map in the %form event for this element. This event will be triggered for your element when a submit element inside of it is activated. All inputs and checkboxes in the form must have id attributes on them; these ids will be the keys in the data map. An id attribute can also be used on the form element itself to identify it.
- `input` — An input that lets the user type text, and send it within a form. The input element has two different rendering strategies: when the height is 1 the text will render in a single scrollable row, and when the height is greater than 1, the text will render in a series of vertically scrollable rows.
- `checkbox` — A checkbox. Its value in form data is a loobean for whether it is checked or not.
- `radio` — A container element that defines a radio group of checkboxes. In other words, in a radio element, there can only be one checkbox that is checked at a time, and when one is checked, the others will un-check.
- `submit` — An element that is used to trigger a %form event on activation.
- Text nodes are written with the usual Sail syntax (either with a col after the element tag in wide-form, or with micfas in tall form).

*Attributes:*

- `w`, `h` — Width and Height; takes a number representing a character amount, or a percentage relative to the room in the parent element; also takes the value `grow` which makes the element grow on that axis to fit the available room within a parent element not taken up by the other sibling elements (this is very useful for making sure a child element always fills the space of a parent element regardless of whether the parent size is an odd or even amount of characters). If the width and height attributes are left out, the element will be sized based on its child content.
- `p`, `pl`, `pr`, `pt`, `pb`, `px`, `py` — Padding; p defines padding for all sides, px for the left and right side, pl for the left side only, etc.
- `m`, `ml`, `mr`, `mt`, `mb`, `mx`, `my` — Margin; m defines margin for all sides, mx for the left and right side, ml for the left side only, etc.
- `fx`, `fy` — Flex x and Flex y; takes either "start", "center", "end", or a number; this positions the element's children along the x or y axis by that amount. E.g. if fx and fy are both "center", the child content will be centered within the element.
- `fl` — Flow; takes either "row", "column", "row-wrap", or "column-wrap"; this determines the flow with which the child elements are positioned; elements can be arranged in a row or column, and they can either clip if they overflow the parent element in that direction, or wrap.
- `cb`, `cf` — Background color and Foreground color; takes either a hex color, or one of the 8 original terminal colors: "red" "green" "blue" "cyan" "magenta" "yellow" "white" "black".
- `d` — Text decorations; takes either "bold", "underline", or "blink"; multiple d attributes will compound (terminal emulator support may vary).
- `b` — Borders; a shorthand for adding four border elements; takes "light", "heavy", "double", or "arc" to specify the style of the border elements. If used on a border element itself, it will define the line style of that border element.
- `l` — Line style; specifies the style of a line element, takes the same values as the b attribute.
- `select-cb`, `select-cf`, `select-d` — Select style; takes the same values as cb, cf, and d; when placed on a select element, or any element under a select element, the element will change to this style when the select element is selected.
- `id` — ID; an identifier for an element which makes %homunculus send event pokes related to that element.

---

> *It works! The moving mass grows clearer,* 
> *and my conviction the more certain:* 
> *what's been extolled as Nature's mystery* 
> *can be investigated, if but Reason dare,* 
> *and what she used to let be just organic* 
> *we can produce by crystallizing.* 
> 
> ~ Goethe, *Faust*, 6855
