# Moodules

Prototype of OO style Libraries for Dune. This is mostly just a much more limited implementation of virtual libraries, but with some fixes for bugs that I discovered in virtual libraries.

- Jump to location working correctly.
- Mysterious link time bugs gone.

This works by simply copying modules around, which simplifies the implementation.

- Allows depending on a concrete library that "extends" a virtual library, and
  then rely on additional modules inside of that concrete library that were not
  part of the virtual library.
- Allows different concrete implementations to diverge in their `.rei` files.

The major downside is that (for now) consumers must specify exactly which
concrete implementation they rely on - you cannot just depend on a virtual
"base class" library, and then defer selection of concrete library until link
time (executable). I could try to add that feature though.

## Usage

Each moodule library is defined like this:

```
(* -*- tuareg -*- *)
open Jbuild_plugin.V1;;
send (String.concat "\n" (run_and_read_lines "Moodules"));;
{|
  PUT YOUR CUSTOM DUNE CONFIG FOR THIS LIBRARY HERE
|};;
```

Moodules currently only work within the same repo/package, and must always use public names.

For example:

```
(* -*- tuareg -*- *)
open Jbuild_plugin.V1;;
send (String.concat "\n" (run_and_read_lines "Moodules"));;
{|
  (library
    (public_name virtualish.baseclass)
    (name VirtualishBaseclass)
  )
|};;
```

Then other modules may "extend" that "moodule" by using `(extends_moodule )`.

```
(* -*- tuareg -*- *)
open Jbuild_plugin.V1;;
send (String.concat "\n" (run_and_read_lines "Moodules"));;
{|
(library
  (public_name virtualish.subclass)
  (extends_moodule virtualish.virtual)
  (name VirtualishSubclass)
)
|};;
```

The contents of the subclass "inherit" the base class.

If you want the base library to never be instantiable, or perhaps even be
misformed until extended, it is by default considered abstract just by nature
of using the Moodules plugin, and not specifying an `(extends )` directive.


```
(* -*- tuareg -*- *)
open Jbuild_plugin.V1;;
send (String.concat "\n" (run_and_read_lines "Moodules"));;
{|
  (library
    (public_name virtualish.baseclass)
    (name VirtualishBaseclass)
  )
|};;
```
