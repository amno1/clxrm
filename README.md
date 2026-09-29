# clXrm

The X resource manager (Xrm) for Common Lisp: resource databases, the Xrm
matching algorithm, resource files, and the `RESOURCE_MANAGER` property
that `xrdb` loads. Works with both CLX and CLXCB.

The parser, database and matching algorithm are a port of
[xcb-util-xrm](https://gitlab.freedesktop.org/xorg/lib/libxcb-xrm), down to its
precedence quirks, with a few deliberate differences (see below).

## Dependencies

The core (clxrm) depends only on UIOP for portability and does no I/O beyond
resource files. The CLX and XCB backends depend on CLX and CLXCB respectively;
the tests need RT (bundled with SBCL, otherwise from Quicklisp).

## Installation

This is currently not in Quicklisp.  Clone to your hard drive and symlink the
directory into Quicklisp's `local-projects`, or clone directly there.  If you
don't use Quicklisp, load the `.asd` by whichever mechanism you prefer.

## Usage

Load either `clxrm/clx` or `clxrm/xcb` system, depending on whether you are
writing an application with CLX or CLXCB backend.

The functions (GET-RESOURCE, ADD-RESOURCE, GET-SEARCH-TABLE, …) keep CLX's
names and lambda lists, so a program written against CLX's resource manager
recognises them. clxrm/clx adds four conveniences that take a window or screen
rather than a display, as CLX's own server functions do: WM-RESOURCES,
SET-WM-RESOURCES, ROOT-RESOURCES, SET-ROOT-RESOURCES; the general forms in xrm
are the same functions with the display as the explicit first argument.
SET-ROOT-RESOURCES is also a DEFSETF target: (setf (root-resources screen) db)
stores db on the root window.

The core's package is `xrm` (so `xrm:get-resource`, as `xcb:` is for CLXCB),
with `clxrm` as a nickname; the system is `clxrm`.  The sub-systems' packages
are named like the systems: `clxrm/clx`, `clxrm/xcb`.

### Example:

```lisp
;; The user's resources, as xcb-util-xrm's xcb_xrm_database_from_default
;; finds them: RESOURCE_MANAGER, else ~/.Xresources, else ~/.Xdefaults,
;; then $XENVIRONMENT or ~/.Xdefaults-<hostname> on top.
(let ((db (xrm:default-resources conn)))          ; conn: CLX display or XCB connection
  (xrm:get-resource db "background" "Background" '("xclock") '("XClock")))
;; => "MidnightBlue", T

(xrm:get-resource-integer db "dpi" "Dpi" '("Xft") '("Xft"))
;; => 96, T, "96"      or NIL, NIL, "abc" (not a number)   or NIL, NIL, NIL (missing)
(xrm:get-resource-boolean db "grip" "Grip" '("xclock") '("XClock"))

(xrm:put-resource db "XClock*hands" "red")         ; name and raw value
(xrm:put-resource-line db "XClock.update: 5")      ; one resource-file line
(xrm:read-resources db #p"~/.Xresources")          ; a file (a string is a pathname, as in CLX)
(xrm:read-resources-from-string db "*font: fixed") ; resource text
(xrm:set-root-resources conn db)                   ; like xrdb -load
```

Xt-style options, with values converted to a Lisp type:

```lisp
(xrm:lookup-option db '("xclock") '("XClock")
                   (list (xrm:make-option :max-width :default 100 :type 'integer))
                   :max-width)
;; resource xclock.maxWidth / XClock.MaxWidth => 100, :RESOURCE
```

The C library's names map as:

xcb_xrm_database_from_default       -> default-resources
xcb_xrm_database_put_resource       -> put-resource
xcb_xrm_database_put_resource_line  -> put-resource-line
xcb_xrm_resource_get_string         -> get-resource
xcb_xrm_resource_get_long           -> get-resource-integer
xcb_xrm_resource_get_bool           -> get-resource-boolean

As in xcb-util-xrm:

- A resource file loses every backslash-newline first; then empty lines are
  dropped, lines starting with `!` or `#` are comments, `#include "file"`
  includes a file (at most 100 deep; a missing file is ignored), and any
  line that does not parse is ignored.

- Names contain only `A-Z a-z 0-9 _ -`; whitespace before the colon is
  skipped, whitespace after it is dropped, and trailing whitespace is part
  of the value.  Value escapes are `\ `, `\<tab>`, `\\`, `\n` and `\ooo`
  (exactly three octal digits); any other backslash is kept.

- `?` matches exactly one component.

- Adding a resource that already exists replaces it, and the new entry
  moves to the end of the database.

- `write-resources` writes `name: value`, escaping a leading space or tab,
  newlines and backslashes.

To run the tests for clxrm: `(asdf:test-system "clxrm")`.

## Differences from CLX

- String resource names work now

  CLX declares them as symbols internally. On SBCL every string query signals a
  `type-error`; symbol queries give the same answers in both.

- `get-resource` also returns `foundp`.

- Parsing, matching, precedence and file syntax are xcb-util-xrm's (see above),
  not CLX's.

- `map-resource` (and so `write-resources` and `merge-resources`) visits every
  resource in database order; CLX dropped `a.b.c` when `a.b` was also set.  In
  name lists a loose binding is the string `"*"` and a wildcard `"?"`.

- `delete-resource` removes exactly the named value and returns T only if it
  existed.

- A query whose names or classes are not plain resource names, or whose class
  list is not as long as its name list, finds nothing, as in xcb-util-xrm.

- Read resource files in correct order `default-resources` (CLX's
  `initialize-resource-database`, which is internal there) also tries
  `~/.Xresources`, before `~/.Xdefaults`.

- `write-resources` supersedes an existing file.

- Setting a property waits for the server (one round trip) It means other
  clients see the new resources as soon as the call returns.

- `RESOURCE_MANAGER` and resource files are decoded as UTF-8 when valid That is
  how xrdb stores them; anything else is decoded as Latin-1 rather than
  signalling an error.  The property is written as UTF-8.

## Differences from xcb-util-xrm

- Names may be symbols.  In name lists and in GET-RESOURCE, a component
  may be a symbol as well as a string; see *UPPERCASE-RESOURCE-SYMBOLS*.

- `?` flushes the pending name.  In the C library's `entry.c`, the `?`  case
  inserts the wildcard component without finalizing the current name buffer, so
  `abc?def` parses as `(? "abcdef")` and writes back as `?.abcdef`.  That looks
  like a bug. The documented meaning of `?` (one component) and the handling of
  `.` and `*`, so clXrm flushes first, as those two do, and `abc?def` is `("abc"
  ? "def")`.  Inputs where the buffer is empty at the `?`, `app.?.width`, `*?b`
  are unaffected.

- The duplicate-entry index.  xcb-util-xrm finds a duplicate by comparing with
  every entry (quadratic time); clXrm keeps a hash index keyed on the components
  (linear time), with the same result.

## License

clXrm is released under the **MIT License**.  `src/database.lisp` and
`src/resources.lisp` are a Common Lisp port of xcb-util-xrm which is licensed
under MIT license, copyright © 2016 Ingo Bürk.  The full texts are in
[LICENSE](LICENSE).
