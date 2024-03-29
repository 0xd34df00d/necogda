# necogda

Agda support plugin for neovim, implemented in Haskell.

Inspired by [derekelkins/agda-vim](https://github.com/derekelkins/agda-vim) and [tsung-ju/vim-agda-async](https://github.com/tsung-ju/vim-agda-async).
In particular, the input method mappings are taken from the latter.

## Mappings

In the following, `<LL>` stands for `<LocalLeader>`.

Common commands are:

| Binding  | Prefixes  | Description                                       |
|----------|-----------|---------------------------------------------------|
| `<LL>l`  |           | Load file in Agda                                 |
| `<LL>gn` |           | Go to the next goal/warning/error                 |
| `<LL>gp` |           | Go to the previous goal/warning/error             |
| `<LL>?`  |           | Show the chords for the symbol under the cursor   |

Goal commands include:

| Binding | Prefixes  | Description                                                                 |
|---------|-----------|-----------------------------------------------------------------------------|
| `<LL>c` |           | Case split                                                                  |
| `<LL>r` |           | Refine goal with a given term                                               |
| `<LL>a` |           | Auto — try to find a solution                                               |
| `<LL>t` | norm      | Show goal type                                                              |
| `<LL>,` | norm      | Show goal type, context                                                     |
| `<LL>.` | norm      | Show goal type, context and inferred type                                   |
| `<LL>;` | norm      | Show goal type, context, and check if the expression in the hole can fit in |

_norm_ prefixes affect normalisation and are as follows:
| Key  | Strategy     |
|------|--------------|
| none | Simplified   |
| `a`  | As is        |
| `i`  | Instantiated |
| `h`  | Head normal  |
| `s`  | Simplified   |
| `n`  | Normalised   |

For example, to show normalised goal type, do `<LL>nt`, or do `<LL>n.` to see everything normalised in the goal context too.

You might have noticed the key is the lowercased first letter of the corresponding strategy.
This is not a coincidence.

## Configuration

* `g:symbol_start_marker`: any character\
  **Default**: `` ` ``\
  The marker for the beginning of an Unicode symbol chord.
  For instance, with the default value, the sequence ```ga`` will be replaced with `α`.
* `g:insert_behaviour`: `"undo"` | unset\
  **Default**: unset\
  How exactly a symbol chord should be replaced with the corresponding symbol. TODO describe more.
* `g:show_inaccessible_bindings`: `1` | `0`\
  **Default**: true\
  If `1`, the goals window shows inaccessible bindings (in `{curly} : braces`), and hides them otherwise.
  Bindings are inaccessible if they come from an implicit (and unbounded) argument or a wildcard pattern,
  or if they have been shadowed.
* `g:necogda_goal_window_orientation`: `"vertical"` | `"horizontal"`\
  **Default**: `"vertical"`\
  The orientation of the goal information window.
* `g:necogda_goal_window_size`: integer\
  **Default**: 60\
  The size of the goal information window.
  Roughly speaking, this is the argument to `resize` that Necogda does for you when it opens the goal preview window.

All these options are read every time they are used, so changing them does not require restarting neovim.
Thus, you might find it useful to make shortcuts for changing some of them, like `g:show_inaccessible_bindings`.

## How to install

TODO ;)

## Why?

I'm too lazy to properly learn vimL or Lua, and writing neovim plugins in Haskell certainly sounds like fun, right?

Also, `vim-agda-async` doesn't work with neovim off the box, and, again, I'm too lazy to fix that.
