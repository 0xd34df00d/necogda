# necogda

Agda support plugin for neovim, implemented in Haskell.

Inspired by [derekelkins/agda-vim](https://github.com/derekelkins/agda-vim) and [tsung-ju/vim-agda-async](https://github.com/tsung-ju/vim-agda-async).
In particular, the input method mappings are taken from the latter.

## Mappings

In the following, `<LL>` stands for `<LocalLeader>`.

Common commands are:

| Binding | Prefixes  | Description                                 |
|---------|-----------|---------------------------------------------|
| `<LL>l` |           | Load file in Agda                           |

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

## How to install

TODO ;)

## Why?

I'm too lazy to properly learn vimL or Lua, and writing neovim plugins in Haskell certainly sounds like fun, right?

Also, `vim-agda-async` doesn't work with neovim off the box, and, again, I'm too lazy to fix that.
