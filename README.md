# twelve

[![Build Status](https://travis-ci.org/ocramz/twelve.png)](https://travis-ci.org/ocramz/twelve)

Like [11ty](https://www.11ty.dev/), but this goes to 12.

## What

`twelve` is a small and very simplified static site generator, inspired by 11ty.


## Why

`twelve` was created for these reasons:

* Modularity
    
* Betting on Haskell
    
* Not a JavaScript Framework

## Why, in detail

The main rationale for this project, and its claim to enabling a modular approach in website construction, is the following :

`twelve` flips the templating logic of 11ty on its head : it lets you reference directly a content splice from within a container template, rather than declaring the name of a layout file from within the file containing the content splice.

For example, say we want to splice the content of `card1.html` into the `body` tag of `base.html` :

```html
<!-- base.html -->
<html>
<body>
{{ card1.html }}
</body>
</html>
```

```html
<!-- card1.html -->
<ul>
<li> Hello from card 1 ! </li>
</ul>
```

This means that you can develop `card1` in isolation, and even easily reuse it in multiple places within `base` or other files that mention it.

## Build

Set up your machine with the [`stack` build tool](haskellstack.org).

Then, build and copy the binary to a local `bin` directory with

    $ stack install
    
## Contribute

The aim of this project is to be simple and straightforward to use; PRs welcome ! 
