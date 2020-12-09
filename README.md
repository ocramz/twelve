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

The main rationale for this project, and its claim to offering modularity to website assembly, can be explained as follows:

`twelve` flips the templating logic of 11ty on its head : it lets you reference directly a content splice from within a larger template, rather than declaring the name of a layout file from within the file containing the content splice.

For example:

Say you have `base.html` and `card1.html` :

```
<!-- base.html -->
<html>
<body>
{{ card1.html }}
</body>
</html>
```

```
<!-- card1.html -->
<ul>
<li> Hello from card 1 ! </li>
</ul>
```
