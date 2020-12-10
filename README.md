# twelve

![CI](https://github.com/ocramz/twelve/workflows/Haskell%20CI/badge.svg)

Like [11ty](https://www.11ty.dev/), but this goes to 12.

## What

`twelve` is a small and simple static site generator, inspired by 11ty.




## Why

`twelve` was created to achieve these goals :

* Modularity

* Ease of use

## Why, in detail

The main rationale for this project, and its claim to enabling a modular approach in website construction, is the following :

`twelve` flips the templating logic of 11ty on its head : it lets you reference directly a content splice from within a container template, rather than declaring the name of a layout file from within the file containing the content splice.

For example, say we want to splice the content of `card1.html` into some list items within `base.html` :

```html
<!-- base.html -->
<html>
    <body>
        <ul>
            <li> {{ card1.html }} </li>
            <li> {{ card1.html }} </li>
        </ul>
    </body>
</html>
```

```html
<!-- card1.html -->
<div>Hello from card 1 ! </div>
```

Let's try building `base.html` and look at the result :

```
$ twelve build -f base.html
```

```
$ cat _site/base.html
```

```html
<html>
    <body>
        <ul>
            <li>
                <div>
                    Hello from card 1 !
                </div>
            </li>
            <li>
                <div>
                    Hello from card 1 !
                </div>
            </li>
        </ul>
    </body>
</html>
```

This means that you can develop `card1` in isolation, and even easily reuse it in multiple places within `base` or other files that mention it. 

### Note

* Template files should be valid HTML splices, i.e. cannot be bare text.

* All input files should have an `.html` file extension.

* Template files can reference other ones up to an arbitrary depth, as long as the reference graph is acyclic. If you do have two templates that reference each other, well, you're asking for trouble.

## Why, in full

Modern websites tend to accrete large amounts of CSS annotations around their HTML tags, and a module system can help in writing easily maintainable websites from reasonably-sized components.

In addition, many website build tools and templating languages try to do too much or to impose many design constraints upon the user. `twelve` aims to help without getting in the way.

## Installation

First, you need to have the [`stack` build tool](https://haskellstack.org) already set up on your machine.

Then, build and copy the binary to a local `bin` directory with

    $ stack install
    
    
## Usage

```
$ twelve
twelve, a little static website build tool

Usage: twelve COMMAND
  twelve lets you build an HTML page from a collection of templates. Please
  refer to the README for details. github.com/ocramz/twelve

Available options:
  -h,--help                Show this help text

Available commands:
  init                     Initialize a 'twelve' project
  build                    Build an HTML page
```

`twelve` has two commands: 

* `init` initializes a project in the current directory, if there isn't one already (i.e. it creates a config file and the input and output directories using the command line options `-i` and `-o`). The input directory contains the HTML templates that will be used for populating the final result.

The default directories can be overridden with command line parameters.

* `build` does the whole work : produces a full HTML file by starting from a given input template file and splicing in the content of all references:

```
$ twelve build
Usage: twelve build [[-i|--dir-in DIR] [-o|--dir-out DIR]] -f FILEPATH
  Build an HTML page

Available options:
  -i,--dir-in DIR          input directory for HTML
                           templates (default: "_templates")
  -o,--dir-out DIR         output directory (default: "_site")
  -f FILEPATH              path of input file
```

By default `build` will try to use the `twelve.json` config file. If that's not found, `twelve` will use the command line defaults, which can also be overridden here.

    
## Contribute

The aim of this project is to be simple and straightforward to use; PRs welcome ! 
