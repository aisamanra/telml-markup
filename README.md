# telml-markup

The `telml-markup` package provides a simple, LaTeX-inspired markup
language with the possibility of writing extensible instructions.

**This is in early phases, and so should not be relied upon for any
kind of stability at this stage.** In particular, the set of tags
and how they are understood could change radically during development.

## Basic Usage

The `render` function takes a [`telml`](https://github.com/aisamanra/telml)
document and renders it into the
[`blaze-html`](http://hackage.haskell.org/package/blaze-html-0.8.0.2)
[`Html`](http://hackage.haskell.org/package/blaze-html-0.8.0.2/docs/Text-Blaze-Html.html#t:Html)
type, which can then be rendered into HTML. For example, the following
is a minimal program which interprets input on `stdin` and prints the
rendered HTML (or an error message) to `stdout`.

~~~~{.haskell}
module Main

import Control.Monad ((>=>))
import Data.TeLML (parse)
import Data.TeLML.Markup (render)
import Text.Blaze.Renderer.String (renderMarkup)

main :: IO ()
main = do
  str <- getContents
  case (parse >=> render) str of
    Left err  -> putStrLn err
    Right doc -> putStrLn (renderMarkup doc)
~~~~

We could invoke it at the command line like so:

~~~~
$ ./telml-markup-test <<EOF
> This should be \em{emphasized}.
>
> This, on the other hand, is \strong{bold}.
> EOF
<p>This should be<em>emphasized</em>.
</p><p>This, on the other hand, is <strong>bold</strong>.
</p>
~~~~

If we give it an unknown tag, or a tag with the wrong arity, it will
give us an error:

~~~~
$ ./telml-markup-test <<EOF
> This is a \fake{tag}.
> EOF
Error: no match for tag fake/1
$ ./telml-markup-test <<EOF
> This is a tag with \em{too|many|arguments}.
> EOF
Error: no match for tag em/3
~~~~

## Extended Usage

The `renderWith` function takes a list of additional tags and their
denotations (in the form of functions from `telml` fragments to
`blaze-html` fragments.) This allows you to add new tags to the
markup for particular purposes.

For example, here we add a tag so that `\hello{...}` will render out to
the HTML string `<strong>Hello, ...!</strong>`:

~~~~{.haskell}
module Main where

import Control.Monad ((>=>))
import Data.TeLML (parse)
import Data.TeLML.Markup (Renderer, renderWith)
import Text.Blaze.Renderer.String (renderMarkup)
import Text.Blaze.Html5 (strong, toMarkup)

myTags :: [(String, Renderer)]
myTags =
  [ ("hello", \ c -> case c of
      (render, [name]) -> do
        rName <- mapM render name
        return $ strong $ do
          toMarkup "Hello, "
          sequence_ rName
          toMarkup "!"
      (_, args) -> Left ("Did not match hello/" ++ show (length args))
    )
  ]

main :: IO ()
main = do
  str <- getContents
  case (parse >=> renderWith myTags) str of
    Left err  -> putStrLn err
    Right doc -> putStrLn (renderMarkup doc)
~~~~

We can execute this to test it:

~~~~
$ ./telml-markup-extended-test <<EOF
> Now we can do this: \hello{friend}.
> EOF
<p>Now we can do this: <strong>Hello, friend!</strong>.
</p>
~~~~
