{-# LANGUAGE LambdaCase #-}

module Data.TeLML.Markup where

import Control.Monad (void)
import Data.TeLML
import Text.Blaze.Html
import Text.Blaze.Internal (MarkupM)
import Text.Blaze.Html5 hiding (map, head, html)
import Text.Blaze.Html5.Attributes hiding (name)

renderWith :: [(String, Renderer)] -> Document -> Either String Html
renderWith rs =
  fmap (void . sequence) . mapM (renderPara (basicTags ++ rs)) . gatherPara

render :: Document -> Either String Html
render = renderWith []

gatherPara :: Document -> [Document]
gatherPara = reverse . map reverse . go [[]]
  where go rs [] = rs
        go (r:rs) (t@Tag {}:ts) = go ((t:r):rs) ts
        go (r:rs) (Text s:ts)   = case splitString s of
          []  -> go (r:rs) ts
          [x] -> go ((Text x:r):rs) ts
          xs  -> go (map ((:[]) . Text) (tail xs) ++
                     ((Text (head xs):r) : rs)) ts
        go _ _ = error "[unreachable]"

splitString :: String -> [String]
splitString = filter (/= "") . go
  where go ('\n':'\n':xs) = "\n":go xs
        go (x:xs)         = let r:rs = go xs in ((x:r):rs)
        go []             = [""]

type HtmlE = Either String Html

type Renderer = (Fragment -> HtmlE, [Document]) -> HtmlE

basicTags :: [(String, Renderer)]
basicTags =
  [ ("em"
    , \case (f,[rs]) -> fmap (em . sequence_) (mapM f rs)
            _        -> Left "wrong arity for em/1"
    )
  , ("strong"
    , \case (f,[rs]) -> fmap (strong . sequence_) (mapM f rs)
            _        -> Left "wrong arity for strong/1"
    )
  , ("link"
    , \case (f,[[Text l],r]) -> let go h = a ! href (stringValue l) $ h
                                in fmap (go . sequence_) (mapM f r)
            (_,[_,_])        -> Left "link target should be string"
            _                -> Left "wrong arity for link/1"
    )
  ]

renderPara :: [(String, Renderer)] -> Document -> Either String Html
renderPara taglist ds = fmap (p . sequence_) (mapM go ds)
  where go (Text ts) = Right (toMarkup ts)
        go (Tag tx rs) = exec tx rs taglist
        exec name args ((tag, func):tags)
          | name == tag = case func (go, args) of
            Right html -> Right html
            Left {}    -> exec name args tags
        exec name args (_:tags) = exec name args tags
        exec name args [] = Left $
          "Error: no match for tag " ++ name ++ "/" ++ show (length args)
