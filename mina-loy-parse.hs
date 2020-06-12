#! /usr/bin/env nix-shell
#! nix-shell -i runhaskell -p "haskellPackages.ghcWithPackages (ps: [ps.HandsomeSoup ps.PyF])"

{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

import Text.HandsomeSoup
import Data.Text hiding (zip)
import Text.XML.HXT.Core
import Control.Monad
import PyF

main :: IO ()
main = do
  let doc = fromUrl "index.html"
  pres <- runX $ doc >>> css "pre" /> getText
  forM_ (zip [0..] pres) $ \(i, contents) -> do
    let contents' = unpack $ replace " & " " &amp; " $ pack contents
    writeFile (show i ++ ".xml") (header ++ contents' ++ footer)

header = [fmt|<?xml version="1.0" encoding="utf-8"?>
              <rdf:RDF
               xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"
               xmlns:dc="http://purl.org/dc/elements/1.1/"
               xmlns:dcterms="http://purl.org/dc/terms/"
               xmlns:collex="http://www.collex.org/schema#"
               xmlns:rdfs="http://www.w3.org/2000/01/rdf-schema#"
               xmlns:mlna="http://mina-loy.com/schema"
               xmlns:role="http://www.loc.gov/loc.terms/relators/">
               |]
footer = "</rdf:RDF>"
