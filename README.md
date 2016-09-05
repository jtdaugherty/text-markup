text-markup
===========

This library provides a data structure for associating arbitrary
metadata ("markup") with subsequences of text. The main interface to the
library is through three functions:

 * `toMarkup` - convert a `Text` into a `Markup a`,
 * `markRegion` - mark a region of the `Text` with a metadata value of
   type `a`, and
 * `fromMarkup` - recover the subsequences of the text with accompanying
   metadata.

For example,

```
let m = toMarkup (T.pack "some@email.com 192.168.1.1 http://google.com/") Nothing
fromMarkup $ markRegion 27 18 (Just "url")
           $ markRegion 15 11 (Just "ipv4")
           $ markRegion 0 14 (Just "e-mail") m
```

yields

```
[ ("some@email.com"    , Just "e-mail")
, (" "                 , Nothing)
, ("192.168.1.1"       , Just "ipv4")
, (" "                 , Nothing)
, ("http://google.com/", Just "url")
]
```
