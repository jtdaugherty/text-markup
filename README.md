text-markup
===========

This library provides a data structure for associating arbitrary
metadata ("markup") with subsequences of text. The motivation for
this library is to provide a tool for tracking the assignment of text
attributes to text sequences in terminal applications where we may
want to perform many such assignments by searching text with regular
expressions or using parsers to do syntax highlighting.

The main interface to the library is through three functions:

 * `toMarkup` - convert a `Text` into a `Markup a`,
 * `markRegion` - mark a region of the `Text` with a metadata value of
   type `a`, and
 * `fromMarkup` - recover the subsequences of the text with accompanying
   metadata.

For example,

```
> let m = toMarkup (T.pack "some@email.com 192.168.1.1 http://google.com/") Nothing
> fromMarkup $ markRegion 27 18 (Just "url")
             $ markRegion 15 11 (Just "ipv4")
             $ markRegion 0 14 (Just "e-mail") m
[ ("some@email.com"    , Just "e-mail")
, (" "                 , Nothing)
, ("192.168.1.1"       , Just "ipv4")
, (" "                 , Nothing)
, ("http://google.com/", Just "url")
]
```

Applying the same markup to adjacent regions results in a merge:

```
> let m = toMarkup (T.pack "foobar") Nothing
> fromMarkup $ markRegion 0 3 (Just "token") m
[("foo",Just "token"),("bar",Nothing)]
> fromMarkup $ markRegion 3 3 (Just "token")
             $ markRegion 0 3 (Just "token") m
[("foobar",Just "token")]
```
