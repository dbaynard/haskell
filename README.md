---
title:  General purpose haskell packages  
author: David Baynard  
date:   13 Nov 2016  
fontfamily:   libertine
csl:    chemical-engineering-science.csl
link-citations: true
abstract: |  

---

After some time writing haskell code, there was a little too much copy-paste-amend for my liking.
With this set of packages I can keep all that information up to date in a single place.

# forestay

> **forestay** /ˈfɔːsteɪ/
>
> “A rope to support a ship’s foremast, running from its top to the deck at the bow.”
>
> ― OED

This is a custom prelude (hooray!) but at the moment it basically re-exports [lifted-protolude](https://github.com/parsonsmatt/lifted-protolude) and [lens](https://github.com/ekmett/lens).

    {-# LANGUAGE OverloadedStrings #-}

    import Forestay

# readp

The `base` library contains a number of parsing modules, providing `ReadP` parsers.
The relevant functions are spread over multiple modules, though, and the interface is a little unclear.
There is a package called [parsers](https://github.com/ekmett/parsers) which provides a unified interface for many different parser libraries on hackage.
This package is simply a copy of that implementation (missing a few functions) but only for `ReadP` parsers, thereby not including the large number of dependencies of `parsers`.

    import Text.ReadP

    _parser_ `parse` _input_
