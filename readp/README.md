# readp

The functions for readp parsing come from a variety of modules and don’t play nicely together. This package exports everything in a simple module.

The interface was designed by [Edward Kmett](https://github.com/ekmett) for his [parsers](https://github.com/ekmett/parsers) package, and much of the code is copied and pasted from there.
His copyright notice is duly attached.
The only advantage of this package is avoiding dependencies on parsec and attoparsec and whatever else the parsers package supports.
