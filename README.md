# R7RS-small

This repository contains an implementation of [R7RS-small](https://small.r7rs.org/).
We intend to keep this implementation didactic while making it feature-complete and useful as an embedded scripting language for .NET applications.
Currently this is by no means the case, so you may want to look at [IronScheme](https://github.com/IronScheme/IronScheme) for a fully-featured R6RS implementation on .NET.

## Missing features

* Decimal point and scientific notation for numbers
* Complex numbers
* Most standard procedures
* Hygienic macro definition and expansion
* First-class continuations