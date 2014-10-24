# ApplicationMaker

This *Mathematica* package facilitates the integration of notebooks
to the Documentation Center. This project was created to answer a
question in
[stackoverflow](http://stackoverflow.com/q/6574710/788553).

The are two answers provided by myself in the question, the [first
one](http://stackoverflow.com/a/6574919/788553) provides a lengthly
explanation on how to do it manually.

The [second one](http://stackoverflow.com/a/6660444/788553) provides
`ApplicationMaker`.

## Installation

Drop the extracted zip into

    SystemOpen@FileNameJoin[{$UserBaseDirectory, "Applications"}]`

As discussed [here](http://mathematica.stackexchange.com/questions/28316/creating-cross-version-compatible-documentation-with-workbench/28321#28321) you should really build the documentation in Mathematica 9.0+ but the documentation should technically work in 8.0+.
