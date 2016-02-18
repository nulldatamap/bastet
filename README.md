# Bastet
Bastet is an experimental programming language that aims to explore new programming language features. It's mainly focused on correctness and as a secondary goal use in game development. 

# Status
Currently the language is still in the design and prototype phase with syntax and semantics still changing.

# Experimental Features
* __Contracts__: Functions and `assume` and `assure` properties about the program's state and those will be enforces at compile time.
* __Open types__: Or more specifically open enums. Open enums are enums which members can be declared anywhere in the code-base. All required behaviour is implemented at the site of member definition, but they are treated as normal enums. This is useful for easily extensible code, for example for monster types in a game.