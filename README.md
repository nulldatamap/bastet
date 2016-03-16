# Bastet
Bastet is an experimental programming language that aims to explore new programming language features. It's mainly focused on correctness and as a secondary goal use in game development. 

# Status
Currently the language is still in the design and prototype phase with syntax and semantics still changing.

## Milestones
### v0.1 - In progress
v0.1 will focus on the basics of the language and compiler.

* Syntax:
  - [x] Function definiton (No mutablility specifications nor contracts)
  - [x] Data structure definition (No record types)
  - [x] Types (No list or record types)
  - [x] Function calls and operators
  - [x] Let expression
  - [x] Case expression
  - [x] If expression
  - [x] Return expression
  - [x] Patterns
  - [x] Literals (No list or record literals)
  - [x] Type aliases
* Features:
  - [ ] Function and binding name semantics (No module namespacing yet)
  - [ ] Data structure and alias name semantics (No module namespacing nor field aliasing)
  - [ ] Assigment right-hand-side validation
  - [ ] Type checking
  - [ ] IR generation
  - [ ] C codegen target

### v0.2
v0.2 will focus on adding some of the expected features to flesh out the basics of the language.
* Syntax:
  - [ ] List and record definition, patterns and literals
  - [ ] Traits and member implementation
  - [ ] Mutability annotations (Note: only in the syntax for now)
  - [ ] Module definiton, imports and aliasing
* Features:
  - [ ] Namespace/module semantics (aliasing and importation)
  - [ ] Type checking for lists and records (no structual types yet)
  - [ ] Trait and member function semantics

### Planned features:
- [ ] Predicates
- [ ] Invariants
- [ ] Structual typing
- [ ] Contracts
- [ ] Mutability system (Implemented with contracts)
- [ ] Dependant variables
- [ ] Set types
- [ ] Range types (which are a subset of set types)
- [ ] Specialization of generic functions
- [ ] Open types

# Syntax
So far the syntax looks something like this:
```haskell

data List a = Cons a (Wow::List a) | Nil

trait Functor where
  has fmap f : (fn a -> b), self : Self a -> Self b

instance List a where
  impl map f : fn a -> b, x : Self -> List b =
     case x of
      Cons a rst => Cons (f a) <| map f rst
      Nil        => Nil

instance List a of Functor where
  impl fmap f : (fn a -> b), self : Self -> List b =
    self.map f

data Event = TakeDamage Int | LevelUp

data JohnCena =
  JohnCena { children : (List CenaChild)
           , health   : Int
           , strength : Int
           , inbox    : Inbox Event }

instance JohnCena where
  impl update mut self : Self
    assures is_empty self.inbox = 
    // Update the children
    self.children.map CenaChild::update;
    
    self.health = self.health + 1;
    inbox.avaialable |> drain |evt| => (
        case evt of
          TakeDamage amount => self.health = self.health - amount
          LevelUp           => ( self.health = self.health + 10;
                                 self.strength = self.strength + 1 )
      )


```

# Experimental Features
* __Contracts__: Functions can `assume` and `assure` properties about the program's state and those will be enforces at compile time.
* __Open types__: Or more specifically open enums. Open enums are enums which members can be declared anywhere in the code-base. All required behaviour is implemented at the site of member definition, but they are treated as normal enums. This is useful for easily extensible code, for example for monster types in a game.
