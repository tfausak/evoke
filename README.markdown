# Evoke

:crystal_ball:
Evoke is a GHC plugin to automatically derive type class instances without
using generics or Template Haskell.

- [Motivation](#motivation)
- [Installation](#installation)
- [Usage](#usage)
- [Options](#options)
- [Type Classes](#type-classes)
  - [Arbitrary](#arbitrary)
  - [FromJSON](#fromjson)
  - [ToJSON](#tojson)
  - [ToSchema](#toschema)
- [Benefits](#benefits)
- [Drawbacks](#drawbacks)

## Motivation

In Haskell there are many ways to provide type class instances for data types.
As a motivating example, let's say you have the following data type:

``` hs
data Person = Person
  { name :: String
  , age :: Int }
```

And let's also say that you want to send and receive this data type over the
wire as JSON. Typically in Haskell you would use the Aeson library and
implement the `FromJSON` and `ToJSON` instances. The simplest and easiest way
to do that is to write the instances by hand, like this:

``` hs
instance FromJSON Person where
  parseJSON = withObject "Person" $ \ object -> do
    theirName <- object .: "name"
    theirAge <- object .: "age"
    pure Person { name = theirName, age = theirAge }

instance ToJSON Person where
  toJSON person = object
    [ "name" .= name person
    , "age" .= age person ]
```

This is not difficult, but it's tedious and error prone. It's tedious because
the instance isn't doing anything interesting --- it's all boilerplate. (What's
worse: We haven't even written all the boilerplate! We should have implemented
`toEncoding` for better performance, but we didn't.) And it's error prone
because we have to type all the key names correctly, get them matched up to the
correct fields, and make sure that `FromJSON` and `ToJSON` agree with each
other.

Thankfully GHC has a couple ways to make this easier. The first is Template
Haskell (TH), and thankfully the Aeson library provides some TH functions for
generating instances. Instead of writing the above instances, we could simply
write this:

``` hs
$( deriveJSON defaultOptions ''Person )
```

The generated instance would be similar to what we wrote by hand, but we don't
have to actually write it. And if we update the data type, the instance will be
updated automatically to match.

Unfortunately this approach has one major problem: It forces the module to be
recompiled more often. If any of this module's transitive dependencies changes,
this module will be recompiled. For a small project that's not too bad, but for
a large project that means TH forces you to recompile way more often.

Fortunately there's another approach that supports generating the instances
without causing unnecessary recompilations: generics. It's also simple to use:

``` hs
data Person = Person
  { name :: String
  , age :: Int }
  deriving (Generic, FromJSON, ToJSON)
```

Again the generated instances will behavior similarly to our hand-written
instances, but we don't have to worry about keeping everything in sync. This
feels like the best of all worlds, so what's the problem?

Generics has one major problem: It's slow to compile. Unlike TH, it avoids
recompiling more often, but when it does recompile it's much slower. And just
like TH, this is fine for small projects but for large projects it means you'll
spend a lot of time waiting for generic type class instances to compile.

So what can you do? Ideally we could fix the recompilation checking for TH
and/or fix the compile time performance for generics. Doing either of those
things feels daunting to me, so I looked for alternative approaches. I saw that
it was technically possible for GHC plugins to generate instances, so I created
Evoke to explore that option.

## Installation

Currently Evoke only supports GHC 8.10. It is possible to support a wider range
of versions, but I haven't put in the legwork necessary to make that possible.
Please let me know if you'd like to use Evoke with a different version of GHC.

You can install Evoke by adding it to the `build-depends` section of your
`*.cabal` file. For example:

```
name: example
version: 0
library
  build-depends: evoke, whatever-else
--               ^^^^^
```

## Usage

Since Evoke is a GHC plugin, you must enable it before you can use it. To
enable it on a per-file basis, add the following pragma to the top of a Haskell
file:

``` hs
{-# OPTIONS_GHC -fplugin=Evoke #-}
--              ^^^^^^^^^^^^^^
```

It can be tedious to add that pragma to each file that needs it. Instead you
can enable Evoke for every file in a package by adding it to the `ghc-options`
section of your `*.cabal` file. For example:

```
name: example
version: 0
library
  ghc-options: -fplugin=Evoke -whatever-else
--             ^^^^^^^^^^^^^^
```

The performance impact of enabling Evoke for an entire package is negligible.
In practice I recommend enabling Evoke once in the `*.cabal` file rather than
with a pragma in each source file.

Enabling Evoke will not change the behavior of any modules that previously
compiled. In order to convince Evoke to derive an instance for you, you must
write a deriving clause like this:

``` hs
newtype Person
  = Person { name :: String }
  deriving SomeTypeClass via "Evoke"
--                       ^^^^^^^^^^^
```

Note that `"Evoke"` is a literal string. You do not need to have any language
extensions, such as `DerivingVia` or `DerivingStrategies`, enabled in order to
use Evoke.

## Options

The Evoke plugin itself accepts options, which you can pass using GHC's
`-fplugin-opt` option. For example, this will output the generated instances:

``` hs
{-# OPTIONS_GHC -fplugin=Evoke -fplugin-opt=Evoke:--verbose #-}
--                             ^^^^^^^^^^^^^^^^^^^^^^^^^^^^
```

Each type class can accept its own options, which you can pass by adding them
to the `"Evoke"` string used. For example:

``` hs
newtype Person
  = Person { name :: String }
  deriving SomeTypeClass via "Evoke --some-option"
--                                  ^^^^^^^^^^^^^
```

## Type Classes

These type classes accept the following options:

- `--kebab`: Convert field names into `kebab-case`. For example `firstName`
  would become `first-name`.

- `--camel`: Convert field names into `camelCase`. For example `FirstName`
  would become `firstName`. This is usually combined with `--strip PREFIX`.

- `--snake`: Convert field names into `snake_case`. For example `firstName`
  would become `first_name`.

- `--strip PREFIX`: Removes the given prefix from field names. If the field
  name doesn't begin with the prefix, an error will be thrown. For example,
  with `--strip person` the field name `personFirstName` would become
  `FirstName`.

- `--title`: Convert field names into `TitleCase`. For example `firstName`
  would become `FirstName`.

Options are processed in order. So for example `--strip person --kebab` would
change `personFirstName` into `first-name`.

### Arbitrary

Here's an example of how to derive `Arbitrary` using Evoke:

``` hs
{-# OPTIONS_GHC -fplugin=Evoke #-}
data Person = Person
  { name :: String
  , age :: Int }
  deriving Arbitrary via "Evoke"
```

And here's what the generated instance might look like:

``` hs
instance Test.QuickCheck.Arbitrary Person where
  arbitrary = do
    name_0001 <- Test.QuickCheck.arbitrary
    age_0002 <- Test.QuickCheck.arbitrary
    Control.Applicative.pure Person
      { name = name_0001
      , age = age_0002
      }
```

### FromJSON

Here's an example of how to derive `FromJSON` using Evoke:

``` hs
{-# OPTIONS_GHC -fplugin=Evoke #-}
data Person = Person
  { name :: String
  , age :: Int }
  deriving FromJSON via "Evoke"
```

And here's what the generated instance might look like:

``` hs
instance Data.Aeson.FromJSON Person where
  parseJSON = Data.Aeson.withObject "Main.Person" (\ object_0001 -> do
    name_0002 <- object_0001 Data.Aeson..: Data.Text.pack "name"
    age_0003 <- object_0001 Data.Aeson..: Data.Text.pack "age"
    Control.Applicative.pure Person
      { name = name_0002
      , age = age_0003
      })
```

### ToJSON

Here's an example of how to derive `ToJSON` using Evoke:

``` hs
{-# OPTIONS_GHC -fplugin=Evoke #-}
data Person = Person
  { name :: String
  , age :: Int }
  deriving ToJSON via "Evoke"
```

And here's what the generated instance might look like:

``` hs
instance Data.Aeson.ToJSON Person where
  toJSON var_0001 = Data.Aeson.object
    [ Data.Text.pack "name" Data.Aeson..= name var_0001
    , Data.Text.pack "age" Data.Aeson..= age var_0001
    ]
  toEncoding var_0002 = Data.Aeson.pairs (Data.Monoid.mconcat
    [ Data.Text.pack "name" Data.Aeson..= name var_0002
    , Data.Text.pack "age" Data.Aeson..= age var_0002
    ])
```

### ToSchema

Here's an example of how to derive `ToSchema` using Evoke:

``` hs
{-# OPTIONS_GHC -fplugin=Evoke #-}
data Person = Person
  { name :: String
  , age :: Int }
  deriving ToSchema via "Evoke"
```

And here's what the generated instance might look like:

``` hs
instance Data.Swagger.ToSchema Person where
  declareNamedSchema _proxy_0001 = do
    name_0001 <- Data.Swagger.declareSchemaRef (Data.Proxy.Proxy :: Data.Proxy.Proxy String)
    age_0002 <- Data.Swagger.declareSchemaRef (Data.Proxy.Proxy :: Data.Proxy.Proxy Int)
    Control.Applicative.pure (Data.Swagger.NamedSchema
      (Data.Maybe.Just (Data.Text.pack "Main.Person"))
      (Data.Monoid.mempty
          Control.Lens.& Data.Swagger.type_ Control.Lens.?~ Data.Swagger.SwaggerObject
          Control.Lens.& Data.Swagger.properties Control.Lens..~ Data.HashMap.Strict.InsOrd.fromList
            [ (Data.Text.pack "name", name_0001)
            , (Data.Text.pack "age", age_0002)
            ]
          Control.Lens.& Data.Swagger.required Control.Lens..~
            [ Data.Text.pack "name"
            , Data.Text.pack "age"
            ]))
```

Note that this instance will require the `ScopedTypeVariables` extension if the
type has any type variables that are used in any of the fields.

## Benefits

Compared to TH and generics, why might you prefer Evoke?

- It's fast to compile. Generating the instances is basically instantaneous.
  Compiling the instances takes just as long as if you wrote them manually.

- It doesn't affect recompilation checks. You can generate instances without
  forcing the module to be recompiled more frequently like you would with TH.

- It's easy to debug. The entire plugin operates as a source-to-source
  conversion process. You can output the generated source to remove the plugin
  entirely if you'd like. Or you can run it as a standalone executable.

- It doesn't require any language extensions. Obviously it requires a plugin
  instead.

## Drawbacks

Compared to TH and generics, why might you avoid Evoke?

- It operates syntactically. Things that are equivalent to the type checker,
  like `a` and `(a)`, typically confuse Evoke.

- It's tightly coupled to GHC. Since it operates on GHC's view of a parsed
  module, it naturally depends on that representation.

- It only supports a subset of possible data types. Currently it doesn't
  support types with multiple constructors or GADTs.

- Each type class must be supported explicitly. And this support must be
  provided by the plugin itself rather than by various other libraries like
  Aeson.

- Some things are uncomfortably magical. For instance a field with type
  `Maybe a` will be optional, but if its type is `Prelude.Maybe a` then it
  won't be optional.
