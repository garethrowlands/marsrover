Mars Rover
==========

* This rover is built using Haskell. Use `stack` to build: https://github.com/commercialhaskell/stack/blob/master/doc/install_and_upgrade.md

* Documentation is in `dist\doc\html\rover\index.html`. It includes colourised sourcecode.

* Source is in `src`

* The build file is `rover.cabal`

Summary of Design
=================

WHICH MODULE DOES WHAT?
=======================

 * `Parser` parses the input string into meaningful data ('Plateau', 'RoverInput', 'Command').
 * `Controller` executes the input data, producing the output data.
 * `Render` renders the output data to a string of the required format.
 * `Environment` represents the 'Plateau' and the rovers on it.
 * `Geometry` represents the geometry of the game world: locations, headings and their operations.
 * `Commands` defines the data types that represent commands.
 * `Main` contains the main program; processes command-line arguments to run unit tests or run interactive.
 * `Test` contains HUnit test cases, as do its child modules.

HANDLING EXCEPTIONAL CONDITIONS
===============================

The following exceptional situations can occur:
 1. _Syntax error in input string._ Report compiler-style syntax error and stop.
 2. _Input plateau size is zero or negative._ Report problem and stop.
 3. _Problem with a rover's initial location: outside the plateau or occupied by another rover._ Report problem and continue with the next rover.
 4. _Rover moves to a bad location: outside the plateau or occupied by another rover._  Report problem and continue with the next rover.

The program represents each of these using the 'Either' type. A return value of:
 * `Right` _some value_ means the function succeeded and the answer is _some value_; and
 * `Left` _some error_ means there was a problem.

The program uses the `Error` monad (`Control.Monad.Error`) to stop when a `Left` is produced.

PARSING THE INPUT
=================

The program uses the _parsec_ library to parse the input string into meaningful data.
Using a parser library makes it easy to parse even complex input and produce good error messages.
The module "Text.ParserCombinators.Parsec.Applicative" allows the parser module to use parsec in an 'Applicative' style
(_parsec_ ought to include that code - it's boiler-plate - but it was made before applicative style became popular).
Applicative style is very compact and very clear... but _only_ if you're used to applicative style.
So I've broken the parser down into one-line functions, and documented each one.
It's probably easiest to think of it as a DSL for parsing.

APPROACH TO TESTING
===================

The program includes unit tests using the HUnit testing framework. The "Test" module and its child modules
define the tests. They're included in the main executable - you wouldn't do that in production code but it's
easy. (The next major release of _cabal_, the Haskell build system, will make it easier to include tests with a package.)

To run unit tests:

    rover.exe --test

APPROACH TO DOCUMENTATION
=========================

The code documentation is at <dist/doc/html/rover/index.html>.
All documentation was extracted from the source code by the Haskell documentation system, _haddock_.
Since Haskell's so different from conventional languages, there no point in sharing the code without
extensive documentation. Without documentation, the entire solution (with test data but no unit tests)
is around 250 lines in one source file!

The bundled documentation uses the latest release of haddock - the one bundled in the current Haskell Platform
produces really ugly HTML. So to generate the documentation you need:

    cabal update
    cabal install haddock
    #make sure haddock is first in path
    cabal configure #pick up the new haddock
    cabal haddock --hyperlink-source

BUILDING
========

The code is packaged and built using `cabal`, the Haskell build system. To build:
 1. Install the Haskell Platform from <http://hackage.haskell.org/platform/>.
 2. Run the commands below.
 3. Run the rover executable in the dist directory.

    cd rover
    cabal configure
    cabal cabal install


CODE ANALYSIS
=============

The code analysis report is at <SourceGraph\rover.html>. It was produced using the /SourceGraph/ package as follows:

    cabal install sourcegraph
    sourcegraph rover.cabal

The graphs are quite interesting. They show, for example, that there's a module for testing the controller code
that's never invoked and doesn't contain much.

OBSERVATIONS ON THE CODE
========================

* All iteration is done through standard library functions that capture particular iteration/recursion patterns.
  None of the functions are recursive and there are no while loops per se.
* All the code is _pure_, except for the `main` driver function. Only `main` does any IO. It's surprising how much code _can_ be pure.
* All the functions are _total_. That is, they have a well-defined return value for every possible input. No preconditions or exceptions.
* The data structures tell the story of the requirements and design (so it's a pity I didn't put them all in the one file). The code has the same case analysis. All the data structures I defined are simple and concrete but I use some standard types such as `Either` and `Maybe`.
* There's very little encapsulation - the data types aren't abstract data types and almost everything is public/exported. But there is no state to protect and no class-invariants to protect - all possible values of each data type are valid. Well, with one exception: not all Plateau values are valid, since negative right and top
are illegal. So the bare Plateau constructor isn't exported and callers have to call mkPlateau instead. Apart from that, the type system enforces everything.

KNOWN PROBLEMS
==============

 1. Unit tests don't test all exceptional conditions.
 2. Some modules are barely tested but others over-tested.
 3. If the input string contains correct input followed by incorrect input,
    it behaves as if the input contained _only_ the correct input.
 4. The test output does not include test coverage statistics;
    production-quality code would run hpc (Haskell Program Coverage).
 5. The code analysis report includes the Test modules, which makes
    it harder to interpret.
