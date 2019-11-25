- Split modules for natural languages from the modules for symbolic language?
  Put symbolic language / TT into a `Formula._` module?
- How to proceed with parsing? A stateful megaparsec stack? Trifecta?
  Something custom along the lines of DynGenPar?
- Parsing should try to stick to local computations when producing nodes of the syntax tree.
  This should help with enabling the program to be run in an incremental manner.
- Is hashing a reasonable approach to saving proofs? We would need to hash all terms
  so that they change if one definition changes. Then proofs only need to be rechecked if
  their hash has changed. We could also link/reference proofs by their hashes. This might
  be useful for a web view of the document.
- Is it worth it to separate `Prop`s into their own datatype? There will probably many data entries
  and functions that will expect only `Prop`s. We can embed expressions into `Prop` via squashing.
- Should parsers typically bring their own data types with them,
  defined in the same module?
- Should we implicitly universally quantify all free variables in formulae? Usually type inference
  should be strong enough to figure out the correct type of those variables, so the user could
  omit the quantification.
