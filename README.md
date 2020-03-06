tries
====

This is a collection and comparison of some basic, pure trie implementations.

So far, there is:

- a Map trie, using `Data.Map` from [containers](https://hackage.haskell.org/package/containers)
- a List trie, using `Data.Tree` from [containers](https://hackage.haskell.org/package/containers)
- a HashMap trie, using `Data.HashMap` from [unordered-containers](https://hackage.haskell.org/package/unordered-containers)
- a Knuth trie, using `Data.Tree.Knuth` from [rose-trees](https://hackage.haskell.org/package/rose-trees)

## Running the Tests

```bash
stack test
```

and

## Running the Benchmarks

for insert / delete:

```bash
stack bench --benchmark-arguments="--output profile.html"
```

for lookups:

```bash
stack bench --benchmark-arguments="--output profile-lookup.html" tries:bench:tries-bench-lookup
```
