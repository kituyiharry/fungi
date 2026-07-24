# Changelog

All notable changes to this project are documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [0.2.0] - 2026-07-24

### Changed (breaking)

- **Edge weights are now persistent.** Each vertex's edge weights moved from a
  mutable `Hashtbl` to a persistent map, so deriving a graph no longer mutates
  any earlier version. `Graph.Weights` is now `Map.S` instead of `Hashtbl.S`,
  and `Vertex.update` / `Vertex.ensure` now return the updated `weights` instead
  of `unit`.
- **`TreeSet` re-implemented on top of `Stdlib.Set`.** The hand-rolled
  (unbalanced) BST was replaced by an adapter over `Set.Make`, giving
  balanced-tree performance. The `TSet` interface is unchanged so all call sites
  keep working; compare-equal inserts still replace the existing element (as the
  old `TreeSet` did), which several algorithms rely on.

### Added

- `Serialize.to_dot_string` and `Serialize.to_dot_channel` for rendering a graph
  to a single dot string or straight to an `out_channel`, alongside the existing
  lazy-sequence `to_dot`.
- A test suite covering `TreeSet`, `Heap`, `UnionFind`, core `Graph`, the graph
  algorithms (SCC, clustering, flow, path, matching, spanning trees) and dot
  serialization.

### Fixed

- **`add_weight` no longer accumulates duplicate bindings** — re-adding an edge
  now replaces its weight instead of stacking entries.
- **`MakeDisjointSet.create`** raises a clear `Invalid_argument` when the element
  sequence is longer than the declared size, instead of a later out-of-bounds
  crash. The unused `ResolvTbl` module was removed.
- **`TreeSet` no longer shadows `Stdlib.Not_found`**, so partial operations raise
  the standard exception callers can catch.
- **`Heap` consolidation table is now per-call**, removing a functor-global
  mutable table that could be clobbered by concurrent consolidations.
- **`toposort`** now returns a valid topological (happens-before) order; it
  previously emitted discovery order, which could place a successor before its
  predecessor.
- **`astar`** no longer raises `"value not in heap"`; path entries are keyed
  consistently and the heap is pre-seeded, matching dijkstra's behaviour.
- **`galeshapely`** now produces a stable matching; the acceptor preference
  comparison was inverted, yielding blocking pairs. A leftover debug print was
  also removed.

### Serialization (dot)

- Node ids, labels and attribute values are now quoted and escaped, so arbitrary
  element/weight strings (spaces, quotes, dashes, keywords) produce valid dot.
- Reciprocal-edge de-duplication is applied only to undirected graphs; directed
  graphs keep genuine mutual edges (`A -> B` and `B -> A`).
- Attribute quoting is consistent across global, node and edge attributes, and
  attribute keys are emitted in sorted order for reproducible output.
- The lazy `to_dot` sequence is now re-forceable (no leaked mutable state), and
  empty attribute brackets / redundant node declarations are avoided.

## [0.1.4]

- Baseline release prior to this changelog.

[0.2.0]: https://github.com/kituyiharry/libset/releases/tag/0.2.0
[0.1.4]: https://github.com/kituyiharry/libset/releases/tag/0.1.4
