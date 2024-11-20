```bash
$ dune build --auto-promote
$ dune utop # use #require "libset" to load in toplevel
```

Debugging note: Sometimes the set implementation is wrong! try replacing  usages of 
`TreeSet` with `Set.Make` and see if that can fix the issue
