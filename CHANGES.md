### v0.9.1 2019-10-08 Palmarin (Sénégal)

* Compatibility layer with `bigarray-compat`

### v0.9.0 2019-07-10 Paris (France)

* Add support of 4.07 and 4.08 in Travis (@XVilka, @dinosaure, #70, #71)
* Use `mmap` (@XVilka, @dinosaure, @hannesm, #68, #69, #71)
* Update documentation (@yurug, @dinosaure, #65, #66)
* Micro-optimization about specialization (@dinosaure, #64)
* Re-organize internals of `decompress` (@dinosaure, #63) 
* GZIP support (@clecat, review by @dinosaure, @cfcs, @hannesm, #60)
 - fix #58 (@dinosaure)

### v0.8.1 2018-10-16 Paris (France)

* _Dunify_ project (@dinosaure)
* *breaking-change* Unbox `Bytes.t` and `Bigstring.t` as I/O buffer (@dinosaure)
* Add foreign tests vectors (@cfcs, @dinosaure)
* Catch invalid distance (@XVilka, @dinosaure)
* Better check on dictionaries (@XVilka, @dinosaure)
* *breaking-change* Add [wbits] argument to check Window size on RFC1951 (@XVilka, @dinosaure)

### v0.8 2018-07-09 Paris (France)

* Implementation of RFC1951 (task from @cfcs)
* *breaking change* New interface of decompress

  We wrap API in `Zlib_{inflate/deflate}` and add `RFC1951_{inflate/deflate}`.
  
* Move to `jbuilder`/`dune` (task from @samoht)
* Better check on `zlib` header
* Fixed infinite loop (task fron @cfcs)

  See 2e3af68, `decompress` has an infinite loop when the inflated dictionary
  does not provide any bindings (and length of opcode is <= 0). In this case,
  `decompress` expects an empty input and provide an empty output in any case.
  
* Use re.1.7.2 on tests
* Use camlzip.1.07 on tests

### v0.7 2017-10-18 Paris (France)

* Fixed Inflate.write function
* Fixed internal state to stick in a internal final state
* Fixed compilation with js_of_ocaml (use trampoline function to avoid
  stack-overflow)
* Fixed clash of name about state variable in the Inflate module
* Add afl program
* Export Adler-32 modules
* Add -i and -o option in the dpipe binary to inform the size of the
  internal chunk
* Change the value of -mode in the dpipe binary

### v0.6 2017-05-11 Cao Lãnh (Vietnam)

- Fixed bug #29
- Produce far pattern (Lz77 compression)
- Optimize memory consumption of the Inflate module
- Move repository from oklm-wsh to mirage
- Learn topkg release

### v0.5 2017-02-17 Essaouira (Maroc)

- Stabilize the interface (@dbuenzli's interface)
- Merge optimization from @yallop
- Add `sync_flush`, `partial_flush`, `full_flush` (experimental)
- Move the build system to `topkg`
