v0.7 2017-10-18 Paris (France)
------------------------------------

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

v0.6 2017-05-11 Cao Lãnh (Vietnam)
------------------------------------

- Fixed bug #29
- Produce far pattern (Lz77 compression)
- Optimize memory consumption of the Inflate module
- Move repository from oklm-wsh to mirage
- Learn topkg release

v0.5 2017-02-17 Essaouira (Maroc)
------------------------------------

- Stabilize the interface (@dbuenzli's interface)
- Merge optimization from @yallop
- Add `sync_flush`, `partial_flush`, `full_flush` (experimental)
- Move the build system to `topkg`
