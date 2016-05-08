  $ cp $DPIPE/dpipe .
  $ cp $BUILDDIR/lib/libdecompress.so .
  $ cp $DPIPE/tests/* .

Test lorem ipsum text

  $ LD_LIBRARY_PATH=. ./dpipe < lorem.txt | LD_LIBRARY_PATH=. ./dpipe -d | diff lorem.txt -

Test binary file

  $ cp -r /bin .
  $ BINS=bin/*
  $ for file in $(ls -trh bin/*); do LD_LIBRARY_PATH=. ./dpipe < $file | LD_LIBRARY_PATH=. ./dpipe -d | diff $file - ; done
