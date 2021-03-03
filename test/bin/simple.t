Simple tests
  $ echo "Hello World!" | decompress.pipe -d > simple.z
  $ decompress.pipe < simple.z
  Hello World!
  $ echo "Hello World!" | decompress.pipe -d -fzlib > simple.z
  $ decompress.pipe -fzlib < simple.z
  Hello World!
  $ cc -o zpipe zpipe.c -lz
  $ ./zpipe -d < simple.z
  Hello World!
  $ ./zpipe < ../corpus/news > news.z
  $ decompress.pipe -fzlib < news.z > news
  $ diff news ../corpus/news
  $ decompress.pipe -fzlib -d < ../corpus/news > news.z
  $ ./zpipe -d < news.z > news
  $ diff news ../corpus/news
  $ decompress.pipe -fgzip -d < ../corpus/news > news.gz
  $ decompress.pipe -fgzip < news.gz > news
  $ diff news ../corpus/news
