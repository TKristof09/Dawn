Test:
  $ dawn loop.eos
  Output: loop.asm
  $ fasm loop.asm > /dev/null
  $ ./loop
  0123456789
