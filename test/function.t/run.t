Test:
  $ dawn function.eos
  Output: function.asm
  $ fasm function.asm > /dev/null
  $ ./function
  21
