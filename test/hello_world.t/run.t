Test:
  $ dawn helloworld.eos
  Output: helloworld.asm

  $ fasm helloworld.asm > /dev/null

  $ ./helloworld
  Hello, World!
