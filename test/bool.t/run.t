Test:
  $ dawn bool.eos
  Output: bool.asm

  $ fasm bool.asm > /dev/null

  $ ./bool
  Hello, Eos!
