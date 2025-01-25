Test:
  $ dawn str_param.eos
  Output: str_param.asm
  $ fasm str_param.asm > /dev/null
  $ ./str_param
  Hello, World!Hello, Eos!
