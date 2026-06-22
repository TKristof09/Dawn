Test:
  $ dawn recursive_struct_return.eos
  $ fasm recursive_struct_return.asm > /dev/null
  $ ./recursive_struct_return
  120
