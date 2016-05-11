- llvm front:
  - asmparser which equates to an assembler
  - asmwriter which equates to a disassembler
- pasm bugs
  - qbeq offset problem when presented as first instruction
  - relative jumps should not _quietly_ truncate to eight bits

## challenges

- register address forms used in `{l,s}b{b,c}o`, `mvi*`, e.g., `&r17`
- op code for three-operand forms of `mvi*`, (e.g., `mvid r8, r9, b0`) are not
  actually present in pasm's source.

## pruce todos
- [x] batched l/sbbo register file spilling
- [ ] loop/iloop pass
- [x] lbbo/sbbo merger
  - [ ] sbbo merge
- [ ] pseudo-instruction expansion pass
- [ ] handle signed ops
- [ ] - sext
- [ ] - s arith
- [ ] support for adc/suc
- [x] follow ti's calling and return conventions
- [ ] hardware multiplier via xin
- [ ] tail calls
- [ ] lower setcc

` vim: set syntax=markdown textwidth=0 tabstop=2 shiftwidth=2 nolist: `
