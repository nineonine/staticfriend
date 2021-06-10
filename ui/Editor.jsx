import React from 'react';
import ControlPanel from './ControlPanel.jsx';
import SourceCode from './SourceCode.jsx';

function Editor() {

  let hs = `module Main where

main :: IO ()
main = putStrLn "Hello, World!"
`
  let x86 = `==================== Asm code ====================
2021-06-10 06:07:57.869552 UTC

.data
.align 3
.align 0
_uKr_srt:
  .quad	stg_SRT_1_info
  .quad	GHC.CString.unpackCString#_closure
  .quad	0


==================== Asm code ====================
2021-06-10 06:07:57.869745 UTC

.section	__TEXT,__cstring,cstring_literals
.align 1
.align 0
cKn_str:
  .string "Hello, World!"


==================== Asm code ====================
2021-06-10 06:07:57.870271 UTC

.text
.align 3
_dsp_sat_sKg_info_dsp:
.align 3
  .quad	0
  .long	21
  .long	_uKr_srt-(sat_sKg_info)+0
sat_sKg_info:
_cKo:
  leaq -16(%rbp),%rax
  cmpq %r15,%rax
  jb _cKp
_cKq:
  subq $8,%rsp
  movq %r13,%rax
  movq %rbx,%rsi
  movq %rax,%rdi
  xorl %eax,%eax
  call newCAF
  addq $8,%rsp
  testq %rax,%rax
  je _cKm
_cKl:
  leaq stg_bh_upd_frame_info(%rip),%rbx
  movq %rbx,-16(%rbp)
  movq %rax,-8(%rbp)
  leaq cKn_str(%rip),%r14
  leaq GHC.CString.unpackCString#_closure(%rip),%rbx
  addq $-16,%rbp
  jmp stg_ap_n_fast
_cKm:
  jmp *(%rbx)
_cKp:
  jmp *-16(%r13)
  .long  sat_sKg_info - _dsp_sat_sKg_info_dsp


==================== Asm code ====================
2021-06-10 06:07:57.870499 UTC

.data
.align 3
.align 0
sat_sKg_closure:
  .quad	sat_sKg_info
  .quad	0
  .quad	0
  .quad	0


==================== Asm code ====================
2021-06-10 06:07:57.871552 UTC

.data
.align 3
.align 0
_uKJ_srt:
  .quad	stg_SRT_2_info
  .quad	System.IO.putStrLn_closure
  .quad	sat_sKg_closure
  .quad	0


==================== Asm code ====================
2021-06-10 06:07:57.871906 UTC

.text
.align 3
_dsp_Main.main_info_dsp:
.align 3
  .quad	0
  .long	21
  .long	_uKJ_srt-(Main.main_info)+0
.globl Main.main_info
Main.main_info:
_cKG:
  leaq -16(%rbp),%rax
  cmpq %r15,%rax
  jb _cKH
_cKI:
  subq $8,%rsp
  movq %r13,%rax
  movq %rbx,%rsi
  movq %rax,%rdi
  xorl %eax,%eax
  call newCAF
  addq $8,%rsp
  testq %rax,%rax
  je _cKF
_cKE:
  leaq stg_bh_upd_frame_info(%rip),%rbx
  movq %rbx,-16(%rbp)
  movq %rax,-8(%rbp)
  leaq sat_sKg_closure(%rip),%r14
  leaq System.IO.putStrLn_closure(%rip),%rbx
  addq $-16,%rbp
  jmp stg_ap_p_fast
_cKF:
  jmp *(%rbx)
_cKH:
  jmp *-16(%r13)
  .long  Main.main_info - _dsp_Main.main_info_dsp


==================== Asm code ====================
2021-06-10 06:07:57.872137 UTC

.data
.align 3
.align 0
.globl Main.main_closure
Main.main_closure:
  .quad	Main.main_info
  .quad	0
  .quad	0
  .quad	0


==================== Asm code ====================
2021-06-10 06:07:57.872835 UTC

.data
.align 3
.align 0
_uL0_srt:
  .quad	stg_SRT_2_info
  .quad	GHC.TopHandler.runMainIO_closure
  .quad	Main.main_closure
  .quad	0


==================== Asm code ====================
2021-06-10 06:07:57.873169 UTC

.text
.align 3
_dsp_:Main.main_info_dsp:
.align 3
  .quad	0
  .long	21
  .long	_uL0_srt-(:Main.main_info)+0
.globl :Main.main_info
:Main.main_info:
_cKX:
  leaq -16(%rbp),%rax
  cmpq %r15,%rax
  jb _cKY
_cKZ:
  subq $8,%rsp
  movq %r13,%rax
  movq %rbx,%rsi
  movq %rax,%rdi
  xorl %eax,%eax
  call newCAF
  addq $8,%rsp
  testq %rax,%rax
  je _cKW
_cKV:
  leaq stg_bh_upd_frame_info(%rip),%rbx
  movq %rbx,-16(%rbp)
  movq %rax,-8(%rbp)
  leaq Main.main_closure(%rip),%r14
  leaq GHC.TopHandler.runMainIO_closure(%rip),%rbx
  addq $-16,%rbp
  jmp stg_ap_p_fast
_cKW:
  jmp *(%rbx)
_cKY:
  jmp *-16(%r13)
  .long  :Main.main_info - _dsp_:Main.main_info_dsp


==================== Asm code ====================
2021-06-10 06:07:57.873392 UTC

.data
.align 3
.align 0
.globl :Main.main_closure
:Main.main_closure:
  .quad	:Main.main_info
  .quad	0
  .quad	0
  .quad	0


==================== Asm code ====================
2021-06-10 06:07:57.873667 UTC

.section	__TEXT,__cstring,cstring_literals
.align 1
.align 0
$trModule1_rDZ_bytes:
  .string "main"


==================== Asm code ====================
2021-06-10 06:07:57.87412 UTC

.data
.align 3
.align 0
$trModule2_rE5_closure:
  .quad	GHC.Types.TrNameS_con_info
  .quad	$trModule1_rDZ_bytes


==================== Asm code ====================
2021-06-10 06:07:57.874389 UTC

.section	__TEXT,__cstring,cstring_literals
.align 1
.align 0
$trModule3_rE6_bytes:
  .string "Main"


==================== Asm code ====================
2021-06-10 06:07:57.874663 UTC

.data
.align 3
.align 0
$trModule4_rE7_closure:
  .quad	GHC.Types.TrNameS_con_info
  .quad	$trModule3_rE6_bytes


==================== Asm code ====================
2021-06-10 06:07:57.875028 UTC

.data
.align 3
.align 0
.globl Main.$trModule_closure
Main.$trModule_closure:
  .quad	GHC.Types.Module_con_info
  .quad	$trModule2_rE5_closure+1
  .quad	$trModule4_rE7_closure+1
  .quad	3
`

  return (
    <div>
      <div id='editor-area-container'>

        <ControlPanel/>

        <div id='editor-area'>
          <SourceCode
            source={hs}
            id={'source-in'}
            language={'haskell'}
          />

          <div id='middle-panel'>
          </div>

          <SourceCode
            source={x86}
            id={'source-out'}
            language={'x86asm'}
          />
        </div>

      </div>
    </div>
  );
}

export default Editor;
