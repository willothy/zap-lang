segment .text
global _start
_start:
    call    main
    mov     rdi, rax
    mov     rax, 60
    syscall
main:
    push    rbp
    mov     rbp, rsp
    sub     rsp, 8
    mov     eax, 15
    mov     ebx, 39
    mov     ecx, 25
    mov     eax, ecx
    mov     dl, 1
    cmp     eax, ebx
    mov     eax, 0
    cmovl   edx, eax
    test    rdx, rdx
    jz      else_0
    mov     eax, eax
tmp_return_block_2:
    add     rsp, 8
    pop     rbp
    ret
    jmp     if_end_1
else_0:
    mov     eax, ebx
    pop     rbp
    ret
    jmp     if_end_1
if_end_1:
tmp_return_block_3:
    add     rsp, 8
segment .data
segment .bss
