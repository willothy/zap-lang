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
    sub     rsp, 0
    mov     rdi, const_str_0
    mov     esi, 14
    call    write
    add     rsp, 0
    pop     rbp
    ret
write:
    push    rbp
    mov     rbp, rsp
    sub     rsp, 0
    mov     rdx, rsi
    mov     rsi, rdi
    mov     rdi, 1
    mov     rax, 1
    syscall
    add     rsp, 0
    pop     rbp
    ret
segment .data
const_str_0:
    db      "Hello", 44, 32, "world", 33, 10 
segment .bss
