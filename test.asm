segment .text
global zip_main
zip_main:
    call    main
    mov     rdi, rax
    mov     rax, 60
    syscall
main:
    push    rbp
    mov     rbp, rsp
    sub     rsp, 8
    mov     ebx, 15
    mov     dword [rbp - 4], ebx
    mov     ecx, 39
    mov     dword [rbp - 8], ecx
    mov     edx, 25
    mov     dword [rbp - 4], edx
mov edi, dword [rbp - 8]
    mov     sil, 1
    cmp     edi, dword [rbp - 4]
    mov     edi, 0
    cmovl   esi, edi
    test    rsi, rsi
    jz      else_0
    mov     eax, dword [rbp - 4]
    add     rsp, 8
    pop     rbp
    ret
    jmp     if_end_1
else_0:
    mov     eax, dword [rbp - 8]
    add     rsp, 8
    pop     rbp
    ret
    jmp     if_end_1
if_end_1:
segment .data
segment .bss
