.intel_syntax noprefix
.global main

main:
    push 5
    push 2

    pop rbx
    pop rax
    sub rax, rbx
    push rax

    pop rax
    ret

