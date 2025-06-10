;; x86 AT&T:
;;
;; movl    $1,%eax
;; movl    $0xff,%ebx
;; int     $0x80

;; x86 Intel:
;;
;; mov     eax,1
;; mov     ebx,0ffh
;; int     80h

(defconst e/asm-guess-flavor-region-max 50000
  "Maximum region size to examine while guessing the ASM flavor.")

(defun e/asm-guess-flavor-after-comments-offset ()
  "Offset of first non-blank or non-comment line."
  (save-excursion
    (goto-char (point-min))
    (let ((done nil))
      (while (and (< (point) 49990) (not done))
        (beginning-of-line)
        (if (or
             (looking-at "^[ \t]*$")      ; blank
             (looking-at "^\\s<.*$"))     ; comment only
            (forward-line 1)
          (setq done t))))
    (point)))

(defun e/asm-guess-flavor-has-x86-registersp ()
  "Look for telltale x86 register names."
  "ax|bx|cx|dx"
  "ax|AX|bx|BX|cx|CX|dx|DX"
  "eax|EAX|ebx|EBX|ecx|ECX|edx|EDX"
  "rax|RAX|rbx|RBX|rcx|RCX|rdx|RDX"
  )

(defun e/asm-guess-flavor ()
  "Attempt to guess dialect and syntax aka ASM flavor."
  (let* ((start (e/asm-guess-flavor-after-comments-offset))
         (end (+ start e/asm-guess-flavor-region-max)))
    (save-excursion
      (goto-char start)
      (message "after comments: %d" start))))

(defun e/asm-mode-hook ()
  "Hook to setup ASM flavor and configure indention, etc."
  (e/asm-guess-flavor))

(add-hook 'asm-mode-hook #'e/asm-mode-hook)
