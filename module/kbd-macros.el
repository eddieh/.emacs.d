(defalias 'fixup-xcode-file-template-comment-c
  (kmacro (concat "<escape> < / * <return> ( <backspace> * <tab> C-k C-n "
		  "C-a C-d C-d ( <backspace> * C-n C-a * C-d C-d C-n C-a "
		  "* C-d C-d <tab> C-n C-a C-k * C-n C-a C-d * <tab> C-x "
		  "C-s C-e C-p C-p C-a C-k C-k C-p C-p C-p C-k C-k C-x C-s "
		  "C-n C-n C-k C-k C-x C-s")))
