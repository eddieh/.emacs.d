;;; xcode.el

(require 'subr-x)
(setq e/xcode-developer-dir
      (string-trim (shell-command-to-string "xcode-select -p")))
(setq e/xcode-sdk-headers-dir
      (concat e/xcode-developer-dir
	      "/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/usr/include"))
(setq e/xcode-sdk-frameworks-dir
      (concat e/xcode-developer-dir
	      "/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/System/Library/Frameworks"))
(setq e/xcode-toolchain-dir
      (concat e/xcode-developer-dir
	      "/Toolchains/XcodeDefault.xctoolchain"))
(setq e/xcode-toolchain-clangd
      (concat e/xcode-toolchain-dir
	      "/usr/bin/clangd"))

(defun e/find-xcode-developer-dir ()
  (interactive)
  (find-file e/xcode-developer-dir))

(defun e/find-xcode-sdk-headers-dir ()
  (interactive)
  (find-file e/xcode-sdk-headers-dir))

(defun e/find-xcode-sdk-frameworks-dir ()
  (interactive)
  (find-file e/xcode-sdk-frameworks-dir))

(global-set-key (kbd "C-c x d") 'e/find-xcode-developer-dir)
(global-set-key (kbd "C-c x i") 'e/find-xcode-sdk-headers-dir)
(global-set-key (kbd "C-c x f") 'e/find-xcode-sdk-frameworks-dir)

(defun e/xcode-clangd-path()
  (string-trim (shell-command-to-string "xcrun -f clangd")))
