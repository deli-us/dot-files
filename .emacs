(require 'cl)
(load-theme 'deeper-blue)

(when (eq system-type 'darwin)
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'super))

(server-start)
(show-paren-mode)
(load-theme 'tango-dark)

;; Turn on irfc.
(pushnew '("\\rfc[0-9]+.txt\\'" . irfc-mode)  auto-mode-alist)
(autoload 'irfc-mode "irfc" nil t)

(defconst home-dir (expand-file-name "~")
  "Users home directory")

;; make calendar display ISO week numbers.
(setq calendar-week-start-day 1
      calendar-intermonth-text
      '(propertize
	(format "%2d"
		(car
		 (calendar-iso-from-absolute
		  (calendar-absolute-from-gregorian (list month day year)))))
	'font-lock-face 'font-lock-function-name-face))

;; Setup tailf specific stuff.
(defun tailf-setup-load-paths (rel-paths)
  (let ((branch (getenv "CONFD_DIR")))
    (if (not (or (eq "" branch) (eq nil branch)))
        (let ((root (file-name-directory branch)))
          (dolist (rel-path rel-paths)
            (let ((path (concat root rel-path)))
              (message "adding %s to load-path" path)
              (pushnew path load-path)))))))

(tailf-setup-load-paths
 '("devel_support/lib/emacs" "system/test/lib/lux/emacs/"))

;; lux
(pushnew '("\\.lux\\'" . lux-mode)  auto-mode-alist)
(autoload 'lux-mode "lux-mode" "" t)

(defun my-lux-mode-hook ()
  "Configuration for lux Mode. Add this to `lux-mode-hook'."
  (if window-system
      (progn
        (local-set-key "\C-c\C-c" 'comment-region)
        (whitespace-mode)
        (setq indent-tabs-mode nil)
        ;; (setq c-basic-offset 2)
        (setq font-lock-maximum-decoration t)
        (font-lock-mode t))))

(add-hook 'lux-mode-hook 'my-lux-mode-hook)

(load-library "tail-f")
;;   Common YANG layout:
(defun my-yang-mode-hook ()
  "Configuration for YANG Mode. Add this to `yang-mode-hook'."
  (if window-system
      (progn
        (whitespace-mode)
        (c-set-style "BSD")
        (setq indent-tabs-mode nil)
        (setq c-basic-offset 2)
        (setq font-lock-maximum-decoration t)
        (font-lock-mode t))))

(add-hook 'yang-mode-hook 'my-yang-mode-hook)

;; Packages
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))

;; Haskell
(autoload 'ghc-init "ghc" nil t)
(add-hook 'haskell-mode-hook (lambda () (ghc-init)))

;; distel
(add-to-list 'load-path "~/git/distel/elisp")
(require 'distel)
(distel-setup)

(setq whitespace-style (quote (face trailing tabs lines)))
(setq whitespace-line-column 80)

(defun my-erlang-mode-hook ()
  
  ; OTP is not whitespace clean.
  (when (and buffer-file-name (not (string-match "/otp/" buffer-file-name)))
    (whitespace-mode))

  (set (make-local-variable 'make-target) " debug")
  (local-set-key "\C-c\C-k" 'cmd-compile)
  (local-set-key "\C-c\C-dc" 'choose-synapse-inferior-node)
  (local-set-key "\C-c\C-d\C-m" 'erlang-man-function)
  (local-set-key "\C-c\C-d\C-d" 'cmd-reload-docs)

  ;; (whitespace-mode)
  ;; (setq whitespace-style '(trailing lines space-before-tab
  ;;                                   indentation space-after-tab)
  ;;       whitespace-line-column 80)
  (auto-fill-mode)
;;  (when (fboundp 'flyspell-prog-mode)
;;    (flyspell-prog-mode))
  (if window-system
      (progn
	(setq font-lock-maximum-decoration t)
	(font-lock-mode 1)))
  (if (and window-system (fboundp 'imenu-add-to-menubar))
      (imenu-add-to-menubar "Imenu")))

(defun my-reload-modules ()
  (erl-rpc (lambda (result) (message "load: %s" result)) nil 
           erl-nodename-cache 'distel 'reload_modules ()))

(defun reload-docs ()
  (erl-rpc (lambda (result) (message "reload docs: %s" result)) nil 
           erl-nodename-cache 'doc_load 'store_all_xml_files ()))

(defun cmd-reload-docs ()
  (interactive)
  (reload-docs))

;; (setq debug-on-error t)
(setq debug-on-error nil)

(setq compilation-finish-functions
      #'(lambda(buffer status)
	  (message "compilation finished with status %s" status)
	  (my-reload-modules)))

(defun compile-synapse ()
  (let* ((compilation-buffer-name-function ;; Dynamically bound...
	  #'(lambda(mode)
	      (concat "*compilation of " (buffer-name (current-buffer)) "*")))
	 (compile-dir (file-name-directory buffer-file-name))
	 (cmd (concat "PATH=/usr/local/bin:$PATH make -C " compile-dir make-target)))
    (compile cmd)))

(defun cmd-compile ()
  (interactive)
  (compile-synapse))

;; (defun get-cvs-tag (dir)
;;   (save-match-data
;;     (let ((index (string-match "cvs/\\(.*\\)/lib" dir)))
;;       (match-string 1 dir))))

;; (defun cvstag-2-nodename (tag)
;;   (let* ((digits
;; 	 (let ((res))
;; 	   (dolist (e (string-to-list tag))
;; 	     (when (and (>= e ?0) (<= e ?9))
;; 	       (push e res)))
;; 	   (nreverse res)))
;; 	 (n (length digits)))
;;     (cond ((= 0 n) "head")
;; 	  ((> n 3)
;; 	   (let ((head (butlast digits (- n 3)))
;; 		 (tail (nthcdr 3 digits)))
;; 	     (concat head "_" tail)))
;; 	  (t (concat digits)))))

(defun choose-synapse-inferior-node ()
  (interactive)
  (set-inferior-node (file-name-directory buffer-file-name)))

(defun set-inferior-node (file)
  (let* ((cvstag (get-cvs-tag file))
	 (node-vsn (cvstag-2-nodename cvstag))
;;	 (node-vsn cvstag)
	 (node-name (if (string-match "head/lib/criterion" file)
			"rsc@criterion"
		      (concat "dcr@criterion"
			      (if (string= node-vsn "head") ""
				(concat "_" node_vsn)))))
	 (tramp (concat "trampoline-to-" node-name)))
    (setq inferior-erlang-machine-options  `("-sname" ,tramp "-remsh" ,node-name)
	  inferior-erlang-buffer-name (concat "*erlang shell (" node-name ")*")
	  erl-nodename-cache (make-symbol node-name))))


(add-hook 'erlang-mode-hook 'my-erlang-mode-hook)

;;(require 'generic-x)

;; A number of the erlang-extended-mode key bindings are useful in the shell too
(defconst distel-shell-keys
  '(("\C-\M-i"   erl-complete)
    ("\M-?"      erl-complete)	
    ("\M-."      erl-find-source-under-point)
    ("\M-,"      erl-find-source-unwind) 
    ("\M-*"      erl-find-source-unwind) 
    )
  "Additional keys to bind when in Erlang shell.")

(add-hook 'erlang-shell-mode-hook
	  (lambda ()
	    ;; add some Distel bindings to the Erlang shell
	    (dolist (spec distel-shell-keys)
	      (define-key erlang-shell-mode-map (car spec) (cadr spec)))))


