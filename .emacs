(require 'cl)
(load-theme 'deeper-blue)

(when (eq system-type 'darwin)
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'super))


(server-start)
(show-paren-mode)

;; Turn on irfc.
(pushnew '("\\rfc[0-9]+.txt\\'" . irfc-mode)  auto-mode-alist)
(autoload 'irfc-mode "irfc" nil t)

(defconst home-dir (expand-file-name "~")
  "Users home directory")

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

(pushnew '("\\.lux\\'" . lux-mode)  auto-mode-alist)
(autoload 'lqux-mode "lux-mode" "" t)

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
	 (cmd (concat "TEST_DIR=/Users/mattias/work2/trunk/system/test PATH=/usr/local/bin:$PATH make -C " compile-dir make-target)))
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


;;(require 'bbdb-autoloads)
;; (require 'bbdb)
;; (load "bbdb-com" t)
;; (bbdb-initialize 'gnus 'message 'reportmail 'w3)
;; ;;(bbdb-insinuate-reportmail)
;; (bbdb-insinuate-message)
;; ;; (bbdb-insinuate-sc)
;; ;;(bbdb-insinuate-w3)
;; (setq bbdb-north-american-phone-numbers nil)
;; (add-hook 'gnus-startup-hook 'bbdb-insinuate-gnus)
;; (setq bbdb-auto-notes-alist
;;       (quote (("To"
;;                ("w3o" . "w3o")
;;                ("plug" . "plug")
;;                ("linux" . "linux")
;;                ("emacs-commit" . "emacs commit")
;;                ("emacs" . "emacs")
;;                ("pinoyjug" . "pinoyjug")
;;                ("digitalfilipino" . "digitalfilipino")
;;                ("sacha" . "personal mail"))
;;               ("From"
;;                ("admu" company "Ateneo de Manila University")
;;                ("Organization" (".*" company 0 nil))
;;                ))))
;; (setq bbdb-auto-notes-ignore (quote (("Organization" . "^Gatewayed from\\\\|^Source only"))))
;; (setq bbdb-auto-notes-ignore-all nil)
;; (setq bbdb-check-zip-codes-p nil)
;; (setq bbdb-default-area-code 632)
;; ;;(setq bbdb-default-country "Philippines")
;; (setq bbdb-ignore-some-messages-alist (quote (("From" . "hotmail") ("To" . "handhelds") ("From" . "yahoo.com"))))
;; (setq bbdb-notice-hook (quote (bbdb-auto-notes-hook)))
;; (setq bbdb/mail-auto-create-p t)
;; (setq bbdb/news-auto-create-p (quote bbdb-ignore-some-messages-hook))
;; BBDB: Address list
;; (when (file-exists-p "/usr/share/emacs/site-lisp/bbdb")
;;   (add-to-list 'load-path "/usr/share/emacs/site-lisp/bbdb")
  ;; (require 'bbdb)
  ;; (bbdb-initialize 'message 'gnus 'sendmail)
  ;; (setq bbdb-file "~/bbdb.db")
  ;; (add-hook 'gnus-startup-hook 'bbdb-insinuate-gnus)
  ;; ;; (setq bbdb/mail-auto-create-p t
  ;; ;;       bbdb/news-auto-create-p t)
  ;; (defvar bbdb-time-internal-format "%Y-%m-%d"
  ;;   "The internal date format.")
  ;; ;;;###autoload
  ;; (defun bbdb-timestamp-hook (record)
  ;;   "For use as a `bbdb-change-hook'; maintains a notes-field called `timestamp'
  ;;   for the given record which contains the time when it was last modified.  If
  ;;   there is such a field there already, it is changed, otherwise it is added."
  ;;   (bbdb-record-putprop record 'timestamp (format-time-string
  ;;                                           bbdb-time-internal-format
  ;;                                           (current-time))))



