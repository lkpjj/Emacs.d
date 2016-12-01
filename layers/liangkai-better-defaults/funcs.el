;;; funcs.el --- liangkai layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: liangkai <kevin.scnu@gmail.com>
;; URL: https://github.com/lkpjj/spacemacs.d
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;;; Code:

(defun indent-buffer()
  (interactive)
  (indent-region (point-min) (point-max)))

(defun indent-region-or-buffer()
  (interactive)
  (save-excursion
    (if (region-active-p)
        (progn
          (indent-region (region-beginning) (region-end))
          (message "Indent selected region."))
      (progn
        (indent-buffer)
        (message "Indent buffer.")))))

(defun dired-up-directory()
  "goto up directory and resue buffer"
  (interactive)
  (find-alternate-file ".."))

(defun dired-open-term ()
  "Open an `ansi-term' that corresponds to current directory."
  (interactive)
  (let* ((current-dir (dired-current-directory))
         (buffer (if (get-buffer "*zshell*")
                     (switch-to-buffer "*zshell*")
                   (ansi-term "/bin/zsh" "zshell")))
         (proc (get-buffer-process buffer)))
    (term-send-string
     proc
     (if (file-remote-p current-dir)
         (let ((v (tramp-dissect-file-name current-dir t)))
           (format "ssh %s@%s\n"
                   (aref v 1) (aref v 2)))
       (format "cd '%s'\n" current-dir)))))

(defun dired-get-size ()
  (interactive)
  (let ((files (dired-get-marked-files)))
    (with-temp-buffer
      (apply 'call-process "/usr/bin/du" nil t nil "-sch" files)
      (message
       "Size of all marked files: %s"
       (progn
         (re-search-backward "\\(^[ 0-9.,]+[A-Za-z]+\\).*total$")
         (match-string 1))))))

(defun dired-copy-file-here (file)
  (interactive "fCopy file: ")
  (copy-file file default-directory))

(defun run-current-file ()
  "Execute the current file.
For example, if the current buffer is the file x.py, then it'll call 「python x.py」 in a shell.
The file can be emacs lisp, php, perl, python, ruby, javascript, bash, ocaml, Visual Basic.
File suffix is used to determine what program to run.

If the file is modified, ask if you want to save first.

URL `http://ergoemacs.org/emacs/elisp_run_current_file.html'
version 2015-08-21"
  (interactive)
  (let* (
         (ξsuffix-map
          ;; (‹extension› . ‹shell program name›)
          `(
            ("php" . "php")
            ("pl" . "perl")
            ("py" . "python")
            ("py3" . ,(if (string-equal system-type "windows-nt") "c:/Python32/python.exe" "python3"))
            ("rb" . "ruby")
            ("js" . "node") ; node.js
            ("sh" . "bash")
            ;; ("clj" . "java -cp /home/xah/apps/clojure-1.6.0/clojure-1.6.0.jar clojure.main")
            ("ml" . "ocaml")
            ("vbs" . "cscript")
            ("tex" . "pdflatex")
            ("lua" . "lua")
            ;; ("pov" . "/usr/local/bin/povray +R2 +A0.1 +J1.2 +Am2 +Q9 +H480 +W640")
            ))
         (ξfname (buffer-file-name))
         (ξfSuffix (file-name-extension ξfname))
         (ξprog-name (cdr (assoc ξfSuffix ξsuffix-map)))
         (ξcmd-str (concat ξprog-name " \""   ξfname "\"")))

    (when (buffer-modified-p)
      (when (y-or-n-p "Buffer modified. Do you want to save first?")
        (save-buffer)))

    (if (string-equal ξfSuffix "el") ; special case for emacs lisp
        (load ξfname)
      (if ξprog-name
          (progn
            (message "Running…")
            (async-shell-command ξcmd-str "*zilongshanren/run-current-file output*"))
        (message "No recognized program file suffix for this file.")))))
