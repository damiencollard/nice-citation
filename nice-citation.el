;;; nice-citation.el --- Nicer, colored citation marks.

;;; Commentary:

;; In Gnus' article mode, replace the traditional chevron citation marks (`>`)
;; with a nicer Unicode vertical bar with the same color as the text it is
;; quoting.
;;
;; The color of a mark at a given citation depth is taken from
;; `gnus-cite-face-list'.
;;
;; The text of the buffer is unchanged, as the colored citation marks make use
;; of text property 'display.

;;; Code:

;; Default: Unicode BOX DRAWINGS LIGHT VERTICAL.
(defcustom nice-citation-mark "│"
  "Citation mark.")

(defun nice-citation--depth (marks)
  "Return the citation depth corresponding to string MARKS.
MARKS is a list of `>` and space characters as found as prefix of
a citation line.  Each occurrence of `>` increments the depth by
1, spaces are ignored."
  (let ((depth 0))
    (dolist (m (string-to-list marks) depth)
      (when (= ?> m)
        (setq depth (1+ depth))))))

(defun nice-citation--make (marks)
   "Make nice citation marks to replace the given MARKS.
Applies to each mark in MARKS the Gnus citation face corresponding
to its depth and returns a list of nice, propertized marks."
   (let ((depth 0))
     (mapcar (lambda (c)
               (if (= ?> c)
                   (let ((face (nth depth gnus-cite-face-list)))
                     (prog1
                         (propertize nice-citation-mark 'face face 'evaporate t)
                       (setq depth (1+ depth))))
                 " "))
             marks)))

(defun nice-citation-apply ()
  "Replace citation marks `>` with a nice colored symbol.
The replacement marks are colored the same as the quoted text
and the symbol used can be customized, see `nice-citation-mark'."
  (save-excursion
    (catch 'done
      (while (re-search-forward "^\\(>[> ]*\\)" nil t)
        (let ((beg (match-beginning 1))
               (end (match-end 1)))
          (if (get-text-property beg 'nice-citation)
              (throw 'done nil)
            (let* ((marks (match-string 1))
                   (depth (nice-citation--depth marks))
                   (ovl (apply 'concat (nice-citation--make marks))))
              (put-text-property beg end 'display ovl)
              (put-text-property beg end 'nice-citation t))))))))

;; No hook worked: `gnus-article-prepare-hook', `gnus-article-mode-hook',
;; `gnus-part-display-hook' -- they were all run too early, and at best the
;; nice citation marks overlays were created but were all the same face instead
;; of having the face of the text they're quoting.
;;
;; So we're abusing the `fontification-functions' list a bit and it works fine
;; as `nice-citation-apply' does its best to actually run only once.
(defun nice-citation-fontification (_pos)
  "Fontification function actually performing nice-ification of citation marks.
_POS is unused."
  (read-only-mode -1)
  (unwind-protect
      (nice-citation-apply)
    (read-only-mode 1)))

(add-hook 'gnus-article-mode-hook
          (lambda ()
            (add-to-list 'fontification-functions #'nice-citation-fontification)))

(provide 'nice-citation)
;;; nice-citation.el ends here
