;;; nice-citation.el --- Nicer, colored citation marks.

;; Copyright (c) 2019 Damien Collard

;; Author: Damien Collard <damien.collard@laposte.net>
;; URL:
;; Version: 0.0.1
;; Keywords: gnus mail convenience
;; Package-Requires: ((emacs "24.3") (gnus "5.13"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

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

(defgroup nice-citation nil
  "Nice depth-colored citation marks for Gnus."
  :prefix "nice-citation-"
  :group 'nice-citation
  :link '(url-link :tag "GitHub" "https://github.com/damiencollard/nice-citation"))

;; Default: Unicode BOX DRAWINGS LIGHT VERTICAL.
(defcustom nice-citation-mark "│"
  "Citation mark to use in place of the original ones.
Each occurrence of the character `>` in a citation suffix is
replaced by this string."
  :type 'string
  :group 'nice-citation)

;; While most citation styles have the first chevron `>` as the first character
;; of a citation line, some infrequent styles have leading spaces.
(defcustom nice-citation-regex "^\\([ ]*>[> ]*\\)"
  "Regex used to find original citation marks to replace.
The part to replace *must* be grouped (parenthesized)."
  :type 'string
  :group 'nice-citation)

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
      (while (re-search-forward nice-citation-regex nil t)
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
