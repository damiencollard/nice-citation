;;; nice-citation.el --- Nicer, colored citation marks.

;; Copyright (c) 2019 Damien Collard

;; Author: Damien Collard <damien.collard@laposte.net>
;; URL:
;; Version: 0.0.2
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

(require 'gnus-art)
(require 'gnus-cite)

(defgroup nice-citation nil
  "Nice depth-colored citation marks for Gnus."
  :prefix "nice-citation-"
  :group 'gnus-article
  :link '(url-link :tag "GitHub" "https://github.com/damiencollard/nice-citation"))

;; Default: Unicode LEFT ONE QUARTER BLOCK.
;; A good alternative is BOX DRAWINGS LIGHT VERTICAL.
(defcustom nice-citation-mark "▎"
  "Citation mark to use in place of the original ones.
Each occurrence of the character `>` in a citation prefix is
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

(defcustom nice-citation-improve-marks-alignment t
  "Make the citation marks align with the text.
This ensures there's always a space between two consecutive
marks, which in effect makes the citation mark at depth N+1
left-align with the text of citation at depth N."
  :type 'boolean
  :group 'nice-citation)

(defun nice-citation--depth (marks)
  "Return the citation depth corresponding to string MARKS.
MARKS is a prefix of cited text, i.e. a string consisting of `>`
and spaces.  Each occurrence of `>` increments the depth by 1,
spaces are ignored."
  (let ((depth 0))
    (dolist (m (string-to-list marks) depth)
      (when (= ?> m)
        (setq depth (1+ depth))))))

(defun nice-citation--make (marks)
   "Make nice citation marks to replace the given MARKS.
Applies to each mark in MARKS the Gnus citation face corresponding
to its depth and returns a list of nice, propertized marks."
   (let ((depth 0)
         (prev-was-mark nil))
     (mapcar (lambda (c)
               (if (= ?> c)
                   (let ((mark (if (and prev-was-mark
                                        nice-citation-improve-marks-alignment)
                                   (concat " " nice-citation-mark)
                                 nice-citation-mark))
                         (face (nth depth gnus-cite-face-list)))
                     (prog1
                         (propertize mark 'face face 'evaporate t)
                       (setq prev-was-mark t)
                       (setq depth (1+ depth))))
                 (setq prev-was-mark nil)
                 " "))
             marks)))

(defun nice-citation-transform (pos limit)
  "Replace citation marks `>` with a nice colored symbol.

The transformation is applied starting at POS on a maximum of
LIMIT characters.

The replacement marks are colored the same as the cited text and
the symbol used can be customized, see `nice-citation-mark'."
  (let ((bound (if limit (+ pos limit) (point-max))))
    (with-silent-modifications
      (save-excursion
        (goto-char pos)
        (while (re-search-forward nice-citation-regex bound t)
          (let ((beg (match-beginning 1))
                (end (match-end 1)))
            (let* ((marks (match-string 1))
                     (depth (nice-citation--depth marks))
                     (ovl (apply 'concat (nice-citation--make marks))))
                (put-text-property beg end 'display ovl)
                (put-text-property beg end 'nice-citation t))))))))

(defun nice-citation-transform-all ()
  "Transform citation marks in the whole buffer."
  (nice-citation-transform (point-min) nil))

(defun nice-citation-fontify (pos)
  "Fontify the citation marks starting at POS.
Meant to be added to `fontification-functions'."
  ;; Limit is set to 500 because that's what message-mode seems to use.
  (nice-citation-transform pos 500))

(defcustom nice-citation-treat-citations t
  "Replace citation marks with nicer highlighted ones.
The new marks use a Unicode character and are highlighted with
the same face as the text they're citing.
Valid values are nil, t, `head', `first', `last', an integer or a
predicate.  See Info node `(gnus)Customizing Articles' for details."
  :group 'nice-citation
  :group 'gnus-article-treat
  :link '(custom-manual "(gnus)Customizing Articles")
  :type gnus-article-treat-custom)
(put 'nice-citation-treat-citation 'highlight t)

;; Prettification of citation marks must be done last, otherwise it doesn't
;; work properly on long, wrapped/filled lines.  Hence we must use `nconc'
;; rather than `add-to-list`.
(nconc gnus-treatment-function-alist
             '((nice-citation-treat-citations nice-citation-transform-all)))

(add-hook 'message-mode-hook
          (lambda ()
            (add-to-list 'fontification-functions #'nice-citation-fontify)))

(provide 'nice-citation)
;;; nice-citation.el ends here
