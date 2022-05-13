;;; ox-confluence-modern.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 Nan0Scho1ar
;;
;; Author: Nan0Scho1ar <https://confluence.com/nan0scho1ar>
;; Maintainer: Nan0Scho1ar <scorch267@gmail.com>
;; Created: May 13, 2022
;; Modified: May 13, 2022
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://confluence.com/nan0scho1ar/ox-confluence-modern
;; Package-Requires: ((emacs "27.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;; The existing ox-confluence package does not work with
;; Modern confluence, as atlassian have made significant changes.
;;
;; This library implements a Markdown back-end (confluence flavor) for Org
;; exporter, based on the `md', `html' and `ox-gfm' back-end.
;;
;;; Code:



(provide 'ox-confluence-modern)

(require 'ox-md)
(require 'ox-publish)

;;; User-Configurable Variables

(defgroup org-export-confluence nil
  "Options specific to Markdown export back-end."
  :tag "Org Confluence Flavored Markdown"
  :group 'org-export
  :version "27.1"
  :package-version '(Org . "8.0"))


(defun org-confluence-headline (headline contents info)
  "Transcode HEADLINE element into Confluence Markdown format.
CONTENTS is the headline contents.  INFO is a plist used as
a communication channel."
  (unless (org-element-property :footnote-section-p headline)
    (let* ((level (org-export-get-relative-level headline info))
           (title (org-export-data (org-element-property :title headline) info))
           (todo (and (plist-get info :with-todo-keywords)
                      (let ((todo (org-element-property :todo-keyword
                                                        headline)))
                        (and todo (concat (org-export-data todo info) " ")))))
           (tags (and (plist-get info :with-tags)
                      (let ((tag-list (org-export-get-tags headline info)))
                        (and tag-list
                             (concat "     " (org-make-tag-string tag-list))))))
           (priority
            (and (plist-get info :with-priority)
                 (let ((char (org-element-property :priority headline)))
                   (and char (format "[#%c] " char)))))
           ;; Headline text without tags.
           (heading (concat todo priority title))
           (style (plist-get info :md-headline-style)))
      (cond
       ;; Cannot create a headline.  Fall-back to a list.
       ((or (org-export-low-level-p headline info)
            (not (memq style '(atx setext)))
            (and (eq style 'atx) (> level 6))
            (and (eq style 'setext) (> level 2)))
        (let ((bullet
               (if (not (org-export-numbered-headline-p headline info)) "-"
                 (concat (number-to-string
                          (car (last (org-export-get-headline-number
                                      headline info))))
                         "."))))
          (concat bullet (make-string (- 4 (length bullet)) ?\s) heading tags "\n\n"
                  (and contents (replace-regexp-in-string "^" "    " contents)))))
       (t
        (concat (org-md--headline-title style level heading nil tags)
                contents))))))


(defun org-confluence-plain-text (text info)
  "Transcode a TEXT string from Org to HTML.
TEXT is the string to transcode.  INFO is a plist holding
contextual information."
  text)


;;; Define Back-End
;;;

(org-export-define-derived-backend 'confluence 'md
  :filters-alist '((:filter-parse-tree . org-md-separate-elements))
  :menu-entry
  '(?g "Export to Confluence Flavored Markdown"
       ((?G "To temporary buffer"
            (lambda (a s v b) (let ((org-html-protect-char-alist '())
                               (org-html-special-string-regexps '())
                               (+web-entities-list '()))
                           (org-confluence-export-as-markdown a s v))))
        (?g "To file" (lambda (a s v b)) (org-confluence-export-to-markdown a s v))
        (?o "To file and open"
            (lambda (a s v b)
              (if a (org-confluence-export-to-markdown t s v)
                (org-open-file (org-confluence-export-to-markdown nil s v)))))))
  :translate-alist '((inner-template . org-confluence-inner-template)
                     (paragraph . org-confluence-paragraph)
                     (strike-through . org-confluence-strike-through)
                     (src-block . org-confluence-src-block)
                     (table-cell . org-confluence-table-cell)
                     (table-row . org-confluence-table-row)
                     (table . org-confluence-table)
                     (template . org-confluence-template)
                     (plain-text . org-confluence-plain-text)
                     (headline . org-confluence-headline)))



;;; Transcode Functions

;;;; Paragraph

(defun org-confluence-paragraph (paragraph contents info)
  "Transcode PARAGRAPH element into Confluence Flavoured Markdown format.
CONTENTS is the paragraph contents.  INFO is a plist used as a
communication channel."
  (unless (plist-get info :preserve-breaks)
    (setq contents (concat (mapconcat 'identity (split-string contents) " ") "\n")))
  (let ((first-object (car (org-element-contents paragraph))))
    ;; If paragraph starts with a #, protect it.
    (if (and (stringp first-object) (string-match "\\`#" first-object))
        (replace-regexp-in-string "\\`#" "\\#" contents nil t)
      contents)))


;;;; Src Block

(defun org-confluence-src-block (src-block contents info)
  "Transcode SRC-BLOCK element into Confluence Flavored Markdown format.
CONTENTS is nil. INFO is a plist used as a communication channel."
  (let* ((lang (org-element-property :language src-block))
         (code (org-export-format-code-default src-block info))
         (prefix (concat "```" lang "\n"))
         (suffix "```"))
    (concat prefix code suffix)))


;;;; Strike-Through

(defun org-confluence-strike-through (strike-through contents info)
  "Transcode STRIKE-THROUGH from Org to Markdown (Confluence).
CONTENTS is the text with strike-through markup.  INFO is a plist
holding contextual information."
  (format "~~%s~~" contents))


;;;; Table-Common

(defvar width-cookies nil)
(defvar width-cookies-table nil)

(defconst confluence-table-left-border "|")
(defconst confluence-table-right-border " |")
(defconst confluence-table-separator " |")

(defun org-confluence-table-col-width (table column info)
  "Return width of TABLE at given COLUMN.
INFO is a plist used as communication channel.
Width of a column is determined either by
inquerying `width-cookies' in the column, or by the maximum cell with in
the column."
  (let ((cookie (when (hash-table-p width-cookies)
                  (gethash column width-cookies))))
    (if (and (eq table width-cookies-table)
             (not (eq nil cookie)))
        cookie
      (progn
        (unless (and (eq table width-cookies-table)
                     (hash-table-p width-cookies))
          (setq width-cookies (make-hash-table))
          (setq width-cookies-table table))
        (let ((max-width 0)
              (specialp (org-export-table-has-special-column-p table)))
          (org-element-map
              table
              'table-row
            (lambda (row)
              (setq max-width
                    (max (length
                          (org-export-data
                           (org-element-contents
                            (elt (if specialp (car (org-element-contents row))
                                   (org-element-contents row))
                                 column))
                           info))
                         max-width)))
            info)
          (puthash column max-width width-cookies))))))


(defun org-confluence-make-hline-builder (table info char)
  "Return a function to build horizontal line in TABLE with given CHAR.
INFO is a plist used as a communication channel."
  `(lambda (col)
     (let ((max-width (max 3 (org-confluence-table-col-width table col info))))
       (when (< max-width 1)
         (setq max-width 1))
       (make-string max-width ,char))))


;;;; Table-Cell

(defun org-confluence-table-cell (table-cell contents info)
  "Transcode TABLE-CELL element from Org into Confluence.
CONTENTS is content of the cell.
INFO is a plist used as a communication channel."
  (let* ((table (org-export-get-parent-table table-cell))
         (column (cdr (org-export-table-cell-address table-cell info)))
         (width (org-confluence-table-col-width table column info))
         (left-border (if (org-export-table-cell-starts-colgroup-p table-cell info) "| " " "))
         (right-border " |")
         (data (or contents "")))
    (setq contents
          (concat data
                  (make-string (max 0 (- width (string-width data)))
                               ?\s)))
    (concat left-border contents right-border)))


;;;; Table-Row

(defun org-confluence-table-row (table-row contents info)
  "Transcode TABLE-ROW element from Org into Confluence.
CONTENTS is cell contents of TABLE-ROW.
INFO is a plist used as a communication channel."
  (let ((table (org-export-get-parent-table table-row)))
    (when (and (eq 'rule (org-element-property :type table-row))
               ;; In Confluence, rule is valid only at second row.
               (eq 1 (cl-position
                      table-row
                      (org-element-map table 'table-row 'identity info))))
      (let* ((table (org-export-get-parent-table table-row))
             (header-p (org-export-table-row-starts-header-p table-row info))
             (build-rule (org-confluence-make-hline-builder table info ?-))
             (cols (cdr (org-export-table-dimensions table info))))
        (setq contents
              (concat confluence-table-left-border
                      (mapconcat (lambda (col) (funcall build-rule col))
                                 (number-sequence 0 (- cols 1))
                                 confluence-table-separator)
                      confluence-table-right-border))))
    contents))



;;;; Table

(defun org-confluence-table (table contents info)
  "Transcode TABLE element into Confluence Flavored Markdown table.
CONTENTS is the contents of the table. INFO is a plist holding
contextual information."
  (let* ((rows (org-element-map table 'table-row 'identity info))
         (no-header (or (<= (length rows) 1)))
         (cols (cdr (org-export-table-dimensions table info)))
         (build-dummy-header
          (function
           (lambda ()
             (let ((build-empty-cell (org-confluence-make-hline-builder table info ?\s))
                   (build-rule (org-confluence-make-hline-builder table info ?-))
                   (columns (number-sequence 0 (- cols 1))))
               (concat confluence-table-left-border
                       (mapconcat (lambda (col) (funcall build-empty-cell col))
                                  columns
                                  confluence-table-separator)
                       confluence-table-right-border "\n" confluence-table-left-border
                       (mapconcat (lambda (col) (funcall build-rule col))
                                  columns
                                  confluence-table-separator)
                       confluence-table-right-border "\n"))))))
    (concat (when no-header (funcall build-dummy-header))
            (replace-regexp-in-string "\n\n" "\n" contents))))


;;;; Table of contents

;;;; Footnote section

(defun org-confluence-footnote-section (info)
  "Format the footnote section.
INFO is a plist used as a communication channel."
  (let* ((fn-alist (org-export-collect-footnote-definitions info))
         (fn-alist
          (cl-loop for (n type raw) in fn-alist collect
                   (cons n (org-trim (org-export-data raw info))))))
    (when fn-alist
      (format
       "## %s\n%s"
       "Footnotes"
       (format
        "\n%s\n"
        (mapconcat
         (lambda (fn)
           (let ((n (car fn)) (def (cdr fn)))
             (format
              "%s %s\n"
              (format
               (plist-get info :html-footnote-format)
               (org-html--anchor
                (format "fn.%d" n)
                n
                (format " class=\"footnum\" href=\"#fnr.%d\"" n)
                info))
              def)))
         fn-alist
         "\n"))))))


;;;; Template

(defun org-confluence-template (contents _info)
  "Return complete document string after Markdown conversion.
CONTENTS is the transcoded contents string.  INFO is a plist used
as a communication channel."
  contents)

(defun org-confluence-inner-template (contents info)
  "Return body of document after converting it to Markdown syntax.
CONTENTS is the transcoded contents string.  INFO is a plist
holding export options."
  (let* ((depth (plist-get info :with-toc))
         (headlines (and depth (org-export-collect-headlines info depth)))
         (toc-string (or (mapconcat (lambda (x) (org-confluence-format-toc x info)) headlines "\n") ""))
         (toc-tail (if headlines "\n\n" "")))
    (org-trim (concat toc-string toc-tail contents "\n" (org-confluence-footnote-section info)))))


(defun org-confluence-format-toc (headline info)
  "Return an appropriate table of contents entry for HEADLINE.
INFO is a plist used as a communication channel."
  (let* ((title (org-export-data (org-export-get-alt-title headline info) info))
         (level (1- (org-element-property :level headline)))
         (indent (concat (make-string (* level 2) ? )))
         (url (replace-regexp-in-string " " "-" title)))
    (concat indent "- [" title "]" "(#" url ")")))





;;; Interactive function

;;;###autoload
(defun org-confluence-export-as-markdown (&optional async subtreep visible-only)
  "Export current buffer to a Confluence Flavored Markdown buffer.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting buffer should be accessible
through the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

Export is done in a buffer named \"*Org Confluence Export*\", which will
be displayed when `org-export-show-temporary-export-buffer' is
non-nil."
  (interactive)
  (org-export-to-buffer 'confluence "*Org Confluence Export*"
    async subtreep visible-only nil nil (lambda () (markdown-mode))))


;;;###autoload
(defun org-confluence-convert-region-to-md ()
  "Assume the current region has 'org-mode' syntax,
and convert it to Confluence Flavored Markdown.
This can be used in any buffer.
For example, you can write an itemized list in 'org-mode' syntax in
a Markdown buffer and use this command to convert it."
  (interactive)
  (org-export-replace-region-by 'confluence))


;;;###autoload
(defun org-confluence-export-to-markdown (&optional async subtreep visible-only)
  "Export current buffer to a Confluence Flavored Markdown file.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting file should be accessible through
the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

Return output file's name."
  (interactive)
  (let ((outfile (org-export-output-file-name ".md" subtreep)))
    (org-export-to-file 'confluence outfile async subtreep visible-only)))

;;;###autoload
(defun org-confluence-publish-to-confluence (plist filename pub-dir)
  "Publish an org file to Markdown.
FILENAME is the filename of the Org file to be published.  PLIST
is the property list for the given project.  PUB-DIR is the
publishing directory.
Return output file name."
  (org-publish-org-to 'confluence filename ".md" plist pub-dir))

(provide 'ox-confluence)

;;; ox-confluence.el ends here
