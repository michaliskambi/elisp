;;;; Some Kambi sql-mode adjustments.

(when (fboundp 'font-lock-add-keywords)
  (font-lock-add-keywords 'sql-mode
    '("\\<varchar\\>"
      "\\<database\\>"
      "\\<password\\>"
      "\\<timestamp\\>"
      "\\<primary\\>"
      "\\<key\\>"
      "\\<generator\\>"
      "\\<term\\>"
      "\\<trigger\\>"
      "\\<before\\>"
      "\\<declare\\>"
      "\\<variable\\>"
      "\\<begin\\>"
      "\\<end\\>"
      "\\<gen_id\\>"
      "\\<if\\>"
      "\\<then\\>"
      "\\<else\\>"
      "\\<after\\>"
      "\\<post_event\\>"
      "\\<connect\\>"
      "\\<references\\>"
      "\\<blob\\>"
      "\\<domain\\>"
      "\\<value\\>"
      "\\<cascade\\>"
      "\\<foreign\\>"
      "\\<constraint\\>"
      "\\<join\\>"
      "\\<exception\\>"
      "\\<procedure\\>"
      "\\<execute\\>"
      "\\<returns\\>"
      "\\<do>"
      "\\<role\\>"
      "\\<date\\>"
      "\\<index\\>"
      "\\<current_user\\>"
      "\\<current_timestamp\\>"
      "\\<collate\\>"
      "\\<bigint\\>"
      "\\<do\\>"
      "\\<suspend\\>"
      "\\<returning_values\\>"
      "\\<inserting\\>"
      "\\<updating\\>"
      "\\<deleting\\>"
      "\\<year\\>"
      "\\<month\\>"
      "\\<cast\\>"
      "\\<descriptor\\>"
      "\\<entry_point\\>"
      "\\<module_name\\>"
      "\\<external\\>"
      "\\<function\\>"
      "\\<coalesce\\>"
      "\\<while\\>"      
  ;;    "\\<\\>"
    )
  )
)

(setq auto-mode-alist (add-to-list-new-items auto-mode-alist
  '(("\\.presql\\'" . sql-mode)
  )))

(add-hook 'sql-mode-hook
  (lambda ()
    (when (same-text (extract-file-ext buffer-file-name) ".presql")
      (set-local-compile-command (concat "presql " (extract-file-name buffer-file-name)))))
  t)

(provide 'kambi-sql)
