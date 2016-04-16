(defun kam-log-after-editing ()
  "Release Emacs lock on file and mark the file as unmodified.

This is useful after edits that you never intend to save back to file,
like usually filtering operations on log files."
  (unlock-buffer)
  (set-buffer-modified-p nil)
)

(defun kam-log-remove (regexp)
  "Delete from buffer all lines matching REGEXP.

Case-sensitive if REGEXP contains any upper-case letters, just like
`delete-matching-lines'.

Deprecated: just use kam-delete-matching-lines-buffer."
  (kam-delete-matching-lines-buffer regexp))

(defun kam-log-remove-cron ()
  "Delete lines matching \"cron\", case ignored."
  (interactive)
  (kam-log-remove "cron"))

(defun kam-log-remove-michalis-to-wpi-logins ()
  "Remove lines in /var/log/auth.log on informatyka.wroc.pl indicating
michalis.ii connected to download a backup of log files."
  (interactive)
  (kam-simple-re-replace-buffer
    "... .. ..:..:.. informatyka sshd\\[[0-9]+\\]: Accepted publickey for michalis from 156\\.17\\.4\\.16 port [0-9]+ ssh2
... .. ..:..:.. informatyka sshd\\[[0-9]+\\]: pam_unix(sshd:session): session opened for user michalis by (uid=0)
... .. ..:..:.. informatyka sshd\\[[0-9]+\\]: pam_unix(sshd:session): session closed for user michalis
" "")
)

(defun kam-log-default-clean ()
  "Remove lines commonly not interesting for this log file.
This tries to remove repeating non-suspicious lines."
  (interactive)
  (kam-log-remove-cron)

  ;; (when (string= (buffer-file-name) "/var/log/auth.log")

  (kam-log-remove-michalis-to-wpi-logins)
  (message "Removed lines for cron and michalis.ii->wpi connections to dl backups.")

  (kam-log-after-editing)
)

(defun kam-log-leave-suspicious ()
  "Leave only suspicious lines in log file. This deletes all lines,
except the ones matching some of the suspicious texts.

In some way, this is more more aggressive counterpart of
`kam-log-default-clean': `kam-log-default-clean' removes only lines
that are known to be safe, but leaves the \"unsure\" lines.
This function removes all lines, except some relatively known
to indicate suspicious activity."
  (interactive)
  (toggle-read-only 0)
  ;; all lowercase, to not be case-sensitive
  (delete-non-matching-lines "\\(phpmyadmin.*setup\\|zmeu\\|choeko\\|86\\.121\\.126\\.226\\|88\\.191\\.102\\.209\\|86\\.58\\.172\\.226\\|92\\.55\\.71\\.28\\|92\\.55\\.66\\.184\\)"
    (point-min-marker) (point-max-marker))

  (kam-log-after-editing)
)

(define-derived-mode kambi-log-mode text-mode
  "K-Log"
  "Major mode for viewing various log files, typically found in /var/log
on Unix systems.

Provides easy filtering shortcuts.

Also makes log files easier for typical filtering operations:
not read only (you want to edit them).
And at the same time most editing commands release lock (that special
symlink Emacs makes to indicate file is \"locked\")
and set `buffer-modified-p' back to nil --- the reasoning behind both
is that you don't want to save your edits.

Practically, releasing lock helps for michalis.ii cron job that replicates
log files --- otherwise it panicks about not being able to copy these
strange symlinks. And marking buffer as unmodified allows easier
killing of the buffer, or quitting Emacs, without being annoyed
by questions whether you want to save the file."
  ;; TODO: this should better be some part of mode keymap...
  (local-set-key (kbd "C-c C-c") 'kam-log-default-clean)
  (local-set-key (kbd "C-c C-s") 'kam-log-leave-suspicious)

  ;; Since log files are often read-only for viewing user, this conveniently
  ;; makes them writeable.
  (toggle-read-only 0)
)

(defconst kam-log-suffix-regexp
  "\\(\\.[0-9]+\\)?\\(\\.gz\\)?\\'"
  "Suffix of typical log filename: optional number, and optional gz extension,
then end.")

(setq auto-mode-alist (add-to-list-new-items auto-mode-alist
  `( ( ,(concat "\\.log" kam-log-suffix-regexp) . kambi-log-mode)
     ( ,(concat "syslog" kam-log-suffix-regexp) . kambi-log-mode)
  )))

(provide 'kambi-log)
