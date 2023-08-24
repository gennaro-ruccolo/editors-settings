;; funzioni custom


;; duplica le linea corrente o la regione selezionata
;; keybinding C-d
(defun gr/duplicate-current-line-or-region (arg)
  "Duplicates the current line or region ARG times.
If there's no region, the current line will be duplicated. However, if
there's a region, all lines that region covers will be duplicated."
  (interactive "p")
  (let (beg end (origin (point)))
    (if (and mark-active (> (point) (mark)))
        (exchange-point-and-mark))
    (setq beg (line-beginning-position))
    (if mark-active
        (exchange-point-and-mark))
    (setq end (line-end-position))
    (let ((region (buffer-substring-no-properties beg end)))
      (dotimes (i arg)
        (goto-char end)
        (newline)
        (insert region)
        (setq end (point)))
      (goto-char (+ origin (* (length region) arg) arg)))))

(global-set-key (kbd "C-d") 'gr/duplicate-current-line-or-region)

;; viaualizza un messaggio al partire di emacs
;; fornendo informazioni sul tempo di caricamento dei pacchetti
(defun gr/display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                     (time-subtract after-init-time before-init-time)))
           gcs-done))

;;(add-hook 'emacs-startup-hook #'gr/display-startup-time)


;;apre il file di configurazione init.el C-c i
(defun gr/open-init ()
  "apre il file di configurazione init.el"
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(global-set-key (kbd "C-c i") 'gr/open-init)
