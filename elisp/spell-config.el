;; settaggi di configurazione per lo spelling checker

(dolist (hook '(text-mode-hook))
  (add-hook hook(lambda() (flyspell-mode 1))))   ; abilita flyspell per alcune modalità
(dolist (hook '(change-log-mode-hook log-edit-mode-hook))
  (add-hook hook(lambda() (flyspell-mode -1)))

) ; disabilita flyspell per alcune modalità
(with-eval-after-load "ispell"
  ;; Configure `LANG`, otherwise ispell.el cannot find a 'default
  ;; dictionary' even though multiple dictionaries will be configured
  ;; in next line.
  (setenv "LANG" "it_IT.UTF-8")
  (setq ispell-program-name "c:/PortableApps/hunspell/bin/hunspell.exe")
  ;; Configure German, Swiss German, and two variants of English.
  (setq ispell-dictionary "it_IT,en_US")
  ;; ispell-set-spellchecker-params has to be called
  ;; before ispell-hunspell-add-multi-dic will work
  (ispell-set-spellchecker-params)
  (ispell-hunspell-add-multi-dic "it_IT,en_US")
  ;; For saving words to the personal dictionary, don't infer it from
  ;; the locale, otherwise it would save to ~/.hunspell_de_DE.
  (setq ispell-personal-dictionary "C:/PortableApps/hunspell/share/personal")

  ;; The personal dictionary file has to exist, otherwise hunspell will
  ;; silently not use it.
  (unless (file-exists-p ispell-personal-dictionary)
    (write-region "" nil ispell-personal-dictionary nil 0))
)
