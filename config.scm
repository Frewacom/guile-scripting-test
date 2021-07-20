(use-modules (gnu services configuration)
             (ice-9 match)
             (srfi srfi-1))

; Since they will be undefined in REPL
; (define MODKEY 0)
; (define MOD-SHIFT 1)

; List of available key modifiers
(define %modifiers (list MOD-SHIFT MODKEY))

(define (anything? val) `(,#t))
(define (string-or-bool? val) (or (string? val) (boolean? val)))
(define (list-of-modifiers? lst) (every (lambda (x) (member x %modifiers)) lst))

; Apply conditional transformations to singular
; values inside the dwl configuration.
(define (transform-value field value)
  (match
    field
    ('rules (map (lambda (rule) (transform-config <dwl-rule> rule)) value))
    ('keys (map (lambda (key) (transform-config <dwl-key> key)) value))
    (_ value)))

; Transforms a record into alist to allow the values to easily be
; fetched via C using `scm_assoc_ref(alist, key)`.
(define transform-config
  (lambda (type config)
    (remove
      (lambda (pair) (equal? (car pair) "%location"))
      (fold-right
        (lambda (field acc)
          (append
            `((,(symbol->string field) . ,(transform-value field ((record-accessor type field) config))))
            acc))
        '()
        (record-type-fields type)))))

; Application rule configuration
(define-configuration
  dwl-rule
  (id
    (string-or-bool #f)
    "id of application")
  (title
    (string-or-bool #f)
    "title of application")
  (tag
    (number 1)
    "tag to place application on")
  (floating
    (boolean #f)
    "if application should be floating initially")
  (monitor
    (number 1)
    "monitor to spawn application on")
  (no-serialization))

; Keybinding configuration
(define-configuration
  dwl-key
  (modifiers
    (list-of-modifiers (list MODKEY))
    "list of modifiers to user for the keybinding")
  (key
    (number)
    "regular key that triggers the keybinding")
  (action
    (anything)
    "function to call when triggered")
  (no-serialization))

(define (list-of-rules? lst) (every dwl-rule? lst))
(define (list-of-keys? lst) (every dwl-key? lst))

; dwl configuration
(define-configuration
  dwl-configuration
  (sloppy-focus
    (number 1)
    "focus follows mouse")
  (border-px
    (number 1)
    "border pixel of windows")
  (repeat-rate
    (number 50)
    "keyboard repeat rate on hold")
  (repeat-delay
    (number 300)
    "keyboard repeat start delay")
  (tap-to-click
    (number 1)
    "trackpad click on tap")
  (natural-scrolling
    (number 0)
    "trackpad natural scrolling")
  (terminal
    (string "st")
    "terminal application to use")
  (menu
    (string "bemenu")
    "menu application to use")
  (rules
    (list-of-rules '())
    "list of application rules")
  (keys
    (list-of-keys '())
    "list of keybindings")
  (no-serialization))

; Create and transform the configuration into
; a format that can be easily accessed from C.
(define config
  (transform-config
    <dwl-configuration>
    (dwl-configuration
      (border-px 2)
      (keys
        (list
          (dwl-key
            (modifiers (list MODKEY MOD-SHIFT))
            (key 1)
            (action (lambda () (test-func "hello johan"))))))
      (rules
        (list
          (dwl-rule
            (id "firefox")
            (tag 4))
          (dwl-rule
            (id "tidal")
            (tag 5)))))))
