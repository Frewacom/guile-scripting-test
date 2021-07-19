(use-modules (gnu services configuration)
             (srfi srfi-1))

; Transforms a record into alist to allow the values to easily be
; fetched via C using `scm_assoc_ref(alist, key)`.
;
; Note that the key will be the same as the name of the field,
; not the name of the getter.
(define transform-config
  (lambda (type config)
    (fold-right
      (lambda (field acc)
        (append
          `((,(symbol->string field) . ,((record-accessor type field) config)))
          acc))
      '()
      (record-type-fields type))))

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
  (modifier-key
    (string "WLR_MODIFIER_LOGO")
    "global modifier key")
  (terminal
    (string "st")
    "terminal application to use")
  (menu
    (string "bemenu")
    "menu application to use")
  (no-serialization))

; create and transform the configuration into
; a format that can be easily accessed from C.
(transform-config
  <dwl-configuration>
  (dwl-configuration))
