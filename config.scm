(use-modules (gnu services configuration)
             (ice-9 match)
             (ice-9 format)
             (ice-9 pretty-print)
             (ice-9 exceptions)
             (srfi srfi-1))

; Syntax helper to easily create callable actions.
; It accepts any number of expressions as arguments
; and will wrap them inside a lambda to lazily evaluate them.
(define-syntax dwl-procedure
  (syntax-rules ()
    ((dwl-procedure exp ...)
     (lambda () (begin exp ...)))))

; List of available key modifiers
(define %modifiers
  (list SHIFT
        CAPS
        CTRL
        ALT
        MOD2
        MOD3
        SUPER
        MOD5))

(define (string-or-bool? val) (or (string? val) (boolean? val)))
(define (procedure-or-bool? val) (or (procedure? val) (boolean? val)))
(define (list-of-modifiers? lst) (every (lambda (x) (member x %modifiers)) lst))

; Layout configuration
(define-configuration
  dwl-layout
  (id
    (string)
    "id that can be used to reference a layout, e.g. in a monitor rule")
  (symbol
    (string)
    "symbol that should be shown when layout is active")
  (arrange
    (procedure-or-bool #f)
    "procedure to call when selected")
  (no-serialization))

; Basic layouts
(define %layout-default
  (dwl-layout
    (id "default")
    (symbol "[]=")
    (arrange
      (dwl-procedure
        (test-func "arrange 1")))))

(define %layout-monocle
  (dwl-layout
    (id "monocle")
    (symbol "[M]")
    (arrange
      (dwl-procedure
        (test-func "arrange 1")))))

(define %layout-floating
  (dwl-layout
    (id "floating")
    (symbol "><>")))

; Default layouts
(define %base-layouts
  (list %layout-default
        %layout-monocle
        %layout-floating))

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
    "tag to place application on. 1 corresponds to the first tag in the 'tags' list")
  (floating
    (boolean #f)
    "if application should be floating initially")
  (monitor
    (number 1)
    "monitor to spawn application on")
  (no-serialization))

; Monitor rule configuration
(define-configuration
  dwl-monitor-rule
  (name
    (string-or-bool #f)
    "name of monitor, e.g. eDP-1")
  (master-factor
    (number 0.55)
    "horizontal scaling factor for master windows")
  (number-of-masters
    (number 1)
    "number of allowed windows in the master area")
  (scale
    (number 1)
    "monitor scaling")
  (layout
    (string "default")
    "default layout (id) to use for monitor")
  (transform
    (number NORMAL)
    "output transformations, e.g. rotation, reflect")
  (x
    (number 0)
    "position on the x-axis")
  (y
    (number 0)
    "position on the y-axis")
  (no-serialization))

; Keybinding configuration
(define-configuration
  dwl-key
  (modifiers
    (list-of-modifiers (list MODKEY))
    "list of modifiers to use for the keybinding")
  (key
    (string)
    "regular key that triggers the keybinding")
  (action
    (procedure-or-bool #f)
    "procedure to call when triggered")
  (no-serialization))

; dwl configuration type predicates
(define (list-of-tags? lst) (every string? lst))
(define (list-of-keys? lst) (every dwl-key? lst))
(define (list-of-rules? lst) (every dwl-rule? lst))
(define (list-of-layouts? lst) (every dwl-layout? lst))
(define (list-of-monitor-rules? lst) (every dwl-monitor-rule? lst))

; Default monitor rules
(define %base-monitor-rules
  (list (dwl-monitor-rule)))

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
  (tags
    (list-of-tags
      (list "1" "2" "3" "4" "5" "6" "7" "8" "9"))
    "list of tag names")
  (layouts
    (list-of-layouts %base-layouts)
    "list of layouts to use")
  (rules
    (list-of-rules '())
    "list of application rules")
  (monitor-rules
    (list-of-monitor-rules %base-monitor-rules)
    "list of monitor rules")
  (keys
    (list-of-keys '())
    "list of keybindings")
  (no-serialization))

; Custom dwl config
(define dwl-config
  (dwl-configuration
    (border-px 2)
    (tags
      (list "1" "2" "3" "4" "5"))
    (rules
      (list
        (dwl-rule
          (id "firefox")
          (tag 4))
        (dwl-rule
          (id "tidal")
          (tag 5))))
    (monitor-rules
      (append
        (list
          (dwl-monitor-rule
            (name "eDP-1")
            (layout "monocle")
            (transform FLIPPED-90)
            (x 1920)
            (y -10)))
        %base-monitor-rules))
    (keys
      (list
        (dwl-key
          (modifiers
            (list SUPER SHIFT))
          (key "p")
          (action
            (dwl-procedure
              (test-func "action 1"))))
        (dwl-key
          (modifiers
            (list SUPER ALT))
          (key "Return"))))))

; Apply conditional transformations to singular
; values inside the monitor rule configuration.
(define (transform-monitor-rule field value original)
  (match
    field
    ('layout
     (let* ((layouts (dwl-configuration-layouts original))
            (index (list-index (lambda (l) (equal? (dwl-layout-id l) value)) layouts)))
       (match
         index
         (#f
          (raise-exception
            (make-exception-with-message
              (string-append value " is not a valid layout id"))))
         (_ index))))
    (_ value)))

; Apply conditional transformations to singular
; values inside the keybinding configuration.
(define (transform-key-value field value original)
  (match
    field
    ('modifiers (delete-duplicates value))
    ('key
     (if
       (xkb-key? value)
       value
       (raise-exception
         (make-exception-with-message
           (string-append value " is not a valid XKB key")))))
    (_ value)))

; Apply conditional transformations to singular
; values inside the application rule configuration.
(define (transform-rule-value field value original)
  (match
    field
    ('tag
     (let ((tags (length (dwl-configuration-tags original)))
           (tag (- value 1)))
       (if
         (< tag tags)
         tag
         (raise-exception
           (make-exception-with-message
             (string-append
               "specified tag ("
               (number->string value)
               ") is out of bounds, there are only "
               (number->string tags)
               " available tags"))))))
    (_ value)))

; Apply conditional transformations to singular
; values inside the root dwl configuration.
(define (transform-config-value field value original)
  (match
    field
    ('keys
     (map
       (lambda
         (key)
         (transform-config
           #:transform-value transform-key-value
           #:type <dwl-key>
           #:config key
           #:original-config original))
       value))
    ('layouts
     (map
       (lambda
         (layout)
         (transform-config
           #:type <dwl-layout>
           #:config layout
           #:original-config original))
       value))
    ('rules
     (map
       (lambda
         (rule)
         (transform-config
           #:type <dwl-rule>
           #:transform-value transform-rule-value
           #:config rule
           #:original-config original))
       value))
    ('monitor-rules
     (map
       (lambda
         (monitor-rule)
         (transform-config
           #:transform-value transform-monitor-rule
           #:type <dwl-monitor-rule>
           #:config monitor-rule
           #:original-config original))
       value))
    (_ value)))

; Transforms a record into alist to allow the values to easily be
; fetched via C using `scm_assoc_ref(alist, key)`.
(define*
  (transform-config
    #:key
    (transform-value transform-config-value)
    (type #f)
    (config '())
    (original-config '()))
  (remove
    (lambda (pair) (equal? (car pair) "%location"))
    (fold-right
      (lambda (field acc)
        (append
          (let ((accessor ((record-accessor type field) config)))
            `((,(symbol->string field) . ,(transform-value field accessor original-config))))
          acc))
      '()
      (record-type-fields type))))

; Transform the configuration into
; a format that can be easily accessed from C.
(define config
  (transform-config
    #:transform-value transform-config-value
    #:type <dwl-configuration>
    #:config dwl-config
    #:original-config dwl-config))
