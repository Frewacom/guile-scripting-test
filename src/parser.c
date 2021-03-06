#include <stdio.h>
#include <libguile.h>
#include <xkbcommon/xkbcommon.h>
#include <wlr/types/wlr_keyboard.h>
#include <wayland-client-protocol.h>
#include <linux/input-event-codes.h>

typedef struct {
    const char *id;
    const char *title;
    unsigned int tags;
    int isfloating;
    int monitor;
} Rule;

static unsigned int borderpx = 1;

static SCM
get_value(SCM alist, const char* key)
{
    return scm_assoc_ref(alist, scm_from_locale_string(key));
}

static char*
get_value_string(SCM alist, const char* key)
{
    SCM value = get_value(alist, key);

    if (scm_is_string(value)) {
        return scm_to_locale_string(value);
    }

    return NULL;
}

static int
get_value_int(SCM alist, const char* key)
{
    SCM value = get_value(alist, key);

    // Allow parsing of booleans as integers
    if (scm_is_bool(value)) {
        return scm_is_true(value) ? 1 : 0;
    }

    return scm_to_int(value);
}

static unsigned int
get_value_unsigned_int(SCM alist, const char* key, int max)
{
    return scm_to_unsigned_integer(get_value(alist, key), 0, 25);
}

static SCM
get_variable(const char *name)
{
    return scm_variable_ref(scm_c_lookup(name));
}

static Rule*
parse_rule(SCM rule_obj)
{
    char *id = get_value_string(rule_obj, "id");
    char *title = get_value_string(rule_obj, "title");
    unsigned int tags = get_value_unsigned_int(rule_obj, "tag", 9);
    int isfloating = get_value_int(rule_obj, "floating");
    int monitor = get_value_int(rule_obj, "monitor");

    /* Allocate the `struct image'. Because we
       use scm_gc_malloc, this memory block will
       be automatically reclaimed when it becomes
       inaccessible, and its members will be traced
       by the garbage collector. */
    Rule *rule = scm_gc_malloc(sizeof(Rule), "rule");

    if (id == NULL && title == NULL) {
        fprintf(stderr, "error: invalid application rule, missing required field 'id' or 'title'\n");
        exit(1);
    }

    rule->id = id;
    rule->title = title;
    rule->tags = tags;
    rule->isfloating = isfloating;
    rule->monitor = monitor;

    return rule;
}

static SCM
test_func(SCM value)
{
    char *str = scm_to_locale_string(value);
    printf("hello from scheme %s\n", str);
    return SCM_UNSPECIFIED;
}

static SCM
valid_key_p(SCM key)
{
    if (!scm_is_string(key)) {
        return SCM_BOOL_F;
    }

    char *name = scm_to_locale_string(key);
    return scm_from_bool(xkb_keysym_from_name(name, 0) != XKB_KEY_NoSymbol);
}

static void
inner_main(void *data, int argc, char **argv)
{
    // Key modifiers
    scm_c_define("SHIFT", scm_from_int(WLR_MODIFIER_SHIFT));
    scm_c_define("CAPS", scm_from_int(WLR_MODIFIER_CAPS));
    scm_c_define("CTRL", scm_from_int(WLR_MODIFIER_CTRL));
    scm_c_define("ALT", scm_from_int(WLR_MODIFIER_ALT));
    scm_c_define("MOD2", scm_from_int(WLR_MODIFIER_MOD2));
    scm_c_define("MOD3", scm_from_int(WLR_MODIFIER_MOD3));
    scm_c_define("SUPER", scm_from_int(WLR_MODIFIER_LOGO));
    scm_c_define("MOD5", scm_from_int(WLR_MODIFIER_MOD5));

    // Monitor transforms
    scm_c_define("NORMAL", scm_from_int(WL_OUTPUT_TRANSFORM_NORMAL));
    scm_c_define("ROTATE-90", scm_from_int(WL_OUTPUT_TRANSFORM_90));
    scm_c_define("ROTATE-180", scm_from_int(WL_OUTPUT_TRANSFORM_180));
    scm_c_define("ROTATE-270", scm_from_int(WL_OUTPUT_TRANSFORM_270));
    scm_c_define("FLIPPED", scm_from_int(WL_OUTPUT_TRANSFORM_FLIPPED));
    scm_c_define("FLIPPED-90", scm_from_int(WL_OUTPUT_TRANSFORM_FLIPPED_90));
    scm_c_define("FLIPPED-180", scm_from_int(WL_OUTPUT_TRANSFORM_FLIPPED_180));
    scm_c_define("FLIPPED-270", scm_from_int(WL_OUTPUT_TRANSFORM_FLIPPED_270));

    // Mouse buttons
    scm_c_define("MOUSE-LEFT", scm_from_int(BTN_LEFT));
    scm_c_define("MOUSE-MIDDLE", scm_from_int(BTN_MIDDLE));
    scm_c_define("MOUSE-RIGHT", scm_from_int(BTN_RIGHT));

    // Functions
    scm_c_define_gsubr("test-func", 1, 0, 0, &test_func);
    scm_c_define_gsubr("xkb-key?", 1, 0, 0, &valid_key_p);

    printf("Reading config file...\n");
    SCM evaluated = scm_c_primitive_load("config.scm");
    SCM config = get_variable("config");

    if (scm_is_null(config)) {
        fprintf(stderr, "error: 'config' is undefined or invalid\n");
        exit(1);
    } else {
        scm_c_eval_string("(pretty-print config)");
    }

    printf("\nEvaluating config file...\n");
    printf("Reading rules...\n");
    borderpx = get_value_unsigned_int(config, "border-px", 25);
    SCM rules = get_value(config, "rules");

    if (!scm_is_null(rules)) {
        int length = scm_to_int(scm_length(rules));
        printf("Parsing %d rules...\n", length);

        for (int i = 0; i < length; i++) {
            SCM item = scm_list_ref(rules, scm_from_int(i));
            Rule *rule = parse_rule(item);
            printf("------------------\n");
            printf("* id: %s\n", rule->id);
            printf("* title: %s\n", rule->title);
            printf("* tag: %d\n", rule->tags);
            printf("* isfloating: %d\n", rule->isfloating);
            printf("* monitor: %d\n", rule->monitor);
        }
        printf("------------------\n");
    } else {
        printf("No application rules, skipping...\n");
    }

    printf("Reading keybindings...\n");
    SCM keys = get_value(config, "keys");

    if (!scm_is_null(keys)) {
        int length = scm_to_int(scm_length(keys));
        printf("Parsing %d keybindings...\n", length);

        for (int i = 0; i < length; i++) {
            SCM item = scm_list_ref(keys, scm_from_int(i));
            SCM modifiers = get_value(item, "modifiers");
            char *key = get_value_string(item, "key");

            printf("------------------\n");
            printf("* modifiers:");
            int modifiers_length = scm_to_int(scm_length(modifiers));
            for (int j = 0; j < modifiers_length; j++) {
                SCM modifier = scm_list_ref(modifiers, scm_from_int(j));
                printf(" %d", scm_to_int(modifier));
            }
            printf("\n");
            xkb_keysym_t sym = xkb_keysym_from_name(key, 0);

            printf("* key: %s (%d)\n", key, sym);

            SCM action = get_value(item, "action");
            if (scm_procedure_p(action) == SCM_BOOL_T) {
                scm_call(action, SCM_UNDEFINED);
            }
        }
        printf("------------------\n");
    } else {
        printf("No application rules, skipping...\n");
    }
}

int
main(int argc, char **argv)
{
    scm_boot_guile(argc, argv, inner_main, NULL);
    return 0;
}
