#include <stdio.h>
#include <libguile.h>

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

static void
inner_main(void *data, int argc, char **argv)
{
    printf("Evaluating config file...\n");
    SCM evaluated = scm_c_primitive_load("config.scm");
    SCM config = get_variable("config");

    if (scm_is_null(config)) {
        fprintf(stderr, "error: 'config' is undefined or invalid\n");
        exit(1);
    }

    borderpx = get_value_unsigned_int(config, "border-px", 25);

    printf("Reading rules...\n");
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
}

int
main(int argc, char **argv)
{
    scm_boot_guile(argc, argv, inner_main, NULL);
    return 0;
}
