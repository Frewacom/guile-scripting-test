#include <stdio.h>
#include <libguile.h>

typedef struct {
    const char *id;
    const char *title;
    unsigned int tags;
    int isfloating;
    int monitor;
} Rule;

static SCM rule_type;

static unsigned int borderpx = 1;

static void
init_rule_type()
{
    SCM name = scm_from_utf8_symbol("rule");
    SCM slots = scm_list_5(
        scm_from_utf8_symbol("id"),
        scm_from_utf8_symbol("title"),
        scm_from_utf8_symbol("tags"),
        scm_from_utf8_symbol("isfloating"),
        scm_from_utf8_symbol("monitor")
    );
    scm_t_struct_finalize finalizer = NULL;
    rule_type = scm_make_foreign_object_type(name, slots, finalizer);
}

static SCM
make_rule(SCM id, SCM title, SCM tags, SCM isfloating, SCM monitor)
{
    /* Allocate the `struct image'. Because we
       use scm_gc_malloc, this memory block will
       be automatically reclaimed when it becomes
       inaccessible, and its members will be traced
       by the garbage collector. */
    Rule *rule = scm_gc_malloc(sizeof(Rule), "rule");

    if (scm_is_string(id)) {
        rule->id = scm_to_locale_string(id);
    }

    if (scm_is_string(title)) {
        rule->title = scm_to_locale_string(title);
    }

    if (!rule->id && !rule->title) {
        fprintf(stderr, "error: invalid application rule, missing required field 'id' or 'title'\n");
        exit(1);
    }

    rule->tags = scm_to_unsigned_integer(tags, 0, 9);
    rule->isfloating = scm_to_int(isfloating);
    rule->monitor = scm_to_signed_integer(monitor, -10, 10);

    return scm_make_foreign_object_1(rule_type, rule);
}

static SCM
rule_p(SCM rule)
{
    if (!scm_is_null(rule)) {
        scm_assert_foreign_object_type(rule_type, rule);
    }

    /* an empty list is interpreted as a valid rule to
       allow for simple configuration defaults */
    return scm_from_bool(1);
}

static SCM
get_value(SCM config, const char* key)
{
    return scm_assoc_ref(config, scm_from_locale_string(key));
}

static char*
get_value_string(SCM config, const char* key)
{
    return scm_to_locale_string(get_value(config, key));
}

static int
get_value_int(SCM config, const char* key)
{
    return scm_to_int(get_value(config, key));
}

static unsigned int
get_value_unsigned_int(SCM config, const char* key, int max)
{
    return scm_to_unsigned_integer(get_value(config, key), 0, 25);
}

static SCM
get_variable(const char *name)
{
    return scm_variable_ref(scm_c_lookup(name));
}

static void
inner_main(void *data, int argc, char **argv)
{
    init_rule_type();
    scm_c_define_gsubr("make-rule", 5, 0, 0, &make_rule);
    scm_c_define_gsubr("rule?", 1, 0, 0, &rule_p);
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
            Rule *rule = scm_foreign_object_ref(item, 0);
            printf("------------------\n");
            printf("* id: %s\n", rule->id);
            printf("* title: %s\n", rule->title);
            printf("* tag: %d\n", rule->tags);
            printf("* isfloating: %d\n", rule->isfloating);
            printf("* monitor: %d\n", rule->monitor);
        }
    } else {
        printf("No application rules, skipping...\n");
    }
}

int
main(int argc, char **argv)
{
    scm_boot_guile (argc, argv, inner_main, NULL);
    return 0;
}
