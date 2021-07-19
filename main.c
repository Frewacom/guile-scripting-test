#include <stdio.h>
#include <libguile.h>

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

static SCM
test_func()
{
  printf("hello from test-func\n");
  return SCM_UNSPECIFIED;
}

static void*
register_functions(void* data)
{
    scm_c_define_gsubr("test-func", 0, 0, 0, &test_func);
    printf("Evaluating config file\n");
    SCM config = scm_c_primitive_load("config.scm");
    int borderpx = get_value_int(config, "border-px");
    char *modkey = get_value_string(config, "modifier-key");
    printf("%s, %d\n", modkey, borderpx);
    return NULL;
}

int
main()
{
    scm_with_guile(&register_functions, NULL);
    return 0;
}
