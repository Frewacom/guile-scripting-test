#include <stdio.h>
#include <libguile.h>
#include <xkbcommon/xkbcommon.h>
#include <wlr/types/wlr_keyboard.h>
#include <wayland-client-protocol.h>
#include <linux/input-event-codes.h>

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

void
init_dwl(void)
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
}
