#include QMK_KEYBOARD_H

// {{{ Aliases
#define CAPS QK_CAPS_WORD_TOGGLE
// }}}
// {{{ Configure tri-layers https://docs.qmk.fm/#/feature_tri_layer
#define TRI_LAYER_LOWER_LAYER 1
#define TRI_LAYER_UPPER_LAYER 2
#define TRI_LAYER_ADJUST_LAYER 3
// }}}

// Combos (currently defined in qwerty notation)
// {{{ Normal keycodes
const uint16_t PROGMEM backspace_combo[] = { KC_G, KC_H, COMBO_END };
const uint16_t PROGMEM escape_combo[] = { KC_Q, KC_W, COMBO_END };
const uint16_t PROGMEM return_combo[] = { KC_E, KC_F, COMBO_END };
const uint16_t PROGMEM tab_combo[] = { KC_D, KC_F, COMBO_END };
// }}}
// {{{ Modifiers
const uint16_t PROGMEM lcontrol_combo[] = { KC_S, KC_F, COMBO_END };
const uint16_t PROGMEM rcontrol_combo[] = { KC_J, KC_L, COMBO_END };
const uint16_t PROGMEM lalt_combo[] = { KC_A, KC_S, COMBO_END };
const uint16_t PROGMEM ralt_combo[] = { KC_L, KC_SCLN, COMBO_END };
const uint16_t PROGMEM lsft_combo[] = { KC_S, KC_D, COMBO_END };
const uint16_t PROGMEM rsft_combo[] = { KC_K, KC_L, COMBO_END };
const uint16_t PROGMEM lgui_combo[] = { KC_S, KC_V, COMBO_END };
const uint16_t PROGMEM rgui_combo[] = { KC_N, KC_L, COMBO_END };
const uint16_t PROGMEM lcontrol_sft_combo[] = { KC_S, KC_D, KC_F, COMBO_END };
const uint16_t PROGMEM rcontrol_sft_combo[] = { KC_J, KC_K, KC_L COMBO_END };
const uint16_t PROGMEM lalt_sft_combo[] = { KC_A, KC_S, KC_D, COMBO_END };
const uint16_t PROGMEM ralt_sft_combo[] = { KC_K, KC_L, KC_SCLN, COMBO_END };
const uint16_t PROGMEM lcontrol_alt_sft_combo[] = { KC_A, KC_S, KC_D, KC_F, COMBO_END };
const uint16_t PROGMEM rcontrol_alt_sft_combo[] = { KC_J, KC_K, KC_L, KC_SCLN, COMBO_END };
// }}}
// {{{ Custom keymaps (bound by specific apps)
const uint16_t PROGMEM normal_mode_combo[] = { KC_J, KC_K, COMBO_END };
const uint16_t PROGMEM save_combo[] = { KC_J, KC_I, COMBO_END };
const uint16_t PROGMEM launcher_combo[] = { KC_N, KC_K, KC_L, COMBO_END };
// }}}
// {{{ Workspace keybinds
const uint16_t PROGMEM workspace_1_combo[] = { KC_N, KC_L, KC_Q, COMBO_END };
const uint16_t PROGMEM workspace_2_combo[] = { KC_N, KC_L, KC_W, COMBO_END };
const uint16_t PROGMEM workspace_3_combo[] = { KC_N, KC_L, KC_E, COMBO_END };
const uint16_t PROGMEM workspace_4_combo[] = { KC_N, KC_L, KC_R, COMBO_END };
const uint16_t PROGMEM workspace_5_combo[] = { KC_N, KC_L, KC_T, COMBO_END };
const uint16_t PROGMEM workspace_6_combo[] = { KC_N, KC_L, KC_A, COMBO_END };
const uint16_t PROGMEM workspace_7_combo[] = { KC_N, KC_L, KC_S, COMBO_END };
const uint16_t PROGMEM workspace_8_combo[] = { KC_N, KC_L, KC_D, COMBO_END };
const uint16_t PROGMEM workspace_9_combo[] = { KC_N, KC_L, KC_F, COMBO_END };
const uint16_t PROGMEM workspace_0_combo[] = { KC_N, KC_L, KC_G, COMBO_END };
// }}}
// {{{ Combo actions
combo_t key_combos[] = {
    // {{{ Normal keycodes
    COMBO(backspace_combo, KC_BACKSPACE),
    COMBO(escape_combo, KC_ESCAPE),
    COMBO(return_combo, KC_ENTER),
    COMBO(tab_combo, KC_TAB),
    // }}}
    // {{{ Modifiers
    COMBO(lcontrol_combo, KC_LCTL),
    COMBO(rcontrol_combo, KC_RCTL),
    COMBO(lsft_combo, KC_LSFT),
    COMBO(rsft_combo, KC_RSFT),
    COMBO(rcontrol_combo, KC_RCTL),
    COMBO(lalt_combo, KC_LALT),
    COMBO(ralt_combo, KC_RALT),
    COMBO(lgui_combo, KC_LGUI),
    COMBO(rgui_combo, KC_RGUI),
    COMBO(lcontrol_sft_combo, S(KC_LCTL)),
    COMBO(rcontrol_sft_combo, S(KC_RCTL)),
    COMBO(lalt_sft_combo, S(KC_LALT)),
    COMBO(ralt_sft_combo, S(KC_RALT)),
    COMBO(lcontrol_alt_sft_combo, S(C(KC_LALT))),
    COMBO(rcontrol_alt_sft_combo, S(C(KC_RALT))),
    // }}}
    // {{{ Custom keybinds (bound in software)
    COMBO(normal_mode_combo, KC_F10),
    COMBO(save_combo, KC_F12),
    COMBO(launcher_combo, G(KC_P)),
    // }}}
    // {{{ These combos represents commands for my window manager
    COMBO(workspace_1_combo, G(KC_1)),
    COMBO(workspace_2_combo, G(KC_2)),
    COMBO(workspace_3_combo, G(KC_3)),
    COMBO(workspace_4_combo, G(KC_4)),
    COMBO(workspace_5_combo, G(KC_5)),
    COMBO(workspace_6_combo, G(KC_6)),
    COMBO(workspace_7_combo, G(KC_7)),
    COMBO(workspace_8_combo, G(KC_8)),
    COMBO(workspace_9_combo, G(KC_9)),
    COMBO(workspace_0_combo, G(KC_0))
    // }}}
};
// }}}

// {{{ Layout macro
#define LAYOUT_split_3x5_2_like( \
        L01, L02, L03, L04, L05,   R01, R02, R03, R04, R05, \
        L06, L07, L08, L09, L10,   R06, R07, R08, R09, R10, \
        L11, L12, L13, L14, L15,   R11, R12, R13, R14, R15, \
                       L16, L17,   R16                      \
    ) \
    { \
        { KC_NO, KC_NO, KC_NO, KC_NO, KC_NO, KC_NO, KC_NO, KC_NO, KC_NO, KC_NO, KC_NO, KC_NO, KC_NO, KC_NO, KC_NO, KC_NO }, \
        { KC_NO, L01, L02, L03, L04, L05, R01, R02, R03, R04, R05, KC_NO, KC_NO, KC_NO, KC_NO, KC_NO }, \
        { KC_NO, L06, L07, L08, L09, L10, R06, R07, R08, R09, R10, KC_NO, KC_NO, KC_NO, KC_NO, KC_NO }, \
        { KC_NO, KC_NO, L11, L12, L13, L14, L15, R11, R12, R13, R14, R15, KC_NO, KC_NO, KC_NO, KC_NO }, \
        { KC_NO, KC_NO, L16, KC_NO, KC_NO, KC_NO, L17, KC_NO, KC_NO, KC_NO, R16, KC_NO, KC_NO, KC_NO, KC_NO, KC_NO } \
    }
// }}}
// {{{ Layers
enum layer_names {
    _BASE,
    _RED,
    _BLUE,
    _PURPLE,
};

const uint16_t PROGMEM keymaps[][MATRIX_ROWS][MATRIX_COLS] = {
    /* Template layer
    [N] = LAYOUT_split_3x5_2_like(
        KC_NO, KC_NO, KC_NO, KC_NO, KC_NO, KC_NO, KC_NO, KC_NO, KC_NO, KC_NO,
        KC_NO, KC_NO, KC_NO, KC_NO, KC_NO, KC_NO, KC_NO, KC_NO, KC_NO, KC_NO,
        KC_NO, KC_NO, KC_NO, KC_NO, KC_NO, KC_NO, KC_NO, KC_NO, KC_NO, KC_NO,
                             KC_NO, KC_NO, KC_NO, KC_NO
    ),
    */

    // QWERTY chars
    [_BASE] = {
      { KC_NO, KC_NO, KC_NO, KC_NO, KC_NO, KC_NO, KC_NO, KC_NO, KC_NO, KC_NO, KC_NO, KC_NO, KC_NO, KC_NO, KC_NO, KC_NO },
      { KC_NO, KC_Q, KC_W, KC_E, KC_R, KC_T, KC_Y, KC_U, KC_I, KC_O, KC_P, KC_NO, KC_NO, KC_NO, KC_NO, KC_NO },
      { KC_NO, KC_A, KC_S, KC_D, KC_F, KC_NO, KC_NO, KC_NO, KC_NO, KC_NO, KC_NO, KC_NO, KC_NO, KC_NO, KC_NO, KC_NO },
      { KC_NO, KC_NO, KC_NO, KC_NO, KC_NO, KC_NO, KC_NO, KC_NO, KC_NO, KC_NO, KC_NO, KC_NO, KC_NO, KC_NO, KC_NO, KC_NO },
      { KC_NO, KC_NO, KC_NO, KC_NO, KC_NO, KC_NO, KC_NO, KC_NO, KC_NO, KC_NO, KC_NO, KC_NO, KC_NO, KC_NO, KC_NO, KC_NO }
    },
    [_BASE] = LAYOUT_split_3x5_2_LIKE(
        KC_Q, KC_W, KC_E, KC_R, KC_T, KC_Y, KC_U, KC_I, KC_O, KC_P,
        KC_A, KC_S, KC_D, KC_F, KC_G, KC_H, KC_J, KC_K, KC_L, KC_SCLN,
        KC_Z, KC_X, KC_C, KC_V, KC_B, KC_N, KC_M, KC_COMM, KC_DOT, KC_QUOT,
                           TL_LOWR, KC_SPC, TL_UPPR
    ),
    [_RED] = LAYOUT_split_3x5_2_like(
        KC_1 , KC_2 , KC_3 , KC_4 , KC_5 , KC_HOME,KC_PGDN,KC_PGUP,KC_END,   KC_DEL,
        KC_6 , KC_7 , KC_8 , KC_9 , KC_0 , KC_LEFT,KC_DOWN,KC_UP,  KC_RIGHT, KC_NO,
        KC_NO, KC_NO, KC_NO, KC_NO, KC_NO, KC_NO,  KC_NO,  KC_NO,  KC_NO,    KC_NO,
                           KC_NO, KC_TRNS, TL_UPPR
    ),
    [_BLUE] = LAYOUT_split_3x5_2_like(
        // +       @          #       $          %       _          &          *       `       ~
        S(KC_EQL), S(KC_2),   S(KC_3),S(KC_4),S(KC_5),   S(KC_MINS),S(KC_7),   S(KC_8),KC_GRV, S(KC_GRV),
        // <       {          [       (       ?          |          -          /       =       :
        S(KC_COMM),S(KC_LBRC),KC_LBRC,S(KC_9),S(KC_SLSH),S(KC_BSLS),KC_MINS,   KC_SLSH,KC_EQL, S(KC_SCLN),
        // >       }          ]       )       !          ^          — em dash                  "
        S(KC_DOT), S(KC_RBRC),KC_RBRC,S(KC_0),S(KC_1),   S(KC_6),   UC(0x2014),KC_TRNS,KC_TRNS,S(KC_QUOT),
                                        TL_LOWR, KC_BSLS, KC_NO
    ),
    [_PURPLE] = LAYOUT_split_3x5_2_like(
        KC_F1, KC_F2, KC_F3, KC_F4, KC_F5,  CAPS, KC_COPY, KC_MNXT,KC_VOLU,KC_BRIU,
        KC_F6, KC_F7, KC_F8, KC_F9, KC_F10,KC_NO, KC_PASTE,KC_MPLY,KC_VOLD,KC_BRID,
        KC_F11,KC_F12,KC_NO, KC_NO, KC_NO, KC_NO, KC_CUT,  KC_MPRV,KC_MUTE,CM_TOGG,
                             KC_NO, KC_NO, KC_NO
    )
};
// }}}

void keyboard_post_init_user(void) {
  debug_enable=true;
  debug_matrix=true;
  debug_keyboard=true;
  debug_mouse=true;
}
