#include QMK_KEYBOARD_H

// {{{ Aliases
#define CAPS QK_CAPS_WORD_TOGGLE
// }}}
// {{{ Configure tri-layers https://docs.qmk.fm/#/feature_tri_layer
#define TRI_LAYER_LOWER_LAYER 1
#define TRI_LAYER_UPPER_LAYER 2
#define TRI_LAYER_ADJUST_LAYER 3
// }}}
// {{{ Combos (currently defined in colemak-dh notation)
// {{{ Normal keycodes
const uint16_t PROGMEM backspace_combo[] = { KC_G, KC_M, COMBO_END };
const uint16_t PROGMEM escape_combo[] = { KC_Q, KC_W, COMBO_END };
const uint16_t PROGMEM return_combo[] = { KC_F, KC_T, COMBO_END };
const uint16_t PROGMEM tab_combo[] = { KC_S, KC_T, COMBO_END };
// }}}
// {{{ Modifiers
const uint16_t PROGMEM lcontrol_combo[] = { KC_R, KC_T, COMBO_END };
const uint16_t PROGMEM rcontrol_combo[] = { KC_N, KC_I, COMBO_END };
const uint16_t PROGMEM lalt_combo[] = { KC_A, KC_R, COMBO_END };
const uint16_t PROGMEM ralt_combo[] = { KC_I, KC_O, COMBO_END };
const uint16_t PROGMEM lsft_combo[] = { KC_R, KC_S, COMBO_END };
const uint16_t PROGMEM rsft_combo[] = { KC_I, KC_E, COMBO_END };
const uint16_t PROGMEM lgui_combo[] = { KC_R, KC_V, COMBO_END };
const uint16_t PROGMEM rgui_combo[] = { KC_H, KC_I, COMBO_END };
const uint16_t PROGMEM lcontrol_sft_combo[] = { KC_R, KC_S, KC_T, COMBO_END };
const uint16_t PROGMEM rcontrol_sft_combo[] = { KC_N, KC_E, KC_I, COMBO_END };
// }}}
// {{{ Custom keymaps (bound by specific apps)
const uint16_t PROGMEM normal_mode_combo[] = { KC_N, KC_E, COMBO_END };
const uint16_t PROGMEM save_combo[] = { KC_N, KC_U, COMBO_END };
const uint16_t PROGMEM launcher_combo[] = { KC_H, KC_I, KC_E, COMBO_END };
// }}}
// {{{ Workspace keybinds
const uint16_t PROGMEM workspace_1_combo[] = { KC_H, KC_I, KC_Q, COMBO_END };
const uint16_t PROGMEM workspace_2_combo[] = { KC_H, KC_I, KC_W, COMBO_END };
const uint16_t PROGMEM workspace_3_combo[] = { KC_H, KC_I, KC_F, COMBO_END };
const uint16_t PROGMEM workspace_4_combo[] = { KC_H, KC_I, KC_P, COMBO_END };
const uint16_t PROGMEM workspace_5_combo[] = { KC_H, KC_I, KC_B, COMBO_END };
const uint16_t PROGMEM workspace_6_combo[] = { KC_H, KC_I, KC_A, COMBO_END };
const uint16_t PROGMEM workspace_7_combo[] = { KC_H, KC_I, KC_R, COMBO_END };
const uint16_t PROGMEM workspace_8_combo[] = { KC_H, KC_I, KC_S, COMBO_END };
const uint16_t PROGMEM workspace_9_combo[] = { KC_H, KC_I, KC_T, COMBO_END };
const uint16_t PROGMEM workspace_0_combo[] = { KC_H, KC_I, KC_G, COMBO_END };
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
// }}}
// {{{ Layers
const uint16_t PROGMEM keymaps[][MATRIX_ROWS][MATRIX_COLS] = {
    /* Template layer
    [N] = LAYOUT_split_3x5_2(
        KC_NO, KC_NO, KC_NO, KC_NO, KC_NO, KC_NO, KC_NO, KC_NO, KC_NO, KC_NO,
        KC_NO, KC_NO, KC_NO, KC_NO, KC_NO, KC_NO, KC_NO, KC_NO, KC_NO, KC_NO,
        KC_NO, KC_NO, KC_NO, KC_NO, KC_NO, KC_NO, KC_NO, KC_NO, KC_NO, KC_NO,
                             KC_NO, KC_NO, KC_NO, KC_NO
    ),
    */
    // COLEMAK-DH chars
    [0] = LAYOUT_split_3x5_2(
        KC_Q, KC_W, KC_F, KC_P, KC_B, KC_J, KC_L, KC_U, KC_Y, KC_SCLN,
        KC_A, KC_R, KC_S, KC_T, KC_G, KC_M, KC_N, KC_E, KC_I, KC_O,
        KC_Z, KC_C, KC_D, KC_V, KC_X, KC_K, KC_H, KC_COMM, KC_DOT, KC_QUOT,
                           TL_LOWR, KC_SPC, KC_LSFT, TL_UPPR
    ),
    // Red layer
    [1] = LAYOUT_split_3x5_2(
        KC_1 , KC_2 , KC_3 , KC_4 , KC_5 , KC_HOME,KC_PGDN,KC_PGUP,KC_END,   KC_DEL,
        KC_6 , KC_7 , KC_8 , KC_9 , KC_0 , KC_LEFT,KC_DOWN,KC_UP,  KC_RIGHT, KC_NO,
        KC_NO, KC_NO, KC_NO, KC_NO, KC_NO, KC_NO,  KC_NO,  KC_NO,  KC_NO,    KC_NO,
                           KC_NO, KC_TRNS, KC_TRNS, TL_UPPR
    ),
    // Blue layer
    [2] = LAYOUT_split_3x5_2(
        // +       @          #       $          %       _          &          *       `      ~
        S(KC_EQL), S(KC_2),   S(KC_3),S(KC_4),S(KC_5),   S(KC_MINS),S(KC_7),   S(KC_8),KC_GRV,S(KC_GRV),
        // <       {          [       (       ?          |          -          /       =      :
        S(KC_COMM),S(KC_LBRC),KC_LBRC,S(KC_9),S(KC_SLSH),S(KC_BSLS),KC_MINS,   KC_SLSH,KC_EQL,S(KC_SCLN),
        // >       }          ]       )       !          ^          — em dash                 "
        S(KC_DOT), S(KC_RBRC),KC_RBRC,S(KC_0),S(KC_1),   S(KC_6),   UC(0x2014),KC_NO,  KC_NO, S(KC_QUOT),
                                        TL_LOWR, KC_BSLS, KC_TRNS, KC_NO
    ),
    // Purple layer (blue + red)
    [3] = LAYOUT_split_3x5_2(
        KC_F1, KC_F2, KC_F3, KC_F4, KC_F5,  CAPS, KC_COPY, KC_MNXT,KC_VOLU,KC_BRIU,
        KC_F6, KC_F7, KC_F8, KC_F9, KC_F10,KC_NO, KC_PASTE,KC_MPLY,KC_VOLD,KC_BRID,
        KC_F11,KC_F12,KC_NO, KC_NO, KC_NO, KC_NO, KC_CUT,  KC_MPRV,KC_MUTE,KC_NO  ,
                             KC_NO, KC_NO, KC_NO, KC_NO
    ),
    // QWERTY chars
    [4] = LAYOUT_split_3x5_2(
        KC_Q, KC_W, KC_E, KC_R, KC_T, KC_Y, KC_U, KC_I, KC_O, KC_P,
        KC_A, KC_S, KC_D, KC_F, KC_G, KC_H, KC_J, KC_K, KC_L, S(KC_SCLN),
        KC_Z, KC_X, KC_C, KC_V, KC_B, KC_N, KC_M, KC_COMM, KC_DOT, KC_QUOT,
                           TL_LOWR, KC_SPC, KC_LSFT, TL_UPPR
    )
};
// }}}

void keyboard_post_init_user(void) {
  debug_enable=true;
  debug_matrix=true;
  debug_keyboard=true;
  debug_mouse=true;
}
