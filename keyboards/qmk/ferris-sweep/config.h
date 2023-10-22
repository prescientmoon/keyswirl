#pragma once

// {{{ Mousekey
// Set the mouse settings to a comfortable speed/accuracy trade-off,
// assuming a screen refresh rate of 60 Htz or higher
// The default is 50. This makes the mouse ~3 times faster and more accurate
#define MOUSEKEY_INTERVAL 16
// The default is 20. Since we made the mouse about 3 times faster with the previous setting,
// give it more time to accelerate to max speed to retain precise control over short distances.
#define MOUSEKEY_TIME_TO_MAX 40
// The default is 300. Let's try and make this as low as possible while keeping the cursor responsive
#define MOUSEKEY_DELAY 100
// It makes sense to use the same delay for the mouseweel
#define MOUSEKEY_WHEEL_DELAY 100
// The default is 100
#define MOUSEKEY_WHEEL_INTERVAL 50
// The default is 40
#define MOUSEKEY_WHEEL_TIME_TO_MAX 100
// }}}
// {{{ Tap-hold
#define TAPPING_TERM 200
#define PERMISSIVE_HOLD
#define QUICK_TAP_TERM 0
// }}}
// {{{ Combos
#define COMBO_MUST_HOLD_MODS
#define COMBO_TERM 30
#define COMBO_HOLD_TERM 30
#define COMBO_ONLY_FROM_LAYER 0
// }}}

// https://github.com/qmk/qmk_firmware/blob/master/docs/feature_split_keyboard.md#handedness-by-eeprom
#define EE_HANDS
#define SPLIT_USB_DETECT
