/*
 * Copyright © 2018 Christian Persch
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 3 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 */

#if !defined(_VTE_OSC)
#error "Must define _VTE_OSC before including this file"
#endif

/* _VTE_OSC(DTTERM_CWD, 3) Conflicts with XTERM_SET_XPROPERTY */

_VTE_OSC(EMACS_51, 51)

_VTE_OSC(ITERM2_GROWL, 9)
_VTE_OSC(ITERM2_133, 133)
_VTE_OSC(ITERM2_1337, 1337)

_VTE_OSC(KONSOLE_30, 30)
_VTE_OSC(KONSOLE_31, 31)

/* _VTE_OSC(MINTTY_CLIPBOARD_COPY_WINDOW_TITLE,    7721) */
/* _VTE_OSC(MINTTY_CHANGE_FONT_SIZE,               7770) */
/* _VTE_OSC(MINTTY_QUERY_FONT_SUPPORTS_CHARACTERS, 7771) */
/* _VTE_OSC(MINTTY_CHANGE_FONT_AND_WINDOW_SIZE,    7777) */
/* _VTE_OSC(MINTTY_INDIC_WIDE, 77119) out of range */

_VTE_OSC(RLOGIN_SET_KANJI_MODE, 800)
_VTE_OSC(RLOGIN_SPEECH, 801)

/* _VTE_OSC(RXVT_MENU, 10) * Conflics with XTERM_SET_COLOR_TEXT_FG */
_VTE_OSC(RXVT_SET_BACKGROUND_PIXMAP, 20)
_VTE_OSC(RXVT_SET_COLOR_FG, 39)
_VTE_OSC(RXVT_SET_COLOR_BG, 49)
_VTE_OSC(RXVT_DUMP_SCREEN, 55)

_VTE_OSC(URXVT_SET_LOCALE, 701)
_VTE_OSC(URXVT_VERSION, 702)
_VTE_OSC(URXVT_SET_COLOR_TEXT_ITALIC, 704)
_VTE_OSC(URXVT_SET_COLOR_TEXT_BOLD, 706)
_VTE_OSC(URXVT_SET_COLOR_UNDERLINE, 707)
_VTE_OSC(URXVT_SET_COLOR_BORDER, 708)
_VTE_OSC(URXVT_SET_FONT, 710)
_VTE_OSC(URXVT_SET_FONT_BOLD, 711)
_VTE_OSC(URXVT_SET_FONT_ITALIC, 712)
_VTE_OSC(URXVT_SET_FONT_BOLD_ITALIC, 713)
_VTE_OSC(URXVT_VIEW_UP, 720)
_VTE_OSC(URXVT_VIEW_DOWN, 721)
_VTE_OSC(URXVT_EXTENSION, 777)

_VTE_OSC(VTECWF, 6)
_VTE_OSC(VTECWD, 7)
_VTE_OSC(VTEHYPER, 8)

_VTE_OSC(XTERM_SET_WINDOW_AND_ICON_TITLE, 0)
_VTE_OSC(XTERM_SET_ICON_TITLE, 1)
_VTE_OSC(XTERM_SET_WINDOW_TITLE, 2)
_VTE_OSC(XTERM_SET_XPROPERTY, 3)
_VTE_OSC(XTERM_SET_COLOR, 4)
_VTE_OSC(XTERM_SET_COLOR_SPECIAL, 5)
/* _VTE_OSC(XTERM_SET_COLOR_MODE, 6) Conflict with our own OSC 6 VTECWF; so use 106 */
_VTE_OSC(XTERM_SET_COLOR_TEXT_FG, 10)
_VTE_OSC(XTERM_SET_COLOR_TEXT_BG, 11)
_VTE_OSC(XTERM_SET_COLOR_CURSOR_BG, 12)
_VTE_OSC(XTERM_SET_COLOR_MOUSE_CURSOR_FG, 13)
_VTE_OSC(XTERM_SET_COLOR_MOUSE_CURSOR_BG, 14)
_VTE_OSC(XTERM_SET_COLOR_TEK_FG, 15)
_VTE_OSC(XTERM_SET_COLOR_TEK_BG, 16)
_VTE_OSC(XTERM_SET_COLOR_HIGHLIGHT_BG, 17)
_VTE_OSC(XTERM_SET_COLOR_TEK_CURSOR, 18)
_VTE_OSC(XTERM_SET_COLOR_HIGHLIGHT_FG, 19)
_VTE_OSC(XTERM_LOGFILE, 46)
_VTE_OSC(XTERM_SET_FONT, 50)
_VTE_OSC(XTERM_SET_XSELECTION, 52)
_VTE_OSC(XTERM_RESET_COLOR, 104)
_VTE_OSC(XTERM_RESET_COLOR_SPECIAL, 105)
_VTE_OSC(XTERM_SET_COLOR_MODE, 106)
_VTE_OSC(XTERM_RESET_COLOR_TEXT_FG, 110)
_VTE_OSC(XTERM_RESET_COLOR_TEXT_BG, 111)
_VTE_OSC(XTERM_RESET_COLOR_CURSOR_BG, 112)
_VTE_OSC(XTERM_RESET_COLOR_MOUSE_CURSOR_FG, 113)
_VTE_OSC(XTERM_RESET_COLOR_MOUSE_CURSOR_BG, 114)
_VTE_OSC(XTERM_RESET_COLOR_TEK_FG, 115)
_VTE_OSC(XTERM_RESET_COLOR_TEK_BG, 116)
_VTE_OSC(XTERM_RESET_COLOR_HIGHLIGHT_BG, 117)
_VTE_OSC(XTERM_RESET_COLOR_TEK_CURSOR, 118)
_VTE_OSC(XTERM_RESET_COLOR_HIGHLIGHT_FG, 119)

_VTE_OSC(YF_RQGWR, 8900)
