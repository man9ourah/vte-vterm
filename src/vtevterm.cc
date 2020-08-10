/*
 * Copyright © 2001-2004 Red Hat, Inc.
 * Copyright © 2015 David Herrmann <dh.herrmann@gmail.com>
 * Copyright © 2008-2018 Christian Persch
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
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
 */

#include "config.h"

#include <search.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>
#ifdef HAVE_SYS_SYSLIMITS_H
#include <sys/syslimits.h>
#endif

#include <glib.h>

#include <vte/vte.h>
#include "vteinternal.hh"
#include "vtegtk.hh"


namespace vte {
namespace terminal {

static gboolean vterm_is_stmt_char(gunichar c) {
    // check for whitespace
    return !g_unichar_isspace(c);
}

static gboolean vterm_is_word_char(gunichar c) {
    // Just like vim's default
    return g_unichar_isalpha(c) || c == '_' || (c >=48 && c <= 57) || (c >=192 && c <= 255);
}

static guint16 vterm_get_row_length(VteRowData const* row){
    // We want to ignore the white spaces at the end of the lines
    guint16 len;
    const VteCell *cell;
    for (len = row->len; len > 0; len--) {
            cell = &row->cells[len - 1];
            if (cell->attr.fragment() || (cell->c != 0 && !g_unichar_isspace(cell->c)))
                    break;
    }
    return len;
}


static long vterm_get_first_non_blank_col(VteRowData const* row){
    // We want to ignore the white spaces at the beginning
    guint16 col;
    const VteCell *cell;
    for (col = 0; col < row->len; col++) {
            cell = &row->cells[col];
            if (cell->c != 0 && !g_unichar_isspace(cell->c))
                    break;
    }
    return col;
}

void
Terminal::vterm_cursor_init(GtkWidget* widget){
    // Set the text font for _vte_draw of our vterm_cursor
    vterm_cursor.cursor_widget = widget;
    _vte_draw_set_text_font (vterm_cursor.draw,
                                     vterm_cursor.cursor_widget,
                                     m_fontdesc.get(),
                                     m_cell_width_scale,
                                     m_cell_height_scale);
}

void
Terminal::vterm_cursor_set_shown(gboolean is_shown){
    vterm_cursor.cursor_is_shown = is_shown;
}

void
Terminal::vterm_cursor_selection(VTermSelectionType selection_type){
    vterm_cursor.selection_type = selection_type;

    // Ensure cursor is on screen
    // this is also to update between restricted cursor position in block mode and
    // non-restricted in others
    vterm_cursor_move(VTermCursorMove::NOP);

    // If selection is really block mode, is will be updated in update_selection
    m_selection_block_mode = false;
    if(selection_type == VTERM_SELECTION_NONE){
        deselect_all();
        return;
    }

    vterm_cursor.selection_start = vterm_cursor.cursor;
    vterm_cursor_update_selection();
}

void
Terminal::vterm_cursor_update_selection(){
    switch(vterm_cursor.selection_type){
        case VTERM_CHAR_SELECTION:{
            // select_text expect its arguments to be ordered such that the
            // first position is before the second postion.. so we need to
            // figure that out. We do it termite's way..
            const long begin = vterm_cursor.selection_start.row * m_column_count +
                vterm_cursor.selection_start.col;
            const long end = vterm_cursor.cursor.row * m_column_count +
                vterm_cursor.cursor.col;
            if(begin < end)
                select_text(
                        vterm_cursor.selection_start.col, vterm_cursor.selection_start.row,
                        vterm_cursor.cursor.col + 1, vterm_cursor.cursor.row);
            else
                select_text(
                        vterm_cursor.cursor.col, vterm_cursor.cursor.row,
                        vterm_cursor.selection_start.col + 1, vterm_cursor.selection_start.row);
            break;
        }

        case VTERM_LINE_SELECTION:{
                 select_text(
                        0, MIN(vterm_cursor.cursor.row, vterm_cursor.selection_start.row),
                        m_column_count, MAX(vterm_cursor.cursor.row, vterm_cursor.selection_start.row));
            break;
        }

        case VTERM_BLOCK_SELECTION:{
            m_selection_block_mode = true;
                 select_text(
                        MIN(vterm_cursor.cursor.col, vterm_cursor.selection_start.col),
                        MIN(vterm_cursor.cursor.row, vterm_cursor.selection_start.row),
                        MAX(vterm_cursor.cursor.col, vterm_cursor.selection_start.col),
                        MAX(vterm_cursor.cursor.row, vterm_cursor.selection_start.row));
            break;
        }

        case VTERM_SELECTION_NONE:{
            // Nothing to be done
            return;
        }
    }
}

void
Terminal::vterm_cursor_move_backword(gboolean (*compare_end)(gunichar c)){
    VteRowData const* rowdata = nullptr;
    long col = vterm_cursor.cursor.col;
    long row = vterm_cursor.cursor.row;

    // Whether we are currently in a word or a seq of non-word
    bool in_word = false;
    bool in_non_word = false;

    long len = 0;

    // We should wrap to previous row anyways if we are already at the first
    // word of the row
    bool do_wrap = false;
    rowdata = find_row_data(row);
    if(!rowdata)
        return;
    if(col <= vterm_get_first_non_blank_col(rowdata))
        do_wrap = true;

    while (true) {
        // Move backword within the row.
        VteCell const* pcell = nullptr;
        // We start from current position and move backword
        for (; col > 0; col--) {

            // This is the cell BEFORE the current position
            pcell = find_charcell(col - 1,row);
            if(!pcell)
                break;

            bool is_word_char = compare_end(pcell->c);
            if(!is_word_char && in_word){
                // we are at the leftmost char of word
                break;
            }

            bool is_non_word_char = !is_word_char && !g_unichar_isspace(pcell->c);
            if(!is_non_word_char && in_non_word){
                // we are at the leftmost char of a seq on non-word
                // we stop just like vim's behavior
                break;
            }

            // Flag start of word
            if(is_word_char)
                in_word = true;

            // Flag start of non char seq
            if(is_non_word_char)
                in_non_word = true;
        }

        if (col > 0) {
            // We hit a stopping point, so stop.
            break;
        }

        rowdata = find_row_data(row - 1);
        if (!rowdata)
            break;

        bool soft_wrapped = rowdata->attr.soft_wrapped;
        if (!soft_wrapped && !do_wrap){
            // Reached a hard newline and we are not explicitly wrapping
            break;
        }

        // We use the actual length instead of non-white space length so that we
        // consider white spaces at the end of the row.
        len = _vte_row_data_nonempty_length(rowdata);

        // Check maybe first char and last char are not related
        // if they are, wrap to prev row.
        // if explicit wrapping, we will wrap to prev row no matter what
        if(!do_wrap){
            // implicit wrap
            // last cell of prev row
            VteCell const* ccell = find_charcell(len - 1,row - 1);
            if(!ccell || !pcell)
                break;

            // For the case if first char of this row is not space and the last
            // char of prev row is a space: dont wrap
            if(!g_unichar_isspace(pcell->c) && g_unichar_isspace(ccell->c))
                break;

            bool is_word_char_prev = compare_end(ccell->c);

            // For the case if we are expecting a word char but the last char of
            // previous row is not a word char
            if(in_word && !is_word_char_prev)
                break;

            // For the case if we are expecting a non-word char but the last
            // char of the previous row is not a non-word char
            if(in_non_word && is_word_char_prev)
                break;
        }

        // Move on to the prev line.
        // We start from len so that (len - 1) is considered
        col = len;
        row--;

        // Explicit wrapping is only for one row
        do_wrap = false;
    }

    vterm_cursor.cursor.col = col;

    // do we need to scroll?
    if(row < first_displayed_row())
        queue_adjustment_value_changed_clamped(row);
    vterm_cursor.cursor.row = row;
}

void
Terminal::vterm_cursor_move_forward_end(gboolean (*compare_end)(gunichar c)){
    VteRowData const* rowdata = nullptr;
    long col = vterm_cursor.cursor.col;
    long row = vterm_cursor.cursor.row;

    // Did we start a range of a new word?
    bool begin_of_word = false;
    bool begin_of_non_word = false;

    // Do explicit wrapping if we are at the end of the current line ignoring
    // the trailing whitespaces
    rowdata = find_row_data(row);
    if(!rowdata)
        return;
    long len = vterm_get_row_length(rowdata);
    bool do_wrap = false;
    if(col >= len-1)
        do_wrap = true;

    VteCell const* pcell = nullptr;
    while (true) {
        rowdata = find_row_data(row);
        if (!rowdata)
            break;

        // We use the actual length with trailing whitespaces
        // to detect word ending at the end of the row
        len = _vte_row_data_nonempty_length(rowdata);

        // Move forward within the row.
        for (; col < len - 1; col++) {
            // This is the char AFTER the current char
            pcell = find_charcell(col + 1,row);
            if(!pcell)
                break;

            bool is_word_char = compare_end(pcell->c);
            if(!is_word_char && begin_of_word){
                // We reached the end of word
                break;
            }

            bool is_non_word_char = !is_word_char && !g_unichar_isspace(pcell->c);
            if(!is_non_word_char && begin_of_non_word){
                // We reached the end of a non-word sequence
                break;
            }

            // Flag beginning of word
            if(is_word_char)
                begin_of_word = true;

            // Flag beginning of non-word
            if(is_non_word_char)
                begin_of_non_word = true;
        }

        if (col < len - 1) {
            // We hit a stopping point, so stop.
            break;
        }

        bool soft_wrapped = rowdata->attr.soft_wrapped;
        if (!soft_wrapped && !do_wrap) {
            // Reached a hard newline and we are not explicitly wrapping
            break;
        }

        // Check maybe last char and first char are not related
        // if they are, wrap to next row.
        // if explicit wrapping, we will wrap to prev row no matter what
        if(!do_wrap){
            // implicit wrap
            // first char of next row
            VteCell const* ccell = find_charcell(0,row + 1);
            if(!ccell || !pcell)
                break;

            // For the case if last char is not a space, but the first char in
            // the next row is a space: dont wrap
            if(!g_unichar_isspace(pcell->c) && g_unichar_isspace(ccell->c))
                break;

            bool is_word_char_next = compare_end(ccell->c);

            // For the case if we are expecting a word char, but the first char
            // is not a word char
            if(begin_of_word && !is_word_char_next)
                break;

            // For the case if we are expecting a non-word char, but the first
            // char is not a non-word char
            if(begin_of_non_word && is_word_char_next)
                break;
        }

        // Move on to the next line.
        // We start from -1, so that the first char is considered. If we stop at
        // -1 it will be clamped when drawing
        col = -1;
        row++;
        do_wrap = false;
    }

    vterm_cursor.cursor.col = col;

    // Do we need to scroll?
    if(row > last_displayed_row())
        queue_adjustment_value_changed_clamped(first_displayed_row() + (row - vterm_cursor.cursor.row));
    vterm_cursor.cursor.row = row;
}

void
Terminal::vterm_cursor_move_forward(gboolean (*compare_end)(gunichar c)){
    VteRowData const* rowdata = nullptr;
    long col = vterm_cursor.cursor.col;
    long row = vterm_cursor.cursor.row;

    // Did we reach an end of a word or a non-word
    bool end_of_word = false;
    bool end_of_non_word = false;

    // Do explicit wrapping if we are at the end of the current line ignoring
    // the trailing whitespaces
    rowdata = find_row_data(row);
    if(!rowdata)
        return;
    long len = vterm_get_row_length(rowdata);
    bool do_wrap = false;
    if(col >= len-1)
        do_wrap = true;

    VteCell const* pcell = nullptr;
    while (true) {
        rowdata = find_row_data(row);
        if (!rowdata)
            break;

        // We use the actual length with trailing whitespaces
        // to detect word ending at the end of the row
        len = _vte_row_data_nonempty_length(rowdata);

        // Move forward within the row.
        for (; col < len; col++) {
            // This is the char we are in, not before or after
            pcell = find_charcell(col,row);
            if(!pcell)
                break;

            bool is_word_char = compare_end(pcell->c);
            if(is_word_char && end_of_word){
                // this is a new word we are on
                break;
            }

            bool is_non_word_char = !is_word_char && !g_unichar_isspace(pcell->c);
            if(is_non_word_char && end_of_non_word){
                // this is a new seq of non words 
                break;
            }

            // Flag end of word
            if(!is_word_char)
                end_of_word = true;

            // Flag end of word
            if(!is_non_word_char)
                end_of_non_word = true;
        }

        if (col < len){
            // We hit a stopping point, so stop.
            break;
        }

        bool soft_wrapped = rowdata->attr.soft_wrapped;
        if (!soft_wrapped && !do_wrap){
            // Reached a hard newline and we are not explicitly wrapping
            break;
        }

        if(do_wrap){
            // If we are wrapping explicitly stop at first non-space
            // BTW, we do not do the check that we do in the other two functions
            // here because the decision depends on the char we are in solely
            // and not on the char before or after like the other two functions
            end_of_word = true;
            end_of_non_word = true;
        }

        // Move on to the next line.
        col = 0;
        row++;
        do_wrap = false;
    }

    vterm_cursor.cursor.col = col;

    // Do we need to scroll?
    if(row > last_displayed_row())
        queue_adjustment_value_changed_clamped(first_displayed_row() + (row - vterm_cursor.cursor.row));
    vterm_cursor.cursor.row = row;
}

void
Terminal::vterm_cursor_move(VTermCursorMove direction){

    switch(direction){
        case VTermCursorMove::NOP:{
            // Does not move the cursor unless it needs to be clamped

            // For the row, we clamp between top and bottom displayed rows
            vterm_cursor.cursor.row = CLAMP(vterm_cursor.cursor.row, first_displayed_row(), last_displayed_row());

            // For the column, we clamp between 0 and the last column
            long len = MAX(0, m_column_count - 1);

            // We allow free motion if in block mode, but restricted to
            // non-whitespace trailing characters in othe modes
            if(vterm_cursor.selection_type != VTermSelectionType::VTERM_BLOCK_SELECTION){
                VteRowData const* rowdata = find_row_data(vterm_cursor.cursor.row);
                if(rowdata)
                    len = MAX(0, vterm_get_row_length(rowdata) - 1);
            }

            vterm_cursor.cursor.col = CLAMP(vterm_cursor.cursor.col, 0, len);

            // Note that this is a return, not a break.. so no infinite loop
            // with the call at the bottom of this function
            return;
        }

        case VTermCursorMove::INPUT:{
            // Set the cursor to the input cursor position
            vterm_cursor.cursor.col = m_screen->cursor.col;
            vterm_cursor.cursor.row = m_screen->cursor.row;

            // This is an explicit col move, remember it
            vterm_cursor.sticky_col = vterm_cursor.cursor.col;
            break;
        }

        case VTermCursorMove::RIGHT:{
            if(vterm_cursor.cursor.col + 1 < m_column_count){
                // There are more columns to the right
                vterm_cursor.cursor.col++;
            }else{
                // We hit the rightmost column, see if we should wrap to next
                // line
                VteRowData const* rowdata = find_row_data(vterm_cursor.cursor.row);
                if(rowdata && rowdata->attr.soft_wrapped){
                    vterm_cursor.cursor.col = 0;
                    vterm_cursor_move(VTermCursorMove::DOWN);
                }
            }

            // This is an explicit col move, remember it
            vterm_cursor.sticky_col = vterm_cursor.cursor.col;
            break;
        }

        case VTermCursorMove::LEFT:{
            if(vterm_cursor.cursor.col - 1 >= 0){
                // There are more columns to the left
                vterm_cursor.cursor.col--;
            }else{
                // We hit the leftmost column, see if we should wrap to prev
                // line
                VteRowData const* rowdata = find_row_data(vterm_cursor.cursor.row - 1);
                if(rowdata && rowdata->attr.soft_wrapped){
                    vterm_cursor.cursor.col = m_column_count -1;
                    vterm_cursor_move(VTermCursorMove::UP);
                }
            }

            // This is an explicit col move, remember it
            vterm_cursor.sticky_col = vterm_cursor.cursor.col;
            break;
        }

        case VTermCursorMove::UP:{
            if(vterm_cursor.cursor.row == first_displayed_row()){
                // We need to scroll one row up
                queue_adjustment_value_changed_clamped(vterm_cursor.cursor.row - 1);
            }
            // Will be clamped when drawn
            vterm_cursor.cursor.row--;

            // Position the cursor at the same column the user last explicitly
            // moved to.. this will be clamped before drawing so we are okay if
            // it is more than the new row columns
            vterm_cursor.cursor.col = vterm_cursor.sticky_col;
            break;
        }

        case VTermCursorMove::DOWN:{
            if(vterm_cursor.cursor.row == last_displayed_row()){
                // We need to scroll one row down
                queue_adjustment_value_changed_clamped(first_displayed_row() + 1);
            }
            // Will be clamped when drawn
            vterm_cursor.cursor.row++;

            // Position the cursor at the same column the user last explicitly
            // moved to.. this will be clamped before drawing so we are okay if
            // it is more than the new row columns
            vterm_cursor.cursor.col = vterm_cursor.sticky_col;
            break;
        }

        case VTermCursorMove::BOL:{
            // Go to the beginning of the line, wrap up if soft wrapped.
            vterm_cursor.cursor.col = 0;
            VteRowData const* rowdata = find_row_data(vterm_cursor.cursor.row - 1);
            while(rowdata && rowdata->attr.soft_wrapped){
                vterm_cursor_move(VTermCursorMove::UP);
                rowdata = find_row_data(vterm_cursor.cursor.row - 1);
            }

            // This is an explicit col move, remember it
            vterm_cursor.sticky_col = vterm_cursor.cursor.col;
            break;
        }

        case VTermCursorMove::EOL:{
            // Go to the end of the line, wrap down if soft wrapped.
            VteRowData const* rowdata = find_row_data(vterm_cursor.cursor.row);
            vterm_cursor.cursor.col = vterm_get_row_length(rowdata);
            while(rowdata && rowdata->attr.soft_wrapped){
                vterm_cursor_move(VTermCursorMove::DOWN);
                rowdata = find_row_data(vterm_cursor.cursor.row);
                vterm_cursor.cursor.col = vterm_get_row_length(rowdata);
            }

            // This is an explicit col move, remember it
            // This a jump to eol move, so we put a really big number and it
            // will be clamped to the max of each row when drawing
            // I hope there is not a screen that can fit this many chars in one
            // row :)
            vterm_cursor.sticky_col = 10000000;
            break;
        }

        case VTermCursorMove::RIGHT_WORD:{
            vterm_cursor_move_forward(vterm_is_word_char);

            // This is an explicit col move, remember it
            vterm_cursor.sticky_col = vterm_cursor.cursor.col;
            break;
        }

        case VTermCursorMove::LEFT_WORD:{
            vterm_cursor_move_backword(vterm_is_word_char);

            // This is an explicit col move, remember it
            vterm_cursor.sticky_col = vterm_cursor.cursor.col;
            break;
        }

        case VTermCursorMove::RIGHT_WORD_END:{
            vterm_cursor_move_forward_end(vterm_is_word_char);

            // This is an explicit col move, remember it
            vterm_cursor.sticky_col = vterm_cursor.cursor.col;
            break;
        }

        case VTermCursorMove::RIGHT_STMT:{
            vterm_cursor_move_forward(vterm_is_stmt_char);

            // This is an explicit col move, remember it
            vterm_cursor.sticky_col = vterm_cursor.cursor.col;
            break;
        }

        case VTermCursorMove::LEFT_STMT:{
            vterm_cursor_move_backword(vterm_is_stmt_char);

            // This is an explicit col move, remember it
            vterm_cursor.sticky_col = vterm_cursor.cursor.col;
            break;
        }

        case VTermCursorMove::RIGHT_STMT_END:{
            vterm_cursor_move_forward_end(vterm_is_stmt_char);

            // This is an explicit col move, remember it
            vterm_cursor.sticky_col = vterm_cursor.cursor.col;
            break;
        }

        case VTermCursorMove::TOP:{
            vterm_cursor.cursor.row = first_displayed_row();

            // Position the cursor at the same column the user last explicitly
            // moved to.. this will be clamped before drawing so we are okay if
            // it is more than the new row columns
            vterm_cursor.cursor.col = vterm_cursor.sticky_col;
            break;
        }

        case VTermCursorMove::BOTTOM:{
            vterm_cursor.cursor.row = last_displayed_row();

            // Position the cursor at the same column the user last explicitly
            // moved to.. this will be clamped before drawing so we are okay if
            // it is more than the new row columns
            vterm_cursor.cursor.col = vterm_cursor.sticky_col;
            break;
        }

        case VTermCursorMove::MIDDLE:{
            long first_row = first_displayed_row();
            long last_row = last_displayed_row();
            vterm_cursor.cursor.row = first_row + (last_row-first_row)/2;

            // Position the cursor at the same column the user last explicitly
            // moved to.. this will be clamped before drawing so we are okay if
            // it is more than the new row columns
            vterm_cursor.cursor.col = vterm_cursor.sticky_col;
            break;
        }
    }

    // Ensure cursor is on screen
    vterm_cursor_move(VTermCursorMove::NOP);

    // Update the selection
    vterm_cursor_update_selection();
}

void
Terminal::vterm_draw_cells(struct _vte_draw_text_request *items,
                               gssize n,
                               uint32_t fore,
                               uint32_t back,
                               uint32_t deco,
                               bool clear,
                               bool draw_default_bg,
                               uint32_t attr,
                               bool hyperlink,
                               bool hilite,
                               int column_width,
                               int row_height)
{
    // A copy of draw_cells in vte.cc

    // shadow the global member m_draw with vterm's cursor
    struct _vte_draw* m_draw = vterm_cursor.draw;

    int i, xl, xr, y;
    gint columns = 0;
    vte::color::rgb fg, bg, dc;

    g_assert(n > 0);
#if 0
    _VTE_DEBUG_IF(VTE_DEBUG_CELLS) {
        GString *str = g_string_new (NULL);
        gchar *tmp;
        for (i = 0; i < n; i++) {
            g_string_append_unichar (str, items[i].c);
        }
        tmp = g_string_free (str, FALSE);
                g_printerr ("draw_cells('%s', fore=%d, back=%d, deco=%d, bold=%d,"
                                " ul=%d, strike=%d, ol=%d, blink=%d,"
                                " hyperlink=%d, hilite=%d, boxed=%d)\n",
                                tmp, fore, back, deco, bold,
                                underline, strikethrough, overline, blink,
                                hyperlink, hilite, boxed);
        g_free (tmp);
    }
#endif

    rgb_from_index<8, 8, 8>(fore, fg);
    rgb_from_index<8, 8, 8>(back, bg);
    // FIXMEchpe defer resolving deco color until we actually need to draw an underline?
    if (deco == VTE_DEFAULT_FG)
        dc = fg;
    else
        rgb_from_index<4, 5, 4>(deco, dc);

    if (clear && (draw_default_bg || back != VTE_DEFAULT_BG)) {
            /* Paint the background. */
            i = 0;
            while (i < n) {
                    xl = items[i].x;
                    xr = items[i].x + items[i].columns * column_width;
                    y = items[i].y;
                    /* Items are not necessarily contiguous in LTR order.
                     * Combine as long as they form a single visual run. */
                    for (i++; i < n && items[i].y == y; i++) {
                            if (G_LIKELY (items[i].x == xr)) {
                                    xr += items[i].columns * column_width;  /* extend to the right */
                            } else if (items[i].x + items[i].columns * column_width == xl) {
                                    xl = items[i].x;                        /* extend to the left */
                            } else {
                                    break;                                  /* break the run */
                            }
                    }
        _vte_draw_fill_rectangle(m_draw,
                                             xl,
                                             y,
                                             xr - xl, row_height,
                                             &bg, VTE_DRAW_OPAQUE);
            }
    }

    if (attr & VTE_ATTR_BLINK) {
        /* Notify the caller that cells with the "blink" attribute were encountered (regardless of
         * whether they're actually painted or skipped now), so that the caller can set up a timer
         * to make them blink if it wishes to. */
        m_text_to_blink = true;

        /* This is for the "off" state of blinking text. Invisible text could also be handled here,
         * but it's not, it's handled outside by not even calling this method.
         * Setting fg = bg and painting the text would not work for two reasons: it'd be opaque
         * even if the background is translucent, and this method can be called with a continuous
         * run of identical fg, yet different bg colored cells. So we simply bail out. */
        if (!m_text_blink_state)
                return;
    }

    /* Draw whatever SFX are required. Do this before drawing the letters,
     * so that if the descent of a letter crosses an underline of a different color,
     * it's the letter's color that wins. Other kinds of decorations always have the
     * same color as the text, so the order is irrelevant there. */
    if ((attr & (VTE_ATTR_UNDERLINE_MASK |
                 VTE_ATTR_STRIKETHROUGH_MASK |
                 VTE_ATTR_OVERLINE_MASK |
                 VTE_ATTR_BOXED_MASK)) |
        hyperlink | hilite) {
        i = 0;
        while (i < n) {
            xl = items[i].x;
            xr = items[i].x + items[i].columns * column_width;
            columns = items[i].columns;
            y = items[i].y;
            /* Items are not necessarily contiguous in LTR order.
             * Combine as long as they form a single visual run. */
            for (i++; i < n && items[i].y == y; i++) {
                if (G_LIKELY (items[i].x == xr)) {
                    xr += items[i].columns * column_width;  /* extend to the right */
                    columns += items[i].columns;
                } else if (items[i].x + items[i].columns * column_width == xl) {
                    xl = items[i].x;                        /* extend to the left */
                    columns += items[i].columns;
                } else {
                    break;                                  /* break the run */
                }
            }
            switch (vte_attr_get_value(attr, VTE_ATTR_UNDERLINE_VALUE_MASK, VTE_ATTR_UNDERLINE_SHIFT)) {
                case 1:
                    _vte_draw_draw_line(m_draw,
                                        xl,
                                        y + m_underline_position,
                                        xr - 1,
                                        y + m_underline_position + m_underline_thickness - 1,
                                        VTE_LINE_WIDTH,
                                        &dc, VTE_DRAW_OPAQUE);
                    break;
                case 2:
                    _vte_draw_draw_line(m_draw,
                                        xl,
                                        y + m_double_underline_position,
                                        xr - 1,
                                        y + m_double_underline_position + m_double_underline_thickness - 1,
                                        VTE_LINE_WIDTH,
                                        &dc, VTE_DRAW_OPAQUE);
                    _vte_draw_draw_line(m_draw,
                                        xl,
                                        y + m_double_underline_position + 2 * m_double_underline_thickness,
                                        xr - 1,
                                        y + m_double_underline_position + 3 * m_double_underline_thickness - 1,
                                        VTE_LINE_WIDTH,
                                        &dc, VTE_DRAW_OPAQUE);
                    break;
                case 3:
                    _vte_draw_draw_undercurl(m_draw,
                                             xl,
                                             y + m_undercurl_position,
                                             m_undercurl_thickness,
                                             columns,
                                             &dc, VTE_DRAW_OPAQUE);
                    break;
            }

            if (attr & VTE_ATTR_STRIKETHROUGH) {
                _vte_draw_draw_line(m_draw,
                                    xl,
                                    y + m_strikethrough_position,
                                    xr - 1,
                                    y + m_strikethrough_position + m_strikethrough_thickness - 1,
                                    VTE_LINE_WIDTH,
                                    &fg, VTE_DRAW_OPAQUE);
            }

            if (attr & VTE_ATTR_OVERLINE) {
                _vte_draw_draw_line(m_draw,
                                    xl,
                                    y + m_overline_position,
                                    xr - 1,
                                    y + m_overline_position + m_overline_thickness - 1,
                                    VTE_LINE_WIDTH,
                                    &fg, VTE_DRAW_OPAQUE);
            }

            if (hilite) {
                _vte_draw_draw_line(m_draw,
                                    xl,
                                    y + m_regex_underline_position,
                                    xr - 1,
                                    y + m_regex_underline_position + m_regex_underline_thickness - 1,
                                    VTE_LINE_WIDTH,
                                    &fg, VTE_DRAW_OPAQUE);
            }else if (hyperlink) {
                for (double j = 1.0 / 6.0; j < columns; j += 0.5) {
                    _vte_draw_fill_rectangle(m_draw,
                                             xl + j * column_width,
                                             y + m_regex_underline_position,
                                             MAX(column_width / 6.0, 1.0),
                                             m_regex_underline_thickness,
                                             &fg, VTE_DRAW_OPAQUE);
                }
            }
            if (attr & VTE_ATTR_BOXED) {
                _vte_draw_draw_rectangle(m_draw,
                                         xl,
                                         y,
                                         xr - xl,
                                         row_height,
                                         &fg, VTE_DRAW_OPAQUE);
            }
        }
    }

    _vte_draw_text(m_draw,
                   items, n,
                   attr,
                   &fg, VTE_DRAW_OPAQUE,
                   _vte_draw_get_style(attr & VTE_ATTR_BOLD,
                                       attr & VTE_ATTR_ITALIC));
}

void
Terminal::vterm_cursor_draw(cairo_t *cr){
    // a copy of paint_cursor in vte.cc

    // Ensure cursor is on screen
    // This call is necessary to clamp the cursor when scrolling or resizing
    vterm_cursor_move(VTermCursorMove::NOP);

    // shadow the global member m_draw with vterm's cursor
    struct _vte_draw* m_draw = vterm_cursor.draw;

    struct _vte_draw_text_request item;
    vte::grid::row_t drow;
    vte::grid::column_t lcol, vcol;
    int width, height, cursor_width;
    guint style = 0;
    guint fore, back, deco;
    vte::color::rgb bg;
    int x, y;
    gboolean selected, focus;

    focus = m_has_focus;

    lcol = vterm_cursor.cursor.col;
    drow = vterm_cursor.cursor.row;
    width = m_cell_width;
    height = m_cell_height;

    // Set the cairo
    _vte_draw_set_cairo(vterm_cursor.draw, cr);

    // clip this cairo and translate it just like the terminal's
    int allocated_width = get_allocated_width();
    int allocated_height = get_allocated_height();
    cairo_save(cr);
    int extra_area_for_cursor = (decscusr_cursor_shape() == CursorShape::eBLOCK && !m_has_focus) ? VTE_LINE_WIDTH : 0;
    cairo_rectangle(cr, 0, m_padding.top - extra_area_for_cursor, allocated_width, allocated_height - m_padding.top - m_padding.bottom + 2 * extra_area_for_cursor);
    cairo_clip(cr);

    cairo_translate(cr, m_padding.left, m_padding.top);

    /* Need to ensure the ringview is updated. */
    ringview_update();

    /* Find the first cell of the character "under" the cursor.
     * This is for CJK.  For TAB, paint the cursor where it really is. */
    VteRowData const *row_data = find_row_data(drow);
    vte::base::BidiRow const *bidirow = m_ringview.get_bidirow(drow);

    auto cell = find_charcell(lcol, drow);
    while (cell != NULL && cell->attr.fragment() && cell->c != '\t' && lcol > 0) {
        lcol--;
        cell = find_charcell(lcol, drow);
    }

    /* Draw the cursor. */
    vcol = bidirow->log2vis(lcol);
    item.c = (cell && cell->c) ? bidirow->vis_get_shaped_char(vcol, cell->c) : ' ';
    item.columns = item.c == '\t' ? 1 : cell ? cell->attr.columns() : 1;
    item.x = (vcol - ((cell && bidirow->vis_is_rtl(vcol)) ? cell->attr.columns() - 1 : 0)) * width;
    item.y = row_to_pixel(drow);
    item.mirror = bidirow->vis_is_rtl(vcol);
    item.box_mirror = (row_data && (row_data->attr.bidi_flags & VTE_BIDI_FLAG_BOX_MIRROR));
    if (cell && cell->c != 0) {
        style = _vte_draw_get_style(cell->attr.bold(), cell->attr.italic());
    }

    selected = false; // Make the vterm cursor always shown (deleted: cell_is_selected_log(lcol, drow));
    determine_cursor_colors(cell, selected, &fore, &back, &deco);
    rgb_from_index<8, 8, 8>(back, bg);

    x = item.x;
    y = item.y;

    switch (decscusr_cursor_shape()) {
        case CursorShape::eIBEAM:{
            /* Draw at the very left of the cell (before the spacing), even in case of CJK.
             * IMO (egmont) not overrunning the letter improves readability, vertical movement
             * looks good (no zigzag even when a somewhat wider glyph that starts filling up
             * the left spacing, or CJK that begins further to the right is encountered),
             * and also this is where it looks good if background colors change, including
             * Shift+arrows highlighting experience in some editors. As per the behavior of
             * word processors, don't increase the height by the line spacing. */
            int stem_width;

            stem_width = (int) (((float) (m_char_ascent + m_char_descent)) * m_cursor_aspect_ratio + 0.5);
            stem_width = CLAMP (stem_width, VTE_LINE_WIDTH, m_cell_width);

            /* The I-beam goes to the right edge of the cell if its character has RTL resolved direction. */
            if (bidirow->vis_is_rtl(vcol))
                    x += item.columns * m_cell_width - stem_width;

            _vte_draw_fill_rectangle(m_draw,
                                     x, y + m_char_padding.top, stem_width, m_char_ascent + m_char_descent,
                                     &bg, VTE_DRAW_OPAQUE);

            /* Show the direction of the current character if the paragraph contains a mixture
             * of directions.
             * FIXME Do this for the other cursor shapes, too. Need to find a good visual design. */
            if (focus && bidirow->has_foreign())
                    _vte_draw_fill_rectangle(m_draw,
                                             bidirow->vis_is_rtl(vcol) ? x - stem_width : x + stem_width,
                                             y + m_char_padding.top,
                                             stem_width, stem_width,
                                             &bg, VTE_DRAW_OPAQUE);
            break;
                }

        case CursorShape::eUNDERLINE:{
            /* The width is at least the overall width of the cell (or two cells) minus the two
             * half spacings on the two edges. That is, underlines under a CJK are more than twice
             * as wide as narrow characters in case of letter spacing. Plus, if necessary, the width
             * is increased to span under the entire glyph. Vertical position is not affected by
             * line spacing. */

            int line_height, left, right;

            /* use height (not width) so underline and ibeam will
             * be equally visible */
            line_height = (int) (((float) (m_char_ascent + m_char_descent)) * m_cursor_aspect_ratio + 0.5);
            line_height = CLAMP (line_height, VTE_LINE_WIDTH, m_char_ascent + m_char_descent);

            left = m_char_padding.left;
            right = item.columns * m_cell_width - m_char_padding.right;

            if (cell && cell->c != 0 && cell->c != ' ' && cell->c != '\t') {
                    int l, r;
                    _vte_draw_get_char_edges (m_draw, cell->c, cell->attr.columns(), style, &l, &r);
                    left = MIN(left, l);
                    right = MAX(right, r);
            }

            _vte_draw_fill_rectangle(m_draw,
                                     x + left, y + m_cell_height - m_char_padding.bottom - line_height,
                                     right - left, line_height,
                                     &bg, VTE_DRAW_OPAQUE);
            break;
        }

        case CursorShape::eBLOCK:{
            /* Include the spacings in the cursor, see bug 781479 comments 39-44.
             * Make the cursor even wider if the glyph is wider. */

            cursor_width = item.columns * width;
            if (cell && cell->c != 0 && cell->c != ' ' && cell->c != '\t') {
                int r;
                _vte_draw_get_char_edges (m_draw, cell->c, cell->attr.columns(), style, NULL, &r);
                cursor_width = MAX(cursor_width, r);
            }

            uint32_t const attr_mask = m_allow_bold ? ~0 : ~VTE_ATTR_BOLD_MASK;

            if (focus) {
                /* just reverse the character under the cursor */
                _vte_draw_fill_rectangle(m_draw,
                                         x, y,
                                         cursor_width, height,
                                         &bg, VTE_DRAW_OPAQUE);

                if (cell && cell->c != 0 && cell->c != ' ' && cell->c != '\t') {
                    vterm_draw_cells(
                                &item, 1,
                                fore, back, deco, TRUE, FALSE,
                                cell->attr.attr & attr_mask,
                                m_allow_hyperlink && cell->attr.hyperlink_idx != 0,
                                FALSE,
                                width,
                                height);
                }

            } else {
                /* draw a box around the character */
                _vte_draw_draw_rectangle(m_draw,
                 x - VTE_LINE_WIDTH,
                 y - VTE_LINE_WIDTH,
                 cursor_width + 2*VTE_LINE_WIDTH,
                                         height + 2*VTE_LINE_WIDTH,
                                         &bg, VTE_DRAW_OPAQUE);
            }

            break;
        }
    }

    cairo_restore(cr);
    _vte_draw_set_cairo(vterm_cursor.draw, nullptr);
}

static int
find_prompt_cb(gconstpointer it, gconstpointer goal)
{
    vte::grid::row_t    row_it = (vte::grid::row_t)(it),
                        row_goal = (vte::grid::row_t)(goal);

    if(row_it <= row_goal)
        return 0;

    return 1;
}

void
Terminal::prompt_marker_scroll(bool direction)
{
    if(g_queue_get_length(m_prompt_markers) == 0)
        return;

    GList* prompt_before_screen = nullptr;

    vte::grid::row_t screen_top = first_displayed_row();
    vte::grid::row_t first_prompt = (vte::grid::row_t) (g_queue_peek_tail(m_prompt_markers));

    // We look for the first prompt that is before the top of the display
    vte::grid::row_t search_prompt = MAX(first_prompt, screen_top - 1);
    prompt_before_screen = g_queue_find_custom(m_prompt_markers,
                                    (gpointer)(search_prompt), find_prompt_cb);

    if(!prompt_before_screen)
        return;

    if(direction){
        // Scroll up
        // go the first prompt before screen top
        queue_adjustment_value_changed_clamped((vte::grid::row_t)(prompt_before_screen->data));
    }else{
        // Scroll down
        if(GList* prompt_in_or_below_screen = prompt_before_screen->prev){
            // We initially decide that we will go to this prompt
            vte::grid::row_t goto_row = (vte::grid::row_t)(prompt_in_or_below_screen->data);

            if(goto_row == screen_top && prompt_in_or_below_screen->prev){
                // Unless it is already on screen top, and there is a prompt
                // after it
                goto_row = (vte::grid::row_t)(prompt_in_or_below_screen->prev->data);
            }

            queue_adjustment_value_changed_clamped(goto_row);
        }
    }
}

void
Terminal::mark_prompt_row() noexcept
{
    vte::grid::row_t new_prompt = m_screen->cursor.row;
    vte::grid::row_t head = (vte::grid::row_t)(g_queue_peek_head(m_prompt_markers));

    // Sometimes precmd is called twice filling this queue with repetitions
    if(new_prompt == head)
        return;

    // Add to head
    g_queue_push_head(m_prompt_markers, (gpointer)(new_prompt));

    // Make sure we did not exceed the limit
    if(g_queue_get_length(m_prompt_markers) > m_prompt_markers_limit)
        g_queue_pop_tail(m_prompt_markers);
}

} // namespace terminal
} // namespace vte
