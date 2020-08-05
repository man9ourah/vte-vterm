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
    GList* prompt_row = nullptr;
    if(direction){
        // We are going up!
        // We look for the first prompt that is before the top of the display
        vte::grid::row_t top = first_displayed_row() == 0 ? 0 : first_displayed_row() - 1;

        prompt_row = g_queue_find_custom(m_prompt_markers,
                                        (gpointer)(top), find_prompt_cb);

        if(prompt_row)
            // Found it! Go there!
            queue_adjustment_value_changed_clamped((vte::grid::row_t)(prompt_row->data));
    }else{
        // We are going down!
        // We look for the first prompt *inside* the display from the bottom
        vte::grid::row_t bottom = last_displayed_row();

        // If the last prompt is already in display, dont search
        vte::grid::row_t last_prompt = (vte::grid::row_t)(g_queue_peek_head(m_prompt_markers));
        if(last_prompt < bottom)
            return;

        prompt_row = g_queue_find_custom(m_prompt_markers,
                                        (gpointer)(bottom), find_prompt_cb);

        if(prompt_row && prompt_row->prev){
            // Found it, and if there is a prompt after it, go there!
            vte::grid::row_t prompt_prev = (vte::grid::row_t)(prompt_row->prev->data);
            queue_adjustment_value_changed_clamped(prompt_prev);
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
