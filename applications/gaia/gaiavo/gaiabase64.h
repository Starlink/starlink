/*
 * Copyright (C), 2000-2007 by the monit project group.
 * Copyright (C), 2008 Science and Technology Facilities Council.
 * All Rights Reserved.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */


#ifndef BASE64_H
#define BASE64_H

namespace gaia {
    bool decode_base64( const char *src, char *dest, size_t *dlength );
    char *encode_base64( int size, char *src );
}
#endif
