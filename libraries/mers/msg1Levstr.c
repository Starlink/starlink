/*
*+
*  Name:
*     msg1Levstr

*  Purpose:
*     Return string form of message level

*  Language:
*     Starlink ANSI C

*  Invocation:
*     const char * msg1Levstr( msglev_t filter );

*  Description:
*     Translates a message reporting level to a string.

*  Arguments:
*     filter = msglev_t (Given)
*        Message reporting level to translate.

*  Returned Value:
*     const char *
*        String corresponding to the messaging level. Returns NULL
*        if the level is not recognized.

*  Copyright:
*     Copyright (C) 2009 Science and Technology Facilities Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 3 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     23-JUL-2009 (TIMJ):
*        Initial version
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

#include "mers1.h"

const char * msg1Levstr( msglev_t filter ) {
  const char * retval = NULL;

  switch ( filter ) {
  case MSG__NONE:
    retval = "NONE";
    break;
  case MSG__QUIET:
    retval = "QUIET";
    break;
  case MSG__NORM:
    retval = "NORMAL";
    break;
  case MSG__VERB:
    retval = "VERBOSE";
    break;
  case MSG__DEBUG:
    retval = "DEBUG";
    break;
  case MSG__DEBUG1:
    retval = "DEBUG1";
    break;
  case MSG__DEBUG2:
    retval = "DEBUG2";
    break;
  case MSG__DEBUG3:
    retval = "DEBUG3";
    break;
  case MSG__DEBUG4:
    retval = "DEBUG4";
    break;
  case MSG__DEBUG5:
    retval = "DEBUG5";
    break;
  case MSG__DEBUG6:
    retval = "DEBUG6";
    break;
  case MSG__DEBUG7:
    retval = "DEBUG7";
    break;
  case MSG__DEBUG8:
    retval = "DEBUG8";
    break;
  case MSG__DEBUG9:
    retval = "DEBUG9";
    break;
  case MSG__DEBUG10:
    retval = "DEBUG10";
    break;
  case MSG__DEBUG11:
    retval = "DEBUG11";
    break;
  case MSG__DEBUG12:
    retval = "DEBUG12";
    break;
  case MSG__DEBUG13:
    retval = "DEBUG13";
    break;
  case MSG__DEBUG14:
    retval = "DEBUG14";
    break;
  case MSG__DEBUG15:
    retval = "DEBUG15";
    break;
  case MSG__DEBUG16:
    retval = "DEBUG16";
    break;
  case MSG__DEBUG17:
    retval = "DEBUG17";
    break;
  case MSG__DEBUG18:
    retval = "DEBUG18";
    break;
  case MSG__DEBUG19:
    retval = "DEBUG19";
    break;
  case MSG__DEBUG20:
    retval = "DEBUG20";
    break;
  case MSG__ALL:
    retval = "ALL";
    break;
  }

  return retval;

}
