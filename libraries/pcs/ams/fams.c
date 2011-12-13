/*****************************************************************************
 *+
 * Name:
 *    fams.c
 *
 * Function:
 *    A Fortran callable interface to the ADAM message system AMS.
 *
 * Authors:
 *    BDK: B.D.Kelly (ROE)
 *    AJC: A.J.Chipperfield (AJC)
 *    TIMJ: Tim Jenness (JAC, Hawaii)
 *
 * History:
 *    xx-xxx-1994 (BDK):
 *       Original version
 *    17-NOV-1994 (AJC)
 *       Test used length of imported Fortran strings
 *     5-DEC-1994 (AJC):
 *       Remove path from includes
 *     3-SEP-2004 (TIMJ):
 *       Fix compiler warning from undefined strlen
 *
 * Description:
 *    Uses the CNF interfaces to provide a portable interface for the
 *    routines: ams_astint
 *              ams_astmsg
 *              ams_extint
 *              ams_getreply
 *              ams_init
 *              ams_kick
 *              ams_path
 *              ams_plookup
 *              ams_receive
 *              ams_reply
 *              ams_resmsg
 *              ams_send

 *  Copyright:
 *     Copyright (C) 1994 Science & Engineering Research Council.
 *     Copyright (C) 2004 Central Laboratory of the Research Councils.
 *     All Rights Reserved.

 *  Licence:
 *     This program is free software; you can redistribute it and/or
 *     modify it under the terms of the GNU General Public License as
 *     published by the Free Software Foundation; either version 2 of
 *     the License, or (at your option) any later version.
 *
 *     This program is distributed in the hope that it will be
 *     useful, but WITHOUT ANY WARRANTY; without even the implied
 *     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
 *     PURPOSE. See the GNU General Public License for more details.
 *
 *     You should have received a copy of the GNU General Public License
 *     along with this program; if not, write to the Free Software
 *     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
 *     02110-1301, USA

 *
 * Implementation Deficiencies:
 *    1. Returned message values are not blank filled so the returned
 *       length must be used.
 *    2. MSG_NAME_LEN etc are defined the same value in C as in Fortran
 *       but C has to include the null terminator. Therefore a size -1
 *       length is imposed on the Fortran string used length.
 *-
 */
#  include "cnf.h"
#  include "f77.h"

#include <string.h>
#include "sae_par.h"
#include "ams.h"
#include "messys_par.h"
#include "messys_err.h"
#include "messys_len.h"



/*+  FAMS_ASTINT */

F77_SUBROUTINE(fams_astint)
(
INTEGER(status)
)

{
   GENPTR_INTEGER(status)

   if ( *status != SAI__OK ) return;

   ams_astint ( status );
}

/*+  FAMS_ASTMSG */

F77_SUBROUTINE(fams_astmsg)
(
CHARACTER(name),
INTEGER(length),
CHARACTER(value),
INTEGER(status)
TRAIL(name)
TRAIL(value)
)

{
   GENPTR_CHARACTER(name)
   GENPTR_INTEGER(length)
   GENPTR_CHARACTER(value)
   GENPTR_INTEGER(status)

   char cname[MSG_NAME_LEN];

   if ( *status != SAI__OK ) return;

   if ( cnf_lenf( name, name_length ) < MSG_NAME_LEN )
   {
      cnf_imprt ( name, name_length, cname );
      ams_astmsg ( cname, *length, value, status );
   }
   else
   {
      *status = MESSYS__TOOLONG;
   }

}

/*+  FAMS_EXTINT */

F77_SUBROUTINE(fams_extint)
(
INTEGER(status)
)

{
   GENPTR_INTEGER(status)

   if ( *status != SAI__OK ) return;

   ams_extint ( status );
}

/*+  FAMS_GETREPLY */

F77_SUBROUTINE(fams_getreply)
(
INTEGER(timeout),
INTEGER(path),
INTEGER(messid),
INTEGER(message_status),
INTEGER(message_context),
CHARACTER(message_name),
INTEGER(message_length),
CHARACTER(message_value),
INTEGER(status)
TRAIL(message_name)
TRAIL(message_value)
)
{
   GENPTR_INTEGER(timeout)
   GENPTR_INTEGER(path)
   GENPTR_INTEGER(messid)
   GENPTR_INTEGER(message_status)
   GENPTR_INTEGER(message_context)
   GENPTR_CHARACTER(message_name)
   GENPTR_INTEGER(message_length)
   GENPTR_CHARACTER(message_value)
   GENPTR_INTEGER(status)

   char cname[MSG_NAME_LEN];
   char cmessage_value[MSG_VAL_LEN];

   if ( *status != SAI__OK ) return;

   ams_getreply ( *timeout, *path, *messid, message_name_length,
     message_value_length, message_status, message_context, cname,
     message_length, message_value, status );
   cnf_exprt ( cname, message_name, message_name_length );
}

/*+  FAMS_INIT */

F77_SUBROUTINE(fams_init)
(
CHARACTER(own_name),
INTEGER(status)
TRAIL(own_name)
)

{
   GENPTR_CHARACTER(own_name)
   GENPTR_INTEGER(status)

   char cown_name[MESSYS__TNAME];

   if ( *status != SAI__OK ) return;

   if ( cnf_lenf( own_name, own_name_length) < MESSYS__TNAME )
   {
      cnf_imprt ( own_name, own_name_length, cown_name );
      ams_init ( cown_name, status );
   }
   else
   {
      *status = MESSYS__TOOLONG;
   }
}

/*+  FAMS_KICK */

F77_SUBROUTINE(fams_kick)
(
CHARACTER(name),
INTEGER(length),
CHARACTER(value),
INTEGER(status)
TRAIL(name)
TRAIL(value)
)
{
   GENPTR_CHARACTER(name)
   GENPTR_INTEGER(length)
   GENPTR_CHARACTER(value)
   GENPTR_INTEGER(status)

   char cname[MSG_NAME_LEN];

   if ( *status != SAI__OK ) return;

   if ( cnf_lenf ( name, name_length ) < MSG_NAME_LEN )
   {
      cnf_imprt ( name, name_length, cname );
      ams_kick ( cname, *length, value, status );
   }
   else
   {
      *status = MESSYS__TOOLONG;
   }
}

/*+  FAMS_PATH */

F77_SUBROUTINE(fams_path)
(
CHARACTER(other_task_name),
INTEGER(path),
INTEGER(status)
TRAIL(other_task_name)
)
{
   GENPTR_CHARACTER(other_task_name)
   GENPTR_INTEGER(path)
   GENPTR_INTEGER(status)

   char cother_task_name[MESSYS__TNAME];

   if ( *status != SAI__OK ) return;

   if ( cnf_lenf( other_task_name, other_task_name_length ) < MESSYS__TNAME )
   {
      cnf_imprt ( other_task_name, other_task_name_length, cother_task_name );
      ams_path ( cother_task_name, path, status );
   }
   else
   {
      *status = MESSYS__TOOLONG;
   }
}

/*+  FAMS_PLOOKUP */

F77_SUBROUTINE(fams_plookup)
(
INTEGER(path),
CHARACTER(name),
INTEGER(status)
TRAIL(name)
)
{
   GENPTR_INTEGER(path)
   GENPTR_CHARACTER(name)
   GENPTR_INTEGER(status)

   char cname[MESSYS__TNAME];

   if ( *status != SAI__OK ) return;


   ams_plookup ( *path, cname, status );

   if ( strlen ( cname ) <= name_length )
   {
      cnf_exprt ( cname, name, name_length );
   }
   else if ( *status == SAI__OK )
   {
      *status = MESSYS__TOOLONG;
   }
}

/*+  FAMS_RECEIVE */

F77_SUBROUTINE(fams_receive)
(
INTEGER(timeout),
INTEGER(message_status),
INTEGER(message_context),
CHARACTER(message_name),
INTEGER(message_length),
CHARACTER(message_value),
INTEGER(path),
INTEGER(messid),
INTEGER(status)
TRAIL(message_name)
TRAIL(message_value)
)
{
   GENPTR_INTEGER(timeout)
   GENPTR_INTEGER(message_status)
   GENPTR_INTEGER(message_context)
   GENPTR_CHARACTER(message_name)
   GENPTR_INTEGER(message_length)
   GENPTR_CHARACTER(message_value)
   GENPTR_INTEGER(path)
   GENPTR_INTEGER(messid)
   GENPTR_INTEGER(status)

   char cname[MSG_NAME_LEN];         /* message name */


   if ( *status != SAI__OK ) return;

   ams_receive ( *timeout, MSG_NAME_LEN, message_value_length,
     message_status, message_context, cname, message_length,
     message_value, path, messid, status );
   cnf_exprt ( cname, message_name, message_name_length );

}

/*+  FAMS_REPLY */

F77_SUBROUTINE(fams_reply)
(
INTEGER(path),
INTEGER(messid),
INTEGER(message_function),
INTEGER(message_status),
INTEGER(message_context),
CHARACTER(message_name),
INTEGER(message_length),
CHARACTER(message_value),
INTEGER(status)
TRAIL(message_name)
TRAIL(message_value)
)
{
   GENPTR_INTEGER(path)
   GENPTR_INTEGER(message_function)
   GENPTR_INTEGER(message_status)
   GENPTR_INTEGER(message_context)
   GENPTR_CHARACTER(message_name)
   GENPTR_INTEGER(message_length)
   GENPTR_CHARACTER(message_value)
   GENPTR_INTEGER(messid)
   GENPTR_INTEGER(status)

   char cname[MSG_NAME_LEN];           /* message name */


   if ( *status != SAI__OK ) return;

   if ( cnf_lenf( message_name, message_name_length ) < MSG_NAME_LEN )
   {

      cnf_imprt ( message_name, message_name_length, cname );
      ams_reply ( *path, *messid, *message_function, *message_status,
        *message_context, cname, *message_length, message_value,
        status );
   }
   else
   {
      *status = MESSYS__TOOLONG;
   }
}

/*+  FAMS_RESMSG */

F77_SUBROUTINE(fams_resmsg)
(
INTEGER(length),
CHARACTER(value),
INTEGER(status)
TRAIL(value)
)

{
   GENPTR_INTEGER(length)
   GENPTR_CHARACTER(value)
   GENPTR_INTEGER(status)

   if ( *status != SAI__OK ) return;

   ams_resmsg ( *length, value, status );

}

/*+  FAMS_SEND */

F77_SUBROUTINE(fams_send)
(INTEGER(path),
INTEGER(message_function),
INTEGER(message_status),
INTEGER(message_context),
CHARACTER(message_name),
INTEGER(message_length),
CHARACTER(message_value),
INTEGER(messid),
INTEGER(status)
TRAIL(message_name)
TRAIL(message_value)
)
{
   GENPTR_INTEGER(path)
   GENPTR_INTEGER(message_function)
   GENPTR_INTEGER(message_status)
   GENPTR_INTEGER(message_context)
   GENPTR_CHARACTER(message_name)
   GENPTR_INTEGER(message_length)
   GENPTR_CHARACTER(message_value)
   GENPTR_INTEGER(messid)
   GENPTR_INTEGER(status)

   char cname[MSG_NAME_LEN];          /* message name */

   if ( *status != SAI__OK ) return;

   if ( cnf_lenf( message_name, message_name_length ) < MSG_NAME_LEN )
   {

      cnf_imprt ( message_name, message_name_length, cname );
      ams_send ( *path, *message_function, *message_status, *message_context,
        cname, *message_length, message_value, messid, status );
   }
   else
   {
      *status = MESSYS__TOOLONG;
   }
}

