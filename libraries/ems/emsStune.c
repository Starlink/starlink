/*
 *+
 *  Name:
 *     emsStune

 *  Purpose:
 *     Set an EMS tuning parameter

 *  Language:
 *     Starlink ANSI C

 *  Invocation:
 *     oldvalue = emsTune( key, value, status )

 *  Arguments:
 *     key = char* (Given)
 *        The name of the tuning parameter to be set
 *     value = int (Given)
 *        The desired value (see description).
 *     status = int* (Given and Returned)
 *        The global status.

 *  Result:
 *     The previous value of the tuning parameter.

 *  Description:
 *     The value of the EMS tuning parameter is set appropriately, according
 *     to the value given. ems1Tune may be called multiple times for the same
 *     parameter. The following keywords and values are permitted:
 *
 *        'SZOUT' Specifies a maximum line length to be used in the line
 *            wrapping process. By default the message to be output is split
 *            into chunks of no more than the maximum line length, and each
 *            chunk is written on a new line. The split is made at word
 *            boundaries if possible. The default maximum line length is 79
 *            characters.
 *
 *            If VALUE is set to 0, no wrapping will occur (that is the
 *            length will be set to the maximum possible). Note that the
 *            minimum is 7, to allow for exclamation marks and indentation.
 *            Using a smaller value is an error.
 *
 *        'MSGDEF' Specifies the default error reporting level. That is a level
 *            below which EMS_RELEASE will not go. It can therefore be used by
 *            environments such as ADAM to prevent any output by EMS due to
 *            unmatched marks and releases. This keyword should not be used in
 *            other cases.
 *
 *        'STREAM' Specifies whether or not ERR should treat its output
 *            unintelligently as a stream of characters. If VALUE is set to 0
 *            (the default) all non-printing characters are replaced by
 *            blanks, and line wrapping occurs (subject to SZOUT). If VALUE is
 *            set to 1, no cleaning or line wrapping occurs.
 *
 *        'REVEAL' Allows the user to display all error messages cancelled
 *            when EMS_ANNUL is called. This is a diagnostic tool which enables
 *            the programmer to see all error reports, even those 'handled'
 *            by the program. If VALUE is set to 0 (the default) annulling
 *            occurs in the normal way. If VALUE is set to 1, the message
 *            will be displayed.
 *
 *     The routine will attempt to execute regardless of the given value of
 *     STATUS. If the given value is not SAI__OK, then it is left unchanged,
 *     even if the routine fails to complete. If the STATUS is SAI__OK on
 *     entry and the routine fails to complete, STATUS will be set and an
 *     error report made.

 *  Notes:
 *     1. SZOUT applies to the case where EMS does its own output - i.e. when
 *        messages are reported or impending when EMS is at its base level.
 *        Normally output will be from the ERR package and is tuned by
 *        ERR_TUNE.
 *
 *     2. Some aspects of output for both ERR and MSG are controlled by EMS and
 *        its tuning parameters therefore ERR_TUNE and MSG_TUNE call this
 *        subroutine to set the EMS tuning parameters appropriately. This may
 *        result in interference. A strategy to stop this from happening is
 *        to use the result from this function to wrap EMS calls in set tune
 *        /restore tune blocks.
 *
 *     3. The use of SZOUT and STREAM may be affected by the message delivery
 *        system in use. For example there may be a limit on the the size of a
 *        line output by a Fortran WRITE and automatic line wrapping may occur.
 *        In particular, a NULL character will terminate a message delivered by
 *        the ADAM message system.

 *  Copyright:
 *     Copyright (C) 1999, 2001 Central Laboratory of the Research Councils.
 *     Copyright (C) 2008 Science and Technology Facilities Council.
 *     All Rights Reserved.

 *  Licence:
 *     This program is free software; you can redistribute it and/or
 *     modify it under the terms of the GNU General Public License as
 *     published by the Free Software Foundation; either version 2 of
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
 *     AJC: A.J. Chipperfield (STARLINK)
 *     RTP: R.T.Platon (STARLINK)
 *     PWD: Peter W. Draper (JAC, Durham University)
 *     {enter_new_authors_here}

 *  History:
 *     22-JUL-1999 (AJC):
 *        Original FORTRAN version.
 *     14-FEB-2001 (RTP):
 *        Rewritten in C from Fortran Routine EMS1_TUNE.
 *      6-MAR-2001 (AJC):
 *        Make public emsTune
 *        Added MSGDEF keyword
 *     13-AUG-2001 (AJC):
 *        Remove unused variables
 *     13-MAY-2008 (PWD):
 *        Use struct to access message table.
 *     30-JUL-2008 (PWD):
 *        Limit SZOUT to EMS__SZMSG as this is the maximum size
 *        EMS will output. Make sure SZOUT is greater than 6 to
 *        also match the documented behaviour.
 *     31-JUL-2008 (PWD):
 *        Renamed from emsTune and added a return value.
 *     {enter_changes_here}

 *  Bugs:
 *     {note_any_bugs_here}

 *-
 */

#include <strings.h>

#include "sae_par.h"                 /* Standard SAE constants */
#include "ems_par.h"                 /* EMS_ public constants */
#include "ems_sys.h"                 /* EMS_ private constants */
#include "ems_err.h"                 /* EMS_ EMS_ error codes*/
#include "ems.h"                     /* EMS_ function prototypes */
#include "ems1.h"                    /* EMS_ private functions prototypes */
#include "ems_defs.h"                /* EMS_ message table */

int emsStune( const char *key, const int value, int *status )
{
    int lstat;                /* Local status */
    int old_value;            /* Old value for parameter. */

    ems_msgtab_t *msgtab = ems1Gmsgtab();  /* Current message table */

    TRACE( "emsTune" );

    /*  Initialise local status */
    lstat = SAI__OK;
    old_value = 0;

    /*  Check that the given KEY is acceptable. */
    if ( strcasecmp( key, "SZOUT" ) == 0 ) {
        old_value = msgtab->msgwsz;
        if ( value == 0 ) {
            msgtab->msgwsz = EMS__SZMSG;
        } else if ( value > 6 ) {
            msgtab->msgwsz = MIN( value, EMS__SZMSG );
        } else {
            lstat = EMS__BTUNE;
        }

    } else if ( strcasecmp( key, "STREAM" ) == 0 ) {
        old_value = msgtab->msgstm;
        if ( value == 0 ) {
            msgtab->msgstm = FALSE;
        } else if ( value == 1 ) {
            msgtab->msgstm = TRUE;
        } else {
            lstat = EMS__BTUNE;
        }

    } else if ( strcasecmp( key, "MSGDEF" ) == 0 ) {
        old_value = msgtab->msgdef;
        if ( ( value < EMS__MXLEV ) && ( value >= EMS__BASE ) ) {
            msgtab->msgdef = value;
        } else {
            lstat = EMS__BTUNE;
        }

    } else if ( strcasecmp( key, "REVEAL" ) == 0 ) {
        old_value = msgtab->msgrvl;
        if ( value == 0 ) {
            msgtab->msgrvl = FALSE;
        } else if ( value == 1 ) {
            msgtab->msgrvl = TRUE;
        } else {
            lstat = EMS__BTUNE;
        }

    } else {
        /*  The given tuning parameter was not in the available set. Set
         *  status and report an error message. We mark and rlse to prevent
         *  possible token name clash */
        emsMark();
        lstat = EMS__BDKEY;
        emsSetc( "KEY", key );
        emsRep( "EMS_TUNE_INV", "EMS_TUNE: Invalid tuning parameter: ^KEY",
                &lstat );
        emsRlse();
    }

    if ( lstat == EMS__BTUNE ) {
        /*  The given tuning parameter value was invalid, report a message.
         *  We mark and rlse to prevent possible token name clash. */
        emsMark();
        emsSetc( "KEY", key );
        emsSeti( "VALUE", value );
        emsRep( "EMS_TUNE_INV", "EMS_TUNE: ^KEY value ^VALUE invalid", &lstat );
        emsRlse();
    }

    /*  Set return status */
    if ( *status == SAI__OK ) *status = lstat;

    return old_value;
}
