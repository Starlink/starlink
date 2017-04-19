/*
*+
*  Name:
*     MSP_PAR

*  Purpose:
*     .H - constants and declarations for msp

*  Language:
*     {routine_language}

*  Copyright:
*     Copyright (C) 1994 Science & Engineering Research Council. All
*     Rights Reserved.

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

*  Authors:
*     B.D.Kelly (ROE)
*     {enter_new_authors_here}

*  History:
*     15-MAR-1994 (BDK):
*        Original
*     22-MAR-1994 (BDK):
*        Add definition of null send and receive queues
*     25-MAR-1994 (BDK):
*        Add MSP__MXMSGSZ
*     19-APR-2017 (DSB):
*        Increase MSP__MXMSGSZ from 2036 to 3536. The increase (1500)
*        is the same as the corresponding increases made to
*        MESSYS__VAL_LEN and MSG_VAL_LEN, in order to allow longer
*        command lines to be used.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

/*   Maximum message size */

#define MSP__MXMSGSZ 3536

/*   definition of queue identifier for sending messages */

typedef struct msp_sendq_type {
   short connection;              /* socket to the other task */
   short ack_queue;               /* receive queue in the other task */
   } sendq_type;

static sendq_type MSP__NULL_SENDQ = {-1,-1};

/*   definition of queue identifier for receiving messages */

typedef int receiveq_type;        /* receive queue in this task */

#define MSP__NULL_RECEIVEQ (receiveq_type)(-1)
