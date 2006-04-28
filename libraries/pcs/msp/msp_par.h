/*
*+
*  Name:
*     MSP_PAR

*  Purpose:
*     .H - constants and declarations for msp 

*  Language:
*     {routine_language}

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
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

/*   Maximum message size */

#define MSP__MXMSGSZ 2036

/*   definition of queue identifier for sending messages */

typedef struct msp_sendq_type {
   short connection;              /* socket to the other task */
   short ack_queue;               /* receive queue in the other task */
   } sendq_type;

static sendq_type MSP__NULL_SENDQ = {-1,-1};

/*   definition of queue identifier for receiving messages */

typedef int receiveq_type;        /* receive queue in this task */

#define MSP__NULL_RECEIVEQ (receiveq_type)(-1)
