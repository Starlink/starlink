/*   MSP_PAR.H - constants and declarations for msp
    Authors :
     B.D.Kelly (ROE)
    History :
     15Mar 1994: original (BDK)
     22Mar 1994: add definition of null send and receive queues (BDK)
     25Mar 1994: add MSP__MXMSGSZ (BDK)
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
