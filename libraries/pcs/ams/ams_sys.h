/*  ams_sys.h - Include file for System constants for ams library


   Authors :
    B.D.Kelly (ROE)
   History :
    12.01.1996: original (BDK)
*/

#define C_LOC_ACK_IN 			1
#define C_LOC_ACK_IN_LEN 		(3 * INT_BYTE_SIZE)

#define C_LOC_GSOC_START_IN 		2
#define C_LOC_GSOC_START_IN_LEN 	((5 * INT_BYTE_SIZE) + MSG_NAME_LEN \
					+ MSG_VAL_LEN)

#define C_LOC_MSG_IN 			4
#define C_LOC_MSG_IN_LEN 		((6 * INT_BYTE_SIZE) + MSG_NAME_LEN \
					+ MSG_VAL_LEN)

#define C_LOC_DEINIT_IN 		5
#define C_LOC_DEINIT_IN_LEN 		(2 * INT_BYTE_SIZE)

#define C_LOC_INIT_IN 			6
#define C_LOC_INIT_IN_LEN 		((2 * INT_BYTE_SIZE)  + \
					(2 * MESSYS__TNAME))

#define C_REM_ACK_IN 			7
#define C_REM_ACK_IN_LEN 		(3 * INT_BYTE_SIZE)

#define C_REM_GSOC_START_IN 		8
#define C_REM_GSOC_START_IN_LEN 	((5 * INT_BYTE_SIZE) + MSG_NAME_LEN \
					+ MSG_VAL_LEN)

#define C_REM_MSG_IN 			10
#define C_REM_MSG_IN_LEN 		((6 * INT_BYTE_SIZE) + MSG_NAME_LEN \
					+ MSG_VAL_LEN)

#define C_REM_DEINIT_IN 		11
#define C_REM_DEINIT_IN_LEN 		(2 * INT_BYTE_SIZE)

#define C_REM_INIT_IN 			12
#define C_REM_INIT_IN_LEN 		((2 * INT_BYTE_SIZE) + \
					(2 * MESSYS__TNAME) + MESSYS__MNAME)

#define C_LOC_GSOC_END_IN 		13
#define C_LOC_GSOC_END_IN_LEN		(INT_BYTE_SIZE)   /* ??????????????? */
#define C_REM_GSOC_END_IN 		14
#define C_REM_GSOC_END_IN_LEN		(INT_BYTE_SIZE)   /* ??????????????? */

#define C_REM_CALL_OUT 			15
#define C_REM_CALL_OUT_LEN 		((1 * INT_BYTE_SIZE) + MESSYS__MNAME)

#define C_REM_ACCEPT_IN 		16
#define C_REM_ACCEPT_IN_LEN 		(2 * INT_BYTE_SIZE)

#define C_LOC_ACK_OUT 			C_LOC_ACK_IN
#define C_LOC_ACK_OUT_LEN 		C_LOC_ACK_IN_LEN

#define C_LOC_GSOC_START_OUT 		C_LOC_GSOC_START_IN
#define C_LOC_GSOC_START_OUT_LEN 	C_LOC_GSOC_START_IN_LEN

#define C_LOC_MSG_OUT 			C_LOC_MSG_IN
#define C_LOC_MSG_OUT_LEN 		C_LOC_MSG_IN_LEN

#define C_LOC_DEINIT_OUT 		C_LOC_DEINIT_IN
#define C_LOC_DEINIT_OUT_LEN 		C_LOC_DEINIT_IN_LEN

#define C_LOC_INIT_OUT 			C_LOC_INIT_IN
#define C_LOC_INIT_OUT_LEN 		C_LOC_INIT_IN_LEN

#define C_REM_ACK_OUT 			C_REM_ACK_IN
#define C_REM_ACK_OUT_LEN 		C_REM_ACK_IN_LEN

#define C_REM_GSOC_START_OUT 		C_REM_GSOC_START_IN
#define C_REM_GSOC_START_OUT_LEN 	C_REM_GSOC_START_IN_LEN

#define C_REM_MSG_OUT 			C_REM_MSG_IN
#define C_REM_MSG_OUT_LEN 		C_REM_MSG_IN_LEN

#define C_REM_DEINIT_OUT 		C_REM_DEINIT_IN
#define C_REM_DEINIT_OUT_LEN 		C_REM_DEINIT_IN_LEN

#define C_REM_INIT_OUT 			C_REM_INIT_IN
#define C_REM_INIT_OUT_LEN 		C_REM_INIT_IN_LEN

#define C_LOC_GSOC_END_OUT 		C_LOC_GSOC_END_IN
#define C_REM_GSOC_END_OUT 		C_REM_GSOC_END_IN

#define C_MAXMSG_LEN C_REM_MSG_IN_LEN


/*   Maximum number of queues waited on */

#define AMS__MXQUEUE 100

