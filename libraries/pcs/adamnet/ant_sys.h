/*   ANT_SYS -  system constants for ANT library */

/*   History :
      22.03.1988:  original (REVAD::BDK)
      25.03.1988: revise CALL and ACCEPT names (REVAD::BDK)
      31.03.1988: add GSOC_END IN and OUT (REVAD::BDK)
      28.04.1988: fix CALL, INIT, ACCEPT _IN_LEN (REVAD::BDK)
      19.05.1988: remove GSOC_ACK add MAXMSG_LEN (REVAD::BDK)
      26.04.1994: C-unix version (REVAD::BDK)
     endhistory
*/


/*   Parameters for incoming messages */

#define C_NET_ACK_IN 1
#define C_NET_ACK_IN_LEN 12
#define C_NET_DEINIT_IN 2 
#define C_NET_DEINIT_IN_LEN 8
#define C_NET_GSOC_START_IN 3 
#define C_NET_GSOC_START_IN_LEN ( 20 + MSG_NAME_LEN + MSG_VAL_LEN )
#define C_NET_MSG_IN 5 
#define C_NET_MSG_IN_LEN ( 24 + MSG_NAME_LEN + MSG_VAL_LEN )
#define C_NET_CALL_IN 6 
#define C_NET_CALL_IN_LEN ( 8 + 2 * MESSYS__MNAME )
#define C_NET_INIT_IN 7 
#define C_NET_INIT_IN_LEN ( 16 + 2 * MESSYS__TNAME + 2 * MESSYS__MNAME )
#define C_NET_ACCEPT_IN 8 
#define C_NET_ACCEPT_IN_LEN ( 12 + 2 * MESSYS__MNAME )
#define C_NET_GSOC_END_IN 9

/*   Constants for outgoing messages */

#define C_NET_ACK_OUT ( C_NET_ACK_IN )
#define C_NET_ACK_OUT_LEN ( C_NET_ACK_IN_LEN )
#define C_NET_DEINIT_OUT ( C_NET_DEINIT_IN )
#define C_NET_DEINIT_OUT_LEN ( C_NET_DEINIT_IN_LEN )
#define C_NET_GSOC_START_OUT ( C_NET_GSOC_START_IN )
#define C_NET_GSOC_START_OUT_LEN ( C_NET_GSOC_START_IN_LEN )
#define C_NET_MSG_OUT ( C_NET_MSG_IN )
#define C_NET_MSG_OUT_LEN ( C_NET_MSG_IN_LEN )
#define C_NET_CALL_OUT ( C_NET_CALL_IN )
#define C_NET_CALL_OUT_LEN ( C_NET_CALL_IN_LEN )
#define C_NET_INIT_OUT ( C_NET_INIT_IN )
#define C_NET_INIT_OUT_LEN ( C_NET_INIT_IN_LEN )
#define C_NET_ACCEPT_OUT ( C_NET_ACCEPT_IN )
#define C_NET_ACCEPT_OUT_LEN ( C_NET_ACCEPT_IN_LEN )
#define C_NET_GSOC_END_OUT ( C_NET_GSOC_END_IN )

/*   maximum message size = actual size of all messages passed across the 
     network */

#define C_NET_MAXMSG_LEN 2048
