/*   ANT_PAR - constants for ANT library

     History :
      23.03.1988:  original (REVAD::BDK)
      19.05.1988:  add NETDEV, MBXNAM (REVAD::BDK)
      18.04.1994:  add PORTNUM (REVAD::BDK)
      26.04.1994:  C-unix version (REVAD::BDK)
     endhistory
*/

#define ANT__MAXLINK 16    /* maximum number of network links */
#define ANT__NULL_MACH 0   /* null machine state */
#define ANT__THIS_START 1  /* partial connection started from this  end */
#define ANT__THIS_INIT 2   /* completed connection started from this end */
#define ANT__OTHER_INIT 3  /* completed connection started from other end */
#define ANT__PORTNUM 13143 /* adamnet port number */
#define ANT__NULL_T 0      /* null transaction state */
#define ANT__THIS_T 1      /* transaction partly inserted from this end */
#define ANT__OTHER_T 2     /* transaction partly inserted from other end */
#define ANT__FULL_T 3      /* transaction fully inserted */
#define ANT__NULL_P 0      /* null path state */
#define ANT__THIS_P 1      /* path partly inserted from this end */
#define ANT__OTHER_P 2     /* path partly inserted from other end */
#define ANT__FULL_P 3      /* path fully inserted */
