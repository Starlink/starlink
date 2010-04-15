/*
*+
*  Name:
*     ANT_PAR

*  Purpose:
*     Constants for ANT library

*  Language:
*     {routine_language}

*  Authors:
*      BDK: Dennis Kelly (ROE)
*     {enter_new_authors_here}

*  History:
*     23-MAR-1988 (REVAD::BDK):
*        Original
*     19-MAY-1988 (REVAD::BDK):
*        Add NETDEV, MBXNAM
*     18-APR-1994 (REVAD::BDK):
*        Add PORTNUM
*     26-APR-1994 (REVAD::BDK):
*        C-unix version
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
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
