C# IL>=a, OL>=0
      SUBROUTINE GKSTDA (ITYPE, ISET)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    Utility
*  Author:             RCS
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*      This subroutine releases integer or real stack space that was allocated
*      by the stack-space allocating subroutine GKSTAL.
*
*  MAINTENANCE LOG
*  ---------------
*     01/01/83  RCS   Original version stabilized
*     04/06/83  AS    Put in calls to GKBUG
*     10/08/83  AS    Change name from GKDEAL
*     22/01/87  JCS   IS conversion. Error number change.
*
*  ARGUMENTS
*  ---------
*     INP   ITYPE 0 = real,  1 = integer
*     INP   ISET  the offset from the stack to the start of the allocated area.
*
      INTEGER  ITYPE, ISET
*
*  COMMON BLOCK USAGE
*  ------------------
*     Modify /STK/    Assorted stack variables
*
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gkstk.cmn'
*
*  ERRORS
*  ------
*     Errors result from logical errors by the caller which are bugs.
*
*  ALGORITHM
*  ---------
*      Subroutine GKSTAL was designed for the purpose of
*      building in a safety check in this subroutine.
*      This check means that a program cannot release any stack
*      other than that LAST provided by GKSTAL.
*      I.e. this subroutine will only free the TOP block of store on
*           the stack "KSTACK" (or "QSTACK").
*
*      This is done by checking that the supplied offset ISET
*      is the same as its current value, which is held at the very top
*      of the stack as the element  KSTACK (KIUSED).
*
*      If this test is passed, we must update KIUSED  to the value
*      that is  KIUSED  less the size of the last block of store .
*                       ----
*      This is given by :
*                KIUSED = ISET  - 1  .
*
*      If ISET is not the required value
*      (i.e. we are trying to delete store that was not allocated just before
*      the position of the current marker ISET),
*      an error message will be output and the subroutine will
*      terminate the user's  program (see below).
*
*
*     EXAMPLE
*     -------
*     Consider the following status of the stack KSTACK
*     after the example given in GKSTAL .
*
*
*
*
*                                  ISET=19           ISET=26
*                                     .                  .
*                                     .                  .
*                                     .                  .
*                                     .                  .
*                                     .                  .
*                                     .                  .
*        <       ISIZE =  17      >   .<  ISIZE = 6  >   .
*       ____________________________________________________________
*       |                          |  |               |  |
*       |                          |  |               |  |
*       |                          | 1|               |19| FREE
*       |                          |  |               |  |
*       |__________________________|__|_______________|__|_______
*                                                     .   ^
*                                                     .   ^
*                                                     .   Units
*                                                     .   26 to KSTK-1
*                                                     .   are free for
*                                                     .   allocation.
*
*                                               KIUSED= 25
*
*
*
*
*     Suppose the user has no further use for his last store (of size 6).
*     To deallocate this store he must call GKSTDA with argument ISET=19.
*     Any other value of ISET will result in an error message,
*     and the program using KSTDA will be terminated.
*
*     Correct use of the subroutine will give :
*
*
*
*
*        <      ISIZE =  17       >
*       ______________________________________________________
*       |                          |  |
*       |                          |  |
*       |                          | 1|       FREE
*       |                          |  |
*       |__________________________|__|_______________________
*                                  .   ^
*                                  .   ^
*                                  .   Units
*                                  .   19 to KSTK-1
*                                  .   are free for
*                                  .   allocation.
*
*                            KIUSED = 18
*
*
*
*     If the user wishes to totally deallocate his store,
*     he must employ KSTDA with ISET = 1 . ( This would leave  KIUSED = 0.)
*
*---------------------------------------------------------------------
*
      IF (ISET.NE.KNIL) THEN

         IF ( KSTACK(KIUSED) .EQ. ISET ) THEN
            KIUSED = ISET - 1
         ELSE
           CALL GKBUG(-2008,'GKSTDA')
         ENDIF

      ENDIF


      END
