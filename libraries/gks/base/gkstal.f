C# IL>=a, OL>=0
      SUBROUTINE GKSTAL ( ITYPE, ISIZE, ISET )
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
*      This subroutine provides stack space for short-term storage
*      of reals or integers.
*
*  MAINTENANCE LOG
*  ---------------
*     01/01/83  RCS   Original version stabilized
*     25/05/83  CJW   "Support" character heap - but is of zero size.
*                     Now includes GKHP.PAR to get KCHARS (=2). Always
*                     faults ITYPE = KCHARS by saying that there is not
*                     enough room
*     06/06/83  AS    Match parentheses on first line of code
*     30/06/83  PGLS  Pass back response in KERROR.
*     10/08/83  AS    Change subroutine name from GKALST
*     22/01/87  JCS   IS conversion. Error number change.
*
*  ARGUMENTS
*  ---------
*     INP   ISIZE  the number of integers or reals to allocate.
*     INP   ITYPE  0 = real,  1 = integer - use GKHP.PAR
*     OUT   ISET   the offset from the stack to the start of the allocated area
*
      INTEGER  ISIZE, ITYPE, ISET
*
*  COMMON BLOCK USAGE
*  ------------------
*     Modify /STK/    Assorted stack variables
*
      INCLUDE '../include/gkhp.par'
      INCLUDE '../include/gkstk.cmn'
      INCLUDE '../include/gkerr.cmn'
*
*  ERRORS
*  ------
*     300   Not enough stack available
*
*  ALGORITHM
*  ---------
*      A large area is provided by real array ,for the storage
*      of reals ("QSTACK"), and an integer array for the storage of
*      integers ("KSTACK").
*      These arrays have the (integer) subscripts "KIUSED" and
*      "KRUSED", respectively.
*
*      In this subroutine, a  large common area is provided by the
*      EQUIVALENCE of KSTACK with QSTACK.
*      These arrays both have the (integer) subscript "KIUSED",
*      since the subscripts KRUSED and KIUSED are also equivalenced.
*
*      KIUSED holds the address of the top of the stack.
*
*      The integer returned argument "ISET" is the offset from KSTACK
*      or QSTACK of the start of the allocated area.
*      This integer is saved in the integer/real array element:
*
*        KSTACK (KIUSED )    [~ QSTACK (KRUSED) for reals ],
*      after each store allocation has been made.
*
*
*      The next available store space is thus in the array element:
*
*            KSTACK (KIUSED + 1)        [~ QSTACK (KRUSED+1) for reals ] .
*      In this way we obtain a collection of pointers, each marking
*      the end of a store request.
*
*
*      This type of stack was chosen because it will facilitate the making of
*      a safety check when using the subroutine KSTDA, ( a subroutine to clear
*      unwanted store) without using additional arrays or subroutine arguments.
*
*
*     The PROGRAM
*     -----------
*
*      A check is made to see whether there is enough  space in the stack for
*      the user's requested  store, of size "ISIZE".
*      If there is enough space, [i.e.  KIUSED + ISIZE < KSTK  ]
*      the store will be allocated as follows :
*
*      (1) ISET is assigned the value  "KIUSED + 1 " ;
*
*      (2) The address KIUSED is appropriately updated:
*
*                KIUSED = KIUSED + ISIZE + 1
*         [this statement creates store space of size ISIZE,
*          with one unit of store reserved for the marker ISET];
*
*
*      If, however, the request is too large,
*      KERROR is set to the value 301, and ISET with the value  -1.
*      (The latter assignment is made to simplify programming for callers.)
*
*
*      The store array dimension is given by a  PARAMETER  "KSTK"
*      in the stack common block file.
*
*
*
*     EXAMPLE
*     -------
*
*      Starting with no allocations ( KIUSED=0 and KRUSED=0 ),
*      a request of 17 units is allocated.
*      This is permissible.
*      The offset marker ISET=1 is stored in the 18th unit;
*      KIUSED is updated to 18; and ISET will be 19 next time.
*
*      Another request of 6 units is allocated.
*      Once again, this is permissible.
*      ISET=19 is stored in the 25th unit; and KIUSED then becomes 26.
*      The next value of ISET will be 26.
*      A request for 2,000,000  units is not permissable,
*      so KERROR is set to 300.
*
*
*                                    ISET=19           ISET=26
*                                       .                  .
*                                       .                  .
*                                       .                  . ISIZE=
*                                       .                  . 2,000,000 ?
*                                       .                  .
*                                                          . KERROR=300
*          <       ISIZE =  17      >   .<  ISIZE = 6  >   .
*         ______________________________________________________________
*         |                          |  |               |  |           /
*         |                          |  |               |  |          /
*         |                          | 1|               |19| FREE    /
*         |                          |  |               |  |        /
*         |__________________________|__|_______________|__|_______/
*       ^ ^                          .                  .   ^
*       . .                          .                  .   ^
*       . .                          .                  .   Units
*       . .                          .                  .   26 to KSTK-1
*       . KIUSED =1                  .                  .   are free for
*       .                            .                  .   allocation.
*       .                            .                  .
*     KIUSED = 0         KIUSED  =  18                 25
*
*
*        In order to deallocate his store completely,
*        the user sets KIUSED and KRUSED to zero .
*
*  COMMENTS
*  --------
*     (1) The user is advised to release a store area immediately he
*         has finished with it. All stack space must be explicitly
*         released.
*
*     (2) The "stack used" variables, KIUSED and KRUSED,
*         should be set to zero on OPEN GKS.
*
*     (3) Succesive calls to allocate space will not give
*         contiguous areas.
*---------------------------------------------------------------------

      IF ( ((KIUSED+ISIZE).LT.KSTK) .AND. (ITYPE.NE.KCHARS) )  THEN
          ISET = KIUSED + 1
          KIUSED = KIUSED + ISIZE + 1
          KSTACK (KIUSED) = ISET
      ELSE
*(The request is too large or asked for characters.)
          KERROR = 300
          ISET = -1
*(Note that ISET is set to -1.)

      ENDIF

      END
