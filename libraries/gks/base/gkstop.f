C# IL>=a, OL>=0
      SUBROUTINE GKSTOP
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    Utility
*  Author:             CJW
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*      Open stack ready for use.
*
*  MAINTENANCE LOG
*  ---------------
*     13/06/83  CJW  Original version stabilized
*     27/06/83  CJW  Implement revised error handling precedure
*                    (No change required)
*
*  COMMON BLOCK USAGE
*  ------------------
*     Modify /STK/    Assorted stack variables
*
      INCLUDE '../include/gkstk.cmn'
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
*      a safety check when using the subroutine KDEAL, ( a subroutine to clear
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
*      (3) The response variable "IRESP" is returned with the value "0";
*
*
*      If, however, the request is too large,
*      IRESP is returned with the value 1, and ISET with the value  -1.
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
*      This is permissible : IRESP=0.
*      The offset marker ISET=1 is stored in the 18th unit;
*      KIUSED is updated to 18; and ISET will be 19 next time.
*
*      Another request of 6 units is allocated.
*      Once again, this is permissible : IRESP=0.
*      ISET=19 is stored in the 25th unit; and KIUSED then becomes 26.
*      The next value of ISET will be 26.
*      A request for 2,000,000  units is not permissable,
*      so a response IRESP=1 is given.
*
*
*                                    ISET=19           ISET=26
*                                       .                  .
*            IRESP=0                    .   IRESP=0        .
*                                       .                  . ISIZE=
*                                       .                  . 2,000,000 ?
*                                       .                  .
*                                       .                  .  :IRESP=1
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
*     (2) Succesive calls to allocate space will not give
*         contiguous areas.
*---------------------------------------------------------------------

      KIUSED = 0
      KRUSED = 0

      END
