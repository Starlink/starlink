C# IL>=a, OL>=0
      SUBROUTINE GKHPOP
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    Front End / Utility
*  Author:             CJW
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Allocate storage from the heap
*
*  MAINTENANCE LOG
*  ---------------
*     17/05/83  CJW   Original version stabilized
*     27/06/83  CJW   Implement revised error handling precedure
*                     (No change required)
*     13/02/87  PLP   Character heap now equivalenced to
*                     Integer/Real. Altered code and comments
*                     accordingly.
*     03/07/87  RMK   Added GKMC.PAR - now needed by GKHP.CMN.
*
*  COMMON BLOCK USAGE
*  ------------------
*     Modify /GKYHP/ Pointers, counts and heap changed
*     Modify /GKZHP/ Heap changed
*
      INCLUDE '../include/gkhp.par'
      INCLUDE '../include/gkmc.par'
      INCLUDE '../include/gkhp.cmn'
*
*  LOCALS
*  ------
*     I       Loop Index
*
      INTEGER I
*
*  HEAP USAGE
*  ----------
*     Heap manager - Open heap system
*
*  COMMENTS
*  --------
*     The heap must be initialised by calling GKHPOP. The heap is
*     initialised to integer zero because the Garbage
*     collector will have to move data about - some systems check that
*     data has been assigned before it is accessed. If this facility
*     does not exist, or is turned off for GKGC, then the HEAP
*     need not be preset.
*
*     The heap consists of three large arrays -
*
*       KHP     The integer heap        An array of integers
*       QHP     The real heap           An array of reals
*       CHP     The character heap      An array of character * 1
*
*     The three arrays are EQUIVALENCED. Not only does this allow
*     a more dynamic allocation system, it also reduces the code.
*
*     The user has access to the allocated data via a directory called
*     KHPXi where i = R, I or C. Examples of usage are -
*
*                       QHP(KHPXR(INDEX)+J)
*                       KHP(KHPXI(INDEX)+J)
*                       CHP(KHPXC(INDEX)+J)
*     or generically
*                       jHP(KHPXi(INDEX)+J)     (i,j) = (I,K), (R,Q), (C,C)
*
*      where INDEX is the value returned by GKHPAL, J = 0,1,...size-1
*
*      Each heap entry consists of an array of items ( integers, reals
*      or characters ). In addition, Integer/Real heaps have an array to
*      hold the back pointers (KHPXPi), an array to hold the sizes (KHPESi),
*      a "lump" count (KHPLMi) and a "lump" index. A lump is contiguous
*      collection of items that have been allocated, or allocated and
*      deallocated.
*
*
*
*
*                                        Back Pointer
*                                        ------------
*                                      _________________
*                                     /    |      |    /
*                                     \    | Back |    \<---------+
*                                     /____|______|____/          |
*                                             |                   |
*    +----------------------------------------+                   |
*    |                                                            |
*    |                                       Size                 |
*    |                                       ----                 |
*    |                                 _________________          |
*    |                                /    |      |    /          |
*    |                                \    | Size |    \<---------+
*    |                                /____|______|____/          |
*    |                                                            |
*    |                                                            |
*    |                                    Lump Index              |
*    |                                    ----------              |
*    |                                 _________________          |
*    |           +-----+              /    |      |    /          |
*    |           |     |----+-------->\    | Lump |    \          |
*    |           |INDEX|              /____|______|____/          |
*    |           |     |----+                 |                   |
*    |           +-----+    |                 +-------------------+
*    |                      |
*    |                      |                Heap
*    |                      |                ----
*    |                      |          _________________
*    |                      |         /    |      |    /
*    |                      |         \    | Data |    \
*    |                      |         /____|______|____/
*    |   Directory          |                  |
*    |   ---------          |                  |
*    |                      |                  |
*    |                      |                  |
*    |   |       |          |                  |
*    |   |-------|<---------+                  |
*    |   |       |                             |
*    +-->|POINTER---->-------------------------+
*        |       |
*        |-------|
*        |       |
*
*      At Deallocation the POINTER is set to zero to indicate that the
*      directory entry is free. Before this is done, the back pointer is
*      replaced by -(POINTER) so that the area may be reused without a
*      garbage collect. The minus is to differentiate between the two uses.
*
*---------------------------------------------------------------------

*     Preset Next pointers to the beginning of the heap

      KHPNXI = 1

*     Preset number of available items

      KHPAVI = KHPSZI

*     Preset number of "lumps" in use

      KHPLMI = 0

*     Preset Directories to "not in use"

      DO 1 I = 1, KHPXSI
         KHPXI(I) = 0
    1 CONTINUE

      DO 2 I = 1, KHPXSC
         KHPXC(I) = 0
    2 CONTINUE

*     Initialize the heaps to  Integer zeroes.
*     Character heap is initialised to blanks
*     only when and if the allocation is granted.

      DO 3 I = 1, KHPSZI
         KHP(I) = 0
    3 CONTINUE

      END
