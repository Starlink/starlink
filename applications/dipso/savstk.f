       SUBROUTINE SAVSTK( NDFNAM, BOT, TOP, STKSZE, BSTSZE, MAXSTK,
     :                    NONSTK, XSTACK, YSTACK, BSTACK, BSTNPT,
     :                    POINTR, STKNPT, BPOINT, STITLE, WORVST,
     :                    STKLST, BSTLST, STATUS )
*+
*  Name:
*     SAVSTK

*  Purpose:
*     Save stack data to an NDF.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SAVSTK( NDFNAM, STKSZE, BSTSZE, MAXSTK, NONSTK, XSTACK,
*                  YSTACK, BSTACK, BSTNPT, POINTR, STKNPT, BPOINT,
*                  STITLE, WORVST, STKLST, BSTLST, STATUS )

*  Description:
*     The NDF is created as the top level object in a new container file.
*     The number of elements in the DATA component is set equal to the
*     number of stack entries to be saved. A DIPSO_EXTRA extension is
*     created and each of the stack entries with indices from BOT to TOP
*     (inclusive) are saved in it. If an error occurs during this routine
*     the new NDF is deleted.
*
*     The arrays in common can be divided into two types; those which
*     hold information describing the properties of each stack entry, and
*     those which hold the data which constitutes the stack entry. The
*     former type have a size of MAXSTK and are indexed using the stack
*     entry number. Each element holds a scalar value describing some
*     property of the corresponding stack entry (such as the number of
*     data elements). The data for each stack entry (X, Y and break data)
*     is stored sequentially in very long, one dimensional arrays (one
*     for X, one for Y and one for break data). Data for each new stack
*     entry is stored at the end of these 1-d arrays, and pointers to the
*     first element, etc, are stored in other common arrays.
*
*     The DIPSO_EXTRA extension contains the following components (where
*     n is the number of stack entries being saved, TOP - BOT + 1):
*        -  TITLE(n) <_CHAR*80>  An array of titles for the stack entries.
*        -  WORVST(n) <_REAL>    An array of WORV values for the stack
*                                entries.
*        -  STKNPT(n) <_INTEGER> No. of elements in the X/Y arrays of each
*                                stack entry.
*        -  NSTNPT(n) <_INTEGER> No. of breaks in the X/Y arrays of each
*                                stack entry.
*        -  STACK_ccc_X(m) <_REAL>  The X data for stack entry 'ccc', where
*                                'ccc' goes from '001' to n. The array
*                                 size, m, is equal to STKNPT(ccc).
*        -  STACK_ccc_Y(m) <_REAL>  The Y data for stack entry 'ccc', where
*                                'ccc' goes from '001' to n. The array
*                                 size, m, is equal to STKNPT(ccc).
*        -  STACK_ccc_B(m) <_REAL>  The break data for stack entry 'ccc',
*                                 where 'ccc' goes from '001' to n. The array
*                                 size, m, is equal to NSTNPT(ccc).
*
*     Each break value is the index of the last pixel before a
*     discontinuity in the X and Y arrays.

*  Arguments:
*     NDFNAM = CHARACTER * ( * ) (Given)
*        The name of the NDF.
*     BOT = INTEGER (Given)
*        The index of the lowest stack entry to be saved.
*     TOP = INTEGER (Given)
*        The index of the highest stack entry to be saved.
*     STKSZE = INTEGER (Given)
*        The size of the XSTACK and YSTACK arrays.
*     BSTSZE = INTEGER (Given)
*        The size of the BSTACK array.
*     MAXSTK = INTEGER (Given)
*        The maximum number of stack entries.
*     NONSTK = INTEGER (Given)
*        The current number of stack entries.
*     XSTACK( STKSZE ) = REAL (Given)
*        An array holding the X values for all the stack entries.
*     YSTACK( STKSZE ) = REAL (Given)
*        An array holding the Y values for all the stack entries.
*     BSTACK( BSTSZE ) = REAL (Given)
*        An array holding the break points for all the stack entries.
*     BSTNPT( MAXSTK ) = INTEGER (Given)
*        The number of breaks in each stack entry.
*     POINTR( MAXSTK ) = INTEGER (Given)
*        The XSTACK indices corresponding to the start of each stack entry.
*     STKNPT( MAXSTK ) = INTEGER (Given)
*        The numbers of points in each stack entry.
*     BPOINT( MAXSTK ) = INTEGER (Given)
*        The BSTACK indices corresponding to the first break for each
*        stack entry.
*     STITLE( MAXSTK ) = CHARACTER* ( * ) (Given)
*        The title associated with each stack entry.
*     WORVST( MAXSTK ) = REAL (Given)
*        The WORV value for each stack entry.
*     STKLST = INTEGER (Given)
*        The index of the last used element in XSTACK.
*     BSTLST = INTEGER (Given)
*        The index of the last used element in BSTACK.
*     STATUS = LOGICAL (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     11-AUG-1994 (DSB):
*        Original version.
*     13-DEC 1995 (DSB):
*        Modified to use NDF_PLACE instead of HDS_NEW.
*     29-SEP-2004 (DSB):
*        Use CNF_PVAL
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT__ constants
      INCLUDE 'MSG_PAR'          ! MSG__ constants
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Arguments Given:
      CHARACTER NDFNAM*(*)
      INTEGER BOT
      INTEGER TOP
      INTEGER STKSZE
      INTEGER BSTSZE
      INTEGER MAXSTK
      INTEGER NONSTK
      REAL XSTACK( STKSZE )
      REAL YSTACK( STKSZE )
      INTEGER BSTACK( BSTSZE )
      INTEGER BSTNPT( MAXSTK )
      INTEGER POINTR( MAXSTK )
      INTEGER STKNPT( MAXSTK )
      INTEGER BPOINT( MAXSTK )
      CHARACTER STITLE( MAXSTK )*(*)
      REAL WORVST( MAXSTK )
      INTEGER STKLST
      INTEGER BSTLST

*  Status:
      INTEGER STATUS

*  Local Variables:
      CHARACTER
     :        ROOT*9,                   ! Root of stack array names
     :        XLOC*(DAT__SZLOC)         ! Locator to DIPSO_EXTRA extension

      INTEGER
     :        IERR,                     ! Index of first conversion error
     :        INDF,			! NDF identifier
     :        IPB,                      ! Pointer to mapped break array
     :        IPDATA,                   ! Pointer to mapped DATA array
     :        IPX,                      ! Pointer to mapped X array
     :        IPY,                      ! Pointer to mapped Y array
     :        ISTK,			! Current stack entries index in NDF
     :        NELMAP,                   ! No. of elements mapped
     :        NERR,                     ! Total no. of conversion errors
     :        NSAVE,	         	! No. of stack entries in NDF
     :        PLACE                     ! Place holder for new NDF

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Report an error if the NDF name contains a dot. This is because the
*  NDF name must be the name of a container file in the current version
*  and so should not have a file type (.sdf is assumed by NDF_OPEN).
*  (.sdf is removed by routine NDFNAM if supplied by the user, prior to
*  calling this routine).
      IF( INDEX( NDFNAM, '.' ) .NE. 0 ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'TYPE', NDFNAM( INDEX( NDFNAM, '.' ): )  )
         CALL ERR_REP( 'SAVSTK_ERR1', 'Illegal file type ''^TYPE'' '//
     :                 'given. Use ''.sdf'' or omit the file type '//
     :                 'altogether.', STATUS )
      END IF

*  Begin an NDF context.
      CALL NDF_BEGIN

*  Create the NDF with the required number of elements in the DATA array
*  (equal to the no. of stack entries being saved).
      CALL NDF_PLACE( DAT__ROOT, NDFNAM, PLACE, STATUS )
      NSAVE = TOP - BOT + 1
      CALL NDF_NEW( '_REAL', 1, 1, NSAVE, PLACE, INDF, STATUS )

*  Map the DATA array. This is necessary to ensure that the DATA array is
*  in a defined state. Otherwise, other applicatiuons will not be able to
*  access the NDF (even though the DATA array is of no interest).
      CALL NDF_MAP( INDF, 'DATA', '_REAL', 'WRITE/ZERO', IPDATA, NELMAP,
     :              STATUS )

*  Create the DIPSO_EXTRA extension.
      CALL NDF_XNEW( INDF, 'DIPSO_EXTRA', 'EXTENSION', 0, 0, XLOC,
     :               STATUS )

*  Create the components which describe each of the stack
*  entries saved in the NDF.
      CALL DAT_NEW1C( XLOC, 'TITLES', 80, NSAVE, STATUS )
      CALL DAT_NEW1R( XLOC, 'WORVST', NSAVE, STATUS )
      CALL DAT_NEW1I( XLOC, 'STKNPT', NSAVE, STATUS )
      CALL DAT_NEW1I( XLOC, 'NSTNPT', NSAVE, STATUS )

*  Store values in these components.
      CALL CMP_PUT1C( XLOC, 'TITLES', NSAVE, STITLE( BOT ), STATUS )
      CALL CMP_PUT1R( XLOC, 'WORVST', NSAVE, WORVST( BOT ), STATUS )
      CALL CMP_PUT1I( XLOC, 'STKNPT', NSAVE, STKNPT( BOT ), STATUS )
      CALL CMP_PUT1I( XLOC, 'NSTNPT', NSAVE, BSTNPT( BOT ), STATUS )

*  Loop round each stack entry to be saved in the NDF.
      DO ISTK = BOT, TOP

*  Form the root for the names of the components containing the stack data.
         WRITE ( ROOT, 10 ) ISTK - BOT + 1
 10      FORMAT ( 'STACK_' , I3.3 )

*  Create and map the array containing the X values for the stack entry.
         CALL DAT_NEW1R( XLOC, ROOT//'_X', STKNPT( ISTK ), STATUS )
         CALL CMP_MAPN( XLOC, ROOT//'_X', '_REAL', 'WRITE', 1, IPX,
     :                  NELMAP, STATUS )

*  Create and map the array containing the Y values for the stack entry.
         CALL DAT_NEW1R( XLOC, ROOT//'_Y', STKNPT( ISTK ), STATUS )
         CALL CMP_MAPN( XLOC, ROOT//'_Y', '_REAL', 'WRITE', 1, IPY,
     :                  NELMAP, STATUS )

*  Create and map the array containing the stack entry break points.
         CALL DAT_NEW1I( XLOC, ROOT//'_B', BSTNPT( ISTK ), STATUS )
         CALL CMP_MAPN( XLOC, ROOT//'_B', '_INTEGER', 'WRITE', 1, IPB,
     :                  NELMAP, STATUS )

*  Copy the supplied arrays to these new components.
         IF( STATUS .EQ. SAI__OK ) THEN
            CALL VEC_RTOR( .FALSE., STKNPT( ISTK ),
     :                     XSTACK( POINTR( ISTK ) ),
     :                     %VAL( CNF_PVAL( IPX ) ), IERR,
     :                     NERR, STATUS )
            CALL VEC_RTOR( .FALSE., STKNPT( ISTK ),
     :                     YSTACK( POINTR( ISTK ) ),
     :                     %VAL( CNF_PVAL( IPY ) ), IERR,
     :                     NERR, STATUS )
            CALL VEC_ITOI( .FALSE., BSTNPT( ISTK ),
     :                     BSTACK( BPOINT( ISTK ) ),
     :                     %VAL( CNF_PVAL( IPB ) ), IERR,
     :                     NERR, STATUS )
         END IF

*  Unmap the arrays.
         CALL CMP_UNMAP( XLOC, ROOT//'_X', STATUS )
         CALL CMP_UNMAP( XLOC, ROOT//'_Y', STATUS )
         CALL CMP_UNMAP( XLOC, ROOT//'_B', STATUS )

*  Abort if an error has occurred.
         IF( STATUS .NE. SAI__OK ) GO TO 999

      END DO

*  Jump to here if an error occurs.
 999  CONTINUE

*  Annul the locator to the DIPSO_EXTRA extension.
      CALL DAT_ANNUL( XLOC, STATUS )

*  If an error has occurred, attempt to delete the NDF.
      IF( STATUS .NE. SAI__OK ) CALL NDF_DELET( INDF, STATUS )

*  End the NDF context. This will annul the NDF identifier, INDF.
      CALL NDF_END( STATUS )

      END
