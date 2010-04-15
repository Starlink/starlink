       SUBROUTINE RESSTK( COMM, INDF, STKSZE, BSTSZE, MAXSTK, NONSTK,
     :                    XSTACK, YSTACK, BSTACK, BSTNPT, POINTR,
     :                    STKNPT, BPOINT, STITLE, WORVST, STKLST,
     :                    BSTLST, STATUS )
*+
*  Name:
*     RESSTK

*  Purpose:
*     Restore stack data from an NDF.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL RESSTK( COMM, INDF, STKSZE, BSTSZE, MAXSTK, NONSTK, XSTACK,
*                  YSTACK, BSTACK, BSTNPT, POINTR, STKNPT, BPOINT,
*                  STITLE, WORVST, STKLST, BSTLST, STATUS )

*  Description:
*     The DIPSO_EXTRA extension is found in the NDF (an error
*     is reported if it is not found). Each of the stack entries saved in
*     the NDF is then restored until all are done or the common arrays
*     are filled. If the arrays are filled before all stack entries have
*     been restored, a warning message is given that not all stack
*     entries have been restored, but no error is flagged.
*
*     The arrays in common can be divided into two types; those which
*     hold information describing the properties of each stack entry, and
*     those which hold the data which constitutes the stack entry. The
*     former type have a size of MAXSTK and are indexed using the stack
*     entry number. Each element holds a scalar value descring some
*     property of the corresponding stack entry (such as the number of
*     data elements). The data for each stack entry (X, Y and break data)
*     is stored sequentially in very long, one dimensional arrays (one
*     for X, one for Y and one for break data). Data for each new stack
*     entry is stored at the end of these 1-d arrays, and pointers to the first
*     element, etc, are stored in other common arrays.

*  Arguments:
*     COMM = CHARACTER * ( * ) (Given)
*        The command name.
*     INDF = INTEGER (Given)
*        NDF identifier.
*     STKSZE = INTEGER (Given)
*        The size of the XSTACK and YSTACK arrays.
*     BSTSZE = INTEGER (Given)
*        The size of the BSTACK array.
*     MAXSTK = INTEGER (Given)
*        The maximum number of stack entries.
*     NONSTK = INTEGER (Given and Returned)
*        The current number of stack entries.
*     XSTACK( STKSZE ) = REAL (Given and Returned)
*        An array holding the X values for all the stack entries.
*     YSTACK( STKSZE ) = REAL (Given and Returned)
*        An array holding the Y values for all the stack entries.
*     BSTACK( BSTSZE ) = REAL (Given and Returned)
*        An array holding the break points for all the stack entries.
*     BSTNPT( MAXSTK ) = INTEGER (Given and Returned)
*        The number of breaks in each stack entry.
*     POINTR( MAXSTK ) = INTEGER (Given and Returned)
*        The XSTACK indices corresponding to the start of each stack entry.
*     STKNPT( MAXSTK ) = INTEGER (Given and Returned)
*        The numbers of points in each stack entry.
*     BPOINT( MAXSTK ) = INTEGER (Given and Returned)
*        The BSTACK indices corresponding to the first break for each
*        stack entry.
*     STITLE( MAXSTK ) = CHARACTER* ( * ) (Given and Returned)
*        The title associated with each stack entry.
*     WORVST( MAXSTK ) = REAL (Given)
*        The WORV value for each stack entry.
*     STKLST = INTEGER (Given and Returned)
*        The index of the last used element in XSTACK.
*     BSTLST = INTEGER (Given and Returned)
*        The index of the last used element in BSTACK.
*     STATUS = INTEGER (Given and Returned)
*        Global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     22-AUG-1994 (DSB):
*        Original version.
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
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Arguments Given:
      CHARACTER COMM * ( * )
      INTEGER INDF
      INTEGER STKSZE
      INTEGER BSTSZE
      INTEGER MAXSTK

*  Arguments Given and Returned:
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

*  Status Argument:
      INTEGER STATUS

*  Local Variables:
      CHARACTER
     :        LOC*(DAT__SZLOC),         ! Locator to a single cell
     :        ROOT*9,                   ! Root of stack array names
     :        TLOC*(DAT__SZLOC),        ! Locator to TITLES array
     :        WLOC*(DAT__SZLOC),        ! Locator to WORVST array
     :        XLOC*(DAT__SZLOC)         ! Locator to DIPSO_EXTRA extension

      INTEGER
     :        IERR,                     ! Index of first conversion error
     :        IPB,                      ! Pointer to mapped break array
     :        IPX,                      ! Pointer to mapped X array
     :        IPY,                      ! Pointer to mapped Y array
     :        ISTK,			! Current stack entries index in NDF
     :        NERR,                     ! Total no. of conversion errors
     :        NSTK 			! No. of stack entries in NDF

      LOGICAL
     :        THERE                     ! Does the named component exist?

*.

*  Check the inherited status.
      IF( STATUS .NE. SAI__OK ) RETURN

*  Attempt to obtain a locator to the DIPSO_EXTRA extension.
      CALL NDF_XLOC( INDF, 'DIPSO_EXTRA', 'READ', XLOC, STATUS )

*  If an error occured, add a context message and abort.
      IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'RESSTK_ERR1', 'Supplied NDF was not'//
     :                 'created by DIPSO.', STATUS )
         GO TO 999
      END IF

*  See if the DIPSO_EXTRA extension contains a component called
*  BREAKS. If it does, the NDF does not contain stack data. Report
*  an error and abort.
      CALL DAT_THERE( XLOC, 'BREAKS', THERE, STATUS )
      IF( THERE .AND. STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL NDF_MSG( 'NDF', INDF )
         CALL ERR_REP( 'RESSTK_ERR2', 'The NDF ''^NDF'' contains'//
     :                 'current array data, not stack data.', STATUS )
         GO TO 999
      END IF

*  Get locators to the components which describe each of the stack
*  entries saved in the NDF.
      CALL DAT_FIND( XLOC, 'TITLES', TLOC, STATUS )
      CALL DAT_FIND( XLOC, 'WORVST', WLOC, STATUS )

*  Find the number of elements in the NDFs DATA array. The values
*  themselves are rubbish, but the number of values should be equal
*  to the number of stack entries saved in the NDF.
      CALL NDF_SIZE( INDF, NSTK, STATUS )

*  Loop round each stack entry in the NDF.
      DO ISTK = 1, NSTK

*  If the maximum number of stack entries has been exceeded, give a
*  warning and return.
         IF( NONSTK .GE. MAXSTK ) THEN
            CALL MSG_SETI( 'NDONE', ISTK - 1 )
            CALL MSG_SETI( 'NTOT', NSTK )
            CALL NDF_MSG( 'NDF', INDF )
            CALL MSGOUT( COMM, 'Cannot store any more stack entries. '//
     :                   '^NDONE entries restored out of ^NTOT '//
     :                   'contained in NDF ''^NDF''.', .TRUE., STATUS )
            GO TO 999
         END IF

*  Calculate the index at which information describing this stack entry
*  will be stored in common.
         NONSTK = NONSTK + 1

*  Get the title.
         CALL DAT_CELL( TLOC, 1, ISTK, LOC, STATUS )
         CALL DAT_GET0C( LOC, STITLE( NONSTK ), STATUS )
         CALL DAT_ANNUL( LOC, STATUS )

*  Get the WORV value.
         CALL DAT_CELL( WLOC, 1, ISTK, LOC, STATUS )
         CALL DAT_GET0R( LOC, WORVST( NONSTK ), STATUS )
         CALL DAT_ANNUL( LOC, STATUS )

*  Form the root for the names of the components containing the stack data.
         WRITE ( ROOT, 10 ) ISTK
 10      FORMAT ( 'STACK_' , I3.3 )

*  Map the array containing the X values for the stack entry.
         CALL CMP_MAPN( XLOC, ROOT//'_X', '_REAL', 'READ', 1, IPX,
     :                  STKNPT( NONSTK ), STATUS )

*  Give a warning if there is insufficient room for the X and Y data.
*  Decrement the number of stack entries and return.
         IF( STKNPT( NONSTK ) + STKLST .GT. STKSZE ) THEN
            NONSTK = NONSTK - 1
            CALL MSG_SETI( 'NDONE', ISTK - 1 )
            CALL MSG_SETI( 'NTOT', NSTK )
            CALL NDF_MSG( 'NDF', INDF )
            CALL MSGOUT( COMM, 'Stack data arrays exhausted. ^NDONE '//
     :                   'stack entries restored out of ^NTOT '//
     :                   'contained in NDF ''^NDF''.', .TRUE., STATUS )

            CALL CMP_UNMAP( XLOC, ROOT//'_X', STATUS )

            GO TO 999

         END IF

*  Map the array containing the Y values for the stack entry.
         CALL CMP_MAPN( XLOC, ROOT//'_Y', '_REAL', 'READ', 1, IPY,
     :                  STKNPT( NONSTK ), STATUS )

*  Map the array containing the stack entry break points.
         CALL CMP_MAPN( XLOC, ROOT//'_B', '_INTEGER', 'READ', 1, IPB,
     :                  BSTNPT( NONSTK ), STATUS )

*  Give a warning if there is insufficient room for the break data.
*  Decrement the number of stack entries and return.
         IF( BSTNPT( NONSTK ) + BSTLST .GT. BSTSZE ) THEN
            NONSTK = NONSTK - 1
            CALL MSG_SETI( 'NDONE', ISTK - 1 )
            CALL MSG_SETI( 'NTOT', NSTK )
            CALL NDF_MSG( 'NDF', INDF )
            CALL MSGOUT( COMM, 'Stack break array exhausted. ^NDONE '//
     :                   'stack entries restored out of ^NTOT '//
     :                   'contained in NDF ''^NDF''.', .TRUE., STATUS )

            CALL CMP_UNMAP( XLOC, ROOT//'_Y', STATUS )
            CALL CMP_UNMAP( XLOC, ROOT//'_B', STATUS )

            GO TO 999

         END IF

*  Copy them to the returned arrays.
         IF( STATUS .EQ. SAI__OK ) THEN
            CALL VEC_RTOR( .FALSE., STKNPT( NONSTK ),
     :                     %VAL( CNF_PVAL( IPX ) ),
     :                     XSTACK( STKLST + 1 ), IERR, NERR, STATUS )
            CALL VEC_RTOR( .FALSE., STKNPT( NONSTK ),
     :                     %VAL( CNF_PVAL( IPY ) ),
     :                     YSTACK( STKLST + 1 ), IERR, NERR, STATUS )
            CALL VEC_ITOI( .FALSE., BSTNPT( NONSTK ),
     :                     %VAL( CNF_PVAL( IPB ) ),
     :                     BSTACK( BSTLST + 1 ), IERR, NERR, STATUS )

*  Store the indices at which the data for the current stack entry starts.
            POINTR( NONSTK ) = STKLST + 1
            BPOINT( NONSTK ) = BSTLST + 1

*  Update the index of the last used elements in X/YSTACK and BSTACK.
            STKLST = STKLST + STKNPT( NONSTK )
            BSTLST = BSTLST + BSTNPT( NONSTK )

         END IF

*  Unmap the mapped arrays.
         CALL CMP_UNMAP( XLOC, ROOT//'_X', STATUS )
         CALL CMP_UNMAP( XLOC, ROOT//'_Y', STATUS )
         CALL CMP_UNMAP( XLOC, ROOT//'_B', STATUS )

*  Abort if an error has occurred.
         IF( STATUS .NE. SAI__OK ) GO TO 999

      END DO

*  Jump to here if an error occurs.
 999  CONTINUE

*  Annul the locator to the DIPSO_EXTRA extension, and its components.
      CALL DAT_ANNUL( TLOC, STATUS )
      CALL DAT_ANNUL( WLOC, STATUS )
      CALL DAT_ANNUL( XLOC, STATUS )

      END
