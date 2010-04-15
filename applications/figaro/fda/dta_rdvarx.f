*+
*  Name:
*     DTA_RDVAR{DFI}

*  Purpose:
*     Read data items from an object.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL DTA_RDVAR<T>( PATH, NITEM, ARRAY, DTA_STATUS )

*  Description:
*     This routine reads data items from a data structure object.

*  Arguments:
*     PATH = CHARACTER * ( * ) (Given)
*        The DTA structure name from which data are to be read. This
*        should be in the standard data system format, i.e. name
*        components separated by dots followed by optional dimensional
*        information enclosed in square brackets. Ideally the name
*        should have been created by a call to DTA_CRVAR.
*     NITEM = INTEGER (Given)
*        The number of data items to be read. Note that this is an item
*        count, and not a byte count.
*     ARRAY( NITEM ) = <TYPE> (Returned)
*        The data to be read. The data is converted into the form
*        required by the call from the form it is held in by the data
*        system, if necessary.
*     DTA_STATUS = INTEGER (Returned)
*        The DTA status.

*  Authors:
*     ks: Keith Shortridge (CIT, AAO)
*     hme: Horst Meyerdierks (UoE, Starlink)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     26 Oct 1982 (ks):
*        Original version.
*     10 Jan 1992 (ks):
*        Syntax of include statements changed to remove VMS logical
*        names and to use lower case, to enable compilation on a SUN.
*     04 Mar 1996 (hme):
*        FDA library.
*     2005 May 31 (MJC):
*        Use CNF_PVAL for pointers to mapped data.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*     SUBROUTINE DTA_RDVAR<T>( PATH, NITEM, ARRAY, STATUS )
      SUBROUTINE DTA_RDVARF( PATH, NITEM, ARRAY, STATUS )

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! Standard DAT constants
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Arguments Given:
      CHARACTER * ( * ) PATH
      INTEGER NITEM

*  Arguments Returned:
*     <TYPE> ARRAY( NITEM )
      REAL ARRAY( NITEM )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Loop variable
      INTEGER IDOT1, IDOT2, IBRA ! Pointers into PATH
      INTEGER NDIM, NDIM2        ! Cell and shape dimensions
      INTEGER CELL( DAT__MXDIM ) ! Cell number
      INTEGER DIMS( DAT__MXDIM ) ! Shape
      INTEGER INDX, SIZE         ! Vectorised cell number and shape
      INTEGER PNTR               ! Map pointer
      CHARACTER * ( DAT__SZLOC ) LOC ! HDS locator

*  Internal References:
      INTEGER CHR_LEN            ! Used length of string

*.

*  Begin error context and translate status.
      CALL ERR_MARK
      STATUS = SAI__OK

*  Split off and read any cell specification.
      CALL DTA1_SPLIT( PATH, DAT__MXDIM,
     :   IDOT1, IDOT2, IBRA, NDIM, CELL, STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 500
      IF ( IBRA .EQ. 0 ) IBRA = 1 + CHR_LEN( PATH )

*  Locate the component.
      CALL DTA1_LOC( PATH(:IBRA-1), LOC, STATUS )

*  Find shape of component and translate cell specification into the
*  element number of a vectorised object. Also find total size of
*  component.
      CALL DAT_SHAPE( LOC, DAT__MXDIM, DIMS, NDIM2, STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 500
      INDX = CELL(1)
      SIZE = DIMS(1)
      IF ( NDIM2 .LE. 0 ) SIZE = 1
      DO 1 I = 2, NDIM2
         INDX = INDX + SIZE * ( CELL(I) - 1 )
         SIZE = SIZE * DIMS(I)
 1    CONTINUE

*  Check last element to be read is not beyond vector length.
      IF ( INDX + NITEM - 1 .GT. SIZE ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'FDA_T016', PATH )
         CALL MSG_SETI( 'FDA_T029', NITEM )
         CALL ERR_REP( 'FDA_E085', 'DTA1_RDVARx: Error reading ' //
     :      '^FDA_T029 elements from ^FDA_T016. The object is not ' //
     :      'that large.', STATUS )
         GO TO 500
      END IF

*  Map the object. Transfer the requested part of the data.
*     CALL DAT_MAPV( LOC, '<TYPE>', 'READ', PNTR, SIZE, STATUS )
*     CALL DTA2_COPY<T>( %VAL( CNF_PVAL(PNTR) ), INDX, NITEM, ARRAY, 1,
*    :                  STATUS )
      CALL DAT_MAPV( LOC, '_REAL', 'READ', PNTR, SIZE, STATUS )
      CALL DTA2_COPYF( %VAL( CNF_PVAL(PNTR) ), INDX, NITEM, ARRAY, 1,
     :                 STATUS )

*  Tidy up.
 500  CONTINUE
      CALL DAT_UNMAP( LOC, STATUS )
      CALL DAT_ANNUL( LOC, STATUS )

*  Translate status and end error context.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_ANNUL( STATUS )
         STATUS = 1
      ELSE
         STATUS = 0
      END IF
      CALL ERR_RLSE

*  Return.
      END



*     SUBROUTINE DTA_RDVAR<T>( PATH, NITEM, ARRAY, STATUS )
      SUBROUTINE DTA_RDVARD( PATH, NITEM, ARRAY, STATUS )

      IMPLICIT NONE              ! No implicit typing

      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! Standard DAT constants
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

      CHARACTER * ( * ) PATH
      INTEGER NITEM

*     <TYPE> ARRAY( NITEM )
      DOUBLE PRECISION ARRAY( NITEM )

      INTEGER STATUS             ! Global status

      INTEGER I                  ! Loop variable
      INTEGER IDOT1, IDOT2, IBRA ! Pointers into PATH
      INTEGER NDIM, NDIM2        ! Cell and shape dimensions
      INTEGER CELL( DAT__MXDIM ) ! Cell number
      INTEGER DIMS( DAT__MXDIM ) ! Shape
      INTEGER INDX, SIZE         ! Vectorised cell number and shape
      INTEGER PNTR               ! Map pointer
      CHARACTER * ( DAT__SZLOC ) LOC ! HDS locator

      INTEGER CHR_LEN            ! Used length of string

      CALL ERR_MARK
      STATUS = SAI__OK

      CALL DTA1_SPLIT( PATH, DAT__MXDIM,
     :   IDOT1, IDOT2, IBRA, NDIM, CELL, STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 500
      IF ( IBRA .EQ. 0 ) IBRA = 1 + CHR_LEN( PATH )

      CALL DTA1_LOC( PATH(:IBRA-1), LOC, STATUS )

      CALL DAT_SHAPE( LOC, DAT__MXDIM, DIMS, NDIM2, STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 500
      INDX = CELL(1)
      SIZE = DIMS(1)
      IF ( NDIM2 .LE. 0 ) SIZE = 1
      DO 1 I = 2, NDIM2
         INDX = INDX + SIZE * ( CELL(I) - 1 )
         SIZE = SIZE * DIMS(I)
 1    CONTINUE

      IF ( INDX + NITEM - 1 .GT. SIZE ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'FDA_T016', PATH )
         CALL MSG_SETI( 'FDA_T029', NITEM )
         CALL ERR_REP( 'FDA_E085', 'DTA1_RDVARx: Error reading ' //
     :      '^FDA_T029 elements from ^FDA_T016. The object is not ' //
     :      'that large.', STATUS )
         GO TO 500
      END IF

*     CALL DAT_MAPV( LOC, '<TYPE>', 'READ', PNTR, SIZE, STATUS )
*     CALL DTA2_COPY<T>( %VAL( CNF_PVAL(PNTR) ), INDX, NITEM, ARRAY, 1,
*    :                   STATUS )
      CALL DAT_MAPV( LOC, '_DOUBLE', 'READ', PNTR, SIZE, STATUS )
      CALL DTA2_COPYD( %VAL( CNF_PVAL(PNTR) ), INDX, NITEM, ARRAY, 1,
     :                 STATUS )

 500  CONTINUE
      CALL DAT_UNMAP( LOC, STATUS )
      CALL DAT_ANNUL( LOC, STATUS )

      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_ANNUL( STATUS )
         STATUS = 1
      ELSE
         STATUS = 0
      END IF
      CALL ERR_RLSE

      END



*     SUBROUTINE DTA_RDVAR<T>( PATH, NITEM, ARRAY, STATUS )
      SUBROUTINE DTA_RDVARI( PATH, NITEM, ARRAY, STATUS )

      IMPLICIT NONE              ! No implicit typing

      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! Standard DAT constants
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

      CHARACTER * ( * ) PATH
      INTEGER NITEM

*     <TYPE> ARRAY( NITEM )
      INTEGER ARRAY( NITEM )

      INTEGER STATUS             ! Global status

      INTEGER I                  ! Loop variable
      INTEGER IDOT1, IDOT2, IBRA ! Pointers into PATH
      INTEGER NDIM, NDIM2        ! Cell and shape dimensions
      INTEGER CELL( DAT__MXDIM ) ! Cell number
      INTEGER DIMS( DAT__MXDIM ) ! Shape
      INTEGER INDX, SIZE         ! Vectorised cell number and shape
      INTEGER PNTR               ! Map pointer
      CHARACTER * ( DAT__SZLOC ) LOC ! HDS locator

      INTEGER CHR_LEN            ! Used length of string

      CALL ERR_MARK
      STATUS = SAI__OK

      CALL DTA1_SPLIT( PATH, DAT__MXDIM,
     :   IDOT1, IDOT2, IBRA, NDIM, CELL, STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 500
      IF ( IBRA .EQ. 0 ) IBRA = 1 + CHR_LEN( PATH )

      CALL DTA1_LOC( PATH(:IBRA-1), LOC, STATUS )

      CALL DAT_SHAPE( LOC, DAT__MXDIM, DIMS, NDIM2, STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 500
      INDX = CELL(1)
      SIZE = DIMS(1)
      IF ( NDIM2 .LE. 0 ) SIZE = 1
      DO 1 I = 2, NDIM2
         INDX = INDX + SIZE * ( CELL(I) - 1 )
         SIZE = SIZE * DIMS(I)
 1    CONTINUE

      IF ( INDX + NITEM - 1 .GT. SIZE ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'FDA_T016', PATH )
         CALL MSG_SETI( 'FDA_T029', NITEM )
         CALL ERR_REP( 'FDA_E085', 'DTA1_RDVARx: Error reading ' //
     :      '^FDA_T029 elements from ^FDA_T016. The object is not ' //
     :      'that large.', STATUS )
         GO TO 500
      END IF

*     CALL DAT_MAPV( LOC, '<TYPE>', 'READ', PNTR, SIZE, STATUS )
*     CALL DTA2_COPY<T>( %VAL( CNF_PVAL(PNTR) ), INDX, NITEM, ARRAY, 1,
*    :                   STATUS )
      CALL DAT_MAPV( LOC, '_INTEGER', 'READ', PNTR, SIZE, STATUS )
      CALL DTA2_COPYI( %VAL( CNF_PVAL(PNTR) ), INDX, NITEM, ARRAY, 1,
     :                 STATUS )

 500  CONTINUE
      CALL DAT_UNMAP( LOC, STATUS )
      CALL DAT_ANNUL( LOC, STATUS )

      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_ANNUL( STATUS )
         STATUS = 1
      ELSE
         STATUS = 0
      END IF
      CALL ERR_RLSE

      END



      SUBROUTINE DTA2_COPYF( ARRAY1, INDX1, NITEM,
     :   ARRAY2, INDX2, STATUS )

      IMPLICIT NONE

      INCLUDE 'SAE_PAR'

      REAL ARRAY1( 1 )
      INTEGER INDX1
      INTEGER NITEM
      INTEGER INDX2

      REAL ARRAY2( 1 )

      INTEGER STATUS

      INTEGER I, J

      IF ( STATUS .NE. SAI__OK ) RETURN

      J = INDX2
      DO 1 I = INDX1, INDX1 + NITEM - 1
         ARRAY2(J) = ARRAY1(I)
         J = J + 1
 1    CONTINUE

      END



      SUBROUTINE DTA2_COPYI( ARRAY1, INDX1, NITEM,
     :   ARRAY2, INDX2, STATUS )

      IMPLICIT NONE

      INCLUDE 'SAE_PAR'

      INTEGER ARRAY1( 1 )
      INTEGER INDX1
      INTEGER NITEM
      INTEGER INDX2

      INTEGER ARRAY2( 1 )

      INTEGER STATUS

      INTEGER I, J

      IF ( STATUS .NE. SAI__OK ) RETURN

      J = INDX2
      DO 1 I = INDX1, INDX1 + NITEM - 1
         ARRAY2(J) = ARRAY1(I)
         J = J + 1
 1    CONTINUE

      END



      SUBROUTINE DTA2_COPYD( ARRAY1, INDX1, NITEM,
     :   ARRAY2, INDX2, STATUS )

      IMPLICIT NONE

      INCLUDE 'SAE_PAR'

      DOUBLE PRECISION ARRAY1( 1 )
      INTEGER INDX1
      INTEGER NITEM
      INTEGER INDX2

      DOUBLE PRECISION ARRAY2( 1 )

      INTEGER STATUS

      INTEGER I, J

      IF ( STATUS .NE. SAI__OK ) RETURN

      J = INDX2
      DO 1 I = INDX1, INDX1 + NITEM - 1
         ARRAY2(J) = ARRAY1(I)
         J = J + 1
 1    CONTINUE

      END
