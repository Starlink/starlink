       SUBROUTINE WRITE_NDF( COMM, NDFNAM, NPOINT, WAVE, FLUX, XLAB,
     :                       YLAB, TITLE, NBREAK, BREAK, WORV, STATUS )
*+
*  Name:
*     WRITE_NDF

*  Purpose:
*     Save supplied array data to an NDF.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL WRITE_NDF( COMM, NDFNAM, NPOINT, WAVE, FLUX, XLAB, YLAB,
*                     TITLE, NBREAK, BREAK, WORV, STATUS )

*  Description:
*     This subroutine is intended for users to use who wish to write
*     applications which create DIPSO NDF structures.
*
*     The NDF is created as the top level object in a new container
*     file.  The FLUX data is copied from the common array to the NDF's
*     DATA array. The WAVE data is copied from common to the NDF's AXIS
*     CENTRE array. The supplied title is stored. The NDF label is set
*     to the supplied Y axis label. The label for axis 1 is set to the
*     supplied X axis label. A DIPSO_EXTRA extension is created
*     containing the WORV value and the supplied BREAK array. If an
*     error occurs during this routine the new NDF is deleted.

*  Arguments:
*     COMM = CHARACTER * ( * ) (Given)
*        The DIPSO command name which invoked this routine.
*     NDFNAM = CHARACTER * ( * ) (Given)
*        The name of the NDF.
*     NPOINT = INTEGER (Given)
*        The number of data elements used in FLUX and WAVE, starting
*        at element 1.
*     WAVE( NPOINT ) = REAL (Given)
*        The X value (usually wavelength or velocity) at the corresponding
*        element in the FLUX array.
*     FLUX( NPOINT ) = REAL (Given)
*        The data value at each element.
*     XLAB = CHARACTER * ( * ) (Given)
*        The label for the X axis.
*     YLAB = CHARACTER * ( * ) (Given)
*        The label for the Y axis.
*     TITLE = CHARACTER * ( * ) (Given)
*        The title.
*     NBREAK = INTEGER (Given)
*        The number of breaks stored in the common BREAK array. The
*        first is stored in BREAK(1) and the last in BREAK(NBREAK).
*     BREAK( NBREAK ) = INTEGER (Read)
*        The pixel indices at which breaks occur in the X and Y
*        arrays.
*     WORV = REAL (Given)
*        The WORV value.
*     STATUS = LOGICAL (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     3-OCT-1994 (DSB):
*        Original version.
*     13-DEC-1995 (DSB):
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
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Arguments Given:
      CHARACTER COMM*(*)
      CHARACTER NDFNAM*(*)
      INTEGER NPOINT
      REAL WAVE( NPOINT )
      REAL FLUX( NPOINT )
      CHARACTER XLAB*(*)
      CHARACTER YLAB*(*)
      CHARACTER TITLE*(*)
      INTEGER NBREAK
      INTEGER BREAK( NBREAK )
      REAL WORV

*  Status:
      INTEGER STATUS

*  External References:
      INTEGER CHR_LEN                   ! Used length of a string

*  Local Variables:
      CHARACTER
     :        ROOT*9,                   ! Root of stack array names
     :        XLOC*(DAT__SZLOC)         ! Locator to DIPSO_EXTRA extension

      INTEGER
     :        F,                        ! Index of first non-blank character
     :        INDF,			! NDF identifier
     :        IPAXIS,                   ! Pointer to mapped AXIS CENTRE array
     :        IPDATA,                   ! Pointer to mapped DATA array
     :        L,                        ! Index of last non-blank character
     :        NDFSIZ,                   ! Size of the NDF
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
         CALL ERR_REP( 'WRITE_NDF_ERR1', 'Illegal file type ''^TYPE'''//
     :                 ' given. Use ''.sdf'' or omit the file type '//
     :                 'altogether.', STATUS )
      END IF

*  Begin an NDF context.
      CALL NDF_BEGIN

*  Create a place holder to identify the location in the data system at
*  which the new NDF is to be created.
      CALL NDF_PLACE( DAT__ROOT, NDFNAM, PLACE, STATUS )

*  Store the size required for the NDF. If we are creating a normal
*  DIPSO NDF, then it is just equal to the used size of the current
*  array (NPOINT). If we are saving a SPECTRUM format 0 NDF (i.e. if
*  COMM is SP0WR) then the NDF will be larger than the current arrays
*  by 2 pixels for every break point (excluding the last break which
*  always marks the end of the current arrays).
      IF( COMM .EQ. 'SP0WR' ) THEN
         NDFSIZ = NPOINT + 2*( NBREAK - 1 )
      ELSE
         NDFSIZ = NPOINT
      END IF

*  Create NDF with the required number of elements.
      CALL NDF_NEW( '_REAL', 1, 1, NDFSIZ, PLACE, INDF, STATUS )

*  Map the DATA array.
      CALL NDF_MAP( INDF, 'DATA', '_REAL', 'WRITE', IPDATA, NDFSIZ,
     :              STATUS )

*  Map the AXIS CENTRE array.
      CALL NDF_AMAP( INDF, 'CENTRE', 1, '_REAL', 'WRITE', IPAXIS,
     :               NDFSIZ, STATUS )

*  Copy the values from common to the DATA array, inserting 2 zeros
*  for each break if we are creating a SPECTRUM format 0 file.
      CALL WRCOPY( COMM, NBREAK, BREAK, NPOINT, FLUX, WAVE, NDFSIZ,
     :             %VAL( CNF_PVAL( IPDATA ) ),
     :             %VAL( CNF_PVAL( IPAXIS ) ), STATUS )

*  Unmap the data array.
      CALL NDF_UNMAP( INDF, 'DATA', STATUS )

*  Unmap the AXIS CENTRE array.
      CALL NDF_AUNMP( INDF, 'CENTRE', 1, STATUS )

*  Store the title (if it is not blank or "(Empty)" ).
      CALL CHR_CLEAN( TITLE )
      CALL CHR_FANDL( TITLE, F, L )
      IF( ( F .LE. L ) .AND. ( TITLE( F : L ) .NE. '(Empty)' ) )
     :   CALL NDF_CPUT( TITLE( : L ), INDF, 'TITLE', STATUS )

*  Store the data label (if it is not blank). Clean it first as there
*  seems to be some non-printable characters tagged on the end which
*  folls CHR_LEN into thinking that the string is longer than it really
*  is.
      CALL CHR_CLEAN( YLAB )
      L = CHR_LEN( YLAB )
      IF( L .NE. 0 ) CALL NDF_CPUT( YLAB( : L ), INDF, 'LABEL',
     :                               STATUS )

*  Store the label for axis 1 (if it is not blank).
      CALL CHR_CLEAN( XLAB )
      L = CHR_LEN( XLAB )
      IF( L .NE. 0 ) CALL NDF_ACPUT( XLAB( : L ), INDF, 'LABEL', 1,
     :                                STATUS )

*  If we are creating a normal DIPSO NDF, create the DIPSO_EXTRA
*  extension.
      IF( COMM .NE. 'SP0WR' ) THEN
         CALL NDF_XNEW( INDF, 'DIPSO_EXTRA', 'EXTENSION', 0, 0, XLOC,
     :                  STATUS )

*  Create the component within the extension which will hold the BREAK
*  array, and then store the BREAK values in it.
         CALL DAT_NEW1I( XLOC, 'BREAKS', NBREAK, STATUS )
         CALL CMP_PUT1I( XLOC, 'BREAKS', NBREAK, BREAK, STATUS )

*  Create the component within the extension which will hold the WORV
*  value, and then store the WORV value in it.
         CALL DAT_NEW0R( XLOC, 'WORV', STATUS )
         CALL CMP_PUT0R( XLOC, 'WORV', WORV, STATUS )

*  Anull the locator to the extension.
         CALL DAT_ANNUL( XLOC, STATUS )

      END IF

*  If an error has occurred, attempt to delete the NDF.
      IF( STATUS .NE. SAI__OK ) CALL NDF_DELET( INDF, STATUS )

*  End the NDF context. This will annul the NDF identifier, INDF.
      CALL NDF_END( STATUS )

      END
