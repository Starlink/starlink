       SUBROUTINE WRNDF( COMM, NDFNAM, WORV, TITLE, STATUS )
*+
*  Name:
*     WRNDF

*  Purpose:
*     Save current array data to an NDF.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL WRNDF( COMM, NDFNAM, WORV, TITLE, STATUS )

*  Description:
*     The NDF is created as the top level object in a new container
*     file.  The FLUX data is copied from the common array to the NDF's
*     DATA array. The WAVE data is copied from common to the NDF's AXIS
*     CENTRE array. An appropriate SpecFrame is created and added into
*     the default NDF WCS FrameSet. The supplied title is stored. The NDF
*     label is set to the current Y axis label. The label for axis 1 is
*     set to the current X axis label. A DIPSO_EXTRA extension is created
*     containing the WORV value and the current BREAK array. If an
*     error occurs during this routine the new NDF is deleted.

*  Arguments:
*     COMM = CHARACTER * ( * ) (Given)
*        The DIPSO command name which invoked this routine.
*     NDFNAM = CHARACTER * ( * ) (Given)
*        The name of the NDF.
*     WORV = REAL (Given)
*        The WORV value.
*     TITLE = CHARACTER* ( * ) (Given)
*        The title.
*     STATUS = LOGICAL (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     23-AUG-1994 (DSB):
*        Original version.
*     13-DEC-1995 (DSB):
*        Modified to use NDF_PLACE instead of HDS_NEW.
*     17-FEB-2003 (DSB):
*        Modified to add a WCS component to the NDF.
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
      INCLUDE 'AST_PAR'          ! AST__ constants and functions
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Global Variables:
      INCLUDE 'DECLARE_STKS'     ! DIPSO array sizes, etc.
*        ASIZE1 = INTEGER (Read)
*           The declared size of the X and Y current arrays.

      INCLUDE 'DECLARE_DATA'     ! DIPSO current arrays
*        MAXBRK = INTEGER (Read)
*           The declared size of the break current array.
*        BREAK( MAXBRK ) = INTEGER (Read)
*           The pixel indices at which breaks occur in the X and Y
*           arrays.
*        FLUX( ASIZE1 ) = REAL (Read)
*           The data value at each element.
*        NBREAK = INTEGER (Read)
*           The number of breaks stored in the common BREAK array. The
*           first is stored in BREAK(1) and the last in BREAK(NBREAK).
*        NPOINT = INTEGER (Read)
*           The number of data elements used in FLUX and WAVE, starting
*           at element 1.
*        WAVE( ASIZE1 ) = REAL (Read)
*           The X value (usually wavelength or velocity) at the
*           corresponding element in the FLUX array.

      INCLUDE 'DECLARE_LBLS'     ! DIPSO current arrays labels
*        XLAB = CHARACTER*100 (Read)
*           The label for the current array X axis.
*        YLAB = CHARACTER*100 (Read)
*           The label for the current array Y axis.


*  Arguments Given:
      CHARACTER COMM*(*)
      CHARACTER NDFNAM*(*)
      REAL WORV
      CHARACTER TITLE*(*)

*  Status:
      INTEGER STATUS

*  External References:
      INTEGER CHR_LEN                   ! Used length of a string

*  Local Constants:
      REAL C                            ! Speed of light (km/s)
      PARAMETER ( C = 2.9979246E+05 )

*  Local Variables:
      CHARACTER
     :        BUFF*50,                  ! Buffer for formatted attribute value
     :        ROOT*9,                   ! Root of stack array names
     :        XLOC*(DAT__SZLOC)         ! Locator to DIPSO_EXTRA extension

      INTEGER
     :        F,                        ! Index of first non-blank character
     :        INDF,			! NDF identifier
     :        IPAXIS,                   ! Pointer to mapped AXIS CENTRE array
     :        IPDATA,                   ! Pointer to mapped DATA array
     :        IWCS,                     ! Pointer to WCS FrameSet
     :        JUNK,                     ! Pointer to an unused FrameSet
     :        L,                        ! Index of last non-blank character
     :        NDFSIZ,                   ! Size of the NDF
     :        PLACE,                    ! Place holder for new NDF
     :        SPCFRM                    ! Pointer to new SpecFrame
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
         CALL ERR_REP( 'WRNDF_ERR1', 'Illegal file type ''^TYPE'' '//
     :                 'given. Use ''.sdf'' or omit the file type '//
     :                 'altogether.', STATUS )
      END IF

*  Begin an AST context.
      CALL AST_BEGIN( STATUS )

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

*  Now create a WCS component within the NDF: we create a SpecFrame
*  describing the contents of the AXIS structure, and then add it into
*  the default NDF WCS FrameSet using a UnitMap to connect it to the AXIS
*  Frame. First create a default SpecFrame.
      SPCFRM = AST_SPECFRAME( ' ', STATUS )

*  Now set its attributes appropriately.
      IF( WORV .EQ. 1.0 ) THEN
         CALL AST_SETC( SPCFRM, 'SYSTEM', 'WAVE', STATUS )
         CALL AST_SETC( SPCFRM, 'UNIT', 'Angstrom', STATUS )
      ELSE
         CALL AST_SETC( SPCFRM, 'SYSTEM', 'VOPT', STATUS )
         CALL AST_SETC( SPCFRM, 'UNIT', 'km/s', STATUS )
         WRITE( BUFF, * ) WORV*C,' Angstrom'
         CALL AST_SETC( SPCFRM, 'RESTFREQ', BUFF, STATUS )
      END IF

*  Get the default WCS FrameSet for the NDF. This will contain GRID,
*  PIXEL and AXIS Frames.
      CALL NDF_GTWCS( INDF, IWCS, STATUS )

*  Find the AXIS Frame and make it the current Frame within the IWCS
*  FrameSet.
      JUNK = AST_FINDFRAME( IWCS, AST_FRAME( 1, 'DOMAIN=AXIS', STATUS ),
     :                      ' ', STATUS )

*  Add the SpecFrame into the IWCS FrameSet, using a 1-dimensional
*  UnitMap to connect it to the current (AXIS) Frame.
      CALL AST_ADDFRAME( IWCS, AST__CURRENT, AST_UNITMAP( 1, ' ',
     :                                                    STATUS ),
     :                   SPCFRM, STATUS )

*  Store the modified FrameSet back in the NDF.
      CALL NDF_PTWCS( IWCS, INDF, STATUS )

*  If an error has occurred, attempt to delete the NDF.
      IF( STATUS .NE. SAI__OK ) CALL NDF_DELET( INDF, STATUS )

*  End the NDF context. This will annul the NDF identifier, INDF.
      CALL NDF_END( STATUS )

*  End the AST context. This will annul all AST objects references.
      CALL AST_END( STATUS )

      END
