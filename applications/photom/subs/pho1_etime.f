      SUBROUTINE PHO1_ETIME( EXSRC, CETIME, INDF, ETIME, STATUS)
*+
*  Name:
*     PHO1_ETIME

*  Purpose:
*     Determines the exposure time for an image.

*  Language:
*     Starlink Fortran-77

*  Invocation:
*      CALL PHO1_ETIME( EXSRC, CETIME, INDF, ETIME, STATUS)

*  Description:
*       This routine attempts to return an exposure time for an
*       image. Where the time is derived from is determined by the value
*       of the argument EXSRC. EXSRC should be one the values:
* 
*          - HDS
*          - CONSTANT
*          - HEADER
*
*        HDS: a fully qualified HDS path to the required object within
*        the NDF should be given as CETIME. For instance if the exposure
*        time is stored in the CCDPACK extension of an NDF, under the
*        item ETIME then a suitable return would be: 
*
*           - more.ccdpack.etime
*
*        CONSTANT: then CETIME should contain a floating point value.
*
*        HEADER: then the name of the associated item in the FITS
*        extension of the NDF should be given in CETIME.
*
*        If any of the above fails then an error is raised and ETIME is
*        returned as 1.0.

*  Arguments:
*     EXSRC = CHARACTER * ( * ) (Given)
*        The source of the exposure value. One of "HDS", "CONSTANT" or
*        "HEADER".
*     CETIME = CHARACTER * ( * ) (Given)
*        The qualifier for EXSRC. Either an HDS path to an object, an
*        numeric value, or the name of a FITS extension item.
*     INDF = INTEGER (Given)
*        Identifier of the NDF.
*     ETIME = REAL (Returned)
*        The exposure time.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     PWD: Peter Draper (STARLINK - Durham University)
*     {enter_new_authors_here}

*  History:
*     16-MAY-1998 (PWD):
*        Original version.
*     07-SEP-2004 (PWD):
*        Changed to use CNF pointers
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE             ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'         ! Standard SAE constants
      INCLUDE 'DAT_PAR'         ! HDS constants
      INCLUDE 'CNF_PAR'         ! CNF functions

*  Arguments Given:
      CHARACTER * ( * ) EXSRC
      CHARACTER * ( * ) CETIME
      INTEGER INDF

*  Arguments Returned:
      REAL ETIME

*  Status:
      INTEGER STATUS            ! Global status

*  Local Constants :
      INTEGER MXELEM
      PARAMETER ( MXELEM = 16 )

*  Local Variables:
      CHARACTER * ( DAT__SZLOC ) ALOC ! HDS locator
      CHARACTER * ( DAT__SZLOC ) BLOC ! HDS locator
      CHARACTER * ( DAT__SZNAM ) COMPS( MXELEM ) ! Component names
      CHARACTER * ( 80 ) TEXT   ! Message string
      INTEGER J                 ! Loop variable
      INTEGER NLEV              ! Number of components
      INTEGER PNTR              ! Pointer to FITS data
      INTEGER NREC              ! Number of FITS records
      LOGICAL EXISTS            ! FITS extension exists

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Set default exposure time.
      ETIME = 1.0

*  Branch according to the work we need to do.
      IF ( EXSRC .EQ. 'HDS' ) THEN 

*  Old method of HDS path first. Get a locator to the NDF.
         CALL NDF_LOC( INDF, 'READ', ALOC, STATUS )

*   Separate the path name into its components
         CALL REF_SPLIT( CETIME, MXELEM, NLEV, COMPS, STATUS )

*   Step through the components to find the exposure time
*   Trust the inherited status to skip routines if there is a problem
         DO 1 J = 1, NLEV
            CALL DAT_FIND( ALOC, COMPS( J ), BLOC, STATUS )
            CALL DAT_ANNUL( ALOC, STATUS )
            CALL DAT_CLONE( BLOC, ALOC, STATUS )
            CALL DAT_ANNUL( BLOC, STATUS )
 1       CONTINUE
      
*   Extract the exposure time
         CALL DAT_GET0R( ALOC, ETIME, STATUS )
         CALL DAT_ANNUL( ALOC, STATUS )

*   If there are any problems then return an exposure time of 1
      ELSE IF ( EXSRC .EQ. 'CONSTANT' ) THEN 

*  CETIME should just be an number.
         CALL CHR_CTOR( CETIME, ETIME, STATUS )

      ELSE IF ( EXSRC .EQ. 'HEADER' ) THEN 

*  Stored in FITS headers. Need to check the NDF for the existence of
*  this first.
         CALL NDF_XSTAT( INDF, 'FITS', EXISTS, STATUS )
         IF ( EXISTS ) THEN 

*  Locate and map the extension.
            CALL NDF_XLOC( INDF, 'FITS', 'READ', ALOC, STATUS )
            CALL DAT_MAPV( ALOC, '_CHAR*80', 'READ', PNTR, NREC, 
     :                     STATUS )

*  Attempt to find and decode the FITS header item. Note the trailing
*  %VAL*( 80 ) is required when passing characters arrays via pointers
*  on UNIX.
            CALL PHO1_GKEY( NREC, %VAL( CNF_PVAL( PNTR ) ), CETIME, 
     :                      EXISTS, ETIME, STATUS, 
     :                      %VAL( CNF_CVAL( 80 ) ) )

            IF ( STATUS .EQ. SAI__OK .AND. .NOT. EXISTS ) THEN 
               ETIME = 1.0
               STATUS = SAI__ERROR
               CALL MSG_SETC( 'ITEM', CETIME )
               CALL ERR_REP( ' ', 
     :              'Failed to locate header item: ^ITEM', STATUS ) 
            END IF
            CALL DAT_ANNUL( ALOC, STATUS )
         ELSE
            STATUS = SAI__ERROR 
            CALL ERR_REP( ' ', 'Image does not contain any headers', 
     :                    STATUS )
         END IF
      ELSE 
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'EXSRC', EXSRC )
         CALL ERR_REP( ' ', 'Unknown exposure time source: ^EXSRC',
     :                 STATUS )
      END IF

*   Print out the exposure time, if all has gone well.
      IF ( STATUS .NE. SAI__OK ) THEN
         ETIME = 1.0
         CALL ERR_REP( ' ', 'Failed to get exposure time', STATUS ) 
      ELSE 
         WRITE( TEXT, '(''Exposure time = '', F7.2 )' ) ETIME
         CALL MSG_OUT( ' ', TEXT, STATUS )
      END IF
      END

* $Id$
