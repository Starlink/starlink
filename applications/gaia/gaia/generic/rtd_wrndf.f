      SUBROUTINE RTD_WRNDF( NAME, TYPE, ID, IP, NX, NY, COMP, HEAD,
     :                      NHEAD, STATUS )
*+
*  Name:
*     RTD_WRNDF

*  Purpose:
*     Write a new NDF with the given NDF component using an existing
*     NDF.

*  Language:
*     Fortran-77

*  Invocation:
*     CALL RTD_WRNDF( NAME, TYPE, ID, IP, NX, NY, COMP, HEAD, NHEAD,
*                     STATUS )

*  Description:
*      This routine copies a given NDF, writing the data pointed to
*      by IP into it as the appropriate component. It also copies any
*      quality information that has been generated for this NDF.

*  Arguments:
*     NAME = CHARACTER * ( * ) (Given)
*        The name of the NDF to be created.
*     TYPE = INTEGER (Given)
*        The data type of the image. This will be one of the codes:
*            8 = byte image
*           -8 = X image data (written as unsigned byte).
*           16 = signed word
*          -16 = unsiged word
*           32 = integer
*          -32 = floating point
*        Obviously these types limit those that an NDF can have.
*     ID = INTEGER (Given)
*        An NDF identifier. If 0 then a new NDF is created.
*     IP = INTEGER (Given)
*        Pointer to the image data.
*     NX = INTEGER (Given)
*        The X dimension of the NDF.
*     NY = INTEGER (Given)
*        The Y dimension of the NDF.
*     COMP = CHARACTER * ( * ) (Given)
*        The data component of the NDF to replace (must be 'data' or
*        'variance', if 'quality' then the behaviour is undefined).
*     HEAD = CHARACTER( NHEAD ) * ( * ) (Given)
*        The FITS headers to write out with the image.
*     NHEAD = INTEGER (Given)
*        Number of headers given.
*     STATUS = INTEGER (Returned)
*        The global status on exit.

*  Authors:
*     PDRAPER: Peter Draper (STARLINK - Durham University)
*     {enter_new_authors_here}

*  Copyright:
*     Copyright (C) 1997 Central Laboratory of the Research Councils

*  History:
*     16-JAN-1996 (PDRAPER):
*        Original version.
*     28-JUN-1996 (PDRAPER):
*        Now propagates NDF.
*     03-DEC-1997 (PDRAPER):
*        Now accepts the name of the component to replace.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE             ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'         ! Standard SAE constants
      INCLUDE 'DAT_PAR'         ! HDS/DAT parameters
      INCLUDE 'NDF_PAR'         ! NDF parameters
      INCLUDE 'AST_PAR'         ! AST parameters

*  Arguments Given:
      CHARACTER * ( * ) NAME
      INTEGER TYPE
      INTEGER ID
      INTEGER IP
      INTEGER NX
      INTEGER NY
      CHARACTER * ( * ) COMP
      INTEGER NHEAD
      CHARACTER * ( * ) HEAD( NHEAD )

*  Status:
      INTEGER STATUS            ! Global status

*  External References:
      EXTERNAL CHR_LEN
      INTEGER CHR_LEN           ! Used length of string

*  Local Variables:
      CHARACTER * ( DAT__SZLOC ) FITLOC ! Locator to FITS extension
      CHARACTER * ( NDF__SZTYP ) DTYPE ! Type of NDF data array
      INTEGER DIM( 1 )          ! Dimension of FITS extension
      INTEGER EL                ! Number of pixels in NDF
      INTEGER I                 ! Loop variable
      INTEGER IAT               ! Position of .sdf in name
      INTEGER IDNEW             ! Identifier of new NDF
      INTEGER IERR              ! Not used
      INTEGER IPDAT             ! Pointer to NDF data array
      INTEGER IPFIT             ! Pointer to mapped FITS extension
      INTEGER IPQUAL            ! Pointer to quality data
      INTEGER NERR              ! Not used
      INTEGER NFITS             ! Number of FITS cards mapped.
      INTEGER PLACE             ! NDF placeholder
      INTEGER UBND( 2 )         ! NDF dimensions
      INTEGER IWCS              ! Pointer to WCS object
      LOGICAL HVQUAL            ! Do we have some additional quality
      LOGICAL THERE             ! FITS extension exists
*.

*  Set the global status.
      STATUS = SAI__OK

*  Start an NDF context.
      CALL NDF_BEGIN

*  Remove the .sdf part of the name (it if exists).
      IAT = INDEX( NAME, '.sdf' )
      IF ( IAT .EQ. 0 ) THEN
         IAT = CHR_LEN( NAME )
      ELSE
         IAT = IAT - 1
      END IF
      IF ( IAT .EQ. 0 ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( ' ', 'No name given', STATUS )
      END IF

*  Attempt to open the new NDF.
      CALL NDF_PLACE( DAT__ROOT, NAME( :IAT ), PLACE, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN

*  Set the size of the NDF.
         UBND( 1 ) = NX
         UBND( 2 ) = NY

*  Define the NDF data type.
         IF ( TYPE .EQ. -32 ) THEN
            DTYPE = '_REAL'
         ELSE IF ( TYPE .EQ. 32 ) THEN
            DTYPE = '_INTEGER'
         ELSE IF ( TYPE .EQ. 16 ) THEN
            DTYPE = '_WORD'
         ELSE IF ( TYPE .EQ. -16 ) THEN
            DTYPE = '_UWORD'
         ELSE IF ( TYPE .EQ. 8 ) THEN
            DTYPE = '_BYTE'
         ELSE IF ( TYPE .EQ. -8 ) THEN
            DTYPE = '_UBYTE'
         END IF

*  If the NDF identifier is 0 then create a new NDF, otherwise propagate
*  the NDF.
         IF ( ID .EQ. 0 ) THEN

*  Create the NDF (use a primitive as no lower bounds info).
            CALL NDF_NEWP( DTYPE, 2, UBND, PLACE, IDNEW, STATUS )
         ELSE

*  Copy the existing NDF.
            CALL NDF_COPY( ID, PLACE, IDNEW, STATUS )
         END IF

*  Now map in the appropriate component.
         CALL NDF_MAP( IDNEW, COMP, DTYPE, 'WRITE', IPDAT, EL,
     :                 STATUS )

*  And copy the data into it.
         IF ( DTYPE .EQ. '_REAL' ) THEN
            CALL VEC_RTOR( .FALSE., EL, %VAL( IP ),
     :                     %VAL( IPDAT ), IERR, NERR, STATUS )
         ELSE IF ( DTYPE .EQ. '_INTEGER' ) THEN
            CALL VEC_ITOI( .FALSE., EL, %VAL( IP ),
     :                     %VAL( IPDAT ), IERR, NERR, STATUS )
         ELSE IF ( DTYPE .EQ. '_WORD' ) THEN
            CALL VEC_WTOW( .FALSE., EL, %VAL( IP ),
     :                     %VAL( IPDAT ), IERR, NERR, STATUS )
         ELSE IF ( DTYPE .EQ. '_UWORD' ) THEN
            CALL VEC_UWTOUW( .FALSE., EL, %VAL( IP ),
     :                       %VAL( IPDAT ), IERR, NERR, STATUS )
         ELSE IF ( DTYPE .EQ. '_BYTE' ) THEN
            CALL VEC_BTOB( .FALSE., EL, %VAL( IP ),
     :                     %VAL( IPDAT ), IERR, NERR, STATUS )
         ELSE IF ( DTYPE .EQ. '_UBYTE' ) THEN
            CALL VEC_UBTOUB( .FALSE., EL, %VAL( IP ),
     :                     %VAL( IPDAT ), IERR, NERR, STATUS )
         END IF

*  Check for any new quality information
         IF ( ID .NE. 0 ) THEN
            CALL RTD1_AQUAL( ID, .TRUE., IPQUAL, HVQUAL )
            IF ( HVQUAL ) THEN
               CALL NDF_MAP( IDNEW, 'QUALITY', '_BYTE', 'WRITE', IPDAT,
     :                       EL, STATUS )
               CALL VEC_BTOB( .FALSE., EL, %VAL( IPQUAL ),
     :                        %VAL( IPDAT ), IERR, NERR, STATUS )
            END IF
         END IF

*  Use the header information to construct the FITS extension, and
*  then check this for a WCS system that can be stored as an NDF
*  component. The header information that we have replaces any that
*  exists already (it should be fundermentally the same).
         CALL NDF_XSTAT( IDNEW, 'FITS', THERE, STATUS )
         IF ( THERE ) THEN
            CALL NDF_XDEL( IDNEW, 'FITS', STATUS )
         END IF
         DIM( 1 ) = NHEAD
         CALL NDF_XNEW( IDNEW, 'FITS', '_CHAR*80', 1, DIM, FITLOC,
     :                  STATUS )
         CALL DAT_MAPV( FITLOC, '_CHAR*80', 'WRITE', IPFIT, NFITS,
     :                  STATUS )
         CALL RTD1_CHEAD( HEAD, NHEAD, %VAL( IPFIT ), STATUS, %VAL(80) )

*  Look for a WCS that can be saved in the NDF WCS component.
         CALL RTD1_DEWCS( HEAD, NHEAD, IWCS, STATUS )
         IF ( IWCS .NE. AST__NULL ) THEN 
            CALL NDF_PTWCS( IWCS, IDNEW, STATUS )
            CALL AST_ANNUL( IWCS, STATUS )
         END IF
         CALL DAT_UNMAP( FITLOC, STATUS )
      END IF

*  Make sure that all the NDF resources etc. are released.
      CALL NDF_END( STATUS )
      END
