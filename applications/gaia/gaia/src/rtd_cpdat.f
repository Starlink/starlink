      SUBROUTINE RTD_CPDAT( ID, IP, COMP, STATUS )
*+
*  Name:
*     RTD_CPDAT

*  Purpose:
*     Copy an NDF's data component into memory using chunking.

*  Language:
*     Fortran-77

*  Invocation:
*     CALL RTD_WRNDF( ID, IP, COMP, STATUS )

*  Description:
*      This routine copies a given NDF's component into another
*      piece of memory. The NDF is copied in chunks to reduce the total
*      amount of memory required.

*  Arguments:
*     ID = INTEGER (Given)
*        An NDF identifier.
*     IP = INTEGER (Given)
*        Pointer to the memory to copy the image data into. This must be
*        large enough.
*     COMP = CHARACTER * ( * ) (Given)
*        The NDF data component to copy.
*     STATUS = INTEGER (Returned)
*        The global status on exit.

*  Copyright:
*     Copyright (C) 1997 Central Laboratory of the Research Councils

*  Authors:
*     PDRAPER: Peter Draper (STARLINK - Durham University)
*     {enter_new_authors_here}

*  History:
*     16-JAN-1997 (PDRAPER):
*        Original version.
*     03-DEC-1997 (PDRAPER):
*        Now copies a named component rather than 'DATA'.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE             ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'         ! Standard SAE constants
      INCLUDE 'NDF_PAR'         ! NDF parameters
      INCLUDE 'PRM_PAR'         ! Primitive data constants

*  Arguments Given:
      INTEGER ID
      INTEGER IP
      CHARACTER * ( * ) COMP

*  Status:
      INTEGER STATUS            ! Global status

*  Local constants:
      INTEGER MXPIX
      PARAMETER ( MXPIX = 250000 )

*  Local Variables:
      CHARACTER * ( NDF__SZTYP ) DTYPE ! Type of NDF data array
      INTEGER NPIX              ! Number of pixels in NDF
      INTEGER IPDAT             ! Pointer to NDF data array
      INTEGER IPNOW             ! Pointer to current position in output array
      INTEGER NERR, IERR        ! Unused
      INTEGER I                 ! Loop variable
      LOGICAL VALID             ! NDF validity flag
      INTEGER NCHUNK            ! Number of chunks used to copy data
      INTEGER ICH               ! Chunk identifier
*.

*  Set the global status.
      STATUS = SAI__OK

*  Start an NDF context.
      CALL NDF_BEGIN

*  Check that the NDF is valid.
      CALL NDF_VALID( ID, VALID, STATUS )
      IF ( VALID ) THEN 

*  Get the size and type of the NDF.
         CALL NDF_TYPE( ID, COMP, DTYPE, STATUS )
         IF ( DTYPE .EQ. '_DOUBLE' ) DTYPE = '_REAL'
         IF ( DTYPE .EQ. '_UBYTE' ) DTYPE = '_WORD'

*  Determine the number of chunks to needed to copy the data.
         CALL NDF_NCHNK( ID, MXPIX, NCHUNK, STATUS )

*  Loop for all chunks copying the data into the new memory.
         IPNOW = IP
         DO 1 I = 1, NCHUNK

*  Access the chunk and map it in.
            CALL NDF_CHUNK( ID, MXPIX, I, ICH, STATUS )
            CALL NDF_MAP( ICH, COMP, DTYPE, 'READ', IPDAT, NPIX, 
     :                    STATUS )

*  And copy the data.
            IF ( DTYPE .EQ. '_REAL' ) THEN
               CALL VEC_RTOR( .FALSE., NPIX, %VAL( IPDAT ),
     :                        %VAL( IPNOW ), IERR, NERR, STATUS )
               IPNOW = IPNOW + VAL__NBR * NPIX
            ELSE IF ( DTYPE .EQ. '_INTEGER' ) THEN
               CALL VEC_ITOI( .FALSE., NPIX, %VAL( IPDAT ),
     :                     %VAL( IPNOW ), IERR, NERR, STATUS )
               IPNOW = IPNOW + VAL__NBI * NPIX
            ELSE IF ( DTYPE .EQ. '_WORD' )THEN
               CALL VEC_WTOW( .FALSE., NPIX, %VAL( IPDAT ),
     :                        %VAL( IPNOW ), IERR, NERR, STATUS )
               IPNOW = IPNOW + VAL__NBW * NPIX
            ELSE IF ( DTYPE .EQ. '_UWORD' ) THEN
               CALL VEC_UWTOUW( .FALSE., NPIX, %VAL( IPDAT ),
     :                          %VAL( IPNOW ), IERR, NERR, STATUS )
               IPNOW = IPNOW + VAL__NBUW * NPIX
            ELSE IF ( DTYPE .EQ. '_BYTE' ) THEN
               CALL VEC_BTOB( .FALSE., NPIX, %VAL( IPDAT ),
     :                        %VAL( IPNOW ), IERR, NERR, STATUS )
               IPNOW = IPNOW + VAL__NBB * NPIX
            ELSE IF ( DTYPE .EQ. '_UBYTE' ) THEN
               CALL VEC_UBTOUB( .FALSE., NPIX, %VAL( IPDAT ),
     :                          %VAL( IPNOW ), IERR, NERR, STATUS )
               IPNOW = IPNOW + VAL__NBUB * NPIX
            END IF

*  Release the chunk.
            CALL NDF_ANNUL( ICH, STATUS )
 1       CONTINUE
      ELSE
         STATUS = SAI__ERROR
         CALL ERR_REP( 'RTD_CPDAT', 'NDF identifier is invalid', 
     :                 STATUS )
      END IF

*  Make sure that all the NDF resources etc. are released.
      CALL NDF_END( STATUS )
      END
