      SUBROUTINE PHO1_WRRES( FD, OBJIND, OBJINF, PSFIND, PSFINF,
     :                       SKYIND, SKYINF, OPTIMA, STATUS )
*+
*  Name:
*     PHO1_WRRES

*  Purpose:
*     Writes the results of AUTOPHOTOM to a file.

*  Language:
*     Starlink Fortran-77

*  Invocation:
*     CALL PHO1_WRRES( FD, OBJIND, OBJINF, SKYIND, SKYINF, OPTIMA, STATUS )

*  Description:
*     Writes the results of a run of AUTOPHOTOM to a formatted
*     file. The results are stored in several GRP groups as strings.

*  Arguments:
*     FD = INTEGER (given)
*        Results file FIO file descriptor.
*     OBJIND = INTEGER (Given)
*        GRP group containing the indices of the objects.
*     OBJINF = INTEGER (Given)
*        GRP group containing the information about the object aperture.
*     PSFIND = INTEGER (Given)
*        GRP group of PSF indices.
*     PSFINF = INTEGER (Given)
*        GRP group of additional information describing PSF aperture.
*     SKYIND = INTEGER (Given)
*        GRP group containing the indices of the objects that the sky
*        regions correspond too.
*     SKYINF = INTEGER (Given)
*        GRP group containing the information about sky regions
*        associated with the objects.
*     OPTIMA = LOGICAL (Given)
*        Flag to perform optimal extraction using Tim Naylor's algorithim
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     PWD: Peter Draper (STARLINK - Durham University)
*     AA: Alasdair Allan (STARLINK - Keele University)
*     {enter_new_authors_here}

*  History:
*     19-APR-1996 (PWD):
*        Original version.
*     1-FEB-1999 (AA):
*        Changes to deal with PSF stars
*     2-FEB-1999 (AA):
*        Added OPTIMA calling parameter
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE             ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'         ! Standard SAE constants
      INCLUDE 'GRP_PAR'         ! GRP constants
      INCLUDE 'PRM_PAR'         ! Primitive data constants

*  Arguments Given:
      INTEGER FD
      INTEGER OBJIND
      INTEGER OBJINF
      INTEGER PSFIND
      INTEGER PSFINF
      INTEGER SKYIND
      INTEGER SKYINF
      LOGICAL OPTIMA

*  Status:
      INTEGER STATUS            ! Global status

*  External References:
      INTEGER CHR_LEN
      EXTERNAL CHR_LEN          ! Used length of string

*  Local Variables:
      CHARACTER * ( GRP__SZNAM ) BUFFER ! GRP line buffer
      CHARACTER * ( VAL__SZI ) INDEXO ! Index of object as string
      CHARACTER * ( VAL__SZI ) INDEXP ! Index of PSF as string
      CHARACTER * ( VAL__SZI ) INDEXS ! Index of sky as string
      INTEGER I                 ! Loop variable
      INTEGER IAT               ! Insertion position
      INTEGER INDLEN            ! Length of index field
      INTEGER J                 ! Loop variable
      INTEGER NOBJ              ! Number of objects
      INTEGER NPSF		! Number of PSF stars
      INTEGER NSKY              ! Number of sky regions
      INTEGER NEWSKY            ! Count of object sky regions
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Find the number of objects and sky regions to process.
      CALL GRP_GRPSZ( OBJIND, NOBJ, STATUS )
      CALL GRP_GRPSZ( PSFIND, NPSF, STATUS )
      CALL GRP_GRPSZ( SKYIND, NSKY, STATUS )

      IF( OPTIMA ) THEN
*  Loop over all PSF stars writing out their details.
      INDLEN = VAL__SZI + 6
      DO 3 I = 1, NPSF
         CALL GRP_GET( PSFIND, I, 1, INDEXP, STATUS )
         BUFFER = INDEXP
         CALL GRP_GET( PSFINF, I, 1, BUFFER( INDLEN: ), STATUS )
         CALL FIO_WRITE( FD, BUFFER( :CHR_LEN( BUFFER ) ), STATUS )
*  Look for corresponding sky regions.
         NEWSKY = 0
         DO 4 J = 1, NSKY
            CALL GRP_GET( SKYIND, J, 1, INDEXS, STATUS )
            IF ( INDEXP .EQ. INDEXS ) THEN
               IF ( INDEX( BUFFER, 'annulus' ) .NE. 0 ) THEN
                  BUFFER = '#ANN  '//INDEXS
                  CALL GRP_GET( SKYINF, J, 1, BUFFER( INDLEN: ),
     :                          STATUS )
               ELSE
                  BUFFER = '#SKY'
                  IAT = 4
                  CALL CHR_PUTI( NEWSKY, BUFFER, IAT )
                  IAT = IAT + 2
                  BUFFER( IAT: ) = INDEXP
                  CALL GRP_GET( SKYINF, J, 1, BUFFER( INDLEN: ),
     :                          STATUS )
               END IF
               CALL FIO_WRITE( FD, BUFFER( :CHR_LEN( BUFFER ) ),
     :                         STATUS )
            END IF
4        CONTINUE
3     CONTINUE

      ENDIF

*  Loop over all the objects writing out their details.
      INDLEN = VAL__SZI + 6
      DO 1 I = 1, NOBJ
         CALL GRP_GET( OBJIND, I, 1, INDEXO, STATUS )
         BUFFER = INDEXO
         CALL GRP_GET( OBJINF, I, 1, BUFFER( INDLEN: ), STATUS )
         CALL FIO_WRITE( FD, BUFFER( :CHR_LEN( BUFFER ) ), STATUS )

*  Look for corresponding sky regions.
         NEWSKY = 0
         DO 2 J = 1, NSKY
            CALL GRP_GET( SKYIND, J, 1, INDEXS, STATUS )
            IF ( INDEXO .EQ. INDEXS ) THEN
               NEWSKY = NEWSKY + 1
               IF ( INDEX( BUFFER, 'annulus' ) .NE. 0 ) THEN
                  BUFFER = '#ANN  '//INDEXS
                  CALL GRP_GET( SKYINF, J, 1, BUFFER( INDLEN: ),
     :                          STATUS )
               ELSE
                  BUFFER = '#SKY'
                  IAT = 4
                  CALL CHR_PUTI( NEWSKY, BUFFER, IAT )
                  IAT = IAT + 2
                  BUFFER( IAT: ) = INDEXO
                  CALL GRP_GET( SKYINF, J, 1, BUFFER( INDLEN: ),
     :                          STATUS )
               END IF
               CALL FIO_WRITE( FD, BUFFER( :CHR_LEN( BUFFER ) ),
     :                         STATUS )
            END IF
 2       CONTINUE
 1    CONTINUE
      END
