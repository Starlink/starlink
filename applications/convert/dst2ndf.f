      SUBROUTINE DST2NDF (STATUS)
*+ 
*   Name:
*      DST2NDF

*   Purpose:
*      Converts a Figaro V2 '.DST' file to a V3 '.SDF' SGP/38 file.

*   Language:
*      Starlink Fortran 77

*   Type of module:
*      ADAM A-task

*   Invocation:
*      CALL DST2NDF (STATUS)

*   Arguments:
*      STATUS = INTEGER (Given and Returned)
*         The global status.

*   Description:
*      Converts a Figaro V2 '.DST' file to a V3 '.SDF' i.e. an NDF.

*   Usage:
*      DST2NDF IN OUT

*   ADAM Parameters:       
*      IN = Figaro file (Read)
*         The file name of the V2 file.  A file extension must not be
*         given after the name, since ".DST" is appended by the
*         application.  The file name is limited to 80 characters.
*      OUT = NDF (Write)     
*         The file name of the output NDF file.  A file extension must
*         not be given after the name, since ".SDF" is appended by the
*         application.  Since the NDF_ library is not used, a section
*         definition may not be given following the name.  The file
*         name is limited to 80 characters.

*  Examples:
*     DST2NDF OLD NEW
*        This converts the Figaro file OLD.DST file to the NDF file
*        NEW.SDF.
*     DST2NDF HORSE HORSE
*        This converts the Figaro file HORSE.DST to the NDF called
*        HORSE in file HORSE.SDF.

*  Notes:
*     The rules for the conversion are as follows:
*______________________________________________________________________
*            Figaro file          NDF
*______________________________________________________________________
*
*            .Z.DATA         ->   .DATA_ARRAY
*            .Z.ERRORS       ->   .VARIANCE (after processing)
*            .Z.QUALITY      ->   .QUALITY.QUALITY (must be BYTE array)
*                                 (see Bad-pixel handling below).
*                            ->   .QUALITY.BADBITS = 255
*            .Z.LABEL        ->   .LABEL
*            .Z.UNITS        ->   .UNITS
*            .Z.IMAGINARY    ->   .DATA_ARRAY.IMAGINARY_DATA
*            .Z.xxxx         ->   .MORE.FIGARO.Z.xxxx
*
*            .X.DATA         ->   .AXIS(1).DATA_ARRAY
*            .X.ERRORS       ->   .AXIS(1).VARIANCE  (after processing)
*            .X.WIDTH        ->   .AXIS(1).WIDTH
*            .X.LABEL        ->   .AXIS(1).LABEL
*            .X.UNITS        ->   .AXIS(1).UNITS
*            .X.LOG          ->   .AXIS(1).MORE.FIGARO.LOG
*            .X.xxxx         ->   .AXIS(1).MORE.FIGARO.xxxx
*            (Similarly for .Y .T .U .V or .W structures which are
*             renamed to AXIS(2:6) in the NDF.) 

*            .OBS.OBJECT     ->   .TITLE
*            .OBS.xxxx       ->   .MORE.FIGARO.OBS.xxxx
*
*            .FITS.xxxx      ->   .MORE.FITS.xxxx  (into value part of
*                                                   the string)
*            .COMMENTS.xxxx  ->   .MORE.FITS.xxxx  (into comment part of
*                                                   the string)
*            .FITS.xxxx.DATA ->   .MORE.FITS.xxxx  (into value part of
*                                                   the string)
*            .FITS.xxxx.DESCRIPTION -> .MORE.FITS.xxxx (into comment
*                                                   part of the string)

*            .MORE.xxxx      ->   .MORE.xxxx

*            .TABLE          ->   .MORE.FIGARO.TABLE
*            .xxxx           ->   .MORE.FIGARO.xxxx
* 

*  Bad-pixel handling:
*     The QUALITY array is only copied if the bad-pixel flag
*     (.Z.FLAGGED) is false or absent.  A simple NDF with the bad-pixel
*     flag set to false (meaning that there are no bad-pixels present)
*     is created when .Z.FLAGGED is absent or false.  Otherwise a
*     primitive NDF, where the data array is at the top level of the
*     data structure, is produced.

*  Implementation Status:
*     -  Note the data array in the NDF is of the primitive form.
*     -  The maximum number of dimensions is 7.

*  Authors:
*     JM: Jo Murray (STARLINK)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History
*     29-June-1989 (JM):
*        Original version.  See the history of DSA_CONVERT_FORMAT.
*     1992 January 31 (MJC):
*        Greatly expanded the prologue into a form suitable for
*        documentation.  Fixed bugs in the generation of card images
*        in the FITS extension, notably the inclusion of quotes around
*        character strings.  Made the OBS structure preserve its type,
*        namely OBS, within the NDF.
*     1992 February 4 (MJC):
*        The location of the FITS comment is not hardwired for long
*        (>18) character values.  Improved efficiency by removing
*        unnecessary code from a second-level loop.  Made the maximum
*        number of dimensions 7.  Handled axis width as a numeric
*        array, rather than a character scalar.
*     1992 August 15 (MJC):
*        Fixed bug that caused a phantom 2-d FITS structure to be
*        created when there is an empty FITS structure within the DST
*        file.
*     {enter_further_changes_here}

*  Bugs:
*     {note_bugs_here}

*- 
*  Type Definitions:
      IMPLICIT NONE                  ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'              ! Standard ADAM constants

*  Status:                  
      INTEGER STATUS                 ! Global status

*  External References:
      INTEGER CHR_LEN                ! Get effective length of string

*  Local Variables:
      INTEGER   NCF                  ! Length of the Figaro file name
      INTEGER   NCN                  ! Length of the NDF file name
      CHARACTER NDFFIL*(80)          ! Name of output file
      CHARACTER FIGFIL*(80)          ! Name of input file

*.

*   Check the inherited status.
      IF (STATUS.NE.SAI__OK) RETURN

*   Get the input file name.
      CALL PAR_GET0C ('IN', FIGFIL, STATUS)

*   Get the output file name.
      CALL PAR_GET0C ('OUT', NDFFIL, STATUS)
      IF (STATUS .EQ. SAI__OK) THEN

*   Add the appropriate extensions to the input and output files.
         NCF = CHR_LEN( FIGFIL ) 
         CALL CHR_APPND( '.DST', FIGFIL, NCF )
         NCN = CHR_LEN( NDFFIL ) 
         CALL CHR_APPND( '.SDF', NDFFIL, NCN )

*   Call the conversion subroutine.
         CALL CON_DST2N(FIGFIL ( :NCF ), NDFFIL ( :NCN ), STATUS)

      END IF
      
      END

