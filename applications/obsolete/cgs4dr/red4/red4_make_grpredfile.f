*+  RED4_MAKE_GRPREDFILE - Create a reduced group file for a particular group
      SUBROUTINE RED4_MAKE_GRPREDFILE( OBSRED, GRPRED, STATUS )
*    Description :
*     This routine creates a container file into which all the
*     observations obtained for a particular group will be co-added.
*     The file is based on the structure of a reduced observation, and
*     a reduced observation file is therefore used as a template.
*    Invocation :
*     CALL RED4_MAKE_GRPREDFILE( OBSRED, GRPRED, STATUS )
*    Parameters :
*     OBSRED   = CHARACTER*(*)( READ )
*           The name of the reduced observation file to be used as a
*           template.
*     GRPRED   = CHARACTER*(*)( READ )
*           The name of the reduced group file to be created.
*     STATUS   = INTEGER( UPDATE )
*           Global ADAM status. This must be ADAM__OK on entry, or the
*           routine will not execute. It will be returned ADAM__OK if
*           the routine is successful. Otherwise it will contain an
*           error status.
*    Method :
*    Deficiencies :
*     DSA status values no not follow the usual ADAM scheme.
*
*     This routine, and all the other routines in RED4 which use GEN_FILL
*     to initialise an array to zero, assumes that the host computer
*     stores a zero floating point number with all 4 bytes zero.
*
*     I have tried to stick to naming conventions which are consistent
*     with those used in the rest of the RED4 task. Hence the variable
*     for a reduced group file is called GRPRED (and not REDGRPFILE,
*     which is what I would have preferred).
*    Bugs :
*    Authors :
*     Steven Beard  (REVAD::SMB)
*     Phil Daly (JACH::PND)
*    History :
*     18-Sep-1990: Original version.                              (SMB)
*     21-Sep-1990: Modified to initialise the EXPOSED parameter
*                  to zero.                                       (SMB)
*      1-Oct-1990: Made to modify the data label.                 (SMB)
*     23-Oct-1990: Modified to initialise the SKYEXP parameter
*                  to zero.                                       (SMB)
*      1-Feb-1991: Modified so that if a SKY observation is used
*                  as a template, the group is labelled
*                  "OBJECT - DARK" instead of "SKY - DARK".       (SMB)
*      8-Feb-1991: Modified to initialise the last observation
*                  record and observation counter.                (SMB)
*     21-Feb-1991: Modified to initialise NOBJ and NSKY counters. (SMB)
*     22-Feb-1991: "Subscript out of range" error trapped when
*                  constructing label.                            (SMB)
*      1-May-1992: Put the .MORE.CGS4_INDEX structure back into
*                  the reduced groups (DRIMP/6, MISCIMP/14)       (PND)
*     22-Feb-1993: Conform to error strategy                      (PND)
*     11-Jan-1994: Allow NDFs via RESHAPE_DATA                    (PND)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'ADAMDEFNS'
      INCLUDE 'ADAMERRS'      ! Contains ADAM__OK.
      INCLUDE 'SAI_ERR'       ! Contains SAI__ERROR.
*    Import :
      CHARACTER*(*)
     :  OBSRED,               ! Name of reduced observation file.
     :  GRPRED                ! Name of reduced group file.
*    Status :
      INTEGER
     :  STATUS                ! Global ADAM status.
*    External references :
      INTEGER DSA_TYPESIZE    ! DSA type size enquiry function
      INTEGER CHR_LEN         ! Character length determining function
*    Global variables :
      INCLUDE 'RED4_COMMON.INC'  ! RED4 common block
*    Local Constants :
      INTEGER BAD             ! "Bad" value for data quality
      PARAMETER ( BAD = 1 )
      INTEGER NINFO           ! Number of "data info" items which DSA uses
      PARAMETER ( NINFO = 2 )
*    Local variables :
      DOUBLE PRECISION
     :  DIGNORE               ! Ignored argument
      INTEGER
     :  FLOATSIZE,            ! Number of bytes in FLOAT data item.
     :  BYTESIZE,             ! Number of bytes in BYTE data item.
     :  SHORTSIZE,            ! Number of bytes in SHORT data item.
     :  NDIM,                 ! Number of dimensions.
     :  DIMS( RMAXDIM ),      ! Dimensions of data arrays.
     :  NELM,                 ! Number of elements in data arrays.
     :  ADDRESS,              ! Address returned by mapping routine
     :  DATA_SLOT,            ! Slot number for data array mapped by DSA
     :  DATA_PTR,             ! Pointer to data array mapped by DSA
     :  VAR_SLOT,             ! Slot number for variance array mapped by DSA
     :  VAR_PTR,              ! Pointer to variance array mapped by DSA
     :  QUAL_SLOT,            ! Slot number for quality array mapped by DSA
     :  QUAL_PTR,             ! Pointer to quality array mapped by DSA
     :  COADDS_SLOT,          ! Slot number for coadds array mapped by DSA
     :  COADDS_PTR,           ! Pointer to coadds array mapped by DSA
     :  SKYPOS,               ! Position of the string 'SKY'
     :  CPOS,                 ! Position in character string
     :  CLEN                  ! Non-blank Length of character string.
      CHARACTER*80
     :  COADDED_INTS,         ! DTA name of COADDED_INTS structure
     :  COADDED_OBS           ! DTA name of COADDED_OBS structure
      CHARACTER*32
     :  CHAR_ARRAY( NINFO ),  ! Data info items
     :  NEWLABEL              ! Buffer for new label.
*    Internal References :
*    Local data :
*-

*   Check for error on entry
      IF ( STATUS .NE. ADAM__OK ) RETURN

*   Issue a message indicating a new reduced group file is being created.
      CALL MSG_SETC( 'GRPRED', GRPRED )
      CALL MSG_OUT( ' ', 'Creating reduced group file ^GRPRED', STATUS )

*   Open DSA
      CALL DSA_OPEN( STATUS )

*   Open the reduced observation file for input.
      CALL RED4_CHECK_INPUT( OBSRED, STATUS )
      CALL DSA_NAMED_INPUT( 'OBSRED', OBSRED, STATUS )

*   Create the reduced group file, using the reduced observation file
*   as a template. The flags given to DSA_NAMED_OUTPUT will ensure
*   that it will copy data and axis arrays, and that if a structure
*   of the same name already exists a new one will be created. Using
*   a template in this way will automatically ensure that the data
*   arrays and axis structures in the reduced group file are the
*   same. The header items in the FITS structure may not be
*   appropriate at this point, but they will be replaced when the
*   first OBJECT observation is co-added.
      CALL DSA_NAMED_OUTPUT( 'GRPRED', GRPRED, 'OBSRED', 0, 1, STATUS )

*   Define some structures
      CGS4_INDEX   = 'GRPRED.MORE.CGS4_INDEX'
      COADDED_INTS = 'GRPRED.MORE.CGS4_COADDS.COADDED_INTS'
      COADDED_OBS  = 'GRPRED.MORE.CGS4_COADDS.COADDED_OBS'

*   Now delete the COADDED_INTS structure and create an empty
*   COADDED_OBS structure of type 'COADDS_LIST'.
      CALL RED4_DELETE_STRUCTURE( COADDED_INTS, STATUS )
      CALL RED4_CREATE_STRUCTURE( COADDED_OBS, 'COADDS_LIST', STATUS )

*   Ensure the total exposure time is initialised to zero.
      CALL DSA_PUT_FITS_F( 'GRPRED', 'EXPOSED', 0.0, ' ', STATUS )
      CALL DSA_SET_EXPOSURE( 'GRPRED', 0.0, STATUS )

*   Initialise the total SKY exposure time to zero.
      CALL DSA_PUT_FITS_F( 'GRPRED', 'SKYEXP', 0.0, ' ', STATUS )

*   Initialise the number of OBJECT and number of SKY observation
*   counters to zero.
      CALL DSA_PUT_FITS_I( 'GRPRED', 'NOBJ', 0, ' ', STATUS )
      CALL DSA_PUT_FITS_I( 'GRPRED', 'NSKY', 0, ' ', STATUS )

*   Obtain the data info and modify the data label from "XXX" to
*   "\gS (XXX)", where XXX is the present label. "\gS" translates
*   to a Greek capital sigma when displayed with FIGARO/PGPLOT.
*   If a SKY observation is being used as a template, the 'SKY'
*   in the label is changed to 'OBJECT', as it is assumed the group
*   will eventually contain a sky-subtracted OBJECT observation.
*   The data label is CHAR_ARRAY(2). The data units, held in
*   CHAR_ARRAY(1), are not changed.
*   (Note that, as DSA_GET_DATA_INFO is a general purpose routine,
*   dummy arguments are needed to take the place of values we don't
*   need or don't want to change).
      CALL DSA_GET_DATA_INFO( 'GRPRED', NINFO, CHAR_ARRAY, 0,
     :  DIGNORE, STATUS )

*   (Replace 'SKY' by 'OBJECT' if necessary, and copy the result
*   back to CHAR_ARRAY(2).)
      SKYPOS = INDEX( CHAR_ARRAY(2), 'SKY' )
      IF ( SKYPOS .GT. 0 ) THEN

         CPOS = 0
         CLEN = MAX( 1, CHR_LEN( CHAR_ARRAY(2) ) )

         IF ( SKYPOS .GT. 1 ) THEN

            CALL CHR_PUTC( CHAR_ARRAY(2)(1:SKYPOS-1),
     :        NEWLABEL, CPOS )
         END IF

         CALL CHR_PUTC( 'OBJECT', NEWLABEL, CPOS )

         IF ( SKYPOS+3 .LE. CLEN ) THEN

            CALL CHR_PUTC( CHAR_ARRAY(2)(SKYPOS+3:CLEN),
     :        NEWLABEL, CPOS )
         END IF

         CHAR_ARRAY(2) = NEWLABEL(1:CPOS)
      END IF

*   Process the label, as described above.
      CPOS = 0
      CALL CHR_PUTC( 'Sum (', NEWLABEL, CPOS )
      CLEN = MAX( 1, CHR_LEN( CHAR_ARRAY(2) ) )
      CALL CHR_PUTC( CHAR_ARRAY(2)(1:CLEN), NEWLABEL, CPOS )

      CALL CHR_PUTC( ')', NEWLABEL, CPOS )

      CHAR_ARRAY(2) = NEWLABEL(1:CPOS)
      CALL DSA_SET_DATA_INFO( 'GRPRED', NINFO, CHAR_ARRAY, 0, 0.0D0,
     :   STATUS )

*   Check everything has worked so far
      IF ( STATUS .EQ. ADAM__OK ) THEN

*      Determine the number of bytes in data items of type FLOAT
*      and SHORT.
         FLOATSIZE = DSA_TYPESIZE( 'FLOAT', STATUS )
         BYTESIZE  = DSA_TYPESIZE( 'BYTE', STATUS )
         SHORTSIZE = DSA_TYPESIZE( 'SHORT', STATUS )

*      Determine the size of the data array in the reduced group file.
         CALL DSA_DATA_SIZE( 'GRPRED', RMAXDIM, NDIM, DIMS, NELM,
     :     STATUS )

*      Indicate to DSA that a data quality array will be used to
*      signal bad values.
         CALL DSA_USE_QUALITY( 'GRPRED', STATUS )

*      Map the data array for write access as floating point numbers.
         CALL DSA_MAP_DATA( 'GRPRED', 'WRITE', 'FLOAT', DATA_PTR,
     :     DATA_SLOT, STATUS )

*      Map the variance array for write access as floating point numbers.
         CALL DSA_MAP_VARIANCE( 'GRPRED', 'WRITE', 'FLOAT', VAR_PTR,
     :     VAR_SLOT, STATUS )

*      Map the quality array for write access as byte values
         CALL DSA_MAP_QUALITY( 'GRPRED', 'WRITE', 'BYTE', QUAL_PTR,
     :     QUAL_SLOT, STATUS )

*      Check the mapping of these arrays has been successful.
         IF ( STATUS .EQ. ADAM__OK ) THEN

*         Fill the data and variance arrays with zeros, and fill the
*         quality array with "bad" values.
            IF ( VERBOSE ) CALL MSG_OUT( ' ',
     :        'Initialising reduced group data array', STATUS )
            CALL GEN_FILL( FLOATSIZE*NELM, 0.0, %val(DATA_PTR) )
            IF ( VERBOSE ) CALL MSG_OUT( ' ',
     :        'Initialising reduced group variance array', STATUS )
            CALL GEN_FILL( FLOATSIZE*NELM, 0.0, %val(VAR_PTR) )
            IF ( VERBOSE ) CALL MSG_OUT( ' ',
     :        'Initialising reduced group quality array', STATUS )
            CALL GEN_FILL( BYTESIZE*NELM, BAD, %val(QUAL_PTR) )

*         Open the COADDS structure within the reduced group structure.
*         The name of this structure is constructed by appending
*         '.MORE.CGS4_COADDS' to the name of the reduced group structure.
            CLEN = MAX( 1, CHR_LEN( GRPRED ) )
            CALL DSA_NAMED_INPUT( 'COADDS',
     :        GRPRED(1:CLEN)//'.MORE.CGS4_COADDS', STATUS )

*         Make sure the data array in this structure is the same size
*         and shape as the main data array, and is of WORD type.
            CALL DSA_COERCE_DATA_ARRAY( 'COADDS', 'SHORT', NDIM,
     :        DIMS, STATUS )

*         Map the COADDS array for write access as SHORT (i.e. 2-byte
*         integer) values.
            CALL DSA_MAP_DATA( 'COADDS', 'WRITE', 'SHORT', ADDRESS,
     :        COADDS_SLOT, STATUS )
            COADDS_PTR = ADDRESS

*         Check the array has been mapped successfully.
            IF ( STATUS .EQ. ADAM__OK ) THEN

*            Fill the COADDS array with zeros.
               CALL GEN_FILL( SHORTSIZE*NELM, 0, %val(COADDS_PTR) )

*            If everything has worked, initialise the last observation
*            record and observation counter for the sky background
*            subtraction.

               LAST_OBSERVATION_NAME = ' '
               LAST_OBSERVATION_TYPE = ' '
               OBSERVATION_COUNTER = 0
            ELSE

               STATUS = SAI__ERROR
               CALL ERR_REP( ' ', 'RED4_MAKE_GRPREDFILE: '/
     :           /'Error accessing COADDS structure', STATUS )
            END IF
         ELSE

            STATUS = SAI__ERROR
            CALL ERR_REP( ' ', 'RED4_MAKE_GRPREDFILE: '/
     :        /'Error accessing arrays in reduced '/
     :        /'group structure', STATUS )
         END IF
      ELSE

         STATUS = SAI__ERROR
         CALL ERR_REP( ' ', 'RED4_MAKE_GRPREDFILE: '/
     :     /'Error opening DSA and accessing '/
     :     /'data structures', STATUS )
      END IF

*   Close DSA and tidy up, regardless of whether an error has occurred.
*   This routine will unmap all the mapped arrays and close any open
*   data structures.
      CALL DSA_CLOSE( STATUS )

      IF ( VERBOSE .AND. STATUS.EQ.SAI__OK ) THEN
        CALL MSG_SETC( 'GRPRED', GRPRED )
        CALL MSG_OUT( ' ', 'Created reduced group ^GRPRED OK', STATUS )
      END IF
      END
