#if HAVE_CONFIG_H
#  include "config.h"
#endif

C+
C                        F I G _ D F I T S _ O P E N
C
C  Routine name:
C     FIG_DFITS_OPEN
C
C  Function:
C     Opens a 'disk FITS' file for input.
C
C  Description:
C     This routine is designed to work with FIG_DFITS_READ and FIG_DFITS_HREAD
C     to provide a simple set of routines that can access 'disk FITS' files.
C     The way unformatted files are read varies between different systems,
C     and these routines exist in order to hide these differences from the
C     main application code.  FIG_DFITS_OPEN will open a 'disk FITS' file,
C     and FIG_DFITS_READ will read a 2880 byte data record from that file
C     and FIG_DFITS_HREAD will read a 2880 byte header record from it.
C     Finally, FIG_DFITS_CLOSE will close the file.
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL FIG_DFITS_OPEN (LUN,FILENAME,STATUS)
C
C  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
C
C     (>) LUN           (Integer, ref) The logical unit number to use
C                       to access the file.
C     (>) FILENAME      (Fixed string,descr) The name of the file in
C                       question.  This will normally be as provided by
C                       the parameter system.  The string will be 'case-
C                       sensitive but not case-prescriptive' - the routine
C                       will first try the string exactly as provided, and
C                       if it does not find such a file will fold it to
C                       the preferred case for the system in question.
C     (<) STATUS        (Integer,ref) Status for the operation.  0=>OK,
C                       non-zero => unable to open file.  If an error has
C                       occurred, suitable error messages will be output
C                       by this routine through PAR_WRUSER calls.
C
C  External variables used:
C     Only internal variables used by the FIG_DFITS_xxx routines.
C
C  External subroutines / functions used:
C     GEN_FORTERR, ICH_LEN, PAR_WRUSER, ICH_DFOLD
C
C  Prior requirements:  None.
C
C  Support: Keith Shortridge, AAO
C
C  Version date: 5th March 1993.
C-
C  Note:
C     This version is for a UNIX system (despite some of the VMS like
C     subroutines it uses - it is assumed that there are UNIX versions
C     of these available), and has been tested on a SUN and a DecStation.
C
C  Subroutine / function details:
C     ICH_LEN      Position of last non-blank character in string.
C     GEN_FORTERR  Returns text describing a FORTRAN error code.
C     PAR_WRUSER   Output a message to a Figaro user.
C     ICH_DFOLD    Folds a string into lower case.
C
C  History:
C     17th Aug 1990.   Original version.  KS / AAO.
C      6th Mar 1993.   Use of pre-processor introduced to handle differences
C                      in record length specifications. KS/AAO.
C     19th Jul 1993.   Disuse pre-processor.  Disuse STR$DNCASE.
C     26th Jul 1994.   Make common blocks SAVE. HME / UoE, Starlink.
C     25th Jul 1996.   Linux version from sun4 version. MJCL / Starlink, UCL.
C      9th Jul 2004    Convert to FPP format for autoconf. AA/ Starlink, Exeter.
C+
      SUBROUTINE FIG_DFITS_OPEN (LUN,FILENAME,STATUS)
C
C     Parameters
C
      INTEGER LUN,STATUS
      CHARACTER*(*) FILENAME
C
C     Functions used
C
      INTEGER ICH_LEN, ICH_DFOLD
C
C     Global variables
C
      INTEGER   LU                   
      INTEGER   RECNUM               
      COMMON /FIG_DFITS/ LU,RECNUM
      SAVE /FIG_DFITS/
C
C     Local variables
C
      CHARACTER ERROR*64             
      CHARACTER STRING*80
      LOGICAL   EXIST                
      CHARACTER FILE*132 
      INTEGER   IGNORE
      INTEGER   RECLEN

C   Use autoconf without trying to do variable substitution
#if FC_RECL_UNIT == 1
       PARAMETER ( BYTEPRU = 1 )
#elif FC_RECL_UNIT == 2
       PARAMETER ( BYTEPRU = 2 )
#elif FC_RECL_UNIT == 4
       PARAMETER ( BYTEPRU = 4 )
#else
 error "Impossible FC_RECL_UNIT"
#endif

C   Calculate record length in local units, not enitrely sure about
C   this. Have to check exactly what FC_RECL_UNIT returns on each OS,
C   values should be as below;
C
C   ix86_linux =  2880    alpha_OSF1 = 720    sun4_Solaris = 2880       
      RECLEN = 2880 / BYTEPRU           

C
C     See if the file exists, either as specified or as a lower case
C     version.
C
      STATUS = 0
      LU = 0
      FILE=FILENAME
      INQUIRE (FILE=FILE,EXIST=EXIST,IOSTAT=IGNORE)
      IF (.NOT.EXIST) THEN
         IGNORE=ICH_DFOLD(FILE)
         INQUIRE (FILE=FILE,EXIST=EXIST,IOSTAT=IGNORE)
      END IF
      IF (.NOT.EXIST) THEN
         STRING='Unable to open disk FITS file '/
     :                         /FILENAME(:ICH_LEN(FILENAME))
         CALL PAR_WRUSER(STRING,IGNORE)
         CALL PAR_WRUSER ('File does not exist.',IGNORE)
         STATUS = 1
      ELSE
C
C        File exists, so open it and record logical unit number
C        in common.  We want a RECL of 2880 bytes, but the units used
C        for RECL varies rom system to system.
C
         OPEN (UNIT=LUN,FILE=FILE,STATUS='OLD',ACCESS='DIRECT',
     :                    FORM='UNFORMATTED',RECL=RECLEN,IOSTAT=STATUS)
         IF (STATUS.EQ.0) THEN
            LU = LUN
         ELSE
            CALL PAR_WRUSER ('Error opening disk FITS file '/
     :                                   /FILE(:ICH_LEN(FILE)),IGNORE)
            CALL GEN_FORTERR (STATUS,.FALSE.,ERROR)
            CALL PAR_WRUSER (ERROR(:ICH_LEN(ERROR)),IGNORE)
         END IF
C
C        Set the record number in common.
C
         RECNUM = 1
      END IF
C
      END
C+
C                      F I G _ D F I T S _ H R E A D
C
C  Routine name:
C     FIG_DFITS_HREAD
C
C  Function:
C     Reads a 2880 byte header record from a disk FITS file.
C
C  Description:
C     This routine reads a single logical 2880 byte header record from the
C     'disk FITS' file that has been opened using FIG_DFITS_OPEN.  This
C     routine is almost identical to FIG_DFITS_READ except that the record
C     read is returned in a character string.
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL FIG_DFITS_HREAD (HEADER,BUFFER,STATUS)
C
C  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
C
C     (<) HEADER        (Fixed string,descr) A 2880 byte character string
C                       into which the header record is to be read.
C     (W) BUFFER        (Numeric array of any type, ref) An array of 2880
C                       bytes that - depending on the system in use - may
C                       have to be used as an intermediate buffer for the
C                       I/O.
C     (<) STATUS        (Integer,ref) Status for the operation.  0=>OK,
C                       non-zero => unable to read record. If an error has
C                       occurred, suitable error messages will be output
C                       by this routine through PAR_WRUSER calls.
C
C  External variables used:
C     Only internal variables used by the FIG_DFITS_xxx routines.
C
C  External subroutines / functions used:
C     FIG_DFITS_READ, PAR_WRUSER
C
C  Prior requirements:  File must have been opened by FIG_DFITS_OPEN.
C
C  Support: Keith Shortridge, AAO
C
C  Version date: 17th Aug 1990.
C-
C  Subroutine / function details:
C     FIG_DFITS_READ    Read next 2880 byte record into buffer.
C     PAR_WRUSER        Output a message to a Figaro user.
C
C  History:
C     17th Aug 1990.   Original version.  KS / AAO.
C+
      SUBROUTINE FIG_DFITS_HREAD (HEADER,BUFFER,STATUS)
C
C     Parameters
C
      INTEGER STATUS
      BYTE BUFFER(2880)
      CHARACTER*(*) HEADER
C
C     Local variables
C
      INTEGER I                      
      INTEGER IGNORE
C
C     Check validity of buffer and check that file is open.
C
      STATUS = 0
      IF (LEN(HEADER).LT.2880) THEN
         CALL PAR_WRUSER (
     :       'Error reading header record from disk FITS file.',IGNORE)
         CALL PAR_WRUSER(
     :       'Header string too small for FITS header record.',IGNORE)
         STATUS = 1
      ELSE
C
C        Read record.
C
         CALL FIG_DFITS_READ (BUFFER,STATUS)
C
C        If OK (FIG_DFITS_READ will have output any error messages if not)
C        copy numeric array into character array.
C
         IF (STATUS.EQ.0) THEN
            DO I=1,2880
               HEADER(I:I)=CHAR(BUFFER(I))
            END DO
         END IF
      END IF
C
      END
C+
C                      F I G _ D F I T S _ R E A D
C
C  Routine name:
C     FIG_DFITS_READ
C
C  Function:
C     Reads a 2880 byte record from a disk FITS file.
C
C  Description:
C     This routine reads a single logical 2880 byte record from the
C     'disk FITS' file that has been opened using FIG_DFITS_OPEN.
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL FIG_DFITS_HREAD (RECORD,STATUS)
C
C  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
C
C     (<) RECORD        (Numeric array of any type, ref) An array of 2880
C                       bytes that will receive the next logical record
C                       in the disk FITS file.
C     (<) STATUS        (Integer,ref) Status for the operation.  0=>OK,
C                       non-zero => unable to read record. If an error has
C                       occurred, suitable error messages will be output
C                       by this routine through PAR_WRUSER calls.
C
C  External variables used:
C     Only internal variables used by the FIG_DFITS_xxx routines.
C
C  External subroutines / functions used:
C     ICH_LEN, GEN_FORTERR
C
C  Prior requirements:  The file must have been opened by FIG_DFITS_OPEN.
C
C  Support: Keith Shortridge, AAO
C
C  Version date: 17th Aug 1990.
C-
C  Subroutine / function details:
C     ICH_LEN      Position of last non-blank character in string.
C     GEN_FORTERR  Returns text describing a FORTRAN error code.
C     PAR_WRUSER   Output a message to a Figaro user.
C
C  History:
C     17th Aug 1990.   Original version.  KS / AAO.
C     26th Jul 1994.   Make common blocks SAVE. HME / UoE, Starlink.
C+
      SUBROUTINE FIG_DFITS_READ (RECORD,STATUS)
C
C     Parameters
C
      INTEGER STATUS
      BYTE RECORD(2880)
C
C     Functions
C
      INTEGER ICH_LEN
C
C     Global variables
C
      INTEGER   LU                   
      INTEGER   RECNUM               
      COMMON /FIG_DFITS/ LU,RECNUM
      SAVE /FIG_DFITS/
C
C     Local variables
C
      INTEGER   IGNORE               
      CHARACTER ERROR*64             
C
C     Check validity of buffer and check that file is open.
C
      STATUS = 0
      IF (LU.EQ.0) THEN
         ERROR='No disk FITS file open.'
         STATUS = 1
      ELSE
C
C        Read record.
C
         READ (LU,REC=RECNUM,IOSTAT=STATUS) RECORD
         IF (STATUS.NE.0) THEN
            CALL GEN_FORTERR (STATUS,.FALSE.,ERROR)
            STATUS = 1
         END IF
         RECNUM = RECNUM + 1
      END IF 
C
C     Log any error
C
      IF (STATUS.NE.0) THEN
         CALL PAR_WRUSER ('Error reading record from disk FITS file.',
     :                                                          IGNORE)
         CALL PAR_WRUSER (ERROR(:ICH_LEN(ERROR)),IGNORE)
      END IF
C            
      END
C+
C                      F I G _ D F I T S _ C L O S E
C
C  Routine name:
C     FIG_DFITS_CLOSE
C
C  Function:
C     Closes a disk FITS file opened by FIG_DFITS_OPEN.
C
C  Description:
C     This routine closes any file opened by FIG_DFITS_OPEN. 
C     If no file was open, this routine does nothing.
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL FIG_DFITS_CLOSE (STATUS)
C
C  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
C
C     (<) STATUS        (Integer,ref) Status for the operation.  0=>OK,
C                       non-zero => error closing file. If an error has
C                       occurred, suitable error messages will be output
C                       by this routine through PAR_WRUSER calls.
C
C  External variables used:
C     Only internal variables used by the FIG_DFITS_xxx routines.
C
C  External subroutines / functions used: None.
C
C  Prior requirements:  The file must have been opened by FIG_DFITS_OPEN.
C
C  Support: Keith Shortridge, AAO
C
C  Version date: 17th Aug 1990.
C-
C  History:
C     17th Aug 1990.   Original version.  KS / AAO.
C     26th Jul 1994.   Make common blocks SAVE. HME / UoE, Starlink.
C+
      SUBROUTINE FIG_DFITS_CLOSE (STATUS)
C
C     Parameters
C
      INTEGER STATUS
C
C     Global variables
C
      INTEGER   LU                   
      INTEGER   RECNUM               
      COMMON /FIG_DFITS/ LU,RECNUM
      SAVE /FIG_DFITS/
C
C     Local variables
C
      INTEGER   IGNORE               
C
C     If file is open, close it.  Always return good status.
C
      IF (LU.NE.0) THEN
         CLOSE (LU,IOSTAT=IGNORE) 
         LU = 0
      END IF
      STATUS = 0
C
      END
