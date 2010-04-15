      PROGRAM WCSCONVERTER

*  Usage:
*     WCSCONVERTER <in file> <encoding> <out file> <attrs>

*  Description:
*     Reads a FrameSet from "in file" (as a FITS header if possible,
*     otherwise as an AST dump of a FrameSet), and writes out the
*     FrameSet to "out file" using the specified encoding.

*  Parameters:
*     in file
*        A text file containing fits headers or an AST dump of a FrameSet.
*     encoding
*        The name of a FITS encoding (e.g. "FITS-WCS", "NATIVE", etc), or
*        "AST".
*     out file
*        The output file. Contains an AST dump of the FrameSet if
*        "encoding" is "AST", or a set of FITS header cards otherwise.
*     attrs
*        A list of attribute settings to apply to the FitsChan before
*        reading the input headers.


      IMPLICIT NONE
      INCLUDE 'AST_PAR'
      EXTERNAL SOURCE, SINK

      INTEGER STATUS, FC, OBJECT, IARGC, CHAN, CHR_LEN, OC
      CHARACTER FILE*80, OFILE*80, LINE*255, ENCODING*50
      CHARACTER ATTRS*200
      LOGICAL CDM

      STATUS = 0
*
* Check command line arguments have been supplied.
*
      IF( IARGC() .LT. 3 ) THEN
         WRITE(*,*) 'Usage: wcsconverter <in file> <encoding> '//
     :              '<out file> <attrs>'
         RETURN
      END IF

*
*  Use object caching to minimise allocation of new memory
*
      OC = AST_TUNE( 'ObjectCaching', 1, STATUS )
      IF( OC .NE. 0 ) THEN
         WRITE(*,'(A,I6)') 'Default ObjectCaching VALUE is ',OC
      END IF

      IF( AST_TUNE( 'ObjectCaching', AST__TUNULL, STATUS ) .NE. 1 ) THEN
         WRITE(*,'(A,I6)') 'Set ObjectCaching VALUE is ',OC
      END IF

*
* Create a FitsChan to store the FITS headers.
*
      FC = AST_FITSCHAN( AST_NULL, AST_NULL, ' ', STATUS )

*
* Apply any attribute settings to the FitsChan.
*
      IF( IARGC() .EQ. 4 ) THEN
         CALL GETARG( 4, ATTRS )
         CALL AST_SET( FC, ATTRS, STATUS )
      ELSE
         ATTRS = ' '
      END IF

*
* Open the input text file.
*
      CALL GETARG( 1, FILE )
      OPEN( UNIT=10, FILE=FILE, STATUS='OLD' )

*
* Read each line out of the text file and store it in the FitsChan.
*
      CALL ERR_MARK
      DO WHILE( .TRUE. )
         READ( 10, '(A)', END = 10 ) LINE
         CALL AST_PUTFITS( FC, LINE, 0, STATUS )
      END DO

 10   CLOSE( 10 )

*
* Set the value of CDMatrix, unless it has already been set.
*
      IF( .NOT. AST_TEST( FC, 'CDMATRIX', STATUS ) ) THEN
         CDM = AST_GETL( FC, 'CDMATRIX', STATUS )
         CALL AST_SETL( FC, 'CDMATRIX', CDM, status )
      END IF

*
* Attempt to read an Object form the FitsChan.
*
      CALL AST_CLEAR( FC, 'CARD', STATUS )
      OBJECT = AST_READ( FC, STATUS )

      IF( STATUS .NE. 0 ) CALL ERR_ANNUL( STATUS )
      CALL ERR_RLSE

* If no object was read, attempt to read an object from the text file as
* an AST dump.
      IF( OBJECT .EQ. AST__NULL ) THEN
         CALL AST_ANNUL( FC, STATUS )
         OPEN( UNIT=10, FILE=FILE, STATUS='OLD' )
         CHAN = AST_CHANNEL( SOURCE, AST_NULL, ' ', STATUS )
         OBJECT = AST_READ( CHAN, STATUS )
         CALL AST_ANNUL( CHAN, STATUS )
         CLOSE( 10 )
      END IF

*
* Abort if no object was read.
*
      IF( OBJECT .EQ. AST__NULL ) THEN
         WRITE(*,*) 'wcsconverter: no WCS could be read from ',
     :              file( : chr_len( file ) )
         RETURN


*
* Otherwise write out the object using the specified encoding.
*
      ELSE
         CALL GETARG( 3, OFILE )
         CALL DELETEFILE( OFILE )

         CALL GETARG( 2, ENCODING )
         IF( ENCODING .EQ. 'AST' .OR. ENCODING .EQ. 'ast' ) THEN
            OPEN( UNIT=10, FILE=OFILE, STATUS='NEW' )
            CHAN = AST_CHANNEL( AST_NULL, SINK, ' ', STATUS )
            IF( AST_WRITE( CHAN, OBJECT, STATUS ) .NE. 1 ) THEN
               WRITE(*,*) 'wcsconverter: WCS read from ',
     :                    file( : chr_len( file ) ),' could not be '//
     :                    'converted to ',
     :                    encoding( : chr_len(encoding ) ),' format.'
            END IF
            CALL AST_ANNUL( CHAN, STATUS )
            CLOSE( 10 )

         ELSE
            OPEN( UNIT=10, FILE=OFILE, STATUS='NEW' )
            IF( FC .EQ. AST__NULL ) THEN
               FC = AST_FITSCHAN( AST_NULL, AST_NULL, ATTRS, STATUS )
            END IF
            CALL AST_SETC( FC, 'ENCODING', ENCODING, STATUS )
            CALL AST_CLEAR( FC, 'CARD', STATUS )

            IF( AST_WRITE( FC, OBJECT, STATUS ) .NE. 1 ) THEN
               WRITE(*,*) 'wcsconverter: WCS read from ',
     :                    file( : chr_len( file ) ),' could not be '//
     :                    'converted to ',
     :                    encoding( : chr_len(encoding ) ),' format.'
            ELSE
               CALL AST_CLEAR( FC, 'CARD', STATUS )
               DO WHILE( AST_FINDFITS( FC, '%f', LINE, .TRUE.,
     :                   STATUS ) )
                 WRITE(10,'(A)') LINE( : 80 )
               END DO
            END IF
            CLOSE( 10 )

            CALL AST_ANNUL( FC, STATUS )


         END IF
         CALL AST_ANNUL( OBJECT, STATUS )
      END IF


      END


*
*  Delete a file if it exists.
*
      SUBROUTINE DELETEFILE( FILNAM )
      IMPLICIT NONE

      CHARACTER FILNAM*(*)
      LOGICAL EXISTS

      INQUIRE ( FILE   = FILNAM,
     :          EXIST  = EXISTS )

      IF( EXISTS ) THEN
         OPEN ( UNIT=10, FILE=FILNAM, STATUS='OLD' )
         CLOSE ( 10, STATUS='DELETE' )
      END IF

      END


*
*  SOURCE FUNCTION FOR AST_CHANNEL.
*
      SUBROUTINE SOURCE( STATUS )
      IMPLICIT NONE
      INTEGER STATUS
      CHARACTER BUFFER*200
      READ( 10, '(A)', END=99 ) BUFFER
      CALL AST_PUTLINE( BUFFER, LEN( BUFFER ), STATUS )
      RETURN
 99   CALL AST_PUTLINE( BUFFER, -1, STATUS )
      END

*
*  SINK FUNCTION FOR AST_CHANNEL.
*
      SUBROUTINE SINK( STATUS )
      IMPLICIT NONE
      INTEGER STATUS, L
      CHARACTER BUFFER*200

      CALL AST_GETLINE( BUFFER, L, STATUS )
      IF( L .GT. 0 ) WRITE( 10, '(A)' ) BUFFER( : L )

      END
