*+  GLTCLT - gets glitch positions from a file and deglitches a 2-d
*            array

      SUBROUTINE GLTCLT ( DIM1, DIM2, FILNAM, ARRAY, STATUS )
*
*    Description :
*
*     This routine deglitches an image. The glitch positions are
*     defined  according to values found in a free-format file, the
*     name of which is given as input. Returned is the deglitched image,
*     and the number of pixels, listed in the free-format file, to be
*     deglitched, and the number that were actually valid and
*     consequently deglitched, are both output. The user has a few
*     attempts to specify a valid filename of an existing file.
*
*     The glitch list file should comprise the a header record followed
*     by a series free-format records.  Each of these contains either a
*     pair of integer x-y pixel indices, or a pair of real x-y
*     pixel co-ordinates.  The latter is indicated by the presence of a
*     decimal point in the record.   The file is terminated just by the
*     end-of-file marker. For example:
*
*       Glitch list for SBRC FPA#005
*       22  45
*       19.3  56.7
*       2   30
*       .   .
*       .   .
*       .   .
*       <EOF>
*
*     The header string is output to the user.
*
*    Invocation :
*
*     CALL GLTCLT( DIM1, DIM2, FILNAM, ARRAY, STATUS )
*
*    Arguments :
*
*     DIM1 = INTEGER( READ )
*         The first dimension of the 2-d array.
*     DIM2 = INTEGER( READ )
*         The second dimension of the 2-d array.
*     FILNAM  =  CHAR( READ )
*         Parameter name for the file containing free-format glitch list
*     ARRAY( DIM1, DIM2 )  =  REAL( WRITE )
*         Output deglitched image
*     STATUS  =  INTEGER( READ, WRITE )
*         Global status parameter
*
*    Method :
*
*     Check for error on entry - return if not o.k.
*     Open the file of pixel positions
*     If successful on opening then
*        Read header from file
*        If successful then
*           Output to user
*           Do while more pixel positions in file and there is no error
*              Read next line of the data file
*              If no error then
*                 Get next pixel position from file as free-format
*                   floating point and convert co-ordinates to indices
*                 If no error then
*                    Increment number of pixels tried by one
*                    If pixel has a valid position on input array then
*                       Call GLTCSB to do the business
*                       Increment valid pixel counter by one
*                    Endif
*                 Else
*                    Report error context
*                 Endif
*              Else
*                 Report error context if not the end of file
*              Endif
*           Enddo
*           If end of file then
*              Annul error
*              Output values of pixel counters
*           Endif
*        Else
*           Report error context
*        Endif
*        Close the file
*     Else
*        Report error context
*     Endif
*     Return
*
*    Bugs :
*
*     None known.
*
*    Authors :
*
*     Mark McCaughrean UoE ( REVA::MJM )
*     Malcolm Currie RAL (UK.AC.RL.STAR::CUR)
*
*    History :
*
*     24-06-1986  : First implementation (REVA::MJM)
*     1986 Aug 13 : Renamed from GLITCHLIST, uses IOSTAT rather than
*                   statement labels and gotos, renamed GLITCHSUB routine
*                   and changed its argument order, completed prologue,
*                   and nearly conformed to Starlink programming
*                   standards (RL.STAR::CUR).
*     1986 Sep 4  : Renamed parameters section to arguments and tidied
*                   (RL.STAR::CUR).
*     1987 Dec 31 : Corrected bug in read status checking (RL.STAR::CUR).
*     1988 Jun 27 : Converted to FIO, added error reporting, made FILNAM
*                   the parameter name rather than the file name itself,
*                   and restructured (RL.STAR::CUR).
*     1989 Jul 27 : Used packaged FIO_ASSOC to open the x,y file, and
*                   passed the array dimensions as two variables
*                   (RL.STAR::CUR).
*     1990 Feb 20 : AIF_OPFIO renamed AIF_ASFIO (RAL::CUR).
*     1993 Feb 9  : Used the improved FIO_ASSOC and the new FIO_ANNUL.
*                   (RAL::CUR).
*     1994 Aug 10 : Since "position" is ambiguous, made this work both
*                   for pixel co-ordinates and indices.  A decimal point
*                   present indicates the former.
*                   (RAL::CUR).
*
*    Type definitions :

      IMPLICIT  NONE           ! no default typing allowed

*    Global constants :

      INCLUDE  'SAE_PAR'       ! SSE global definitions
      INCLUDE  'FIO_ERR'       ! Fortran-I/O-system errors

*    Import :

      INTEGER
     :    DIM1, DIM2

      CHARACTER*(*)
     :    FILNAM

*    Import-Export :

      REAL
     :    ARRAY( DIM1, DIM2 )

*    Status :

      INTEGER  STATUS

*    Local Constants :

      INTEGER  
     :    NCHLIN               ! maximum number of characters in a
                               ! an input record
      PARAMETER ( NCHLIN = 132 )

*    Local variables :

      INTEGER
     :    FD,                  ! file description
     :    NCHAR,               ! number of characters in a record
     :    NCO,                 ! Character column counter
     :    POS( 2 ),            ! X-y positions of current pixel
     :    TRIED,               ! number of pixel positions read in
     :    DONE                 ! number of valid positions deglitched

      REAL
     :    FPOS( 2 ),           ! X-y positions of current pixel
     :    OLDVAL,              ! old value for current pixel
     :    NEWVAL               ! new (deglitched) value for current
                               ! pixel

      CHARACTER*80
     :    BUFFER*(NCHLIN),     ! buffer to hold records read
     :    HEADER               ! header string in glitch-list file

      LOGICAL                  ! True if:
     :    COORD                ! Record contains co-ordinates

*-
*    check status on entry - return if not o.k.

      IF ( STATUS .NE. SAI__OK ) RETURN

*    attempt to obtain and open a data file of glitch pixel POSitions

      CALL FIO_ASSOC( FILNAM, 'READ', 'LIST', 0, FD, STATUS )

      IF ( STATUS .EQ. SAI__OK ) THEN

*       get the header string from the file

         CALL FIO_READ( FD, HEADER, NCHAR, STATUS )
         IF ( STATUS .EQ. SAI__OK ) THEN

*          write out the header to the user

            CALL MSG_OUT( 'BLANK', ' ', STATUS )
            CALL MSG_OUT( 'LINE1', 'Title of glitch list file :',
     :                    STATUS )
            CALL MSG_SETC( 'HEADER', HEADER )
            CALL MSG_OUT( 'LINE2', '^HEADER', STATUS )

*          initialise the counters

            TRIED  =  0
            DONE   =  0

*          scan round the file until the end is reached

            DO WHILE ( STATUS .EQ. SAI__OK )

*            read in the current pixel position as a free-format integer
*            pair - jump out of loop if the end of file is found

               CALL FIO_READ( FD, BUFFER, NCHAR, STATUS )
               IF ( STATUS .EQ. SAI__OK ) THEN

*                See whether the positions are co-ordinates (indicated
*                by the presence of a decimal point somewhere in the
*                record), or indices (integer values).

                  COORD = INDEX( BUFFER, '.' ) .NE. 0

*               Extract the positions from the string.

                  NCO = 1
                  CALL KPG1_FFRR( BUFFER, 2, NCO, FPOS, STATUS )

*               Convert the co-ordinates into the nearest pixel indices.

                  IF ( COORD ) THEN
                     POS( 1 ) = NINT( FPOS( 1 ) + 0.5 )
                     POS( 2 ) = NINT( FPOS( 2 ) + 0.5 )

*               Convert the floating-point indices to integers.

                  ELSE
                     POS( 1 ) = NINT( FPOS( 1 ) )
                     POS( 2 ) = NINT( FPOS( 2 ) )
                  END IF

                  IF ( STATUS .EQ. SAI__OK ) THEN

*                   increment the counter keeping track of the number of
*                   requested pixels

                     TRIED  =  TRIED + 1

*                   check that given pixel position lies on the input
*                   data array

                     IF ( POS( 1 ) .GE. 1 .AND. POS( 1 ) .LE. DIM1 .AND.
     :                    POS( 2 ) .GE. 1 .AND. POS( 2 ) .LE. DIM2 )
     :                 THEN

*                      position is valid - deglitch it

                        CALL GLTCSB( DIM1, DIM2, POS( 1 ), POS( 2 ),
     :                               ARRAY, OLDVAL, NEWVAL, STATUS )

*                      increment valid deglitch counter by one

                        DONE  =  DONE + 1

*                   end of if-pixel-position-is-on-array check

                     END IF

                  ELSE

*                   Report the error.

                     CALL MSG_SETC( 'BUFFER', BUFFER )
                     CALL ERR_REP( 'ERR_GLTCLT_INTPDA',
     :                 'GLTCLT: Error reading x-y positions in line '/
     :                 /'^BUFFER', STATUS )

*                end of no-error-reading-internal-file check

                  END IF

               ELSE

*                report error context unless the end-of-file has been
*                reached

                  IF ( STATUS .NE. FIO__EOF ) THEN
                     CALL ERR_REP( 'ERR_GLTCLT_RDDATA',
     :                 'GLTCLT: Error reading data record from the '/
     :                 /'file', STATUS )
                  END IF

*             end of no-error-reading-record-from-file check

               END IF

*          end of loop round glitch-list file

            END DO

*          the glitch file has no more entries or the file is empty

            IF ( STATUS .EQ. FIO__EOF ) THEN

               CALL ERR_ANNUL( STATUS )

*             output messages to give the number of pixel positions read
*             in and actually deglitched

               CALL MSG_OUT( 'BLANK', ' ', STATUS )
               CALL MSG_SETI( 'TRIED', TRIED )
               CALL MSG_OUT( 'NTRIED',
     :           ' Number of pixel positions read in from file    ='/
     :           /'  ^TRIED', STATUS )
               CALL MSG_SETI( 'DONE', DONE )
               CALL MSG_OUT( 'NDONE',
     :           ' Number of valid positions actually deglitched  ='/
     :           /'  ^DONE', STATUS )
               CALL MSG_OUT( 'BLANK', ' ', STATUS )

*          end of file-end-status check

            END IF

         ELSE

*         report error context

            CALL ERR_REP( 'ERR_GLTCLT_RDHEAD',
     :        'GLTCLT: Error reading header record from the file',
     :        STATUS )

*       end of read-header-string test

         END IF

*       close the file

         CALL FIO_ANNUL( FD, STATUS )

*    abort for repeated error

      ELSE
         CALL ERR_REP( 'ERR_GLTCLT_NOOPEN',
     :     'GLTCLT: Repeatedly unable to open a file. Aborting',
     :     STATUS )
         GOTO 999

*    end of file-open-status check

      END IF

 999  CONTINUE

*    return and end

      END
