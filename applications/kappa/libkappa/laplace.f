*+  LAPLACE - Performs a Laplacian convolution as an edge detector in a
*             2-d data array

      SUBROUTINE LAPLACE ( STATUS )
*
*    Description :
*
*     This routine works out the Laplacian of the 2-d data array, in the
*     input IMAGE structure, and subtracts it from the original array to
*     create a new data array in the output IMAGE structure. The
*     subtractions can be done a specified integer number of times.
*     This operation can be approximated with a convolution by
*
*                           -N   -N   -N
*                           -N   +8N  -N
*                           -N   -N   -N
*
*     where N is the integer number of times the Laplacian is
*     subtracted.  This convolution is used as a uni-directional edge
*     detector.  Areas where the input data array is flat become zero
*     in the output data array.
*
*     The magic-value method is used for processing bad data.
*
*    Invocation :
*
*     CALL LAPLACE( STATUS )
*
*    Parameters :
*
*     INPIC = IMAGE( READ )
*         Input IMAGE structure containing the 2-d data array to be
*           processed
*     NUMBER = INTEGER( READ )
*         Number of times the Laplacian is to be subtracted
*     OUTPIC = IMAGE( WRITE )
*         Output IMAGE structure containing the processed data array
*     OTITLE = CHAR( READ )
*         Title string for output IMAGE structure
*
*    Arguments:
*
*     STATUS  = INTEGER( READ, WRITE )
*         Global status value
*
*    Method :
*
*     Check for error on entry - return if not o.k.
*     Get an input IMAGE structure
*     If no error so far then
*        Map its data-array component
*        If no error so far then
*           Get a value for N, the number of Laplacian subtractions
*           If error other than abort then annul error, set N to 1 and
*             warn the user
*           Create an output IMAGE structure and title string
*           Propagate NDF MORE from the input data file
*           If no error so far then
*              Map a data-array component to hold convolved array
*              If no error so far then
*                 Call LAPLSB to perform the convolution
*                 Unmap output data array
*              Else
*                 Report error
*              Endif
*              Annul output IMAGE structure
*           Else
*              Report error
*           Endif
*           Unmap input data array
*        Else
*           Report error
*        Endif
*        Annul input IMAGE structure
*     Else
*        Report error
*     Endif
*     End
*
*    Bugs :
*
*     None known.
*
*    Authors :
*
*     Mark McCaughrean UoE ( REVA::MJM )
*     Malcolm Currie RAL ( UK.AC.RL.STAR::CUR )
*
*    History :
*
*     10-12-1985 : First implementation (UKTH::MARK)
*     17-04-1986 : Tidied and more error checking (REVA::MJM)
*     1986 Aug 7 : Renamed algorithmic subroutine to LAPLSB.
*     1986 Aug 29: Added argument section to prologue, and nearly
*                  conformed to Starlink standards (RAL::CUR).
*     1987 Oct 15: Reordered tidying and extra status checks
*                  (RAL::CUR).
*     1988 Mar 17: Referred to `array' rather than `image'
*                  (RAL::CUR).
*     1988 May 31: More reporting of error context (RAL::CUR).
*     1989 Jun 13: Allow for processing primitive NDFs (RAL::CUR).
*     1989 Aug  8: Passed array dimensions as separate variables
*                  to LAPLSB (RAL::CUR).
*     1991 Oct 25: Propagates AXIS, LABEL and HISTORY (RAL::CUR).
*     1992 Feb 25: Limited processing of simple NDFs (RAL::CUR).
*     1992 Mar  3: Replaced AIF parameter-system calls by the extended
*                  PAR library (RAL::CUR).
*
*    Type definitions :

      IMPLICIT  NONE           ! no default typing allowed

*    Global constants :

      INCLUDE 'SAE_PAR'        ! global SSE definitions
      INCLUDE 'DAT_PAR'        ! Data-system constants
      INCLUDE 'PAR_ERR'        ! parameter-system errors

*    Status :

      INTEGER  STATUS

*    Local Constants :

      INTEGER  NDIMS           ! dimensionality of input data
      PARAMETER( NDIMS = 2 )   ! 2-d arrays only

*    Local variables :

      INTEGER
     :  DIMS( NDIMS ),         ! input and output array dimensions
     :  ORIGIN( DAT__MXDIM ),  ! Origin of the data array
     :  PNTRI,                 ! pointer to input array
     :  PNTRO,                 !    "     " output  "
     :  NUMBER                 ! number of times Laplacian is removed

      CHARACTER*(DAT__SZLOC)   ! locators for :
     :  LOCDI,                 ! structure containing the input data
                               ! array
     :  LOCDO,                 ! structure containing the output data
                               ! array
     :  LOCI,                  ! input data structure
     :  LOCO                   ! output data structure

      CHARACTER * ( DAT__SZNAM )
     :  DNAMEI,                ! Name of the input data-array component
     :  DNAMEO                 ! Name of the output data-array component

*-
*    check status on entry - return if not o.k.

      IF ( STATUS .NE. SAI__OK ) RETURN

*    start by obtaining the input IMAGE structure locator

      CALL KPG1_GETIM( 'INPIC', LOCI, LOCDI, DNAMEI, ORIGIN, STATUS )

*    if no error so far then continue

      IF ( STATUS .EQ. SAI__OK ) THEN

*       map its data-array component onto a pointer

         CALL CMP_MAPN( LOCDI, DNAMEI, '_REAL', 'READ', NDIMS,
     :                  PNTRI, DIMS, STATUS )

*       if no error so far then continue

         IF ( STATUS .EQ. SAI__OK ) THEN

*          get the number of times Laplacian is to be removed

            CALL PAR_GDR0I( 'NUMBER', 1, 1, 100, .TRUE., NUMBER,
     :                      STATUS )

*          get an output IMAGE structure

            CALL KPG1_CROUT( 'OUTPIC', 'OTITLE', NDIMS, DIMS,
     :                       ORIGIN, LOCO, LOCDO, DNAMEO, STATUS )

*          propagate AXIS, LABEL, HISTORY and extensions from the
*          input data file

            CALL KPG1_IMPRG( LOCI, 'AXIS', LOCO, STATUS )

*          if no error so far then continue

            IF ( STATUS .EQ. SAI__OK ) THEN

*             map its data-array component onto a pointer

               CALL CMP_MAPN( LOCDO, DNAMEO, '_REAL', 'WRITE', 
     :                        NDIMS, PNTRO, DIMS, STATUS )

*             check status before accessing pointers

               IF ( STATUS .EQ. SAI__OK ) THEN

*                now call the subroutine that does the actual work

                  CALL LAPLSB( %VAL( PNTRI ), DIMS( 1 ), DIMS( 2 ),
     :                         NUMBER, %VAL( PNTRO ), STATUS )

*                unmap output data array

                  CALL CMP_UNMAP( LOCDO, DNAMEO, STATUS )

               ELSE

                  CALL ERR_REP( 'ERR_LAPLACE_NOMPO',
     :              'LAPLACE : Error occurred whilst trying to map '/
     :              /'output frame', STATUS )

               END IF

*             tidy up the output structures

               CALL DAT_ANNUL( LOCDO, STATUS )
               CALL DAT_ANNUL( LOCO, STATUS )

            ELSE

               IF ( STATUS .NE. PAR__ABORT ) THEN
                  CALL ERR_REP( 'ERR_LAPLACE_NOFRO',
     :              'LAPLACE : Error occurred whilst trying to '/
     :              /'access output frame', STATUS )
               END IF

*          end of if-error-after-getting-output-structure check

            END IF

*          unmap input data array

            CALL CMP_UNMAP( LOCDI, DNAMEI, STATUS )

         ELSE

            CALL ERR_REP( 'ERR_LAPLACE_NOMPI',
     :        'LAPLACE : Error occurred whilst trying to map input '/
     :        /'frame', STATUS )

*       end of if-no-error-after-mapping-input-array check

         END IF

*       tidy up the input structures

         CALL DAT_ANNUL( LOCDI, STATUS )
         CALL DAT_ANNUL( LOCI, STATUS )

      ELSE

         IF ( STATUS .NE. PAR__ABORT ) THEN
            CALL ERR_REP( 'ERR_LAPLACE_NOFRI',
     :        'LAPLACE : Error occurred whilst trying to access '/
     :        /'input frame', STATUS )
         END IF

*    end of if-error-after-getting-input-structure check

      END IF

*    end

      END
