*+  TRIG - Performs a trigonometric transformation on a data array

      SUBROUTINE TRIG ( STATUS )
*
*    Description :
*
*     This routine allows the user to select one of a set of several
*     basic trigonometrical functions (sine, cosine, tangent, arcsine,
*     etc.) and operate on each pixel of the data array, in the input
*     IMAGE structure, with this function, and then to output a
*     transformed version of the array. The trigonometric functions can
*     be selected to act as if the input data are to be treated as
*     radians or degrees. If a scalar value rather than a data array is
*     input, the application acts purely on that scalar value.
*
*     The magic-value method is used for processing bad data.  Undefined
*     results are set to the magic value.
*
*    Invocation :
*
*     CALL TRIG( STATUS )
*
*    Parameters :
*
*     INPIC = IMAGE( READ )
*         Input IMAGE structure containing the data array to be
*           transformed.
*     TRIGFUNC = CHAR( READ )
*         Trigonometrical function to be applied.  The options are
*           'SIN', 'COS', 'TAN', 'SIND', 'COSD', 'TAND', 'ASIN', 
*           'ACOS', 'ATAN', 'ASIND', 'ACOSD', 'ATAND'.
*     OUTPIC = IMAGE( WRITE )
*         Output IMAGE structure containing the transformed data array.
*     OTITLE = CHAR( READ )
*         Title string for the output IMAGE structure.
*
*    Arguments:
*
*     STATUS  = INTEGER( READ, WRITE )
*         Global status value
*
*    Method :
*
*     Check for error on entry - return if not o.k.
*     Initialise the valid and scalar input flags
*     Loop until a valid input of some sort is obtained, aborting if
*       a null is input
*        Get input IMAGE structure to be examined
*        If no error then
*           Get shape of array
*           Map its data-array component onto a pointer
*           If no error then
*              The input array was o.k. so set the valid flag
*                accordingly
*           Else
*              Report error
*              Tidy input structure
*              Exit
*           Endif
*        Else
*           If null or abort status then
*              Exit
*           Endif
*           Annul the error and try to get a scalar
*           If this succeeds, set the valid and scalar flags
*             accordingly
*           Else
*              A valid scalar was not given either - report error and
*                try to get another input
*           Endif
*        Endif
*     Enddo
*     Now get the desired trigonometrical function from the interface
*     If a scalar was input then
*        Call the subroutine that does trig on a scalar
*        If no error has occurred in TRGSCL then
*           A valid result was returned, so output
*        Endif
*        Clean up
*     Else if no error then
*        An array was input, so get an output IMAGE structure
*        Propagate NDF QUALITY and MORE from the input data file
*        If no error creating output structure then
*           Map its data-array component onto a pointer
*           If no error then
*              Call the subroutine that does the actual work
*              Unmap output data array
*           Else
*              Report error
*           Endif
*           Tidy up output data structure
*        Else
*           Report error
*        Endif
*        Tidy input data structure
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
*     Malcolm Currie RAL (UK.AC.RL.STAR)
*
*    History :
*
*     10-12-1985 : First implementation (UKTH::MARK)
*     08-01-1986 : Allowed option of scalar processing (REVA::MJM)
*     1986 Aug 8 : Renamed routines TRIGSCAL and TRIGSUB to TRGSCL and
*                  TRIGSB respectively. 2nd argument of TRIGSB changed
*                  to the number of pixels in the input array. Status
*                  checking of mapping of output array added 
*                  (RAL::CUR).
*     1986 Sep 1 : Added argument section to prologue, generalised to
*                  vector and nearly conformed to Starlink standards
*                  (RAL::CUR).
*     1987 Oct 16: Reordered tidying (particularly in VALID loop), extra
*                  status checks and used CMP_SHAPE, rewrote method
*                  section (RAL::CUR).
*     1988 Mar 17: Referred to `array' rather than `image'
*                  (RAL::CUR).
*     1988 Jun 1 : More reporting of error context (RAL::CUR).
*     1989 Jun 13: Allow for processing primitive NDFs (RAL::CUR).
*     1991 Oct 25: Propagates AXIS, LABEL, and HISTORY (RAL::CUR).
*     1992 Feb 25: Limited processing of simple NDFs (RAL::CUR).
*
*    Type definitions :

      IMPLICIT  NONE           ! no implicit typing allowed

*    Global constants :

      INCLUDE 'SAE_PAR'        ! SSE global definitions
      INCLUDE 'DAT_PAR'        ! Data-system constants
      INCLUDE 'PAR_ERR'        ! parameter system error definitions

*    Status :

      INTEGER  STATUS

*    Local Constants :

      INTEGER
     :  MXDIMS                 ! maximum dimensionality of input arrays
      PARAMETER( MXDIMS = DAT__MXDIM )

*    Local variables :

      INTEGER
     :  DIMS( MXDIMS ),        ! input and output array dimensions
     :  DIMTOT,                ! number of pixels in an array
     :  NDIMS,                 ! number of dimensions of input array
     :  ORIGIN( DAT__MXDIM ),  ! Origin of the data array
     :  PNTRI,                 ! pointer to input data-array component
     :  PNTRO                  !    "     " output  "    "       "

      REAL
     :  SCALAR,                ! input scalar value for processing
     :  RESULT                 ! result of processing scalar value

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

      CHARACTER*5
     :  TRIGFN                 ! requested trigonometrical function

      CHARACTER*80
     :  OPTION                 ! list of acceptable trig options

      LOGICAL                  ! true if:
     :  VALID,                 ! a valid input has been given
     :  FSCAL                  ! a scalar was given

*-
*    check status on entry - return if not o.k.

      IF ( STATUS .NE. SAI__OK ) RETURN

*    initialise the valid and scalar input flags

      VALID  =  .FALSE.   
      FSCAL  =  .FALSE.

*    loop until a valid input of some sort is obtained, aborting if
*    a null is input

      DO WHILE( .NOT. VALID )

*       start by trying to obtain an input IMAGE structure locator

         CALL KPG1_GETIM( 'INPIC', LOCI, LOCDI, DNAMEI, ORIGIN, STATUS )

*       check for error

         IF ( STATUS .EQ. SAI__OK ) THEN

*          get shape of array

            CALL CMP_SHAPE( LOCDI, DNAMEI, MXDIMS, DIMS, NDIMS, STATUS )

*          map its data-array component onto a pointer

            CALL CMP_MAPV( LOCDI, DNAMEI, '_REAL', 'READ',
     :                     PNTRI, DIMTOT, STATUS )

*          check for error

            IF ( STATUS .EQ. SAI__OK ) THEN

*             the input array was o.k. so set the valid flag accordingly

               VALID = .TRUE.

            ELSE 

               CALL ERR_REP( 'ERR_TRIG_NOMPI',
     :           'TRIG : Error mapping or finding the shape of the '/
     :           /'input data array', STATUS )

*             tidy input structure and return

               CALL DAT_ANNUL( LOCI, STATUS )
               GOTO 999
 
            END IF

         ELSE
            IF ( STATUS .EQ. PAR__NULL .OR.
     :           STATUS .EQ. PAR__ABORT ) THEN

*             null parameter specified or abort requested - return

               GOTO 999
            END IF

*          attempt to get a scalar value as input
*
*          annul the error and try to get a scalar

            CALL ERR_ANNUL( STATUS )
            CALL DAT_GET0R( LOCI, SCALAR, STATUS )

*          if this succeeds, set the valid and scalar flags
*          accordingly

            IF ( STATUS .EQ. SAI__OK ) THEN

               VALID  =  .TRUE.
               FSCAL  =  .TRUE.

            ELSE

*             a valid scalar was not given either - report error and
*             try to get another input

               CALL ERR_REP( 'ERR_TRIG_BADINP',
     :           'TRIG : Failed to make sense of input, e.g. no data '/
     :           /'array. Try again', STATUS )
               CALL ERR_FLUSH( STATUS )
               CALL PAR_CANCL( 'INPIC', STATUS )

            END IF

*       end of if-not-valid-input check

         END IF

      END DO

*    now get the desired trigonometrical function from the interface
*    - permissible functions are :
*        SIN  -  sine (radians)       ASIN  -  arcsine (radians)
*        COS  -  cosine (radians)     ACOS  -  arccosine (radians)
*        TAN  -  tangent (radians)    ATAN  -  arctangent (radians)
*        SIND -  sine (degrees)       ASIND -  arcsine (degrees)
*        COSD -  cosine (degrees)     ACOSD -  arccosine (degrees)
*        TAND -  tangent (degrees)    ATAND -  arctangent (degrees)

      OPTION  =  
     :    'SIN,COS,TAN,SIND,COSD,TAND,ASIN,ACOS,ATAN,ASIND,ACOSD,ATAND'

      CALL PAR_CHOIC( 'TRIGFUNC', 'SIN', OPTION, .FALSE., TRIGFN,
     :                STATUS )

*    process the input accordingly depending on whether or not
*    a scalar was input

      IF ( FSCAL ) THEN

*       call the subroutine that does trig on a scalar

         CALL TRGSCL( SCALAR, TRIGFN, RESULT, STATUS )

         IF ( STATUS .EQ. SAI__OK ) THEN

*          a valid result was returned, so output

            CALL MSG_OUT( 'BLANK', ' ', STATUS )
            CALL MSG_SETR( 'SCALAR', SCALAR )
            CALL MSG_SETR( 'RESULT', RESULT )
            CALL MSG_OUT( 'TRIG_RES', TRIGFN//' of ^SCALAR is ^RESULT', 
     :                    STATUS )
            CALL MSG_OUT( 'BLANK', ' ', STATUS )

         END IF

*       clean up

         CALL DAT_ANNUL( LOCI, STATUS )

      ELSE IF ( STATUS .EQ. SAI__OK ) THEN

*       an array was input, so get an output IMAGE structure

         CALL KPG1_CROUT( 'OUTPIC', 'OTITLE', NDIMS, DIMS,
     :                    ORIGIN, LOCO, LOCDO, DNAMEO, STATUS )

*       propagate AXIS, QUALITY, LABEL, HISTORY and extensions from the
*       input data file

         CALL KPG1_IMPRG( LOCI, 'AXIS,QUALITY', LOCO, STATUS )

*       check for error creating output structure

         IF ( STATUS .EQ. SAI__OK ) THEN

*          map its data-array component onto a pointer

            CALL CMP_MAPV( LOCDO, DNAMEO, '_REAL', 'WRITE',
     :                     PNTRO, DIMTOT, STATUS )

*          check status before accessing pointers

            IF ( STATUS .EQ. SAI__OK ) THEN

*             now call the subroutine that does the actual work

               CALL TRIGSB( %VAL( PNTRI ), DIMTOT, TRIGFN,
     :                      %VAL( PNTRO ), STATUS )

*             unmap output data array

               CALL CMP_UNMAP( LOCDO, DNAMEO, STATUS )

            ELSE

               CALL ERR_REP( 'ERR_TRIG_NOMPO',
     :           'TRIG : Error occurred trying to map output frame',
     :           STATUS )

*             end of if-no-error-before-accessing-pointers check

            END IF

*          tidy up output data structures

            CALL DAT_ANNUL( LOCDO, STATUS )
            CALL DAT_ANNUL( LOCO, STATUS )

         ELSE

            IF ( STATUS .NE. PAR__ABORT ) THEN
               CALL ERR_REP( 'ERR_TRIG_NOFRO',
     :           'TRIG : Error occurred whilst trying to access '/
     :           /'output frame', STATUS )
            END IF

*       end of if-no-error-after-creating-output-structure check

         END IF

*       tidy input data structures

         CALL CMP_UNMAP( LOCDI, DNAMEI, STATUS )
         CALL DAT_ANNUL( LOCDI, STATUS )
         CALL DAT_ANNUL( LOCI, STATUS )

*    end of if-scalar-or-array check

      END IF

 999  CONTINUE

*    end

      END
