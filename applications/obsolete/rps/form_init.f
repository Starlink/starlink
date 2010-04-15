*+FORM_INIT        Initialises a form filling facility for the package.
*-  Author M.D.C.Harris ( R.A.L )                    11th August 1987.
*	9 APR 1992	M. DUESTERHAUS (GSFC)	REMOVE VAX RTL CALLS
*     1993 June         P. Brisco       Got rid of SMG stuff.
*     1994 Jan		M Ricketts	RAL version
***********************************************************************
      SUBROUTINE FORM_INIT(FORM_PART)

*  Calling Arguments
      CHARACTER*(*) FORM_PART	! 'COVER', 'GEN', 'TARGET', or 'CONSTR'

      INCLUDE 'com_form_data.inc'
      INCLUDE 'com_form_points.inc'


*   Functions
      CHARACTER*60 DBS_INFOC	! Gets character information.
      INTEGER      DBS_FIELDNO	! Gets number of field.
      INTEGER      DBS_INFOI

*  Local Variables
      INTEGER KFILE		! File section
      INTEGER IDB		! Database file
      INTEGER LIMIT1, LIMIT2	!   ::     section limits
*      CHARACTER*17 FIELD(60)	! Vector form of FIELDS.
      INTEGER      J		! Loop variables.
     & ,           OFFSET   	! Temporary varable.
     & ,           I 		! Number of fields.

*  ________________________________  Executable Code  __________________________

*  Get section-dependent parameters

      IF (FORM_PART .EQ. 'COVER') THEN
         LIMIT1 = FLD_LIMS_COVER(1)
         LIMIT2 = FLD_LIMS_COVER(2)
         KFILE = 1
         IDB = 1
      ELSE IF (FORM_PART .EQ. 'GEN') THEN
         LIMIT1 = FLD_LIMS_GEN(1)
         LIMIT2 = FLD_LIMS_GEN(2)
         KFILE = 2
         IDB = 1
      ELSE IF (FORM_PART .EQ. 'TARGET') THEN
         LIMIT1 = FLD_LIMS_TARG(1)
         LIMIT2 = FLD_LIMS_TARG(2)
         KFILE = 3
         IDB = 2
      ELSE IF (FORM_PART .EQ. 'CONSTR') THEN
         LIMIT1 = FLD_LIMS_CONS(1)
         LIMIT2 = FLD_LIMS_CONS(2)
         KFILE = 4
         IDB = 2
      END IF

      I = 0
      DO J = LIMIT1 , LIMIT2							! Do for each field.

        I = I + 1								!  Calculate the field number for the form.
        FRMT( I, KFILE ) = DBS_INFOC( IDB , J , 'NULFORMAT' )			!  Get the field format.
        UNITS( I,KFILE ) = DBS_INFOC( IDB , J , 'UNITS' )			!  Field units.
        FIELDS(I,KFILE ) = DBS_INFOC( IDB , J , 'FIELDNAME' )			!  Field names.
        FLENTH(I,KFILE)  = DBS_INFOI( IDB , J , 'LENGTH' )			!  Field length
        CONDOFF(I,KFILE) = DBS_INFOI( IDB , J , 'CONDITIONAL' )			!  Offset if read is conditional
        IF (CONDOFF(I,KFILE) .LT. 0) CONDNULL(I,KFILE) = DBS_INFOC( IDB, J, 'NULVALUE' )	! array null value
      END DO									! End do.

      IF ( KFILE .EQ. 3 ) THEN							! If third form then.

        OFFSET = 1 - LIMIT1							!  Calculate correction value.
        RA_POS = DBS_FIELDNO( IDB , 'TARGET.RA' ) + OFFSET			!  Get source position field numbers.
        DEC_POS = DBS_FIELDNO( IDB , 'TARGET.DEC' ) + OFFSET
        FRMT( RA_POS, KFILE  ) = 'See help'					!  Formats are real.
        FRMT( DEC_POS, KFILE ) = 'See help'
        HRIFLD = DBS_FIELDNO( IDB , 'HRI.CODE' ) + OFFSET			!  Get instrument field numbers.
        WFCFLD = DBS_FIELDNO( IDB , 'WFC.CODE' ) + OFFSET

      ELSE IF ( KFILE .EQ. 4 ) THEN						! Else if fourth form then.

        OFFSET = 1 - LIMIT1							!  Calculate correction value.
        CONTG = DBS_FIELDNO( IDB, 'CONTIGUOUS.OBS' ) + OFFSET			!  Get constraint mode field numbers.
        MONIT = DBS_FIELDNO( IDB, 'MONITOR' ) + OFFSET
        PHASE = DBS_FIELDNO( IDB, 'PHASE.DEPENDENT' ) + OFFSET
        COORD = DBS_FIELDNO( IDB, 'COORD.OBSERVATION') + OFFSET

      END IF									! End if,


      END									! End.
