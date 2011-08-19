      SUBROUTINE SCULIB_WRITE_SKYDIP_DATAMODEL(PARAM,
     :     N_FITS, FITS, LABEL, TITLE, UNITS, AXLABEL,
     :     NPTS, INDATA, HASVAR, INVAR, AXDATA, HASAXVAR, AXVAR,
     :     SUB_INST, FILT, WAVE,
     :     T_COLD, ETA_TEL_FIT, B_FIT, TAUZ_FIT,
     :     STATUS )

*+
*  Name:
*     SCULIB_WRITE_SKYDIP_DATAMODEL

*  Purpose:
*     Write Skydip result data or model to output file

*  Language:
*     Starlink Fortran 77

*  Invocation:

*  Description:
*     Write the data that was fitted by the skydip out to a file
*     with associated metadata. Can also be used to write model
*     data.

*  Arguments:
*     PARAM = _CHAR (Given)
*        Name of the ADAM parameter to use to obtain the output file name.
*     N_FITS = _INTEGER (Given)
*        Number of FITS items.
*     FITS(N_FITS) = _CHAR*80 (Given & Returned)
*        The FITS array.
*     LABEL = _CHAR (Given)
*        Data label of output file.
*     TITLE = _CHAR (Given)
*        Title of output file.
*     UNITS = _CHAR (Given)
*        Units of output data.
*     AXLABEL = _CHAR (Given)
*        Label of axis data.
*     NPTS = _INTEGER (Given)
*        Number of points in data array to be written out.
*     INDATA = _REAL (Given)
*        Data to write.
*     HASVAR = _LOGICAL (Given)
*        .TRUE. if variance data should be written.
*     INVAR = _REAL (Given)
*        Variance data to write if HASVAR is true.
*     AXDATA = _REAL (Given)
*        Axis data associated with INDATA.
*     HASAXVAR = _LOGICAL (Given)
*        .TRUE. if variance data should be associated with the axis data.
*     AXVAR = _REAL (Given)
*        Variance associated with AXDATA if HASAXVAR is true.
*     SUB_INST = _CHAR (Given)
*        Sub instrument being written out.
*     FILT = _CHAR (Given)
*        Name of selected filter
*     WAVE = _REAL (Given)
*        Wavelength (microns) of selected sub instrument
*     T_COLD = _REAL (Given)
*        Cold load temperature to store in extension. Not written if
*        value is bad.
*     ETA_TEL_FIT = _REAL (Given)
*        Eta tel value to store in extension. Not written if
*        value is bad.
*     B_FIT = _REAL (Given)
*        Bandwidth factor from fit to store in extension. Not written if
*        value is bad.
*     TAUZ_FIT = _REAL (Given)
*        Fitted zenith tau to store in extension. Not written if
*        value is bad.
*     STATUS = _INTEGER (Given & Returned)
*        Global status

*  Notes:
*     Might be a general purpose 1D data writing routine.

*  Authors:
*     TIMJ: Tim Jenness (JACH)
*     JFL:  John Lightfoot (RoE)


*  Copyright:
*     Copyright (C) 2011 Science & Technology Facilities Council.
*     All Rights Reserved.

*  History:
*     2011-08-18 (TIMJ):
*        Extract from surf_skydip

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'PRM_PAR'          ! for VAL__ constants
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'SURF_PAR'         ! SURF constants
      INCLUDE 'DAT_PAR'          ! DAT__ constants
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL
      INCLUDE 'PAR_ERR'          ! For PAR__NULL

*  Arguments Given:
      CHARACTER * (*) PARAM
      INTEGER         N_FITS
      CHARACTER * (*) FITS(N_FITS)
      INTEGER         NPTS
      CHARACTER * (*) LABEL
      CHARACTER * (*) TITLE
      CHARACTER * (*) UNITS
      CHARACTER * (*) AXLABEL
      LOGICAL         BADPIX
      REAL            INDATA(NPTS)
      LOGICAL         HASVAR
      REAL            INVAR(NPTS)
      REAL            AXDATA(NPTS)
      LOGICAL         HASAXVAR
      REAL            AXVAR(NPTS)
      CHARACTER * (*) SUB_INST
      CHARACTER * (*) FILT
      REAL            WAVE
      REAL            T_COLD
      REAL            ETA_TEL_FIT
      REAL            B_FIT
      REAL            TAUZ_FIT

*  Arguments Returned:

*  Status:
      INTEGER STATUS             ! Global status

*  Local constants:
      INTEGER MAXDIM
      PARAMETER (MAXDIM = 1)

*  Local Variables:
      INTEGER IERR                      ! For VEC_
      INTEGER ITEMP                     ! scratch integer
      INTEGER LBND (MAXDIM)             ! lower bounds of array
      CHARACTER*(DAT__SZLOC) LOC1       ! Dummy locator

      INTEGER NDIM                      ! the number of dimensions in an array
      INTEGER NERR                      ! For VEC_
      INTEGER OUT_AXIS_PTR              ! pointer to axis of observed output file
      INTEGER OUT_DATA_PTR              ! pointer to data array of output file
      CHARACTER *80 OUT_NAME            ! Name of output NDF
      INTEGER OUT_VAR_PTR               ! pointer to variance array of output file
      INTEGER OUT_NDF                   ! NDF identifier of output file
      INTEGER PLACE                     ! A placeholder
      INTEGER UBND (MAXDIM)             ! upper bounds of array

*.
      IF (STATUS .NE. SAI__OK) RETURN

*     Find out the name of the output file
      CALL PAR_GET0C(PARAM, OUT_NAME, STATUS)

*     Only write out if required
      IF (STATUS .EQ. PAR__NULL) THEN
         CALL ERR_ANNUL(STATUS)
         RETURN
      END IF

*  now open the output NDF
      NDIM = 1
      LBND (1) = 1
      UBND (1) = NPTS

      CALL NDF_PLACE(DAT__ROOT, OUT_NAME, PLACE, STATUS)
      CALL NDF_NEW('_REAL',NDIM, LBND, UBND, PLACE, OUT_NDF,
     :     STATUS)

* probably should store the FITS header

      CALL NDF_XNEW(OUT_NDF, 'FITS','_CHAR*80', 1, N_FITS, LOC1,
     :     STATUS)
      CALL DAT_PUT1C(LOC1, N_FITS, FITS, STATUS)
      CALL DAT_ANNUL(LOC1, STATUS)

* Create a REDS extension to store the FIT parameters
      CALL NDF_XNEW (OUT_NDF, 'REDS', 'SURF_EXTENSION', 0, 0,
     :     LOC1, STATUS)
      CALL DAT_ANNUL (LOC1, STATUS)

* Store description of selected sub instrument

      CALL NDF_XPT0C(SUB_INST, OUT_NDF, 'REDS', 'SUB_INSTRUMENT',
     :     STATUS)
      CALL NDF_XPT0C(FILT, OUT_NDF, 'REDS', 'FILTER', STATUS)
      CALL NDF_XPT0R(WAVE, OUT_NDF, 'REDS', 'WAVELENGTH', STATUS)


*     Store the fit results if good values

      IF (ETA_TEL_FIT .NE. VAL__BADR) THEN
         CALL NDF_XPT0R(ETA_TEL_FIT, OUT_NDF, 'REDS', 'ETA_TEL',
     :        STATUS)
      END IF
      IF (B_FIT .NE. VAL__BADR) THEN
         CALL NDF_XPT0R(B_FIT, OUT_NDF, 'REDS', 'B_FIT', STATUS)
      END IF
      IF (TAUZ_FIT .NE. VAL__BADR) THEN
         CALL NDF_XPT0R(TAUZ_FIT, OUT_NDF, 'REDS', 'TAU_Z',STATUS)
      END IF
      IF (T_COLD .NE. VAL__BADR) THEN
         CALL NDF_XPT0R(T_COLD, OUT_NDF,'REDS','T_COLD',
     :        STATUS)
      END IF

*  create a history component in the output file
      CALL NDF_HCRE (OUT_NDF, STATUS)

*     Title, label and units
      CALL NDF_CPUT (TITLE, OUT_NDF,'TITLE',STATUS)
      CALL NDF_CPUT (LABEL, OUT_NDF,'LABEL',STATUS)
      CALL NDF_CPUT (UNITS, OUT_NDF, 'UNITS', STATUS)

* Create and label the AXES
      CALL NDF_ACRE(OUT_NDF, STATUS)
      CALL NDF_ACPUT(AXLABEL,OUT_NDF,'LABEL',1,STATUS)

*     Map the data
      CALL NDF_MAP (OUT_NDF, 'DATA', '_REAL', 'WRITE',
     :     OUT_DATA_PTR, ITEMP, STATUS)

      CALL VEC_RTOR(.FALSE.,UBND(1), INDATA,
     :     %VAL(CNF_PVAL(OUT_DATA_PTR)), IERR, NERR, STATUS)

*     Only data has VARIANCE
      IF (HASVAR) THEN
         CALL NDF_MAP (OUT_NDF, 'VARIANCE', '_REAL', 'WRITE',
     :        OUT_VAR_PTR, ITEMP, STATUS)

         CALL VEC_RTOR(.FALSE., UBND(1), INVAR,
     :        %VAL(CNF_PVAL(OUT_VAR_PTR)), IERR, NERR, STATUS)

      END IF

*     Map the Axes
      CALL NDF_AMAP (OUT_NDF, 'Centre', 1, '_REAL', 'WRITE',
     :     OUT_AXIS_PTR, UBND(1), STATUS)

      CALL VEC_RTOR(.FALSE., UBND(1), AXDATA,
     :     %VAL(CNF_PVAL(OUT_AXIS_PTR)), IERR, NERR, STATUS)

* if data then write the AXIS variance (probably pretty inaccurate)
      IF (HASAXVAR) THEN

         CALL NDF_AMAP (OUT_NDF, 'Error', 1, '_REAL', 'WRITE',
     :        OUT_AXIS_PTR, UBND(1), STATUS)

         CALL VEC_RTOR(.FALSE., UBND(1), AXVAR,
     :        %VAL(CNF_PVAL(OUT_AXIS_PTR)), IERR, NERR, STATUS)
      END IF

*     Set the bad pixel flag
      CALL NDF_SBAD(BADPIX, OUT_NDF, 'Data', STATUS)

*     Tidy up
      CALL NDF_ANNUL(OUT_NDF, STATUS)

      END
