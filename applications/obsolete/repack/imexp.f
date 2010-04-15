*+IMEXP - Apply the corrections
      SUBROUTINE IMEXP (EXP,ECOR,FILT,IMAG,DAT,VAR,QUAL,NX,NY,STATUS)
      IMPLICIT 	NONE
      INCLUDE	'CONSTANTS.INC'
      INCLUDE	'EXPOS_DEF.INC'

* Input
      RECORD	/EXPOS_DEF/ EXP
      logical	ecor
      integer   filt
      INTEGER	NX, NY
      REAL      DAT(NX,NY)
      REAL	IMAG(360,181)
      REAL      VAR(NX,NY)
      INTEGER   QUAL(NX,NY)

* output
      INTEGER	STATUS

* Local
      integer	        rdim1, rdim2
	parameter (rdim1=360, rdim2=180)
      INTEGER	IX, IY
      REAL	DXAZ, DXEL
      DOUBLE PRECISION	XAZ, XEL
      DOUBLE PRECISION	CEL(2), C2I(3,3), I2C(3,3), I2E(3,3)
      DOUBLE PRECISION	ELON, ELAT
      REAL	ELOD, ELAD
      REAL	EXPO
      INTEGER           FC, LC
      integer		mx, my
      integer		lun2
      integer		blksz
      integer		hdutyp
      logical		anyf
      CHARACTER*255	rgwimag
      character*80  	c_dum
      character*80	re_res
      real		arru(rdim1, rdim2)	! RGW sky maps - uncorr
      real		arrc(rdim1, rdim2)	! RGW sky maps - eff. corr
      real 	        u_corr, corr

* M. Denby OCT 91
* P. McGale OCT 94 - UNIX mods and add in QUAL and VAR values.
* P McGale May 95  - option to correct for detector efficiency
*-

      IF (STATUS .NE. 0) RETURN
*
      DXAZ = 2.*EXP.DAZ/NX
      DXEL = 2.*EXP.DEL/NY

      CEL(1) = EXP.FRA
      CEL(2) = EXP.FDEC
      CALL AX_DMAT (CEL,EXP.ROLL,C2I,I2C)
      CALL AX_DONMXM (I2C,CTOE,I2E)

* Open up efficiency maps if required.
      if (ecor) then
        call getenv("RECAL", re_res)
        if (re_res .eq. ' ') then
	  write(*,*)'   IMEXP'
	  write(*,*)'   Can''t get RECAL environment variable'
        endif
        call chr_fandl(re_res, fc, lc)
        rgwimag = re_res(fc:lc)//'/reexp_eff.fit'
        call ftgiou(lun2, status)
        call ftopen(lun2, rgwimag, 0, blksz, status)
        if (status .eq. 0) then
	  call chr_fandl(rgwimag, fc, lc)
          write(*,*)'   Using efficiency maps: ',rgwimag(fc:lc)
        else
	  STOP '   Error in IMEXP - Opening the efficiency images'
        endif
* Point to correct FITS extension and read in exposure image.
        if (filt .eq. 1) then
          call ftmahd(lun2, 2, hdutyp, status)	  ! S1a with no det. effic.
          call ftg2de(lun2, 1, -1, rdim1, rdim1, rdim2,
     &                                   arru, anyf, status)
          call ftmahd(lun2, 4, hdutyp, status)	  ! S1a with det. effic.
          call ftg2de(lun2, 1, -1, rdim1, rdim1, rdim2,
     &                                   arrc, anyf, status)
        else
          call ftmahd(lun2, 3, hdutyp, status)	  ! S2a with no det. effic.
          call ftg2de(lun2, 1, -1, rdim1, rdim1, rdim2,
     &                                   arru, anyf, status)
          call ftmahd(lun2, 5, hdutyp, status)	  ! S2a with det. effic.
          call ftg2de(lun2, 1, -1, rdim1, rdim1, rdim2,
     &                                   arrc, anyf, status)
	endif

        IF (STATUS .NE. 0) THEN
	  STOP '   Error in IMEXP - Opening the efficiency images'
        ENDIF

      endif

*   Loop over the data
      DO IY = 1,NY
	XEL = ((REAL(IY)-0.5)*DXEL - EXP.DEL)
        DO IX = 1,NX
	  XAZ = (EXP.DAZ - (REAL(IX)-0.5)*DXAZ)
	  CALL AX_DONVRT (XAZ,XEL,I2E,ELON,ELAT)
	  ELOD = ELON/DTOR
	  ELAD = ELAT/DTOR
	  CALL GRIDINT (ELOD,ELAD,IMAG,360,181,EXPO)
	  IF (EXPO .GT. 0.) THEN
	    if (ecor) then
              mx=int(mod(int(elod+0.5)+real(nx), real(nx)))+1
              my=int((elad+90.0)+0.5)+1
              u_corr=arru(mx,my)
              corr=arrc(mx,my)
	      expo = expo*(corr/u_corr)
	    endif
	    DAT(IX,IY) = DAT(IX,IY)/EXPO
            QUAL(IX,IY) = 0
            IF (DAT(IX,IY) .EQ. 0) THEN
	      VAR(IX,IY) = 1.0/EXPO**2
	    ELSE
	      VAR(IX,IY) = DAT(IX,IY)/EXPO**2
	    ENDIF
	  ELSE
	    DAT(IX,IY) =0.
            VAR(IX,IY) = 0.
            QUAL(IX,IY) = 1
	  ENDIF
        ENDDO
      ENDDO

      if (ecor) call ftclos(lun2, status)

      END
