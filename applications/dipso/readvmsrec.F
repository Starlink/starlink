#if HAVE_CONFIG_H
#  include "config.h"
#endif
	SUBROUTINE readvmsrec(sys,type,irec,obuffer,nrec,status)

*  This routine provides the ability to read VMS UNFORMATTED data files
*  on other machines (UNIX workstations).
*  No special compiler facilities are expected or used. The file access
*  is done using DIRECT access reads of 512 byte blocks.
*  Thus if the files are resident on a VMS machine (eg NFS served disk)
*  then they must first have their characteristics altered to 512-byte
*  block (eg like VMS .EXE files) before being read with this routine.
*  The public domain FILE program can do this conversion and is included
*  in the VMS DIPSO distribution.
*
*  This routine should not be called by the VMS version as it is unnecessary.
*
	CHARACTER*80      type
	CHARACTER*32      sys
	INTEGER        ibuffer(32000),status
	INTEGER*2      i2num(64000)
	INTEGER        nrec,irec
        INTEGER        kk,kw,nb
	CHARACTER*80   sbuffer
	BYTE           buffer(120000)
	BYTE           obuffer(120000)
	BYTE           tbuffer(120000)
	REAL           rbuffer(32000)
	REAL           trbuffer(32000)
        INTEGER        ablock(128)
        LOGICAL        more,last
	INTEGER        idx
	LOGICAL        rev,revo
	INTEGER        irecl
        INTEGER        bptr
        INTEGER        i4seg
        INTEGER*2      i2seg(2)
        BYTE           segbuffer(0:3)
        EQUIVALENCE ( segbuffer,i2seg )
        EQUIVALENCE ( segbuffer,i4seg )

	EQUIVALENCE ( ibuffer,buffer )
	EQUIVALENCE ( rbuffer,buffer )
	EQUIVALENCE ( tbuffer,trbuffer )
	EQUIVALENCE ( i2num,buffer )
	EQUIVALENCE ( sbuffer,buffer )

*  If we are on a DEC machine then there are a couple of special settings...
*
*     idx and idx2 point to the most and least significant words of a
*     longword (DEC is the reverse of everybody else)
*
*     rev and rev2 specifiy wether byte reversal is required
*     (rev=.TRUE. means reverse input longword,
*      revo=.TRUE. means reverse output longword and is only used by makeieee)
*
*     irecl specifies the record length in longwords (for DEC0 and in
*     bytes for everybody else.
*
	IF ( sys .EQ. 'ULTRIX' .OR. sys .EQ. 'OSF1' ) THEN
	   idx = 1
           idx2= 2
	   rev=.false.
	   revo=.true.
	   irecl=128

*     If not a DEC machine then assume normal UNIX box values.
*     A new box may require an extra ELSE clause. This code has
*     been tested on   SunOS,Solaris,HP-UX
*
	ELSE
	   irecl=512
	   idx=2
           idx2=1
	   rev=.true.
	   revo=.false.
	ENDIF

*
*  Open the stack file using DIRECT access and 512 byte records
*
	IF ( type(1:5) .EQ. 'OPEN ' ) THEN
	   OPEN ( UNIT=123, FILE=type(6:), FORM='UNFORMATTED',
     :            ACCESS='DIRECT', RECL=irecl, STATUS='OLD' ,
     :            IOSTAT=status )

*
*  Initialise pointers and read first record
*
	   irec=1
	   bptr=1
	   READ ( 123, REC=irec, IOSTAT=status ) ablock
           irec=irec+1

*
*  Close the file
*
	ELSE IF ( type(1:5) .EQ. 'CLOSE' ) THEN
	   CLOSE (123)

*
*  When not OPEN/CLOSing the file we simply read a whole unformatted
*  record from the file, this may overlap many 512 byte blocks.
*  There is not provision at present to read partial records but this
*  could easily be added by specifying number of bytes to read in vmssiz
*  on input to the routine (and changing the code below a bit).
*
*
	ELSE

*
*  There are multiple copies of this block of code but it is faster this
*  way then calling a subroutine every time.
*
*  If we have exhausted the current buffer then get the next 512 byte record
*  otherwise just point to the next longword (4 bytes).
*
           IF ( bptr .LE. 128 ) THEN
             ibuffer(1)=ablock(bptr)
             bptr=bptr+1
           ELSE
	    READ ( 123, REC=irec, IOSTAT=status ) ablock
            irec=irec+1
            bptr=1
            ibuffer(1)=ablock(bptr)
            bptr=bptr+1
           ENDIF

*
*  If machine architecture needs byte reversal do it
*
	   IF ( rev ) CALL makereverse(ibuffer(1))

*
*  Check if end of data in file reached (there may be additional padding
*  but we don't bother reading it)
*
	   IF ( i2num(idx) .EQ. -1 ) status=-1

*
*  If more data in file then
*
	   IF ( status .EQ. 0 ) THEN

*
*  If record-type is 1 then this is the beginning of a segmented record
*  which is split into 2048 byte clumps. Each clump has a record type of
*  zero except the final clump which has a record type of 2.
*
            IF ( i2num(idx2) .EQ. 1 ) THEN

*
*  Get number of bytes in record (excluding 2 bytes for the count itself)
*
	     nrec=i2num(idx)-2
             kk=1
             kw=1
	     nb = 0
             more = .TRUE.
             last = .FALSE.

*
*  Loop reading longwords until we have the whole record (all clumps)
*
             DO WHILE ( more )

*
*  Get next longword of data
*
                IF ( bptr .LE. 128 ) THEN
                 i4seg=ablock(bptr)
                 bptr=bptr+1
                ELSE
	         READ ( 123, REC=irec, IOSTAT=status ) ablock
                 irec=irec+1
                 bptr=1
                 i4seg=ablock(bptr)
                 bptr=bptr+1
                ENDIF

*
*  If we have got all (exactly) the bytes in a clump then
*     if this was the last clump, set exit flag
*     else get length of next clump (setting last flag if last clump)
*
*
                nb = nb + 4
                IF ( nb .EQ. nrec ) THEN
                 buffer(kk)=segbuffer(0)
                 buffer(kk+1)=segbuffer(1)
                 buffer(kk+2)=segbuffer(2)
                 buffer(kk+3)=segbuffer(3)
                 kk=kk+4
                 IF ( last ) THEN
		  more=.FALSE.
                 ELSE

*
*  Get next longword of data
*
                  IF ( bptr .LE. 128 ) THEN
                    i4seg=ablock(bptr)
                    bptr=bptr+1
                  ELSE
               	   READ ( 123, REC=irec, IOSTAT=status ) ablock
                   irec=irec+1
                   bptr=1
                   i4seg=ablock(bptr)
                   bptr=bptr+1
                  ENDIF

  	          IF ( rev ) CALL makereverse(segbuffer)
                  nb = 0
                  nrec=i2seg(idx)-2
                  IF ( i2seg(idx2) .EQ. 2 ) last=.TRUE.
                 ENDIF


*
*  Else if we have got too many bytes (actually always 2 bytes more than
*  we wanted cos everything is I*2 aligned.
*
*     if it was the last clump the set exit flag
*     else copy final two bytes of clump and decode length of next
*        clump. Then read next longword and check record type of next clump
*
                ELSE IF ( nb .GT. nrec ) THEN
                 IF ( last ) THEN
                  more=.FALSE.
                 ELSE
                  buffer(kk)=segbuffer(0)
                  buffer(kk+1)=segbuffer(1)
  	          IF ( rev ) CALL makereverse(segbuffer)
                  nrec=i2seg(idx2)-2

*
*  Get next longword of data
*
                  IF ( bptr .LE. 128 ) THEN
                    i4seg=ablock(bptr)
                    bptr=bptr+1
                  ELSE
       	           READ ( 123, REC=irec, IOSTAT=status ) ablock
                   irec=irec+1
                   bptr=1
                   i4seg=ablock(bptr)
                   bptr=bptr+1
                  ENDIF

  	          IF ( rev ) CALL makereverse(segbuffer)
                  if ( i2seg(idx) .EQ. 2 ) last=.true.
  	          IF ( rev ) CALL makereverse(segbuffer)
                  buffer(kk+2)=segbuffer(2)
                  buffer(kk+3)=segbuffer(3)
                  kk=kk+4
                  nb=2

*
*  In the rare case where we only needed 2 bytes to complete the current record
*  we have to immediately deal with a new record (this block of code
*  could be avoided by complicating the flags etc but I think it is
*  clearer in-line.
*
                  IF ( nb .EQ. nrec ) THEN
                   IF ( last ) THEN
    	            more=.FALSE.
                   ELSE

*
*  Get next longword of data
*
                    IF ( bptr .LE. 128 ) THEN
                      i4seg=ablock(bptr)
                      bptr=bptr+1
                    ELSE
	             READ ( 123, REC=irec, IOSTAT=status ) ablock
                     irec=irec+1
                     bptr=1
                     i4seg=ablock(bptr)
                     bptr=bptr+1
                    ENDIF

  	            IF ( rev ) CALL makereverse(segbuffer)
                    nb = 0
                    nrec=i2seg(idx)-2
                    IF ( i2seg(idx2) .EQ. 2 ) last=.TRUE.
                   ENDIF
                  ENDIF
                 ENDIF

*
*  Otherwise just copy the longword into another buffer
*
                ELSE
                  buffer(kk)=segbuffer(0)
                  buffer(kk+1)=segbuffer(1)
                  buffer(kk+2)=segbuffer(2)
                  buffer(kk+3)=segbuffer(3)
                  kk=kk+4
                ENDIF
		 IF ( type(1:3) .EQ. 'INT' ) THEN
		   IF ( rev) CALL makereverse(ibuffer(kw))
                ENDIF
*
*  Count how many longwords of data we have
*

                kw=kw+1
	     END DO
             nrec=(kw-1)*4+2


*
*  Other record type are MUCH easier to deal with as they are contained
*  in a single section.
*
           ELSE
	     nrec=i2num(idx)
	     do kk=1,(nrec-2)/4

*
*  Get next longword of data
*
              IF ( bptr .LE. 128 ) THEN
                ibuffer(kk)=ablock(bptr)
                bptr=bptr+1
              ELSE
     	       READ ( 123, REC=irec, IOSTAT=status ) ablock
               irec=irec+1
               bptr=1
               ibuffer(kk)=ablock(bptr)
               bptr=bptr+1
              ENDIF

	      IF ( type(1:3) .EQ. 'INT' ) THEN
	        IF ( rev) CALL makereverse(ibuffer(kk))
	      ENDIF
	     end do
            ENDIF
	   ENDIF
	   nrec=nrec-2

*
*  If we have got a buffer full of REAL numbers then they will need
*  converting from VAXF float into IEEE float format.
*  There is no provision for processing REAL*8 but this could be
*  added without much work.
*
*
	   IF ( type(1:4) .EQ. 'REAL' ) THEN
              CALL makeieee(revo,nrec/4,rbuffer,trbuffer)
	      DO kk=1,nrec
		obuffer(kk)=tbuffer(kk)
	      END DO
	   ELSE
	      DO kk=1,nrec
		obuffer(kk)=buffer(kk)
 	      END DO
           ENDIF

	ENDIF

	END
	SUBROUTINE makereverse (data)

*
*  Swap order of bytes in a longword
*

	BYTE     data(0:3),temp(0:3)
	temp(0)=data(3)
	temp(1)=data(2)
	temp(2)=data(1)
	temp(3)=data(0)

	data(0)=temp(0)
	data(1)=temp(1)
	data(2)=temp(2)
	data(3)=temp(3)

	END
	SUBROUTINE makeieee(rev,nn,in,out)


*
*  This routine is translated into fortran from the original C code
*  (shown as comments). For detail see the original source code
*  routine DAT1_CNV_FORMAT.C in the HDS library.
*
        INTEGER     nn
	REAL        in(nn),out(nn)
        LOGICAL     rev
	REAL       vaxf_value,ieee_value
	BYTE       vaxf_byte(0:3),ieee_byte(0:3)
	EQUIVALENCE ( vaxf_byte,vaxf_value)
	EQUIVALENCE ( ieee_byte,ieee_value)
	INTEGER    e,f,iv1,iv2,iv3,iv0

*  Bit masks
	INTEGER    MASK_FF
	INTEGER    MASK_FE
	INTEGER    MASK_7F
	INTEGER    MASK_80
	INTEGER    MASK_01

#if HAVE_BOZ
	DATA MASK_FF / Z'000000FF' /
	DATA MASK_FE / Z'000000FE' /
	DATA MASK_7F / Z'0000007F' /
	DATA MASK_80 / Z'00000080' /
	DATA MASK_01 / Z'00000001' /
#elif HAVE_OLD_TYPELESS_BOZ
	DATA MASK_FF / '000000FF'X /
	DATA MASK_FE / '000000FE'X /
	DATA MASK_7F / '0000007F'X /
	DATA MASK_80 / '00000080'X /
	DATA MASK_01 / '00000001'X /
#else
        Do not have BOZ constants
#endif


        DO ii = 1,  nn
	vaxf_value = in(ii)

!			   e = ( ( vaxf.byte[ 1 ] << 1 ) & 0xfe ) |
!			       ( ( vaxf.byte[ 0 ] >> 7 ) & 0x1 );
!



	iv0 = vaxf_byte(0)
	iv1 = vaxf_byte(1)
	iv2 = vaxf_byte(2)
	iv3 = vaxf_byte(3)
	iv0 = IAND ( iv0 , MASK_FF )
	iv1 = IAND ( iv1 , MASK_FF )
	iv2 = IAND ( iv2 , MASK_FF )
	iv3 = IAND ( iv3 , MASK_FF )

	e =  IOR ( IAND ( ISHFT ( iv1,  1  ) , MASK_FE ) ,
     :             IAND (  iv0 / 128         , MASK_01 )    )


!/* If the (biased) exponent is greater than 2, then the VAXF number can be  */
!/* represented in IEEE_S form as a normalised number. Decrement the	    */
!/* exponent by 2. This allows for a difference of 1 between the exponent    */
!/* bias values of the two number formats and a further difference of one in */
!/* the assumed position of the binary radix point.			    */
!			   if ( e > 2 )
!			   {
!			      e -= 2;
!

	IF ( e .GT. 2 ) THEN

		e = e-2

!/* Construct the resulting IEEE_S number, using the appropriate bytes of    */
!/* the VAXF number but replacing the exponent field with its modified	    */
!/* value.								    */
!			      ieee_s.byte[ 0 ] = ( vaxf.byte[ 1 ] & 0x80 ) |
!						 ( ( e >> 1 ) & 0x7f );
!			      ieee_s.byte[ 1 ] = ( vaxf.byte[ 0 ] & 0x7f ) |
!						 ( ( e << 7 ) & 0x80 );
!			      ieee_s.byte[ 2 ] = vaxf.byte[ 3 ];
!			      ieee_s.byte[ 3 ] = vaxf.byte[ 2 ];
!			   }

	     IF ( .NOT.rev ) THEN
		ieee_byte(0) =  IOR (   IAND ( iv1,  MASK_80 ),
     :                                  IAND ( e/2 , MASK_7F )   )

		ieee_byte(1) =  IOR (   IAND ( iv0,    MASK_7F ) ,
     :                                  IAND ( e*128 , MASK_80 )   )

		ieee_byte(2) = vaxf_byte(3)
		ieee_byte(3) = vaxf_byte(2)

	     ELSE
		ieee_byte(3) =  IOR (   IAND ( iv1,  MASK_80 ) ,
     :                                  IAND ( e/2 , MASK_7F )   )

		ieee_byte(2) =  IOR (   IAND ( iv0,    MASK_7F ) ,
     :                                  IAND ( e*128 , MASK_80 )   )

		ieee_byte(1) = vaxf_byte(3)
		ieee_byte(0) = vaxf_byte(2)
             ENDIF
!
!/* If the (biased) VAXF exponent is zero, then the resulting IEEE_S value   */
!/* is zero (or we have a VAX reserved operand, but we assume that can't	    */
!/* happen).								    */
!			   else if ( e == 0 )
!			   {
!			      ieee_s.value = 0;
!			   }
!


	ELSE IF ( e .EQ. 0 ) THEN
		ieee_value = 0.0

!/* Otherwise, if the (biased) exponent is 2 or less, then the IEEE_S	    */
!/* equivalent will be a denormalised number, so the fraction field must be  */
!/* modified.  Extract all the bits of the VAXF fraction field into a single */
!/* integer (remember we can't assume what order the integer's bytes are	    */
!/* stored in).  Also add the (normally omitted) leading 1.		    */
!			   else
!			   {
!			      f = vaxf.byte[ 2 ] |
!			          ( vaxf.byte[ 3 ] << 8 ) |
!				  ( ( vaxf.byte[ 0 ] & 0x7f ) << 16 ) |
!				  ( 0x1 << 23 );
!


	ELSE

		f = IOR ( iv2 ,
     :                    IOR ( iv3*256 ,
     :				IOR ( IAND ( iv0, MASK_7F ) * 65536 ,
     :                                128*65536 )
     :                         )
     :                  )


!/* Shift the fraction bits to account for the limited range of the	    */
!/* exponent. Then pack the fraction field into the IEEE_S number. Retain    */
!/* the VAXF sign bit, but set the exponent field to zero (indicating a	    */
!/* denormalised number).						    */
!			      f = f >> ( 3 - e );
!			      ieee_s.byte[ 0 ] = vaxf.byte[ 1 ] & 0x80;
!			      ieee_s.byte[ 1 ] = ( f >> 16 ) & 0x7f;
!			      ieee_s.byte[ 2 ] = ( f >> 8 ) & 0xff;
!			      ieee_s.byte[ 3 ] = f & 0xff;
!			   }
!			}
!

		f =  ISHFT ( f , -(3-e) )
             IF ( .NOT.rev ) THEN
		ieee_byte(0) = IAND ( iv1 , MASK_80 )
		ieee_byte(1) = IAND ( f/65536 , MASK_7F )
		ieee_byte(2) = IAND ( f/256 , MASK_FF )
		ieee_byte(3) = IAND ( f , MASK_FF )
             ELSE
		ieee_byte(3) = IAND ( iv1 , MASK_80 )
		ieee_byte(2) = IAND ( f/65536 , MASK_7F )
		ieee_byte(1) = IAND ( f/256 , MASK_FF )
		ieee_byte(0) = IAND ( f , MASK_FF )
	     ENDIF




	ENDIF
!
!
!


	out(ii) =  ieee_value

	END DO

	END
