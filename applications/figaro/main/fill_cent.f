      subroutine fill_cent(cent,line_count,sdata,sdens,wavdim,left
     :            ,right,line_name,ncent,wavelength,dwaves)
*+
* Name:
*    FILL_CENT

* Invocation:
*    CALL FILL_CENT(CENT,LINE_COUNT,SDATA,SDENS,WAVDIM,LEFT
*                 ,RIGHT,LINE_NAME,NCENT,WAVELENGTH,DWAVES)

* Purpose:
*    To read the centres and wavelengths of lines into the arrays CENT
*    and WAVES when the line id is known.

* Description:
*    To read the centres and wavelengths of lines into the arrays CENT
*    and WAVES when the line id is known.
*
* Arguments:
*      LINE_COUNT = INTEGER (Given)
*        Number of lines found
*      WAVDIM = INTEGER (Given)
*        Number of channels in data
*      SDENS(WAVDIM) = REAL ARRAY (Given)
*        Intensity data
*      SDATA(WAVDIM) = REAL ARRAY (Given)
*        Wavelength array (for SDENS)
*      LEFT(LINE_COUNT) = REAL ARRAY (Given)
*        Lower limits of line bounds
*      RIGHT(LINE_COUNT) = REAL ARRAY (Given)
*        Upper limits of line bounds
*      WAVELENGTH(LINE_COUNT) = REAL ARRAY (Given)
*        Wavelengths of lines
*      LINE_NAME(LINE_COUNT) = CHARACTER*10 ARRAY (Given)
*        Line names
*      NCENT = INTEGER (Returned)
*        Number of centres found
*      DWAVES(LINE_COUNT) = DOUBLE PRECISION ARRAY (Returned)
*        Wavelengths of lines with ids
*      CENT(LINE_COUNT) = DOUBLE PRECISION ARRAY (Returned)
*        Centres of lines in same units as SDENS
* History:
*     T.N.Wilkins. Manchester. February 1987
*         "        Cambridge. 6th February 1991 Check for wavelength
*                             greater han zero.
*-
      implicit none
      integer line_count,wavdim,ncent
      double precision cent(line_count),dwaves(line_count)
      real sdata(wavdim),sdens(wavdim),wavelength(line_count)
      real left(line_count)
      real right(line_count)
      character*10 line_name(line_count)
      integer i

      ncent = 0
      do i = 1, line_count
        if((line_name(i).ne.'UNKNOWN   ')
     :                  .and.(wavelength(i).gt.0.0)) then
          ncent = ncent + 1
          call get_peak(cent(ncent),sdata,sdens,wavdim,left(i),
     :           right(i))
          dwaves(ncent) = wavelength(i)
        end if
      end do
      end
