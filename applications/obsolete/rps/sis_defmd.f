*+SIS_DEFMD
        subroutine SIS_DEFMD( rate, m_data, m_clock, m_bit )

        implicit none

        real rate
        character m_data, m_clock, m_bit

*       Description:
*         Given an estimated count rate, returns a default mode setting for SIS
*
*       Arguments:
*         rate         (i) : Estimated SIS count rate
*         m_data       (o) : Default data mode
*                            F: Faint, B: Bright, H: Fast (high speed)
*         m_clock      (o) : Default clocking mode
*                            4: 4 CCD, 2: 2 CCD, 1: 1 CCD (inc. parallel sum)
*         m_bit        (o) : Default bit rate
*                            M: Meidum rate okay, H: High rate required
*                            L: Low rate okay (NEW as of 2/16/93)
*                            (L assumes <0.4 cps/4-CCD extragalactic+Galactic
*                                background!!!!!)
*
*       Origin:
*         Suggested by KM on 1992 Sep 30, slightly modified.
*
*       Dependencies:
*         None
*
*       Authors:
*         Koji Mukai, 1993 Feb 16, Added Low Bit Rate
*         Koji Mukai, 1992 Dec 17, original version
*-SIS_DEFMD

        if( rate .gt. 50.0 ) then
          m_data = 'H'
          m_clock = '1'
          m_bit = 'H'
        else if( rate .gt. 25.0 ) then
          m_data = 'H'
          m_clock = '1'
          m_bit = 'M'
        else if( rate .gt. 12.5 ) then
          m_data = 'B'
          m_clock = '4'
          m_bit = 'M'
        else if( rate .gt. 6.0 ) then
          m_data = 'B'
          m_clock = '2'
          m_bit = 'M'
        else if( rate .gt. 1.4 ) then
          m_data = 'B'
          m_clock = '4'
          m_bit = 'M'
        else if( rate .gt. 0.1 ) then
          m_data = 'F'
          m_clock = '4'
          m_bit = 'M'
        else
          m_data = 'F'
          m_clock = '4'
          m_bit = 'L'
        end if

        end
