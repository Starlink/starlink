/*
** A clock which shows the hour as fixed-point number X,
** such that
**
**      0.000 <= X < 24.000
**
** X represents a fractional hour, not hours and minutes.
** Thus the time "8.500" means half past 8 o'clock, not
** ten minutes till 9.
*/
#include <time.h>

void main(int argc, char **argv){
  Et_Init(&argc,argv);
  ET_INSTALL_COMMANDS;
  ET( 
    label .x -width 6 -text 00.000 -relief raised -bd 2
    pack .x
    UpdateTime
  );
  Et_MainLoop();
}

/* Update the time displayed in the text widget named ".x".
** Reschedule this routine to be called again after 3.6 
** seconds.
*/
ET_PROC( UpdateTime ){
  struct tm *pTime; /* The time of day, decoded */
  time_t t;         /* Number of seconds since the epoch */
  char buf[40];     /* The time value is written here */

  t = time(0);
  pTime = localtime(&t);
  sprintf(buf,"%2d.%03d",pTime->tm_hour,
     (pTime->tm_sec + 60*pTime->tm_min)*10/36);
  ET( .x config -text %s(buf); after 3600 UpdateTime );
  return ET_OK;
}
