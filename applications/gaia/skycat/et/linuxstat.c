/* 
** A system performance monitor for Linux written in ET.
*/
#include <stdio.h>
#include <time.h>
#include "tcl.h"

/*
** Global variables shared between C and Tcl/Tk.
**
**     CpuUpdateTime         is how often to update the CPU usage meter,
**                           in milliseconds.
**
**     CpuAveragingWindow    is how long of an interval to measure the 
**                           CPU usage, in milliseconds.  This is normally
**                           several times larger than CpuUpdateTime.
**
**     MemUpdateTime         How often to update the memory usage displays
**
**     PowerUpdateTime       How often to update the power display.  If
**                           0, then don't show the power.
*/
int CpuUpdateTime = 100;
int CpuAveragingWindow = 400;
int MemUpdateTime = 500;
int PowerUpdateTime = 3000;

/*
** This is the main procedure.
*/
int main(int argc,char **argv){
  Et_Init(&argc,argv);
  Tcl_LinkVar(Et_Interp,"CpuUpdateTime",(char*)&CpuUpdateTime,TCL_LINK_INT);
  Tcl_LinkVar(Et_Interp,"CpuAveragingWindow",
              (char*)&CpuAveragingWindow,TCL_LINK_INT);
  Tcl_LinkVar(Et_Interp,"MemUpdateTime",(char*)&MemUpdateTime,TCL_LINK_INT);
  Tcl_LinkVar(Et_Interp,"PowerUpdateTime",(char*)&PowerUpdateTime,TCL_LINK_INT);
  if( access("/proc/apm",0)!=0 ){
    PowerUpdateTime = 0;           /* Turn off power display if this system
                                    * doesn't support APM */
  }
  ET_INSTALL_COMMANDS;             /* Install Tcl/Tk commands defined below */
  ET_INCLUDE( linuxstat.tcl );     /* Setup the main window */
  Et_MainLoop();                   /* Process events forever */
}

/*
** Read CPU usage statistics from the /proc/stat file and update
** the screen accordingly.
**
** After the display has been updated, this function arranges for
** itself to be called again after the number of milliseconds specified
** by the variable "CpuUpdateTime".
*/
ET_PROC( UpdateCPU ){
  /* These 4 variables hold the user, nice, system and idle times
  ** from the previous invocations of this function.  These values
  ** are subtracted from the current times to get the amount of CPU
  ** time spent in each mode.  The times are kept in a circular
  ** buffer whose size is determined by the size of the averaging
  ** window */
  static int *prevUsr;
  static int *prevNice;
  static int *prevSys;
  static int *prevIdle;

  /* This is the index of the next slot of the circular buffers to use */
  static int index;

  /* This is the size of the circular buffers */
  static int limit = 0;

  /* These variables hold the CPU times obtained at this function call */
  int thisUsr, thisNice, thisSys, thisIdle;

  /* These variables hold the amount of CPU type devoted to user, nice,
  ** system and idle, over the averaging window time. "total"
  ** is the total of the four, and is used for normalization */
  int usr, nice, sys, idle, total;

  /* These variables hold the X-coordinate of the right edge of the line
  ** which is drawn to represent each of the times */
  int usrEnd, niceEnd, sysEnd, idleEnd;

  /* This is the input stream for the /proc/stat file */
  FILE *pIn;

  /* The current correct value for "limit" */
  int newLimit;

  /* The next value for "index" */
  int nextIndex;

  /* open the /proc/stat file and extract the four CPU times contained
  ** therein */
  pIn = fopen("/proc/stat","r");
  if( pIn==0 ) return ET_OK;
  if( fscanf(pIn,"cpu %d %d %d %d",&thisUsr,&thisNice,&thisSys,&thisIdle)<4 ){
    fclose(pIn);
    return ET_OK;
  }
  fclose(pIn);

  /* Update the size of the circular buffer, if necessary */
  newLimit = CpuUpdateTime>0 ? CpuAveragingWindow/CpuUpdateTime : 1;
  if( newLimit<1 ) newLimit = 1;
  if( newLimit>100 ) newLimit = 100;
  if( newLimit!=limit ){
    if( limit>0 ){
      free( prevUsr );
    }
    limit = newLimit;
    prevUsr = (int*)malloc( sizeof(int)*limit*4 );
    memset(prevUsr,0,sizeof(int)*limit*4);
    prevNice = &prevUsr[limit];
    prevSys = &prevNice[limit];
    prevIdle = &prevSys[limit];
    index = 0;
  }

  /* Get the CPU statistics averaged over time */
  nextIndex = index + 1;
  if( nextIndex>=limit ) nextIndex = 0;
  usr = prevUsr[nextIndex] - thisUsr;
  prevUsr[nextIndex] = thisUsr;
  nice = prevNice[nextIndex] - thisNice;
  prevNice[nextIndex] = thisNice;
  sys = prevSys[nextIndex] - thisSys;
  prevSys[nextIndex] = thisSys;
  idle = prevIdle[nextIndex] - thisIdle;
  prevIdle[nextIndex] = thisIdle;
  total = usr + nice + sys + idle;
  index = nextIndex;

  /* Eliminate flashing on the right margin */
  if( usr>=-1 ){ usr = 0; }
  if( nice>=-1 ) nice = 0;
  if( sys>=-1 ) sys = 0;

  /* Update the display */
  usrEnd = 35 + usr*100/total;
  niceEnd = usrEnd + nice*100/total;
  sysEnd = niceEnd + sys*100/total;
  ET(
    .c coords cU 35 18 %d(usrEnd) 18
    .c coords cN %d(usrEnd) 18 %d(niceEnd) 18
    .c coords cS %d(niceEnd) 18 %d(sysEnd) 18
    .c coords cI %d(sysEnd) 18 135 18
    after %d(CpuUpdateTime) UpdateCPU
  );
  return ET_OK;
}

/*
** Read memory usage statistics from the /proc/meminfo file and update
** the screen accordingly.
**
** After the display has been updated, this function arranges for
** itself to be called again after the number of milliseconds specified
** by the variable "MemUpdateTime".
*/
ET_PROC( UpdateMem )
{
  /* The following variables hold the values of the memory statistics
  ** as taken from the /proc/meminfo file */
  int coreFree;       /* Unused memory */
  int coreUsed;       /* Memory used for programs */
  int coreBuffer;     /* Memory used for disk buffers */
  int coreCache;      /* Memory used for disk cache */
  int swapFree;       /* Unused swap space */
  int swapUsed;       /* Swap space in use */

  /* The next variables are intermediate results */
  int coreNormalizer;   /* Used to normalize the core memory sizes */
  int coreUsedEnd;      /* End of the bar showing core in use */
  int coreBufferEnd;    /* End coordinate of the bar showing core used for
                        ** as disk buffer space */
  int coreCacheEnd;     /* End of the bar showing core used for disk cache */
  int swapUsedEnd;      /* End coordinate of the bar showing swap used */
  char zCache[100];      /* Text of the cache used amount */

  /* This is the input stream for the /proc/stat file */
  FILE *pIn;

  /* Open the /proc/stat file and extract the values we need.
  ** This is a little tricky because some versions of Linux do no
  ** report the disk cache amount.  On such systems, the disk cache
  ** and disk buffers are combined into one number.
  */
  pIn = fopen("/proc/meminfo","r");
  if( pIn==0 ) return ET_OK;
  zCache[0] = 0;
  if( fscanf(pIn,"%*[^\n]\nMem: %*d %d %d %*d %d%99[^\n]\nSwap: %*d %d %d",
                 &coreUsed,&coreFree,&coreBuffer,zCache,
                 &swapUsed,&swapFree)!=6 ){
    fclose(pIn);
    return ET_OK;
  }
  fclose(pIn);

  /* Update the display */
  coreCache = atoi(zCache);
  if( coreCache==0 ){
    coreCache = coreBuffer;
    coreBuffer = 0;
  }
  coreNormalizer = (coreUsed + coreFree)/100;
  coreUsedEnd = (coreUsed-coreBuffer-coreCache)/coreNormalizer + 35;
  coreBufferEnd = coreBuffer/coreNormalizer + coreUsedEnd;
  coreCacheEnd = coreCache/coreNormalizer + coreBufferEnd;
  if( swapUsed>0 ){
    swapUsedEnd = swapUsed/((swapUsed+swapFree)/100) + 35;
  }else{
    swapUsedEnd = 35;
  }
  ET(
    .c coords mU 35 6 %d(coreUsedEnd) 6
    .c coords mB %d(coreUsedEnd) 6 %d(coreBufferEnd) 6
    .c coords mC %d(coreBufferEnd) 6 %d(coreCacheEnd) 6
    .c coords mF %d(coreCacheEnd) 6 135 6
    .c coords sU 35 12 %d(swapUsedEnd) 12
    .c coords sF %d(swapUsedEnd) 12 135 12
    after %d(MemUpdateTime) UpdateMem
  );
  return ET_OK;
}

/*
** Update the time display.  Do this every 60 seconds
*/
ET_PROC( UpdateTime ){
  struct tm *pTime; /* The time of day, decoded */
  time_t t;         /* Number of seconds since the epoch */
  int hour;         /* The 12-hour clock hour */
  char zBuf[50];    /* Write the time value here */
  static char *zDay[] = { "Sun", "Mon", "Tue", "Wed", "Thr", "Fri", "Sat" };
  static char *zMonth[] = {
     "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jly", "Aug",
     "Sep", "Oct", "Nov", "Dec" };

  t = time(0);
  pTime = localtime(&t);
  if( pTime->tm_hour==0 ){
    hour = 12;
  }else if( pTime->tm_hour>12 ){
    hour = pTime->tm_hour - 12;
  }else{
    hour = pTime->tm_hour;
  }
  sprintf(zBuf,"%d:%02d%s",hour,pTime->tm_min,
     (pTime->tm_hour > 11) ? "pm" : "am");
  ET( 
     .c itemconfig tT -text %s(zBuf)
     .c itemconfig tD -text \
     "%s(zDay[pTime->tm_wday]) %s(zMonth[pTime->tm_mon]) %d(pTime->tm_mday)"
     after 60000 UpdateTime
   );
  return ET_OK;
}

/*
** Update the power display.
*/
ET_PROC( UpdatePower ){
  FILE *pIn;
  int i;
  int cnt;
  int percent;
  char zLine[200];

  if( PowerUpdateTime<=0 ) goto quit;
  pIn = fopen("/proc/apm","r");
  if( pIn==0 ) goto quit;
  percent = 100;
  zLine[0] = 0;
  fgets(zLine,sizeof(zLine),pIn);
  fclose(pIn);
  for(i=cnt=0; zLine[i] && cnt<6; i++){
    if( zLine[i]==' ' ) cnt++;
  }
  if( zLine[i] ){
    percent = atoi(&zLine[i]);
  }
  ET(
    .c coords pU 35 24 %d(135-percent) 24
    .c coords pA %d(135-percent) 24 135 24
    after %d(PowerUpdateTime) UpdatePower
  );
  return ET_OK;

quit:
  ET(
    .c coords pU 35 24 35 24
    .c coords pA 35 24 135 24
  );
  return ET_OK;
}
