/*
** This file codes a demonstration utility which can be used to change
** the pitch, duration, and volume of the X11 "bell".
*/
#include "tk.h"

int main(int argc, char **argv){
  Et_Init(&argc,argv);        /* Initialize Tcl/Tk */
  ET_INSTALL_COMMANDS;        /* Install the "bell" command defined below */
  ET(                         # Construct the main window
    frame .f
    pack .f -padx 5 -pady 5
    label .f.l -text {Bell Parameters} -width 80
    pack .f.l -side top
    scale .f.sv -label Volume -from 0 -to 100 -orient horizontal
    .f.sv set 50
    scale .f.sp -label Pitch -from 100 -to 3000 -orient horizontal
    .f.sp set 440
    scale .f.sd -label Duration -from 10 -to 1000 -orient horizontal
    .f.sd set 100
    pack .f.sv .f.sp .f.sd -padx 4 -pady 8 -side top -fill x
    frame .f.b -bd 2 -relief sunken
    pack .f.b -side top -fill x
    button .f.b.quit -text Quit -command exit
    button .f.b.test -text Test -command {
      bell -pitch [.f.sp get] -volume [.f.sv get] -duration [.f.sd get]
    }
    button .f.b.set -text Set -command {
      bell -pitch [.f.sp get] -volume [.f.sv get] -duration [.f.sd get] -set 1
    }
    pack .f.b.set .f.b.test .f.b.quit -side left -expand 1 -padx 5 -pady 5
  );
  Et_MainLoop();              /* Enter the event loop */
  return 0;
}

/* A Tcl/Tk command to ring the bell.  Options -pitch, -duration and
** -volume alter the sound of the bell.  The changes are normally temporary,
** but can be made permanent by the -set option.
*/
ET_PROC( bell ){
  XKeyboardControl values;    /* For changing parameters of the bell */
  XKeyboardState kbstate;     /* Prior state of the bell */
  int mask;                   /* Mask of which parameters to change */
  int set_flag = 0;           /* True to make bell setting permanent */
  int i;                      /* Loop counter */

  /* Loop over the command line arguments, and set the "values" and "mask"
  ** variables in response to the "-pitch", "-duration" and "-volume" 
  ** switches.  The "set_flag" variable is changed by the "-set" option. */
  mask = 0;
  for(i=1; i<argc; i+=2){
    if( i+1==argc ){
      Tcl_AppendResult(Et_Interp,"Wrong # of arguments",0);
      return ET_ERROR;
    }else if( strcmp(argv[i],"-pitch")==0 ){
      mask |= KBBellPitch;
      if( Tcl_GetInt(Et_Interp,argv[i+1],&values.bell_pitch)==TCL_ERROR ){
        return ET_ERROR;
      }
    }else if( strcmp(argv[i],"-volume")==0 ){
      mask |= KBBellPercent;
      if( Tcl_GetInt(Et_Interp,argv[i+1],&values.bell_percent)==TCL_ERROR ){
        return ET_ERROR;
      }
      values.bell_percent = atoi(argv[i+1]);
    }else if( strcmp(argv[i],"-duration")==0 ){
      mask |= KBBellDuration;
      if( Tcl_GetInt(Et_Interp,argv[i+1],&values.bell_duration)==TCL_ERROR ){
        return ET_ERROR;
      }
    }else if( strcmp(argv[i],"-set")==0 ){
      if( Tcl_GetBoolean(Et_Interp,argv[i+1],&set_flag)==TCL_ERROR ){
        return ET_ERROR;
      }
    }else{
      Tcl_AppendResult(Et_Interp,"Unknown option: ",argv[i],
         ".  Valid options are -pitch, -volume, -duration and -set.",0);
      return ET_ERROR;
    }
  }

  /* Set the appropriate parameters and sound the bell */
  if( mask ){
    XGetKeyboardControl(Et_Display,&kbstate);
    XChangeKeyboardControl(Et_Display,mask,&values);
  }
  XBell(Et_Display,0);
  /* XForceScreenSaver(Et_Display,ScreenSaverReset); */

  /* If the bell's parameters changed and user doesn't wants to make these
  ** changes permanent, then restore the bell parameters to their initial
  ** value before exiting. */
  if( mask && !set_flag ){
    values.bell_percent = kbstate.bell_percent;
    values.bell_pitch = kbstate.bell_pitch;
    values.bell_duration = kbstate.bell_duration;
    XChangeKeyboardControl(Et_Display,mask,&values);
  }
  return ET_OK;
}
