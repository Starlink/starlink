{ PROCEDURE RELOAD_OBSRAP : kills obsrap, deletes SDF file and re-loads
proc reload_obsrap
{  print "Killing task OBSRAP..."
{  killw obsrap
{  print "Deleting file ADAM_USER:OBSRAP.SDF..."
{  delfile $ADAM_USER/obsrap.sdf
{  print "Re-loading task OBSRAP..."
{  welcome_obsrap
  exception ADAMERR
  end exception
end proc
