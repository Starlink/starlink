{+
{ ACVT.ICL
{
{ Author: Phil Daly, JAC, 18-May-1994
{         modified by Colin Aspin to work under IRCAM_CLRED, 8th Aug 1994
{-

{ Defstring some frequently used commands
defstring acvt_open       send acvt obey open
defstring acvt_openw      obeyw acvt open
defstring acvt_close      send acvt obey close
defstring acvt_convert    send acvt obey convert
defstring acvt_cancel     send acvt cancel convert
defstring acvt_insert     send acvt obey insert
defstring acvt_list       send acvt obey list
defstring acvt_verbose    send acvt obey verbose
defstring acvt_noverbose  send acvt obey noverbose
defstring acvt_noverbosew obeyw acvt noverbose
defstring acvt_reset      send acvt obey reset
defstring acvt_status     send acvt obey status

{ Defstring END checking off and on
defstring end_off        send acvt set endcheck false
defstring end_on         send acvt set endcheck true

proc load_acvt
  checktask acvt (loaded)
  if not loaded
    loadw $LIRCAMDIR/acvt
  end if
  acvt_noverbosew
  acvt_openw
  acvt_convert
end proc

proc reload_acvt
  kill_acvt
  load_acvt
end proc

proc kill_acvt
  checktask acvt (loaded)
  if loaded
    acvt_cancel
    acvt_close
    acvt_noverbose
    killw acvt
  end if
end proc

proc acvt_range root first last
  root_len = LEN( root )
  if SUBSTR( root, root_len, 1 ) = '_'
    observation_root = root
  else
    observation_root = root & '_'
  end if
  loop for i = first to last
    observation = SNAME(observation_root,i,1)
    string = '''REDUCE ' & (observation) & ' '' '
    obeyw acvt insert (string)
    string = '''END ' & (observation) & ' '' '
    obeyw acvt insert (string)
  end loop
end proc

proc exit_acvt
  checktask acvt (loaded)
  if loaded
    acvt_cancel
    acvt_close
    acvt_noverbose
    killw acvt
  end if
  exit
end proc

