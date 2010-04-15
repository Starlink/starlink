{+
{
{  p4.icl
{
{  author: Phil Daly
{  date:   30-Nov-1994
{
{-

{
{ Do not save procedures
set nosave
set nocheckpars

{
{ Get some semi-global variables
p4_alias    = getenv("PID") & 'p4'
p4_nb_alias = 'p' & getenv("PID") & '_plotnb'
p4_date     = getenv("P4_DATE")
p4_hc       = getenv("PWD")&'/gks74.ps'
p4_df       = getenv("HOME")&'/cgs4dr_configs/default.p4'

{
{ Print welcome message
print ' '
print '      Welcome to P4'
print ' '

{
{ Defstring the basic commands
defstring p4_bar        obeyw (p4_alias) bar
defstring p4_clear      obeyw (p4_alias) clear
defstring p4_close_nb   obeyw (p4_alias) close_nb
defstring p4_close_port obeyw (p4_alias) close_port
defstring p4_close_dev  obeyw (p4_alias) close_port
defstring p4_cursor     obeyw (p4_alias) cursor
defstring p4_cursorval  obeyw (p4_alias) cursorval
defstring p4_display    obeyw (p4_alias) display
defstring p4_identify   obeyw (p4_alias) identify
defstring p4_list_cb    obeyw (p4_alias) list_cb
defstring p4_list_nb    obeyw (p4_alias) list_nb
defstring p4_lut        obeyw (p4_alias) lut
defstring p4_open_nb    obeyw (p4_alias) open_nb
defstring p4_reset      obeyw (p4_alias) reset
defstring p4_restore    obeyw (p4_alias) restore
defstring p4_save       obeyw (p4_alias) save
defstring p4_status     obeyw (p4_alias) status
defstring p4_verbose    obeyw (p4_alias) verbose
defstring p4_noverbose  obeyw (p4_alias) noverbose
defstring p4_clearall   obeyw (p4_alias) clear port=0
defstring p4_saved      obeyw (p4_alias) save file=(p4_df) port=-1
defstring p4_restored   obeyw (p4_alias) restore file=(p4_df) port=-1

{
{ Defstring some miscellaneous commands
defstring p4_loadw         loadw ${P4_EXE}/p4 (p4_alias)
defstring p4_killw         killw (p4_alias)
defstring p4_set_nb        send (p4_alias) set noticeboard (p4_nb_alias)
defstring p4_get_nb        send (p4_alias) get noticeboard
defstring p4_get_cs        get (p4_alias) cursor_status
defstring p4_get_param     get (p4_alias)
defstring p4_get_act_x     get (p4_alias) act_x
defstring p4_get_act_y     get (p4_alias) act_y
defstring p4_get_dataval   get (p4_alias) dataval
defstring p4_get_dataerr   get (p4_alias) dataerr
defstring p4_get_dataqual  get (p4_alias) dataqual
defstring p4_get_keystroke get (p4_alias) keystroke
defstring p4_psprint       sh lp -c (p4_hc)
defstring p4_psprintc      sh lp -c -dcolor (p4_hc)
defstring p4_rmpsfile      sh rm (p4_hc)
defstring p4_rmfile        sh rm (p4_df)
defstring p4_devices       sh more /star/starlink/lib/gns/gksnames.dat

{
{ Defstring the set_ commands
defstring set_device0    putnbs ((p4_nb_alias)&'.port_0.device_name')
defstring set_device1    putnbs ((p4_nb_alias)&'.port_1.device_name')
defstring set_device2    putnbs ((p4_nb_alias)&'.port_2.device_name')
defstring set_device3    putnbs ((p4_nb_alias)&'.port_3.device_name')
defstring set_device4    putnbs ((p4_nb_alias)&'.port_4.device_name')
defstring set_device5    putnbs ((p4_nb_alias)&'.port_5.device_name')
defstring set_device6    putnbs ((p4_nb_alias)&'.port_6.device_name')
defstring set_device7    putnbs ((p4_nb_alias)&'.port_7.device_name')
defstring set_device8    putnbs ((p4_nb_alias)&'.port_8.device_name')
defstring set_lut0       putnbs ((p4_nb_alias)&'.port_0.device_lut')
defstring set_lut1       putnbs ((p4_nb_alias)&'.port_1.device_lut')
defstring set_lut2       putnbs ((p4_nb_alias)&'.port_2.device_lut')
defstring set_lut3       putnbs ((p4_nb_alias)&'.port_3.device_lut')
defstring set_lut4       putnbs ((p4_nb_alias)&'.port_4.device_lut')
defstring set_lut5       putnbs ((p4_nb_alias)&'.port_5.device_lut')
defstring set_lut6       putnbs ((p4_nb_alias)&'.port_6.device_lut')
defstring set_lut7       putnbs ((p4_nb_alias)&'.port_7.device_lut')
defstring set_lut8       putnbs ((p4_nb_alias)&'.port_8.device_lut')
defstring set_title0     putnbs ((p4_nb_alias)&'.port_0.title')
defstring set_title1     putnbs ((p4_nb_alias)&'.port_1.title')
defstring set_title2     putnbs ((p4_nb_alias)&'.port_2.title')
defstring set_title3     putnbs ((p4_nb_alias)&'.port_3.title')
defstring set_title4     putnbs ((p4_nb_alias)&'.port_4.title')
defstring set_title5     putnbs ((p4_nb_alias)&'.port_5.title')
defstring set_title6     putnbs ((p4_nb_alias)&'.port_6.title')
defstring set_title7     putnbs ((p4_nb_alias)&'.port_7.title')
defstring set_title8     putnbs ((p4_nb_alias)&'.port_8.title')
defstring set_type0      putnbs ((p4_nb_alias)&'.port_0.display_type')
defstring set_type1      putnbs ((p4_nb_alias)&'.port_1.display_type')
defstring set_type2      putnbs ((p4_nb_alias)&'.port_2.display_type')
defstring set_type3      putnbs ((p4_nb_alias)&'.port_3.display_type')
defstring set_type4      putnbs ((p4_nb_alias)&'.port_4.display_type')
defstring set_type5      putnbs ((p4_nb_alias)&'.port_5.display_type')
defstring set_type6      putnbs ((p4_nb_alias)&'.port_6.display_type')
defstring set_type7      putnbs ((p4_nb_alias)&'.port_7.display_type')
defstring set_type8      putnbs ((p4_nb_alias)&'.port_8.display_type')
defstring set_plane0     putnbs ((p4_nb_alias)&'.port_0.display_plane')
defstring set_plane1     putnbs ((p4_nb_alias)&'.port_1.display_plane')
defstring set_plane2     putnbs ((p4_nb_alias)&'.port_2.display_plane')
defstring set_plane3     putnbs ((p4_nb_alias)&'.port_3.display_plane')
defstring set_plane4     putnbs ((p4_nb_alias)&'.port_4.display_plane')
defstring set_plane5     putnbs ((p4_nb_alias)&'.port_5.display_plane')
defstring set_plane6     putnbs ((p4_nb_alias)&'.port_6.display_plane')
defstring set_plane7     putnbs ((p4_nb_alias)&'.port_7.display_plane')
defstring set_plane8     putnbs ((p4_nb_alias)&'.port_8.display_plane')
defstring set_autoscale0 putnbs ((p4_nb_alias)&'.port_0.autoscale')
defstring set_autoscale1 putnbs ((p4_nb_alias)&'.port_1.autoscale')
defstring set_autoscale2 putnbs ((p4_nb_alias)&'.port_2.autoscale')
defstring set_autoscale3 putnbs ((p4_nb_alias)&'.port_3.autoscale')
defstring set_autoscale4 putnbs ((p4_nb_alias)&'.port_4.autoscale')
defstring set_autoscale5 putnbs ((p4_nb_alias)&'.port_5.autoscale')
defstring set_autoscale6 putnbs ((p4_nb_alias)&'.port_6.autoscale')
defstring set_autoscale7 putnbs ((p4_nb_alias)&'.port_7.autoscale')
defstring set_autoscale8 putnbs ((p4_nb_alias)&'.port_8.autoscale')
defstring set_erase0     putnbs ((p4_nb_alias)&'.port_0.pre_erase_plot')
defstring set_erase1     putnbs ((p4_nb_alias)&'.port_1.pre_erase_plot')
defstring set_erase2     putnbs ((p4_nb_alias)&'.port_2.pre_erase_plot')
defstring set_erase3     putnbs ((p4_nb_alias)&'.port_3.pre_erase_plot')
defstring set_erase4     putnbs ((p4_nb_alias)&'.port_4.pre_erase_plot')
defstring set_erase5     putnbs ((p4_nb_alias)&'.port_5.pre_erase_plot')
defstring set_erase6     putnbs ((p4_nb_alias)&'.port_6.pre_erase_plot')
defstring set_erase7     putnbs ((p4_nb_alias)&'.port_7.pre_erase_plot')
defstring set_erase8     putnbs ((p4_nb_alias)&'.port_8.pre_erase_plot')
defstring set_axes0      putnbs ((p4_nb_alias)&'.port_0.plot_axes')
defstring set_axes1      putnbs ((p4_nb_alias)&'.port_1.plot_axes')
defstring set_axes2      putnbs ((p4_nb_alias)&'.port_2.plot_axes')
defstring set_axes3      putnbs ((p4_nb_alias)&'.port_3.plot_axes')
defstring set_axes4      putnbs ((p4_nb_alias)&'.port_4.plot_axes')
defstring set_axes5      putnbs ((p4_nb_alias)&'.port_5.plot_axes')
defstring set_axes6      putnbs ((p4_nb_alias)&'.port_6.plot_axes')
defstring set_axes7      putnbs ((p4_nb_alias)&'.port_7.plot_axes')
defstring set_axes8      putnbs ((p4_nb_alias)&'.port_8.plot_axes')
defstring set_high0      putnbs ((p4_nb_alias)&'.port_0.high')
defstring set_high1      putnbs ((p4_nb_alias)&'.port_1.high')
defstring set_high2      putnbs ((p4_nb_alias)&'.port_2.high')
defstring set_high3      putnbs ((p4_nb_alias)&'.port_3.high')
defstring set_high4      putnbs ((p4_nb_alias)&'.port_4.high')
defstring set_high5      putnbs ((p4_nb_alias)&'.port_5.high')
defstring set_high6      putnbs ((p4_nb_alias)&'.port_6.high')
defstring set_high7      putnbs ((p4_nb_alias)&'.port_7.high')
defstring set_high8      putnbs ((p4_nb_alias)&'.port_8.high')
defstring set_low0       putnbs ((p4_nb_alias)&'.port_0.low')
defstring set_low1       putnbs ((p4_nb_alias)&'.port_1.low')
defstring set_low2       putnbs ((p4_nb_alias)&'.port_2.low')
defstring set_low3       putnbs ((p4_nb_alias)&'.port_3.low')
defstring set_low4       putnbs ((p4_nb_alias)&'.port_4.low')
defstring set_low5       putnbs ((p4_nb_alias)&'.port_5.low')
defstring set_low6       putnbs ((p4_nb_alias)&'.port_6.low')
defstring set_low7       putnbs ((p4_nb_alias)&'.port_7.low')
defstring set_low8       putnbs ((p4_nb_alias)&'.port_8.low')
defstring set_whole0     putnbs ((p4_nb_alias)&'.port_0.plot_whole')
defstring set_whole1     putnbs ((p4_nb_alias)&'.port_1.plot_whole')
defstring set_whole2     putnbs ((p4_nb_alias)&'.port_2.plot_whole')
defstring set_whole3     putnbs ((p4_nb_alias)&'.port_3.plot_whole')
defstring set_whole4     putnbs ((p4_nb_alias)&'.port_4.plot_whole')
defstring set_whole5     putnbs ((p4_nb_alias)&'.port_5.plot_whole')
defstring set_whole6     putnbs ((p4_nb_alias)&'.port_6.plot_whole')
defstring set_whole7     putnbs ((p4_nb_alias)&'.port_7.plot_whole')
defstring set_whole8     putnbs ((p4_nb_alias)&'.port_8.plot_whole')
defstring set_errors0    putnbs ((p4_nb_alias)&'.port_0.plot_errors')
defstring set_errors1    putnbs ((p4_nb_alias)&'.port_1.plot_errors')
defstring set_errors2    putnbs ((p4_nb_alias)&'.port_2.plot_errors')
defstring set_errors3    putnbs ((p4_nb_alias)&'.port_3.plot_errors')
defstring set_errors4    putnbs ((p4_nb_alias)&'.port_4.plot_errors')
defstring set_errors5    putnbs ((p4_nb_alias)&'.port_5.plot_errors')
defstring set_errors6    putnbs ((p4_nb_alias)&'.port_6.plot_errors')
defstring set_errors7    putnbs ((p4_nb_alias)&'.port_7.plot_errors')
defstring set_errors8    putnbs ((p4_nb_alias)&'.port_8.plot_errors')
defstring set_bins0      putnbs ((p4_nb_alias)&'.port_0.histogram_bins')
defstring set_bins1      putnbs ((p4_nb_alias)&'.port_1.histogram_bins')
defstring set_bins2      putnbs ((p4_nb_alias)&'.port_2.histogram_bins')
defstring set_bins3      putnbs ((p4_nb_alias)&'.port_3.histogram_bins')
defstring set_bins4      putnbs ((p4_nb_alias)&'.port_4.histogram_bins')
defstring set_bins5      putnbs ((p4_nb_alias)&'.port_5.histogram_bins')
defstring set_bins6      putnbs ((p4_nb_alias)&'.port_6.histogram_bins')
defstring set_bins7      putnbs ((p4_nb_alias)&'.port_7.histogram_bins')
defstring set_bins8      putnbs ((p4_nb_alias)&'.port_8.histogram_bins')
defstring set_xstart0    putnbs ((p4_nb_alias)&'.port_0.xstart')
defstring set_xstart1    putnbs ((p4_nb_alias)&'.port_1.xstart')
defstring set_xstart2    putnbs ((p4_nb_alias)&'.port_2.xstart')
defstring set_xstart3    putnbs ((p4_nb_alias)&'.port_3.xstart')
defstring set_xstart4    putnbs ((p4_nb_alias)&'.port_4.xstart')
defstring set_xstart5    putnbs ((p4_nb_alias)&'.port_5.xstart')
defstring set_xstart6    putnbs ((p4_nb_alias)&'.port_6.xstart')
defstring set_xstart7    putnbs ((p4_nb_alias)&'.port_7.xstart')
defstring set_xstart8    putnbs ((p4_nb_alias)&'.port_8.xstart')
defstring set_ystart0    putnbs ((p4_nb_alias)&'.port_0.ystart')
defstring set_ystart1    putnbs ((p4_nb_alias)&'.port_1.ystart')
defstring set_ystart2    putnbs ((p4_nb_alias)&'.port_2.ystart')
defstring set_ystart3    putnbs ((p4_nb_alias)&'.port_3.ystart')
defstring set_ystart4    putnbs ((p4_nb_alias)&'.port_4.ystart')
defstring set_ystart5    putnbs ((p4_nb_alias)&'.port_5.ystart')
defstring set_ystart6    putnbs ((p4_nb_alias)&'.port_6.ystart')
defstring set_ystart7    putnbs ((p4_nb_alias)&'.port_7.ystart')
defstring set_ystart8    putnbs ((p4_nb_alias)&'.port_8.ystart')
defstring set_xend0      putnbs ((p4_nb_alias)&'.port_0.xend')
defstring set_xend1      putnbs ((p4_nb_alias)&'.port_1.xend')
defstring set_xend2      putnbs ((p4_nb_alias)&'.port_2.xend')
defstring set_xend3      putnbs ((p4_nb_alias)&'.port_3.xend')
defstring set_xend4      putnbs ((p4_nb_alias)&'.port_4.xend')
defstring set_xend5      putnbs ((p4_nb_alias)&'.port_5.xend')
defstring set_xend6      putnbs ((p4_nb_alias)&'.port_6.xend')
defstring set_xend7      putnbs ((p4_nb_alias)&'.port_7.xend')
defstring set_xend8      putnbs ((p4_nb_alias)&'.port_8.xend')
defstring set_yend0      putnbs ((p4_nb_alias)&'.port_0.yend')
defstring set_yend1      putnbs ((p4_nb_alias)&'.port_1.yend')
defstring set_yend2      putnbs ((p4_nb_alias)&'.port_2.yend')
defstring set_yend3      putnbs ((p4_nb_alias)&'.port_3.yend')
defstring set_yend4      putnbs ((p4_nb_alias)&'.port_4.yend')
defstring set_yend5      putnbs ((p4_nb_alias)&'.port_5.yend')
defstring set_yend6      putnbs ((p4_nb_alias)&'.port_6.yend')
defstring set_yend7      putnbs ((p4_nb_alias)&'.port_7.yend')
defstring set_yend8      putnbs ((p4_nb_alias)&'.port_8.yend')
defstring set_istart0    putnbs ((p4_nb_alias)&'.port_0.istart')
defstring set_istart1    putnbs ((p4_nb_alias)&'.port_1.istart')
defstring set_istart2    putnbs ((p4_nb_alias)&'.port_2.istart')
defstring set_istart3    putnbs ((p4_nb_alias)&'.port_3.istart')
defstring set_istart4    putnbs ((p4_nb_alias)&'.port_4.istart')
defstring set_istart5    putnbs ((p4_nb_alias)&'.port_5.istart')
defstring set_istart6    putnbs ((p4_nb_alias)&'.port_6.istart')
defstring set_istart7    putnbs ((p4_nb_alias)&'.port_7.istart')
defstring set_istart8    putnbs ((p4_nb_alias)&'.port_8.istart')
defstring set_jstart0    putnbs ((p4_nb_alias)&'.port_0.jstart')
defstring set_jstart1    putnbs ((p4_nb_alias)&'.port_1.jstart')
defstring set_jstart2    putnbs ((p4_nb_alias)&'.port_2.jstart')
defstring set_jstart3    putnbs ((p4_nb_alias)&'.port_3.jstart')
defstring set_jstart4    putnbs ((p4_nb_alias)&'.port_4.jstart')
defstring set_jstart5    putnbs ((p4_nb_alias)&'.port_5.jstart')
defstring set_jstart6    putnbs ((p4_nb_alias)&'.port_6.jstart')
defstring set_jstart7    putnbs ((p4_nb_alias)&'.port_7.jstart')
defstring set_jstart8    putnbs ((p4_nb_alias)&'.port_8.jstart')
defstring set_iend0      putnbs ((p4_nb_alias)&'.port_0.iend')
defstring set_iend1      putnbs ((p4_nb_alias)&'.port_1.iend')
defstring set_iend2      putnbs ((p4_nb_alias)&'.port_2.iend')
defstring set_iend3      putnbs ((p4_nb_alias)&'.port_3.iend')
defstring set_iend4      putnbs ((p4_nb_alias)&'.port_4.iend')
defstring set_iend5      putnbs ((p4_nb_alias)&'.port_5.iend')
defstring set_iend6      putnbs ((p4_nb_alias)&'.port_6.iend')
defstring set_iend7      putnbs ((p4_nb_alias)&'.port_7.iend')
defstring set_iend8      putnbs ((p4_nb_alias)&'.port_8.iend')
defstring set_jend0      putnbs ((p4_nb_alias)&'.port_0.jend')
defstring set_jend1      putnbs ((p4_nb_alias)&'.port_1.jend')
defstring set_jend2      putnbs ((p4_nb_alias)&'.port_2.jend')
defstring set_jend3      putnbs ((p4_nb_alias)&'.port_3.jend')
defstring set_jend4      putnbs ((p4_nb_alias)&'.port_4.jend')
defstring set_jend5      putnbs ((p4_nb_alias)&'.port_5.jend')
defstring set_jend6      putnbs ((p4_nb_alias)&'.port_6.jend')
defstring set_jend7      putnbs ((p4_nb_alias)&'.port_7.jend')
defstring set_jend8      putnbs ((p4_nb_alias)&'.port_8.jend')
defstring set_overcol0   putnbs ((p4_nb_alias)&'.port_0.overcolour')
defstring set_overcol1   putnbs ((p4_nb_alias)&'.port_1.overcolour')
defstring set_overcol2   putnbs ((p4_nb_alias)&'.port_2.overcolour')
defstring set_overcol3   putnbs ((p4_nb_alias)&'.port_3.overcolour')
defstring set_overcol4   putnbs ((p4_nb_alias)&'.port_4.overcolour')
defstring set_overcol5   putnbs ((p4_nb_alias)&'.port_5.overcolour')
defstring set_overcol6   putnbs ((p4_nb_alias)&'.port_6.overcolour')
defstring set_overcol7   putnbs ((p4_nb_alias)&'.port_7.overcolour')
defstring set_overcol8   putnbs ((p4_nb_alias)&'.port_8.overcolour')
defstring set_fg_colour0 putnbs ((p4_nb_alias)&'.port_0.fg_colour')
defstring set_fg_colour1 putnbs ((p4_nb_alias)&'.port_1.fg_colour')
defstring set_fg_colour2 putnbs ((p4_nb_alias)&'.port_2.fg_colour')
defstring set_fg_colour3 putnbs ((p4_nb_alias)&'.port_3.fg_colour')
defstring set_fg_colour4 putnbs ((p4_nb_alias)&'.port_4.fg_colour')
defstring set_fg_colour5 putnbs ((p4_nb_alias)&'.port_5.fg_colour')
defstring set_fg_colour6 putnbs ((p4_nb_alias)&'.port_6.fg_colour')
defstring set_fg_colour7 putnbs ((p4_nb_alias)&'.port_7.fg_colour')
defstring set_fg_colour8 putnbs ((p4_nb_alias)&'.port_8.fg_colour')
defstring set_bg_colour0 putnbs ((p4_nb_alias)&'.port_0.bg_colour')
defstring set_bg_colour1 putnbs ((p4_nb_alias)&'.port_1.bg_colour')
defstring set_bg_colour2 putnbs ((p4_nb_alias)&'.port_2.bg_colour')
defstring set_bg_colour3 putnbs ((p4_nb_alias)&'.port_3.bg_colour')
defstring set_bg_colour4 putnbs ((p4_nb_alias)&'.port_4.bg_colour')
defstring set_bg_colour5 putnbs ((p4_nb_alias)&'.port_5.bg_colour')
defstring set_bg_colour6 putnbs ((p4_nb_alias)&'.port_6.bg_colour')
defstring set_bg_colour7 putnbs ((p4_nb_alias)&'.port_7.bg_colour')
defstring set_bg_colour8 putnbs ((p4_nb_alias)&'.port_8.bg_colour')
defstring set_contour0   putnbs ((p4_nb_alias)&'.port_0.contour_type')
defstring set_contour1   putnbs ((p4_nb_alias)&'.port_1.contour_type')
defstring set_contour2   putnbs ((p4_nb_alias)&'.port_2.contour_type')
defstring set_contour3   putnbs ((p4_nb_alias)&'.port_3.contour_type')
defstring set_contour4   putnbs ((p4_nb_alias)&'.port_4.contour_type')
defstring set_contour5   putnbs ((p4_nb_alias)&'.port_5.contour_type')
defstring set_contour6   putnbs ((p4_nb_alias)&'.port_6.contour_type')
defstring set_contour7   putnbs ((p4_nb_alias)&'.port_7.contour_type')
defstring set_contour8   putnbs ((p4_nb_alias)&'.port_8.contour_type')
defstring set_cut_dir0   putnbs ((p4_nb_alias)&'.port_0.cut_direction')
defstring set_cut_dir1   putnbs ((p4_nb_alias)&'.port_1.cut_direction')
defstring set_cut_dir2   putnbs ((p4_nb_alias)&'.port_2.cut_direction')
defstring set_cut_dir3   putnbs ((p4_nb_alias)&'.port_3.cut_direction')
defstring set_cut_dir4   putnbs ((p4_nb_alias)&'.port_4.cut_direction')
defstring set_cut_dir5   putnbs ((p4_nb_alias)&'.port_5.cut_direction')
defstring set_cut_dir6   putnbs ((p4_nb_alias)&'.port_6.cut_direction')
defstring set_cut_dir7   putnbs ((p4_nb_alias)&'.port_7.cut_direction')
defstring set_cut_dir8   putnbs ((p4_nb_alias)&'.port_8.cut_direction')
defstring set_cut_start0 putnbs ((p4_nb_alias)&'.port_0.slice_start')
defstring set_cut_start1 putnbs ((p4_nb_alias)&'.port_1.slice_start')
defstring set_cut_start2 putnbs ((p4_nb_alias)&'.port_2.slice_start')
defstring set_cut_start3 putnbs ((p4_nb_alias)&'.port_3.slice_start')
defstring set_cut_start4 putnbs ((p4_nb_alias)&'.port_4.slice_start')
defstring set_cut_start5 putnbs ((p4_nb_alias)&'.port_5.slice_start')
defstring set_cut_start6 putnbs ((p4_nb_alias)&'.port_6.slice_start')
defstring set_cut_start7 putnbs ((p4_nb_alias)&'.port_7.slice_start')
defstring set_cut_start8 putnbs ((p4_nb_alias)&'.port_8.slice_start')
defstring set_cut_end0 putnbs ((p4_nb_alias)&'.port_0.slice_end')
defstring set_cut_end1 putnbs ((p4_nb_alias)&'.port_1.slice_end')
defstring set_cut_end2 putnbs ((p4_nb_alias)&'.port_2.slice_end')
defstring set_cut_end3 putnbs ((p4_nb_alias)&'.port_3.slice_end')
defstring set_cut_end4 putnbs ((p4_nb_alias)&'.port_4.slice_end')
defstring set_cut_end5 putnbs ((p4_nb_alias)&'.port_5.slice_end')
defstring set_cut_end6 putnbs ((p4_nb_alias)&'.port_6.slice_end')
defstring set_cut_end7 putnbs ((p4_nb_alias)&'.port_7.slice_end')
defstring set_cut_end8 putnbs ((p4_nb_alias)&'.port_8.slice_end')

{
{ Defstring the get_ commands
defstring get_device0       getnbs ((p4_nb_alias)&'.port_0.device_name')
defstring get_device1       getnbs ((p4_nb_alias)&'.port_1.device_name')
defstring get_device2       getnbs ((p4_nb_alias)&'.port_2.device_name')
defstring get_device3       getnbs ((p4_nb_alias)&'.port_3.device_name')
defstring get_device4       getnbs ((p4_nb_alias)&'.port_4.device_name')
defstring get_device5       getnbs ((p4_nb_alias)&'.port_5.device_name')
defstring get_device6       getnbs ((p4_nb_alias)&'.port_6.device_name')
defstring get_device7       getnbs ((p4_nb_alias)&'.port_7.device_name')
defstring get_device8       getnbs ((p4_nb_alias)&'.port_8.device_name')
defstring get_lut0          getnbs ((p4_nb_alias)&'.port_0.device_lut')
defstring get_lut1          getnbs ((p4_nb_alias)&'.port_1.device_lut')
defstring get_lut2          getnbs ((p4_nb_alias)&'.port_2.device_lut')
defstring get_lut3          getnbs ((p4_nb_alias)&'.port_3.device_lut')
defstring get_lut4          getnbs ((p4_nb_alias)&'.port_4.device_lut')
defstring get_lut5          getnbs ((p4_nb_alias)&'.port_5.device_lut')
defstring get_lut6          getnbs ((p4_nb_alias)&'.port_6.device_lut')
defstring get_lut7          getnbs ((p4_nb_alias)&'.port_7.device_lut')
defstring get_lut8          getnbs ((p4_nb_alias)&'.port_8.device_lut')
defstring get_title0        getnbs ((p4_nb_alias)&'.port_0.title')
defstring get_title1        getnbs ((p4_nb_alias)&'.port_1.title')
defstring get_title2        getnbs ((p4_nb_alias)&'.port_2.title')
defstring get_title3        getnbs ((p4_nb_alias)&'.port_3.title')
defstring get_title4        getnbs ((p4_nb_alias)&'.port_4.title')
defstring get_title5        getnbs ((p4_nb_alias)&'.port_5.title')
defstring get_title6        getnbs ((p4_nb_alias)&'.port_6.title')
defstring get_title7        getnbs ((p4_nb_alias)&'.port_7.title')
defstring get_title8        getnbs ((p4_nb_alias)&'.port_8.title')
defstring get_type0         getnbs ((p4_nb_alias)&'.port_0.display_type')
defstring get_type1         getnbs ((p4_nb_alias)&'.port_1.display_type')
defstring get_type2         getnbs ((p4_nb_alias)&'.port_2.display_type')
defstring get_type3         getnbs ((p4_nb_alias)&'.port_3.display_type')
defstring get_type4         getnbs ((p4_nb_alias)&'.port_4.display_type')
defstring get_type5         getnbs ((p4_nb_alias)&'.port_5.display_type')
defstring get_type6         getnbs ((p4_nb_alias)&'.port_6.display_type')
defstring get_type7         getnbs ((p4_nb_alias)&'.port_7.display_type')
defstring get_type8         getnbs ((p4_nb_alias)&'.port_8.display_type')
defstring get_plane0        getnbs ((p4_nb_alias)&'.port_0.display_plane')
defstring get_plane1        getnbs ((p4_nb_alias)&'.port_1.display_plane')
defstring get_plane2        getnbs ((p4_nb_alias)&'.port_2.display_plane')
defstring get_plane3        getnbs ((p4_nb_alias)&'.port_3.display_plane')
defstring get_plane4        getnbs ((p4_nb_alias)&'.port_4.display_plane')
defstring get_plane5        getnbs ((p4_nb_alias)&'.port_5.display_plane')
defstring get_plane6        getnbs ((p4_nb_alias)&'.port_6.display_plane')
defstring get_plane7        getnbs ((p4_nb_alias)&'.port_7.display_plane')
defstring get_plane8        getnbs ((p4_nb_alias)&'.port_8.display_plane')
defstring get_autoscale0    getnbs ((p4_nb_alias)&'.port_0.autoscale')
defstring get_autoscale1    getnbs ((p4_nb_alias)&'.port_1.autoscale')
defstring get_autoscale2    getnbs ((p4_nb_alias)&'.port_2.autoscale')
defstring get_autoscale3    getnbs ((p4_nb_alias)&'.port_3.autoscale')
defstring get_autoscale4    getnbs ((p4_nb_alias)&'.port_4.autoscale')
defstring get_autoscale5    getnbs ((p4_nb_alias)&'.port_5.autoscale')
defstring get_autoscale6    getnbs ((p4_nb_alias)&'.port_6.autoscale')
defstring get_autoscale7    getnbs ((p4_nb_alias)&'.port_7.autoscale')
defstring get_autoscale8    getnbs ((p4_nb_alias)&'.port_8.autoscale')
defstring get_erase0        getnbs ((p4_nb_alias)&'.port_0.pre_erase_plot')
defstring get_erase1        getnbs ((p4_nb_alias)&'.port_1.pre_erase_plot')
defstring get_erase2        getnbs ((p4_nb_alias)&'.port_2.pre_erase_plot')
defstring get_erase3        getnbs ((p4_nb_alias)&'.port_3.pre_erase_plot')
defstring get_erase4        getnbs ((p4_nb_alias)&'.port_4.pre_erase_plot')
defstring get_erase5        getnbs ((p4_nb_alias)&'.port_5.pre_erase_plot')
defstring get_erase6        getnbs ((p4_nb_alias)&'.port_6.pre_erase_plot')
defstring get_erase7        getnbs ((p4_nb_alias)&'.port_7.pre_erase_plot')
defstring get_erase8        getnbs ((p4_nb_alias)&'.port_8.pre_erase_plot')
defstring get_axes0         getnbs ((p4_nb_alias)&'.port_0.plot_axes')
defstring get_axes1         getnbs ((p4_nb_alias)&'.port_1.plot_axes')
defstring get_axes2         getnbs ((p4_nb_alias)&'.port_2.plot_axes')
defstring get_axes3         getnbs ((p4_nb_alias)&'.port_3.plot_axes')
defstring get_axes4         getnbs ((p4_nb_alias)&'.port_4.plot_axes')
defstring get_axes5         getnbs ((p4_nb_alias)&'.port_5.plot_axes')
defstring get_axes6         getnbs ((p4_nb_alias)&'.port_6.plot_axes')
defstring get_axes7         getnbs ((p4_nb_alias)&'.port_7.plot_axes')
defstring get_axes8         getnbs ((p4_nb_alias)&'.port_8.plot_axes')
defstring get_high0         getnbs ((p4_nb_alias)&'.port_0.high')
defstring get_high1         getnbs ((p4_nb_alias)&'.port_1.high')
defstring get_high2         getnbs ((p4_nb_alias)&'.port_2.high')
defstring get_high3         getnbs ((p4_nb_alias)&'.port_3.high')
defstring get_high4         getnbs ((p4_nb_alias)&'.port_4.high')
defstring get_high5         getnbs ((p4_nb_alias)&'.port_5.high')
defstring get_high6         getnbs ((p4_nb_alias)&'.port_6.high')
defstring get_high7         getnbs ((p4_nb_alias)&'.port_7.high')
defstring get_high8         getnbs ((p4_nb_alias)&'.port_8.high')
defstring get_low0          getnbs ((p4_nb_alias)&'.port_0.low')
defstring get_low1          getnbs ((p4_nb_alias)&'.port_1.low')
defstring get_low2          getnbs ((p4_nb_alias)&'.port_2.low')
defstring get_low3          getnbs ((p4_nb_alias)&'.port_3.low')
defstring get_low4          getnbs ((p4_nb_alias)&'.port_4.low')
defstring get_low5          getnbs ((p4_nb_alias)&'.port_5.low')
defstring get_low6          getnbs ((p4_nb_alias)&'.port_6.low')
defstring get_low7          getnbs ((p4_nb_alias)&'.port_7.low')
defstring get_low8          getnbs ((p4_nb_alias)&'.port_8.low')
defstring get_whole0        getnbs ((p4_nb_alias)&'.port_0.plot_whole')
defstring get_whole1        getnbs ((p4_nb_alias)&'.port_1.plot_whole')
defstring get_whole2        getnbs ((p4_nb_alias)&'.port_2.plot_whole')
defstring get_whole3        getnbs ((p4_nb_alias)&'.port_3.plot_whole')
defstring get_whole4        getnbs ((p4_nb_alias)&'.port_4.plot_whole')
defstring get_whole5        getnbs ((p4_nb_alias)&'.port_5.plot_whole')
defstring get_whole6        getnbs ((p4_nb_alias)&'.port_6.plot_whole')
defstring get_whole7        getnbs ((p4_nb_alias)&'.port_7.plot_whole')
defstring get_whole8        getnbs ((p4_nb_alias)&'.port_8.plot_whole')
defstring get_errors0       getnbs ((p4_nb_alias)&'.port_0.plot_errors')
defstring get_errors1       getnbs ((p4_nb_alias)&'.port_1.plot_errors')
defstring get_errors2       getnbs ((p4_nb_alias)&'.port_2.plot_errors')
defstring get_errors3       getnbs ((p4_nb_alias)&'.port_3.plot_errors')
defstring get_errors4       getnbs ((p4_nb_alias)&'.port_4.plot_errors')
defstring get_errors5       getnbs ((p4_nb_alias)&'.port_5.plot_errors')
defstring get_errors6       getnbs ((p4_nb_alias)&'.port_6.plot_errors')
defstring get_errors7       getnbs ((p4_nb_alias)&'.port_7.plot_errors')
defstring get_errors8       getnbs ((p4_nb_alias)&'.port_8.plot_errors')
defstring get_bins0         getnbs ((p4_nb_alias)&'.port_0.histogram_bins')
defstring get_bins1         getnbs ((p4_nb_alias)&'.port_1.histogram_bins')
defstring get_bins2         getnbs ((p4_nb_alias)&'.port_2.histogram_bins')
defstring get_bins3         getnbs ((p4_nb_alias)&'.port_3.histogram_bins')
defstring get_bins4         getnbs ((p4_nb_alias)&'.port_4.histogram_bins')
defstring get_bins5         getnbs ((p4_nb_alias)&'.port_5.histogram_bins')
defstring get_bins6         getnbs ((p4_nb_alias)&'.port_6.histogram_bins')
defstring get_bins7         getnbs ((p4_nb_alias)&'.port_7.histogram_bins')
defstring get_bins8         getnbs ((p4_nb_alias)&'.port_8.histogram_bins')
defstring get_xstart0       getnbs ((p4_nb_alias)&'.port_0.xstart')
defstring get_xstart1       getnbs ((p4_nb_alias)&'.port_1.xstart')
defstring get_xstart2       getnbs ((p4_nb_alias)&'.port_2.xstart')
defstring get_xstart3       getnbs ((p4_nb_alias)&'.port_3.xstart')
defstring get_xstart4       getnbs ((p4_nb_alias)&'.port_4.xstart')
defstring get_xstart5       getnbs ((p4_nb_alias)&'.port_5.xstart')
defstring get_xstart6       getnbs ((p4_nb_alias)&'.port_6.xstart')
defstring get_xstart7       getnbs ((p4_nb_alias)&'.port_7.xstart')
defstring get_xstart8       getnbs ((p4_nb_alias)&'.port_8.xstart')
defstring get_ystart0       getnbs ((p4_nb_alias)&'.port_0.ystart')
defstring get_ystart1       getnbs ((p4_nb_alias)&'.port_1.ystart')
defstring get_ystart2       getnbs ((p4_nb_alias)&'.port_2.ystart')
defstring get_ystart3       getnbs ((p4_nb_alias)&'.port_3.ystart')
defstring get_ystart4       getnbs ((p4_nb_alias)&'.port_4.ystart')
defstring get_ystart5       getnbs ((p4_nb_alias)&'.port_5.ystart')
defstring get_ystart6       getnbs ((p4_nb_alias)&'.port_6.ystart')
defstring get_ystart7       getnbs ((p4_nb_alias)&'.port_7.ystart')
defstring get_ystart8       getnbs ((p4_nb_alias)&'.port_8.ystart')
defstring get_xend0         getnbs ((p4_nb_alias)&'.port_0.xend')
defstring get_xend1         getnbs ((p4_nb_alias)&'.port_1.xend')
defstring get_xend2         getnbs ((p4_nb_alias)&'.port_2.xend')
defstring get_xend3         getnbs ((p4_nb_alias)&'.port_3.xend')
defstring get_xend4         getnbs ((p4_nb_alias)&'.port_4.xend')
defstring get_xend5         getnbs ((p4_nb_alias)&'.port_5.xend')
defstring get_xend6         getnbs ((p4_nb_alias)&'.port_6.xend')
defstring get_xend7         getnbs ((p4_nb_alias)&'.port_7.xend')
defstring get_xend8         getnbs ((p4_nb_alias)&'.port_8.xend')
defstring get_yend0         getnbs ((p4_nb_alias)&'.port_0.yend')
defstring get_yend1         getnbs ((p4_nb_alias)&'.port_1.yend')
defstring get_yend2         getnbs ((p4_nb_alias)&'.port_2.yend')
defstring get_yend3         getnbs ((p4_nb_alias)&'.port_3.yend')
defstring get_yend4         getnbs ((p4_nb_alias)&'.port_4.yend')
defstring get_yend5         getnbs ((p4_nb_alias)&'.port_5.yend')
defstring get_yend6         getnbs ((p4_nb_alias)&'.port_6.yend')
defstring get_yend7         getnbs ((p4_nb_alias)&'.port_7.yend')
defstring get_yend8         getnbs ((p4_nb_alias)&'.port_8.yend')
defstring get_istart0       getnbs ((p4_nb_alias)&'.port_0.istart')
defstring get_istart1       getnbs ((p4_nb_alias)&'.port_1.istart')
defstring get_istart2       getnbs ((p4_nb_alias)&'.port_2.istart')
defstring get_istart3       getnbs ((p4_nb_alias)&'.port_3.istart')
defstring get_istart4       getnbs ((p4_nb_alias)&'.port_4.istart')
defstring get_istart5       getnbs ((p4_nb_alias)&'.port_5.istart')
defstring get_istart6       getnbs ((p4_nb_alias)&'.port_6.istart')
defstring get_istart7       getnbs ((p4_nb_alias)&'.port_7.istart')
defstring get_istart8       getnbs ((p4_nb_alias)&'.port_8.istart')
defstring get_jstart0       getnbs ((p4_nb_alias)&'.port_0.jstart')
defstring get_jstart1       getnbs ((p4_nb_alias)&'.port_1.jstart')
defstring get_jstart2       getnbs ((p4_nb_alias)&'.port_2.jstart')
defstring get_jstart3       getnbs ((p4_nb_alias)&'.port_3.jstart')
defstring get_jstart4       getnbs ((p4_nb_alias)&'.port_4.jstart')
defstring get_jstart5       getnbs ((p4_nb_alias)&'.port_5.jstart')
defstring get_jstart6       getnbs ((p4_nb_alias)&'.port_6.jstart')
defstring get_jstart7       getnbs ((p4_nb_alias)&'.port_7.jstart')
defstring get_jstart8       getnbs ((p4_nb_alias)&'.port_8.jstart')
defstring get_iend0         getnbs ((p4_nb_alias)&'.port_0.iend')
defstring get_iend1         getnbs ((p4_nb_alias)&'.port_1.iend')
defstring get_iend2         getnbs ((p4_nb_alias)&'.port_2.iend')
defstring get_iend3         getnbs ((p4_nb_alias)&'.port_3.iend')
defstring get_iend4         getnbs ((p4_nb_alias)&'.port_4.iend')
defstring get_iend5         getnbs ((p4_nb_alias)&'.port_5.iend')
defstring get_iend6         getnbs ((p4_nb_alias)&'.port_6.iend')
defstring get_iend7         getnbs ((p4_nb_alias)&'.port_7.iend')
defstring get_iend8         getnbs ((p4_nb_alias)&'.port_8.iend')
defstring get_jend0         getnbs ((p4_nb_alias)&'.port_0.jend')
defstring get_jend1         getnbs ((p4_nb_alias)&'.port_1.jend')
defstring get_jend2         getnbs ((p4_nb_alias)&'.port_2.jend')
defstring get_jend3         getnbs ((p4_nb_alias)&'.port_3.jend')
defstring get_jend4         getnbs ((p4_nb_alias)&'.port_4.jend')
defstring get_jend5         getnbs ((p4_nb_alias)&'.port_5.jend')
defstring get_jend6         getnbs ((p4_nb_alias)&'.port_6.jend')
defstring get_jend7         getnbs ((p4_nb_alias)&'.port_7.jend')
defstring get_jend8         getnbs ((p4_nb_alias)&'.port_8.jend')
defstring get_overcol0      getnbs ((p4_nb_alias)&'.port_0.overcolour')
defstring get_overcol1      getnbs ((p4_nb_alias)&'.port_1.overcolour')
defstring get_overcol2      getnbs ((p4_nb_alias)&'.port_2.overcolour')
defstring get_overcol3      getnbs ((p4_nb_alias)&'.port_3.overcolour')
defstring get_overcol4      getnbs ((p4_nb_alias)&'.port_4.overcolour')
defstring get_overcol5      getnbs ((p4_nb_alias)&'.port_5.overcolour')
defstring get_overcol6      getnbs ((p4_nb_alias)&'.port_6.overcolour')
defstring get_overcol7      getnbs ((p4_nb_alias)&'.port_7.overcolour')
defstring get_overcol8      getnbs ((p4_nb_alias)&'.port_8.overcolour')
defstring get_fg_colour0    getnbs ((p4_nb_alias)&'.port_0.fg_colour')
defstring get_fg_colour1    getnbs ((p4_nb_alias)&'.port_1.fg_colour')
defstring get_fg_colour2    getnbs ((p4_nb_alias)&'.port_2.fg_colour')
defstring get_fg_colour3    getnbs ((p4_nb_alias)&'.port_3.fg_colour')
defstring get_fg_colour4    getnbs ((p4_nb_alias)&'.port_4.fg_colour')
defstring get_fg_colour5    getnbs ((p4_nb_alias)&'.port_5.fg_colour')
defstring get_fg_colour6    getnbs ((p4_nb_alias)&'.port_6.fg_colour')
defstring get_fg_colour7    getnbs ((p4_nb_alias)&'.port_7.fg_colour')
defstring get_fg_colour8    getnbs ((p4_nb_alias)&'.port_8.fg_colour')
defstring get_bg_colour0    getnbs ((p4_nb_alias)&'.port_0.bg_colour')
defstring get_bg_colour1    getnbs ((p4_nb_alias)&'.port_1.bg_colour')
defstring get_bg_colour2    getnbs ((p4_nb_alias)&'.port_2.bg_colour')
defstring get_bg_colour3    getnbs ((p4_nb_alias)&'.port_3.bg_colour')
defstring get_bg_colour4    getnbs ((p4_nb_alias)&'.port_4.bg_colour')
defstring get_bg_colour5    getnbs ((p4_nb_alias)&'.port_5.bg_colour')
defstring get_bg_colour6    getnbs ((p4_nb_alias)&'.port_6.bg_colour')
defstring get_bg_colour7    getnbs ((p4_nb_alias)&'.port_7.bg_colour')
defstring get_bg_colour8    getnbs ((p4_nb_alias)&'.port_8.bg_colour')
defstring get_contour0      getnbs ((p4_nb_alias)&'.port_0.contour_type')
defstring get_contour1      getnbs ((p4_nb_alias)&'.port_1.contour_type')
defstring get_contour2      getnbs ((p4_nb_alias)&'.port_2.contour_type')
defstring get_contour3      getnbs ((p4_nb_alias)&'.port_3.contour_type')
defstring get_contour4      getnbs ((p4_nb_alias)&'.port_4.contour_type')
defstring get_contour5      getnbs ((p4_nb_alias)&'.port_5.contour_type')
defstring get_contour6      getnbs ((p4_nb_alias)&'.port_6.contour_type')
defstring get_contour7      getnbs ((p4_nb_alias)&'.port_7.contour_type')
defstring get_contour8      getnbs ((p4_nb_alias)&'.port_8.contour_type')
defstring get_cut_dir0      getnbs ((p4_nb_alias)&'.port_0.cut_direction')
defstring get_cut_dir1      getnbs ((p4_nb_alias)&'.port_1.cut_direction')
defstring get_cut_dir2      getnbs ((p4_nb_alias)&'.port_2.cut_direction')
defstring get_cut_dir3      getnbs ((p4_nb_alias)&'.port_3.cut_direction')
defstring get_cut_dir4      getnbs ((p4_nb_alias)&'.port_4.cut_direction')
defstring get_cut_dir5      getnbs ((p4_nb_alias)&'.port_5.cut_direction')
defstring get_cut_dir6      getnbs ((p4_nb_alias)&'.port_6.cut_direction')
defstring get_cut_dir7      getnbs ((p4_nb_alias)&'.port_7.cut_direction')
defstring get_cut_dir8      getnbs ((p4_nb_alias)&'.port_8.cut_direction')
defstring get_cut_start0    getnbs ((p4_nb_alias)&'.port_0.slice_start')
defstring get_cut_start1    getnbs ((p4_nb_alias)&'.port_1.slice_start')
defstring get_cut_start2    getnbs ((p4_nb_alias)&'.port_2.slice_start')
defstring get_cut_start3    getnbs ((p4_nb_alias)&'.port_3.slice_start')
defstring get_cut_start4    getnbs ((p4_nb_alias)&'.port_4.slice_start')
defstring get_cut_start5    getnbs ((p4_nb_alias)&'.port_5.slice_start')
defstring get_cut_start6    getnbs ((p4_nb_alias)&'.port_6.slice_start')
defstring get_cut_start7    getnbs ((p4_nb_alias)&'.port_7.slice_start')
defstring get_cut_start8    getnbs ((p4_nb_alias)&'.port_8.slice_start')
defstring get_cut_end0      getnbs ((p4_nb_alias)&'.port_0.slice_end')
defstring get_cut_end1      getnbs ((p4_nb_alias)&'.port_1.slice_end')
defstring get_cut_end2      getnbs ((p4_nb_alias)&'.port_2.slice_end')
defstring get_cut_end3      getnbs ((p4_nb_alias)&'.port_3.slice_end')
defstring get_cut_end4      getnbs ((p4_nb_alias)&'.port_4.slice_end')
defstring get_cut_end5      getnbs ((p4_nb_alias)&'.port_5.slice_end')
defstring get_cut_end6      getnbs ((p4_nb_alias)&'.port_6.slice_end')
defstring get_cut_end7      getnbs ((p4_nb_alias)&'.port_7.slice_end')
defstring get_cut_end8      getnbs ((p4_nb_alias)&'.port_8.slice_end')

{
{ PROC EXIT
proc exit
  inputl 'EXIT - Are you sure? (Y/N)  > ' (sure)
  if sure
    p4_kill
    #exit
  endif
endproc

{
{ PROC P4_LOAD
proc p4_load
  p4_loadw
  p4_set_nb
  p4_open_nb
  p4_restored
  p4_lut port=0
  set_title0 ' '
  set_axes0 FALSE
  p4_display data=$P4_CT/cgs4
  set_axes0 TRUE
  p4_status
endproc

{
{ PROC P4_KILL
proc p4_kill
  p4_rmfile
  p4_saved
  p4_close_nb
  p4_killw
endproc

{
{ PROC SET_DEVICE
proc set_device p1
  if UNDEFINED(p1)
    input 'Plot device?  > ' (device)
  else
    device = p1
  endif
  set_device0 (device)
  set_device1 (device)
  set_device2 (device)
  set_device3 (device)
  set_device4 (device)
  set_device5 (device)
  set_device6 (device)
  set_device7 (device)
  set_device8 (device)
endproc

{
{ PROC SET_LUT
proc set_lut p1
  if UNDEFINED(p1)
    input 'Colour (look-up) table?  > ' (lut)
  else
    lut = p1
  endif
  set_lut0 (lut)
  set_lut1 (lut)
  set_lut2 (lut)
  set_lut3 (lut)
  set_lut4 (lut)
  set_lut5 (lut)
  set_lut6 (lut)
  set_lut7 (lut)
  set_lut8 (lut)
endproc

{
{ PROC SET_TITLE
proc set_title p1
  if UNDEFINED(p1)
    input 'Plot title?  > ' (title)
  else
    title = p1
  endif
  set_title0 (title)
  set_title1 (title)
  set_title2 (title)
  set_title3 (title)
  set_title4 (title)
  set_title5 (title)
  set_title6 (title)
  set_title7 (title)
  set_title8 (title)
endproc

{
{ PROC SET_TYPE
proc set_type p1
  if UNDEFINED(p1)
    input 'Plot type?  > ' (type)
  else
    type = p1
  endif
  type = UPCASE(type)
  set_type0 (type)
  set_type1 (type)
  set_type2 (type)
  set_type3 (type)
  set_type4 (type)
  set_type5 (type)
  set_type6 (type)
  set_type7 (type)
  set_type8 (type)
endproc

{
{ PROC SET_PLANE
proc set_plane p1
  if UNDEFINED(p1)
    input 'Plot plane?  > ' (plane)
  else
    plane = p1
  endif
  plane = UPCASE(plane)
  set_plane0 (plane)
  set_plane1 (plane)
  set_plane2 (plane)
  set_plane3 (plane)
  set_plane4 (plane)
  set_plane5 (plane)
  set_plane6 (plane)
  set_plane7 (plane)
  set_plane8 (plane)
endproc

{
{ PROC SET_AUTOSCALE
proc set_autoscale p1
  if UNDEFINED(p1)
    inputl 'Enable autoscaling?  > ' (autoscale)
  else
    autoscale = LOGICAL(p1)
  endif
  set_autoscale0 (autoscale)
  set_autoscale1 (autoscale)
  set_autoscale2 (autoscale)
  set_autoscale3 (autoscale)
  set_autoscale4 (autoscale)
  set_autoscale5 (autoscale)
  set_autoscale6 (autoscale)
  set_autoscale7 (autoscale)
  set_autoscale8 (autoscale)
endproc

{
{ PROC SET_ERASE
proc set_erase p1
  if UNDEFINED(p1)
    inputl 'Pre-erase plot?  > ' (erase)
  else
    erase = LOGICAL(p1)
  endif
  set_erase0 (erase)
  set_erase1 (erase)
  set_erase2 (erase)
  set_erase3 (erase)
  set_erase4 (erase)
  set_erase5 (erase)
  set_erase6 (erase)
  set_erase7 (erase)
  set_erase8 (erase)
endproc

{
{ PROC SET_HIGH
proc set_high p1
  if UNDEFINED(p1)
    inputr 'High data value for scaling?  > ' (high)
  else
    high = REAL(p1)
  endif
  set_high0 (high)
  set_high1 (high)
  set_high2 (high)
  set_high3 (high)
  set_high4 (high)
  set_high5 (high)
  set_high6 (high)
  set_high7 (high)
  set_high8 (high)
endproc

{
{ PROC SET_LOW
proc set_low p1
  if UNDEFINED(p1)
    inputr 'Low data value for scaling?  > ' (low)
  else
    low = REAL(p1)
  endif
  set_low0 (low)
  set_low1 (low)
  set_low2 (low)
  set_low3 (low)
  set_low4 (low)
  set_low5 (low)
  set_low6 (low)
  set_low7 (low)
  set_low8 (low)
endproc

{
{ PROC SET_WHOLE
proc set_whole p1
  if UNDEFINED(p1)
    inputl 'Plot whole array?  > ' (whole)
  else
    whole = LOGICAL(p1)
  endif
  set_whole0 (whole)
  set_whole1 (whole)
  set_whole2 (whole)
  set_whole3 (whole)
  set_whole4 (whole)
  set_whole5 (whole)
  set_whole6 (whole)
  set_whole7 (whole)
  set_whole8 (whole)
endproc

{
{ PROC SET_XSTART
proc set_xstart p1
  if UNDEFINED(p1)
    inputr 'X-start for sub-array plotting?  > ' (xstart)
  else
    xstart = REAL(p1)
  endif
  set_xstart0 (xstart)
  set_xstart1 (xstart)
  set_xstart2 (xstart)
  set_xstart3 (xstart)
  set_xstart4 (xstart)
  set_xstart5 (xstart)
  set_xstart6 (xstart)
  set_xstart7 (xstart)
  set_xstart8 (xstart)
endproc

{
{ PROC SET_YSTART
proc set_ystart p1
  if UNDEFINED(p1)
    inputr 'Ystart for sub-array plotting?  > ' (ystart)
  else
    ystart = REAL(p1)
  endif
  set_ystart0 (ystart)
  set_ystart1 (ystart)
  set_ystart2 (ystart)
  set_ystart3 (ystart)
  set_ystart4 (ystart)
  set_ystart5 (ystart)
  set_ystart6 (ystart)
  set_ystart7 (ystart)
  set_ystart8 (ystart)
endproc

{
{ PROC SET_XEND
proc set_xend p1
  if UNDEFINED(p1)
    inputr 'X-end for sub-array plotting?  > ' (xend)
  else
    xend = REAL(p1)
  endif
  set_xend0 (xend)
  set_xend1 (xend)
  set_xend2 (xend)
  set_xend3 (xend)
  set_xend4 (xend)
  set_xend5 (xend)
  set_xend6 (xend)
  set_xend7 (xend)
  set_xend8 (xend)
endproc

{
{ PROC SET_YEND
proc set_yend p1
  if UNDEFINED(p1)
    inputr 'Yend for sub-array plotting?  > ' (yend)
  else
    yend = REAL(p1)
  endif
  set_yend0 (yend)
  set_yend1 (yend)
  set_yend2 (yend)
  set_yend3 (yend)
  set_yend4 (yend)
  set_yend5 (yend)
  set_yend6 (yend)
  set_yend7 (yend)
  set_yend8 (yend)
endproc

{
{ PROC SET_ISTART
proc set_istart p1
  if UNDEFINED(p1)
    inputi 'I-start for sub-array plotting?  > ' (istart)
  else
    istart = INTEGER(p1)
  endif
  set_istart0 (istart)
  set_istart1 (istart)
  set_istart2 (istart)
  set_istart3 (istart)
  set_istart4 (istart)
  set_istart5 (istart)
  set_istart6 (istart)
  set_istart7 (istart)
  set_istart8 (istart)
endproc

{
{ PROC SET_JSTART
proc set_jstart p1
  if UNDEFINED(p1)
    inputi 'J-start for sub-array plotting?  > ' (jstart)
  else
    jstart = INTEGER(p1)
  endif
  set_jstart0 (jstart)
  set_jstart1 (jstart)
  set_jstart2 (jstart)
  set_jstart3 (jstart)
  set_jstart4 (jstart)
  set_jstart5 (jstart)
  set_jstart6 (jstart)
  set_jstart7 (jstart)
  set_jstart8 (jstart)
endproc

{
{ PROC SET_IEND
proc set_iend p1
  if UNDEFINED(p1)
    inputi 'I-end for sub-array plotting?  > ' (iend)
  else
    iend = INTEGER(p1)
  endif
  set_iend0 (iend)
  set_iend1 (iend)
  set_iend2 (iend)
  set_iend3 (iend)
  set_iend4 (iend)
  set_iend5 (iend)
  set_iend6 (iend)
  set_iend7 (iend)
  set_iend8 (iend)
endproc

{
{ PROC SET_JEND
proc set_jend p1
  if UNDEFINED(p1)
    inputi 'J-end for sub-array plotting?  > ' (jend)
  else
    jend = INTEGER(p1)
  endif
  set_jend0 (jend)
  set_jend1 (jend)
  set_jend2 (jend)
  set_jend3 (jend)
  set_jend4 (jend)
  set_jend5 (jend)
  set_jend6 (jend)
  set_jend7 (jend)
  set_jend8 (jend)
endproc

{
{ PROC SET_ERRORS
proc set_errors p1
  if UNDEFINED(p1)
    inputl 'Plot error bars?  > ' (errors)
  else
    errors = LOGICAL(p1)
  endif
  set_errors0 (errors)
  set_errors1 (errors)
  set_errors2 (errors)
  set_errors3 (errors)
  set_errors4 (errors)
  set_errors5 (errors)
  set_errors6 (errors)
  set_errors7 (errors)
  set_errors8 (errors)
endproc

{
{ PROC SET_BINS
proc set_bins p1
  if UNDEFINED(p1)
    inputi 'Number of histogram bins?  > ' (bins)
  else
    bins = INTEGER(p1)
  endif
  set_bins0 (bins)
  set_bins1 (bins)
  set_bins2 (bins)
  set_bins3 (bins)
  set_bins4 (bins)
  set_bins5 (bins)
  set_bins6 (bins)
  set_bins7 (bins)
  set_bins8 (bins)
endproc

{
{ PROC SET_OVERCOL
proc set_overcol p1
  if UNDEFINED(p1)
    input 'Plot graph overcol?  > ' (overcol)
  else
    overcol = p1
  endif
  overcol = UPCASE(overcol)
  set_overcol0 (overcol)
  set_overcol1 (overcol)
  set_overcol2 (overcol)
  set_overcol3 (overcol)
  set_overcol4 (overcol)
  set_overcol5 (overcol)
  set_overcol6 (overcol)
  set_overcol7 (overcol)
  set_overcol8 (overcol)
endproc

{
{ PROC SET_FG_COLOUR
proc set_fg_colour p1
  if UNDEFINED(p1)
    input 'Plot foreground colour?  > ' (fg_colour)
  else
    fg_colour = p1
  endif
  fg_colour = UPCASE(fg_colour)
  set_fg_colour0 (fg_colour)
  set_fg_colour1 (fg_colour)
  set_fg_colour2 (fg_colour)
  set_fg_colour3 (fg_colour)
  set_fg_colour4 (fg_colour)
  set_fg_colour5 (fg_colour)
  set_fg_colour6 (fg_colour)
  set_fg_colour7 (fg_colour)
  set_fg_colour8 (fg_colour)
endproc

{
{ PROC SET_BG_COLOUR
proc set_bg_colour p1
  if UNDEFINED(p1)
    input 'Plot background colour?  > ' (bg_colour)
  else
    bg_colour = p1
  endif
  bg_colour = UPCASE(bg_colour)
  set_bg_colour0 (bg_colour)
  set_bg_colour1 (bg_colour)
  set_bg_colour2 (bg_colour)
  set_bg_colour3 (bg_colour)
  set_bg_colour4 (bg_colour)
  set_bg_colour5 (bg_colour)
  set_bg_colour6 (bg_colour)
  set_bg_colour7 (bg_colour)
  set_bg_colour8 (bg_colour)
endproc

{
{ PROC SET_CONTOUR
proc set_contour p1
  if UNDEFINED(p1)
    input 'Contour type?  > ' (contour)
  else
    contour = p1
  endif
  contour = UPCASE(contour)
  set_contour0 (contour)
  set_contour1 (contour)
  set_contour2 (contour)
  set_contour3 (contour)
  set_contour4 (contour)
  set_contour5 (contour)
  set_contour6 (contour)
  set_contour7 (contour)
  set_contour8 (contour)
endproc

{
{ PROC SET_CUT_DIR
proc set_cut_dir p1
  if UNDEFINED(p1)
    input 'Direction of cut?  > ' (cut_dir)
  else
    cut_dir = p1
  endif
  set_cut_dir0 (cut_dir)
  set_cut_dir1 (cut_dir)
  set_cut_dir2 (cut_dir)
  set_cut_dir3 (cut_dir)
  set_cut_dir4 (cut_dir)
  set_cut_dir5 (cut_dir)
  set_cut_dir6 (cut_dir)
  set_cut_dir7 (cut_dir)
  set_cut_dir8 (cut_dir)
endproc
{
{ PROC SET_CUT_START
proc set_cut_start p1
  if UNDEFINED(p1)
    input 'Start row/column for cut summation?  > ' (cut_start)
  else
    cut_start = REAL(p1)
  endif
  set_cut_start0 (cut_start)
  set_cut_start1 (cut_start)
  set_cut_start2 (cut_start)
  set_cut_start3 (cut_start)
  set_cut_start4 (cut_start)
  set_cut_start5 (cut_start)
  set_cut_start6 (cut_start)
  set_cut_start7 (cut_start)
  set_cut_start8 (cut_start)
endproc

{
{ PROC SET_CUT_END
proc set_cut_end p1
  if UNDEFINED(p1)
    input 'End row/column for cut summation?  > ' (cut_end)
  else
    cut_end = REAL(p1)
  endif
  set_cut_end0 (cut_end)
  set_cut_end1 (cut_end)
  set_cut_end2 (cut_end)
  set_cut_end3 (cut_end)
  set_cut_end4 (cut_end)
  set_cut_end5 (cut_end)
  set_cut_end6 (cut_end)
  set_cut_end7 (cut_end)
  set_cut_end8 (cut_end)
endproc

{
{ PROC SET_DISPLAY
proc set_display p0
  if UNDEFINED(p0)
    input 'Port number?  > ' (port)
  else
    port = INTEGER(p0)
  endif
  defval = ' '
  actval = ' '
  setval = ' '
  if port = 0
    get_type0 (defval)
    msj = 'Display type? /'&(defval)&'/  > '
    input (msj) (actval)
    if actval = ''
      actval = defval
    endif
    setval = UPCASE( actval )
    set_type0 (setval)
    if setval = 'GRAPH'
      get_cut_dir0 (defval)
      msj = 'Direction of cut? /'&(defval)&'/  > '
      input (msj) (actval)
      if actval = ''
        actval = defval
      endif
      set_cut_dir0 (actval)
      get_cut_start0 (defval)
      msj = 'Start of cut? /'&(defval)&'/  > '
      input (msj) (actval)
      if actval = ''
        actval = defval
      endif
      setval = REAL( actval )
      set_cut_start0 (setval)
      get_cut_end0 (defval)
      msj = 'End of cut? /'&(defval)&'/  > '
      input (msj) (actval)
      if actval = ''
        actval = defval
      endif
      setval = REAL( actval )
      set_cut_end0 (setval)
      get_errors0 (defval)
      msj = 'Plot error bars? /'&(defval)&'/  > '
      input (msj) (actval)
      if actval = ''
        actval = defval
      endif
      is_logical (actval)
      set_errors0 (actval)
    else if setval = 'OVERGRAPH'
      get_cut_dir0 (defval)
      msj = 'Direction of cut? /'&(defval)&'/  > '
      input (msj) (actval)
      if actval = ''
        actval = defval
      endif
      set_cut_dir0 (actval)
      get_cut_start0 (defval)
      msj = 'Start of cut? /'&(defval)&'/  > '
      input (msj) (actval)
      if actval = ''
        actval = defval
      endif
      setval = REAL( actval )
      set_cut_start0 (setval)
      get_cut_end0 (defval)
      msj = 'End of cut? /'&(defval)&'/  > '
      input (msj) (actval)
      if actval = ''
        actval = defval
      endif
      setval = REAL( actval )
      set_cut_end0 (setval)
      get_errors0 (defval)
      msj = 'Plot error bars? /'&(defval)&'/  > '
      input (msj) (actval)
      if actval = ''
        actval = defval
      endif
      is_logical (actval)
      set_errors0 (actval)
      get_overcol0 (defval)
      mssj = 'Overgraph colour? /'&(defval)&'/  > '
      input (msj) (actval)
      if actval = ''
        actval = defval
      endif
      set_overcol0 (actval)
    else if setval = 'HISTOGRAM'
      get_bins0 (defval)
      msj = 'Number of bins? /'&(defval)&'/  > '
      input (msj) (actval)
      if actval = ''
        actval = defval
      endif
      setval = INTEGER( actval )
      set_bins0 (setval)
    esle if setval = 'CONTOUR'
      get_contour0 (defval)
      msj = 'Contour type? /'&(defval)&'/ > '
      input (msj) (actval)
      if actval = ''
        actval = defval
      endif
      set_contour0 (actval)
    endif
    get_plane0 (defval)
    msj = 'Data plane? /'&(defval)&'/  > '
    input (msj) (actval)
    if actval = ''
      actval = defval
    endif
    set_plane0 (actval)
    get_autoscale0 (defval)
    msj = 'Autoscale? /'&(defval)&'/  > '
    input (msj) (actval)
    if actval = ''
      actval = defval
    endif
    is_logical (actval)
    set_autoscale0 (actval)
    if not actval
      get_low0 (defval)
      msj = 'Low value? /'&(defval)&'/  > '
      input (msj) (actval)
      if actval = ''
        actval = defval
      endif
      setval = REAL( actval )
      set_low0 (setval)
      get_high0 (defval)
      msj = 'High value? /'&(defval)&'/  > '
      input (msj) (actval)
      if actval = ''
        actval = defval
      endif
      setval = REAL( actval )
      set_high0 (setval)
    endif
    get_whole0 (defval)
    msj = 'Plot whole array? /'&(defval)&'/  > '
    input (msj) (actval)
    if actval = ''
      actval = defval
    endif
    is_logical (actval)
    set_whole0 (actval)
    if not actval
      get_xstart0 (defval)
      msj = 'X start? /'&(defval)&'/  > '
      input (msj) (actval)
      if actval = ''
        actval = defval
      endif
      setval = REAL( actval )
      set_xstart0 (setval)
      get_xend0 (defval)
      msj = 'X end? /'&(defval)&'/  > '
      input (msj) (actval)
      if actval = ''
        actval = defval
      endif
      setval = REAL( actval )
      set_xend0 (setval)
      get_ystart0 (defval)
      msj = 'Y start? /'&(defval)&'/  > '
      input (msj) (actval)
      if actval = ''
        actval = defval
      endif
      setval = REAL( actval )
      set_ystart0 (setval)
      get_yend0 (defval)
      msj = 'Y end? /'&(defval)&'/  > '
      input (msj) (actval)
      if actval = ''
        actval = defval
      endif
      setval = REAL( actval )
      set_yend0 (setval)
    endif
  else if port = 1
    get_type1 (defval)
    msj = 'Display type? /'&(defval)&'/  > '
    input (msj) (actval)
    if actval = ''
      actval = defval
    endif
    setval = UPCASE( actval )
    set_type1 (setval)
    if setval = 'GRAPH'
      get_cut_dir1 (defval)
      msj = 'Direction of cut? /'&(defval)&'/  > '
      input (msj) (actval)
      if actval = ''
        actval = defval
      endif
      set_cut_dir1 (actval)
      get_cut_start1 (defval)
      msj = 'Start of cut? /'&(defval)&'/  > '
      input (msj) (actval)
      if actval = ''
        actval = defval
      endif
      setval = REAL( actval )
      set_cut_start1 (setval)
      get_cut_end1 (defval)
      msj = 'End of cut? /'&(defval)&'/  > '
      input (msj) (actval)
      if actval = ''
        actval = defval
      endif
      setval = REAL( actval )
      set_cut_end1 (setval)
      get_errors1 (defval)
      msj = 'Plot error bars? /'&(defval)&'/  > '
      input (msj) (actval)
      if actval = ''
        actval = defval
      endif
      is_logical (actval)
      set_errors1 (actval)
    else if setval = 'OVERGRAPH'
      get_cut_dir1 (defval)
      msj = 'Direction of cut? /'&(defval)&'/  > '
      input (msj) (actval)
      if actval = ''
        actval = defval
      endif
      set_cut_dir1 (actval)
      get_cut_start1 (defval)
      msj = 'Start of cut? /'&(defval)&'/  > '
      input (msj) (actval)
      if actval = ''
        actval = defval
      endif
      setval = REAL( actval )
      set_cut_start1 (setval)
      get_cut_end1 (defval)
      msj = 'End of cut? /'&(defval)&'/  > '
      input (msj) (actval)
      if actval = ''
        actval = defval
      endif
      setval = REAL( actval )
      set_cut_end1 (setval)
      get_errors1 (defval)
      msj = 'Plot error bars? /'&(defval)&'/  > '
      input (msj) (actval)
      if actval = ''
        actval = defval
      endif
      is_logical (actval)
      set_errors1 (actval)
      get_overcol1 (defval)
      mssj = 'Overgraph colour? /'&(defval)&'/  > '
      input (msj) (actval)
      if actval = ''
        actval = defval
      endif
      set_overcol1 (actval)
    else if setval = 'HISTOGRAM'
      get_bins1 (defval)
      msj = 'Number of bins? /'&(defval)&'/  > '
      input (msj) (actval)
      if actval = ''
        actval = defval
      endif
      setval = INTEGER( actval )
      set_bins1 (setval)
    else if setval = 'CONTOUR'
      get_contour1 (defval)
      msj = 'Contour type? /'&(defval)&'/ > '
      input (msj) (actval)
      if actval = ''
        actval = defval
      endif
      set_contour1 (actval)
    endif
    get_plane1 (defval)
    msj = 'Data plane? /'&(defval)&'/  > '
    input (msj) (actval)
    if actval = ''
      actval = defval
    endif
    set_plane1 (actval)
    get_autoscale1 (defval)
    msj = 'Autoscale? /'&(defval)&'/  > '
    input (msj) (actval)
    if actval = ''
      actval = defval
    endif
    is_logical (actval)
    set_autoscale1 (actval)
    if not actval
      get_low1 (defval)
      msj = 'Low value? /'&(defval)&'/  > '
      input (msj) (actval)
      if actval = ''
        actval = defval
      endif
      setval = REAL( actval )
      set_low1 (setval)
      get_high1 (defval)
      msj = 'High value? /'&(defval)&'/  > '
      input (msj) (actval)
      if actval = ''
        actval = defval
      endif
      setval = REAL( actval )
      set_high1 (setval)
    endif
    get_whole1 (defval)
    msj = 'Plot whole array? /'&(defval)&'/  > '
    input (msj) (actval)
    if actval = ''
      actval = defval
    endif
    is_logical (actval)
    set_whole1 (actval)
    if not actval
      get_xstart1 (defval)
      msj = 'X start? /'&(defval)&'/  > '
      input (msj) (actval)
      if actval = ''
        actval = defval
      endif
      setval = REAL( actval )
      set_xstart1 (setval)
      get_xend1 (defval)
      msj = 'X end? /'&(defval)&'/  > '
      input (msj) (actval)
      if actval = ''
        actval = defval
      endif
      setval = REAL( actval )
      set_xend1 (setval)
      get_ystart1 (defval)
      msj = 'Y start? /'&(defval)&'/  > '
      input (msj) (actval)
      if actval = ''
        actval = defval
      endif
      setval = REAL( actval )
      set_ystart1 (setval)
      get_yend1 (defval)
      msj = 'Y end? /'&(defval)&'/  > '
      input (msj) (actval)
      if actval = ''
        actval = defval
      endif
      setval = REAL( actval )
      set_yend1 (setval)
    endif
  else if port = 2
    get_type2 (defval)
    msj = 'Display type? /'&(defval)&'/  > '
    input (msj) (actval)
    if actval = ''
      actval = defval
    endif
    setval = UPCASE( actval )
    set_type2 (setval)
    if setval = 'GRAPH'
      get_cut_dir2 (defval)
      msj = 'Direction of cut? /'&(defval)&'/  > '
      input (msj) (actval)
      if actval = ''
        actval = defval
      endif
      set_cut_dir2 (actval)
      get_cut_start2 (defval)
      msj = 'Start of cut? /'&(defval)&'/  > '
      input (msj) (actval)
      if actval = ''
        actval = defval
      endif
      setval = REAL( actval )
      set_cut_start2 (setval)
      get_cut_end2 (defval)
      msj = 'End of cut? /'&(defval)&'/  > '
      input (msj) (actval)
      if actval = ''
        actval = defval
      endif
      setval = REAL( actval )
      set_cut_end2 (setval)
      get_errors2 (defval)
      msj = 'Plot error bars? /'&(defval)&'/  > '
      input (msj) (actval)
      if actval = ''
        actval = defval
      endif
      is_logical (actval)
      set_errors2 (actval)
    else if setval = 'OVERGRAPH'
      get_cut_dir2 (defval)
      msj = 'Direction of cut? /'&(defval)&'/  > '
      input (msj) (actval)
      if actval = ''
        actval = defval
      endif
      set_cut_dir2 (actval)
      get_cut_start2 (defval)
      msj = 'Start of cut? /'&(defval)&'/  > '
      input (msj) (actval)
      if actval = ''
        actval = defval
      endif
      setval = REAL( actval )
      set_cut_start2 (setval)
      get_cut_end2 (defval)
      msj = 'End of cut? /'&(defval)&'/  > '
      input (msj) (actval)
      if actval = ''
        actval = defval
      endif
      setval = REAL( actval )
      set_cut_end2 (setval)
      get_errors2 (defval)
      msj = 'Plot error bars? /'&(defval)&'/  > '
      input (msj) (actval)
      if actval = ''
        actval = defval
      endif
      is_logical (actval)
      set_errors2 (actval)
      get_overcol2 (defval)
      mssj = 'Overgraph colour? /'&(defval)&'/  > '
      input (msj) (actval)
      if actval = ''
        actval = defval
      endif
      set_overcol2 (actval)
    else if setval = 'HISTOGRAM'
      get_bins2 (defval)
      msj = 'Number of bins? /'&(defval)&'/  > '
      input (msj) (actval)
      if actval = ''
        actval = defval
      endif
      setval = INTEGER( actval )
      set_bins2 (setval)
    else if setval = 'CONTOUR'
      get_contour2 (defval)
      msj = 'Contour type? /'&(defval)&'/ > '
      input (msj) (actval)
      if actval = ''
        actval = defval
      endif
      set_contour2 (actval)
    endif
    get_plane2 (defval)
    msj = 'Data plane? /'&(defval)&'/  > '
    input (msj) (actval)
    if actval = ''
      actval = defval
    endif
    set_plane2 (actval)
    get_autoscale2 (defval)
    msj = 'Autoscale? /'&(defval)&'/  > '
    input (msj) (actval)
    if actval = ''
      actval = defval
    endif
    is_logical (actval)
    set_autoscale2 (actval)
    if not actval
      get_low2 (defval)
      msj = 'Low value? /'&(defval)&'/  > '
      input (msj) (actval)
      if actval = ''
        actval = defval
      endif
      setval = REAL( actval )
      set_low2 (setval)
      get_high2 (defval)
      msj = 'High value? /'&(defval)&'/  > '
      input (msj) (actval)
      if actval = ''
        actval = defval
      endif
      setval = REAL( actval )
      set_high2 (setval)
    endif
    get_whole2 (defval)
    msj = 'Plot whole array? /'&(defval)&'/  > '
    input (msj) (actval)
    if actval = ''
      actval = defval
    endif
    is_logical (actval)
    set_whole2 (actval)
    if not actval
      get_xstart2 (defval)
      msj = 'X start? /'&(defval)&'/  > '
      input (msj) (actval)
      if actval = ''
        actval = defval
      endif
      setval = REAL( actval )
      set_xstart2 (setval)
      get_xend2 (defval)
      msj = 'X end? /'&(defval)&'/  > '
      input (msj) (actval)
      if actval = ''
        actval = defval
      endif
      setval = REAL( actval )
      set_xend2 (setval)
      get_ystart2 (defval)
      msj = 'Y start? /'&(defval)&'/  > '
      input (msj) (actval)
      if actval = ''
        actval = defval
      endif
      setval = REAL( actval )
      set_ystart2 (setval)
      get_yend2 (defval)
      msj = 'Y end? /'&(defval)&'/  > '
      input (msj) (actval)
      if actval = ''
        actval = defval
      endif
      setval = REAL( actval )
      set_yend2 (setval)
    endif
  else if port = 3
    get_type3 (defval)
    msj = 'Display type? /'&(defval)&'/  > '
    input (msj) (actval)
    if actval = ''
      actval = defval
    endif
    setval = UPCASE( actval )
    set_type3 (setval)
    if setval = 'GRAPH'
      get_cut_dir3 (defval)
      msj = 'Direction of cut? /'&(defval)&'/  > '
      input (msj) (actval)
      if actval = ''
        actval = defval
      endif
      set_cut_dir3 (actval)
      get_cut_start3 (defval)
      msj = 'Start of cut? /'&(defval)&'/  > '
      input (msj) (actval)
      if actval = ''
        actval = defval
      endif
      setval = REAL( actval )
      set_cut_start3 (setval)
      get_cut_end3 (defval)
      msj = 'End of cut? /'&(defval)&'/  > '
      input (msj) (actval)
      if actval = ''
        actval = defval
      endif
      setval = REAL( actval )
      set_cut_end3 (setval)
      get_errors3 (defval)
      msj = 'Plot error bars? /'&(defval)&'/  > '
      input (msj) (actval)
      if actval = ''
        actval = defval
      endif
      is_logical (actval)
      set_errors3 (actval)
    else if setval = 'OVERGRAPH'
      get_cut_dir3 (defval)
      msj = 'Direction of cut? /'&(defval)&'/  > '
      input (msj) (actval)
      if actval = ''
        actval = defval
      endif
      set_cut_dir3 (actval)
      get_cut_start3 (defval)
      msj = 'Start of cut? /'&(defval)&'/  > '
      input (msj) (actval)
      if actval = ''
        actval = defval
      endif
      setval = REAL( actval )
      set_cut_start3 (setval)
      get_cut_end3 (defval)
      msj = 'End of cut? /'&(defval)&'/  > '
      input (msj) (actval)
      if actval = ''
        actval = defval
      endif
      setval = REAL( actval )
      set_cut_end3 (setval)
      get_errors3 (defval)
      msj = 'Plot error bars? /'&(defval)&'/  > '
      input (msj) (actval)
      if actval = ''
        actval = defval
      endif
      is_logical (actval)
      set_errors3 (actval)
      get_overcol3 (defval)
      mssj = 'Overgraph colour? /'&(defval)&'/  > '
      input (msj) (actval)
      if actval = ''
        actval = defval
      endif
      set_overcol3 (actval)
    else if setval = 'HISTOGRAM'
      get_bins3 (defval)
      msj = 'Number of bins? /'&(defval)&'/  > '
      input (msj) (actval)
      if actval = ''
        actval = defval
      endif
      setval = INTEGER( actval )
      set_bins3 (setval)
    else if setval = 'CONTOUR'
      get_contour3 (defval)
      msj = 'Contour type? /'&(defval)&'/ > '
      input (msj) (actval)
      if actval = ''
        actval = defval
      endif
      set_contour3 (actval)
    endif
    get_plane3 (defval)
    msj = 'Data plane? /'&(defval)&'/  > '
    input (msj) (actval)
    if actval = ''
      actval = defval
    endif
    set_plane3 (actval)
    get_autoscale3 (defval)
    msj = 'Autoscale? /'&(defval)&'/  > '
    input (msj) (actval)
    if actval = ''
      actval = defval
    endif
    is_logical (actval)
    set_autoscale3 (actval)
    if not actval
      get_low3 (defval)
      msj = 'Low value? /'&(defval)&'/  > '
      input (msj) (actval)
      if actval = ''
        actval = defval
      endif
      setval = REAL( actval )
      set_low3 (setval)
      get_high3 (defval)
      msj = 'High value? /'&(defval)&'/  > '
      input (msj) (actval)
      if actval = ''
        actval = defval
      endif
      setval = REAL( actval )
      set_high3 (setval)
    endif
    get_whole3 (defval)
    msj = 'Plot whole array? /'&(defval)&'/  > '
    input (msj) (actval)
    if actval = ''
      actval = defval
    endif
    is_logical (actval)
    set_whole3 (actval)
    if not actval
      get_xstart3 (defval)
      msj = 'X start? /'&(defval)&'/  > '
      input (msj) (actval)
      if actval = ''
        actval = defval
      endif
      setval = REAL( actval )
      set_xstart3 (setval)
      get_xend3 (defval)
      msj = 'X end? /'&(defval)&'/  > '
      input (msj) (actval)
      if actval = ''
        actval = defval
      endif
      setval = REAL( actval )
      set_xend3 (setval)
      get_ystart3 (defval)
      msj = 'Y start? /'&(defval)&'/  > '
      input (msj) (actval)
      if actval = ''
        actval = defval
      endif
      setval = REAL( actval )
      set_ystart3 (setval)
      get_yend3 (defval)
      msj = 'Y end? /'&(defval)&'/  > '
      input (msj) (actval)
      if actval = ''
        actval = defval
      endif
      setval = REAL( actval )
      set_yend3 (setval)
    endif
  else if port = 4
    get_type4 (defval)
    msj = 'Display type? /'&(defval)&'/  > '
    input (msj) (actval)
    if actval = ''
      actval = defval
    endif
    setval = UPCASE( actval )
    set_type4 (setval)
    if setval = 'GRAPH'
      get_cut_dir4 (defval)
      msj = 'Direction of cut? /'&(defval)&'/  > '
      input (msj) (actval)
      if actval = ''
        actval = defval
      endif
      set_cut_dir4 (actval)
      get_cut_start4 (defval)
      msj = 'Start of cut? /'&(defval)&'/  > '
      input (msj) (actval)
      if actval = ''
        actval = defval
      endif
      setval = REAL( actval )
      set_cut_start4 (setval)
      get_cut_end4 (defval)
      msj = 'End of cut? /'&(defval)&'/  > '
      input (msj) (actval)
      if actval = ''
        actval = defval
      endif
      setval = REAL( actval )
      set_cut_end4 (setval)
      get_errors4 (defval)
      msj = 'Plot error bars? /'&(defval)&'/  > '
      input (msj) (actval)
      if actval = ''
        actval = defval
      endif
      is_logical (actval)
      set_errors4 (actval)
    else if setval = 'OVERGRAPH'
      get_cut_dir4 (defval)
      msj = 'Direction of cut? /'&(defval)&'/  > '
      input (msj) (actval)
      if actval = ''
        actval = defval
      endif
      set_cut_dir4 (actval)
      get_cut_start4 (defval)
      msj = 'Start of cut? /'&(defval)&'/  > '
      input (msj) (actval)
      if actval = ''
        actval = defval
      endif
      setval = REAL( actval )
      set_cut_start4 (setval)
      get_cut_end4 (defval)
      msj = 'End of cut? /'&(defval)&'/  > '
      input (msj) (actval)
      if actval = ''
        actval = defval
      endif
      setval = REAL( actval )
      set_cut_end4 (setval)
      get_errors4 (defval)
      msj = 'Plot error bars? /'&(defval)&'/  > '
      input (msj) (actval)
      if actval = ''
        actval = defval
      endif
      is_logical (actval)
      set_errors4 (actval)
      get_overcol4 (defval)
      mssj = 'Overgraph colour? /'&(defval)&'/  > '
      input (msj) (actval)
      if actval = ''
        actval = defval
      endif
      set_overcol4 (actval)
    else if setval = 'HISTOGRAM'
      get_bins4 (defval)
      msj = 'Number of bins? /'&(defval)&'/  > '
      input (msj) (actval)
      if actval = ''
        actval = defval
      endif
      setval = INTEGER( actval )
      set_bins4 (setval)
    else if setval = 'CONTOUR'
      get_contour4 (defval)
      msj = 'Contour type? /'&(defval)&'/ > '
      input (msj) (actval)
      if actval = ''
        actval = defval
      endif
      set_contour4 (actval)
    endif
    get_plane4 (defval)
    msj = 'Data plane? /'&(defval)&'/  > '
    input (msj) (actval)
    if actval = ''
      actval = defval
    endif
    set_plane4 (actval)
    get_autoscale4 (defval)
    msj = 'Autoscale? /'&(defval)&'/  > '
    input (msj) (actval)
    if actval = ''
      actval = defval
    endif
    is_logical (actval)
    set_autoscale4 (actval)
    if not actval
      get_low4 (defval)
      msj = 'Low value? /'&(defval)&'/  > '
      input (msj) (actval)
      if actval = ''
        actval = defval
      endif
      setval = REAL( actval )
      set_low4 (setval)
      get_high4 (defval)
      msj = 'High value? /'&(defval)&'/  > '
      input (msj) (actval)
      if actval = ''
        actval = defval
      endif
      setval = REAL( actval )
      set_high4 (setval)
    endif
    get_whole4 (defval)
    msj = 'Plot whole array? /'&(defval)&'/  > '
    input (msj) (actval)
    if actval = ''
      actval = defval
    endif
    is_logical (actval)
    set_whole4 (actval)
    if not actval
      get_xstart4 (defval)
      msj = 'X start? /'&(defval)&'/  > '
      input (msj) (actval)
      if actval = ''
        actval = defval
      endif
      setval = REAL( actval )
      set_xstart4 (setval)
      get_xend4 (defval)
      msj = 'X end? /'&(defval)&'/  > '
      input (msj) (actval)
      if actval = ''
        actval = defval
      endif
      setval = REAL( actval )
      set_xend4 (setval)
      get_ystart4 (defval)
      msj = 'Y start? /'&(defval)&'/  > '
      input (msj) (actval)
      if actval = ''
        actval = defval
      endif
      setval = REAL( actval )
      set_ystart4 (setval)
      get_yend4 (defval)
      msj = 'Y end? /'&(defval)&'/  > '
      input (msj) (actval)
      if actval = ''
        actval = defval
      endif
      setval = REAL( actval )
      set_yend4 (setval)
    endif
  else if port = 5
    get_type5 (defval)
    msj = 'Display type? /'&(defval)&'/  > '
    input (msj) (actval)
    if actval = ''
      actval = defval
    endif
    setval = UPCASE( actval )
    set_type5 (setval)
    if setval = 'GRAPH'
      get_cut_dir5 (defval)
      msj = 'Direction of cut? /'&(defval)&'/  > '
      input (msj) (actval)
      if actval = ''
        actval = defval
      endif
      set_cut_dir5 (actval)
      get_cut_start5 (defval)
      msj = 'Start of cut? /'&(defval)&'/  > '
      input (msj) (actval)
      if actval = ''
        actval = defval
      endif
      setval = REAL( actval )
      set_cut_start5 (setval)
      get_cut_end5 (defval)
      msj = 'End of cut? /'&(defval)&'/  > '
      input (msj) (actval)
      if actval = ''
        actval = defval
      endif
      setval = REAL( actval )
      set_cut_end5 (setval)
      get_errors5 (defval)
      msj = 'Plot error bars? /'&(defval)&'/  > '
      input (msj) (actval)
      if actval = ''
        actval = defval
      endif
      is_logical (actval)
      set_errors5 (actval)
    else if setval = 'OVERGRAPH'
      get_cut_dir5 (defval)
      msj = 'Direction of cut? /'&(defval)&'/  > '
      input (msj) (actval)
      if actval = ''
        actval = defval
      endif
      set_cut_dir5 (actval)
      get_cut_start5 (defval)
      msj = 'Start of cut? /'&(defval)&'/  > '
      input (msj) (actval)
      if actval = ''
        actval = defval
      endif
      setval = REAL( actval )
      set_cut_start5 (setval)
      get_cut_end5 (defval)
      msj = 'End of cut? /'&(defval)&'/  > '
      input (msj) (actval)
      if actval = ''
        actval = defval
      endif
      setval = REAL( actval )
      set_cut_end5 (setval)
      get_errors5 (defval)
      msj = 'Plot error bars? /'&(defval)&'/  > '
      input (msj) (actval)
      if actval = ''
        actval = defval
      endif
      is_logical (actval)
      set_errors5 (actval)
      get_overcol5 (defval)
      mssj = 'Overgraph colour? /'&(defval)&'/  > '
      input (msj) (actval)
      if actval = ''
        actval = defval
      endif
      set_overcol5 (actval)
    else if setval = 'HISTOGRAM'
      get_bins5 (defval)
      msj = 'Number of bins? /'&(defval)&'/  > '
      input (msj) (actval)
      if actval = ''
        actval = defval
      endif
      setval = INTEGER( actval )
      set_bins5 (setval)
    else if setval = 'CONTOUR'
      get_contour5 (defval)
      msj = 'Contour type? /'&(defval)&'/ > '
      input (msj) (actval)
      if actval = ''
        actval = defval
      endif
      set_contour5 (actval)
    endif
    get_plane5 (defval)
    msj = 'Data plane? /'&(defval)&'/  > '
    input (msj) (actval)
    if actval = ''
      actval = defval
    endif
    set_plane5 (actval)
    get_autoscale5 (defval)
    msj = 'Autoscale? /'&(defval)&'/  > '
    input (msj) (actval)
    if actval = ''
      actval = defval
    endif
    is_logical (actval)
    set_autoscale5 (actval)
    if not actval
      get_low5 (defval)
      msj = 'Low value? /'&(defval)&'/  > '
      input (msj) (actval)
      if actval = ''
        actval = defval
      endif
      setval = REAL( actval )
      set_low5 (setval)
      get_high5 (defval)
      msj = 'High value? /'&(defval)&'/  > '
      input (msj) (actval)
      if actval = ''
        actval = defval
      endif
      setval = REAL( actval )
      set_high5 (setval)
    endif
    get_whole5 (defval)
    msj = 'Plot whole array? /'&(defval)&'/  > '
    input (msj) (actval)
    if actval = ''
      actval = defval
    endif
    is_logical (actval)
    set_whole5 (actval)
    if not actval
      get_xstart5 (defval)
      msj = 'X start? /'&(defval)&'/  > '
      input (msj) (actval)
      if actval = ''
        actval = defval
      endif
      setval = REAL( actval )
      set_xstart5 (setval)
      get_xend5 (defval)
      msj = 'X end? /'&(defval)&'/  > '
      input (msj) (actval)
      if actval = ''
        actval = defval
      endif
      setval = REAL( actval )
      set_xend5 (setval)
      get_ystart5 (defval)
      msj = 'Y start? /'&(defval)&'/  > '
      input (msj) (actval)
      if actval = ''
        actval = defval
      endif
      setval = REAL( actval )
      set_ystart5 (setval)
      get_yend5 (defval)
      msj = 'Y end? /'&(defval)&'/  > '
      input (msj) (actval)
      if actval = ''
        actval = defval
      endif
      setval = REAL( actval )
      set_yend5 (setval)
    endif
  else if port = 6
    get_type6 (defval)
    msj = 'Display type? /'&(defval)&'/  > '
    input (msj) (actval)
    if actval = ''
      actval = defval
    endif
    setval = UPCASE( actval )
    set_type6 (setval)
    if setval = 'GRAPH'
      get_cut_dir6 (defval)
      msj = 'Direction of cut? /'&(defval)&'/  > '
      input (msj) (actval)
      if actval = ''
        actval = defval
      endif
      set_cut_dir6 (actval)
      get_cut_start6 (defval)
      msj = 'Start of cut? /'&(defval)&'/  > '
      input (msj) (actval)
      if actval = ''
        actval = defval
      endif
      setval = REAL( actval )
      set_cut_start6 (setval)
      get_cut_end6 (defval)
      msj = 'End of cut? /'&(defval)&'/  > '
      input (msj) (actval)
      if actval = ''
        actval = defval
      endif
      setval = REAL( actval )
      set_cut_end6 (setval)
      get_errors6 (defval)
      msj = 'Plot error bars? /'&(defval)&'/  > '
      input (msj) (actval)
      if actval = ''
        actval = defval
      endif
      is_logical (actval)
      set_errors6 (actval)
    else if setval = 'OVERGRAPH'
      get_cut_dir6 (defval)
      msj = 'Direction of cut? /'&(defval)&'/  > '
      input (msj) (actval)
      if actval = ''
        actval = defval
      endif
      set_cut_dir6 (actval)
      get_cut_start6 (defval)
      msj = 'Start of cut? /'&(defval)&'/  > '
      input (msj) (actval)
      if actval = ''
        actval = defval
      endif
      setval = REAL( actval )
      set_cut_start6 (setval)
      get_cut_end6 (defval)
      msj = 'End of cut? /'&(defval)&'/  > '
      input (msj) (actval)
      if actval = ''
        actval = defval
      endif
      setval = REAL( actval )
      set_cut_end6 (setval)
      get_errors6 (defval)
      msj = 'Plot error bars? /'&(defval)&'/  > '
      input (msj) (actval)
      if actval = ''
        actval = defval
      endif
      is_logical (actval)
      set_errors6 (actval)
      get_overcol6 (defval)
      mssj = 'Overgraph colour? /'&(defval)&'/  > '
      input (msj) (actval)
      if actval = ''
        actval = defval
      endif
      set_overcol6 (actval)
    else if setval = 'HISTOGRAM'
      get_bins6 (defval)
      msj = 'Number of bins? /'&(defval)&'/  > '
      input (msj) (actval)
      if actval = ''
        actval = defval
      endif
      setval = INTEGER( actval )
      set_bins6 (setval)
    else if setval = 'CONTOUR'
      get_contour6 (defval)
      msj = 'Contour type? /'&(defval)&'/ > '
      input (msj) (actval)
      if actval = ''
        actval = defval
      endif
      set_contour6 (actval)
    endif
    get_plane6 (defval)
    msj = 'Data plane? /'&(defval)&'/  > '
    input (msj) (actval)
    if actval = ''
      actval = defval
    endif
    set_plane6 (actval)
    get_autoscale6 (defval)
    msj = 'Autoscale? /'&(defval)&'/  > '
    input (msj) (actval)
    if actval = ''
      actval = defval
    endif
    is_logical (actval)
    set_autoscale6 (actval)
    if not actval
      get_low6 (defval)
      msj = 'Low value? /'&(defval)&'/  > '
      input (msj) (actval)
      if actval = ''
        actval = defval
      endif
      setval = REAL( actval )
      set_low6 (setval)
      get_high6 (defval)
      msj = 'High value? /'&(defval)&'/  > '
      input (msj) (actval)
      if actval = ''
        actval = defval
      endif
      setval = REAL( actval )
      set_high6 (setval)
    endif
    get_whole6 (defval)
    msj = 'Plot whole array? /'&(defval)&'/  > '
    input (msj) (actval)
    if actval = ''
      actval = defval
    endif
    is_logical (actval)
    set_whole6 (actval)
    if not actval
      get_xstart6 (defval)
      msj = 'X start? /'&(defval)&'/  > '
      input (msj) (actval)
      if actval = ''
        actval = defval
      endif
      setval = REAL( actval )
      set_xstart6 (setval)
      get_xend6 (defval)
      msj = 'X end? /'&(defval)&'/  > '
      input (msj) (actval)
      if actval = ''
        actval = defval
      endif
      setval = REAL( actval )
      set_xend6 (setval)
      get_ystart6 (defval)
      msj = 'Y start? /'&(defval)&'/  > '
      input (msj) (actval)
      if actval = ''
        actval = defval
      endif
      setval = REAL( actval )
      set_ystart6 (setval)
      get_yend6 (defval)
      msj = 'Y end? /'&(defval)&'/  > '
      input (msj) (actval)
      if actval = ''
        actval = defval
      endif
      setval = REAL( actval )
      set_yend6 (setval)
    endif
  else if port = 7
    get_type7 (defval)
    msj = 'Display type? /'&(defval)&'/  > '
    input (msj) (actval)
    if actval = ''
      actval = defval
    endif
    setval = UPCASE( actval )
    set_type7 (setval)
    if setval = 'GRAPH'
      get_cut_dir7 (defval)
      msj = 'Direction of cut? /'&(defval)&'/  > '
      input (msj) (actval)
      if actval = ''
        actval = defval
      endif
      set_cut_dir7 (actval)
      get_cut_start7 (defval)
      msj = 'Start of cut? /'&(defval)&'/  > '
      input (msj) (actval)
      if actval = ''
        actval = defval
      endif
      setval = REAL( actval )
      set_cut_start7 (setval)
      get_cut_end7 (defval)
      msj = 'End of cut? /'&(defval)&'/  > '
      input (msj) (actval)
      if actval = ''
        actval = defval
      endif
      setval = REAL( actval )
      set_cut_end7 (setval)
      get_errors7 (defval)
      msj = 'Plot error bars? /'&(defval)&'/  > '
      input (msj) (actval)
      if actval = ''
        actval = defval
      endif
      is_logical (actval)
      set_errors7 (actval)
    else if setval = 'OVERGRAPH'
      get_cut_dir7 (defval)
      msj = 'Direction of cut? /'&(defval)&'/  > '
      input (msj) (actval)
      if actval = ''
        actval = defval
      endif
      set_cut_dir7 (actval)
      get_cut_start7 (defval)
      msj = 'Start of cut? /'&(defval)&'/  > '
      input (msj) (actval)
      if actval = ''
        actval = defval
      endif
      setval = REAL( actval )
      set_cut_start7 (setval)
      get_cut_end7 (defval)
      msj = 'End of cut? /'&(defval)&'/  > '
      input (msj) (actval)
      if actval = ''
        actval = defval
      endif
      setval = REAL( actval )
      set_cut_end7 (setval)
      get_errors7 (defval)
      msj = 'Plot error bars? /'&(defval)&'/  > '
      input (msj) (actval)
      if actval = ''
        actval = defval
      endif
      is_logical (actval)
      set_errors7 (actval)
      get_overcol7 (defval)
      mssj = 'Overgraph colour? /'&(defval)&'/  > '
      input (msj) (actval)
      if actval = ''
        actval = defval
      endif
      set_overcol7 (actval)
    else if setval = 'HISTOGRAM'
      get_bins7 (defval)
      msj = 'Number of bins? /'&(defval)&'/  > '
      input (msj) (actval)
      if actval = ''
        actval = defval
      endif
      setval = INTEGER( actval )
      set_bins7 (setval)
    else if setval = 'CONTOUR'
      get_contour7 (defval)
      msj = 'Contour type? /'&(defval)&'/ > '
      input (msj) (actval)
      if actval = ''
        actval = defval
      endif
      set_contour7 (actval)
    endif
    get_plane7 (defval)
    msj = 'Data plane? /'&(defval)&'/  > '
    input (msj) (actval)
    if actval = ''
      actval = defval
    endif
    set_plane7 (actval)
    get_autoscale7 (defval)
    msj = 'Autoscale? /'&(defval)&'/  > '
    input (msj) (actval)
    if actval = ''
      actval = defval
    endif
    is_logical (actval)
    set_autoscale7 (actval)
    if not actval
      get_low7 (defval)
      msj = 'Low value? /'&(defval)&'/  > '
      input (msj) (actval)
      if actval = ''
        actval = defval
      endif
      setval = REAL( actval )
      set_low7 (setval)
      get_high7 (defval)
      msj = 'High value? /'&(defval)&'/  > '
      input (msj) (actval)
      if actval = ''
        actval = defval
      endif
      setval = REAL( actval )
      set_high7 (setval)
    endif
    get_whole7 (defval)
    msj = 'Plot whole array? /'&(defval)&'/  > '
    input (msj) (actval)
    if actval = ''
      actval = defval
    endif
    is_logical (actval)
    set_whole7 (actval)
    if not actval
      get_xstart7 (defval)
      msj = 'X start? /'&(defval)&'/  > '
      input (msj) (actval)
      if actval = ''
        actval = defval
      endif
      setval = REAL( actval )
      set_xstart7 (setval)
      get_xend7 (defval)
      msj = 'X end? /'&(defval)&'/  > '
      input (msj) (actval)
      if actval = ''
        actval = defval
      endif
      setval = REAL( actval )
      set_xend7 (setval)
      get_ystart7 (defval)
      msj = 'Y start? /'&(defval)&'/  > '
      input (msj) (actval)
      if actval = ''
        actval = defval
      endif
      setval = REAL( actval )
      set_ystart7 (setval)
      get_yend7 (defval)
      msj = 'Y end? /'&(defval)&'/  > '
      input (msj) (actval)
      if actval = ''
        actval = defval
      endif
      setval = REAL( actval )
      set_yend7 (setval)
    endif
  else if port = 8
    get_type8 (defval)
    msj = 'Display type? /'&(defval)&'/  > '
    input (msj) (actval)
    if actval = ''
      actval = defval
    endif
    setval = UPCASE( actval )
    set_type8 (setval)
    if setval = 'GRAPH'
      get_cut_dir8 (defval)
      msj = 'Direction of cut? /'&(defval)&'/  > '
      input (msj) (actval)
      if actval = ''
        actval = defval
      endif
      set_cut_dir8 (actval)
      get_cut_start8 (defval)
      msj = 'Start of cut? /'&(defval)&'/  > '
      input (msj) (actval)
      if actval = ''
        actval = defval
      endif
      setval = REAL( actval )
      set_cut_start8 (setval)
      get_cut_end8 (defval)
      msj = 'End of cut? /'&(defval)&'/  > '
      input (msj) (actval)
      if actval = ''
        actval = defval
      endif
      setval = REAL( actval )
      set_cut_end8 (setval)
      get_errors8 (defval)
      msj = 'Plot error bars? /'&(defval)&'/  > '
      input (msj) (actval)
      if actval = ''
        actval = defval
      endif
      is_logical (actval)
      set_errors8 (actval)
    else if setval = 'OVERGRAPH'
      get_cut_dir8 (defval)
      msj = 'Direction of cut? /'&(defval)&'/  > '
      input (msj) (actval)
      if actval = ''
        actval = defval
      endif
      set_cut_dir8 (actval)
      get_cut_start8 (defval)
      msj = 'Start of cut? /'&(defval)&'/  > '
      input (msj) (actval)
      if actval = ''
        actval = defval
      endif
      setval = REAL( actval )
      set_cut_start8 (setval)
      get_cut_end8 (defval)
      msj = 'End of cut? /'&(defval)&'/  > '
      input (msj) (actval)
      if actval = ''
        actval = defval
      endif
      setval = REAL( actval )
      set_cut_end8 (setval)
      get_errors8 (defval)
      msj = 'Plot error bars? /'&(defval)&'/  > '
      input (msj) (actval)
      if actval = ''
        actval = defval
      endif
      is_logical (actval)
      set_errors8 (actval)
      get_overcol8 (defval)
      mssj = 'Overgraph colour? /'&(defval)&'/  > '
      input (msj) (actval)
      if actval = ''
        actval = defval
      endif
      set_overcol8 (actval)
    else if setval = 'HISTOGRAM'
      get_bins8 (defval)
      msj = 'Number of bins? /'&(defval)&'/  > '
      input (msj) (actval)
      if actval = ''
        actval = defval
      endif
      setval = INTEGER( actval )
      set_bins8 (setval)
    else if setval = 'CONTOUR'
      get_contour8 (defval)
      msj = 'Contour type? /'&(defval)&'/ > '
      input (msj) (actval)
      if actval = ''
        actval = defval
      endif
      set_contour8 (actval)
    endif
    get_plane8 (defval)
    msj = 'Data plane? /'&(defval)&'/  > '
    input (msj) (actval)
    if actval = ''
      actval = defval
    endif
    set_plane8 (actval)
    get_autoscale8 (defval)
    msj = 'Autoscale? /'&(defval)&'/  > '
    input (msj) (actval)
    if actval = ''
      actval = defval
    endif
    is_logical (actval)
    set_autoscale8 (actval)
    if not actval
      get_low8 (defval)
      msj = 'Low value? /'&(defval)&'/  > '
      input (msj) (actval)
      if actval = ''
        actval = defval
      endif
      setval = REAL( actval )
      set_low8 (setval)
      get_high8 (defval)
      msj = 'High value? /'&(defval)&'/  > '
      input (msj) (actval)
      if actval = ''
        actval = defval
      endif
      setval = REAL( actval )
      set_high8 (setval)
    endif
    get_whole8 (defval)
    msj = 'Plot whole array? /'&(defval)&'/  > '
    input (msj) (actval)
    if actval = ''
      actval = defval
    endif
    is_logical (actval)
    set_whole8 (actval)
    if not actval
      get_xstart8 (defval)
      msj = 'X start? /'&(defval)&'/  > '
      input (msj) (actval)
      if actval = ''
        actval = defval
      endif
      setval = REAL( actval )
      set_xstart8 (setval)
      get_xend8 (defval)
      msj = 'X end? /'&(defval)&'/  > '
      input (msj) (actval)
      if actval = ''
        actval = defval
      endif
      setval = REAL( actval )
      set_xend8 (setval)
      get_ystart8 (defval)
      msj = 'Y start? /'&(defval)&'/  > '
      input (msj) (actval)
      if actval = ''
        actval = defval
      endif
      setval = REAL( actval )
      set_ystart8 (setval)
      get_yend8 (defval)
      msj = 'Y end? /'&(defval)&'/  > '
      input (msj) (actval)
      if actval = ''
        actval = defval
      endif
      setval = REAL( actval )
      set_yend8 (setval)
    endif
  else
    print 'No such port number!'
  endif
endproc

{
{ PROC GET_DEVICE
proc get_device
  device0 = ' '
  get_device0 (device0)
  print 'Port 0 plot device = ' (device0)
  device1 = ' '
  get_device1 (device1)
  print 'Port 1 plot device = ' (device1)
  device2 = ' '
  get_device2 (device2)
  print 'Port 2 plot device = ' (device2)
  device3 = ' '
  get_device3 (device3)
  print 'Port 3 plot device = ' (device3)
  device4 = ' '
  get_device4 (device4)
  print 'Port 4 plot device = ' (device4)
  device5 = ' '
  get_device5 (device5)
  print 'Port 5 plot device = ' (device5)
  device6 = ' '
  get_device6 (device6)
  print 'Port 6 plot device = ' (device6)
  device7 = ' '
  get_device7 (device7)
  print 'Port 7 plot device = ' (device7)
  device8 = ' '
  get_device8 (device8)
  print 'Port 8 plot device = ' (device8)
endproc

{
{ PROC GET_LUT
proc get_lut
  lut0 = ' '
  get_lut0 (lut0)
  print 'Port 0 colour (look-up) table = ' (lut0)
  lut1 = ' '
  get_lut1 (lut1)
  print 'Port 1 colour (look-up) table = ' (lut1)
  lut2 = ' '
  get_lut2 (lut2)
  print 'Port 2 colour (look-up) table = ' (lut2)
  lut3 = ' '
  get_lut3 (lut3)
  print 'Port 3 colour (look-up) table = ' (lut3)
  lut4 = ' '
  get_lut4 (lut4)
  print 'Port 4 colour (look-up) table = ' (lut4)
  lut5 = ' '
  get_lut5 (lut5)
  print 'Port 5 colour (look-up) table = ' (lut5)
  lut6 = ' '
  get_lut6 (lut6)
  print 'Port 6 colour (look-up) table = ' (lut6)
  lut7 = ' '
  get_lut7 (lut7)
  print 'Port 7 colour (look-up) table = ' (lut7)
  lut8 = ' '
  get_lut8 (lut8)
  print 'Port 8 colour (look-up) table = ' (lut8)
endproc

{
{ PROC GET_TITLE
proc get_title
  title0 = ' '
  get_title0 (title0)
  print 'Port 0 title = ' (title0)
  title1 = ' '
  get_title1 (title1)
  print 'Port 1 title = ' (title1)
  title2 = ' '
  get_title2 (title2)
  print 'Port 2 title = ' (title2)
  title3 = ' '
  get_title3 (title3)
  print 'Port 3 title = ' (title3)
  title4 = ' '
  get_title4 (title4)
  print 'Port 4 title = ' (title4)
  title5 = ' '
  get_title5 (title5)
  print 'Port 5 title = ' (title5)
  title6 = ' '
  get_title6 (title6)
  print 'Port 6 title = ' (title6)
  title7 = ' '
  get_title7 (title7)
  print 'Port 7 title = ' (title7)
  title8 = ' '
  get_title8 (title8)
  print 'Port 8 title = ' (title8)
endproc

{
{ PROC GET_TYPE
proc get_type
  type0 = ' '
  get_type0 (type0)
  print 'Port 0 display type = ' (type0)
  type1 = ' '
  get_type1 (type1)
  print 'Port 1 display type = ' (type1)
  type2 = ' '
  get_type2 (type2)
  print 'Port 2 display type = ' (type2)
  type3 = ' '
  get_type3 (type3)
  print 'Port 3 display type = ' (type3)
  type4 = ' '
  get_type4 (type4)
  print 'Port 4 display type = ' (type4)
  type5 = ' '
  get_type5 (type5)
  print 'Port 5 display type = ' (type5)
  type6 = ' '
  get_type6 (type6)
  print 'Port 6 display type = ' (type6)
  type7 = ' '
  get_type7 (type7)
  print 'Port 7 display type = ' (type7)
  type8 = ' '
  get_type8 (type8)
  print 'Port 8 display type = ' (type8)
endproc

{
{ PROC GET_PLANE
proc get_plane
  plane0 = ' '
  get_plane0 (plane0)
  print 'Plot 0 display plane = ' (plane0)
  plane1 = ' '
  get_plane1 (plane1)
  print 'Plot 1 display plane = ' (plane1)
  plane2 = ' '
  get_plane2 (plane2)
  print 'Plot 2 display plane = ' (plane2)
  plane3 = ' '
  get_plane3 (plane3)
  print 'Plot 3 display plane = ' (plane3)
  plane4 = ' '
  get_plane4 (plane4)
  print 'Plot 4 display plane = ' (plane4)
  plane5 = ' '
  get_plane5 (plane5)
  print 'Plot 5 display plane = ' (plane5)
  plane6 = ' '
  get_plane6 (plane6)
  print 'Plot 6 display plane = ' (plane6)
  plane7 = ' '
  get_plane7 (plane7)
  print 'Plot 7 display plane = ' (plane7)
  plane8 = ' '
  get_plane8 (plane8)
  print 'Plot 8 display plane = ' (plane8)
endproc

{
{ PROC GET_AUTOSCALE
proc get_autoscale
  autoscale0 = ' '
  get_autoscale0 (autoscale0)
  print 'Port 0 autoscaling = ' (autoscale0)
  autoscale1 = ' '
  get_autoscale1 (autoscale1)
  print 'Port 1 autoscaling = ' (autoscale1)
  autoscale2 = ' '
  get_autoscale2 (autoscale2)
  print 'Port 2 autoscaling = ' (autoscale2)
  autoscale3 = ' '
  get_autoscale3 (autoscale3)
  print 'Port 3 autoscaling = ' (autoscale3)
  autoscale4 = ' '
  get_autoscale4 (autoscale4)
  print 'Port 4 autoscaling = ' (autoscale4)
  autoscale5 = ' '
  get_autoscale5 (autoscale5)
  print 'Port 5 autoscaling = ' (autoscale5)
  autoscale6 = ' '
  get_autoscale6 (autoscale6)
  print 'Port 6 autoscaling = ' (autoscale6)
  autoscale7 = ' '
  get_autoscale7 (autoscale7)
  print 'Port 7 autoscaling = ' (autoscale7)
  autoscale8 = ' '
  get_autoscale8 (autoscale8)
  print 'Port 8 autoscaling = ' (autoscale8)
endproc

{
{ PROC GET_ERASE
proc get_erase
  erase0 = ' '
  get_erase0 (erase0)
  print 'Port 0 pre-erase plot = ' (erase0)
  erase1 = ' '
  get_erase1 (erase1)
  print 'Port 1 pre-erase plot = ' (erase1)
  erase2 = ' '
  get_erase2 (erase2)
  print 'Port 2 pre-erase plot = ' (erase2)
  erase3 = ' '
  get_erase3 (erase3)
  print 'Port 3 pre-erase plot = ' (erase3)
  erase4 = ' '
  get_erase4 (erase4)
  print 'Port 4 pre-erase plot = ' (erase4)
  erase5 = ' '
  get_erase5 (erase5)
  print 'Port 5 pre-erase plot = ' (erase5)
  erase6 = ' '
  get_erase6 (erase6)
  print 'Port 6 pre-erase plot = ' (erase6)
  erase7 = ' '
  get_erase7 (erase7)
  print 'Port 7 pre-erase plot = ' (erase7)
  erase8 = ' '
  get_erase8 (erase8)
  print 'Port 8 pre-erase plot = ' (erase8)
endproc

{
{ PROC GET_HIGH
proc get_high
  high0 = ' '
  get_high0 (high0)
  print 'Port 0 high value = ' (high0)
  high1 = ' '
  get_high1 (high1)
  print 'Port 1 high value = ' (high1)
  high2 = ' '
  get_high2 (high2)
  print 'Port 2 high value = ' (high2)
  high3 = ' '
  get_high3 (high3)
  print 'Port 3 high value = ' (high3)
  high4 = ' '
  get_high4 (high4)
  print 'Port 4 high value = ' (high4)
  high5 = ' '
  get_high5 (high5)
  print 'Port 5 high value = ' (high5)
  high6 = ' '
  get_high6 (high6)
  print 'Port 6 high value = ' (high6)
  high7 = ' '
  get_high7 (high7)
  print 'Port 7 high value = ' (high7)
  high8 = ' '
  get_high8 (high8)
  print 'Port 8 high value = ' (high8)
endproc

{
{ PROC GET_LOW
proc get_low
  low0 = ' '
  get_low0 (low0)
  print 'Port 0 low value = ' (low0)
  low1 = ' '
  get_low1 (low1)
  print 'Port 1 low value = ' (low1)
  low2 = ' '
  get_low2 (low2)
  print 'Port 2 low value = ' (low2)
  low3 = ' '
  get_low3 (low3)
  print 'Port 3 low value = ' (low3)
  low4 = ' '
  get_low4 (low4)
  print 'Port 4 low value = ' (low4)
  low5 = ' '
  get_low5 (low5)
  print 'Port 5 low value = ' (low5)
  low6 = ' '
  get_low6 (low6)
  print 'Port 6 low value = ' (low6)
  low7 = ' '
  get_low7 (low7)
  print 'Port 7 low value = ' (low7)
  low8 = ' '
  get_low8 (low8)
  print 'Port 8 low value = ' (low8)
endproc

{
{ PROC GET_WHOLE
proc get_whole
  whole0 = ' '
  get_whole0 (whole0)
  print 'Port 0 plot whole array = ' (whole0)
  whole1 = ' '
  get_whole1 (whole1)
  print 'Port 1 plot whole array = ' (whole1)
  whole2 = ' '
  get_whole2 (whole2)
  print 'Port 2 plot whole array = ' (whole2)
  whole3 = ' '
  get_whole3 (whole3)
  print 'Port 3 plot whole array = ' (whole3)
  whole4 = ' '
  get_whole4 (whole4)
  print 'Port 4 plot whole array = ' (whole4)
  whole5 = ' '
  get_whole5 (whole5)
  print 'Port 5 plot whole array = ' (whole5)
  whole6 = ' '
  get_whole6 (whole6)
  print 'Port 6 plot whole array = ' (whole6)
  whole7 = ' '
  get_whole7 (whole7)
  print 'Port 7 plot whole array = ' (whole7)
  whole8 = ' '
  get_whole8 (whole8)
  print 'Port 8 plot whole array = ' (whole8)
endproc

{
{ PROC GET_XSTART
proc get_xstart
  xstart0 = ' '
  get_xstart0 (xstart0)
  print 'Port 0 X-start = ' (xstart0)
  xstart1 = ' '
  get_xstart1 (xstart1)
  print 'Port 1 X-start = ' (xstart1)
  xstart2 = ' '
  get_xstart2 (xstart2)
  print 'Port 2 X-start = ' (xstart2)
  xstart3 = ' '
  get_xstart3 (xstart3)
  print 'Port 3 X-start = ' (xstart3)
  xstart4 = ' '
  get_xstart4 (xstart4)
  print 'Port 4 X-start = ' (xstart4)
  xstart5 = ' '
  get_xstart5 (xstart5)
  print 'Port 5 X-start = ' (xstart5)
  xstart6 = ' '
  get_xstart6 (xstart6)
  print 'Port 6 X-start = ' (xstart6)
  xstart7 = ' '
  get_xstart7 (xstart7)
  print 'Port 7 X-start = ' (xstart7)
  xstart8 = ' '
  get_xstart8 (xstart8)
  print 'Port 8 X-start = ' (xstart8)
endproc

{
{ PROC GET_XEND
proc get_xend
  xend0 = ' '
  get_xend0 (xend0)
  print 'Port 0 X-end = ' (xend0)
  xend1 = ' '
  get_xend1 (xend1)
  print 'Port 1 X-end = ' (xend1)
  xend2 = ' '
  get_xend2 (xend2)
  print 'Port 2 X-end = ' (xend2)
  xend3 = ' '
  get_xend3 (xend3)
  print 'Port 3 X-end = ' (xend3)
  xend4 = ' '
  get_xend4 (xend4)
  print 'Port 4 X-end = ' (xend4)
  xend5 = ' '
  get_xend5 (xend5)
  print 'Port 5 X-end = ' (xend5)
  xend6 = ' '
  get_xend6 (xend6)
  print 'Port 6 X-end = ' (xend6)
  xend7 = ' '
  get_xend7 (xend7)
  print 'Port 7 X-end = ' (xend7)
  xend8 = ' '
  get_xend8 (xend8)
  print 'Port 8 X-end = ' (xend8)
endproc

{
{ PROC GET_YSTART
proc get_ystart
  ystart0 = ' '
  get_ystart0 (ystart0)
  print 'Port 0 Y-start = ' (ystart0)
  ystart1 = ' '
  get_ystart1 (ystart1)
  print 'Port 1 Y-start = ' (ystart1)
  ystart2 = ' '
  get_ystart2 (ystart2)
  print 'Port 2 Y-start = ' (ystart2)
  ystart3 = ' '
  get_ystart3 (ystart3)
  print 'Port 3 Y-start = ' (ystart3)
  ystart4 = ' '
  get_ystart4 (ystart4)
  print 'Port 4 Y-start = ' (ystart4)
  ystart5 = ' '
  get_ystart5 (ystart5)
  print 'Port 5 Y-start = ' (ystart5)
  ystart6 = ' '
  get_ystart6 (ystart6)
  print 'Port 6 Y-start = ' (ystart6)
  ystart7 = ' '
  get_ystart7 (ystart7)
  print 'Port 7 Y-start = ' (ystart7)
  ystart8 = ' '
  get_ystart8 (ystart8)
  print 'Port 8 Y-start = ' (ystart8)
endproc

{
{ PROC GET_YEND
proc get_yend
  yend0 = ' '
  get_yend0 (yend0)
  print 'Port 0 Y-end = ' (yend0)
  yend1 = ' '
  get_yend1 (yend1)
  print 'Port 1 Y-end = ' (yend1)
  yend2 = ' '
  get_yend2 (yend2)
  print 'Port 2 Y-end = ' (yend2)
  yend3 = ' '
  get_yend3 (yend3)
  print 'Port 3 Y-end = ' (yend3)
  yend4 = ' '
  get_yend4 (yend4)
  print 'Port 4 Y-end = ' (yend4)
  yend5 = ' '
  get_yend5 (yend5)
  print 'Port 5 Y-end = ' (yend5)
  yend6 = ' '
  get_yend6 (yend6)
  print 'Port 6 Y-end = ' (yend6)
  yend7 = ' '
  get_yend7 (yend7)
  print 'Port 7 Y-end = ' (yend7)
  yend8 = ' '
  get_yend8 (yend8)
  print 'Port 8 Y-end = ' (yend8)
endproc

{
{ PROC GET_ISTART
proc get_istart
  istart0 = ' '
  get_istart0 (istart0)
  print 'Port 0 I-start = ' (istart0)
  istart1 = ' '
  get_istart1 (istart1)
  print 'Port 1 I-start = ' (istart1)
  istart2 = ' '
  get_istart2 (istart2)
  print 'Port 2 I-start = ' (istart2)
  istart3 = ' '
  get_istart3 (istart3)
  print 'Port 3 I-start = ' (istart3)
  istart4 = ' '
  get_istart4 (istart4)
  print 'Port 4 I-start = ' (istart4)
  istart5 = ' '
  get_istart5 (istart5)
  print 'Port 5 I-start = ' (istart5)
  istart6 = ' '
  get_istart6 (istart6)
  print 'Port 6 I-start = ' (istart6)
  istart7 = ' '
  get_istart7 (istart7)
  print 'Port 7 I-start = ' (istart7)
  istart8 = ' '
  get_istart8 (istart8)
  print 'Port 8 I-start = ' (istart8)
endproc

{
{ PROC GET_IEND
proc get_iend
  iend0 = ' '
  get_iend0 (iend0)
  print 'Port 0 I-end = ' (iend0)
  iend1 = ' '
  get_iend1 (iend1)
  print 'Port 1 I-end = ' (iend1)
  iend2 = ' '
  get_iend2 (iend2)
  print 'Port 2 I-end = ' (iend2)
  iend3 = ' '
  get_iend3 (iend3)
  print 'Port 3 I-end = ' (iend3)
  iend4 = ' '
  get_iend4 (iend4)
  print 'Port 4 I-end = ' (iend4)
  iend5 = ' '
  get_iend5 (iend5)
  print 'Port 5 I-end = ' (iend5)
  iend6 = ' '
  get_iend6 (iend6)
  print 'Port 6 I-end = ' (iend6)
  iend7 = ' '
  get_iend7 (iend7)
  print 'Port 7 I-end = ' (iend7)
  iend8 = ' '
  get_iend8 (iend8)
  print 'Port 8 I-end = ' (iend8)
endproc

{
{ PROC GET_JSTART
proc get_jstart
  jstart0 = ' '
  get_jstart0 (jstart0)
  print 'Port 0 J-start = ' (jstart0)
  jstart1 = ' '
  get_jstart1 (jstart1)
  print 'Port 1 J-start = ' (jstart1)
  jstart2 = ' '
  get_jstart2 (jstart2)
  print 'Port 2 J-start = ' (jstart2)
  jstart3 = ' '
  get_jstart3 (jstart3)
  print 'Port 3 J-start = ' (jstart3)
  jstart4 = ' '
  get_jstart4 (jstart4)
  print 'Port 4 J-start = ' (jstart4)
  jstart5 = ' '
  get_jstart5 (jstart5)
  print 'Port 5 J-start = ' (jstart5)
  jstart6 = ' '
  get_jstart6 (jstart6)
  print 'Port 6 J-start = ' (jstart6)
  jstart7 = ' '
  get_jstart7 (jstart7)
  print 'Port 7 J-start = ' (jstart7)
  jstart8 = ' '
  get_jstart8 (jstart8)
  print 'Port 8 J-start = ' (jstart8)
endproc

{
{ PROC GET_JEND
proc get_jend
  jend0 = ' '
  get_jend0 (jend0)
  print 'Port 0 J-end = ' (jend0)
  jend1 = ' '
  get_jend1 (jend1)
  print 'Port 1 J-end = ' (jend1)
  jend2 = ' '
  get_jend2 (jend2)
  print 'Port 2 J-end = ' (jend2)
  jend3 = ' '
  get_jend3 (jend3)
  print 'Port 3 J-end = ' (jend3)
  jend4 = ' '
  get_jend4 (jend4)
  print 'Port 4 J-end = ' (jend4)
  jend5 = ' '
  get_jend5 (jend5)
  print 'Port 5 J-end = ' (jend5)
  jend6 = ' '
  get_jend6 (jend6)
  print 'Port 6 J-end = ' (jend6)
  jend7 = ' '
  get_jend7 (jend7)
  print 'Port 7 J-end = ' (jend7)
  jend8 = ' '
  get_jend8 (jend8)
  print 'Port 8 J-end = ' (jend8)
endproc

{
{ PROC GET_ERRORS
proc get_errors
  errors0 = ' '
  get_errors0 (errors0)
  print 'Port 0 plot errors = ' (errors0)
  errors1 = ' '
  get_errors1 (errors1)
  print 'Port 1 plot errors = ' (errors1)
  errors2 = ' '
  get_errors2 (errors2)
  print 'Port 2 plot errors = ' (errors2)
  errors3 = ' '
  get_errors3 (errors3)
  print 'Port 3 plot errors = ' (errors3)
  errors4 = ' '
  get_errors4 (errors4)
  print 'Port 4 plot errors = ' (errors4)
  errors5 = ' '
  get_errors5 (errors5)
  print 'Port 5 plot errors = ' (errors5)
  errors6 = ' '
  get_errors6 (errors6)
  print 'Port 6 plot errors = ' (errors6)
  errors7 = ' '
  get_errors7 (errors7)
  print 'Port 7 plot errors = ' (errors7)
  errors8 = ' '
  get_errors8 (errors8)
  print 'Port 8 plot errors = ' (errors8)
endproc

{
{ PROC GET_BINS
proc get_bins
  bins0 = ' '
  get_bins0 (bins0)
  print 'Port 0 histogram bins = ' (bins0)
  bins1 = ' '
  get_bins1 (bins1)
  print 'Port 1 histogram bins = ' (bins1)
  bins2 = ' '
  get_bins2 (bins2)
  print 'Port 2 histogram bins = ' (bins2)
  bins3 = ' '
  get_bins3 (bins3)
  print 'Port 3 histogram bins = ' (bins3)
  bins4 = ' '
  get_bins4 (bins4)
  print 'Port 4 histogram bins = ' (bins4)
  bins5 = ' '
  get_bins5 (bins5)
  print 'Port 5 histogram bins = ' (bins5)
  bins6 = ' '
  get_bins6 (bins6)
  print 'Port 6 histogram bins = ' (bins6)
  bins7 = ' '
  get_bins7 (bins7)
  print 'Port 7 histogram bins = ' (bins7)
  bins8 = ' '
  get_bins8 (bins8)
  print 'Port 8 histogram bins = ' (bins8)
endproc

{
{ PROC GET_OVERCOL
proc get_overcol
  overcol0 = ' '
  get_overcol0 (overcol0)
  print 'Port 0 colour for overgraph = ' (overcol0)
  overcol1 = ' '
  get_overcol1 (overcol1)
  print 'Port 1 colour for overgraph = ' (overcol1)
  overcol2 = ' '
  get_overcol2 (overcol2)
  print 'Port 2 colour for overgraph = ' (overcol2)
  overcol3 = ' '
  get_overcol3 (overcol3)
  print 'Port 3 colour for overgraph = ' (overcol3)
  overcol4 = ' '
  get_overcol4 (overcol4)
  print 'Port 4 colour for overgraph = ' (overcol4)
  overcol5 = ' '
  get_overcol5 (overcol5)
  print 'Port 5 colour for overgraph = ' (overcol5)
  overcol6 = ' '
  get_overcol6 (overcol6)
  print 'Port 6 colour for overgraph = ' (overcol6)
  overcol7 = ' '
  get_overcol7 (overcol7)
  print 'Port 7 colour for overgraph = ' (overcol7)
  overcol8 = ' '
  get_overcol8 (overcol8)
  print 'Port 8 colour for overgraph = ' (overcol8)
endproc

{
{ PROC GET_FG_COLOUR
proc get_fg_colour
  fg_colour0 = ' '
  get_fg_colour0 (fg_colour0)
  print 'Port 0 foreground colour = ' (fg_colour0)
  fg_colour1 = ' '
  get_fg_colour1 (fg_colour1)
  print 'Port 1 foreground colour = ' (fg_colour1)
  fg_colour2 = ' '
  get_fg_colour2 (fg_colour2)
  print 'Port 2 foreground colour = ' (fg_colour2)
  fg_colour3 = ' '
  get_fg_colour3 (fg_colour3)
  print 'Port 3 foreground colour = ' (fg_colour3)
  fg_colour4 = ' '
  get_fg_colour4 (fg_colour4)
  print 'Port 4 foreground colour = ' (fg_colour4)
  fg_colour5 = ' '
  get_fg_colour5 (fg_colour5)
  print 'Port 5 foreground colour = ' (fg_colour5)
  fg_colour6 = ' '
  get_fg_colour6 (fg_colour6)
  print 'Port 6 foreground colour = ' (fg_colour6)
  fg_colour7 = ' '
  get_fg_colour7 (fg_colour7)
  print 'Port 7 foreground colour = ' (fg_colour7)
  fg_colour8 = ' '
  get_fg_colour8 (fg_colour8)
  print 'Port 8 foreground colour = ' (fg_colour8)
endproc

{
{ PROC GET_BG_COLOUR
proc get_bg_colour
  bg_colour0 = ' '
  get_bg_colour0 (bg_colour0)
  print 'Port 0 background colour = ' (bg_colour0)
  bg_colour1 = ' '
  get_bg_colour1 (bg_colour1)
  print 'Port 1 background colour = ' (bg_colour1)
  bg_colour2 = ' '
  get_bg_colour2 (bg_colour2)
  print 'Port 2 background colour = ' (bg_colour2)
  bg_colour3 = ' '
  get_bg_colour3 (bg_colour3)
  print 'Port 3 background colour = ' (bg_colour3)
  bg_colour4 = ' '
  get_bg_colour4 (bg_colour4)
  print 'Port 4 background colour = ' (bg_colour4)
  bg_colour5 = ' '
  get_bg_colour5 (bg_colour5)
  print 'Port 5 background colour = ' (bg_colour5)
  bg_colour6 = ' '
  get_bg_colour6 (bg_colour6)
  print 'Port 6 background colour = ' (bg_colour6)
  bg_colour7 = ' '
  get_bg_colour7 (bg_colour7)
  print 'Port 7 background colour = ' (bg_colour7)
  bg_colour8 = ' '
  get_bg_colour8 (bg_colour8)
  print 'Port 8 background colour = ' (bg_colour8)
endproc

{
{ PROC GET_CONTOUR
proc get_contour
  contour0 = ' '
  get_contour0 (contour0)
  print 'Port 0 contour type = ' (contour0)
  contour1 = ' '
  get_contour1 (contour1)
  print 'Port 1 contour type = ' (contour1)
  contour2 = ' '
  get_contour2 (contour2)
  print 'Port 2 contour type = ' (contour2)
  contour3 = ' '
  get_contour3 (contour3)
  print 'Port 3 contour type = ' (contour3)
  contour4 = ' '
  get_contour4 (contour4)
  print 'Port 4 contour type = ' (contour4)
  contour5 = ' '
  get_contour5 (contour5)
  print 'Port 5 contour type = ' (contour5)
  contour6 = ' '
  get_contour6 (contour6)
  print 'Port 6 contour type = ' (contour6)
  contour7 = ' '
  get_contour7 (contour7)
  print 'Port 7 contour type = ' (contour7)
  contour8 = ' '
  get_contour8 (contour8)
  print 'Port 8 contour type = ' (contour8)
endproc

{
{ PROC GET_CUT_DIR
proc get_cut_dir
  cut_dir0 = ' '
  get_cut_dir0 (cut_dir0)
  print 'Port 0 Cut direction = ' (cut_dir0)
  cut_dir1 = ' '
  get_cut_dir1 (cut_dir1)
  print 'Port 1 Cut direction = ' (cut_dir1)
  cut_dir2 = ' '
  get_cut_dir2 (cut_dir2)
  print 'Port 2 Cut direction = ' (cut_dir2)
  cut_dir3 = ' '
  get_cut_dir3 (cut_dir3)
  print 'Port 3 Cut direction = ' (cut_dir3)
  cut_dir4 = ' '
  get_cut_dir4 (cut_dir4)
  print 'Port 4 Cut direction = ' (cut_dir4)
  cut_dir5 = ' '
  get_cut_dir5 (cut_dir5)
  print 'Port 5 Cut direction = ' (cut_dir5)
  cut_dir6 = ' '
  get_cut_dir6 (cut_dir6)
  print 'Port 6 Cut direction = ' (cut_dir6)
  cut_dir7 = ' '
  get_cut_dir7 (cut_dir7)
  print 'Port 7 Cut direction = ' (cut_dir7)
  cut_dir8 = ' '
  get_cut_dir8 (cut_dir8)
  print 'Port 8 Cut direction = ' (cut_dir8)
endproc

{
{ PROC GET_CUT_START
proc get_cut_start
  cut_start0 = ' '
  get_cut_start0 (cut_start0)
  print 'Port 0 Cut start = ' (cut_start0)
  cut_start1 = ' '
  get_cut_start1 (cut_start1)
  print 'Port 1 Cut start = ' (cut_start1)
  cut_start2 = ' '
  get_cut_start2 (cut_start2)
  print 'Port 2 Cut start = ' (cut_start2)
  cut_start3 = ' '
  get_cut_start3 (cut_start3)
  print 'Port 3 Cut start = ' (cut_start3)
  cut_start4 = ' '
  get_cut_start4 (cut_start4)
  print 'Port 4 Cut start = ' (cut_start4)
  cut_start5 = ' '
  get_cut_start5 (cut_start5)
  print 'Port 5 Cut start = ' (cut_start5)
  cut_start6 = ' '
  get_cut_start6 (cut_start6)
  print 'Port 6 Cut start = ' (cut_start6)
  cut_start7 = ' '
  get_cut_start7 (cut_start7)
  print 'Port 7 Cut start = ' (cut_start7)
  cut_start8 = ' '
  get_cut_start8 (cut_start8)
  print 'Port 8 Cut start = ' (cut_start8)
endproc

{
{ PROC GET_CUT_END
proc get_cut_end
  cut_end0 = ' '
  get_cut_end0 (cut_end0)
  print 'Port 0 Cut end = ' (cut_end0)
  cut_end1 = ' '
  get_cut_end1 (cut_end1)
  print 'Port 1 Cut end = ' (cut_end1)
  cut_end2 = ' '
  get_cut_end2 (cut_end2)
  print 'Port 2 Cut end = ' (cut_end2)
  cut_end3 = ' '
  get_cut_end3 (cut_end3)
  print 'Port 3 Cut end = ' (cut_end3)
  cut_end4 = ' '
  get_cut_end4 (cut_end4)
  print 'Port 4 Cut end = ' (cut_end4)
  cut_end5 = ' '
  get_cut_end5 (cut_end5)
  print 'Port 5 Cut end = ' (cut_end5)
  cut_end6 = ' '
  get_cut_end6 (cut_end6)
  print 'Port 6 Cut end = ' (cut_end6)
  cut_end7 = ' '
  get_cut_end7 (cut_end7)
  print 'Port 7 Cut end = ' (cut_end7)
  cut_end8 = ' '
  get_cut_end8 (cut_end8)
  print 'Port 8 Cut end = ' (cut_end8)
endproc

{
{ PROC GET_DISPLAY
proc get_display p0
  if UNDEFINED(p0)
    inputi 'Port number?  > ' (port)
  else
    port = INTEGER(p0)
  endif
  junk = ' '
  if port = 0
    get_device0 (junk)
    print 'Port' (port) 'device     = ' (junk)
    get_lut0 (junk)
    print 'Port' (port) 'lut        = ' (junk)
    get_title0 (junk)
    print 'Port' (port) 'title      = ' (junk)
    get_type0 (junk)
    print 'Port' (port) 'type       = ' (junk)
    get_plane0 (junk)
    print 'Port' (port) 'plane      = ' (junk)
    get_autoscale0 (junk)
    print 'Port' (port) 'autoscale  = ' (junk)
    get_erase0 (junk)
    print 'Port' (port) 'pre-erase  = ' (junk)
    get_high0 (junk)
    print 'Port' (port) 'high       = ' (junk)
    get_low0 (junk)
    print 'Port' (port) 'low        = ' (junk)
    get_whole0 (junk)
    print 'Port' (port) 'whole      = ' (junk)
    get_xstart0 (junk)
    print 'Port' (port) 'xstart     = ' (junk)
    get_xend0 (junk)
    print 'Port' (port) 'xend       = ' (junk)
    get_ystart0 (junk)
    print 'Port' (port) 'ystart     = ' (junk)
    get_yend0 (junk)
    print 'Port' (port) 'yend       = ' (junk)
    get_istart0 (junk)
    print 'Port' (port) 'istart     = ' (junk)
    get_iend0 (junk)
    print 'Port' (port) 'iend       = ' (junk)
    get_jstart0 (junk)
    print 'Port' (port) 'jstart     = ' (junk)
    get_jend0 (junk)
    print 'Port' (port) 'jend       = ' (junk)
    get_cut_dir0 (junk)
    print 'Port' (port) 'cut dir    = ' (junk)
    get_cut_start0 (junk)
    print 'Port' (port) 'cut start  = ' (junk)
    get_cut_end0 (junk)
    print 'Port' (port) 'cut end    = ' (junk)
    get_errors0 (junk)
    print 'Port' (port) 'errors     = ' (junk)
    get_bins0 (junk)
    print 'Port' (port) 'bins       = ' (junk)
    get_overcol0 (junk)
    print 'Port' (port) 'overcolour = ' (junk)
    get_fg_colour0 (junk)
    print 'Port' (port) 'fg_colour  = ' (junk)
    get_bg_colour0 (junk)
    print 'Port' (port) 'bg_colour  = ' (junk)
    get_contour0 (junk)
    print 'Port' (port) 'contour    = ' (junk)
  else if port = 1
    get_device1 (junk)
    print 'Port' (port) 'device     = ' (junk)
    get_lut1 (junk)
    print 'Port' (port) 'lut        = ' (junk)
    get_title1 (junk)
    print 'Port' (port) 'title      = ' (junk)
    get_type1 (junk)
    print 'Port' (port) 'type       = ' (junk)
    get_plane1 (junk)
    print 'Port' (port) 'plane      = ' (junk)
    get_autoscale1 (junk)
    print 'Port' (port) 'autoscale  = ' (junk)
    get_erase1 (junk)
    print 'Port' (port) 'pre-erase  = ' (junk)
    get_high1 (junk)
    print 'Port' (port) 'high       = ' (junk)
    get_low1 (junk)
    print 'Port' (port) 'low        = ' (junk)
    get_whole1 (junk)
    print 'Port' (port) 'whole      = ' (junk)
    get_xstart1 (junk)
    print 'Port' (port) 'xstart     = ' (junk)
    get_xend1 (junk)
    print 'Port' (port) 'xend       = ' (junk)
    get_ystart1 (junk)
    print 'Port' (port) 'ystart     = ' (junk)
    get_yend1 (junk)
    print 'Port' (port) 'yend       = ' (junk)
    get_istart1 (junk)
    print 'Port' (port) 'istart     = ' (junk)
    get_iend1 (junk)
    print 'Port' (port) 'iend       = ' (junk)
    get_jstart1 (junk)
    print 'Port' (port) 'jstart     = ' (junk)
    get_jend1 (junk)
    print 'Port' (port) 'jend       = ' (junk)
    get_cut_dir1 (junk)
    print 'Port' (port) 'cut dir    = ' (junk)
    get_cut_start1 (junk)
    print 'Port' (port) 'cut start  = ' (junk)
    get_cut_end1 (junk)
    print 'Port' (port) 'cut end    = ' (junk)
    get_errors1 (junk)
    print 'Port' (port) 'errors     = ' (junk)
    get_bins1 (junk)
    print 'Port' (port) 'bins       = ' (junk)
    get_overcol1 (junk)
    print 'Port' (port) 'overcolour = ' (junk)
    get_fg_colour1 (junk)
    print 'Port' (port) 'fg_colour  = ' (junk)
    get_bg_colour1 (junk)
    print 'Port' (port) 'bg_colour  = ' (junk)
    get_contour1 (junk)
    print 'Port' (port) 'contour    = ' (junk)
  else if port = 2
    get_device2 (junk)
    print 'Port' (port) 'device     = ' (junk)
    get_lut2 (junk)
    print 'Port' (port) 'lut        = ' (junk)
    get_title2 (junk)
    print 'Port' (port) 'title      = ' (junk)
    get_type2 (junk)
    print 'Port' (port) 'type       = ' (junk)
    get_plane2 (junk)
    print 'Port' (port) 'plane      = ' (junk)
    get_autoscale2 (junk)
    print 'Port' (port) 'autoscale  = ' (junk)
    get_erase2 (junk)
    print 'Port' (port) 'pre-erase  = ' (junk)
    get_high2 (junk)
    print 'Port' (port) 'high       = ' (junk)
    get_low2 (junk)
    print 'Port' (port) 'low        = ' (junk)
    get_whole2 (junk)
    print 'Port' (port) 'whole      = ' (junk)
    get_xstart2 (junk)
    print 'Port' (port) 'xstart     = ' (junk)
    get_xend2 (junk)
    print 'Port' (port) 'xend       = ' (junk)
    get_ystart2 (junk)
    print 'Port' (port) 'ystart     = ' (junk)
    get_yend2 (junk)
    print 'Port' (port) 'yend       = ' (junk)
    get_istart2 (junk)
    print 'Port' (port) 'istart     = ' (junk)
    get_iend2 (junk)
    print 'Port' (port) 'iend       = ' (junk)
    get_jstart2 (junk)
    print 'Port' (port) 'jstart     = ' (junk)
    get_jend2 (junk)
    print 'Port' (port) 'jend       = ' (junk)
    get_cut_dir2 (junk)
    print 'Port' (port) 'cut dir    = ' (junk)
    get_cut_start2 (junk)
    print 'Port' (port) 'cut start  = ' (junk)
    get_cut_end2 (junk)
    print 'Port' (port) 'cut end    = ' (junk)
    get_errors2 (junk)
    print 'Port' (port) 'errors     = ' (junk)
    get_bins2 (junk)
    print 'Port' (port) 'bins       = ' (junk)
    get_overcol2 (junk)
    print 'Port' (port) 'overcolour = ' (junk)
    get_fg_colour2 (junk)
    print 'Port' (port) 'fg_colour  = ' (junk)
    get_bg_colour2 (junk)
    print 'Port' (port) 'bg_colour  = ' (junk)
    get_contour2 (junk)
    print 'Port' (port) 'contour    = ' (junk)
  else if port = 3
    get_device3 (junk)
    print 'Port' (port) 'device     = ' (junk)
    get_lut3 (junk)
    print 'Port' (port) 'lut        = ' (junk)
    get_title3 (junk)
    print 'Port' (port) 'title      = ' (junk)
    get_type3 (junk)
    print 'Port' (port) 'type       = ' (junk)
    get_plane3 (junk)
    print 'Port' (port) 'plane      = ' (junk)
    get_autoscale3 (junk)
    print 'Port' (port) 'autoscale  = ' (junk)
    get_erase3 (junk)
    print 'Port' (port) 'pre-erase  = ' (junk)
    get_high3 (junk)
    print 'Port' (port) 'high       = ' (junk)
    get_low3 (junk)
    print 'Port' (port) 'low        = ' (junk)
    get_whole3 (junk)
    print 'Port' (port) 'whole      = ' (junk)
    get_xstart3 (junk)
    print 'Port' (port) 'xstart     = ' (junk)
    get_xend3 (junk)
    print 'Port' (port) 'xend       = ' (junk)
    get_ystart3 (junk)
    print 'Port' (port) 'ystart     = ' (junk)
    get_yend3 (junk)
    print 'Port' (port) 'yend       = ' (junk)
    get_istart3 (junk)
    print 'Port' (port) 'istart     = ' (junk)
    get_iend3 (junk)
    print 'Port' (port) 'iend       = ' (junk)
    get_jstart3 (junk)
    print 'Port' (port) 'jstart     = ' (junk)
    get_jend3 (junk)
    print 'Port' (port) 'jend       = ' (junk)
    get_cut_dir3 (junk)
    print 'Port' (port) 'cut dir    = ' (junk)
    get_cut_start3 (junk)
    print 'Port' (port) 'cut start  = ' (junk)
    get_cut_end3 (junk)
    print 'Port' (port) 'cut end    = ' (junk)
    get_errors3 (junk)
    print 'Port' (port) 'errors     = ' (junk)
    get_bins3 (junk)
    print 'Port' (port) 'bins       = ' (junk)
    get_overcol3 (junk)
    print 'Port' (port) 'overcolour = ' (junk)
    get_fg_colour3 (junk)
    print 'Port' (port) 'fg_colour  = ' (junk)
    get_bg_colour3 (junk)
    print 'Port' (port) 'bg_colour  = ' (junk)
    get_contour3 (junk)
    print 'Port' (port) 'contour    = ' (junk)
  else if port = 4
    get_device4 (junk)
    print 'Port' (port) 'device     = ' (junk)
    get_lut4 (junk)
    print 'Port' (port) 'lut        = ' (junk)
    get_title4 (junk)
    print 'Port' (port) 'title      = ' (junk)
    get_type4 (junk)
    print 'Port' (port) 'type       = ' (junk)
    get_plane4 (junk)
    print 'Port' (port) 'plane      = ' (junk)
    get_autoscale4 (junk)
    print 'Port' (port) 'autoscale  = ' (junk)
    get_erase4 (junk)
    print 'Port' (port) 'pre-erase  = ' (junk)
    get_high4 (junk)
    print 'Port' (port) 'high       = ' (junk)
    get_low4 (junk)
    print 'Port' (port) 'low        = ' (junk)
    get_whole4 (junk)
    print 'Port' (port) 'whole      = ' (junk)
    get_xstart4 (junk)
    print 'Port' (port) 'xstart     = ' (junk)
    get_xend4 (junk)
    print 'Port' (port) 'xend       = ' (junk)
    get_ystart4 (junk)
    print 'Port' (port) 'ystart     = ' (junk)
    get_yend4 (junk)
    print 'Port' (port) 'yend       = ' (junk)
    get_istart4 (junk)
    print 'Port' (port) 'istart     = ' (junk)
    get_iend4 (junk)
    print 'Port' (port) 'iend       = ' (junk)
    get_jstart4 (junk)
    print 'Port' (port) 'jstart     = ' (junk)
    get_jend4 (junk)
    print 'Port' (port) 'jend       = ' (junk)
    get_cut_dir4 (junk)
    print 'Port' (port) 'cut dir    = ' (junk)
    get_cut_start4 (junk)
    print 'Port' (port) 'cut start  = ' (junk)
    get_cut_end4 (junk)
    print 'Port' (port) 'cut end    = ' (junk)
    get_errors4 (junk)
    print 'Port' (port) 'errors     = ' (junk)
    get_bins4 (junk)
    print 'Port' (port) 'bins       = ' (junk)
    get_overcol4 (junk)
    print 'Port' (port) 'overcolour = ' (junk)
    get_fg_colour4 (junk)
    print 'Port' (port) 'fg_colour  = ' (junk)
    get_bg_colour4 (junk)
    print 'Port' (port) 'bg_colour  = ' (junk)
    get_contour4 (junk)
    print 'Port' (port) 'contour    = ' (junk)
  else if port = 5
    get_device5 (junk)
    print 'Port' (port) 'device     = ' (junk)
    get_lut5 (junk)
    print 'Port' (port) 'lut        = ' (junk)
    get_title5 (junk)
    print 'Port' (port) 'title      = ' (junk)
    get_type5 (junk)
    print 'Port' (port) 'type       = ' (junk)
    get_plane5 (junk)
    print 'Port' (port) 'plane      = ' (junk)
    get_autoscale5 (junk)
    print 'Port' (port) 'autoscale  = ' (junk)
    get_erase5 (junk)
    print 'Port' (port) 'pre-erase  = ' (junk)
    get_high5 (junk)
    print 'Port' (port) 'high       = ' (junk)
    get_low5 (junk)
    print 'Port' (port) 'low        = ' (junk)
    get_whole5 (junk)
    print 'Port' (port) 'whole      = ' (junk)
    get_xstart5 (junk)
    print 'Port' (port) 'xstart     = ' (junk)
    get_xend5 (junk)
    print 'Port' (port) 'xend       = ' (junk)
    get_ystart5 (junk)
    print 'Port' (port) 'ystart     = ' (junk)
    get_yend5 (junk)
    print 'Port' (port) 'yend       = ' (junk)
    get_istart5 (junk)
    print 'Port' (port) 'istart     = ' (junk)
    get_iend5 (junk)
    print 'Port' (port) 'iend       = ' (junk)
    get_jstart5 (junk)
    print 'Port' (port) 'jstart     = ' (junk)
    get_jend5 (junk)
    print 'Port' (port) 'jend       = ' (junk)
    get_cut_dir5 (junk)
    print 'Port' (port) 'cut dir    = ' (junk)
    get_cut_start5 (junk)
    print 'Port' (port) 'cut start  = ' (junk)
    get_cut_end5 (junk)
    print 'Port' (port) 'cut end    = ' (junk)
    get_errors5 (junk)
    print 'Port' (port) 'errors     = ' (junk)
    get_bins5 (junk)
    print 'Port' (port) 'bins       = ' (junk)
    get_overcol5 (junk)
    print 'Port' (port) 'overcolour = ' (junk)
    get_fg_colour5 (junk)
    print 'Port' (port) 'fg_colour  = ' (junk)
    get_bg_colour5 (junk)
    print 'Port' (port) 'bg_colour  = ' (junk)
    get_contour5 (junk)
    print 'Port' (port) 'contour    = ' (junk)
  else if port = 6
    get_device6 (junk)
    print 'Port' (port) 'device     = ' (junk)
    get_lut6 (junk)
    print 'Port' (port) 'lut        = ' (junk)
    get_title6 (junk)
    print 'Port' (port) 'title      = ' (junk)
    get_type6 (junk)
    print 'Port' (port) 'type       = ' (junk)
    get_plane6 (junk)
    print 'Port' (port) 'plane      = ' (junk)
    get_autoscale6 (junk)
    print 'Port' (port) 'autoscale  = ' (junk)
    get_erase6 (junk)
    print 'Port' (port) 'pre-erase  = ' (junk)
    get_high6 (junk)
    print 'Port' (port) 'high       = ' (junk)
    get_low6 (junk)
    print 'Port' (port) 'low        = ' (junk)
    get_whole6 (junk)
    print 'Port' (port) 'whole      = ' (junk)
    get_xstart6 (junk)
    print 'Port' (port) 'xstart     = ' (junk)
    get_xend6 (junk)
    print 'Port' (port) 'xend       = ' (junk)
    get_ystart6 (junk)
    print 'Port' (port) 'ystart     = ' (junk)
    get_yend6 (junk)
    print 'Port' (port) 'yend       = ' (junk)
    get_istart6 (junk)
    print 'Port' (port) 'istart     = ' (junk)
    get_iend6 (junk)
    print 'Port' (port) 'iend       = ' (junk)
    get_jstart6 (junk)
    print 'Port' (port) 'jstart     = ' (junk)
    get_jend6 (junk)
    print 'Port' (port) 'jend       = ' (junk)
    get_cut_dir6 (junk)
    print 'Port' (port) 'cut dir    = ' (junk)
    get_cut_start6 (junk)
    print 'Port' (port) 'cut start  = ' (junk)
    get_cut_end6 (junk)
    print 'Port' (port) 'cut end    = ' (junk)
    get_errors6 (junk)
    print 'Port' (port) 'errors     = ' (junk)
    get_bins6 (junk)
    print 'Port' (port) 'bins       = ' (junk)
    get_overcol6 (junk)
    print 'Port' (port) 'overcolour = ' (junk)
    get_fg_colour6 (junk)
    print 'Port' (port) 'fg_colour  = ' (junk)
    get_bg_colour6 (junk)
    print 'Port' (port) 'bg_colour  = ' (junk)
    get_contour6 (junk)
    print 'Port' (port) 'contour    = ' (junk)
  else if port = 7
    get_device7 (junk)
    print 'Port' (port) 'device     = ' (junk)
    get_lut7 (junk)
    print 'Port' (port) 'lut        = ' (junk)
    get_title7 (junk)
    print 'Port' (port) 'title      = ' (junk)
    get_type7 (junk)
    print 'Port' (port) 'type       = ' (junk)
    get_plane7 (junk)
    print 'Port' (port) 'plane      = ' (junk)
    get_autoscale7 (junk)
    print 'Port' (port) 'autoscale  = ' (junk)
    get_erase7 (junk)
    print 'Port' (port) 'pre-erase  = ' (junk)
    get_high7 (junk)
    print 'Port' (port) 'high       = ' (junk)
    get_low7 (junk)
    print 'Port' (port) 'low        = ' (junk)
    get_whole7 (junk)
    print 'Port' (port) 'whole      = ' (junk)
    get_xstart7 (junk)
    print 'Port' (port) 'xstart     = ' (junk)
    get_xend7 (junk)
    print 'Port' (port) 'xend       = ' (junk)
    get_ystart7 (junk)
    print 'Port' (port) 'ystart     = ' (junk)
    get_yend7 (junk)
    print 'Port' (port) 'yend       = ' (junk)
    get_istart7 (junk)
    print 'Port' (port) 'istart     = ' (junk)
    get_iend7 (junk)
    print 'Port' (port) 'iend       = ' (junk)
    get_jstart7 (junk)
    print 'Port' (port) 'jstart     = ' (junk)
    get_jend7 (junk)
    print 'Port' (port) 'jend       = ' (junk)
    get_cut_dir7 (junk)
    print 'Port' (port) 'cut dir    = ' (junk)
    get_cut_start7 (junk)
    print 'Port' (port) 'cut start  = ' (junk)
    get_cut_end7 (junk)
    print 'Port' (port) 'cut end    = ' (junk)
    get_errors7 (junk)
    print 'Port' (port) 'errors     = ' (junk)
    get_bins7 (junk)
    print 'Port' (port) 'bins       = ' (junk)
    get_overcol7 (junk)
    print 'Port' (port) 'overcolour = ' (junk)
    get_fg_colour7 (junk)
    print 'Port' (port) 'fg_colour  = ' (junk)
    get_bg_colour7 (junk)
    print 'Port' (port) 'bg_colour  = ' (junk)
    get_contour7 (junk)
    print 'Port' (port) 'contour    = ' (junk)
  else if port = 8
    get_device8 (junk)
    print 'Port' (port) 'device     = ' (junk)
    get_lut8 (junk)
    print 'Port' (port) 'lut        = ' (junk)
    get_title8 (junk)
    print 'Port' (port) 'title      = ' (junk)
    get_type8 (junk)
    print 'Port' (port) 'type       = ' (junk)
    get_plane8 (junk)
    print 'Port' (port) 'plane      = ' (junk)
    get_autoscale8 (junk)
    print 'Port' (port) 'autoscale  = ' (junk)
    get_erase8 (junk)
    print 'Port' (port) 'pre-erase  = ' (junk)
    get_high8 (junk)
    print 'Port' (port) 'high       = ' (junk)
    get_low8 (junk)
    print 'Port' (port) 'low        = ' (junk)
    get_whole8 (junk)
    print 'Port' (port) 'whole      = ' (junk)
    get_xstart8 (junk)
    print 'Port' (port) 'xstart     = ' (junk)
    get_xend8 (junk)
    print 'Port' (port) 'xend       = ' (junk)
    get_ystart8 (junk)
    print 'Port' (port) 'ystart     = ' (junk)
    get_yend8 (junk)
    print 'Port' (port) 'yend       = ' (junk)
    get_istart8 (junk)
    print 'Port' (port) 'istart     = ' (junk)
    get_iend8 (junk)
    print 'Port' (port) 'iend       = ' (junk)
    get_jstart8 (junk)
    print 'Port' (port) 'jstart     = ' (junk)
    get_jend8 (junk)
    print 'Port' (port) 'jend       = ' (junk)
    get_cut_dir8 (junk)
    print 'Port' (port) 'cut dir    = ' (junk)
    get_cut_start8 (junk)
    print 'Port' (port) 'cut start  = ' (junk)
    get_cut_end8 (junk)
    print 'Port' (port) 'cut end    = ' (junk)
    get_errors8 (junk)
    print 'Port' (port) 'errors     = ' (junk)
    get_bins8 (junk)
    print 'Port' (port) 'bins       = ' (junk)
    get_overcol8 (junk)
    print 'Port' (port) 'overcolour = ' (junk)
    get_fg_colour8 (junk)
    print 'Port' (port) 'fg_colour  = ' (junk)
    get_bg_colour8 (junk)
    print 'Port' (port) 'bg_colour  = ' (junk)
    get_contour8 (junk)
    print 'Port' (port) 'contour    = ' (junk)
  endif
endproc

{
{ PROC LUT
proc lut
  if UNDEFINED(p1)
    input 'New colour (look-up) table?  > ' (table)
  else
    table = p1
  endif
  set_lut (table)
  p4_lut
endproc

{
{ PROC DISPLAY
proc display p1 p2
  if UNDEFINED(p1)
    input 'Dataset to be displayed?  > ' (data)
  else
    data = p1
  endif
  if UNDEFINED(p2)
    port = 0
  else
    port = INTEGER(p2)
  endif
  p4_display data=(data) port=(port)
endproc

{
{ PROC CURSOR
proc cursor p1 p2
  if UNDEFINED(p1)
    input 'Task alias?  > ' (task)
  else
    task = p1
  endif
  if UNDEFINED(p2)
    port = 0
  else
    port = INTEGER(p2)
  endif

  print ' '
  print 'Move cursor to desired points and press a key [q to quit]'
  print ' '
  print '      X         Y           Data          Error    Quality'
  print '      _         _           ____          _____    _______'

  keys = 'X'
  stat = 0
  loop while stat = 0
    p4_cursorval port=(port)
    get (task) cursor_status (stat)
    if stat = 0
      act_x = ' '
      get (task) act_x    (act_x)
      act_y = ' '
      get (task) act_y    (act_y)
      dval = ' '
      get (task) dataval  (dval)
      vval = ' '
      get (task) dataerr  (vval)
      qval = ' '
      get (task) dataqual (qval)
      rx = REAL(act_x)
      ry = REAL(act_y)
      rd = REAL(dval)
      rv = REAL(vval)
      iq = INTEGER(qval)
      print (rx:7:2) ' ' (ry:7:2) ' ' (rd:12:5) ~
        ' ' (rv:12:4) ' ' (iq:8)
      get (task) keystroke (keys)
      if keys = 'q'
        stat = -1
      else if keys = 'Q'
        stat = -1
      endif
    endif
  endloop
endproc

{
{ PROC PLOT_IMAGE
proc plot_image p1 p2
  {
  { Get the dataset and port
  if UNDEFINED(p1)
    input 'Dataset to be displayed?  > ' (data)
  else
    data = p1
  endif
  if UNDEFINED(p2)
    port = 0
  else
    port = INTEGER(p2)
  endif
  {
  { Set display type to IMAGE
  if port = 0
    set_type0 'IMAGE'
  else if port = 1
    set_type1 'IMAGE'
  else if port = 2
    set_type2 'IMAGE'
  else if port = 3
    set_type3 'IMAGE'
  else if port = 4
    set_type4 'IMAGE'
  else if port = 5
    set_type5 'IMAGE'
  else if port = 6
    set_type6 'IMAGE'
  else if port = 7
    set_type7 'IMAGE'
  else if port = 8
    set_type8 'IMAGE'
  endif
  {
  { Display the dataset
  p4_lut
  p4_display data=(data) port=(port)
endproc

{
{ PROC PLOT_GRAPH
proc plot_graph p1 p2
  {
  { Get the dataset and port
  if UNDEFINED(p1)
    input 'Dataset to be displayed?  > ' (data)
  else
    data = p1
  endif
  if UNDEFINED(p2)
    port = 0
  else
    port = INTEGER(p2)
  endif
  {
  { Set display type to GRAPH
  if port = 0
    set_type0 'GRAPH'
  else if port = 1
    set_type1 'GRAPH'
  else if port = 2
    set_type2 'GRAPH'
  else if port = 3
    set_type3 'GRAPH'
  else if port = 4
    set_type4 'GRAPH'
  else if port = 5
    set_type5 'GRAPH'
  else if port = 6
    set_type6 'GRAPH'
  else if port = 7
    set_type7 'GRAPH'
  else if port = 8
    set_type8 'GRAPH'
  endif
  {
  { Display the dataset
  p4_lut
  p4_display data=(data) port=(port)
endproc

{
{ PROC PLOT_OVERGRAPH
proc plot_overgraph p1 p2 p3
  {
  { Get the dataset and port
  if UNDEFINED(p1)
    input 'Dataset to be displayed?  > ' (data)
  else
    data = p1
  endif
  if UNDEFINED(p2)
    port = 0
  else
    port = INTEGER(p2)
  endif
  {
  { Set display type to OVERGRAPH
  if port = 0
    set_type0 'OVERGRAPH'
  else if port = 1
    set_type1 'OVERGRAPH'
  else if port = 2
    set_type2 'OVERGRAPH'
  else if port = 3
    set_type3 'OVERGRAPH'
  else if port = 4
    set_type4 'OVERGRAPH'
  else if port = 5
    set_type5 'OVERGRAPH'
  else if port = 6
    set_type6 'OVERGRAPH'
  else if port = 7
    set_type7 'OVERGRAPH'
  else if port = 8
    set_type8 'OVERGRAPH'
  endif
  {
  { Display the dataset
  p4_lut
  p4_display data=(data) port=(port)
endproc

{
{ PROC PLOT_SURFACE
proc plot_surface p1 p2
  {
  { Get the dataset and port
  if UNDEFINED(p1)
    input 'Dataset to be displayed?  > ' (data)
  else
    data = p1
  endif
  if UNDEFINED(p2)
    port = 0
  else
    port = INTEGER(p2)
  endif
  {
  { Set display type to SURFACE
  if port = 0
    set_type0 'SURFACE'
  else if port = 1
    set_type1 'SURFACE'
  else if port = 2
    set_type2 'SURFACE'
  else if port = 3
    set_type3 'SURFACE'
  else if port = 4
    set_type4 'SURFACE'
  else if port = 5
    set_type5 'SURFACE'
  else if port = 6
    set_type6 'SURFACE'
  else if port = 7
    set_type7 'SURFACE'
  else if port = 8
    set_type8 'SURFACE'
  endif
  {
  { Display the dataset
  p4_lut
  p4_display data=(data) port=(port)
endproc

{
{ PROC PLOT_CONTOUR
proc plot_contour p1 p2
  {
  { Get the dataset and port
  if UNDEFINED(p1)
    input 'Dataset to be displayed?  > ' (data)
  else
    data = p1
  endif
  if UNDEFINED(p2)
    port = 0
  else
    port = INTEGER(p2)
  endif
  {
  { Set display type to CONTOUR
  if port = 0
    set_type0 'CONTOUR'
  else if port = 1
    set_type1 'CONTOUR'
  else if port = 2
    set_type2 'CONTOUR'
  else if port = 3
    set_type3 'CONTOUR'
  else if port = 4
    set_type4 'CONTOUR'
  else if port = 5
    set_type5 'CONTOUR'
  else if port = 6
    set_type6 'CONTOUR'
  else if port = 7
    set_type7 'CONTOUR'
  else if port = 8
    set_type8 'CONTOUR'
  endif
  {
  { Display the dataset
  p4_lut
  p4_display data=(data) port=(port)
endproc

{
{ PROC PLOT_HISTOGRAM
proc plot_histogram p1 p2
  {
  { Get the dataset and port
  if UNDEFINED(p1)
    input 'Dataset to be displayed?  > ' (data)
  else
    data = p1
  endif
  if UNDEFINED(p2)
    port = 0
  else
    port = INTEGER(p2)
  endif
  {
  { Set display type to HISTOGRAM
  if port = 0
    set_type0 'HISTOGRAM'
  else if port = 1
    set_type1 'HISTOGRAM'
  else if port = 2
    set_type2 'HISTOGRAM'
  else if port = 3
    set_type3 'HISTOGRAM'
  else if port = 4
    set_type4 'HISTOGRAM'
  else if port = 5
    set_type5 'HISTOGRAM'
  else if port = 6
    set_type6 'HISTOGRAM'
  else if port = 7
    set_type7 'HISTOGRAM'
  else if port = 8
    set_type8 'HISTOGRAM'
  endif
  {
  { Display the dataset
  p4_lut
  p4_display data=(data) port=(port)
endproc

{
{ PROC PRINT_PS
proc print_ps p1 p2
  {
  { Get the dataset and port
  if UNDEFINED(p1)
    input 'Dataset to be displayed?  > ' (data)
  else
    data = p1
  endif
  if UNDEFINED(p2)
    port = 0
  else
    port = INTEGER(p2)
  endif
  {
  { Display the dataset
  p4_display data=(data) port=(port)
  p4_close_dev
  p4_psprint
  p4_rmpsfile
endproc

{
{ PROC PRINT_PSCOL
proc print_pscol p1 p2
  {
  { Get the dataset and port
  if UNDEFINED(p1)
    input 'Dataset to be displayed?  > ' (data)
  else
    data = p1
  endif
  if UNDEFINED(p2)
    port = 0
  else
    port = INTEGER(p2)
  endif
  {
  { Display the dataset
  p4_display data=(data) port=(port)
  p4_close_dev
  p4_psprintc
  p4_rmpsfile
endproc

{
{ PROC IS_LOGICAL
proc is_logical value
  val = UPCASE( SUBSTR( STRING(value), 1, 1 ) )
  if val = '0'
    val = LOGICAL( FALSE )
  else if val = 'F'
    val = LOGICAL( FALSE )
  else if val = 'f'
    val = LOGICAL( FALSE )
  else if val = 'N'
    val = LOGICAL( FALSE )
  else if val = 'n'
    val = LOGICAL( FALSE )
  else if val = '1'
    val = LOGICAL( TRUE )
  else if val = 'T'
    val = LOGICAL( TRUE )
  else if val = 'y'
    val = LOGICAL( TRUE )
  else if val = 'Y'
    val = LOGICAL( TRUE )
  else if val = 'y'
    val = LOGICAL( TRUE )
  else
    val = LOGICAL( FALSE )
  endif
  value = val
endproc

{
{ Invoke it
p4_load
