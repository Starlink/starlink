/* sc2store_struct.h - structure for scuba2 header storage

   History :
    17Aug2004 : original (bdk)
    17Feb2005 : add sc2_heat (bdk)
*/


/* Structure containing per-frame header items */


struct sc2head
{
   float fts_pos;
   double pol_ang;
   int rts_num;
   double rts_step;
   double rts_end;
   char rts_tasks[81];
   char rts_errs[81];
   double sc2_heat;
   double smu_az_off_x;
   double smu_az_off_y;
   double smu_x;
   double smu_y;
   double smu_z;
   double smu_tr_off_x;
   double smu_tr_off_y;
   double tcs_airmass;
   char tcs_az_sys[17];
   double tcs_az_ang;
   double tcs_az_ac1;
   double tcs_az_ac2;
   double tcs_az_dc1;
   double tcs_az_dc2;
   double tcs_az_bc1;
   double tcs_az_bc2;
   int tcs_index;
   char tcs_source[33];
   char tcs_tr_sys[17];
   double tcs_tr_ang;
   double tcs_tr_ac1;
   double tcs_tr_ac2;
   double tcs_tr_dc1;
   double tcs_tr_dc2;
   double tcs_tr_bc1;
   double tcs_tr_bc2;
   float wvm_th;
   float wvm_t12;
   float wvm_t42;
   float wvm_t78;
   float wvm_tw;
   int wvm_qual;
   float wvm_time;
};


