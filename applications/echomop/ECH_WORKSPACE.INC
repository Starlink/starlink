*  COMMON defining workspace for NAG etc. routines
      REAL work_real1 ( max_points_per_axis )
      REAL work_real2 ( max_points_per_axis )
      REAL*8 work_double1 ( max_points_per_axis )
      REAL*8 work_double2 ( max_points_per_axis )
      INTEGER work_int1 ( max_points_per_axis )
      INTEGER work_int2 ( max_points_per_axis )

      COMMON / workspace / work_int1, work_int2,
     :       work_real1, work_real2

      COMMON /workspace_d/
     :       work_double1, work_double2
