module charstrings

  use param

  implicit none

   character(len=120) ::  obssrc="/scratch2/NCEPDEV/climate/Denise.Worthen/"
   
   character(len= 10), dimension(nhemi) :: psrc = (/     'psn',      'pss'/)
   character(len= 10), dimension(nhemi) :: fsrc = (/'_nh_f17_', '_sh_f17_'/)
   character(len= 10), dimension(nhemi) :: hsrc = (/   'north',    'south'/)
   character(len= 10), dimension(nhemi) :: asrc = (/       'N',        'S'/)

   character(len= 30) ::    aisrc="seaice_conc_daily"
   !only cdr is avail in the interim files (2018)
   !character(len= 30) ::   ainame="seaice_conc_cdr"
   !character(len= 30) ::   ainame="goddard_bt_seaice_conc"
   character(len= 30) ::   ainame="goddard_nt_seaice_conc"
   character(len= 30) ::    hisrc="RDEFT4_"
   character(len= 30) ::   hiname="sea_ice_thickness"

   character(len= 80) :: rtname

   character(len=8) :: i2fmt = '(i2.2)'
   character(len=8) :: i4fmt = '(i4.4)'

end module charstrings
