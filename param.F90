module param

  implicit none
 
! ice obs on n/s polar stereographic grids
  integer, parameter :: nhemi =  2
  integer, parameter, dimension(nhemi) :: iobs = (/304, 316/)
  integer, parameter, dimension(nhemi) :: jobs = (/448, 332/)

  integer, parameter ::    yrbeg = 2013, yrend = 2015
  !integer, parameter ::    yrbeg = 2011, yrend = 2012
  integer, parameter ::   nyears = (yrend-yrbeg)+1
  integer, parameter ::     nmon = 12
  integer, parameter ::    ndays = 35
  !integer, parameter ::    ndays = 5
  integer, parameter ::  maxdays = 366
  integer, parameter :: maxsteps = nyears*maxdays
  integer, parameter ::  maxexps = 2*nmon*nyears

  integer, parameter, dimension(nhemi) :: nregs = (/15, 6/)
  integer, parameter :: nreg = nregs(1) + nregs(2)

  character(len=18), dimension(nregs(1)) :: npnames = (/ "Gulf of Alaska", &
                                                         "Bering Sea    ", &
                                                         "Chukchi Sea   ", &
                                                         "Beaufort Sea  ", &
                                                         "Baffin Bay    ", &
                                                         "Lincoln Sea   ", &
                                                         "White Sea     ", &
                                                         "EastSib Sea   ", &
                                                         "NW Passages   ", &
                                                         "Central Arctic", &
                                                         "Barents Sea   ", &
                                                         "Greenland Sea ", &
                                                         "Kara Sea      ", &
                                                         "Laptev Sea    ", &
                                                         "Nocn >50N     "  &
                                                     /)

  character(len=18), dimension(nregs(2)) :: spnames =  (/"Weddell Sea      ", & !20E:60W
                                                         "Indian Ocean     ", & !90E:20E
                                                         "Pacific Ocean    ", & !160E:90E
                                                         "Ross Sea         ", & !130W:160E
                                                         "Belling-Amund Sea", & !60W:130W
                                                         "Socn <50S        "  &
                                                       /)

  ! SH has no ice thickness, but again make space for ease of plotting
  integer :: iiobs, jjobs, nnreg
  integer, parameter :: nvars = 2   &  !region masks
                              + 3   &  !variables
                              + 1      !nsidc extent

end module param
