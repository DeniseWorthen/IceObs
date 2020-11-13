module variablelist

  use param
  use cdf

  implicit none

  type IceFieldDefs
    character(len=20)                           :: field_name
    character(len=20)                           ::  long_name
    character(len=20)                           ::  unit_name
  end type IceFieldDefs

  type(IceFieldDefs) :: icefields(nvars)

  contains

  !---------------------------------------------------------------------

  subroutine def_vartypes

            integer :: i,ii

  ii = 0

  ii = ii + 1
  icefields(ii)%field_name    = 'csm_psn'
  icefields(ii)%long_name     = 'reg mask NH'
  icefields(ii)%unit_name     = ' '

  ii = ii + 1
  icefields(ii)%field_name    = 'csm_pss'
  icefields(ii)%long_name     = 'reg mask SH'
  icefields(ii)%unit_name     = ' '

  ii = ii + 1
  icefields(ii)%field_name    = 'exnsidc'
  icefields(ii)%long_name     = 'NSIDC Obs ice extent'
  icefields(ii)%unit_name     = '10^6 km2'

  ii = ii + 1
  icefields(ii)%field_name    = 'exobs'
  icefields(ii)%long_name     = 'Obs. ice extent'
  icefields(ii)%unit_name     = '10^6 km2'

  ii = ii + 1
  icefields(ii)%field_name    = 'arobs'
  icefields(ii)%long_name     = 'Obs ice area'
  icefields(ii)%unit_name     = '10^6 km2'

  ii = ii + 1
  icefields(ii)%field_name    = 'hiobs'
  icefields(ii)%long_name     = 'Obs ice thickness'
  icefields(ii)%unit_name     = 'm'

  do i = 1,ii
   print *,i,icefields(i)%field_name
  enddo
  end subroutine def_vartypes
end module variablelist
