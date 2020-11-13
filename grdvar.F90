module grdvar

  use param
  use cdf

  implicit none

  integer(kind=4), allocatable, dimension(:,:)   :: lsmask
     real(kind=8), allocatable, dimension(:,:)   ::  plat, plon
     real(kind=4), allocatable, dimension(:,:)   :: parea
     real(kind=4), allocatable, dimension(:,:)   :: phole
     real(kind=4), allocatable, dimension(:,:)   :: rmask
     real(kind=4), allocatable, dimension(:,:,:) :: csm

  contains

  !---------------------------------------------------------------------

  subroutine get_grid(cdffile,nhh)

  character(len=*), intent(in) :: cdffile
           integer, intent(in) :: nhh

  rc = nf90_open(trim(cdffile), nf90_nowrite, ncid)
  print *,'grid file ',trim(cdffile)
  if(rc .ne. 0)print *,trim(nf90_strerror(rc))

   rc = nf90_inq_varid(ncid,   'area',   datid)
   rc = nf90_get_var(ncid,      datid,   parea)
   rc = nf90_inq_varid(ncid,    'lon',   datid)
   rc = nf90_get_var(ncid,      datid,    plon)
   rc = nf90_inq_varid(ncid,    'lat',   datid)
   rc = nf90_get_var(ncid,      datid,    plat)

   rc = nf90_inq_varid(ncid, 'lsmask',   datid)
   rc = nf90_get_var(ncid,      datid,  lsmask)
   ! for SH, these the phole mask = 0 everywhere
   rc = nf90_inq_varid(ncid,  'pmask',   datid)
   rc = nf90_get_var(ncid,      datid,   phole)
   ! for NH, use IHOV regions
   if(nhh .eq. 1)vname = 'rmask'
   ! for SH, use GSFC regions
   if(nhh .eq. 2)vname = 'gmask'
   rc = nf90_inq_varid(ncid,  trim(vname),   datid)
   rc = nf90_get_var(ncid,          datid,   rmask)
   rc = nf90_close(ncid)

  end subroutine get_grid
end module grdvar
