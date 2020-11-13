module cdf

  use param
  use netcdf

  implicit none

  integer, parameter :: ndim1 = 1, ndim2 = 2, ndim3 = 3, ndim4 = 4

  integer, dimension(ndim4) :: dim4, corner4, edge4
  integer, dimension(ndim3) :: dim3, corner3, edge3
  integer, dimension(ndim2) :: dim2, corner2, edge2
  integer, dimension(ndim1) :: dim1, corner1, edge1

  integer :: xtndim, ytndim, zrndim
  integer :: xtsdim, ytsdim, zrsdim
  integer :: yrdim, yhdim, tdim

  integer :: rc, ncid, timid, datid
  integer :: clen

  real(kind=4) :: mval = -9999.

  real(kind=8)                      :: tstamp
  real(kind=8), dimension(2)        :: tsbound
  character(len=31)    :: torg = 'hours since 1991-01-01 00:00:00'
  character(len=31)    :: tcal = 'gregorian'

  real(kind=8), allocatable, dimension(:) :: taxis

  character(len=64)    :: vname,vunit,vlong,fname
  contains

!---------------------------------------------------------------------

  subroutine set_taxis(ytmp,mtmp,dtmp)

  implicit none
  real*8 tm_secs_from_bc

  integer, intent(in) :: ytmp, mtmp, dtmp

  real(kind = 8) :: tday

  tday = tm_secs_from_bc(ytmp, mtmp, dtmp,  &
                           12,    0,    0) - &
         tm_secs_from_bc(1991,    1,    1,   &
                            0,    0,    0)
    tday = tday/(60.d0*60.d0)
  tstamp = tday

  tsbound(1) = tstamp - 12.0
  tsbound(2) = tstamp + 12.0
  end subroutine set_taxis

!---------------------------------------------------------------------

  subroutine get_pfld(cdffile,varname,aout)

  implicit none

                 character(len=*), intent( in) :: cdffile
                 character(len=*), intent( in) :: varname
       real, dimension(iiobs,jjobs), intent(out) :: aout

  integer(kind=1), dimension(iiobs,jjobs,1) :: i3d
     real(kind=4), dimension(iiobs,jjobs)   :: a2d

  integer :: j,xtype

  aout = mval
    rc = nf90_open(trim(cdffile), nf90_nowrite, ncid)
  !if(rc .ne. 0)print *,'looking for file ',trim(cdffile)
  if(rc .ne. 0)then
   rc = nf90_close(ncid)
   return
  endif

  rc = nf90_inq_varid(ncid, trim(varname), datid)
  if(rc .ne. 0)then
   rc = nf90_close(ncid)
   return
  endif

  rc = nf90_inq_varid(ncid, trim(varname), datid)
  rc = nf90_inquire_variable(ncid, datid, xtype = xtype)
  !print *,trim(cdffile),'   ',xtype,'  ',trim(varname)

  ! byte, with time dimension
  if(xtype .eq. 1)then
   rc = nf90_get_var(ncid, datid,  i3d)
   !print *,nf90_strerror(rc)
   aout(:,:) = 0.01*float(i3d(:,:,1))
  else
  ! float, no time dimension
   rc = nf90_get_var(ncid, datid,  a2d)
   aout = a2d
  endif
   rc = nf90_close(ncid)

        a2d = aout
  do j = 1,jjobs
   aout(:,j) = a2d(:,jjobs-(j-1))
  enddo

  where(aout .lt. 0.0)aout = mval

  end subroutine get_pfld
end module cdf
