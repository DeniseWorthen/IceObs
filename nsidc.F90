module nsidc

  use param
  use charstrings
  use runparams
  use cdf

  implicit none

  real, allocatable, dimension(:,:) :: exnsidc

  contains

!---------------------------------------------------------------------

  subroutine read_nsidc
  
   character(len=100) :: filename
   character(len=200) :: chead
   character(len=200) :: cstring

   integer :: maxlines = 42*366
   integer :: nt,nl,iyr, imon, iday, lll
      real :: exval, exmiss

   real(kind=8) :: tstamp0, tval

   !---------------------------------------------------------------------

   allocate(exnsidc(1:nhemi,1:nsteps))

   call set_taxis(yrbeg,1,1)
   tstamp0 = tstamp

   !---------------------------------------------------------------------

    exnsidc = mval

   filename = trim(obssrc)//'IceData/N_seaice_extent_daily_v3.0.csv'
   open(unit=20,file=trim(filename),status='old',form='formatted')
   read(20,'(a200)')chead
   !print *,trim(chead)
   read(20,'(a200)')chead
   !print *,trim(chead)

     lll = 0
   do nl = 1,maxlines
    read(20,*,end=99)iyr, imon, iday, exval, exmiss,cstring
    call set_taxis(iyr,imon,iday)
    tval = tstamp
     lll = 1+int(tval - tstamp0)/24
    if(lll .ge. 1 .and. lll .le. nsteps)exnsidc(1,lll) = exval
    !print *,iyr,imon,iday,tval-tstamp0,lll
   enddo
99 continue
   close(20)

   filename = trim(obssrc)//'IceData/S_seaice_extent_daily_v3.0.csv'
   open(unit=20,file=trim(filename),status='old',form='formatted')
   read(20,'(a200)')chead
   !print *,trim(chead)
   read(20,'(a200)')chead
   !print *,trim(chead)

     lll = 0
   do nl = 1,maxlines
    read(20,*,end=88)iyr, imon, iday, exval, exmiss,cstring
    call set_taxis(iyr,imon,iday)
    tval = tstamp
     lll = 1+int(tval - tstamp0)/24
    if(lll .ge. 1 .and. lll .le. nsteps)exnsidc(2,lll) = exval
    !print *,iyr,imon,iday,tval-tstamp0,lll
   enddo
88 continue

   !do nt = 1,nsteps
   ! print *,ymd(:,nt),exnsidc(:,nt)
   !enddo

  end subroutine read_nsidc
end  module nsidc
