module runparams

  use param
  use caldata

  implicit none

  integer, dimension(3,maxsteps) :: ymd

  integer :: nsteps
  integer :: mnend(nmon)
  integer :: lbeg, lend

  contains

  subroutine setup_runs

  integer :: ny, nm, nd
  integer :: lll, nex, year, mon, day

  !---------------------------------------------------------------------

      lll = 0
      ymd = 0
    do ny = 1,nyears
     year = yrbeg + (ny-1)
                           mnend = mnendn
     if(mod(year,4) .eq. 0)mnend = mnendl

     do nm = 1,nmon
      do nd = 1, mnend(nm)
              lll = lll +1
       ymd(1,lll) = year
       ymd(2,lll) = nm
       ymd(3,lll) = nd
 
       if((year .eq. 2011) .and. &
           ( nm .eq.    4) .and. &
            ( nd  .eq.   1))lbeg = lll

       if((year .eq. 2018) .and. &
           ( nm  .eq.   3) .and. &
            ( nd  .eq.  15))lend = lll + (ndays-1)
      enddo
     enddo
   enddo!ny
   nsteps = lll
   print *,'lbeg,lend = ',lbeg, lend

  end subroutine setup_runs
end module runparams
