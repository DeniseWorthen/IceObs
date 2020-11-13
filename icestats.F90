program icestats

  use param
  use runparams
  use cdf
  use variablelist
  use nsidc
  use stats
  use grdvar
  use charstrings
  use netcdf

  implicit none

!---------------------------------------------------------------------
! local variables
!---------------------------------------------------------------------

  character(len=240) :: cdffile, outcdf

  integer :: i,j,n,nh,nr,nrr,nt
  integer :: year, mon, day, lll
  integer :: nr1

  character(len= 4) :: cyear
  character(len= 2) ::  cmon, cday
  character(len=10) :: cdate

     real(kind=4) :: rnum, area, extent, mvar

  real, allocatable, dimension(:,:) :: ai, hi, aifill

  real, dimension(iobs(1),jobs(1),nregs(1)) :: csm1
  real, dimension(iobs(2),jobs(2),nregs(2)) :: csm2

  !---------------------------------------------------------------------

   call setup_runs
   call def_vartypes

   print *,nsteps
   do nr = 1,nregs(1)
    print *,nr,nr,trim(npnames(nr))
   enddo
    print *
   do nr = 1,nregs(2)
    print *,nr,nr+nregs(1),trim(spnames(nr))
   enddo
  !stop

  allocate(exobs(1:nreg,1:nsteps))
  allocate(arobs(1:nreg,1:nsteps))
  allocate(hiobs(1:nreg,1:nsteps))
  exobs = mval; arobs = mval; hiobs = mval

  !---------------------------------------------------------------------

  call read_nsidc

  !---------------------------------------------------------------------

  !nh = 1
  do nh = 1,nhemi
   iiobs = iobs(nh); jjobs = jobs(nh); nnreg = nregs(nh)
                nr1 = 0
   if(nh .eq. 2)nr1 = nregs(1)

   allocate(lsmask(1:iiobs,1:jjobs))
   allocate(  plat(1:iiobs,1:jjobs))
   allocate(  plon(1:iiobs,1:jjobs))
   allocate( parea(1:iiobs,1:jjobs))
   allocate( phole(1:iiobs,1:jjobs))
   allocate( rmask(1:iiobs,1:jjobs))
   allocate(   csm(1:iiobs,1:jjobs,1:nnreg))

   allocate(    hi(1:iiobs,1:jjobs))
   allocate(    ai(1:iiobs,1:jjobs))
   allocate(aifill(1:iiobs,1:jjobs))

  !---------------------------------------------------------------------

   ! get the model grid variables
   cdffile = trim(obssrc)//'IceData/'//trim(psrc(nh))//'25.grid.nc'
   call get_grid(trim(cdffile),nh)

   outcdf = trim(obssrc)//'IceData/iceobs_stats.'//trim(ainame)//'.nc'
   !print *,'working on ',trim(outcdf)
   call setup_cdf(trim(outcdf))

   ! set up the region mask
    csm = 0.0
   do j = 1,jjobs
    do i = 1,iiobs
     do nr = 1,nnreg
      if(rmask(i,j) .eq. real(nr,4))csm(i,j,nr) = real(nr,4)
     enddo
    enddo
   enddo

   ! make special regions
   do j = 1,jjobs
    do i = 1,iiobs
     if((nh .eq. 1) .and. &
         (plat(i,j) .ge.  50.0))csm(i,j,nnreg) = float(nnreg)
     if((nh .eq. 2) .and. &
         (plat(i,j) .le. -50.0))csm(i,j,nnreg) = float(nnreg)
    enddo
   enddo
   print *,minval(csm),maxval(csm)
   print *,size(csm,1),size(csm,2),size(csm,3)

   do nr = 1,nnreg
    where(lsmask(:,:) .eq. 0)csm(:,:,nr) = mval
   enddo
   
   if(nh .eq. 1)csm1 = csm
   if(nh .eq. 2)csm2 = csm
   !this does not write psn,only pss: WHY?
   !write the csm
   !vname = 'csm_'//trim(psrc(nh))
   !print *,'writing ',trim(vname),' to ',trim(outcdf)

   !rc = nf90_open(trim(outcdf), nf90_write,  ncid)
   !rc = nf90_inq_varid(ncid, trim(vname), datid)
   !rc = nf90_put_var(ncid,         datid,   csm)
   !rc = nf90_close(ncid)

  !---------------------------------------------------------------------

   lll = 0
  do nt = 1,nsteps
  !do nt = 1,30
    nrr = nr1
    lll = lll + 1
   year = ymd(1,nt)
    mon = ymd(2,nt)
    day = ymd(3,nt)

   write(cyear, i4fmt)year
   write( cmon, i2fmt)mon
   write( cday, i2fmt)day
   cdate = trim(cyear)//trim(cmon)//trim(cday)
   if(mod(nt,30) .eq. 0)print *,'working on ',trim(cdate)
   !print *,trim(cdate),lll,nh

   ! hiobs
   hi = mval
   if(nh .eq. 1)then
    cdffile = trim(obssrc)//'IceSat/'//trim(cyear)//'/'//trim(hisrc)//trim(cdate)//'.nc'
    call get_pfld(trim(cdffile),     trim(hiname),      hi)
   end if
   ! aiobs
   cdffile = trim(obssrc)//'IceData/'//trim(hsrc(nh))//'/'//trim(cyear)//'/'//trim(aisrc)//trim(fsrc(nh))//trim(cdate)//'_v03r01.nc'
   call get_pfld(trim(cdffile),     trim(ainame),      ai)
   
   ! create ice field filled in at pole hole
                                               aifill = ai
   where((ai .eq. mval) .and. (phole .gt. 0.0))aifill = 1.0

    do nr = 1,nnreg
      nrr = nrr+1
     rnum = float(nr)
     call areaextent(rnum, ai,aifill,area,extent)

     arobs(nrr,lll) =   area
     exobs(nrr,lll) = extent

     call    meanvar(rnum, hi,ai,aifill,mvar,0.70)
     hiobs(nrr,lll) = mvar
     !print *,cdate,nh,nr,nrr,mvar,hiobs(nrr,lll)
    enddo !nr
  
    ! write the time values
    call set_taxis(year,mon,day)
    rc = nf90_open(trim(outcdf), nf90_write, ncid)

    corner1(1) = lll
      edge1(1) = 1
    rc = nf90_inq_varid(ncid, 'time',  timid)
    rc = nf90_put_var(ncid, timid, tstamp, corner1)
    rc = nf90_close(ncid)

   !testing
   if(nh.eq. 1)call write_obs(trim(obssrc)//'IceData/hi.nc','hi',hi,lll)
   if(nh.eq. 1)call write_obs(trim(obssrc)//'IceData/ai.nc','ai',ai,lll)
   enddo !nt

   deallocate(lsmask,plat,plon,parea,rmask,csm,phole)
   deallocate(ai,hi,aifill)
  enddo !nh

  !---------------------------------------------------------------------

   !write values

   rc = nf90_open(trim(outcdf), nf90_write,  ncid)
  
   vname = 'csm_psn'
   print *,'writing ',trim(vname),' to ',trim(outcdf)
   rc = nf90_inq_varid(ncid, trim(vname), datid)
   rc = nf90_put_var(ncid,         datid,  csm1)

   vname = 'csm_pss'
   print *,'writing ',trim(vname),' to ',trim(outcdf)
   rc = nf90_inq_varid(ncid, trim(vname), datid)
   rc = nf90_put_var(ncid,         datid,  csm2)
  
   vname = 'exnsidc'
   print *,'writing ',trim(vname),' to ',trim(outcdf)
   rc = nf90_inq_varid(ncid, trim(vname),   datid)
   rc = nf90_put_var(ncid,         datid, exnsidc)

   vname = 'exobs'
   print *,'writing ',trim(vname),' to ',trim(outcdf)
   rc = nf90_inq_varid(ncid, trim(vname),   datid)
   rc = nf90_put_var(ncid,         datid,   exobs)

   vname = 'arobs'
   print *,'writing ',trim(vname),' to ',trim(outcdf)
   rc = nf90_inq_varid(ncid, trim(vname),   datid)
   rc = nf90_put_var(ncid,         datid,   arobs)

   vname = 'hiobs'
   print *,'writing ',trim(vname),' to ',trim(outcdf)
   rc = nf90_inq_varid(ncid, trim(vname),   datid)
   rc = nf90_put_var(ncid,         datid,   hiobs)
   rc = nf90_close(ncid)

   print *
  deallocate(exnsidc)

end program icestats
