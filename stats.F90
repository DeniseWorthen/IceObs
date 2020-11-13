module stats

  use param
  use grdvar
 
  implicit none

  real, allocatable, dimension(:,:)         :: exobs, arobs
  real, allocatable, dimension(:,:)         :: hiobs

  contains
  !---------------------------------------------------------------------
  !NSIDC v3.0
  !The area value is always less than the extent value because extent
  !includes the entire expanse within the ice edge and includes the area
  !under the Arctic Pole Hole, while area takes ice concentration within
  !that edge into account and excludes the area under the Arctic pole hole.

   subroutine areaextent(reg,ain,afill,asum,esum)

       real, intent( in) :: reg
       real, intent( in), dimension(iiobs,jjobs) :: ain,afill
       real, intent(out) :: asum, esum

    integer :: nr,i,j

   ! parea is in km2
      nr = int(reg)
    asum = 0.0; esum = 0.0
    do j = 1,jjobs
     do i = 1,iiobs
      if((csm(i,j,nr) .eq.  reg) .and. &
          (  ain(i,j)  .ge. 0.15))asum = asum + parea(i,j)*ain(i,j)
      if((csm(i,j,nr) .eq.  reg) .and. &
          (afill(i,j)  .ge. 0.15))esum = esum + parea(i,j)
     enddo !i
    enddo !j
    if(asum .eq. 0.0)asum = mval
    if(esum .eq. 0.0)esum = mval
    ! areas in km2
    if(asum .ne. 0.0)asum = asum/1.0e6
    if(esum .ne. 0.0)esum = esum/1.0e6

   end subroutine areaextent

  !---------------------------------------------------------------------

   subroutine meanvar(reg,avar,ai,afill,amean,aimin)

    real, intent( in) :: reg,aimin
    real, intent( in), dimension(iiobs,jjobs) :: avar, ai, afill
    real, intent(out) :: amean

    integer :: nr,i,j
       real :: asum, acnt

   real, dimension(iiobs,jjobs) :: atmp
   ! use actual ai (let the pole hole exist)
   !atmp = ai
   ! use filled ai (fill in the pole hole w/ 100% ice)
   atmp = afill

    ! sum(hi*ai*area)/sum(ai*area) = total ice vol/total ice area
    ! => mean ice thick
       nr = int(reg)
    amean = mval
     asum = 0.0; acnt = 0.0
    do j = 1,jjobs
     do i = 1,iiobs
      if((csm(i,j,nr) .eq.  reg) .and. &
          (avar(i,j)   .ne. mval) .and. &
           (atmp(i,j)    .ge. aimin))then
        asum = asum +  avar(i,j)*atmp(i,j)*parea(i,j)
        acnt = acnt +            atmp(i,j)*parea(i,j)
       ! asum = asum +  avar(i,j)*parea(i,j)
       ! acnt = acnt +            parea(i,j)
       !print *,j,avar(i,j),atmp(i,j),asum,acnt
      endif
     enddo !i
    enddo !j
    if(acnt .ne. 0.0)amean = asum/acnt

   end subroutine meanvar
end module stats
