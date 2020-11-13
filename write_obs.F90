subroutine write_obs(cdffile,obsname,ain,lstep)

  use param
  use grdvar
  use netcdf
  use charstrings
  use cdf

  implicit none

                     character(len=*), intent(in) :: cdffile
                     character(len=*), intent(in) :: obsname
         real, dimension(iiobs,jjobs), intent(in) :: ain
                              integer, intent(in) :: lstep

  !-----------------------------------------------------------------------------

            integer :: n
  character(len=64) :: varname, varunit, varlong
  character(len=64) :: history

  !-----------------------------------------------------------------------------

  if(lstep .eq. 1)then
   history = trim(ainame)

   rc = nf90_create(trim(cdffile), nf90_clobber, ncid)

   rc = nf90_def_dim(ncid,    'Xt',           iiobs,   xtndim)
   rc = nf90_def_dim(ncid,    'Yt',           jjobs,   ytndim)
   rc = nf90_def_dim(ncid,  'time', nf90_unlimited,      tdim)

   dim1(1) =  tdim
   rc = nf90_def_var(ncid, 'time', nf90_double,  dim1,      timid)
   rc = nf90_put_att( ncid, timid,            'units', trim(torg))
   rc = nf90_put_att( ncid, timid,         'calendar', trim(tcal))
   rc = nf90_put_att( ncid, timid,             'axis',        'T')

   dim3(3) =   tdim
   dim3(2) = ytndim
   dim3(1) = xtndim
    varname = trim(obsname)
    rc = nf90_def_var(ncid, trim(varname), nf90_float, dim3, datid)
    rc = nf90_put_att(ncid, datid, 'missing_value', mval)
    rc = nf90_put_att(ncid, datid,    '_FillValue', mval)
    rc = nf90_enddef(ncid)

    rc = nf90_close(ncid)
  endif

  !-----------------------------------------------------------------------------

  rc = nf90_open(trim(cdffile), nf90_write, ncid)

   corner1(1) = lstep
     edge1(1) = 1
   rc = nf90_inq_varid(ncid, 'time',  timid)
   rc = nf90_put_var(ncid, timid, tstamp, corner1)

   corner3(3) = lstep
   corner3(2) = 1
   corner3(1) = 1
     edge3(3) = 1
     edge3(2) = jjobs
     edge3(1) = iiobs
   rc = nf90_inq_varid(ncid,       trim(obsname), datid)
   rc = nf90_put_var(ncid, datid,   ain, corner3, edge3)

   rc = nf90_close(ncid)

end subroutine write_obs
