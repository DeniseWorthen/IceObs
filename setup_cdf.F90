subroutine setup_cdf(cdffile)

  use param
  use stats
  use variablelist
  use netcdf
  use charstrings
  use cdf

  implicit none

                       character(len=*), intent(in) :: cdffile

  !-----------------------------------------------------------------------------

            integer :: n,nlast
  character(len=20) :: varname, varunit, varlong

  !-----------------------------------------------------------------------------

   rc = nf90_create(trim(cdffile), nf90_clobber, ncid)
   !print *,'setting up ',trim(cdffile)

   rc = nf90_def_dim(ncid,    'Xtn',       iobs(1),    xtndim)
   rc = nf90_def_dim(ncid,    'Ytn',       jobs(1),    ytndim)
   rc = nf90_def_dim(ncid,    'Zrn',      nregs(1),    zrndim)

   rc = nf90_def_dim(ncid,    'Xts',       iobs(2),    xtsdim)
   rc = nf90_def_dim(ncid,    'Yts',       jobs(2),    ytsdim)
   rc = nf90_def_dim(ncid,    'Zrs',      nregs(2),    zrsdim)

   rc = nf90_def_dim(ncid,    'Yh',          nhemi,     yhdim)
   rc = nf90_def_dim(ncid,    'Yr',           nreg,     yrdim)
   rc = nf90_def_dim(ncid,  'time', nf90_unlimited,      tdim)

   dim1(1) =  tdim
   rc = nf90_def_var(ncid, 'time', nf90_double,  dim1,      timid)
   rc = nf90_put_att( ncid, timid,            'units', trim(torg))
   rc = nf90_put_att( ncid, timid,         'calendar', trim(tcal))
   rc = nf90_put_att( ncid, timid,             'axis',        'T')

    dim3(3) = zrndim
    dim3(2) = ytndim
    dim3(1) = xtndim
          n = 1
    varname = icefields(n)%field_name
    varunit = icefields(n)%unit_name
    varlong = icefields(n)%long_name
    rc = nf90_def_var(ncid, trim(varname), nf90_float, dim3, datid)
    !print *,trim(nf90_strerror(rc)),'    ',trim(varname)
    rc = nf90_put_att(ncid, datid,     'units', trim(varunit))
    rc = nf90_put_att(ncid, datid, 'long_name', trim(varlong))
    rc = nf90_put_att(ncid, datid, 'missing_value', mval)
    rc = nf90_put_att(ncid, datid,    '_FillValue', mval)

    dim3(3) = zrsdim
    dim3(2) = ytsdim
    dim3(1) = xtsdim
          n = 2
    varname = icefields(n)%field_name
    varunit = icefields(n)%unit_name
    varlong = icefields(n)%long_name
    rc = nf90_def_var(ncid, trim(varname), nf90_float, dim3, datid)
    !print *,trim(nf90_strerror(rc)),'    ',trim(varname)
    rc = nf90_put_att(ncid, datid,     'units', trim(varunit))
    rc = nf90_put_att(ncid, datid, 'long_name', trim(varlong))
    rc = nf90_put_att(ncid, datid, 'missing_value', mval)
    rc = nf90_put_att(ncid, datid,    '_FillValue', mval)

   dim2(2) =  tdim
   dim2(1) = yhdim
          n = 3
    varname = icefields(n)%field_name
    varunit = icefields(n)%unit_name
    varlong = icefields(n)%long_name
    rc = nf90_def_var(ncid, trim(varname), nf90_float, dim2, datid)
    !print *,trim(nf90_strerror(rc)),'    ',trim(varname)
    rc = nf90_put_att(ncid, datid,     'units', trim(varunit))
    rc = nf90_put_att(ncid, datid, 'long_name', trim(varlong))
    rc = nf90_put_att(ncid, datid, 'missing_value', mval)
    rc = nf90_put_att(ncid, datid,    '_FillValue', mval)

   dim2(2) =  tdim
   dim2(1) = yrdim
   do n = 4,nvars
    varname = icefields(n)%field_name
    varunit = icefields(n)%unit_name
    varlong = icefields(n)%long_name
    rc = nf90_def_var(ncid, trim(varname), nf90_float, dim2, datid)
    rc = nf90_put_att(ncid, datid,     'units', trim(varunit))
    rc = nf90_put_att(ncid, datid, 'long_name', trim(varlong))
    rc = nf90_put_att(ncid, datid, 'missing_value', mval)
    rc = nf90_put_att(ncid, datid,    '_FillValue', mval)
   enddo

    rc = nf90_enddef(ncid)
    rc = nf90_close(ncid)

end subroutine setup_cdf
