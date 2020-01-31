c	Read geographic coordinate information
c	It is head file of DEM describing coordinate projection information
c	Use it to convert the coordinate of the projection to Longitude and latitude
	subroutine Read_GEO_hrd(name,nr,nc,x0,y0,s)
	implicit none
      integer nc,nr
	character*80  name
	character*5 ncols,nrows
      character*9 xllcorner,yllcorner
      character*8 cellsize
      real x0,y0,s
	open(1, file= name ,status='old',err=9001)
		read(1,'(A, i)') ncols,nc
		read(1,'(A, i)') nrows,nr
		read(1,'(A, f)') xllcorner,x0
		read(1,'(A, f)') yllcorner,y0
		read(1,'(A, f15.0)') cellsize,s
      close (1)
	Return
9001	PRINT *, 'ERROR IN READ GEOGRAPHY FILE: ', name
	RETURN
	end
