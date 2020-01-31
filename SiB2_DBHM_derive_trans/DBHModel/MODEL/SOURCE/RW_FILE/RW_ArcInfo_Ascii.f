ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c	Read Write ascii file, the file format is ArcInfo ascii file format 
c	Input: 
c		name:			filename of the read/write file
c		nr:				nrows
c		nc:				ncols
c		nnr,nnc:		the size of input/output array
c		x0:				xllcorner	
c		y0:				yllcorner
c 		s:				cellsize
c		znodata:		NODATA_value
c		array(/write):	the array will be written into the file			
c	Output(/read):
c		array:			the file data will be read into the array
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

	subroutine readfile_int(name,nr,nc,x0,y0,s,znodata,array,nnr,nnc)
	implicit none
      integer znodata,nc,nr
	integer nnr,nnc
	character*80  name
	integer array(nnr,nnc)
	character*14 ncols,nrows
      character*14 xllcorner,yllcorner
      character*14 cellsize
      character*14 nodata
      real x0,y0,s
	integer i,j

	open(1, file= name ,status='old')
      read(1,'(A, i)') ncols,nc
      read(1,'(A, i)') nrows,nr
      read(1,'(A, f)') xllcorner,x0
      read(1,'(A, f)') yllcorner,y0
      read(1,'(A, f15.0)') cellsize,s
      read(1,'(A, i)') nodata,znodata    
      do i=1,nr        
      	read(1,*) (array(i,j), j=1,nc)
      end do          
      close (1)
	end

	subroutine writefile_int(name,nr,nc,x0,y0,s,znodata,array,nnr,nnc)
	implicit none
      integer znodata,nc,nr
	integer nnr,nnc
	character*80  name
	integer array(nnr,nnc)
      real x0,y0,s
	integer i,j
	
      open(1,file=name,status='unknown')
      write(1,'(A, i)') 'ncols',nc
      write(1,'(A, i)') 'nrows',nr
      write(1,'(A, f15.3)') 'xllcorner',x0
      write(1,'(A, f15.3)') 'yllcorner',y0
      write(1,'(A, f)') 'cellsize',s
      write(1,'(A, i15)') 'NODATA_value',znodata
      do i=1,nr
      	do j=1,nc
      		write (1,'(i,$)') array(i,j)
      	end do
		write (1,*)
      end do
      close(1)
	end

	subroutine readfile_float(name,nr,nc,x0,y0,s,znodata,array,nnr,nnc)
	implicit none
      integer nc,nr
	integer nnr,nnc
	character*80  name
	real*4 array(nnr,nnc),znodata
	character*5 ncols,nrows
      character*9 xllcorner,yllcorner
      character*14 cellsize
      character*12 nodata
      real x0,y0,s
	integer i,j

	open(1, file= name ,status='old')
      read(1,'(A, i)') ncols,nc
      read(1,'(A, i)') nrows,nr
      read(1,'(A, f)') xllcorner,x0
      read(1,'(A, f)') yllcorner,y0
      read(1,'(A, f15.0)') cellsize,s
      read(1,'(A, f15.0)') nodata,znodata  
      do i=1,nr        
      	read(1,*) (array(i,j), j=1,nc)
      end do          
      close (1)
	end

	subroutine writefile_float(name,nr,nc,x0,y0,s,znodata,array,nnr,nnc)
	implicit none
      integer nc,nr
	integer nnr,nnc
	character*80  name
	real array(nnr,nnc), znodata
      real x0,y0,s
	integer i,j

      open(1,file=name,status='unknown')
      write(1,'(A, i)') 'ncols',nc
      write(1,'(A, i)') 'nrows',nr
      write(1,'(A, f15.3)') 'xllcorner',x0
      write(1,'(A, f15.3)') 'yllcorner',y0
      write(1,'(A, f)') 'cellsize',s
      write(1,'(A, f)') 'NODATA_value',znodata
      do i=1,nr
      	do j=1,nc
      		write (1,'(G,$)') array(i,j)
      	end do
		write (1,*)
      end do
      close(1)
	end

	subroutine wr_fb(name,nr,nc,x0,y0,s,znodata,array,nnr,nnc)
	implicit none
      integer nc,nr
	integer nnr,nnc
	character*80  name
	real array(nnr,nnc), znodata
      real x0,y0,s
	integer i,j,isz

	isz=nnr*nnc*4
      open(1,form='unformatted',file=name,status='unknown',
     $	access='direct',recl=isz)
		write(1,rec=1) array
      close(1)
	end

	subroutine rd_fb(name,nr,nc,x0,y0,s,znodata,array,nnr,nnc)
	implicit none
      integer nc,nr
	integer nnr,nnc
	character*80  name
	real array(nnr,nnc), znodata
      real x0,y0,s
	integer i,j,isz

	isz=nnr*nnc*4
      open(1,form='unformatted',file=name,status='unknown',
     $	access='direct',recl=isz)
		read(1,rec=1) array
      close(1)
	end