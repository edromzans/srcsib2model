c	Get mean value of the Arcinfo Ascii file
c	name:	the Arcinfo file
c	area:	the area file of each grid
c	mask:	the mask file
c	nnr,nnc:max_nc,max_nc
c	fave:	averaged value in the Arcinfo (within maskfile)
c	fmin:	min value
c	fmax:	max value

	subroutine ana_float(name,area,mask,nnr,nnc,fave,inum,fmin,fmax)
	implicit none
	include '../INCLUDE/common.inc'
      integer nc,nr
	integer nnr,nnc
	real dataf(max_nr,max_nc),areaf(max_nr,max_nc),maskf(max_nr,max_nc)
	real	znodata
	character*80  name,area,mask
	character*5 ncols,nrows
      character*9 xllcorner,yllcorner
      character*14 cellsize
      character*12 nodata
      real*4 x0,y0,s
	integer i,j,inum
	real	adatasum, areasum,fave,fmin,fmax

	open(1, file= name ,status='old')
      read(1,'(A, i)') ncols,nc
      read(1,'(A, i)') nrows,nr
      read(1,'(A, f)') xllcorner,x0
      read(1,'(A, f)') yllcorner,y0
      read(1,'(A, f15.0)') cellsize,s
      read(1,'(A, f15.0)') nodata,znodata  
      do i=1,nr        
      	read(1,*) (dataf(i,j), j=1,nc)
      end do          
      close (1)
	open(1, file= area ,status='old')
      read(1,'(A, i)') ncols,nc
      read(1,'(A, i)') nrows,nr
      read(1,'(A, f)') xllcorner,x0
      read(1,'(A, f)') yllcorner,y0
      read(1,'(A, f15.0)') cellsize,s
      read(1,'(A, f15.0)') nodata,znodata  
      do i=1,nr        
      	read(1,*) (areaf(i,j), j=1,nc)
      end do          
      close (1)
	open(1, file= mask ,status='old')
      read(1,'(A, i)') ncols,nc
      read(1,'(A, i)') nrows,nr
      read(1,'(A, f)') xllcorner,x0
      read(1,'(A, f)') yllcorner,y0
      read(1,'(A, f15.0)') cellsize,s
      read(1,'(A, f15.0)') nodata,znodata  
      do i=1,nr        
      	read(1,*) (maskf(i,j), j=1,nc)
      end do          
      close (1)

	adatasum=0.0
	areasum= 0.0
	inum=0
	do i=1, nr
		do j =1, nc
			if (maskf(i,j).ne.znodata) then
				inum= inum+1
				if (inum.eq.1) then
					fmin = dataf(i,j)
					fmax = dataf(i,j)
				endif		
				fmin = min(fmin, dataf(i,j))	
				fmax = max(fmax, dataf(i,j))		
				adatasum=adatasum+ areaf(i,j)*dataf(i,j)
				areasum = areasum+ areaf(i,j)
			endif
		enddo
	enddo
	fave =adatasum / areasum

	end

c	Get mean value of FROM matrix
c	dataf: INPUT MATRIX	
c	areaf:	grida area 
c	fracf:	fraction in the grid area
c	maskf:	the mask 
c	nnr,nnc:max_nc,max_nc
c	fave:	averaged value in the Arcinfo (within maskfile)
c	fmin:	min value
c	fmax:	max value
	subroutine ana_DATAF(dataf,areaf1,fracf1,maskf,nnr,
     $	nnc,fave,fmin,fmax,areasum)
	implicit none
      integer nnr,nnc !nc,nr
	real	fracf1(nnr,nnc),areaf1(nnr,nnc)
	real	dataf(nnr,nnc),maskf(nnr,nnc) !,znodata
	integer i,j,inum
	real	adatasum, areasum,fave,fmin,fmax

	adatasum=0.0
	areasum= 0.0
	inum=0
	do i=1, nnr
		do j =1, nnc
			if (maskf(i,j).ne.-9999.and.dataf(i,j).ne.-9999.) then
			inum= inum+1
			if (inum.eq.1) then
				fmin = dataf(i,j)
				fmax = dataf(i,j)
			endif		
			fmin = min(fmin, dataf(i,j))	
			fmax = max(fmax, dataf(i,j))		
			adatasum=adatasum+ dataf(i,j)*areaf1(i,j)*fracf1(i,j)/100.
			areasum = areasum+ areaf1(i,j)*fracf1(i,j)/100.
			endif
		enddo
	enddo
	fave =adatasum / areasum

	end