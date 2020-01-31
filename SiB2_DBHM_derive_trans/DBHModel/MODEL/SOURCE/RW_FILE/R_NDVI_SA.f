c This subroutine is used to abstract NDVI in SA (study area) from original files 
c Input:
c	NDVI_file:	The original file of NDVI
c	nr,nc:		Effective size (real size) of output SA array
c	x0,y0,s:	output array Geographic Referencing Info
c	N_div:		Resample resolution (Resample from Goode -> lambert projection)
c	reallat,reallon:	lat,lon Infomation of Goode (from Goode projection ancillary files)
c	NT:			Times of the saved data, e.g. if one month data are save in one file
c						then it will be 29/30/31 time daily data, so NT=29/30/31
c Output:
c	ndvi:		Output ndvi
c Note:
c	The subroutine need 'common.inc' for max_nr,max_nc
c				   need 'Convert_Lambert_latlon.f' for Convert_Lambert_latlon
c				   need 'Get_Goode_coordinate.f' for Get_Goode_coordinate
	subroutine R_NDVI_SA(NDVI_file,nr,nc,x0,y0,s,N_div,
     $	reallat,reallon,NT,ndvi,KER)
	implicit none
	include '../INCLUDE/common.inc'
	CHARACTER*80	NDVI_file
	real			x0,y0,s,longitude,latitude,x(max_nc),y(max_nr)
	real			reallat(ndvi_nr,ndvi_nc),lam_0,phi_1
	real			reallon(ndvi_nr,ndvi_nc)
	integer			nr,nc,i,j,m,n,nnr,nnc,N_div,Irecl,KER,l1,l2
	integer			iunsign,nondvi(ndvi_nt),NT,T,RT1,RT2,RF,k,SumN(ndvi_nt)
	real			xm,yn,fndiv,SumNdiv(ndvi_nt)
	real			ndvi(ndvi_nt,max_nr,max_nc),SumR
	character*1		Unsign(ndvi_nt*ndvi_nr*ndvi_nc)
	integer			ReclSize,onec,oner

	real			r,lambda0,phi1
	common			/Lambert_para/r,lambda0,phi1
	character*80	DAnciDir,DLat,DLon,DSuffix,D_dir,DPre
	integer			Col0,Col1,Row0,Row1,DN_div
	common			/Read_NDVI_para/DAnciDir,DLat,DLon,DSuffix,
     $				Col0,Col1,Row0,Row1,D_dir,DPre,DN_div

	KER=0

	lam_0=lambda0*pi/180.0
	phi_1=phi1*pi/180.0

c	Get Lambert coordinate
	do j=1,nc
		x(j)=x0+(real(j)-0.5)*s
	end do		
      do i=1,nr
		y(i)=y0+0.5*s+real(nr-i)*s
      end do

	ReclSize=(Row1-Row0+1)*(Col1-Col0+1)*125*125*NT
	onec=(Col1-Col0+1)*125
	oner=(Row1-Row0+1)*125
      open(10,form='unformatted',file=NDVI_file,access='direct',
     $	recl=ReclSize,status='old',ERR=7001)
		read(10,rec=1,ERR=7002) (Unsign(i),i=1,ReclSize)
	close(10)

c	NNdiv=real(N_div*N_div)
	DO i=1,nr
	DO j=1,nc
c	Resample one grid, divide it indo N_div grids, then averaged
		Do T=1,NT
			SumNdiv(T)=0.0
			nondvi(T)=0
			SumN(T)=0
		Enddo
		do n=1,N_div
		do m=1,N_div
			xm=x(j)-0.5*s+(real(m)-0.5)*s/real(N_div)
			yn=y(i)+0.5*s-(real(n)-0.5)*s/real(N_div)
			call Convert_Lambert_latlon(xm,yn,
     $			longitude,latitude,lam_0,phi_1,r,1)
			longitude=longitude*180.0/pi
			latitude=latitude*180.0/pi
			call Get_Goode_coordinate(reallat,reallon,
     $			ndvi_nc,ndvi_nr,latitude,longitude,nnr,nnc)
			Do T=1,NT
				if (nnr.ne.0.and.nnc.ne.0) then
					Irecl=(T-1)*onec*oner+onec*(nnr-1)+nnc	
					iunsign = Ichar(Unsign(Irecl))
					if (iunsign.gt.2.and.iunsign.lt.256) then
						SumNdiv(T)=SumNdiv(T)+(iunsign-128)*0.008
						SumN(T)=SumN(T)+1
					else
						nondvi(T) = 1
					endif
				else
					nondvi(T) = 1
				endif
			Enddo
		enddo
		enddo
	  Do T=1,NT
		if (nondvi(T).eq.0.and.SumN(T).ne.0) then
			ndvi(T,i,j) = SumNdiv(T) /real(SumN(T))
		else
			ndvi(T,i,j) = -9999
c	If this timestep, no data, set it as the data in the nearest day 
c			RF=0
c			do k=1,NT
c				RT1=T-k
c				RT2=T+k
c				if (RT1.ge.1.and.RF.eq.0) then
c					if (nondvi(RT1).eq.0.and.SumN(RT1).ne.0) then
c						ndvi(T,i,j)=SumNdiv(RT1)/real(SumN(RT1))
c						RF=1
c					endif
c				endif
c				if (RT2.le.NT.and.RF.eq.0) then
c					if (nondvi(RT2).eq.0.and.SumN(RT2).ne.0) then
c						ndvi(T,i,j)=SumNdiv(RT2)/real(SumN(RT2))
c						RF=1
c					endif
c				endif
c			enddo
		endif
	  Enddo
	ENDDO
	ENDDO
	RETURN
7001	KER=2
	Call Strlen(NDVI_file,l1,l2)
	PRINT*,'ERROR OPEN NDVI_file: ',NDVI_file(l1:l2)
	RETURN
7002	KER=3
	Call Strlen(NDVI_file,l1,l2)
	PRINT*,'ERROR READ NDVI_file: ',NDVI_file(l1:l2)
	RETURN
	end
