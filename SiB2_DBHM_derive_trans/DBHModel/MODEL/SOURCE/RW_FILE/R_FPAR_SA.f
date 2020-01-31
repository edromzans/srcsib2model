c This subroutine is used to abstract FPAR in SA (study area) from original files 
c Input:
c	FPAR_file:	The original file of FPAR
c	nr,nc:		Effective size (real size) of output SA array
c	x0,y0,s:	output array Geographic Referencing Info
c	N_div:		Resample resolution (Resample from Goode -> lambert projection)
c	reallat,reallon:	lat,lon Infomation of Goode (from Goode projection ancillary files)
c	NT:			Times of the saved data, e.g. if one month data are save in one file
c						then it will be 29/30/31 time daily data, so NT=29/30/31
c Output:
c	fpar:		Output fpar
c Note:
c	The subroutine need 'common.inc' for max_nr,max_nc
c				   need 'Convert_Lambert_latlon.f' for Convert_Lambert_latlon
c				   need 'Get_Goode_coordinate.f' for Get_Goode_coordinate
	subroutine R_FPAR_SA(FPAR_file,nr,nc,x0,y0,s,N_div,
     $	reallat,reallon,fpar,gain,KER)
	implicit none
	include '../INCLUDE/common.inc'
	CHARACTER*80	FPAR_file
	real			x0,y0,s,longitude,latitude,x(max_nc),y(max_nr)
	real			reallat(fpar_nr,fpar_nc),lam_0,phi_1,gain
	real			reallon(fpar_nr,fpar_nc)
	integer			nr,nc,i,j,m,n,nnr,nnc,N_div,Irecl,KER,l1,l2
	integer			iunsign,nondvi,SumN !,T,RT1,RT2,RF,k
	real			xm,yn,SumNdiv !,fndiv
	real			fpar(max_nr,max_nc) !,SumR
	character*1		Unsign(fpar_nr*fpar_nc)
	integer			ReclSize !,onec,oner

	real			r,lambda0,phi1
	common			/Lambert_para/r,lambda0,phi1
	character*80	FPAR_AnciDir,FPAR_Lat,FPAR_Lon,FPAR_dir,FPAR_Pre
	integer			FPAR_D_nc,FPAR_D_nr,FPAR_S_c,FPAR_S_r,FPAR_S_nc
	integer			FPAR_S_nr,FPAR_N_div
	common			/Read_FPAR_para/FPAR_AnciDir,FPAR_Lat,FPAR_Lon,
     $				FPAR_dir,FPAR_Pre,FPAR_D_nc,FPAR_D_nr,FPAR_S_c,
     $				FPAR_S_r,FPAR_S_nc,FPAR_S_nr,FPAR_N_div

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

	ReclSize=FPAR_S_nc*FPAR_S_nr
c	onec=(Col1-Col0+1)*125
c	oner=(Row1-Row0+1)*125
      open(10,form='unformatted',file=FPAR_file,access='direct',
     $	recl=ReclSize,status='old',ERR=7001)
		read(10,rec=1,ERR=7002) (Unsign(i),i=1,ReclSize)
	close(10)
c	NNdiv=real(N_div*N_div)
	DO i=1,nr
	DO j=1,nc
c	Resample one grid, divide it indo N_div grids, then averaged
		SumNdiv=0.0
		nondvi=0
		SumN=0
		do n=1,N_div
		do m=1,N_div
			xm=x(j)-0.5*s+(real(m)-0.5)*s/real(N_div)
			yn=y(i)+0.5*s-(real(n)-0.5)*s/real(N_div)
			call Convert_Lambert_latlon(xm,yn,
     $			longitude,latitude,lam_0,phi_1,r,1)
			longitude=longitude*180.0/pi
			latitude=latitude*180.0/pi
			call Get_Goode_coordinate(reallat,reallon,
     $			fpar_nc,fpar_nr,latitude,longitude,nnr,nnc)
				if (nnr.ne.0.and.nnc.ne.0) then
					Irecl=FPAR_S_nc*(nnr-1)+nnc	
					iunsign = Ichar(Unsign(Irecl))
					if (iunsign.ge.0.and.iunsign.le.250) then
						SumNdiv=SumNdiv+iunsign*gain
						SumN=SumN+1
					else
						nondvi = 1
					endif
				else
					nondvi = 1
				endif
		enddo
		enddo
		if (SumN.ne.0) then
			fpar(i,j) = SumNdiv /real(SumN)
		else
			fpar(i,j) = -9999.0
		endif
	ENDDO
	ENDDO
	RETURN
7001	KER=2
	Call Strlen(FPAR_file,l1,l2)
	PRINT*,'ERROR OPEN FPAR_file: ',FPAR_file(l1:l2)
	RETURN
7002	KER=3
	Call Strlen(FPAR_file,l1,l2)
	PRINT*,'ERROR READ FPAR_file: ',FPAR_file(l1:l2)
	RETURN
	end
