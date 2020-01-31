c	Get Calculate Region, DEM, Longitude etc.
	SUBROUTINE Get_Base_Maps()
      INCLUDE './SIB2/PARDIF.H'                                                      
      INCLUDE './SIB2/COMSIBC.H' 
	INCLUDE './SIB2/SiB2par.inc'	
	INCLUDE '../INCLUDE/Def_Rriver_routing.inc'
	INCLUDE '../INCLUDE/Def_Parameters.inc'
	INCLUDE './SIB2/SiB2River.inc'
	real	x,y,lam_0,phi_1,longitude,latitude
	real	x0,y0,s,nodata
	character*80	precmf
	integer id

	precmf='../GEO_INPUT_DATA/precm.asc'
	id=0
	open (1,file=precmf,status='old',err=41)
	id=1
	close(1)
	CALL readfile_float (precmf,nr,nc,x0,y0,s,
     $	nodata,precm,max_nr,max_nc)	
41	if (id.eq.0) then
		precm = -9999
	endif

c read the area of each grid (the grid area can be different from each other if it is needed)
	CALL readfile_float (area_file,nr,nc,x0,y0,s,
     $	nodata,gridarea,max_nr,max_nc)	   
c read fraction inside study area does NOT includes dissipative area (Smaller)
	CALL readfile_float (frac_file,nr,nc,x0,y0,s,
     $	nodata,fracnd,max_nr,max_nc)	
c read DEM data which does NOT includes dissipative area (Smaller)
	CALL readfile_float (dem_file,nr,nc,x0,y0,s,
     $	nodata,demnd,max_nr,max_nc)
c read fraction inside study area includes dissipative area (Larger)
	CALL readfile_float (FRAC_FD,nr,nc,x0,y0,s,
     $	nodata,fracdd,max_nr,max_nc)	
c read DEM data which includes dissipative area (Larger)
	CALL readfile_float (DEM_FD,nr,nc,x0,y0,s,
     $	nodata,demd,max_nr,max_nc)

c read flow direction in the gird
	CALL readfile_int (dir_file,nr,nc,x0,y0,s,
     $	nodata,griddir,max_nr,max_nc)	

	lon0	=	120.		!For './SUBROUTINE/Radiation_YANG.f'
	lam_0=lambda0*pi/180.0
	phi_1=phi1*pi/180.0
	
c	Get DEM data which includes dissipative area
	CALL Read_GEO_hrd(GEO_hrd,nr,nc,x0,y0,s)
	DO i=1,nr
		y=y0+0.5*s+real(nr-i)*s
		DO j=1,nc
			x=x0+(real(j)-0.5)*s
			CALL Convert_Lambert_latlon(x,y,
     $			longitude,latitude,lam_0,phi_1,r,1)
			long_10(i,j)=longitude*180.0/pi
			lat_10(i,j)=latitude*180.0/pi
		END DO	
      END DO

	END !SUBROUTINE Get_Base_Maps()

	SUBROUTINE Get_Irr_PARs(n_sub,sub)
      INCLUDE './SIB2/PARDIF.H'                                                      
      INCLUDE './SIB2/COMSIBC.H' 
	INCLUDE './SIB2/SiB2par.inc'	
	INCLUDE '../INCLUDE/Def_Rriver_routing.inc'
	INCLUDE '../INCLUDE/Def_Parameters.inc'
	INCLUDE './SIB2/SiB2River.inc'
	integer	IRRID,RIVID,INTERID,RESID
	real	RIVPER,	RESPER
	real fsave2(max_nr,max_nc)
	real fsum_lai(max_nr,max_nc),fsum_fpar(max_nr,max_nc)
	character*80	FName
	character*2 chmm         

c	Get the irrcode map (irrigation area id)
c	Assign water withdraw from river way or reservoir
		CALL readfile_int(irrcode_file,nr,nc,x0,y0,s,no_i,
     $		irrcode,max_nr,max_nc)	

	irrgmax=0
	do i=1,nr
		do j=1,nc
			if (irrcode(i,j).ne.-9999) then
				open (101, file =irrpro_file, status='old')
				read (101, *)
33				read (101,*,end=22) IRRID,RIVID,INTERID,
     $			RIVPER,RESID,RESPER
				if (irrcode(i,j).eq.IRRID) goto 22
				goto 33
22				close(101)
				if (irrcode(i,j).ne.IRRID) Print *,'Irrcode,Irrpro Error'
				if (RIVID.eq.0.or.INTERID.eq.0.or.RIVPER.eq.0.) then
					irr_riv(i,j)	=0
					irrriper(i,j)	=0
				else
					do ii=1,n_sub
					if (sub(ii).code.eq.RIVID) then
					  ifound =0
					  jj=1
					  do while (ifound.eq.0.and.jj.lt.max_irrg) 
					  if (sub(ii).flow(INTERID).irrxy(jj,1).eq.0) then
					  sub(ii).flow(INTERID).nirrg=jj
					  sub(ii).flow(INTERID).irrxy(jj,1)=i
					  sub(ii).flow(INTERID).irrxy(jj,2)=j
					  irrgmax= max (irrgmax, jj)
					  ifound =1
					  endif
					  jj= jj+1
					  enddo
					endif
					enddo
					irr_riv(i,j)	=RIVID*1000+INTERID
					irrriper(i,j)	=RIVPER
				endif
				if (RESID.eq.0.or.RESPER.eq.0.) then
					irr_res(i,j)	=0
					irresper(i,j)	=0
				else
					irr_res(i,j)	=RESID
					irresper(i,j)	=RESPER
				endif
			else
				irr_riv(i,j)	=0
				irrriper(i,j)	=0
				irr_res(i,j)	=0
				irresper(i,j)	=0
			endif
		enddo
	enddo
c	print *,irrgmax

c	Get the irrigation ratio map
		CALL readfile_float(irriratio_file,max_nr,max_nc,x0,y0,s,no_i,
     $		irriratioo,max_nr,max_nc)
		irriratio=irriratioo


c	Get parameter for extend LAI/FPAR data to 1960s
	call strlen(NDVI_ANA,l1,l2)

	FName=NDVI_ANA(l1:l2)//"LAIa.bin"
	CALL rd_fb (FName,max_nr,max_nc,x0,y0,s,
     $	fnodata,fb_LAI,max_nr,max_nc)
	FName=NDVI_ANA(l1:l2)//"LAIb.bin"
	CALL rd_fb (FName,max_nr,max_nc,x0,y0,s,
     $	fnodata,fm_LAI,max_nr,max_nc)
	FName=NDVI_ANA(l1:l2)//"FPARa.bin"
	CALL rd_fb (FName,max_nr,max_nc,x0,y0,s,
     $	fnodata,fb_FPAR,max_nr,max_nc)
	FName=NDVI_ANA(l1:l2)//"FPARb.bin"
	CALL rd_fb (FName,max_nr,max_nc,x0,y0,s,
     $	fnodata,fm_FPAR,max_nr,max_nc)

	fsum_lai=0.
	fsum_fpar=0.
	do imth=1,12
		write(chmm,'(G2.0)') imth
		FName=NDVI_ANA(l1:l2)//"LAI011982122000_avm"//chmm//".bin"
		call rd_fb (FName,max_nr,max_nc,x0,y0,s,
     $	fnodata,fsave2,max_nr,max_nc)
		do i=1,nr
		do j=1,nc
			fsave_LAI(imth,i,j) = fsave2(i,j)
			if (fsave2(i,j).ne.-9999.) then
				fsum_lai(i,j)=fsum_lai(i,j)+fsave2(i,j)
			else
				fsum_lai(i,j)=-9999.
			endif
		enddo
		enddo

		FName=NDVI_ANA(l1:l2)//"FPAR011982122000_avm"//chmm//".bin"
		call rd_fb (FName,max_nr,max_nc,x0,y0,s,
     $	fnodata,fsave2,max_nr,max_nc)
		do i=1,nr
		do j=1,nc
			fsave_FPAR(imth,i,j) = fsave2(i,j)
			if (fsave2(i,j).ne.-9999.) then
				fsum_fpar(i,j)=fsum_fpar(i,j)+fsave2(i,j)
			else
				fsum_fpar(i,j)=-9999.
			endif
		enddo
		enddo
	enddo		

	do imth=1,12
		do i=1,nr
		do j=1,nc
		if (fsum_lai(i,j).gt.0.) then
			fsave_LAI(imth,i,j)=fsave_LAI(imth,i,j)/fsum_lai(i,j)
		else
			fsave_LAI(imth,i,j)=-9999.
		endif
		if (fsum_fpar(i,j).gt.0.) then
			fsave_fpar(imth,i,j)=fsave_fpar(imth,i,j)/fsum_fpar(i,j)
		else
			fsave_fpar(imth,i,j)=-9999.
		endif
		enddo
		enddo
	enddo

	IF (nc_cl.eq.0) THEN !Set NO Climate change scenario
c	Get parameter for extend Climate data to 1960s
	!Tm
	FName=NDVI_ANA(l1:l2)//"TMaIDW"//"19602000.bin"
	CALL rd_fb (FName,max_nr,max_nc,x0,y0,s,
     $	fnodata,fb_tm,max_nr,max_nc)
	FName=NDVI_ANA(l1:l2)//"TMbIDW"//"19602000.bin"
	CALL rd_fb (FName,max_nr,max_nc,x0,y0,s,
     $	fnodata,fm_tm,max_nr,max_nc)
	FName=NDVI_ANA(l1:l2)//"TMavyIDW19601969.bin"
	CALL rd_fb (FName,max_nr,max_nc,x0,y0,s,
     $	fnodata,fav_tm,max_nr,max_nc)

	!Tmax
	FName=NDVI_ANA(l1:l2)//"TMAXaIDW"//"19602000.bin"
	CALL rd_fb (FName,max_nr,max_nc,x0,y0,s,
     $	fnodata,fb_tmax,max_nr,max_nc)
	FName=NDVI_ANA(l1:l2)//"TMAXbIDW"//"19602000.bin"
	CALL rd_fb (FName,max_nr,max_nc,x0,y0,s,
     $	fnodata,fm_tmax,max_nr,max_nc)
	FName=NDVI_ANA(l1:l2)//"TMAXavyIDW19601969.bin"
	CALL rd_fb (FName,max_nr,max_nc,x0,y0,s,
     $	fnodata,fav_tmax,max_nr,max_nc)
	
	!Tmin
	FName=NDVI_ANA(l1:l2)//"TMINaIDW"//"19602000.bin"
	CALL rd_fb (FName,max_nr,max_nc,x0,y0,s,
     $	fnodata,fb_tmin,max_nr,max_nc)
	FName=NDVI_ANA(l1:l2)//"TMINbIDW"//"19602000.bin"
	CALL rd_fb (FName,max_nr,max_nc,x0,y0,s,
     $	fnodata,fm_tmin,max_nr,max_nc)
	FName=NDVI_ANA(l1:l2)//"TMINavyIDW19601969.bin"
	CALL rd_fb (FName,max_nr,max_nc,x0,y0,s,
     $	fnodata,fav_tmin,max_nr,max_nc)

	!um
	FName=NDVI_ANA(l1:l2)//"UMaIDW"//"19602000.bin"
	CALL rd_fb (FName,max_nr,max_nc,x0,y0,s,
     $	fnodata,fb_um,max_nr,max_nc)
	FName=NDVI_ANA(l1:l2)//"UMbIDW"//"19602000.bin"
	CALL rd_fb (FName,max_nr,max_nc,x0,y0,s,
     $	fnodata,fm_um,max_nr,max_nc)
	FName=NDVI_ANA(l1:l2)//"UMavyIDW19601969.bin"
	CALL rd_fb (FName,max_nr,max_nc,x0,y0,s,
     $	fnodata,fav_um,max_nr,max_nc)
	FName=NDVI_ANA(l1:l2)//"UMminIDW19602000_rv.bin"
	CALL rd_fb (FName,max_nr,max_nc,x0,y0,s,
     $	fnodata,fmm_um,max_nr,max_nc)

	!rsum
	FName=NDVI_ANA(l1:l2)//"PRECaIDW"//"19602000.bin"
	CALL rd_fb (FName,max_nr,max_nc,x0,y0,s,
     $	fnodata,fb_rsum,max_nr,max_nc)
	FName=NDVI_ANA(l1:l2)//"PRECbIDW"//"19602000.bin"
	CALL rd_fb (FName,max_nr,max_nc,x0,y0,s,
     $	fnodata,fm_rsum,max_nr,max_nc)
	FName=NDVI_ANA(l1:l2)//"PRECavyIDW19601969.bin"
	CALL rd_fb (FName,max_nr,max_nc,x0,y0,s,
     $	fnodata,fav_rsum,max_nr,max_nc)
	FName=NDVI_ANA(l1:l2)//"PRECminIDW19602000_rv.bin"
	CALL rd_fb (FName,max_nr,max_nc,x0,y0,s,
     $	fnodata,fmm_rsum,max_nr,max_nc)

	!sun
	FName=NDVI_ANA(l1:l2)//"SUNaIDW"//"19602000.bin"
	CALL rd_fb (FName,max_nr,max_nc,x0,y0,s,
     $	fnodata,fb_sun,max_nr,max_nc)
	FName=NDVI_ANA(l1:l2)//"SUNbIDW"//"19602000.bin"
	CALL rd_fb (FName,max_nr,max_nc,x0,y0,s,
     $	fnodata,fm_sun,max_nr,max_nc)
	FName=NDVI_ANA(l1:l2)//"SUNavyIDW19601969.bin"
	CALL rd_fb (FName,max_nr,max_nc,x0,y0,s,
     $	fnodata,fav_sun,max_nr,max_nc)
	FName=NDVI_ANA(l1:l2)//"SUNminIDW19602000_rv.bin"
	CALL rd_fb (FName,max_nr,max_nc,x0,y0,s,
     $	fnodata,fmm_sun,max_nr,max_nc)

	!Reference ET
	FName=NDVI_ANA(l1:l2)//"RETaIDW"//"19602000.bin"
	CALL rd_fb (FName,max_nr,max_nc,x0,y0,s,
     $	fnodata,fb_et,max_nr,max_nc)
	FName=NDVI_ANA(l1:l2)//"RETbIDW"//"19602000.bin"
	CALL rd_fb (FName,max_nr,max_nc,x0,y0,s,
     $	fnodata,fm_et,max_nr,max_nc)
	FName=NDVI_ANA(l1:l2)//"RETavyIDW19601969.bin"
	CALL rd_fb (FName,max_nr,max_nc,x0,y0,s,
     $	fnodata,fav_et,max_nr,max_nc)
	FName=NDVI_ANA(l1:l2)//"RETminIDW19602000_rv.bin"
	CALL rd_fb (FName,max_nr,max_nc,x0,y0,s,
     $	fnodata,fmm_et,max_nr,max_nc)

	ENDIF


	END !SUBROUTINE Get_Irr_PARs()

	SUBROUTINE CHANGE_IRR_RATIO(i_y)
      INCLUDE './SIB2/PARDIF.H'                                                      
      INCLUDE './SIB2/COMSIBC.H' 
	INCLUDE './SIB2/SiB2par.inc'	
	INCLUDE '../INCLUDE/Def_Rriver_routing.inc'
	INCLUDE '../INCLUDE/Def_Parameters.inc'
	INCLUDE './SIB2/SiB2River.inc'
	real farea(3)	
	real U54_5,U64_5,U74_5,U84_5,U92_5
	data U54_5,U64_5,U74_5,U84_5,U92_5 /68,88.5,112.7,130.3,131.5/
	real M54_5,M64_5,M74_5,M84_5,M92_5
	data M54_5,M64_5,M74_5,M84_5,M92_5 /42.3,80.2,114.4,130.1,133.3/
	real D54_5,D64_5,D74_5,D84_5,D92_5
	data D54_5,D64_5,D74_5,D84_5,D92_5 /30,33.3,100,146.8,222.3/
	real Ureal,Mreal,Dreal
	character*80	tmpname

	if (i_y.lt.1964.5) then
		Ureal= U64_5-(U64_5-U54_5)/10.*(1964.5-i_y)
		Mreal= M64_5-(M64_5-M54_5)/10.*(1964.5-i_y)
		Dreal= D64_5-(D64_5-D54_5)/10.*(1964.5-i_y)
	elseif (i_y.lt.1974.5) then
		Ureal= U74_5-(U74_5-U64_5)/10.*(1974.5-i_y)
		Mreal= M74_5-(M74_5-M64_5)/10.*(1974.5-i_y)
		Dreal= D74_5-(D74_5-D64_5)/10.*(1974.5-i_y)
	elseif (i_y.lt.1984.5) then
		Ureal= U84_5-(U84_5-U74_5)/10.*(1984.5-i_y)
		Mreal= M84_5-(M84_5-M74_5)/10.*(1984.5-i_y)
		Dreal= D84_5-(D84_5-D74_5)/10.*(1984.5-i_y)
	else
		Ureal= U92_5-(U92_5-U84_5)/8.*(1992.5-i_y)
		Mreal= M92_5-(M92_5-M84_5)/8.*(1992.5-i_y)
		Dreal= D92_5-(D92_5-D84_5)/8.*(1992.5-i_y)
	endif

	IF (nc_ia.eq.0) THEN !Set NO irrigated area change scenario
		Ureal= U64_5
		Mreal= M64_5
		Dreal= D64_5
	ENDIF

	do inr=1,max_nr
	do inc=1,max_nc
		if (iumd(inr,inc).eq.1) then	
			if(irriratioo(inr,inc).ge.0.) then
				irriratio(inr,inc)=irriratioo(inr,inc)*Ureal*100./areairr(1)
			endif
		elseif (iumd(inr,inc).eq.2) then
			if(irriratioo(inr,inc).ge.0.) then
				irriratio(inr,inc)=irriratioo(inr,inc)*Mreal*100./areairr(2)
			endif
		else
			if(irriratioo(inr,inc).ge.0.) then
				irriratio(inr,inc)=irriratioo(inr,inc)*Dreal*100./areairr(3)
			endif
		endif
	enddo
	enddo		

	farea=0.
	do inr=1,max_nr
		do inc=1,max_nc
		if (iumd(inr,inc).eq.1) then
			if (fracdd(inr,inc).gt.0..and.irriratio(inr,inc).gt.0.) then
				farea(1)=farea(1)+gridarea(inr,inc)*fracdd(inr,inc)*
     $				  irriratio(inr,inc)/10000.		
     			endif							
		elseif (iumd(inr,inc).eq.2) then
			if (fracdd(inr,inc).gt.0..and.irriratio(inr,inc).gt.0.) then
				farea(2)=farea(2)+gridarea(inr,inc)*fracdd(inr,inc)*
     $				  irriratio(inr,inc)/10000.		
     			endif		
		else
			if (fracdd(inr,inc).gt.0..and.irriratio(inr,inc).gt.0.) then
				farea(3)=farea(3)+gridarea(inr,inc)*fracdd(inr,inc)*
     $				  irriratio(inr,inc)/10000.		
     			endif		
		endif
		enddo
	enddo

	CALL strlen(Derive_D,o1,o2)
	tmpname=Derive_D(o1:o2)//"areairr.txt"
	open (11, file=tmpname, status='unknown',access='append')
		write(11,*) "Irrigated area (Up,Mid,Down;KM^2) at year",i_y
		write(11,*) farea(1)
		write(11,*) farea(2)
		write(11,*) farea(3)
	close(11)

	END !SUBROUTINE CHANGE_IRR_RATIO



c	Get Verfication relatived inputs
	SUBROUTINE Get_VER_PARs()
      INCLUDE './SIB2/PARDIF.H'                                                      
      INCLUDE './SIB2/COMSIBC.H' 
	INCLUDE './SIB2/SiB2par.inc'	
	INCLUDE '../INCLUDE/Def_Rriver_routing.inc'
	INCLUDE '../INCLUDE/Def_Parameters.inc'
	INCLUDE './SIB2/SiB2River.inc'

c	read Verfication parameters
	CALL Read_VER_para(KER)	

	END !SUBROUTINE Get_VER_PARs()

c	Get SIB2 relatived inputs
	SUBROUTINE Get_SIB_PARs(n_sub,sub)
      INCLUDE './SIB2/PARDIF.H'                                                      
      INCLUDE './SIB2/COMSIBC.H' 
	INCLUDE './SIB2/SiB2par.inc'	
	INCLUDE '../INCLUDE/Def_Rriver_routing.inc'
	INCLUDE '../INCLUDE/Def_Parameters.inc'
	INCLUDE './SIB2/SiB2River.inc'
	!soilpara(SNUM; Porosity,psi_s,K_s,b_exp,avg_slope,[yield])
	real soilpara(7000,6)			!soil parmater from FAO soil map
	real slpar3_2(max_nr,max_nc)	!slpar3 *1.0E6
	CHARACTER*80	soilfile,ppfile
c	read sib2 parameters
	CALL Read_SIB2_para(KER)		
	CALL Read_ANA_para(KER)

c	Get the SiB2 paramaters list (related to vegetation types or soil types)	
	CALL SiB2data
c	Get initial conditions:dtt,itrunk,ilw,initial values of tc,tg,td,www
	CALL cntrol(icho1, ichmet, 0) 

c	Get the SIB2 vegetation map
	CALL readfile_int(vege_file,nr,nc,x0,y0,s,no_i,
     $	vege_sib,max_nr,max_nc)
c	Get distanct to river month
	CALL readfile_float(dis_file,nr,nc,x0,y0,s,no_i,
     $	distomon,max_nr,max_nc)

c	Get slope for sib, input slope UNIT:PERCENTRISE, then slope/100=rise/run
c	Slope for SIB2 is sin(a) where a is slope in radian
c	Usually, slope_sib can directly equals to slope PERCENTRISE
	CALL readfile_float(slpsib_f,nr,nc,x0,y0,s,no_i,
     $	slope_sib,max_nr,max_nc)
	do i = 1, max_nr
		do j =1, max_nc
			if (slope_sib(i,j).ne.-9999.0) then
		 slope_sib(i,j)=slope_sib(i,j)/sqrt(10000.+slope_sib(i,j)**2)
				if (slope_sib(i,j).le.0.00005) slope_sib(i,j)=0.00005
			endif
		enddo
	enddo

c	Calculate parameters for SIB2 (save it in order to save time)
	do j =1, 9
		do i = 1, 65
			ivtype_  = j
			flai_    = real(i)*0.1
			rhoair_ = 1.225
			CALL  derive_trans(ivtype_,flai_,rhoair_,
     &			ha, z0d, dd,g2, g3, cc1, cc2, corb1, corb2)
			varcal_para(j,(i-1)*9+1)=	ha
			varcal_para(j,(i-1)*9+2)=	z0d
			varcal_para(j,(i-1)*9+3)=	dd
			varcal_para(j,(i-1)*9+4)=	g2
			varcal_para(j,(i-1)*9+5)=	g3
			varcal_para(j,(i-1)*9+6)=	cc1
			varcal_para(j,(i-1)*9+7)=	cc2
			varcal_para(j,(i-1)*9+8)=	corb1
			varcal_para(j,(i-1)*9+9)=	corb2
		end do
      end do

c	Get the SIB2 soil map
	CALL readfile_int(soil_file,nr,nc,x0,y0,s,no_i,
     $	soil_sib,max_nr,max_nc)

c	Get SIB2 soil parameters (derived from FAO soil map by Soil_texture.f)
	DO i=1,7000
		soilpara(i,1)=0.458
		soilpara(i,2)=-0.2
		soilpara(i,3)=3.5E-06
		soilpara(i,4)=7.797
		soilpara(i,5)=0.08
		soilpara(i,6)=0.20
	ENDDO
	OPEN(417,file=soil_para,status='old')
	DO j=1, 7000
		READ(417,418,end=4171) ISNUM,(soilpara(ISNUM,i),i=1,6)
	ENDDO	
4171	CLOSE(417)
  418	FORMAT (I5,6F15.10)

	do i =1,max_nr
		do j=1,max_nc
			istype	=	soil_sib(i,j)
			if (istype.gt.0) then
			slpar1(i,j)	=	soilpara(istype,1)
			slpar2(i,j)	=	soilpara(istype,2)
			slpar3(i,j)	=	soilpara(istype,3)
			slpar4(i,j)	=	soilpara(istype,4)
			slpar5(i,j)	=	soilpara(istype,5)
			else
			slpar1(i,j)	=	0.458
			slpar2(i,j)	=	-0.2
			slpar3(i,j)	=	3.5E-06
			slpar4(i,j)	=	7.797
			slpar5(i,j)	=	0.08
			endif
		enddo
	enddo

	!Output the input parameters
	CALL strlen(Derive_D,l1,l2) !Porosity,psi_s,K_s,b_exp,avg_slope,[yield]
	CALL Read_GEO_hrd(GEO_hrd,nr,nc,x0,y0,s)
	soilfile =Derive_D(l1:l2)//"Porosity.asc" 
	CALL writefile_float (soilfile,max_nr,max_nc,x0,y0,s,
     $	-9999.0,slpar1,max_nr,max_nc)
	soilfile =Derive_D(l1:l2)//"Psi_s.asc" 
	CALL writefile_float (soilfile,max_nr,max_nc,x0,y0,s,
     $	-9999.0,slpar2,max_nr,max_nc)
	soilfile =Derive_D(l1:l2)//"K_s.asc"
	slpar3_2 =  slpar3 * 1.0E6
	CALL writefile_float (soilfile,max_nr,max_nc,x0,y0,s,
     $	-9999.0,slpar3_2,max_nr,max_nc)
	soilfile =Derive_D(l1:l2)//"B_exp.asc" 
	CALL writefile_float (soilfile,max_nr,max_nc,x0,y0,s,
     $	-9999.0,slpar4,max_nr,max_nc)
	soilfile =Derive_D(l1:l2)//"Soil_slp.asc" 
	CALL writefile_float (soilfile,max_nr,max_nc,x0,y0,s,
     $	-9999.0,slpar5,max_nr,max_nc)

	!Set and output the precipiation distribution paramters (for sib2)
	open(201, file=sibpp,status='old',err=209)
		appcoff1= 20.
		bppcoff1= 20.
		cppcoff1= .206e-8
		slpcoff	= 1.
208		read(201, *,end=210,err=210) nmain,app,bpp,cpp,slpp
		if (nmain.gt.0.and.nmain.le.9.and.app.gt.0.) then
			do isub1=1,n_sub
				if ( sub(isub1).digit(levels).eq. nmain) then
					do iflow1=1,sub(isub1).nflow
					do igrid1=1,sub(isub1).flow(iflow1).ngrid
						inr = sub(isub1).flow(iflow1).gridxy(igrid1,1)
						inc = sub(isub1).flow(iflow1).gridxy(igrid1,2)	
						appcoff1(inr,inc)= app
						bppcoff1(inr,inc)= bpp
						cppcoff1(inr,inc)= cpp
						slpcoff (inr,inc)= slpp
					enddo
					enddo
				endif
			enddo			
		endif
		goto 208
209		appcoff1= 20.
		bppcoff1= 20.
		cppcoff1= .206e-8
		slpcoff	= 1.
210	close(201)

	ppfile = Derive_D(l1:l2)//"appcoff1.asc" 
	CALL writefile_float (ppfile,max_nr,max_nc,x0,y0,s,
     $	-9999.0,appcoff1,max_nr,max_nc)
	ppfile = Derive_D(l1:l2)//"bppcoff1.asc" 
	CALL writefile_float (ppfile,max_nr,max_nc,x0,y0,s,
     $	-9999.0,bppcoff1,max_nr,max_nc)
	ppfile = Derive_D(l1:l2)//"cppcoff1.asc" 
	CALL writefile_float (ppfile,max_nr,max_nc,x0,y0,s,
     $	-9999.0,cppcoff1,max_nr,max_nc)

	END !SUBROUTINE Get_SIB_PARs()

	SUBROUTINE Set_SIBDHM_INI()
      INCLUDE './SIB2/PARDIF.H'                                                      
      INCLUDE './SIB2/COMSIBC.H' 
	INCLUDE './SIB2/SiB2par.inc'	
	INCLUDE '../INCLUDE/Def_Rriver_routing.inc'
	INCLUDE '../INCLUDE/Def_Parameters.inc'
	INCLUDE './SIB2/SiB2River.inc'
	
	do i = 1, max_nr
		do j =1, max_nc
			Dimp(i,j) = 15.0
			Speyield(i,j) = 0.1
		enddo
	enddo

	do i = 1, max_nr
		do j =1, max_nc
			irrireq(i,j) = 0.0
			irriter(i,j) = 0
		enddo
	enddo	
	
	END !SUBROUTINE Set_SIBDHM_INI()

c set the location of output station , name of output station
c and read the observations of the stations (if there are any)
	SUBROUTINE Set_OUT_stn(n_sub,sub)
      INCLUDE './SIB2/PARDIF.H'                                                      
      INCLUDE './SIB2/COMSIBC.H' 
	INCLUDE './SIB2/SiB2par.inc'	
	INCLUDE '../INCLUDE/Def_Rriver_routing.inc'
	INCLUDE '../INCLUDE/Def_Parameters.inc'
	INCLUDE './SIB2/SiB2River.inc'
	integer			codeid2(max_nr,max_nc),l1,l2,o1,o2,o3,o4
	character*80	tmpname,obvname,chtmp
      integer			dmth(12),dyear,nday,nmon,nyear
	real			Qdo,Qmo,Qyo,accare
      data			dmth /31,28,31,30,31,30,31,31,30,31,30,31/
	real			lam_0,phi_1,flat_,flong_
	real dtmp1(max_nr,max_nc), dtmp2(max_nr,max_nc)

c	read soil moisture check stations
c	Location of the stations (soilgr) and the station name (slstnn)
	CALL Read_GEO_hrd(GEO_hrd,nr,nc,x0,y0,s)
	lam_0=lambda0*pi/180.0
	phi_1=phi1*pi/180.0

	nsoilg =0
	soilgr =0
	open (18, file=out_soil, status='old',err=18) 
		read(18,*)
		do i=1,max_soilo
17			nsoilg = nsoilg +1
			read(18,'(I8,A12,1X,2F8.2)',end=18) iid,slstnn(nsoilg),flat,flong
			flong_ = flong *pi/180.0
			flat_  = flat *pi/180.0
			CALL Convert_Lambert_latlon(x,y,flong_,flat_,lam_0,phi_1,r,0)
			soilgr(nsoilg,1)=nr-aint((y-y0)/s)  !inr
			soilgr(nsoilg,2)=aint((x-x0)/s)+1  !inc
c			print *,flong,flat,soilgr(nsoilg,1),soilgr(nsoilg,2)
			goto 17
		enddo
18	close(18)
	nsoilg		= nsoilg -1
	laydep(1)	= layo1
	if (laydep(1).lt.0.0.or.laydep(1).gt.1.0) laydep(1) = 0.02  
	laydep(2) = 1.0 - laydep(1)

	DayOBV=0.
	MonOBV=0.
	YearOBV=0.

	dtmp1 = demnd
	dtmp2 = demd
	demnd = -9999
	demd  = -9999
	iumd  = 3
	n_stno=0
	CALL strlen(Derive_D,o1,o2)
	CALL strlen(obv_dir,o3,o4)
	tmpname=Derive_D(o1:o2)//"stnctl_area.asc"
	open(99, file=tmpname, status='unknown')
	write(99,'(3(A20,1x))') 'StnName','CtlArea','IrrArea' 
	open(19, file=Outstn, status='old',err=19)
	do i=1,max_stno
	read(19,'(2i5,1X,A20)',end=20) subcode(i),flowid(i),outstnn(i)
	CALL Stn_ctl_area(subcode(i),flowid(i),levels,n_sub,sub,codeid2,KID)
	if (KID.ne.0) then
		accare=0.0
		accar2=0.0
		do inr =1, max_nr
			do inc = 1, max_nc
				if (codeid2(inr,inc).ne.-9999 ) then
				accare=accare+gridarea(inr,inc)*fracnd(inr,inc)/100.
				 if (irriratio(inr,inc).gt.0.) then
				  accar2=accar2+gridarea(inr,inc)*fracnd(inr,inc)*
     $				  irriratio(inr,inc)/10000.
				 endif
				endif
			enddo
		enddo
		ctlacca(i)=	accare ! km2
		tmpname=""
		obvname=""
		write(tmpname,*) outstnn(i)
		CALL strlen(tmpname,l1,l2)
		write(99,'(A20,1x,2(F20.3,1x))') tmpname(l1:l2),accare,accar2
		obvname=obv_dir(o3:o4)//tmpname(l1:l2)//".daily"
		tmpname=Derive_D(o1:o2)//tmpname(l1:l2)//"_loca.asc"
		call writefile_int (tmpname,max_nr,max_nc,x0,y0,s,
     $		-9999,codeid2,max_nr,max_nc)
		if (i.eq.5) then !Toudaoguai station
			do inr=1,max_nr
				do inc=1,max_nc
				if (codeid2(inr,inc).ne.-9999) then
					iumd(inr,inc) = 1
				endif
				enddo
			enddo
		elseif (i.eq.11) then !Huayuankou station
			do inr=1,max_nr
				do inc=1,max_nc
				if (codeid2(inr,inc).ne.-9999.and.iumd(inr,inc).ne.1) then
					iumd(inr,inc) = 2
				endif
				enddo
			enddo
		endif
cccccccccccccccccccccccccccccccc!Only calculate Zhangjiashan,tangnaihai
		if (i.eq.12) then
			do inr=1,max_nr
				do inc=1,max_nc
				if (codeid2(inr,inc).ne.-9999) then
					demnd(inr,inc) = dtmp1(inr,inc)
					demd(inr,inc)  = dtmp2(inr,inc)
				endif
				enddo
			enddo
			demnd=dtmp1
			demd =dtmp2
		endif
cccccccccccccccccccccccccccccccc
		open(199, file=obvname, status='old', err=199)
		nday=1
		nmon=1
		nyear=1
		Qmo=0.
		Qyo=0.
888		read (199,*,end=999),i_y,i_m,i_d,Qdo
		if (i_y.ge.startyear.and.i_y.le.endyear) then
			DayOBV(nday,i) = Qdo
			nday = nday + 1 
			if(mod(i_y,4).eq.0 ) then
				dyear=366
				if (i_m.eq.2) dmth(i_m)=29					    
			endif
			if(mod(i_y,4).ne.0 ) then
				dyear=365
				if (i_m.eq.2) dmth(i_m)=28
			endif
			if (i_d.eq.1) Qmo=0.
			if (i_d.eq.1.and.i_m.eq.1) Qyo=0.
			Qmo= Qmo + Qdo/dmth(i_m)
			Qyo= Qyo + Qdo/dyear
c			if (i.eq.1) then
c				print *,i_y,i_m,i_d,Qdo,Qmo,Qyo,dyear,dmth(i_m)
c				pause
c			endif
			if (i_d.eq.dmth(i_m)) then
				if (abs(Qmo+9999.).lt.1. ) then
					MonOBV(nmon,i)= -9999.0
				else
					MonOBV(nmon,i)= Qmo
				endif
				nmon = nmon + 1 
				if (i_m.eq.12) then
					if (abs(Qyo+9999.).lt.1. ) then
						YearOBV(nyear,i) = -9999.0
					else
						YearOBV(nyear,i) = Qyo
					endif
					nyear = nyear + 1
				endif
			endif
		endif
		goto 888
999		close(199)

c		print *,obvname,startyear,endyear

		goto 200
199		Write(icho1,*) "The file can not open:",
     $     obv_dir(o3:o4)//tmpname(l1:l2)//".daily"	
		print *, "The file can not open:",
     $     obv_dir(o3:o4)//tmpname(l1:l2)//".daily"
200	endif 
	n_stno=n_stno+1
	end do
	close(19)
	close(99)

c	print*,n_stno
c	CALL totarea(n_sub,sub,area,ngrids)
c	pause
	goto 20
19	Write(icho1,*) "The file can not open:",Outstn
	Write(icho1,*) "No discharge result will be output"
	print *, "The file can not open:",Outstn
	print *, "No discharge result will be output"
	n_stno = 0

20	continue

		areairr=0.
		do inr=1,max_nr
			do inc=1,max_nc
			if (iumd(inr,inc).eq.1) then
				if (fracdd(inr,inc).gt.0..and.irriratio(inr,inc).gt.0.) then
				  areairr(1)=areairr(1)+gridarea(inr,inc)*fracdd(inr,inc)*
     $				  irriratio(inr,inc)/10000.		
     				endif							
			elseif (iumd(inr,inc).eq.2) then
				if (fracdd(inr,inc).gt.0..and.irriratio(inr,inc).gt.0.) then
				  areairr(2)=areairr(2)+gridarea(inr,inc)*fracdd(inr,inc)*
     $				  irriratio(inr,inc)/10000.		
     				endif		
			else
				if (fracdd(inr,inc).gt.0..and.irriratio(inr,inc).gt.0.) then
				  areairr(3)=areairr(3)+gridarea(inr,inc)*fracdd(inr,inc)*
     $				  irriratio(inr,inc)/10000.		
     				endif		
			endif
			enddo
		enddo

		tmpname=Derive_D(o1:o2)//"iumd.asc"
		call writefile_int (tmpname,max_nr,max_nc,x0,y0,s,
     $		-9999,iumd,max_nr,max_nc)

		tmpname=Derive_D(o1:o2)//"areairr.txt"
		open (11, file=tmpname, status='unknown')
			write(11,*) "Irrigated area from digit map (Up,Mid,Down;KM^2)"
			write(11,*) areairr(1)
			write(11,*) areairr(2)
			write(11,*) areairr(3)
		close(11)

	RETURN
	END !SUBROUTINE Set_OUT_stn


c	Set initial status parameters for each grid
c	IDX: an index
c	IDX=0, use fix initial status
c	IDX=1, use initial status from file (priority)	
	SUBROUTINE Initial_cond(IDX,sub)
      INCLUDE './SIB2/PARDIF.H'
      INCLUDE './SIB2/COMSIBC.H' 
	INCLUDE './SIB2/SiB2par.inc'	
	INCLUDE '../INCLUDE/Def_Rriver_routing.inc'
	INCLUDE '../INCLUDE/Def_Parameters.inc'
	INCLUDE './SIB2/SiB2River.inc'	
	integer IDX,isub1,iflow1,irec

	if (IDX.eq.1) then	! IDX=1, try to get initial data from files
		read(icho4,err=410, rec=1) tc_ni
		read(icho4,err=410, rec=2) tg_ni
		read(icho4,err=410, rec=3) td_ni
		read(icho4,err=410, rec=4) capac1_ni
		read(icho4,err=410, rec=5) capac2_ni
		read(icho4,err=410, rec=6) snoww1_ni
		read(icho4,err=410, rec=7) snoww2_ni
		read(icho4,err=410, rec=8) www1_ni
		read(icho4,err=410, rec=9) www2_ni
		read(icho4,err=410, rec=10) www3_ni
		read(icho4,err=410, rec=11) tc_i
		read(icho4,err=410, rec=12) tg_i
		read(icho4,err=410, rec=13) td_i
		read(icho4,err=410, rec=14) capac1_i
		read(icho4,err=410, rec=15) capac2_i
		read(icho4,err=410, rec=16) snoww1_i
		read(icho4,err=410, rec=17) snoww2_i
		read(icho4,err=410, rec=18) www1_i
		read(icho4,err=410, rec=19) www2_i
		read(icho4,err=410, rec=20) www3_i
		read(icho4,err=410, rec=21) SWdepA
		read(icho4,err=410, rec=22) GWdepA
		read(icho4,err=410, rec=23) surdep_ni
		read(icho4,err=410, rec=24) surdep_i

		do isub1=1,max_sub
			do iflow1=1, max_flow
				irec = (isub1-1)*max_flow*2 + (iflow1-1)*2
				read(icho5,err=410, rec=irec+1) 
     $				sub(isub1).flow(iflow1).Qi1_j1
				read(icho5,err=410, rec=irec+2) 
     $				sub(isub1).flow(iflow1).WDj1
			enddo
		enddo

		WRITE(icho1, *) "READ INITIAL PARAMETERS FROM FIEL"
		PRINT *, "READ INITIAL PARAMETERS FROM FIEL"
		go to 411

410		IDX = 0					
		WRITE(icho1, *) "CANNOT READ INITIAL PARAMETERS FROM FIEL"
		PRINT *, "CANNOT READ INITIAL PARAMETERS FROM FIEL"
411	endif

	if (IDX.eq.0) then  ! IDX=0, use fix initial data
	WRITE(icho1, *) "THE FIXED INITIAL PARAMETERS ARE USDED"
	PRINT *, "THE FIXED INITIAL PARAMETERS ARE USDED"
	tc_ni = tc_ini
	tg_ni = tg_ini
	td_ni = td_ini
	capac1_ni = 0.
	capac2_ni = 0.
	snoww1_ni = 0.
	snoww2_ni = 0.
	www1_ni = www_ini(1)
	www2_ni = www_ini(2)
	www3_ni = www_ini(3)
	surdep_ni = 0.

	tc_i = tc_ini
	tg_i = tg_ini
	td_i = td_ini
	capac1_i = 0.
	capac2_i = 0.
	snoww1_i = 0.
	snoww2_i = 0.
	www1_i = www_ini(1)
	www2_i = www_ini(2)
	www3_i = www_ini(3)
	surdep_i = 0.
	retnfw_i = 0.
	tgw_irr	 = 0.

	SWdepA	= 0.0
	GWdepA	= 6.0
	do inr =1,max_nr
		do inc=1,max_nc
			if (vege_sib(inr,inc).eq.10) GWdepA(inr,inc)=0.0
		enddo
	enddo

	endif

	runoff1	= 0.0
	riv_rof	= 0.0
	runoff2	= 0.0
	QreA	= 0.0
	DayPRE	= 0.0
	MonPRE	= 0.0
	YearPRE	= 0.0
	www1	= 0.0
	www2	= 0.0
	www3	= 0.0
	sw1		= 0.0
	sw2		= 0.0
	sw3		= 0.0

	DO inr=1,nr
	DO inc=1,nc
	IF (demd(inr,inc).ne.-9999) THEN
		
		ivtype  =	9
		sodep	=	sodep_v(ivtype)
		rootd	=	rootd_v(ivtype)
		rootd	=	amin1( rootd, sodep*0.75 )                                        
		zdepth1_i(inr,inc) =0.02                                                          
		zdepth2_i(inr,inc) =rootd - 0.02                                                  
		zdepth3_i (inr,inc)=sodep-zdepth1_i(inr,inc)-zdepth2_i(inr,inc)  	

		ivtype  =	vege_sib(inr,inc)
		if (ivtype.eq.10) then
		zdepth1_ni(inr,inc) =0.                                                          
		zdepth2_ni(inr,inc) =0.                                                 
		zdepth3_ni (inr,inc)=0.  
		else
		sodep	=	sodep_v(ivtype)
		rootd	=	rootd_v(ivtype)
		rootd	=	amin1( rootd, sodep*0.75 )                                        
		zdepth1_ni(inr,inc)=0.02                                                          
		zdepth2_ni(inr,inc)=rootd - 0.02                                                  
		zdepth3_ni(inr,inc)=sodep-rootd  
		endif	

		istype = soil_sib(inr,inc)
		if (istype.gt.0) then
			sporos(inr,inc)	=	slpar1(inr,inc)					
		else
			sporos(inr,inc)	=	0.458
		endif
	ENDIF
	ENDDO !inc
	ENDDO !inr
	END ! END SUBROUTINE Initial_cond

c	Save status parameters for each grid and river
	SUBROUTINE Save_cond(sub)
      INCLUDE './SIB2/PARDIF.H'
      INCLUDE './SIB2/COMSIBC.H' 
	INCLUDE './SIB2/SiB2par.inc'	
	INCLUDE '../INCLUDE/Def_Rriver_routing.inc'
	INCLUDE '../INCLUDE/Def_Parameters.inc'
	INCLUDE './SIB2/SiB2River.inc'	
	integer isub1,iflow1,irec

	write(icho4,err=420, rec=1) tc_ni
	write(icho4,err=420, rec=2) tg_ni
	write(icho4,err=420, rec=3) td_ni
	write(icho4,err=420, rec=4) capac1_ni
	write(icho4,err=420, rec=5) capac2_ni
	write(icho4,err=420, rec=6) snoww1_ni
	write(icho4,err=420, rec=7) snoww2_ni
	write(icho4,err=420, rec=8) www1_ni
	write(icho4,err=420, rec=9) www2_ni
	write(icho4,err=420, rec=10) www3_ni
	write(icho4,err=420, rec=11) tc_i
	write(icho4,err=420, rec=12) tg_i
	write(icho4,err=420, rec=13) td_i
	write(icho4,err=420, rec=14) capac1_i
	write(icho4,err=420, rec=15) capac2_i
	write(icho4,err=420, rec=16) snoww1_i
	write(icho4,err=420, rec=17) snoww2_i
	write(icho4,err=420, rec=18) www1_i
	write(icho4,err=420, rec=19) www2_i
	write(icho4,err=420, rec=20) www3_i
	write(icho4,err=420, rec=21) SWdepA
	write(icho4,err=420, rec=22) GWdepA
	write(icho4,err=420, rec=23) surdep_ni
	write(icho4,err=420, rec=24) surdep_i

	do isub1=1,max_sub
		do iflow1=1, max_flow
			irec = (isub1-1)*max_flow*2 + (iflow1-1)*2
			write(icho5,err=420, rec=irec+1) 
     $				sub(isub1).flow(iflow1).Qi1_j1
			write(icho5,err=420, rec=irec+2) 
     $				sub(isub1).flow(iflow1).WDj1
		enddo
	enddo

	WRITE(icho1, *) "SAVE Initial PARAMETERS TO FIEL"
	PRINT *, "SAVE Initial PARAMETERS TO FIEL"

	go to 421
420	WRITE(icho1, *) "CANNOT SAVE Initial PARAMETERS TO FIEL"
	PRINT *, "CANNOT SAVE Initial PARAMETERS TO FIEL"

421	END ! END SUBROUTINE Save_cond


c	Set initial status parameters for each grid
c	IDX: an index
c	IDX=0, use fix initial status
c	IDX=1, use initial status from file (priority)	
	SUBROUTINE River_para(irx,Rivpara,sub,n_sub)
      INCLUDE './SIB2/PARDIF.H'
      INCLUDE './SIB2/COMSIBC.H' 
	INCLUDE './SIB2/SiB2par.inc'	
	INCLUDE '../INCLUDE/Def_Rriver_routing.inc'
	INCLUDE '../INCLUDE/Def_Parameters.inc'
	INCLUDE './SIB2/SiB2River.inc'
	real	Rivpara(max_sub,max_flow,14),RivWid(max_sub,max_flow)	
	integer irx,isub1,iflow1,imon,n_sub

	if (irx.eq.1) then	! IDX=1, try to get initial data from files
		read(icho6,err=610, rec=1) RivWid
		do iflow1=1,max_flow
		do isub1=1,max_sub
			Rivpara(isub1,iflow1,1)=	RivWid(isub1,iflow1)
		enddo
		enddo
		read(icho6,err=610, rec=2) RivWid !sub.flow.height
		do iflow1=1,max_flow
		do isub1=1,max_sub
			Rivpara(isub1,iflow1,2)=	RivWid(isub1,iflow1)
		enddo
		enddo
		read(icho6,err=610, rec=3) RivWid !sub.flow.rough
		do iflow1=1,max_flow
		do isub1=1,max_sub
			Rivpara(isub1,iflow1,3)=	RivWid(isub1,iflow1)
		enddo
		enddo
		
		do isub1 =1, n_sub
		do iflow1 =1, sub(isub1).nflow
			sub(isub1).flow(iflow1).width=Rivpara(isub1,iflow1,1)
			sub(isub1).flow(iflow1).height=Rivpara(isub1,iflow1,2)
			sub(isub1).flow(iflow1).rough=Rivpara(isub1,iflow1,3)
		enddo
		enddo

		WRITE(icho1, *) "derived river PARAMETERS USED"
		PRINT *, "derived river PARAMETERS USED"	
		 
		go to 611
610		irx = 0					
		WRITE(icho1, *) "CANNOT use derived river PARAMETERS"
		WRITE(icho1, *) "THE fixed RIVER PARAMETERS ARE USDED"
		PRINT *, "CANNOT use derived river PARAMETERS"
		PRINT *, "THE fixed RIVER PARAMETERS ARE USDED"
611	endif
	END ! END SUBROUTINE River_para

c	IDX: an index
c	IDX=1, use initial status from file (priority)	
c	if idx=1, updata river parameter per month
	SUBROUTINE UPDATARV(Rivpara,sub,n_sub,i_m)
      INCLUDE './SIB2/PARDIF.H'
      INCLUDE './SIB2/COMSIBC.H' 
	INCLUDE './SIB2/SiB2par.inc'	
	INCLUDE '../INCLUDE/Def_Rriver_routing.inc'
	INCLUDE '../INCLUDE/Def_Parameters.inc'
	INCLUDE './SIB2/SiB2River.inc'
	real	Rivpara(max_sub,max_flow,14)	
	integer irx,isub1,iflow1,imon,i_m
	
	do isub1 =1, n_sub
	do iflow1 =1, sub(isub1).nflow
		sub(isub1).flow(iflow1).width=Rivpara(isub1,iflow1,i_m)
	enddo
	enddo

	END ! END SUBROUTINE UPDATARV

c calculate the sum of discharge at each sub basin and flow interval
c record the max water depth in river (from river bed to river water level)
	Subroutine Q_derived(sub,irx,i_m,NQSsf,QSsubflow,MWDsf)	
	include '../INCLUDE/Def_Rriver_routing.inc'
	INCLUDE './SIB2/SiB2River.inc'
      INCLUDE './SIB2/PARDIF.H'      
	INCLUDE './SIB2/SiB2par.inc'  	                                                
      INCLUDE './SIB2/COMSIBC.H' 	
	integer irx,i_m,NQSsf(12)
	real	QSsubflow(max_sub,max_flow,12),MWDsf(max_sub,max_flow)

	if (irx.eq.0) then	
		do iflow1=1,max_flow
		do isub1=1,max_sub
			MWDsf(isub1,iflow1)= max ( MWDsf(isub1,iflow1),
     $			  sub(isub1).flow(iflow1).WDj1 )
			QSsubflow(isub1,iflow1,i_m)=QSsubflow(isub1,iflow1,i_m)+
     $			sub(isub1).flow(iflow1).Qi1_j1
		enddo
		enddo
		NQSsf(i_m)=NQSsf(i_m)+1
	endif

	End !Subroutine Q_derived

c Save derived river parameters
	Subroutine Save_derived(n_sub,sub,NQSsf,QSsubflow,MWDsf)	
	include '../INCLUDE/Def_Rriver_routing.inc'
	INCLUDE './SIB2/SiB2River.inc'
      INCLUDE './SIB2/PARDIF.H'      
	INCLUDE './SIB2/SiB2par.inc'  	                                                
      INCLUDE './SIB2/COMSIBC.H' 	
	character*80	code_file,dis_file,dir_file,slope_file,dem_file
	character*80	area_file,frac_file,riverway_file,Outstn,Output_D
	character*80	Derive_D       
      integer	levels,startyear,endyear,startmont,startday,endmont,endday
	real			dx_max,dt,rivlen
	common			/Input_para/ levels,startyear,startmont,startday,
     $				endyear,endmont,endday,dx_max,
     $		code_file,dis_file,dir_file,slope_file,dem_file,area_file,
     $frac_file,riverway_file,Outstn,Output_D,Derive_D,dt,rivlen
	integer			INTPLT,NDVIID
	character*80	ATM_ITP,GEO_hrd,LambNDVI
	common			/Get_Grid_ATM_para/
     $				INTPLT,NDVIID,ATM_ITP,GEO_hrd,LambNDVI
	real	QSsubflow(max_sub,max_flow,12),QSall(max_sub,max_flow)
	real	MWDsf(max_sub,max_flow),Rough(max_sub,max_flow)
	real	Qmyear,Qmmom,Zmon,Wmon(max_sub,max_flow), Qmmouth
	integer	isub1,iflow1,imon,NQSsf(12),NQSall,l1,l2,n_sub,igrid1
      real			x0,y0,s
      integer			inr,inc
	character*80	derout
	character*2		chmon
	real			wout(max_nr,max_nc)

	CALL strlen(Derive_D,l1,l2)
	call Read_GEO_hrd(GEO_hrd,nr,nc,x0,y0,s)

	Rough	=0.035
	QSall	=0.
	NQSall	=0
	do imon=1,12
		NQSall=NQSall+NQSsf(imon)
		do iflow1=1,max_flow
		do isub1=1,max_sub		
			QSall(isub1,iflow1)=QSall(isub1,iflow1)+
     $			QSsubflow(isub1,iflow1,imon)
		enddo
		enddo
	enddo	
		
	Qmmouth = QSall(n_sub,1) / NQSall
	Qmmouth = max ( Qmmouth , 1.0)
	Zmon = (6.0 + Qmmouth*1.0E-4)
	do iflow1=1,max_flow 
	do isub1=1,max_sub		
		Qmyear=QSall(isub1,iflow1)/NQSall
		Wmon(isub1,iflow1) = max (25.0 , Zmon* (Qmyear**0.5) )
	enddo
	enddo

	write(icho6,err=520, rec=1)	Wmon
		do isub1=1,n_sub
		do iflow1=1,sub(isub1).nflow
		do igrid1=1,sub(isub1).flow(iflow1).ngrid
			inr = sub(isub1).flow(iflow1).gridxy(igrid1,1)
			inc = sub(isub1).flow(iflow1).gridxy(igrid1,2)	
			wout(inr,inc)= Wmon(isub1,iflow1)
		enddo
		enddo
		enddo
		derout=""
		derout=Derive_D(l1:l2)//"RivB.asc"
		CALL writefile_float(derout,nr,nc,x0,y0,s,-9999.,
     $	wout,max_nr,max_nc)

	do iflow1=1,max_flow 
	do isub1=1,max_sub		
		MWDsf(isub1,iflow1) =MWDsf(isub1,iflow1) / 1000.	!(mm->m)
		MWDsf(isub1,iflow1) = max (MWDsf(isub1,iflow1) , 1.0 )
		MWDsf(isub1,iflow1) = min (MWDsf(isub1,iflow1) , 10.0 )
	enddo
	enddo
	write(icho6,err=520, rec=2)	MWDsf	
		do isub1=1,n_sub
		do iflow1=1,sub(isub1).nflow
		do igrid1=1,sub(isub1).flow(iflow1).ngrid
			inr = sub(isub1).flow(iflow1).gridxy(igrid1,1)
			inc = sub(isub1).flow(iflow1).gridxy(igrid1,2)	
			wout(inr,inc)= MWDsf(isub1,iflow1) 
		enddo
		enddo
		enddo
		derout=""
		derout=Derive_D(l1:l2)//"RivH.asc"
		CALL writefile_float(derout,nr,nc,x0,y0,s,-9999.,
     $	wout,max_nr,max_nc)

	write(icho6,err=520, rec=3)	Rough
		do isub1=1,n_sub
		do iflow1=1,sub(isub1).nflow
		do igrid1=1,sub(isub1).flow(iflow1).ngrid
			inr = sub(isub1).flow(iflow1).gridxy(igrid1,1)
			inc = sub(isub1).flow(iflow1).gridxy(igrid1,2)	
			wout(inr,inc)= Rough(isub1,iflow1)
		enddo
		enddo
		enddo
		derout=""
		derout=Derive_D(l1:l2)//"RivN.asc"
		CALL writefile_float(derout,nr,nc,x0,y0,s,-9999.,
     $	wout,max_nr,max_nc)

	WRITE(icho1, *) "SAVE derived river PARAMETERS"
	PRINT *, "SAVE derived river PARAMETERS"

	go to 521
520	WRITE(icho1, *) "CANNOT SAVE derived river PARAMETERS TO FIEL"
	PRINT *, "CANNOT SAVE derived river PARAMETERS TO FIEL"

521	End !Subroutine Q_derived
