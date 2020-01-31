	include './CHARACTER/Strlen.f'
	include './CHARACTER/ConvI2C.f'
	include './CHARACTER/ConvYYMM.f'
	include './CHARACTER/ConvMMYYYY.f'
	include './GEO_REFER/Patch_Tiled_file.f'
	include './GEO_REFER/Get_Goode_coordinate.f'
	include './GEO_REFER/Convert_Lambert_latlon.f'	
	include './GEO_REFER/Get_Ancil_latlon.f'	
	include './GEO_REFER/Abstract_SA.f'	
	include './RW_FILE/R_ancillary_file.f'
	include './RW_FILE/RW_ArcInfo_Ascii.f'
	include './RW_FILE/R_GEO_hrd.f'
	include './RW_FILE/RW_Real_Binary.f'
	include './RW_FILE/R_parameters.f'
	include './RW_FILE/R_FPAR_SA.f'
	include './SUBROUTINE/GET_LAI.f'

	include '../INCLUDE/common.inc'
		
	integer			year,month,ID,KER,i,j
	CHARACTER*50	tmpname
	CHARACTER*6		MMYYYY
	real			LAI(max_nr,max_nc)

	integer			INTPLT,NDVIID
	character*80	ATM_ITP,GEO_hrd,LambNDVI
	common			/Get_Grid_ATM_para/
     $				INTPLT,NDVIID,ATM_ITP,GEO_hrd,LambNDVI
	character*80	LAI_AnciDir,LAI_Lat,LAI_Lon,LAI_dir,LAI_Pre
	integer			LAI_D_nc,LAI_D_nr,LAI_S_c,LAI_S_r,LAI_S_nc
	integer			LAI_S_nr,LAI_N_div
	common			/Read_LAI_para/LAI_AnciDir,LAI_Lat,LAI_Lon,
     $				LAI_dir,LAI_Pre,LAI_D_nc,LAI_D_nr,LAI_S_c,
     $				LAI_S_r,LAI_S_nc,LAI_S_nr,LAI_N_div

	call Read_parameters(KER)
	call Read_GEO_hrd(GEO_hrd,nr,nc,x0,y0,s)
	StartY=1981
	StartM=7
	EndY=2001
	EndM=5


	DOIT=1
	year=StartY
	month=StartM
	DO While (DOIT.EQ.1) 
		KER=0
		print *, year, month
		call GET_LAI(year,month,LAI,KER)

c	Call ConvMMYYYY(month,year,MMYYYY)
c		tmpname=MMYYYY//'_LAI.xyz'
c	Call ConvMMYYYY(month,year,MMYYYY)
c	tmpname='asciigrid '//MMYYYY//'_LAI.asc '//MMYYYY//' float'
c	print *,tmpname

c	call writefile_float (tmpname,max_nr,max_nc,x0,y0,s,
c     $	-9999.0,LAI,max_nr,max_nc)
c      open(1,file=tmpname,status='unknown')
c	do i =1, nr
c		do j =1 ,nc
c			write (1, "(3f12.3)") real(j), real(max_nr+1-i), LAI(i,j)
c		enddo
c	enddo
c	close(1)

	if (month.EQ.EndM.and.year.EQ.EndY) DOIT=0
	month=month+1
	if (month.gt.12) then
		month=1
		year=year+1
	endif
	ENDDO

	end