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
	include './SUBROUTINE/GET_FPAR.f'

	include '../INCLUDE/common.inc'
		
	integer			year,month,ID,KER,i,j
	CHARACTER*80	tmpname
	CHARACTER*6		MMYYYY
	real			fpar(max_nr,max_nc)

	integer			INTPLT,NDVIID
	character*80	ATM_ITP,GEO_hrd,LambNDVI
	common			/Get_Grid_ATM_para/
     $				INTPLT,NDVIID,ATM_ITP,GEO_hrd,LambNDVI
	character*80	FPAR_AnciDir,FPAR_Lat,FPAR_Lon,FPAR_dir,FPAR_Pre
	integer			FPAR_D_nc,FPAR_D_nr,FPAR_S_c,FPAR_S_r,FPAR_S_nc
	integer			FPAR_S_nr,FPAR_N_div
	common			/Read_FPAR_para/FPAR_AnciDir,FPAR_Lat,FPAR_Lon,
     $				FPAR__dir,FPAR_Pre,FPAR_D_nc,FPAR_D_nr,FPAR_S_c,
     $				FPAR_S_r,FPAR_S_nc,FPAR_S_nr,FPAR_N_div

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
		call GET_FPAR(year,month,fpar,KER)

c	Call ConvMMYYYY(month,year,MMYYYY)
c		tmpname=MMYYYY//'_fpar.xyz'

c	call writefile_float (tmpname,max_nr,max_nc,x0,y0,s,
c     $	-9999.0,fpar,max_nr,max_nc)

c export to xyz file for GMT to make animation
c      open(1,file=tmpname,status='unknown')
c	do i =1, nr
c		do j =1 ,nc
c			write (1, "(3f12.3)") real(j), real(max_nr+1-i), fpar(i,j)
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