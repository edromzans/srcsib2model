c	Read  Goode projection ancillary files, get Dlat and DLon array
c	Input:	Read the Read_NDVI_para from '../PARAMETER/Input_para'
c	Output:	Dlat,DLon Array
	subroutine Get_Ancil_latlon(reallat,reallon)
	include '../INCLUDE/common.inc'
	integer			ID,ipix,iscan
	CHARACTER*80	FileLat,FileLon
	CHARACTER*2		CharR0,CharR1,CharC0,CharC1
	integer			l1,l2,ll1,ll2,s1,s2,KER
	real			reallat(ndvi_nr,ndvi_nc),reallon(ndvi_nr,ndvi_nc)

	character*80	DAnciDir,DLat,DLon,DSuffix,D_dir,DPre
	integer			Col0,Col1,Row0,Row1,DN_div
	common			/Read_NDVI_para/DAnciDir,DLat,DLon,DSuffix,
     $				Col0,Col1,Row0,Row1,D_dir,DPre,DN_div

	ipix=(Col1-Col0+1)*125
	iscan=(Row1-Row0+1)*125
	Call Strlen(DAnciDir,l1,l2)
	Call Strlen(DSuffix,s1,s2)
	call ConvI2C(Row0,CharR0)
	call ConvI2C(Row1,CharR1)
	call ConvI2C(Col0,CharC0)
	call ConvI2C(Col1,CharC1)
	Call Strlen(Dlat,ll1,ll2)
	FileLat=DAnciDir(l1:l2)//Dlat(ll1:ll2)//
     $	CharC0//CharR0//CharC1//CharR1//DSuffix(s1:s2)
	Call Strlen(DLon,ll1,ll2)
	FileLon=DAnciDir(l1:l2)//DLon(ll1:ll2)//
     $	CharC0//CharR0//CharC1//CharR1//DSuffix(s1:s2)
	open(1, file=FileLat ,status='old',ERR=8000)
	close(1)
	open(1, file=FileLon ,status='old',ERR=8000)
	close(1)
	GOTO 8001
8000	PRINT *, 'Patch up Tiled Dlat,DLon files'
	ID=2
c	Patch up Tiled Dlat,DLon files if they are not existing
	call Patch_Tiled_file(DAnciDir,Dlat,DSuffix,
     $	Col0,Col1,Row0,Row1,1,ID,KER)
	call Patch_Tiled_file(DAnciDir,DLon,DSuffix,
     $	Col0,Col1,Row0,Row1,1,ID,KER)
c	If the files are patched, use the existing files
8001	call Read_ancillary_file(FileLat,FileLon,
     $	ndvi_nc,ndvi_nr,reallat,reallon,ipix,iscan)
	END