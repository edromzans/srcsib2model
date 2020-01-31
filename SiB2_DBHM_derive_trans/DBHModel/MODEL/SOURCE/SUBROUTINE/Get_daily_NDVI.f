c	This subroutine get NDVI data under Lambert Projection
c	Input:
c		Year/Month:		The Year/Month when the NDVI is required
c		reallat,reallon:Ancillary DLat/DLon (from Get_Ancil_latlon)
c	Output:
c		ndvi(T,nr,nc):	Daily NDVI in the required month
c		KER:			KER=0, successful, KER.gt.0, Fail			
	subroutine Get_daily_NDVI(Year,month,NT,ndvi,reallat,reallon,KER)
	include '../INCLUDE/common.inc'
	integer			NT,ID	
	CHARACTER*80	NDVI_file,DSuffix2
	integer			year,month
	character*6		MMYYYY
	integer			DayM(12)
	data			DayM /31,28,31,30,31,30,31,31,30,31,30,31/
	CHARACTER*2		CharR0,CharR1,CharC0,CharC1
	integer			l1,l2,ll1,ll2,s1,s2,KER
	real			reallat(ndvi_nr,ndvi_nc),reallon(ndvi_nr,ndvi_nc)
	real			ndvi(ndvi_nt,max_nr,max_nc)

	integer			INTPLT,NDVIID
	character*80	ATM_ITP,GEO_hrd,LambNDVI
	common			/Get_Grid_ATM_para/
     $				INTPLT,NDVIID,ATM_ITP,GEO_hrd,LambNDVI
	character*80	DAnciDir,DLat,DLon,DSuffix,D_dir,DPre
	integer			Col0,Col1,Row0,Row1,DN_div
	common			/Read_NDVI_para/DAnciDir,DLat,DLon,DSuffix,
     $				Col0,Col1,Row0,Row1,D_dir,DPre,DN_div

	KER=0
	call Read_GEO_hrd(GEO_hrd,nr,nc,x0,y0,s)

	call ConvI2C(Row0,CharR0)
	call ConvI2C(Row1,CharR1)
	call ConvI2C(Col0,CharC0)
	call ConvI2C(Col1,CharC1)

	Call Strlen(D_dir,l1,l2)	
	Call Strlen(DPre,ll1,ll2)	
	Call ConvMMYYYY(month,year,MMYYYY)
	DSuffix2='.'//MMYYYY//'.bin'
	Call Strlen(DSuffix2,s1,s2)
	NDVI_file=D_dir(l1:l2)//DPre(ll1:ll2)
     $	//CharC0//CharR0//CharC1//CharR1//DSuffix2(s1:s2)

	open(1, file=NDVI_file ,status='old',ERR=8005)
	close(1)
	GOTO 8006
c	Patch up Tiled files to one file
8005	PRINT *, 'Patch up Tiled files to one file.'
	ID=1
	Call Patch_Tiled_file(D_dir,DPre,DSuffix2,
     $		Col0,Col1,Row0,Row1,NT,ID,KER)
c	If the files are patched, use the existing files
c	Resample to Lambert projection
8006	call R_NDVI_SA(NDVI_file,nr,nc,x0,y0,s,DN_div,
     $		reallat,reallon,NT,ndvi,KER)
	end

