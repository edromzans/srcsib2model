c Read all the parameters in 'ANALYSIS.par'
c Set them as common paramters
c Call this subroutine when data analysis is required
	subroutine Read_ANA_para(KER)
	implicit none
	integer	KER

	character*80	NDVI_ANA         
	common			/NDVI_ANALYSIS/ NDVI_ANA

	namelist		/NDVI_ANALYSIS/ NDVI_ANA

	KER=0

	open(1, file='../PARAMETER/ANALYSIS.par' ,status='old',err=44144)
		read (1, NML=NDVI_ANALYSIS, err=44244)
	close(1)
			
	RETURN

44144	KER=1
	PRINT *, 'ERROR IN OPEN FILE ../PARAMETER/ANALYSIS.par'
	RETURN
44244	KER=2
	PRINT *, 'ERROR IN READING NDVI_ANALYSIS.'
	RETURN
	end
