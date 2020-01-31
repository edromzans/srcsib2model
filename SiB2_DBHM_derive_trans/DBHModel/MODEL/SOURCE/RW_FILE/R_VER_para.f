c Read all the parameters in 'SIB2.par'
c Set them as common paramters
c Call this subroutine when data analysis is required
	subroutine Read_VER_para(KER)
	implicit none
	integer	KER

	character*80	obv_dir,out_soil
	real			layo1         
	common			/Verf_OBV/ obv_dir,out_soil,layo1
	namelist		/Verf_OBV/ obv_dir,out_soil,layo1

	integer	GMT_NNC,GMT_WID,GMT_HGH,GMT_DDX,GMT_DDY,GMT_IDX,GMT_IDY
	real	GMT_r1,GMT_r2,GMT_r3,GMT_r4,GMT_cx,
     $		GMT_cxx,GMT_cy,GMT_cyy,GMT_tyy
	common		/GMT_SET_VER/ GMT_NNC,GMT_WID,GMT_HGH,GMT_DDX,
     $	GMT_DDY,GMT_IDX,GMT_IDY,
     $	GMT_r1,GMT_r2,GMT_r3,GMT_r4,GMT_cx,
     $	GMT_cxx,GMT_cy,GMT_cyy,GMT_tyy
	namelist	/GMT_SET_VER/ GMT_NNC,GMT_WID,GMT_HGH,GMT_DDX,
     $	GMT_DDY,GMT_IDX,GMT_IDY,
     $	GMT_r1,GMT_r2,GMT_r3,GMT_r4,GMT_cx,
     $	GMT_cxx,GMT_cy,GMT_cyy,GMT_tyy

	KER=0

	open(1, file='../PARAMETER/Verification.par' ,status='old',err=144)
		read (1, NML=Verf_OBV, err=244)
		read (1, NML=GMT_SET_VER, err=344)
	close(1)
			
	RETURN

144	KER=1
	PRINT *, 'ERROR IN OPEN FILE ../PARAMETER/Verification.par.'
	RETURN
244	KER=2
	PRINT *, 'ERROR IN READING Verification.par. Verf_OBV'
	RETURN
344	KER=3
	PRINT *, 'ERROR IN READING Verification.par. GMT_SET_VER'
	RETURN
	end
