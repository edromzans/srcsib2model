c Read all the parameters in 'SIB2.par'
c Set them as common paramters
c Call this subroutine when data analysis is required
	subroutine Read_SIB2_para(KER)
	implicit none
	integer	KER

	character*80	vege_file,soil_file,soil_para,slpsib_f,sibpp         
	common		/SIB2_LSM/ vege_file,soil_file,soil_para,slpsib_f,sibpp
	namelist	/SIB2_LSM/ vege_file,soil_file,soil_para,slpsib_f,sibpp

	character*80	irriratio_file,irrcode_file,irrpro_file 
	common		/Water_withdraw/ irriratio_file,irrcode_file,irrpro_file
	namelist	/Water_withdraw/ irriratio_file,irrcode_file,irrpro_file

	KER=0

	open(1, file='../PARAMETER/SIB2.par' ,status='old',err=144)
		read (1, NML=SIB2_LSM, err=244)
		read (1, NML=Water_withdraw, err=344)
	close(1)
			
	RETURN

144	KER=1
	PRINT *, 'ERROR IN OPEN FILE ../PARAMETER/SIB2.par.'
	RETURN
244	KER=2
	PRINT *, 'ERROR IN READING SIB2.par.'
	RETURN
344	KER=3
	PRINT *, 'ERROR IN READING Water_withdraw.'
	RETURN
	end
