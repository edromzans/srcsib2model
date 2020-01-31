	include	'./SUBROUTINE/FAO_PMON.f'
	include './RW_file/RW_ArcInfo_Ascii.f'
	include './RW_file/R_GEO_hrd.f'
	include './RW_file/RW_Real_Binary.f'
	include './RW_file/R_Interpolate_data.f'
	include './RW_file/R_ATM_data.f'
	include	'./RW_file/R_parameters.f'
	include	'./Interpolate/ThinPlateSplines2.f'
	include './Interpolate/InverseDistance.f'
	include './Interpolate/ThiessenPolygon.f'
	include './Interpolate/Interpolate_tm.f'
	include './Interpolate/Interpolate_tmax.f'
	include './Interpolate/Interpolate_tmin.f'
	include './Interpolate/Interpolate_um.f'
	include './Interpolate/Interpolate_n_summ.f'
	include './Interpolate/Interpolate_fsm.f'
	include './Interpolate/Interpolate_rsum.f'
	include './Interpolate/Interpolate_sun.f'
	include './GEO_refer/Convert_Lambert_latlon.f'
	include './Temporal/Days_S_E.f'
	include './CHARACTER/Strlen.f'
	include	'./SUBROUTINE/Get_Grid_ATM.f'
	include	'./SUBROUTINE/Get_potential_ET.f'

	implicit none
	include '../INCLUDE/common.inc'
	include '../INCLUDE/Def_Parameters.inc'
	integer	KER,i
	real	ET(max_time,max_nr,max_nc)

	call Read_parameters(KER)
	do i=1980,2000
		call Get_potential_ET(i,ET)
	enddo
	end

