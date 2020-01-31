	include	'./SUBROUTINE/FAO_PMON.f'
	include './RW_FILE/RW_ArcInfo_Ascii.f'
	include './RW_FILE/R_GEO_hrd.f'
	include './RW_FILE/RW_Real_Binary.f'
	include './RW_FILE/R_Interpolate_data.f'
	include './RW_FILE/R_ATM_data.f'
	include	'./RW_FILE/R_parameters.f'
	include	'./INTERPOLATE/ThinPlateSplines2.f'
	include './INTERPOLATE/InverseDistance.f'
	include './INTERPOLATE/ThiessenPolygon.f'
	include './INTERPOLATE/Interpolate_tm.f'
	include './INTERPOLATE/Interpolate_tmax.f'
	include './INTERPOLATE/Interpolate_tmin.f'
	include './INTERPOLATE/Interpolate_um.f'
	include './INTERPOLATE/Interpolate_n_summ.f'
	include './INTERPOLATE/Interpolate_fsm.f'
	include './INTERPOLATE/Interpolate_rsum.f'
	include './INTERPOLATE/Interpolate_sun.f'
	include './GEO_REFER/Convert_Lambert_latlon.f'
	include './TEMPORAL/Days_S_E.f'
	include './CHARACTER/Strlen.f'
	include	'./SUBROUTINE/Get_Grid_ATM.f'
	include	'./SUBROUTINE/Get_potential_ET.f'

	implicit none
	include '../INCLUDE/common.inc'
	include '../INCLUDE/Def_Parameters.inc'
	integer	KER,i
	real	ET(max_time,max_nr,max_nc)

	call Read_parameters(KER)
	do i=1951,1993
		call Get_potential_ET(i,ET)
	enddo
	end

