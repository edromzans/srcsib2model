c	This file declare the common variables in the program where river routing is needed
c	e.g. structure sub(), nsub, etc. These variables are commonly used in every subroutine  

	include 'common.inc'

c subparameter save the structure iflow interval of sub_basin
c nflow: number of flow intervals of the sub-basin
c width,height of riverway cross section, rough: Manning's roughness coefficient
c	'According to subbasin_coding, set the width,height,and roughness
c	'e.g. basin 313 is in the mainstem, use level4's parameters, see readme file.
c flowparameter save the structure of flow interval
c ngrid: number of the grids of the iflow interval
c gridxy: gridxy(m,1) save x (nr) of the grid, (m,2) save y(nc) of the grid
c dis: distance (length) of the flow interval  
c slope: averaged bedslope of the flow interval 
c	'Unit of slope :PERCENTRISE, then slope/100=rise/run, please see help of ArcInfo
c	'Under grid, Usage:  (F) SLOPE (<grid>, {DEGREE | PERCENTRISE})
c	'Qi1_j,Lj,Qi1_j1,Lj1,WDj,WDj1, see "River Routing Model (Kinematic Wave method)"
	structure /subparameter/
		integer		code,digit(10)
		integer		nflow
		structure /flowparameter/ flow (max_flow)
			real		width,height,rough		
			integer		ngrid
			integer		gridxy(max_grid,2)
			integer		nirrg
			integer		irrxy(max_irrg,2)
			real		dis,slope
c			real Qi1_j,Lj,Qi1_j1,Lj1,WDj,WDj1,accarea
			real Qi1_j1,Lj1,WDj1
		end structure
	end structure
	record	/subparameter/ sub(max_sub)

c	integer			n_sub

