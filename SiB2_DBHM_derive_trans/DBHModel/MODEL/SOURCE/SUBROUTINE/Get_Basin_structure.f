c	read the parameters of subbasin, saved in structure sub()
c	n_sub is the total number of sub-basin
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c	Get basin structure 
c	Input: 
c		levels:			the level of Pfafstetter code
c		dx_max:			the refered distance of flow intervals
c		code_file:		name of Pfafstetter code file
c		dis_file:		name of distance file, ArcInfo(flowlength)
c		slope_file:		name of slope file, ArcInfo(slope |PERCENTRISE)
c 		riverway_file:	name of riverway file for cross section parameters, roughness
c	Output:
c		n_sub:			the total number of the sub_basins
c		sub():			structure of sub_basin	
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
	subroutine Get_Basin_structure(n_sub,sub)	
      implicit none
	include '../INCLUDE/Def_Rriver_routing.inc'

	character*80	code_file,dis_file,dir_file,slope_file,dem_file
	character*80	area_file,frac_file,riverway_file,Outstn,Output_D
	character*80	Derive_D       
      integer	levels,startyear,endyear,startmont,startday,endmont,endday
	real			dx_max,dt,rivlen
	common			/Input_para/ levels,startyear,startmont,startday,
     $				endyear,endmont,endday,dx_max,
     $		code_file,dis_file,dir_file,slope_file,dem_file,area_file,
     $frac_file,riverway_file,Outstn,Output_D,Derive_D,dt,rivlen

	integer			INTPLT,NDVIID
	character*80	ATM_ITP,GEO_hrd,LambNDVI
	common			/Get_Grid_ATM_para/
     $				INTPLT,NDVIID,ATM_ITP,GEO_hrd,LambNDVI

	integer			l1,l2 
	character*80	inval_file

	integer			i,j,k,mm !,levels,n,nn,first
      real*4			x0,y0,znodata_f,s
      integer			znodata_i,nc,nr
      integer			code(max_nr,max_nc)
      integer			id, id_stop,n_sub
c	character*80	code_file,dis_file,slope_file,riverway_file
	real			slope(max_nr,max_nc)
	real			distance(max_nr,max_nc),dis_min,dis_max !,dx_max
	integer			isub,subexist,subgrid ,iflow1 !,n_flow
	integer			coordxy(10000,2),interval(max_nr,max_nc)
	integer			level
c	integer			digit(10)
	real			width(10),height(10),rough(10)
	real			totlen,kriv
	integer			imain,icode,iflow

	id=10**levels-1
	id_stop=10**(levels-1)
	call readfile_int(code_file,nr,nc,x0,y0,s,znodata_i,code,max_nr,max_nc)	    
      do i=1,nr
      	do j=1,nc
	      	if (code(i,j).gt.id) then
	      		code(i,j)=mod(code(i,j),id+1)
	      	end if      	
      	end do
      end do
	
	call readfile_float(dis_file,nr,nc,x0,y0,s,znodata_f,
     $	distance,max_nr,max_nc)


	call readfile_float(slope_file,nr,nc,x0,y0,s,znodata_f,
     $	slope,max_nr,max_nc)

c	read width,height,and roughness set for riverway level
	open(1,file=riverway_file,status='old')
		do i=1,levels+1
			read (1,*) level,width(level),height(level),rough(level)
		end do
	close(1)
		
	isub=1
      do i=id,id_stop,-1
		subexist=0
		do mm=1,10000
			coordxy(mm,1)=0
			coordxy(mm,2)=0
		end do

c get grids in sub_basin(i), save the coordinate into array coordxy
		subgrid=0
      	do j=1,nr
      		do k=1,nc
      			if (code(j,k).eq.i) then
					subexist=1
					if (subgrid.eq.0) then
						dis_min=distance(j,k)
						dis_max=distance(j,k)
					else
						if (distance(j,k).lt.dis_min) dis_min=distance(j,k)
						if (distance(j,k).gt.dis_max) dis_max=distance(j,k)
					end if
					subgrid=subgrid+1
					coordxy(subgrid,1)=j
					coordxy(subgrid,2)=k	
      			end if
      		end do
      	end do 

		if (subexist.eq.1) then

			sub(isub).code=i

c if the sub_basin is not NULL, the get the flow interval according to distance and dx_max,
c and calculate the averaged paprameters for each flow interval			
			call cal_interval(levels,sub,dx_max,isub,subgrid,
     $			dis_min,dis_max,coordxy,distance,slope,interval,s)

c decide the level of riverway according to the Pfafstetter code
			call cal_level(levels,sub,isub,level)
c according to level of riverway, decide the width,heigth,and roughness for riverway
			do iflow1=1,sub(isub).nflow
				sub(isub).flow(iflow1).width=width(level)
				sub(isub).flow(iflow1).height=height(level)
				sub(isub).flow(iflow1).rough=rough(level)
			enddo
						
			n_sub=isub
			isub=isub+1     			
		end if
      end do

	totlen =0.0
	do  isub=1,n_sub
		imain=0
		do icode=1,levels
			if (mod(sub(isub).digit(icode),2).eq.0.and.
     $			sub(isub).digit(icode).ne.0) imain=1
		enddo
		if (imain.eq.0) then
			do  iflow=sub(isub).nflow,1,-1
				totlen =totlen + sub(isub).flow(iflow).dis
			enddo
		endif
	enddo
	kriv = rivlen/totlen
	do  isub=1,n_sub
		do  iflow=sub(isub).nflow,1,-1
			sub(isub).flow(iflow).dis=sub(isub).flow(iflow).dis * kriv
		enddo
	enddo

	call strlen(Derive_D,l1,l2)
	inval_file=Derive_D(l1:l2)//"interval.asc"
	call Read_GEO_hrd(GEO_hrd,nr,nc,x0,y0,s)
	CALL writefile_int(inval_file,nr,nc,x0,y0,s,0,
     $	interval,max_nr,max_nc)
	end 


c To get the flow interval according to distance and dx_max,
c and calculate the averaged paprameters for each flow interval	
	subroutine cal_interval(levels,sub,dx_max,isub,subgrid,
     $	dis_min,dis_max,coordxy,distance,slope,interval,s)
      implicit none
	include '../INCLUDE/Def_Rriver_routing.inc'
	integer	isub,n_flow,subgrid,levels !,subexist,iflow
	integer	coordxy(10000,2),interval(max_nr,max_nc)
	real	slope(max_nr,max_nc)
	real	distance(max_nr,max_nc),dis_min,dis_max,dx_max,s
	real	dis_min2

	integer	nn,n

c	Change dis_min to dis_min2, the distance to next grid of the outlet
	dis_min2=dis_min-s

	n_flow=Int(dis_max-dis_min2)/dx_max+1
	sub(isub).nflow=n_flow	

c Get the interval number according to distance to outlet			
	do n=1,subgrid
		interval(coordxy(n,1),coordxy(n,2))=
     $		Int((distance(coordxy(n,1),coordxy(n,2))-dis_min2)
     $		/dx_max)+1
	end do	
				
c Calculate averaged parameters according interval number
c e.g. All the first flow interval grids (=1) are averaged
	do n=1,subgrid
		do nn=1,n_flow
			if(nn.eq.interval(coordxy(n,1),coordxy(n,2))) then
				sub(isub).flow(nn).dis = sub(isub).flow(nn).dis
     $					+distance(coordxy(n,1),coordxy(n,2))-dis_min2
				sub(isub).flow(nn).slope =sub(isub).flow(nn).slope
     $					+slope(coordxy(n,1),coordxy(n,2))
				sub(isub).flow(nn).ngrid=sub(isub).flow(nn).ngrid+1
				sub(isub).flow(nn).gridxy(sub(isub).
     $						flow(nn).ngrid,1)=coordxy(n,1)
				sub(isub).flow(nn).gridxy(sub(isub).
     $						flow(nn).ngrid,2)=coordxy(n,2)
			end if
		end do
	end do
	do nn=1,n_flow					
		sub(isub).flow(nn).dis = sub(isub).flow(nn).dis/ 
     $			sub(isub).flow(nn).ngrid
c	'Unit of slope :PERCENTRISE, then slope/100=rise/run, please see help of ArcInfo
		sub(isub).flow(nn).slope = sub(isub).flow(nn).slope/
     $			sub(isub).flow(nn).ngrid/100 

c	set initial values of the Qi1_j,Lj,Qi1_j1,Lj1,WDj,WDj1
c		sub(isub).flow(nn).Qi1_j =	0.1e-9 
c		sub(isub).flow(nn).Lj =		0.1e-9 
		sub(isub).flow(nn).Qi1_j1 = 0.1e-9
		sub(isub).flow(nn).Lj1 =	0.1e-9 
c		sub(isub).flow(nn).WDj =	0.1e-9 
		sub(isub).flow(nn).WDj1 =	0.1e-9 
	end do
	do nn=n_flow,2,-1
		sub(isub).flow(nn).dis =sub(isub).flow(nn).dis
     $			-sub(isub).flow(nn-1).dis
	end do

	end

c decide the level of riverway according to the Pfafstetter code, please see readme file
	subroutine cal_level(levels,sub,isub,level)
      implicit none
	include '../INCLUDE/Def_Rriver_routing.inc'
	integer	isub,level,levels
	integer	n,nn,mm,digit(10),first,i

c digit(n): digit of the code from ones place, e.g. for 729: digit(1)=9, digit(2)=2, digit(3)=7
	i=sub(isub).code
	do n=1, levels
		if (n.eq.1) then
			digit(n)=mod(i,10**n)
		else
			digit(n)=(mod(i,10**n)-mod(i,10**(n-1)))/10**(n-1)
		endif
		sub(isub).digit(n)=digit(n)
	enddo			

	level=1
	nn=0
	do	n=1,levels
		if ((mod(digit(n),2).eq.1.or.digit(n).eq.0).and.nn.eq.0) then
			if (digit(n).eq.9) then
				first=0
				do mm=n,levels
					if (digit(mm).ne.9.and.first.eq.0) then
						if(mod(digit(mm),2).eq.0) then
							nn=1
						else
							level=level+1
						endif
						first=1
					endif
				enddo
			else
			level=level+1
			endif
		else
			nn=1
		end if
	enddo

	end