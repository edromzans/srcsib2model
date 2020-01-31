c	This subroutine get raster coordinate under Goode projection
c	Input:	
c		reallat,reallon:	Array the ancillary lat,lon information saved
c		ipix_max,iscan_max:	Size of array lat,lon
c		latitude,longitude: The input latitude and longitude
c	Output:	nnr,nnc,raster coordinate under Goode projection
c	Methodology: 
c		1. Search latitude from north to south, get j (nnc)
c		2. Search longitude from west to east, get i (nnr)
c		if return 0, means the latitude,longitude is not in the map
	subroutine	Get_Goode_coordinate(reallat,reallon,
     $	ipix_max,iscan_max,latitude,longitude,nnr,nnc)	
	implicit none
	integer			ipix_max,iscan_max
	real			reallat(iscan_max,ipix_max)
	real			reallon(iscan_max,ipix_max)
	REAL			latitude,longitude,divlat,divlon
	INTEGER			nnr,nnc	,i,j,ilat,ilon
	nnr=0
	nnc=0			
	ilat=0	!record whether the latitude found or not
	ilon=0	!record whether the longitude found or not
	Do j=1,iscan_max
		If (reallat(j,1).lt.latitude.and.ilat.eq.0) Then
			if (j.gt.1) then
				divlat=(reallat(j,1)+reallat(j-1,1))/2.0
			else
				divlat=reallat(j,1)+(reallat(j+1,1)-reallat(j,1))/2.0
			endif
			If (latitude.gt.divlat) Then
				nnr = j - 1
			Else
				nnr = j
			Endif
			DO i=1,ipix_max
				if (nnr.gt.0) then
					IF (reallon(nnr,i).gt.longitude.and.ilon.eq.0) THEN
						if (i.gt.1) then
							divlon=(reallon(nnr,i)+reallon(nnr,i-1))/2.0
						else 
							divlon=reallon(nnr,i)-
     $							(reallon(nnr,i+1)-reallon(nnr,i))/2.0
						endif
						IF (longitude.lt.divlon) THEN
							nnc = i - 1
						ELSE
							nnc = i
						ENDIF
						ilon=1
					ENDIF
				else
					nnc=0
				endif
			ENDDO
			ilat=1
		endif
	Enddo

      end