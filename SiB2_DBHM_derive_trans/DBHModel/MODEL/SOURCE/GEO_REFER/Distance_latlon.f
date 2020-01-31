c This function return the distance of two point according to the longitude and latitude
c In current model, this function is not used.  (2004/11/03)
c The longitude and latitude is in radians
c r: Radius of Sphere of Influence 
	real function distance(long1,lat1,long2,lat2,r)
	implicit none
	real long1,lat1,long2,lat2,r
	real tmp1,tmp2,tmp3
	tmp1=cos(lat1)*cos(long1)*cos(lat2)*cos(long2)
	tmp2=cos(lat1)*sin(long1)*cos(lat2)*sin(long2)
	tmp3=sin(lat1)*sin(lat2)
	distance=acos(tmp1+tmp2+tmp3)*r
	return
	end
