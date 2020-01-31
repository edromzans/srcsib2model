	
	real pi,lambda,phi,lambda0,phi1,x,y

	pi = 3.1415926
	lambda = 48.40/180.0*pi
	phi = 38.30/180.0*pi

	lambda0 = 100.0/180.0*pi
	phi1 = 45.0/180.0*pi

	r = 6370997.0

	id =0
	call Convert_Lambert_latlon(x,y,lambda,phi,lambda0,phi1,r,id)

	print *, x ,y

	end
c This subroutine is used to convert coordinate between Lambert Azimuthal Equal-Area Projection and latitude and longitude
c The equation is refered to http://mathworld.wolfram.com/LambertAzimuthalEqual-AreaProjection.html
c x,y:			coordinate under Lambert Azimuthal Equal-Area Projection
c lambda,phi:		longitude,latitude
c lambda0,phi1:	the central longitude, the standard parallel
c r:				Radius of Sphere of Influence
c id:				When id=0 THEN lambda,phi->x,y ELSE x,y->lambda,phi
	subroutine Convert_Lambert_latlon(x,y,lambda,phi,lambda0,phi1,r,id)
	implicit none
	real	x,y,lambda,phi,lambda0,phi1,r,k1,tmp,rho,c,tmp2
	integer id

	if (id.eq.0) then
c	lambda,phi->x,y
		tmp=1.0+sin(phi1)*sin(phi)+cos(phi1)*cos(phi)*cos(lambda-lambda0)
		k1=sqrt(2.0/tmp)
		x=k1*cos(phi)*sin(lambda-lambda0)*r
		y=k1*(cos(phi1)*sin(phi)-sin(phi1)*cos(phi)*cos(lambda-lambda0))*r
	else
c	x,y->lambda,phi
	tmp2=x*x+y*y
		if (tmp2.gt.0.0) then
			rho=sqrt(tmp2)
			c=2.0*asin(rho/r/2.0)
			phi=asin(cos(c)*sin(phi1)+y*sin(c)*cos(phi1)/rho)
			tmp=rho*cos(phi1)*cos(c)-y*sin(phi1)*sin(c)
			lambda=lambda0+atan(x*sin(c)/tmp)
		else
			phi=phi1
			lambda=lambda0
		endif
	endif
	end