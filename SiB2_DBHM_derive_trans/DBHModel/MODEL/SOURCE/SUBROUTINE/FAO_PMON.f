c	Calculate daily Evapoaration according to FAO_Penman_monteith method
c	The meaning of parameters refers to 
c	URL:http://www.fao.org/docrep/X0490E/x0490e00.htm#Contents
c	Comments (E21) means Equation 21 in the reference
c	In order to use the subroutine easily, the as,bs,alpha,r_a, r_s is input
c	If ID=1 THEN
c		 RETURN reference ET
c	Else
c		calculate from input as,bs,alpha,r_a, r_s
	subroutine FAO_PMON_day(Lat,Alt,DJ,DN,as,bs,alpha,r_a,r_s,
     $	tm,tmax,tmin,um,n_summ,fsm,rsum,sun,ID,ET)
      implicit none
	Real	r_a,r_s			!Aerodynamics resistance(r_a),Surface resist(r_s)
	Real	Lat,Alt			!Latitude (DD), Altitude (m)
	Real	as,bs,alpha		!Fraction of extraterrestrial radiation, Albedo
	Integer	DJ,DN,ID		!Number of the day in the year between 1 and 365/6
	Real	tm,tmax,tmin,um,n_summ,fsm,rsum,sun

	real	Ra,Gsc,dr,ws,varphi,delta,pi				!(E21)
	real	N,Rs,Rns,sunN								!(E38)
	real	sigmaT,eoTmax,eoTmin,ea,Rso,Rnl,RsRso		!(E39)
	real	Rn											!(E40)
	real	Gday										!(E42)
	real	Dlt											!(E13)
	real	es,es_ea									!(E12)
	real	P,gamma										!(E8)
	real	ET,lambda,pacp,lamET						!(E3, E6 and Box6)
	real	d,zom,zoh

	IF (ID.EQ.1) THEN	!Calculate Reference ET
		as=0.25
		bs=0.5
		alpha=0.23
c	Calculate surface resistance (r_s)
		r_s=100.0/(0.5*24.0*0.12)
c	Calculate Aerodynamic resistance (r_a)				!(E4)
		d=0.12*2.0/3.0
		zom=0.123*0.12
		zoh=0.1*zom
		r_a=log((2.0-d)/zom)*log((2.0-d)/zoh)/(0.41**2*fsm)
	ENDIF

c	Calculate Extraterrestrial radiation for daily periods (Ra) (E21)
	pi=atan(1.0)*4.0
	Gsc=0.0820
	dr=1.0+0.033*cos(2*pi*real(DJ)/real(DN))			!(E23)
	delta=0.406*sin(2*pi*real(DJ)/real(DN)-1.39)		!(E24)
	varphi=Lat*pi/180.0									!(E22)
	ws=acos(-tan(varphi)*tan(delta))					!(E25)
	Ra=24.0*60.0/pi*Gsc*dr*
     $	(ws*sin(varphi)*sin(delta)+cos(varphi)*cos(delta)*sin(ws))

c	Calculate Net solar or net shortwave radiation (Rns) (E38)	
	N=24.0/pi*ws										!(E34)
	sunN=sun/N
	if (sunN.lt.0.0) sunN=1-n_summ
	if (sunN.lt.0.0) THEN
		sunN=0.5
		PRINT*,'SUNSHINE=',sun,'CLOUD=',n_summ
		PRINT*,'WARNING: SUNSHINE DATA/ CLOUD DATA ERROR. SET TO 0.5.'
	endif
	if (sunN.gt.1.0) sunN=1.0
	Rs=(as+bs*sunN)*Ra									!(E35)
	Rns=(1.0-alpha)*Rs									!(E38)

c	Calculate Net longwave radiation (Rnl) (E39)
	eoTmax=0.6108*exp(17.27*Tmax/(Tmax+237.3))			!(E11)
	eoTmin=0.6108*exp(17.27*Tmin/(Tmin+237.3))
	ea=um*(eoTmax+eoTmin)/2.0							!(E19)
c For near sea level or when calibrated values for as and bs are available:
c	Rso=(as+bs)*Ra										!(E36)
c When calibrated values for as and bs are not available:
	Rso=(0.75+2.0*Alt/(10.0**5))*Ra						!(E37)
	sigmaT=4.903E-9*((Tmax+273.16)**4+(Tmin+273.16)**4)/2.0
	RsRso=Rs/Rso
	If (RsRso.gt.1.0) then
		PRINT *, 'Rs',Rs,'Rso',Rso,'Ra',Ra,'Rs/Rso is set to 1.0.'
		RsRso=1.0
	Endif
	Rnl=sigmaT*(0.34-0.14*sqrt(ea))*(1.35*RsRso-0.35)	!(E39)

c	Calculate Net radiation (Rn) (E40)
	Rn=Rns-Rnl											!(E40)

c	Calculate Soil heat flux (G) (E42)
	Gday=0.0											!(E42)

c	Calculate Slope of saturation vapour pressure curve (Dlt) (E13)
	Dlt=2049.0*(eoTmax/(Tmax+237.3)**2.0+eoTmin/(Tmin+237.3)**2.0)
	
c	Calculate Vapour pressure deficit (es_ea)
	es=(eoTmax+eoTmin)/2.0								!(E12)
	es_ea=es-ea	

c	Calculate Atmospheric parameters
	P=101.3*((293.0-0.0065*Alt)/293.0)**5.26			!(E7)
	gamma=0.665E-3*P									!(E8)

c	Calculate evapotranspiration (ET) (E3, E6 and Box6)
	lambda=2.45
	pacp=86400.0*lambda*gamma*0.622/(tm+273.16)/1.01/0.287
	lamET=(Dlt*(Rn-Gday)+pacp*es_ea/r_a)/(Dlt+gamma*(1.0+r_s/r_a))
	ET=lamET/lambda
	RETURN
	end


