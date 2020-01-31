c According to the Pfafstetter system, give a isub, found the 2 subs that contribute flow to isub
c Input: isub		Output: insub1,insub2  output the two sub_id
c For the sub that no sub contributes flow to, output sub_id=0, e.g. sub.code=122, insub1=insub2=0
	subroutine inflow_subs(levels,sub,isub,insub1,insub2)
	implicit none
	include '../INCLUDE/Def_Rriver_routing.inc'
	integer	insub1,insub2,levels
	integer isub,i,j,nn,digi,ndigi,outnum(2)

	nn=0
	do 234 i=levels,1,-1
	if (nn.eq.0) then
		if (mod(sub(isub).digit(i),2).eq.1.and.sub(isub).digit(i).ne.9) then
			if (sub(isub-1).digit(i).ne.sub(isub).digit(i)) then
				ndigi=0
				digi=sub(isub).digit(i)
				do j=isub,1,-1
					if (digi.ne.sub(j).digit(i).and.ndigi.lt.2) then
						ndigi=ndigi+1	
						outnum(ndigi)=j
						digi=sub(j).digit(i)			
					endif
				enddo
				nn=1
			endif
		else 
			if(i.eq.1) then
				outnum(1)=0
				outnum(2)=0
			endif
		endif
	endif
234	continue
	insub1=outnum(1)
	insub2=outnum(2)
	end

c  Kinematic Wave Model (Nonlinear Scheme using Newton's method)
c  Definition of Variales:
c
c  Timestep	Inflow (i)		Lateral		Outflow(i+1)	  WaterDepth
c	j		Qi_j(known)		Lj			Qi1_j(known)		WDj (known, mm)
c	j+1		Qi_j1(known)	Lj1			Qi1_j1(unknown)		WDj1 (unknown,mm)
c
c  P: Wetted perimeter; B: River width; S0: Riverbed slope; n:roughness
	subroutine	kinematic_wave(dt,dx,B,S0,n,WDj,
     $	Qi_j1,Lj1,Qi1_j1,WDj1)
	implicit none
	real	dt,dx,B,S0,n,WDj
	real	Qi_j1,Lj1,Qi1_j1,WDj1
	real Pj,alfa0,Qbeta,FracU,FracD,Qi1j1,cons,criterion
	real	f,df,alfa,beta,Qtmp,Qtmp2 !,tmp,h1,h2,qq2
	integer k,ktmp

	criterion= 1.0E-20

c	CALL Q2h(n,B,S0,criterion,Qi1_j,WDj)

	Pj=B+0.002*WDj
	alfa0=(n*Pj**(2.0/3.0)/sqrt(S0))**0.6
	beta=0.6	
	if((Qi_j1).le.0.0) then
		Qbeta=0.0
	else
		Qbeta=(Qi_j1)**(beta-1.0)
      endif

	cons=dt*Qi_j1/dx +WDj*B*0.001+dt*Lj1
	if (cons.le.0) then
c		print *,"Warning: Newton cons<0, changed to 1.0E-9", cons
		cons = 1.0E-9
	endif

	FracU=dt*Qi_j1/dx + alfa0*beta*Qi_j1*Qbeta +Lj1*dt
	FracD=dt/dx + alfa0*beta*Qbeta
	Qi1j1=FracU/FracD
	!if initial estimate is less then 0.0, changet to a postive value
	if (Qi1j1.lt.10.0) Qi1j1=1.0E-30

	ktmp =1
c     Using Newton's method to calculate discharge	
	Qi1_j1 =Qi1j1
9901	CALL Q2h(n,B,S0,Qi1_j1,WDj1)
	Pj=B+0.002*WDj1
	alfa=(n*Pj**(2.0/3.0)/sqrt(S0))**0.6
	Qi1j1=Qi1_j1
	Qtmp =Qi1_j1
	Qtmp2 =Qi1j1+criterion+10.
	k= 1
	f=dt*Qi1j1/dx+ alfa*Qi1j1**beta-cons
	do while (k.le.20.and.(abs(f).gt.criterion).and.
     $	abs(Qtmp2-Qi1j1).gt.criterion)
		f=dt*Qi1j1/dx+ alfa*Qi1j1**beta-cons
		df=dt/dx+ alfa*beta*Qi1j1**(beta-1.0)
		Qtmp2 = Qi1j1
		Qi1j1 = Qi1j1 -f /df
		if (Qi1j1.lt.0.0) Qi1j1=1.0E-30
		k=k+1
	enddo
	Qi1_j1=Qi1j1

	if (Qi1_j1.lt.0.0) then
		Qi1_j1=1.0E-9
		print*,'Kinematic Wave:',Qi1j1,Qi1_j1
		print*,dt,dx,B,S0,n,WDj,
     $	Qi_j1,Lj1,Qi1_j1,WDj1
	endif

	if (abs (WDj1*B*0.001-alfa*Qi1_j1**beta).gt.criterion.and.ktmp
     $	.lt.20.and.abs (Qtmp -Qi1_j1).gt.criterion) then
		ktmp =ktmp+1
		goto 9901
	endif

c	WDj1=(cons-dt*Qi1_j1/dx)/B*1000.
c	WDj1=max (WDj1 , 1.0E-20)
c	print *,ktmp,WDj1*B-alfa*Qi1_j1**beta,k
	end

c Convert Q to H
c input : n,B,SO,Q,H
c H (unit: mm)
	Subroutine Q2h(n,B,S0,Q,H)
	implicit none
	real n,B,S0,Q,H
	real*8 f,df,tmp,h1,criterion,htmp
	integer k

	criterion=1.0E-20
	k= 1
	h1= Q/B*1000.	
	tmp= (n*Q/sqrt(S0))**0.6
	htmp =h1+criterion+10.
	f=b*h1*0.001- tmp*((b+0.002*h1)**0.4)
	do while (k.le.30.and.(abs(f).gt.criterion)
     $ .and.abs(htmp-h1).gt.criterion	)
		f=b*h1*0.001- tmp*((b+0.002*h1)**0.4)
		df= b*0.001-0.0008*tmp/((b+0.002*h1)**0.6) !o
		htmp =h1
		h1= h1-f/df
		k= k+1
	enddo
	H=h1

	END !Subroutine Q2h
