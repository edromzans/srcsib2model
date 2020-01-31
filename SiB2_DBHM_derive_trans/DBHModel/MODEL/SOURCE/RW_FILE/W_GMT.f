c Get the Shell script to draw figure table by GMT
c fname: csh file name; psname: ps file name
c Nfig: total number of figure; NNC: columns of figures
c WID: width of figure (point); HGH: heigh of figure (point = 1/72 inch) (I)
c DDX: interval between 2 figure; DDY: interval between 2 figure (inch) (I)
c IDX,IDY: margin of the paper 
c r1,r2: xmin, xmax
c r3,r4: ymin, ymax
c cx,cxx: location of legend % x axis, location of legend text dx
c cy,cyy: location of legend % y axis, location of legend text dy
c tyy: location of title text dy (over the y max or below y min)
c stname: title list, dname: data file name
	subroutine write_csh(fname,psname,
     $	Nfig,NNC,WID,HGH,DDX,DDY,IDX,IDY,
     $	r1,r2,r3,r4,cx,cxx,tyy,cy,cyy,
     $    stname,dname)
	character*80 fname,psname,stname(30),dname(30)
	character*1	legend(10)
	character*20 chleg(10)
	integer l1,l2,Nfig,ifig,n1,n2,m1,m2
	integer	WID,HGH,DDX,DDY,dx,dy,IDX,IDY
	real	r1,r2,r3,r4,cx,cxx,tyy,cy,cyy
	integer I1,I2,I3,I4,b1,b2,nnr,nnc

	legend(1)='i'
	legend(2)='h'
	legend(3)='d'
	chleg(1)='PREC'
	chleg(2)='OBV'
	chleg(3)='SIM'

	I1=r1
	I2=r2
	I3=r3
	I4=r4
	nnr = INT (AINT ( real(Nfig) / nnc - 0.0001) + 1)

	b1=ANINT((I2-I1)/10.0/5.0)*5
	b2=ANINT((I4-I3)/5.0/5.0)*5

	call strlen(psname, l1,l2)
	open (1, file=fname,status="unknown")
	write(1,'(A)') "#!/bin/csh"
	write(1,'(A)') "#by Tang Qiuhong: tangqh@iis.u-tokyo.ac.jp"
	write(1,'(A)') "#Generated with Fortan"
	write(1,'(A)') "rm -f .gmtdefaults4"
	write(1,'(A)') "gmtset PAPER_MEDIA A4"
	write(1,'(A)') "gmtset ANNOT_FONT_SIZE_PRIMARY 8 ANOT_OFFSET 0.07c"
	write(1,'(A)') "gmtset HEADER_FONT_SIZE 11 TICK_LENGTH 0.07c"
	write(1,'(A,A)') "set outps=",psname(l1:l2)
	write(1,'(A)') "rm -f $outps"
c	write(1,*)

		!Fig 1
		ifig = 1
		dx=IDX
		dy=IDY + (nnr-1)*DDY
		call strlen(stname(ifig), n1,n2)
		call strlen(dname(ifig), m1,m2)
		write(1,101) I1,I2,I3,I4,WID,HGH,b1,b2,dx,dy
		xtit=(r2-r1)*0.5
		if (tyy.ge.0.) ytit=(r4-r3)+tyy
		if (tyy.lt.0.) ytit=tyy
		write(1,90)	 xtit,ytit,stname(ifig)(n1:n2)
		xleg=r1+(r2-r1)*cx
		yleg=r3+(r4-r3)*cy+cyy*(3.-3.)
		write(1,91)	 xleg,yleg,legend(1)
		write(1,92)  xleg+cxx,yleg,chleg(1)
		xleg=r1+(r2-r1)*cx
		yleg=r3+(r4-r3)*cy-cyy*(3.-2.)
		write(1,91)	 xleg,yleg,legend(2)
		write(1,92)  xleg+cxx,yleg,chleg(2)
		xleg=r1+(r2-r1)*cx
		yleg=r3+(r4-r3)*cy-cyy*(3.-1.)
		write(1,91)	 xleg,yleg,legend(3)
		write(1,92)  xleg+cxx,yleg,chleg(3)
		write(1,102) 1,dname(ifig)(m1:m2)
		write(1,103) 1,dname(ifig)(m1:m2),legend(1)
		write(1,102) 2,dname(ifig)(m1:m2)
		write(1,103) 2,dname(ifig)(m1:m2),legend(2)
		write(1,102) 3,dname(ifig)(m1:m2)
		write(1,103) 3,dname(ifig)(m1:m2),legend(3)

		!Fig 2 -> Nfig
		do ifig=2,Nfig
			if (mod(ifig-1,nnc).eq.0) then
				dx = -(nnc-1)*DDX
				dy = -DDY
			else
				dx = DDX
				dy = 0.0
			endif
			call strlen(stname(ifig), n1,n2)
			call strlen(dname(ifig), m1,m2)
			if (ifig.ne.Nfig) then
			if (dx.le.0.0.and.dy.le.0.0) then
				write(1,2012) I1,I2,I3,I4,WID,HGH,b1,b2,dx,dy
			else
				write(1,201) I1,I2,I3,I4,WID,HGH,b1,b2,dx,dy
			endif
			xtit=(r2-r1)*0.5
			if (tyy.ge.0.) ytit=(r4-r3)+tyy
			if (tyy.lt.0.) ytit=tyy
			write(1,90)	 xtit,ytit,stname(ifig)(n1:n2)
			xleg=r1+(r2-r1)*cx
			yleg=r3+(r4-r3)*cy+cyy*(3.-3.)
			write(1,91)	 xleg,yleg,legend(1)
			write(1,92)  xleg+cxx,yleg,chleg(1)
			xleg=r1+(r2-r1)*cx
			yleg=r3+(r4-r3)*cy-cyy*(3.-2.)
			write(1,91)	 xleg,yleg,legend(2)
			write(1,92)  xleg+cxx,yleg,chleg(2)
			xleg=r1+(r2-r1)*cx
			yleg=r3+(r4-r3)*cy-cyy*(3.-1.)
			write(1,91)	 xleg,yleg,legend(3)
			write(1,92)  xleg+cxx,yleg,chleg(3)
			write(1,102) 1,dname(ifig)(m1:m2)
			write(1,103) 1,dname(ifig)(m1:m2),legend(1)
			write(1,102) 2,dname(ifig)(m1:m2)
			write(1,103) 2,dname(ifig)(m1:m2),legend(2)
			write(1,102) 3,dname(ifig)(m1:m2)
			write(1,103) 3,dname(ifig)(m1:m2),legend(3)
			else
			if (dx.le.0.0.and.dy.le.0.0) then
				write(1,2012) I1,I2,I3,I4,WID,HGH,b1,b2,dx,dy
			else
				write(1,201) I1,I2,I3,I4,WID,HGH,b1,b2,dx,dy
			endif
			xtit=(r2-r1)*0.5
			if (tyy.ge.0.) ytit=(r4-r3)+tyy
			if (tyy.lt.0.) ytit=tyy
			write(1,90)	 xtit,ytit,stname(ifig)(n1:n2)
			xleg=r1+(r2-r1)*cx
			yleg=r3+(r4-r3)*cy+cyy*(3.-3.)
			write(1,91)	 xleg,yleg,legend(1)
			write(1,92)  xleg+cxx,yleg,chleg(1)
			xleg=r1+(r2-r1)*cx
			yleg=r3+(r4-r3)*cy-cyy*(3.-2.)
			write(1,91)	 xleg,yleg,legend(2)
			write(1,92)  xleg+cxx,yleg,chleg(2)
			xleg=r1+(r2-r1)*cx
			yleg=r3+(r4-r3)*cy-cyy*(3.-1.)
			write(1,91)	 xleg,yleg,legend(3)
			write(1,92)  xleg+cxx,yleg,chleg(3)
			write(1,102) 1,dname(ifig)(m1:m2)
			write(1,103) 1,dname(ifig)(m1:m2),legend(1)
			write(1,102) 2,dname(ifig)(m1:m2)
			write(1,103) 2,dname(ifig)(m1:m2),legend(2)
			write(1,102) 3,dname(ifig)(m1:m2)
			write(1,203) 3,dname(ifig)(m1:m2),legend(3)
			endif	
		enddo
		write(1,'(A)') "rm -f .gmtdefaults4"
		write(1,'(A,A)') "gv ",psname(l1:l2)	
	close(1)

90	format("pstext -R -JX -O -K -N <<END >> $outps",/,F,F,
     $" 12 0 0 MC @%5%",A," @%%",/,"END") 
91	format("echo """,F,F,""" | psxy   -R -JX -O -S"
     $,A,"0.05i -K >> $outps")
92	format("pstext -R -JX -O -K <<END >> $outps ",/,F,F,
     $" 8 0 0 ML ",A,/,"END")
101	format("psbasemap -R",3(I5.5,"/"),I5.5," -JX",I5.5,"p/",I5.5,"p",
     $" -B",I5.5,"/",I5.5," -P -K -X",I5.5,"p -Y",I5.5,
     $"p >! $outps" )
102	format("awk '{print NR,$",I3.3,"}' ",A," | psxy ",
     $" -R -JX -O -W0.5p -K >> $outps")
103	format("awk '{print NR,$",I3.3,"}' ",A," | psxy ",
     $" -R -JX -O -S",A,"0.05i -K >> $outps")
c101	format("psbasemap -R",3(I5.5,"/"),I5.5," -JX",I5.5,"p/",I5.5,"p",
c     $" -B",I5.5,"/",I5.5," -P -K -X",I5.5,"p -Y",I5.5,
c     $"p >! $outps" )
c102	format("psxy ",A," -R -JX -O -B -P -W0.5p -K >> $outps")
c103	format("psxy ",A," -R -JX -O -W -Si0.1i -K >> $outps")

201	format("psbasemap -R",3(I8.8,"/"),I8.8," -JX",I5.5,"p/",I5.5,"p",
     $" -B",I5.5,"/",I5.5," -P -K -X",I5.5,"p -Y",I5.5,
     $"p -O >> $outps" )
2012	format("psbasemap -R",3(I8.8,"/"),I8.8," -JX",I5.5,"p/",I5.5,"p",
     $" -B",I5.5,"/",I5.5," -P -K -X",I6.5,"p -Y",I6.5,
     $"p -O >> $outps" )
203	format("awk '{print NR,$",I3.3,"}' ",A," | psxy ",
     $" -R -JX -O -S",A,"0.05i >> $outps")
c203	format("psxy ",A," -R -JX -O -W -Si0.1i>> $outps")
	
	end