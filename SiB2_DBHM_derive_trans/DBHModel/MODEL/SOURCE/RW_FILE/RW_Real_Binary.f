C	Write/Read scratchpad in Plain Real Binary Format (No Swap)
c	WR: =0, WRITE  =1, READ
c	nt:	timestep
c	NOTE (VERY Important):
c		USE 'f77 FILENAME /assume:byterecl' under windows OS
	subroutine WR_binary(name,WR,nt,nr,nc,array,nnt,nnr,nnc)
	implicit none
	character*80  name
	integer WR,nt,nr,nc,nnt,nnr,nnc
	real*4	array(nnt,nnr,nnc)
	integer I,J,K !,REC
	
	if (WR.eq.0) then
	open(1,form='unformatted',file=name,access='direct',
     *		recl=4*nr*nc,err=9002)
	else
	open(1,form='unformatted',file=name,access='direct',
     *		recl=4*nr*nc,status='old',err=9002)
	endif
	Do I=1,nt
		IF (WR.eq.0) then
			WRITE(1,rec=I,err=9003) ((array(I,J,K),K=1,nc),J=1,nr)	
		Else
			READ(1,rec=I,err=9004) ((array(I,J,K),K=1,nc),J=1,nr)	
		Endif
	Enddo
	CLOSE(1)
	Return
9002	PRINT *, 'ERROR IN OPEN BINARY FILE, ERROR CODE: 9002',name
	RETURN
9003  PRINT *, 'ERROR IN WRITE BINARY FILE, ERROR CODE: 9003',name
	RETURN
9004	PRINT *, 'ERROR IN READING BINARY FILE, ERROR CODE: 9004',name
	PRINT *, 'PLEASE MAKE SURE USE "/ASSUME:BYTERECL" UNDER OS WINDOWS'
	RETURN
	end

C	Write/Read scratchpad in Plain Real Binary Format (No Swap)
C	!!! ONLY READ OR WRITE THE nt TIME STEP DATA
c	WR: =0, WRITE  =1, READ
c	nt:	timestep
c	NOTE (VERY Important):
c		USE 'f77 FILENAME /assume:byterecl' under windows OS
	subroutine WR_binary_nt(name,WR,nt,nr,nc,array,nnr,nnc)
	implicit none
	character*80  name
	integer WR,nt,nr,nc,nnr,nnc !,nnt
	real*4	array(nnr,nnc)
	integer J,K !,REC,I
	
	if (WR.eq.0) then
	open(1,form='unformatted',file=name,access='direct',
     *		recl=4*nr*nc,err=9002)
	else
	open(1,form='unformatted',file=name,access='direct',
     *		recl=4*nr*nc,status='old',err=9002)
	endif
c	print *,name
	IF (WR.eq.0) then
		WRITE(1,rec=nt,err=9003) ((array(J,K),K=1,nc),J=1,nr)	
	Else
		READ(1,rec=nt,err=9004) ((array(J,K),K=1,nc),J=1,nr)
c		print *, array(1,1)	,array(1,2)	
	Endif
	CLOSE(1)
	Return
9002	PRINT *, 'ERROR IN OPEN BINARY FILE, ERROR CODE: 9002_2',name
	RETURN
9003  PRINT *, 'ERROR IN WRITE BINARY FILE, ERROR CODE: 9003_2',name
	RETURN
9004	PRINT *, 'ERROR IN READING BINARY FILE, ERROR CODE: 9004_2',name
	PRINT *, 'PLEASE MAKE SURE USE "/ASSUME:BYTERECL" UNDER OS WINDOWS'
	RETURN
	end

C	Write/Read scratchpad in Plain Real Binary Format (No Swap)
c	WR: =0, WRITE  =1, READ
c	NOTE (VERY Important):
c		USE 'f77 FILENAME /assume:byterecl' under windows OS
	subroutine WR_binary_1(name,WR,nr,nc,array,nnr,nnc)
	implicit none
	character*80  name
	integer WR,nr,nc,nnr,nnc
	real*4	array(nnr,nnc)
	integer J,K !,REC,I
	
	if (WR.eq.0) then
	open(1,form='unformatted',file=name,access='direct',
     *		recl=4*nr*nc,err=9002)
	else
	open(1,form='unformatted',file=name,access='direct',
     *		recl=4*nr*nc,status='old',err=9002)
	endif
		IF (WR.eq.0) then
			WRITE(1,rec=1,err=9003) ((array(J,K),K=1,nc),J=1,nr)	
		Else
			READ(1,rec=1,err=9004) ((array(J,K),K=1,nc),J=1,nr)	
		Endif
	CLOSE(1)
	Return
9002	PRINT *, 'ERROR IN OPEN BINARY FILE, ERROR CODE: 9002_2',name
	RETURN
9003  PRINT *, 'ERROR IN WRITE BINARY FILE, ERROR CODE: 9003_2',name
	RETURN
9004	PRINT *, 'ERROR IN READING BINARY FILE, ERROR CODE: 9004_2',name
	PRINT *, 'PLEASE MAKE SURE USE "/ASSUME:BYTERECL" UNDER OS WINDOWS'
	RETURN
	end